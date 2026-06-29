! Surface energy balance solver (Noah-MP style sequential canopy/ground solve)
module mod_surface
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_forcing, t_state, t_param, t_config, t_flux
  use mod_physics, only: STOM_MEDLYN, CANOPY_TWOLEAF
  use mod_radiation
  use mod_turbulence
  use mod_conductance
  use mod_soil_heat, only: implicit_soil_heat_flux, soil_ground_resistance, skin_storage_flux
  use mod_snow,      only: snow_ground_flux
  use mod_radtran,   only: compute_radiation, dRn_dTs_rad, noah_lw_coeff_canopy, noah_lw_coeff_ground, &
                             noah_rad_lw_net_outward, surface_emissivities
  use mod_canopy,    only: canopy_stomatal_two_leaf
  use mod_planthydro, only: water_stress_factor
  use mod_photosyn,  only: photosyn_stomatal_coupling_tl
  implicit none

  integer, parameter :: mo_inner = 3
  integer, parameter :: num_iter_c = 20
  integer, parameter :: num_iter_g = 5
  integer, parameter :: picard_outer = 3
  real(dp), parameter :: dt_tol = 0.01_dp
  real(dp), parameter :: newton_damp = 1.0_dp
  real(dp), parameter :: leaf_dim = 0.05_dp
  real(dp), parameter :: canopy_wind_ext = 0.5_dp

contains

  subroutine solve_energy_balance(force, state, par, cfg, flux)
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    type(t_flux),    intent(out)   :: flux
    logical :: two_leaf, converged
    real(dp) :: ts, tc, ts_prev, qa, wstress, f_exposed
    real(dp) :: ts_min, ts_max, tc_min, tc_max

    ts_prev = state%Ts
    if (state%snow_present) then
       ts = state%snow_T
       ts_prev = state%snow_T
       tc = force%Ta
    else
       ts = state%Ts
       tc = state%Tc
    end if
    qa = specific_humidity(force)
    ts_min = force%Ta - 30.0_dp
    ts_max = force%Ta + 30.0_dp
    tc_min = force%Ta - 25.0_dp
    tc_max = force%Ta + 25.0_dp
    flux%ra = 50.0_dp
    wstress = water_stress_factor(cfg%ihydro, state)
    f_exposed = max(1.0_dp - state%LAI * 0.3_dp, 0.1_dp)
    two_leaf = cfg%icanopy == CANOPY_TWOLEAF .and. state%LAI > 0.01_dp .and. .not. state%snow_present
    converged = .false.

    if (two_leaf) then
       call solve_two_leaf_noahmp(force, state, par, cfg, flux, ts, tc, ts_prev, qa, wstress, &
            f_exposed, ts_min, ts_max, tc_min, tc_max, converged)
    else
       call solve_surface_balance(force, state, par, cfg, flux, ts, ts_prev, qa, wstress, &
            f_exposed, .false., ts_min, ts_max, converged)
       tc = force%Ta
    end if

    state%Ts = ts
    state%Tc = tc
    if (state%snow_present) state%snow_T = ts
    flux%tau_momentum = rho * 0.001_dp * force%WS**2

    if (.not. converged .and. abs(flux%ebal_res) > 5.0_dp * cfg%tol) then
       write(*,'(A,ES12.4,A)') 'WARN: energy balance not converged, residual=', flux%ebal_res, ' W/m2'
    end if
  end subroutine solve_energy_balance

  subroutine solve_two_leaf_noahmp(force, state, par, cfg, flux, ts, tc, ts_prev, qa, wstress, &
       f_exposed, ts_min, ts_max, tc_min, tc_max, converged)
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    type(t_flux),    intent(inout) :: flux
    real(dp), intent(inout) :: ts, tc
    real(dp), intent(in) :: ts_prev, qa, wstress, f_exposed
    real(dp), intent(in) :: ts_min, ts_max, tc_min, tc_max
    logical, intent(out) :: converged
    integer :: it, last_iter, ip
    logical :: stomatal_done
    real(dp) :: ra, rb, rac, rs_sun, rs_shade, rs_soil
    real(dp) :: t_ca, ea_ca, lai_eff, lai_sun, lai_shd
    real(dp) :: lw_air_c, lw_can_c, sw_can, sw_ground
    real(dp) :: rad_lw_c, h_c, le_c, storage_c, dtc, flux_coeff
    real(dp) :: lw_air_g, lw_can_g, rad_lw_g, h_g, le_g, g, storage_g, dts, flux_coeff_g
    real(dp) :: sh_mo, h_abv, h_und, L, psi_m, psi_h, ustar
    real(dp) :: g_abv, g_rb, g_rac, g_tot, sh_frac, et_frac
    real(dp) :: ce_abv, ce_transp, ce_und, ce_tot
    real(dp) :: es_tc, des_tc, es_ts, des_ts, ea_ref, gamma
    real(dp) :: heat_cap_can, emiss_surf, emiss_can, tc_prev, instant_res
    real(dp) :: sh_coeff, lh_coeff, transp_coeff, grd_coeff

    converged = .false.
    stomatal_done = .false.
    rs_soil = 50.0_dp
    lai_eff = min(state%LAI, 6.0_dp)
    lai_sun = min(max(state%LAI_sun, 0.0_dp), 6.0_dp)
    lai_shd = min(max(state%LAI_shade, 0.0_dp), 6.0_dp)
    tc_prev = state%Tc
    call surface_emissivities(state, par, emiss_surf, emiss_can)

    call compute_radiation(force, state, par, cfg, flux, ts, tc)
    sw_can = flux%SW_abs_canopy
    sw_ground = flux%SW_abs_ground

    gamma = psychrometric_const(force%PA)
    ea_ref = vapor_pressure_from_qa(qa, force%PA)
    ra = flux%ra
    ustar = 0.1_dp
    heat_cap_can = par%skin_heat_cap * 0.5_dp * lai_eff
    ce_abv = 1.0_dp / ra

    picard_loop: do ip = 1, picard_outer
       stomatal_done = .false.
       last_iter = 0

       call noah_lw_coeff_canopy(emiss_can, emiss_surf, force%LW, ts, lw_air_c, lw_can_c)

       loop_canopy: do it = 1, num_iter_c
          call noah_leaf_ground_resistance(force, par, lai_eff, ustar, rb, rac)

          if (.not. stomatal_done) then
             call canopy_stomatal_two_leaf(force, state, par, cfg, flux, tc, rs_sun, rs_shade)
             stomatal_done = .true.
          end if

          g_abv = 1.0_dp / ra
          g_rb = 2.0_dp * lai_eff / rb
          g_rac = 1.0_dp / rac
          g_tot = g_abv + g_rb + g_rac
          sh_frac = g_rb / g_tot
          t_ca = (force%Ta * g_abv + ts * g_rac + tc * g_rb) / g_tot

          ce_abv = g_abv
          ce_transp = lai_sun / max(rb + rs_sun, 1.0_dp) + lai_shd / max(rb + rs_shade, 1.0_dp)
          ce_und = 1.0_dp / max(rac + rs_soil, 1.0_dp)
          ce_tot = ce_abv + ce_transp + ce_und
          et_frac = ce_transp / max(ce_tot, 1.0e-8_dp)
          ea_ca = (ea_ref * ce_abv + vapor_pressure_from_qa(qs_from_T_P(ts, force%PA), force%PA) * ce_und) &
                / max(ce_tot, 1.0e-8_dp)
          es_tc = sat_vapor_pressure(tc)
          ea_ca = ea_ca + et_frac * es_tc

          rad_lw_c = noah_rad_lw_net_outward(lw_air_c, lw_can_c, tc)
          h_c = rho * cp * g_rb * (tc - t_ca)
          le_c = wstress * rho * cp * ce_transp * max(es_tc - ea_ca, 0.0_dp) / gamma

          sh_coeff = (1.0_dp - sh_frac) * rho * cp * g_rb
          transp_coeff = wstress * (1.0_dp - et_frac) * ce_transp * rho * cp / gamma
          call sat_vapor_slope(tc, des_tc)
          flux_coeff = 4.0_dp * lw_can_c * tc**3 + sh_coeff + transp_coeff * des_tc &
                     + heat_cap_can / cfg%dt

          dtc = 0.0_dp
          if (abs(flux_coeff) > 1.0e-8_dp) then
             dtc = (sw_can - rad_lw_c - h_c - le_c) / flux_coeff
             dtc = max(min(dtc, 5.0_dp), -5.0_dp)
             tc = tc + dtc
             tc = max(min(tc, tc_max), tc_min)
          end if

          h_abv = rho * cp * (t_ca - force%Ta) / ra
          h_und = rho * cp * (ts - t_ca) / rac
          sh_mo = h_abv + h_und
          call mo_stability(ts, force%Ta, force%WS, sh_mo, par, L, psi_m, psi_h)
          ra = aero_resistance(force, par, L)
          ustar = max(karman * force%WS / (log((zref - par%zdisp) / par%z0) - psi_m), 0.05_dp)

          if (last_iter == 1) exit loop_canopy
          if (it >= 5 .and. abs(dtc) <= dt_tol .and. last_iter == 0) last_iter = 1
       end do loop_canopy

       call noah_leaf_ground_resistance(force, par, lai_eff, ustar, rb, rac)
       g_abv = 1.0_dp / ra
       g_rb = 2.0_dp * lai_eff / rb
       g_rac = 1.0_dp / rac
       g_tot = g_abv + g_rb + g_rac
       t_ca = (force%Ta * g_abv + ts * g_rac + tc * g_rb) / g_tot
       ce_transp = lai_sun / max(rb + rs_sun, 1.0_dp) + lai_shd / max(rb + rs_shade, 1.0_dp)
       ce_und = 1.0_dp / max(rac + rs_soil, 1.0_dp)
       ce_abv = g_abv
       ce_tot = ce_abv + ce_transp + ce_und
       ea_ca = (ea_ref * ce_abv + vapor_pressure_from_qa(qs_from_T_P(ts, force%PA), force%PA) * ce_und) &
             / max(ce_tot, 1.0e-8_dp) &
             + (ce_transp / max(ce_tot, 1.0e-8_dp)) * sat_vapor_pressure(tc)

       call noah_lw_coeff_ground(emiss_can, emiss_surf, force%LW, tc, lw_air_g, lw_can_g)
       sh_coeff = rho * cp / rac
       lh_coeff = rho * cp / (gamma * (rac + rs_soil))
       if (state%snow_present) then
          grd_coeff = -k_snow_dz(state%snow_depth)
       else
          grd_coeff = 1.0_dp / soil_ground_resistance(state, par, cfg%dt)
       end if

       dts = 1.0e6_dp
       loop_ground: do it = 1, num_iter_g
          t_ca = (force%Ta * g_abv + ts * g_rac + tc * g_rb) / g_tot
          ce_tot = ce_abv + ce_transp + ce_und
          ea_ca = (ea_ref * ce_abv + vapor_pressure_from_qa(qs_from_T_P(ts, force%PA), force%PA) * ce_und) &
                / max(ce_tot, 1.0e-8_dp) &
                + (ce_transp / max(ce_tot, 1.0e-8_dp)) * sat_vapor_pressure(tc)

          es_ts = sat_vapor_pressure(ts)
          call sat_vapor_slope(ts, des_ts)
          rad_lw_g = noah_rad_lw_net_outward(lw_air_g, lw_can_g, ts)
          h_g = sh_coeff * (ts - t_ca)
          le_g = lh_coeff * max(es_ts - ea_ca, 0.0_dp)
          if (state%snow_present) then
             g = snow_ground_flux(ts, state%Tsoil(1), state%snow_depth)
          else
             g = grd_coeff * (ts - state%Tsoil(1))
          end if

          flux_coeff_g = 4.0_dp * lw_can_g * ts**3 + sh_coeff + lh_coeff * des_ts + grd_coeff
          if (.not. state%snow_present) flux_coeff_g = flux_coeff_g + par%skin_heat_cap / cfg%dt

          dts = 0.0_dp
          if (abs(flux_coeff_g) > 1.0e-8_dp) then
             dts = (sw_ground - rad_lw_g - h_g - le_g - g) / flux_coeff_g
             dts = max(min(dts, 5.0_dp), -5.0_dp)
             ts = ts + dts
             ts = max(min(ts, ts_max), ts_min)
          end if
          if (abs(dts) <= dt_tol) exit loop_ground
       end do loop_ground
    end do picard_loop

    t_ca = (force%Ta * g_abv + ts * g_rac + tc * g_rb) / g_tot
    ce_tot = ce_abv + ce_transp + ce_und
    ea_ca = (ea_ref * ce_abv + vapor_pressure_from_qa(qs_from_T_P(ts, force%PA), force%PA) * ce_und) &
          / max(ce_tot, 1.0e-8_dp) &
          + (ce_transp / max(ce_tot, 1.0e-8_dp)) * sat_vapor_pressure(tc)

    call noah_leaf_ground_resistance(force, par, lai_eff, ustar, rb, rac)
    g_abv = 1.0_dp / ra
    g_rb = 2.0_dp * lai_eff / rb
    g_rac = 1.0_dp / rac
    g_tot = g_abv + g_rb + g_rac
    t_ca = (force%Ta * g_abv + ts * g_rac + tc * g_rb) / g_tot
    ce_abv = g_abv
    ce_transp = lai_sun / max(rb + rs_sun, 1.0_dp) + lai_shd / max(rb + rs_shade, 1.0_dp)
    ce_und = 1.0_dp / max(rac + rs_soil, 1.0_dp)
    ce_tot = ce_abv + ce_transp + ce_und
    ea_ca = (ea_ref * ce_abv + vapor_pressure_from_qa(qs_from_T_P(ts, force%PA), force%PA) * ce_und) &
          / max(ce_tot, 1.0e-8_dp) &
          + (ce_transp / max(ce_tot, 1.0e-8_dp)) * sat_vapor_pressure(tc)
    call noah_lw_coeff_canopy(emiss_can, emiss_surf, force%LW, ts, lw_air_c, lw_can_c)
    call noah_lw_coeff_ground(emiss_can, emiss_surf, force%LW, tc, lw_air_g, lw_can_g)
    sh_coeff = rho * cp / rac
    lh_coeff = rho * cp / (gamma * (rac + rs_soil))
    es_tc = sat_vapor_pressure(tc)
    es_ts = sat_vapor_pressure(ts)
    rad_lw_c = noah_rad_lw_net_outward(lw_air_c, lw_can_c, tc)
    rad_lw_g = noah_rad_lw_net_outward(lw_air_g, lw_can_g, ts)
    h_c = rho * cp * g_rb * (tc - t_ca)
    le_c = wstress * rho * cp * ce_transp * max(es_tc - ea_ca, 0.0_dp) / gamma
    h_g = sh_coeff * (ts - t_ca)
    le_g = lh_coeff * max(es_ts - ea_ca, 0.0_dp)
    if (state%snow_present) then
       g = snow_ground_flux(ts, state%Tsoil(1), state%snow_depth)
       storage_g = 0.0_dp
    else
       g = implicit_soil_heat_flux(ts, state, par, cfg%dt)
       storage_g = skin_storage_flux(ts, ts_prev, par, cfg%dt)
    end if
    storage_c = heat_cap_can * (tc - tc_prev) / cfg%dt

    call compute_radiation(force, state, par, cfg, flux, ts, tc)
    flux%ra = ra
    flux%H_canopy = h_c
    flux%LE_canopy = le_c
    flux%LE_soil = le_g
    flux%H = h_c + h_g
    flux%LE = le_c + le_g
    flux%G = g
    flux%ET = flux%LE / lambda
    instant_res = (flux%Rn_canopy - h_c - le_c) + (flux%Rn_ground - h_g - le_g - g)
    flux%ebal_res = instant_res
    converged = abs(instant_res) < cfg%tol
  end subroutine solve_two_leaf_noahmp

  subroutine noah_leaf_ground_resistance(force, par, lai_eff, ustar, rb, rac)
    type(t_forcing), intent(in) :: force
    type(t_param), intent(in)   :: par
    real(dp), intent(in) :: lai_eff, ustar
    real(dp), intent(out) :: rb, rac
    real(dp) :: wind_top, wind_ext, k_h, tmp1, tmp2, tmprah2, tmprb

    wind_top = max(force%WS, 0.1_dp) &
             * log((par%hc - par%zdisp + par%z0) / par%z0) / log(zref / par%z0)
    wind_ext = sqrt(canopy_wind_ext * lai_eff * par%hc)
    wind_ext = max(wind_ext, 0.05_dp)
    tmp1 = exp(-wind_ext * par%z0 / par%hc)
    tmp2 = exp(-wind_ext * (par%z0 + par%zdisp) / par%hc)
    tmprah2 = par%hc * exp(wind_ext) / wind_ext * (tmp1 - tmp2)
    k_h = max(karman * ustar * (par%hc - par%zdisp), 1.0e-6_dp)
    rac = max(tmprah2 / k_h, 5.0_dp)
    tmprb = wind_ext * 50.0_dp / (1.0_dp - exp(-wind_ext / 2.0_dp))
    rb = tmprb * sqrt(leaf_dim / max(wind_top, 0.5_dp))
    rb = min(max(rb, 5.0_dp), 50.0_dp)
  end subroutine noah_leaf_ground_resistance

  function psychrometric_const(pa) result(gamma)
    real(dp), intent(in) :: pa
    real(dp) :: gamma
    gamma = cp * pa / (eps * lambda)
  end function psychrometric_const

  function vapor_pressure_from_qa(qa, pa) result(ea)
    real(dp), intent(in) :: qa, pa
    real(dp) :: ea
    ea = qa * pa / (eps + (1.0_dp - eps) * qa)
  end function vapor_pressure_from_qa

  subroutine sat_vapor_slope(tk, des)
    real(dp), intent(in)  :: tk
    real(dp), intent(out) :: des
    real(dp) :: es
    es = sat_vapor_pressure(tk)
    des = 4098.0_dp * es / (tk - tfrz + 243.5_dp)**2
  end subroutine sat_vapor_slope

  function dq_dT(tk, pa) result(dq)
    real(dp), intent(in) :: tk, pa
    real(dp) :: dq, des
    call sat_vapor_slope(tk, des)
    dq = eps * pa / (pa - (1.0_dp - eps) * sat_vapor_pressure(tk))**2 * des
  end function dq_dT

  subroutine solve_surface_balance(force, state, par, cfg, flux, ts, ts_prev, qa, wstress, &
       f_exposed, two_leaf, ts_min, ts_max, converged)
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    type(t_flux),    intent(inout) :: flux
    real(dp), intent(inout) :: ts
    real(dp), intent(in) :: ts_prev, qa, wstress, f_exposed, ts_min, ts_max
    logical, intent(in) :: two_leaf
    logical, intent(out) :: converged
    integer :: it
    real(dp) :: resid, dresid, rn, g, h, le, storage, rs_soil, rs_le, an, ci, dts

    converged = .false.
    do it = 1, cfg%max_iter
       call compute_radiation(force, state, par, cfg, flux, ts, force%Ta)
       rn = flux%Rn_ground

       if (two_leaf) then
          rs_soil = 50.0_dp
          le = f_exposed * latent_heat_pm(ts, qa, force%PA, flux%ra, rs_soil, 1.0_dp)
          rs_le = rs_soil
          h = f_exposed * sensible_heat(ts, force%Ta, flux%ra)
       else
          if (cfg%istomatal == STOM_MEDLYN) then
             call photosyn_stomatal_coupling_tl(force, state, par, cfg, ts, flux%gs, an, ci)
             flux%An = an
             flux%Ci = ci
          else
             call stomatal_conductance(force, state, par, cfg, 0.0_dp, flux%gs)
          end if
          flux%rs = canopy_resistance(flux%gs, state%LAI)
          le = latent_heat_pm(ts, qa, force%PA, flux%ra, flux%rs, wstress)
          rs_le = flux%rs
          h = sensible_heat(ts, force%Ta, flux%ra)
       end if
       call update_aero_resistance(force, par, ts, force%Ta, force%WS, h, flux%ra, two_leaf, f_exposed)

       if (state%snow_present) then
          g = snow_ground_flux(ts, state%Tsoil(1), state%snow_depth)
          storage = 0.0_dp
       else
          g = implicit_soil_heat_flux(ts, state, par, cfg%dt)
          storage = skin_storage_flux(ts, ts_prev, par, cfg%dt)
       end if

       resid = rn - h - le - g - storage
       if (abs(resid) < cfg%tol) then
          converged = .true.
          exit
       end if

       dresid = dRn_dTs_rad(state, par, cfg, ts) &
              - merge(1.0_dp, f_exposed, two_leaf) * rho * cp / flux%ra &
              - dLE_dTs(ts, force%PA, flux%ra, rs_le, merge(1.0_dp, wstress, two_leaf))
       if (state%snow_present) then
          dresid = dresid - k_snow_dz(state%snow_depth)
       else
          dresid = dresid - 1.0_dp / soil_ground_resistance(state, par, cfg%dt) &
                 - par%skin_heat_cap / cfg%dt
       end if
       if (abs(dresid) < 1.0e-8_dp) exit
       dts = newton_damp * resid / dresid
       ts = ts + dts
       ts = max(min(ts, ts_max), ts_min)
       if (abs(dts) <= dt_tol) then
          converged = .true.
          exit
       end if
    end do

    call finalize_surface_fluxes(force, state, par, cfg, flux, ts, ts_prev, qa, wstress, &
         f_exposed, two_leaf, force%Ta, converged)
  end subroutine solve_surface_balance

  subroutine finalize_surface_fluxes(force, state, par, cfg, flux, ts, ts_prev, qa, wstress, &
       f_exposed, two_leaf, tc_dummy, converged)
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    type(t_flux),    intent(inout) :: flux
    real(dp), intent(in) :: ts, ts_prev, qa, wstress, f_exposed, tc_dummy
    logical, intent(in) :: two_leaf, converged
    real(dp) :: g, h, le, le_soil, storage, rs_soil

    call compute_radiation(force, state, par, cfg, flux, ts, tc_dummy)

    if (two_leaf) then
       rs_soil = 50.0_dp
       le_soil = f_exposed * latent_heat_pm(ts, qa, force%PA, flux%ra, rs_soil, 1.0_dp)
       flux%LE_soil = le_soil
       h = f_exposed * sensible_heat(ts, force%Ta, flux%ra)
       le = le_soil
       flux%H_canopy = 0.0_dp
       flux%LE_canopy = 0.0_dp
    else
       h = sensible_heat(ts, force%Ta, flux%ra)
       if (cfg%istomatal == STOM_MEDLYN) then
          flux%rs = canopy_resistance(flux%gs, state%LAI)
       end if
       le = latent_heat_pm(ts, qa, force%PA, flux%ra, flux%rs, wstress)
       flux%LE_soil = 0.0_dp
       flux%H_canopy = 0.0_dp
       flux%LE_canopy = 0.0_dp
    end if

    if (state%snow_present) then
       g = snow_ground_flux(ts, state%Tsoil(1), state%snow_depth)
       storage = 0.0_dp
    else
       g = implicit_soil_heat_flux(ts, state, par, cfg%dt)
       storage = skin_storage_flux(ts, ts_prev, par, cfg%dt)
    end if

    flux%Rn = flux%Rn_ground + flux%Rn_canopy
    flux%H = h
    flux%LE = le
    flux%G = g
    flux%ET = le / lambda
    flux%ebal_res = flux%Rn - flux%H - flux%LE - flux%G - storage
  end subroutine finalize_surface_fluxes

  subroutine update_aero_resistance(force, par, ts, ta, ws, h, ra, two_leaf, f_exposed)
    type(t_forcing), intent(in) :: force
    type(t_param), intent(in)   :: par
    real(dp), intent(in) :: ts, ta, ws, f_exposed
    logical, intent(in) :: two_leaf
    real(dp), intent(inout) :: h
    real(dp), intent(out) :: ra
    real(dp) :: L, psi_m, psi_h, h_raw
    integer :: k

    ra = 50.0_dp
    do k = 1, mo_inner
       call mo_stability(ts, ta, ws, h, par, L, psi_m, psi_h)
       ra = aero_resistance(force, par, L)
       h_raw = sensible_heat(ts, ta, ra)
       if (two_leaf) then
          h = f_exposed * h_raw
       else
          h = h_raw
       end if
    end do
  end subroutine update_aero_resistance

  function k_snow_dz(snow_depth) result(dg)
    real(dp), intent(in) :: snow_depth
    real(dp) :: dg
    dg = -0.3_dp / max(snow_depth, 0.001_dp)
  end function k_snow_dz

  function latent_heat_pm(ts, qa, pa, ra, rs, wstress) result(le)
    real(dp), intent(in) :: ts, qa, pa, ra, rs, wstress
    real(dp) :: le
    real(dp) :: qs, dq
    qs = qs_from_T_P(ts, pa)
    dq = max(qs - qa, 0.0_dp)
    le = rho * lambda * wstress * dq / (ra + rs)
  end function latent_heat_pm

  function dLE_dTs(ts, pa, ra, rs, wstress) result(dle)
    real(dp), intent(in) :: ts, pa, ra, rs, wstress
    real(dp) :: dle
    dle = rho * lambda * wstress / (ra + rs) * dq_dT(ts, pa)
  end function dLE_dTs

end module mod_surface