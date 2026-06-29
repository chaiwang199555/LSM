! Two-big-leaf canopy: sun/shade stomatal conductance and flux aggregation
module mod_canopy
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_forcing, t_state, t_param, t_config, t_flux
  use mod_physics, only: CANOPY_TWOLEAF, STOM_MEDLYN
  use mod_conductance, only: stomatal_conductance, canopy_resistance
  use mod_planthydro,  only: water_stress_factor
  use mod_photosyn, only: stomatal_inner_max, stomatal_gs_tol
  implicit none

contains

  subroutine canopy_stomatal_two_leaf(force, state, par, cfg, flux, tc, rs_sun, rs_shade)
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(in)    :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    type(t_flux),    intent(inout) :: flux
    real(dp), intent(in)  :: tc
    real(dp), intent(out) :: rs_sun, rs_shade
    real(dp) :: gs_sun, gs_shade, an_s, an_h, ci_s, ci_h
    real(dp) :: lai_s, lai_h

    rs_sun = 1.0e6_dp
    rs_shade = 1.0e6_dp
    if (cfg%icanopy /= CANOPY_TWOLEAF .or. state%LAI < 0.01_dp) return

    lai_s = max(state%LAI_sun, 0.0_dp)
    lai_h = max(state%LAI_shade, 0.0_dp)
    if (cfg%istomatal == STOM_MEDLYN) then
       call leaf_coupling_tl(force, state, par, cfg, flux%PAR_sun, lai_s, tc, gs_sun, an_s, ci_s)
       call leaf_coupling_tl(force, state, par, cfg, flux%PAR_shade, lai_h, tc, gs_shade, an_h, ci_h)
    else
       call stomatal_conductance(force, state, par, cfg, 0.0_dp, gs_sun)
       gs_shade = gs_sun * 0.5_dp
    end if
    flux%gs_sun = gs_sun
    flux%gs_shade = gs_shade
    rs_sun = canopy_resistance(gs_sun, max(lai_s, 0.01_dp))
    rs_shade = canopy_resistance(gs_shade, max(lai_h, 0.01_dp))
    flux%gs = (gs_sun * lai_s + gs_shade * lai_h) / max(state%LAI, 0.01_dp)
    flux%rs = canopy_resistance(flux%gs, state%LAI)
  end subroutine canopy_stomatal_two_leaf

  subroutine canopy_fluxes(force, state, par, cfg, flux, tc, qa, ra, wstress, le_canopy, rs_canopy)
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(in)    :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    type(t_flux),    intent(inout) :: flux
    real(dp), intent(in) :: tc, qa, ra, wstress
    real(dp), intent(out) :: le_canopy, rs_canopy
    real(dp) :: gs_sun, gs_shade, rs_sun, rs_shade
    real(dp) :: le_sun, le_shade
    real(dp) :: an_sun, an_shade, ci_sun, ci_shade
    real(dp) :: lai_s, lai_h

    le_canopy = 0.0_dp
    rs_canopy = 1.0e6_dp
    if (cfg%icanopy /= CANOPY_TWOLEAF .or. state%LAI < 0.01_dp) then
       return
    end if

    lai_s = max(state%LAI_sun, 0.0_dp)
    lai_h = max(state%LAI_shade, 0.0_dp)

    if (cfg%istomatal == STOM_MEDLYN) then
       call leaf_coupling_tl(force, state, par, cfg, flux%PAR_sun, lai_s, tc, gs_sun, an_sun, ci_sun)
       call leaf_coupling_tl(force, state, par, cfg, flux%PAR_shade, lai_h, tc, gs_shade, an_shade, ci_shade)
    else
       call stomatal_conductance(force, state, par, cfg, 0.0_dp, gs_sun)
       gs_shade = gs_sun * 0.5_dp
    end if

    flux%gs_sun = gs_sun
    flux%gs_shade = gs_shade
    rs_sun = canopy_resistance(gs_sun, max(lai_s, 0.01_dp))
    rs_shade = canopy_resistance(gs_shade, max(lai_h, 0.01_dp))

    le_sun = leaf_latent(tc, qa, force%PA, ra, rs_sun, wstress)
    le_shade = leaf_latent(tc, qa, force%PA, ra, rs_shade, wstress)
    le_canopy = le_sun * lai_s + le_shade * lai_h

    flux%gs = (gs_sun * lai_s + gs_shade * lai_h) / max(state%LAI, 0.01_dp)
    flux%rs = canopy_resistance(flux%gs, state%LAI)
    rs_canopy = flux%rs
    flux%LE_canopy = le_canopy
    flux%LE = le_canopy + flux%LE_soil
    flux%ET = flux%LE / lambda
  end subroutine canopy_fluxes

  subroutine leaf_coupling_tl(force, state, par, cfg, par_leaf, lai_frac, tl, gs, an, ci)
    type(t_forcing), intent(in)  :: force
    type(t_state),   intent(in)  :: state
    type(t_param),   intent(in)  :: par
    type(t_config),  intent(in)  :: cfg
    real(dp), intent(in)  :: par_leaf, lai_frac, tl
    real(dp), intent(out) :: gs, an, ci
    type(t_flux) :: ftmp
    real(dp) :: gs_old, an_old
    integer :: k

    if (lai_frac < 0.01_dp) then
       gs = 1.0e-6_dp
       an = 0.0_dp
       ci = force%CO2 * 0.7_dp
       return
    end if

    ftmp%Ci = force%CO2 * 0.7_dp
    gs_old = -1.0_dp
    an_old = -1.0_dp
    do k = 1, stomatal_inner_max
       call photosynthesis_leaf_tl(force, state, par, cfg, par_leaf, tl, ftmp)
       an = ftmp%An
       ci = ftmp%Ci
       call stomatal_conductance(force, state, par, cfg, an, gs)
       ftmp%Ci = force%CO2 - an * 1.6_dp / max(gs, 1.0e-6_dp) * force%PA &
               / (8.314_dp * max(tl, 200.0_dp))
       ftmp%Ci = max(ftmp%Ci, 10.0_dp)
       if (k > 1) then
          if (abs(gs - gs_old) < stomatal_gs_tol .and. abs(an - an_old) < 0.01_dp) exit
       end if
       gs_old = gs
       an_old = an
    end do
  end subroutine leaf_coupling_tl

  subroutine photosynthesis_leaf(force, state, par, cfg, par_abs, flux)
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(in)    :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    real(dp), intent(in)    :: par_abs
    type(t_flux), intent(inout) :: flux

    call photosynthesis_leaf_tl(force, state, par, cfg, par_abs, force%Ta, flux)
  end subroutine photosynthesis_leaf

  subroutine photosynthesis_leaf_tl(force, state, par, cfg, par_abs, tl, flux)
    use mod_acclimation, only: vcmax_at_temp, jmax_at_temp, rd_at_temp
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(in)    :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    real(dp), intent(in)    :: par_abs, tl
    type(t_flux), intent(inout) :: flux
    real(dp) :: vcmax, jmax, rd, j, wc, wj, a, stress, nu_p
    real(dp), parameter :: o2 = 210000.0_dp
    real(dp), parameter :: kc = 404.9_dp
    real(dp), parameter :: ko = 278400.0_dp
    real(dp), parameter :: gamma_star = 42.75_dp

    stress = water_stress_factor(cfg%ihydro, state)
    nu_p = state%nu_stress * state%np_stress
    vcmax = vcmax_at_temp(state%Vcmax25, tl) * stress * nu_p
    jmax  = jmax_at_temp(par%Jmax, tl) * stress * nu_p
    rd    = rd_at_temp(state%Rd25, tl)
    j = jmax * par_abs / (par_abs + 150.0_dp)
    wc = vcmax * max(flux%Ci - gamma_star, 0.0_dp) / (flux%Ci + kc * (1.0_dp + o2 / ko))
    wj = j * max(flux%Ci - gamma_star, 0.0_dp) / (flux%Ci + 2.0_dp * gamma_star)
    a  = min(wc, wj) - rd
    flux%An = max(a, 0.0_dp)
  end subroutine photosynthesis_leaf_tl

  function leaf_latent(tl, qa, pa, ra, rs, wstress) result(le)
    use mod_radiation, only: qs_from_T_P
    real(dp), intent(in) :: tl, qa, pa, ra, rs, wstress
    real(dp) :: le, qs, dq
    qs = qs_from_T_P(tl, pa)
    dq = max(qs - qa, 0.0_dp)
    le = rho * lambda * wstress * dq / (ra + rs)
  end function leaf_latent

  subroutine canopy_photosynthesis(force, state, par, cfg, flux)
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(in)    :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    type(t_flux),    intent(inout) :: flux
    type(t_flux) :: ftmp
    real(dp) :: an_s, an_h

    if (cfg%icanopy /= CANOPY_TWOLEAF) return

    ftmp%Ci = force%CO2 * 0.7_dp
    call photosynthesis_leaf_tl(force, state, par, cfg, flux%PAR_sun, state%Tc, ftmp)
    an_s = ftmp%An
    flux%GPP_sun = an_s * state%LAI_sun

    ftmp%Ci = force%CO2 * 0.7_dp
    call photosynthesis_leaf_tl(force, state, par, cfg, flux%PAR_shade, state%Tc, ftmp)
    an_h = ftmp%An
    flux%GPP_shade = an_h * state%LAI_shade

    flux%GPP = flux%GPP_sun + flux%GPP_shade
    flux%An = flux%GPP / max(state%LAI, 0.01_dp)
  end subroutine canopy_photosynthesis

end module mod_canopy