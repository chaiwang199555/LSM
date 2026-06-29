! Canopy radiation: Noah-MP two-stream SW + Noah-MP LW exchange
module mod_radtran
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_forcing, t_state, t_param, t_config, t_flux
  use mod_physics, only: RAD_OFFLINE, RAD_TWOSTREAM, RAD_GCM_CPL
  use mod_radiation, only: net_radiation, infer_sw_geometry
  implicit none

  integer, parameter :: n_sw_band = 2
  real(dp), parameter :: sw_vis_frac = 0.48_dp
  real(dp), parameter :: min_thr = 1.0e-6_dp
  real(dp), parameter :: rho_leaf_tab(n_sw_band) = (/0.11_dp, 0.35_dp/)
  real(dp), parameter :: tau_leaf_tab(n_sw_band) = (/0.08_dp, 0.35_dp/)

contains

  subroutine noah_lw_coeff_canopy(emiss_can, emiss_surf, lw_down, ts_grd, lw_air, lw_can)
    real(dp), intent(in)  :: emiss_can, emiss_surf, lw_down, ts_grd
    real(dp), intent(out) :: lw_air, lw_can

    lw_air = -emiss_can * (1.0_dp + (1.0_dp - emiss_can) * (1.0_dp - emiss_surf)) * lw_down &
           - emiss_can * emiss_surf * sigma * ts_grd**4
    lw_can = (2.0_dp - emiss_can * (1.0_dp - emiss_surf)) * emiss_can * sigma
  end subroutine noah_lw_coeff_canopy

  subroutine noah_lw_coeff_ground(emiss_can, emiss_surf, lw_down, tc_can, lw_air, lw_can)
    real(dp), intent(in)  :: emiss_can, emiss_surf, lw_down, tc_can
    real(dp), intent(out) :: lw_air, lw_can

    lw_air = -emiss_surf * (1.0_dp - emiss_can) * lw_down - emiss_surf * emiss_can * sigma * tc_can**4
    lw_can = emiss_surf * sigma
  end subroutine noah_lw_coeff_ground

  function noah_rad_lw_net_outward(lw_air, lw_can, temp) result(rad_lw)
    real(dp), intent(in) :: lw_air, lw_can, temp
    real(dp) :: rad_lw

    rad_lw = lw_air + lw_can * temp**4
  end function noah_rad_lw_net_outward

  function noah_rn_layer(sw_abs, lw_air, lw_can, temp) result(rn)
    real(dp), intent(in) :: sw_abs, lw_air, lw_can, temp
    real(dp) :: rn

    rn = sw_abs - noah_rad_lw_net_outward(lw_air, lw_can, temp)
  end function noah_rn_layer

  function noah_drn_dtemp(lw_can_coeff, temp) result(drn)
    real(dp), intent(in) :: lw_can_coeff, temp
    real(dp) :: drn

    drn = -4.0_dp * lw_can_coeff * temp**3
  end function noah_drn_dtemp

  subroutine surface_emissivities(state, par, emiss_surf, emiss_can)
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    real(dp), intent(out) :: emiss_surf, emiss_can

    emiss_can = par%emiss
    if (state%snow_present) then
       emiss_surf = par%snow_emiss
    else
       emiss_surf = par%emiss
    end if
  end subroutine surface_emissivities

  subroutine noah_twostream_unit(cos_sza, orient_idx, vai, rho_leaf, tau_leaf, alb_grd, &
       is_diffuse, gap_dir, gap_dif, abs_veg, alb_sfc, tran_dir_grd, tran_dif_grd, veg_proj_dir)
    real(dp), intent(in)  :: cos_sza, orient_idx, vai, rho_leaf, tau_leaf, alb_grd
    integer, intent(in)   :: is_diffuse
    real(dp), intent(in)  :: gap_dir, gap_dif
    real(dp), intent(out) :: abs_veg, alb_sfc, tran_dir_grd, tran_dif_grd, veg_proj_dir
    real(dp) :: cos_sza_tmp, orient_tmp, phi1, phi2, optic_dir, optic_dif
    real(dp) :: scat_leaf, scat_can, upscat_dir, upscat_dif, single_scat_alb
    real(dp) :: tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9
    real(dp) :: b, c, d, f, h, sigma_ts, p1, p2, p3, p4, s1, s2
    real(dp) :: u1, u2, u3, d1, d2, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10
    real(dp) :: rad_refl_can, rad_refl_grd, rad_trans_dir, rad_trans_dif
    real(dp) :: gap_use, vai_eff

    abs_veg = 0.0_dp
    alb_sfc = alb_grd
    tran_dir_grd = 1.0_dp
    tran_dif_grd = 1.0_dp
    veg_proj_dir = 0.0_dp
    if (vai < 0.01_dp) return

    vai_eff = min(6.0_dp, vai)
    cos_sza_tmp = max(0.001_dp, cos_sza)
    orient_tmp = min(max(orient_idx, -0.4_dp), 0.6_dp)
    if (abs(orient_tmp) <= 0.01_dp) orient_tmp = 0.01_dp

    phi1 = 0.5_dp - 0.633_dp * orient_tmp - 0.330_dp * orient_tmp**2
    phi2 = 0.877_dp * (1.0_dp - 2.0_dp * phi1)
    veg_proj_dir = phi1 + phi2 * cos_sza_tmp
    optic_dir = veg_proj_dir / cos_sza_tmp
    optic_dif = (1.0_dp - phi1 / phi2 * log((phi1 + phi2) / phi1)) / phi2

    scat_leaf = rho_leaf + tau_leaf
    tmp0 = veg_proj_dir + phi2 * cos_sza_tmp
    tmp1 = phi1 * cos_sza_tmp
    single_scat_alb = 0.5_dp * scat_leaf * veg_proj_dir / tmp0 &
                    * (1.0_dp - tmp1 / tmp0 * log((tmp1 + tmp0) / tmp1))
    upscat_dir = (1.0_dp + optic_dif * optic_dir) / (scat_leaf * optic_dif * optic_dir) * single_scat_alb
    upscat_dif = 0.5_dp * (rho_leaf + tau_leaf + (rho_leaf - tau_leaf) * ((1.0_dp + orient_tmp) / 2.0_dp)**2) &
               / scat_leaf

    scat_can = scat_leaf

    b = 1.0_dp - scat_can + scat_can * upscat_dif
    c = scat_can * upscat_dif
    tmp0 = optic_dif * optic_dir
    d = tmp0 * scat_can * upscat_dir
    f = tmp0 * scat_can * (1.0_dp - upscat_dir)
    tmp1 = b * b - c * c
    h = sqrt(tmp1) / optic_dif
    sigma_ts = tmp0 * tmp0 - tmp1
    if (abs(sigma_ts) < 1.0e-6_dp) sigma_ts = sign(1.0e-6_dp, sigma_ts)
    p1 = b + optic_dif * h
    p2 = b - optic_dif * h
    p3 = b + tmp0
    p4 = b - tmp0
    s1 = exp(-h * vai_eff)
    s2 = exp(-optic_dir * vai_eff)

    if (is_diffuse == 0) then
       u1 = b - c / alb_grd
       u2 = b - c * alb_grd
       u3 = f + c * alb_grd
    else
       u1 = b - c / alb_grd
       u2 = b - c * alb_grd
       u3 = f + c * alb_grd
    end if

    tmp2 = u1 - optic_dif * h
    tmp3 = u1 + optic_dif * h
    d1 = p1 * tmp2 / s1 - p2 * tmp3 * s1
    tmp4 = u2 + optic_dif * h
    tmp5 = u2 - optic_dif * h
    d2 = tmp4 / s1 - tmp5 * s1
    h1 = -d * p4 - c * f
    tmp6 = d - h1 * p3 / sigma_ts
    tmp7 = (d - c - h1 / sigma_ts * (u1 + tmp0)) * s2
    h2 = (tmp6 * tmp2 / s1 - p2 * tmp7) / d1
    h3 = -(tmp6 * tmp3 * s1 - p1 * tmp7) / d1
    h4 = -f * p3 - c * d
    tmp8 = h4 / sigma_ts
    tmp9 = (u3 - tmp8 * (u2 - tmp0)) * s2
    h5 = -(tmp8 * tmp4 / s1 + tmp9) / d2
    h6 = (tmp8 * tmp5 * s1 + tmp9) / d2
    h7 = (c * tmp2) / (d1 * s1)
    h8 = (-c * tmp3 * s1) / d1
    h9 = tmp4 / (d2 * s1)
    h10 = (-tmp5 * s1) / d2

    if (is_diffuse == 0) then
       gap_use = gap_dir
       rad_trans_dir = s2 * (1.0_dp - gap_use) + gap_use
       rad_trans_dif = (h4 * s2 / sigma_ts + h5 * s1 + h6 / s1) * (1.0_dp - gap_use)
       rad_refl_can = (h1 / sigma_ts + h2 + h3) * (1.0_dp - gap_use)
       rad_refl_grd = alb_grd * gap_use
       alb_sfc = rad_refl_can + rad_refl_grd
       abs_veg = 1.0_dp - alb_sfc - (1.0_dp - alb_grd) * rad_trans_dir - (1.0_dp - alb_grd) * rad_trans_dif
       tran_dir_grd = rad_trans_dir
       tran_dif_grd = rad_trans_dif
    else
       gap_use = gap_dif
       rad_trans_dir = 0.0_dp
       rad_trans_dif = (h9 * s1 + h10 / s1) * (1.0_dp - gap_use) + gap_use
       rad_refl_can = (h7 + h8) * (1.0_dp - gap_use)
       alb_sfc = rad_refl_can + alb_grd * gap_use
       abs_veg = 1.0_dp - alb_sfc - (1.0_dp - alb_grd) * rad_trans_dir - (1.0_dp - alb_grd) * rad_trans_dif
       tran_dir_grd = rad_trans_dir
       tran_dif_grd = rad_trans_dif
    end if

    abs_veg = max(abs_veg, 0.0_dp)
    alb_sfc = max(min(alb_sfc, 0.99_dp), 0.0_dp)
  end subroutine noah_twostream_unit

  subroutine noah_sw_fluxes(sw_total, cos_sza, sw_beam_frac, vai, orient_idx, alb_grd, &
       sw_abs_can, sw_abs_grd, sw_trans_grd, alb_sfc, &
       abs_veg_dir, abs_veg_dif, veg_proj_dir, sunlit_frac)
    real(dp), intent(in)  :: sw_total, cos_sza, sw_beam_frac, vai, orient_idx, alb_grd
    real(dp), intent(out) :: sw_abs_can, sw_abs_grd, sw_trans_grd, alb_sfc
    real(dp), intent(out) :: abs_veg_dir(n_sw_band), abs_veg_dif(n_sw_band), veg_proj_dir
    real(dp), intent(out) :: sunlit_frac
    integer :: ib
    real(dp) :: sw_dir, sw_dif, sw_band(n_sw_band)
    real(dp) :: frac_dir_grd(n_sw_band), frac_dif_grd_dir(n_sw_band), frac_dif_grd_dif(n_sw_band)
    real(dp) :: alb_dir(n_sw_band), alb_dif(n_sw_band), tran_grd_dir, tran_grd_dif
    real(dp) :: abs_grd_band, light_ext_dir, rho_vis, tau_vis, refl_band
    real(dp) :: gap_dir, gap_dif

    sw_abs_can = 0.0_dp
    sw_abs_grd = 0.0_dp
    sw_trans_grd = 0.0_dp
    alb_sfc = 0.0_dp
    abs_veg_dir = 0.0_dp
    abs_veg_dif = 0.0_dp
    veg_proj_dir = 0.0_dp
    sunlit_frac = 0.0_dp

    if (vai < 0.01_dp .or. sw_total < 1.0e-8_dp) then
       alb_sfc = alb_grd
       sw_abs_grd = sw_total * (1.0_dp - alb_grd)
       sw_trans_grd = sw_total
       return
    end if

    sw_dir = sw_total * sw_beam_frac
    sw_dif = sw_total - sw_dir
    sw_band(1) = sw_vis_frac
    sw_band(2) = 1.0_dp - sw_vis_frac
    gap_dir = 0.0_dp
    gap_dif = 0.0_dp

    do ib = 1, n_sw_band
       call noah_twostream_unit(cos_sza, orient_idx, vai, rho_leaf_tab(ib), tau_leaf_tab(ib), alb_grd, &
            0, gap_dir, gap_dif, abs_veg_dir(ib), alb_dir(ib), frac_dir_grd(ib), frac_dif_grd_dir(ib), veg_proj_dir)
       call noah_twostream_unit(cos_sza, orient_idx, vai, rho_leaf_tab(ib), tau_leaf_tab(ib), alb_grd, &
            1, gap_dir, gap_dif, abs_veg_dif(ib), alb_dif(ib), tran_grd_dir, frac_dif_grd_dif(ib), veg_proj_dir)
       sw_abs_can = sw_abs_can + sw_band(ib) * (sw_dir * abs_veg_dir(ib) + sw_dif * abs_veg_dif(ib))
       tran_grd_dir = sw_band(ib) * sw_dir * frac_dir_grd(ib)
       tran_grd_dif = sw_band(ib) * (sw_dir * frac_dif_grd_dir(ib) + sw_dif * frac_dif_grd_dif(ib))
       sw_trans_grd = sw_trans_grd + tran_grd_dir + tran_grd_dif
       abs_grd_band = tran_grd_dir * (1.0_dp - alb_grd) + tran_grd_dif * (1.0_dp - alb_grd)
       sw_abs_grd = sw_abs_grd + abs_grd_band
       refl_band = sw_band(ib) * (alb_dir(ib) * sw_dir + alb_dif(ib) * sw_dif)
       alb_sfc = alb_sfc + refl_band / max(sw_total, min_thr)
    end do

    rho_vis = rho_leaf_tab(1)
    tau_vis = tau_leaf_tab(1)
    light_ext_dir = veg_proj_dir / max(cos_sza, 0.001_dp) * sqrt(max(1.0_dp - rho_vis - tau_vis, 0.0_dp))
    sunlit_frac = (1.0_dp - exp(-light_ext_dir * min(6.0_dp, vai))) / max(light_ext_dir * min(6.0_dp, vai), min_thr)
    if (sunlit_frac < 0.01_dp) sunlit_frac = 0.0_dp
    sunlit_frac = min(max(sunlit_frac, 0.0_dp), 1.0_dp)
  end subroutine noah_sw_fluxes

  subroutine partition_par(abs_veg_dir, abs_veg_dif, sw_total, sw_beam_frac, cos_sza, &
       sunlit_frac, lai, par_sun, par_shade, lai_sun, lai_shade)
    real(dp), intent(in)  :: abs_veg_dir(n_sw_band), abs_veg_dif(n_sw_band)
    real(dp), intent(in)  :: sw_total, sw_beam_frac, cos_sza, sunlit_frac, lai
    real(dp), intent(out) :: par_sun, par_shade, lai_sun, lai_shade
    real(dp) :: sw_dir, sw_dif, sw_vis, abs_par_dir, abs_par_dif, shade_frac, leaf_frac

    sw_dir = sw_total * sw_beam_frac
    sw_dif = sw_total - sw_dir
    sw_vis = sw_total * sw_vis_frac
    abs_par_dir = sw_vis_frac * abs_veg_dir(1)
    abs_par_dif = sw_vis_frac * abs_veg_dif(1)
    shade_frac = 1.0_dp - sunlit_frac
    lai_sun = lai * sunlit_frac
    lai_shade = lai * shade_frac
    leaf_frac = 1.0_dp

    if (sunlit_frac > 0.0_dp .and. lai_sun > 0.01_dp) then
       par_sun = (sw_dir * abs_par_dir + sunlit_frac * sw_dif * abs_par_dif) * leaf_frac / lai_sun
    else
       par_sun = 0.0_dp
    end if

    if (lai_shade > 0.01_dp) then
       if (sunlit_frac > 0.0_dp) then
          par_shade = (shade_frac * sw_dif * abs_par_dif) * leaf_frac / lai_shade
       else
          par_shade = (sw_dir * abs_par_dir + sw_dif * abs_par_dif) * leaf_frac / lai_shade
       end if
    else
       par_shade = 0.0_dp
    end if

    if (cos_sza <= 0.001_dp) then
       par_sun = 0.0_dp
       par_shade = 0.0_dp
    end if
  end subroutine partition_par

  subroutine compute_radiation(force, state, par, cfg, flux, ts_surf, tc_canopy)
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    type(t_flux),    intent(inout) :: flux
    real(dp), intent(in) :: ts_surf, tc_canopy
    real(dp) :: alb_surf, emiss_surf, emiss_can
    real(dp) :: lai_eff, cos_sza, sw_beam_frac, orient_idx
    real(dp) :: lw_air_c, lw_can_c, lw_air_g, lw_can_g
    real(dp) :: abs_veg_dir(n_sw_band), abs_veg_dif(n_sw_band), veg_proj_dir, sunlit_frac
    real(dp) :: sw_abs_grd

    call surface_emissivities(state, par, emiss_surf, emiss_can)

    if (state%snow_present) then
       alb_surf = state%snow_albedo
    else
       alb_surf = par%albedo
    end if

    if (cfg%irad <= RAD_OFFLINE) then
       flux%SW_abs_ground = force%SW * (1.0_dp - alb_surf)
       flux%SW_trans_ground = force%SW
       flux%SW_abs_canopy = 0.0_dp
       flux%PAR_sun = force%SW * 0.5_dp
       flux%PAR_shade = 0.0_dp
       flux%Rn_ground = net_radiation(force%SW, force%LW, alb_surf, emiss_surf, ts_surf)
       flux%Rn_canopy = 0.0_dp
       flux%Rn = flux%Rn_ground
       flux%albedo_eff = alb_surf
       state%LAI_sun = state%LAI
       state%LAI_shade = 0.0_dp
       return
    end if

    lai_eff = min(6.0_dp, state%LAI * par%clumping)
    orient_idx = 0.0_dp
    if (force%cos_sza > miss + 1.0_dp .and. force%sw_beam_frac >= 0.0_dp &
         .and. force%sw_beam_frac <= 1.0_dp) then
       cos_sza = max(0.001_dp, min(1.0_dp, force%cos_sza))
       sw_beam_frac = max(0.0_dp, min(1.0_dp, force%sw_beam_frac))
    else
       call infer_sw_geometry(force%SW, cos_sza, sw_beam_frac)
    end if

    if (lai_eff > 0.01_dp) then
       call noah_sw_fluxes(force%SW, cos_sza, sw_beam_frac, lai_eff, orient_idx, alb_surf, &
            flux%SW_abs_canopy, sw_abs_grd, flux%SW_trans_ground, flux%albedo_eff, &
            abs_veg_dir, abs_veg_dif, veg_proj_dir, sunlit_frac)
       flux%SW_abs_ground = sw_abs_grd
       flux%SW_trans_ground = max(flux%SW_trans_ground, 0.0_dp)
       call partition_par(abs_veg_dir, abs_veg_dif, force%SW, sw_beam_frac, cos_sza, &
            sunlit_frac, state%LAI, flux%PAR_sun, flux%PAR_shade, &
            state%LAI_sun, state%LAI_shade)

       call noah_lw_coeff_canopy(emiss_can, emiss_surf, force%LW, ts_surf, lw_air_c, lw_can_c)
       call noah_lw_coeff_ground(emiss_can, emiss_surf, force%LW, tc_canopy, lw_air_g, lw_can_g)
       flux%Rn_canopy = noah_rn_layer(flux%SW_abs_canopy, lw_air_c, lw_can_c, tc_canopy)
       flux%Rn_ground = noah_rn_layer(flux%SW_abs_ground, lw_air_g, lw_can_g, ts_surf)
    else
       flux%SW_abs_canopy = 0.0_dp
       flux%SW_abs_ground = force%SW * (1.0_dp - alb_surf)
       flux%SW_trans_ground = force%SW
       flux%albedo_eff = alb_surf
       flux%PAR_sun = 0.0_dp
       flux%PAR_shade = 0.0_dp
       state%LAI_sun = 0.0_dp
       state%LAI_shade = state%LAI
       flux%Rn_canopy = 0.0_dp
       flux%Rn_ground = noah_rn_layer(force%SW * (1.0_dp - alb_surf), -emiss_surf * force%LW, emiss_surf * sigma, ts_surf)
    end if

    flux%Rn = flux%Rn_canopy + flux%Rn_ground
    flux%albedo_eff = min(max(flux%albedo_eff, 0.0_dp), 0.99_dp)
  end subroutine compute_radiation

  function dRn_dTs_rad(state, par, cfg, ts) result(drn)
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    type(t_config), intent(in) :: cfg
    real(dp), intent(in) :: ts
    real(dp) :: drn, emiss_surf, emiss_can, lw_can_g

    call surface_emissivities(state, par, emiss_surf, emiss_can)
    lw_can_g = emiss_surf * sigma
    drn = noah_drn_dtemp(lw_can_g, ts)
  end function dRn_dTs_rad

  function dRn_dTc_canopy(state, par, cfg, tc, ts_surf) result(drn)
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    type(t_config), intent(in) :: cfg
    real(dp), intent(in) :: tc, ts_surf
    real(dp) :: drn, emiss_surf, emiss_can, lw_air_c, lw_can_c

    if (cfg%irad <= RAD_OFFLINE .or. state%LAI * par%clumping < 0.01_dp) then
       drn = 0.0_dp
       return
    end if

    call surface_emissivities(state, par, emiss_surf, emiss_can)
    call noah_lw_coeff_canopy(emiss_can, emiss_surf, 0.0_dp, ts_surf, lw_air_c, lw_can_c)
    drn = noah_drn_dtemp(lw_can_c, tc)
  end function dRn_dTc_canopy

end module mod_radtran