! Single-layer snow pack: accumulation, melt, albedo, insulation
module mod_snow
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_state, t_param, t_config, t_flux
  use mod_physics, only: SNOW_OFF, SNOW_ON
  implicit none

  real(dp), parameter :: rho_snow = 250.0_dp   ! snow density (kg/m3)
  real(dp), parameter :: k_snow   = 0.3_dp     ! snow thermal conductivity (W/m/K)
  real(dp), parameter :: lf_ice   = 3.34e5_dp  ! latent heat fusion (J/kg)

contains

  subroutine init_snow(state)
    type(t_state), intent(inout) :: state
    state%snow_swe = 0.0_dp
    state%snow_depth = 0.0_dp
    state%snow_T = tfrz - 5.0_dp
    state%snow_albedo = 0.85_dp
    state%snow_age = 0.0_dp
    state%snow_present = .false.
  end subroutine init_snow

  subroutine partition_precip(ta, precip, par, p_rain, p_snow)
    real(dp), intent(in)  :: ta, precip
    type(t_param), intent(in) :: par
    real(dp), intent(out) :: p_rain, p_snow
    if (ta < par%rain_snow_thresh) then
       p_snow = precip
       p_rain = 0.0_dp
    else
       p_snow = 0.0_dp
       p_rain = precip
    end if
  end subroutine partition_precip

  subroutine update_snow(state, par, cfg, ta, precip, rn, h, g_in, dt, flux)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    real(dp), intent(in) :: ta, precip, rn, h, g_in, dt
    type(t_flux), intent(inout) :: flux
    real(dp) :: p_rain, p_snow, melt_energy, melt_mm
    real(dp) :: age_decay

    if (cfg%isnow == SNOW_OFF) return

    call partition_precip(ta, precip, par, p_rain, p_snow)

    state%snow_swe = state%snow_swe + p_snow
    state%snow_age = state%snow_age + dt / 86400.0_dp

    if (state%snow_swe > 0.1_dp) then
       state%snow_present = .true.
       state%snow_depth = state%snow_swe / 1000.0_dp * rho_snow / 1000.0_dp
       state%snow_depth = max(state%snow_depth, 0.001_dp)
       age_decay = exp(-state%snow_age / 5.0_dp)
       state%snow_albedo = par%fresh_snow_alb * age_decay + 0.55_dp * (1.0_dp - age_decay)
    else
       state%snow_present = .false.
       state%snow_swe = 0.0_dp
       state%snow_depth = 0.0_dp
       state%snow_albedo = par%albedo
       state%snow_age = 0.0_dp
    end if

    melt_energy = rn - h - g_in
    if (melt_energy > 0.0_dp .and. state%snow_present) then
       melt_mm = melt_energy * dt / lf_ice * 1000.0_dp
       melt_mm = min(melt_mm, state%snow_swe)
       state%snow_swe = state%snow_swe - melt_mm
       flux%melt_rate = melt_mm / dt
       if (state%snow_swe < 0.1_dp) then
          state%snow_present = .false.
          state%snow_swe = 0.0_dp
          state%snow_depth = 0.0_dp
       end if
    else
       flux%melt_rate = 0.0_dp
    end if
  end subroutine update_snow

  function snow_ground_flux(snow_T, tsoil1, snow_depth) result(g)
    real(dp), intent(in) :: snow_T, tsoil1, snow_depth
    real(dp) :: g
    g = k_snow * (tsoil1 - snow_T) / max(snow_depth, 0.001_dp)
    g = sign(min(abs(g), 300.0_dp), g)
  end function snow_ground_flux

  function surface_emissivity(state, par) result(em)
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    real(dp) :: em
    if (state%snow_present) then
       em = par%snow_emiss
    else
       em = par%emiss
    end if
  end function surface_emissivity

  function surface_albedo(state, par) result(alb)
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    real(dp) :: alb
    if (state%snow_present) then
       alb = state%snow_albedo
    else
       alb = par%albedo
    end if
  end function surface_albedo

end module mod_snow