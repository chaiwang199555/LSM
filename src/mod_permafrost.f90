! Soil ice phase change and unfrozen water (MVP permafrost)
module mod_permafrost
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_state, t_param, t_config
  use mod_physics, only: FROST_OFF, FROST_ON
  implicit none

  real(dp), parameter :: lf_ice = 3.34e5_dp  ! latent heat fusion (J/kg)
  real(dp), parameter :: rho_w  = 1000.0_dp  ! water density (kg/m3)

contains

  subroutine init_permafrost(state, par, cfg)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    integer :: i, n

    if (cfg%ifrost == FROST_OFF) return
    n = size(state%theta)
    if (allocated(state%theta_ice)) deallocate(state%theta_ice)
    allocate(state%theta_ice(n))
    do i = 1, n
       if (state%Tsoil(i) < tfrz) then
          state%theta_ice(i) = min(par%soil_sat_ice, state%theta(i) * 0.5_dp)
       else
          state%theta_ice(i) = 0.0_dp
       end if
    end do
  end subroutine init_permafrost

  function unfrozen_water(tk, theta_total, par) result(theta_liq)
    real(dp), intent(in) :: tk, theta_total
    type(t_param), intent(in) :: par
    real(dp) :: theta_liq, frac
    if (tk >= tfrz) then
       theta_liq = theta_total
    else
       frac = max(0.0_dp, (tk - (tfrz - 5.0_dp)) / 5.0_dp)
       theta_liq = theta_total * frac
       theta_liq = max(theta_liq, 0.02_dp)
    end if
    theta_liq = min(theta_liq, theta_total)
  end function unfrozen_water

  subroutine update_phase_change(state, par, cfg, dt)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    real(dp), intent(in) :: dt
    integer :: i, n
    real(dp) :: theta_liq, theta_ice_new, d_ice
    real(dp) :: cap, dtemp, heat_avail

    if (cfg%ifrost == FROST_OFF) return
    n = size(state%Tsoil)

    do i = 1, n
       theta_liq = unfrozen_water(state%Tsoil(i), state%theta(i), par)
       theta_ice_new = max(state%theta(i) - theta_liq, 0.0_dp)
       theta_ice_new = min(theta_ice_new, par%soil_sat_ice)
       d_ice = theta_ice_new - state%theta_ice(i)

       if (abs(d_ice) > 1.0e-6_dp) then
          cap = par%soil_heat_cap * state%dz(i)
          heat_avail = -rho_w * lf_ice * d_ice * state%dz(i) / dt
          dtemp = heat_avail / max(cap, 1.0_dp)
          state%Tsoil(i) = state%Tsoil(i) + dtemp * 0.1_dp
       end if
       state%theta_ice(i) = theta_ice_new
    end do
  end subroutine update_phase_change

  function ice_impedance(theta_ice, par) result(fice)
    real(dp), intent(in) :: theta_ice
    type(t_param), intent(in) :: par
    real(dp) :: fice
    fice = max(0.0_dp, 1.0_dp - theta_ice / max(par%soil_sat_ice, 1.0e-6_dp))
    fice = max(fice, 0.05_dp)
  end function ice_impedance

end module mod_permafrost