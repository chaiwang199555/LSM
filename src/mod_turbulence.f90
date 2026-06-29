! Aerodynamic resistance and Monin-Obukhov stability
module mod_turbulence
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_forcing, t_param
  implicit none

  real(dp), parameter :: zref = 2.0_dp  ! reference height (m)

contains

  subroutine mo_stability(ts, ta, ws, h, par, L, psi_m, psi_h)
    real(dp), intent(in)    :: ts, ta, ws, h
    type(t_param),   intent(in) :: par
    real(dp), intent(out)   :: L      ! Obukhov length (m)
    real(dp), intent(out)   :: psi_m  ! momentum stability correction
    real(dp), intent(out)   :: psi_h  ! heat stability correction
    real(dp) :: ustar        ! friction velocity (m/s)
    real(dp) :: theta_v      ! virtual potential temperature (K)
    real(dp) :: theta_vstar  ! virtual temperature scale (K)
    real(dp) :: zeta         ! stability parameter z/L
    integer :: k

    L = 1.0e6_dp
    psi_m = 0.0_dp
    psi_h = 0.0_dp

    do k = 1, 3
       ustar = max(karman * ws &
            / (log((zref - par%zdisp) / par%z0) - psi_m), 0.05_dp)
       theta_v = ta
       theta_vstar = h / (rho * cp * ustar + 1.0e-8_dp)
       if (abs(theta_vstar) > 1.0e-8_dp) then
          L = -ustar**2 * theta_v / (karman * grav * theta_vstar)
       else
          L = 1.0e6_dp
       end if
       zeta = (zref - par%zdisp) / L
       call stability_funcs(zeta, psi_m, psi_h)
    end do
  end subroutine mo_stability

  subroutine stability_funcs(zeta, psi_m, psi_h)
    real(dp), intent(in)  :: zeta
    real(dp), intent(out) :: psi_m, psi_h
    real(dp) :: x          ! auxiliary variable for unstable case

    if (zeta < 0.0_dp) then
       x = (1.0_dp - 16.0_dp * zeta)**0.25_dp
       psi_m = 2.0_dp * log((1.0_dp + x) / 2.0_dp) &
             + log((1.0_dp + x**2) / 2.0_dp) &
             - 2.0_dp * atan(x) + atan(1.0_dp)
       psi_h = 2.0_dp * log((1.0_dp + (1.0_dp - 16.0_dp * zeta)**0.5_dp) / 2.0_dp)
    else
       psi_m = -5.0_dp * zeta
       psi_h = -5.0_dp * zeta
    end if
  end subroutine stability_funcs

  function aero_resistance(force, par, L) result(ra)
    type(t_forcing), intent(in) :: force
    type(t_param),   intent(in) :: par
    real(dp), intent(in) :: L
    real(dp) :: ra
    real(dp) :: psi_m, psi_h, zeta

    zeta = (zref - par%zdisp) / L
    call stability_funcs(zeta, psi_m, psi_h)

    ra = (log((zref - par%zdisp) / par%z0) - psi_m) &
       * (log((zref - par%zdisp) / par%z0) - psi_h) &
       / (karman**2 * max(force%WS, 0.1_dp))
    ra = max(ra, 1.0_dp)
  end function aero_resistance

  function sensible_heat(ts, ta, ra) result(h)
    real(dp), intent(in) :: ts, ta, ra
    real(dp) :: h

    h = rho * cp * (ts - ta) / ra
  end function sensible_heat

end module mod_turbulence