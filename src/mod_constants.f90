! Physical constants
module mod_constants
  use mod_kinds, only: dp
  implicit none

  real(dp), parameter :: cp    = 1005.0_dp      ! J/(kg K)  air heat capacity
  real(dp), parameter :: rho   = 1.225_dp       ! kg/m^3    air density (sea level)
  real(dp), parameter :: sigma  = 5.67e-8_dp     ! W/(m^2 K^4) Stefan-Boltzmann
  real(dp), parameter :: lambda = 2.45e6_dp      ! J/kg      latent heat of vaporization
  real(dp), parameter :: karman = 0.4_dp         ! von Karman constant
  real(dp), parameter :: grav   = 9.81_dp        ! m/s^2
  real(dp), parameter :: Rd    = 287.0_dp       ! J/(kg K)  dry air gas constant
  real(dp), parameter :: eps   = 0.622_dp       ! ratio Mw/Md
  real(dp), parameter :: tfrz   = 273.15_dp      ! K         freezing point
  real(dp), parameter :: miss   = -9999.0_dp    ! missing value flag

end module mod_constants