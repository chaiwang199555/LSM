! Physics scheme identifiers (Noah-MP style switches)
module mod_physics
  use mod_kinds, only: dp
  implicit none

  integer, parameter :: STOM_JARVIS = 1   ! Jarvis empirical stomatal conductance
  integer, parameter :: STOM_MEDLYN  = 2  ! Medlyn semi-mechanistic stomatal conductance

  integer, parameter :: HYDRO_BETA = 1    ! empirical beta soil moisture stress
  integer, parameter :: HYDRO_PHS  = 2    ! plant hydraulics (PHS)

  integer, parameter :: SWATER_BUCKET  = 1    ! single-layer bucket soil water
  integer, parameter :: SWATER_RICHARDS = 2   ! multi-layer Richards equation

  integer, parameter :: SCARB_Q10      = 1    ! Q10 soil respiration
  integer, parameter :: SCARB_MICROBIAL = 2   ! explicit microbial soil carbon

  integer, parameter :: CNP_OFF = 0   ! nutrient cycling off
  integer, parameter :: CNP_C   = 1   ! carbon only
  integer, parameter :: CNP_CN  = 2   ! carbon-nitrogen coupling
  integer, parameter :: CNP_CNP = 3   ! carbon-nitrogen-phosphorus coupling

  integer, parameter :: CANOPY_BIGLEAF    = 1
  integer, parameter :: CANOPY_TWOLEAF    = 2
  integer, parameter :: CANOPY_MULTILAYER = 3

  integer, parameter :: SNOW_OFF = 0
  integer, parameter :: SNOW_ON  = 1

  integer, parameter :: FROST_OFF = 0
  integer, parameter :: FROST_ON  = 1

  integer, parameter :: RAD_OFFLINE   = 0
  integer, parameter :: RAD_TWOSTREAM = 1
  integer, parameter :: RAD_GCM_CPL   = 2

end module mod_physics