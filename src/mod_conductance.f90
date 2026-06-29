! Stomatal conductance: Jarvis / Medlyn switches
module mod_conductance
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_forcing, t_state, t_param, t_config
  use mod_physics, only: STOM_JARVIS, STOM_MEDLYN
  implicit none

contains

  subroutine stomatal_conductance(force, state, par, cfg, an, gs)
    type(t_forcing), intent(in)  :: force
    type(t_state),   intent(in)  :: state
    type(t_param),   intent(in)  :: par
    type(t_config),  intent(in)  :: cfg
    real(dp), intent(in)  :: an
    real(dp), intent(out) :: gs

    if (cfg%istomatal == STOM_MEDLYN) then
       gs = medlyn_conductance(force, par, an)
    else
       gs = jarvis_conductance(force, state, par)
    end if
    gs = max(gs, 1.0e-6_dp)
  end subroutine stomatal_conductance

  function jarvis_conductance(force, state, par) result(gs)
    type(t_forcing), intent(in) :: force
    type(t_state),   intent(in) :: state
    type(t_param),   intent(in) :: par
    real(dp) :: gs
    real(dp) :: f_light  ! light response factor
    real(dp) :: f_temp    ! temperature response factor
    real(dp) :: f_vpd     ! VPD response factor
    real(dp) :: f_soil    ! soil/hydraulic stress factor

    f_light = max(force%SW / (force%SW + 200.0_dp), 0.05_dp)
    f_temp = max(min(1.0_dp - 0.0016_dp * (force%Ta - (tfrz + 25.0_dp))**2, 1.0_dp), 0.0_dp)
    f_vpd = exp(-0.001_dp * force%VPD)
    f_soil = max(state%beta, state%stress_hydro)
    gs = par%gs_max * f_light * f_temp * f_vpd * f_soil
  end function jarvis_conductance

  function medlyn_conductance(force, par, an) result(gs)
    type(t_forcing), intent(in) :: force
    type(t_param),   intent(in) :: par
    real(dp), intent(in) :: an
    real(dp) :: gs
    real(dp) :: cs        ! leaf-surface CO2 concentration (mol/m3)
    real(dp) :: vpd_kpa   ! VPD (kPa)

    cs = force%CO2 * 1.0e-6_dp * force%PA / (8.314_dp * force%Ta)
    vpd_kpa = max(force%VPD * 1.0e-3_dp, 0.05_dp)
    gs = par%g0 + 1.6_dp * (1.0_dp + par%g1 / sqrt(vpd_kpa)) * max(an, 0.0_dp) / max(cs, 1.0e-6_dp)
  end function medlyn_conductance

  function canopy_resistance(gs, lai) result(rs)
    real(dp), intent(in) :: gs, lai
    real(dp) :: rs

    if (lai > 0.01_dp) then
       rs = 1.0_dp / (lai * gs)
    else
       rs = 1.0e6_dp
    end if
  end function canopy_resistance

end module mod_conductance