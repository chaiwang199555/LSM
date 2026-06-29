! EEO-style acclimation of Vcmax25 and Rd25 (Ren et al. 2025 simplified)
module mod_acclimation
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_forcing, t_state, t_param, t_config
  implicit none

contains

  subroutine update_acclimation(force, state, par, cfg, dt)
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    real(dp), intent(in) :: dt
    real(dp) :: tau      ! acclimation time constant (s)
    real(dp) :: w        ! exponential smoothing weight
    real(dp) :: t_opt    ! optimal temperature (K)
    real(dp) :: vc_fac   ! Vcmax acclimation factor
    real(dp) :: rd_fac   ! Rd acclimation factor

    if (.not. cfg%eeo_on) return

    tau = cfg%accl_days * 86400.0_dp
    w = dt / (tau + dt)
    state%T_accl = (1.0_dp - w) * state%T_accl + w * force%Ta
    state%n_accl = state%n_accl + dt / 86400.0_dp

    t_opt = tfrz + 25.0_dp
    vc_fac = exp(0.04_dp * (state%T_accl - t_opt))
    vc_fac = max(min(vc_fac, 2.0_dp), 0.5_dp)
    rd_fac = exp(0.06_dp * (state%T_accl - t_opt))
    rd_fac = max(min(rd_fac, 2.5_dp), 0.4_dp)

    state%Vcmax25 = par%Vcmax * vc_fac
    state%Rd25    = par%Rd25_base * rd_fac
  end subroutine update_acclimation

  function vcmax_at_temp(vc25, tk) result(vc)
    real(dp), intent(in) :: vc25, tk
    real(dp) :: vc
    real(dp) :: ha        ! activation enthalpy (J/mol)
    real(dp) :: hd        ! deactivation enthalpy (J/mol)
    real(dp) :: sv        ! entropy parameter (J/mol/K)

    ha = 65330.0_dp
    hd = 149250.0_dp
    sv = 485.0_dp
    vc = vc25 * exp(ha * (tk - (tfrz + 25.0_dp)) / (tk * (tfrz + 25.0_dp) * 8.314_dp)) &
       / (1.0_dp + exp((sv * tk - hd) / (8.314_dp * tk)))
    vc = max(vc, 1.0_dp)
  end function vcmax_at_temp

  function jmax_at_temp(j25, tk) result(j)
    real(dp), intent(in) :: j25, tk
    real(dp) :: j

    j = j25 * exp(37000.0_dp * (tk - (tfrz + 25.0_dp)) / (tk * (tfrz + 25.0_dp) * 8.314_dp))
    j = max(j, 1.0_dp)
  end function jmax_at_temp

  function rd_at_temp(rd25, tk) result(rd)
    real(dp), intent(in) :: rd25, tk
    real(dp) :: rd

    rd = rd25 * 2.0_dp**((tk - (tfrz + 25.0_dp)) / 10.0_dp)
    rd = max(rd, 0.1_dp)
  end function rd_at_temp

end module mod_acclimation