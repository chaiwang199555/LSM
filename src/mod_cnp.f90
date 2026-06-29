! Simplified CASA-CNP: C / CN / CNP three-level switch
module mod_cnp
  use mod_kinds,   only: dp
  use mod_types,   only: t_state, t_param, t_config, t_flux
  use mod_physics, only: CNP_OFF, CNP_C, CNP_CN, CNP_CNP
  implicit none

contains

  subroutine init_cnp(state, par)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par

    state%Nlab = 5.0_dp
    state%Plab = 0.5_dp
    state%nu_stress = 1.0_dp
    state%np_stress = 1.0_dp
  end subroutine init_cnp

  subroutine update_cnp(state, par, cfg, flux, dt)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    type(t_flux),    intent(in)    :: flux
    real(dp), intent(in) :: dt
    real(dp) :: n_demand   ! nitrogen demand (gN/m2)
    real(dp) :: p_demand   ! phosphorus demand (gP/m2)
    real(dp) :: n_uptake   ! nitrogen uptake (gN/m2)
    real(dp) :: p_uptake   ! phosphorus uptake (gP/m2)

    if (cfg%icnp <= CNP_C) then
       state%nu_stress = 1.0_dp
       state%np_stress = 1.0_dp
       return
    end if

    n_demand = flux%GPP * 1.0e-6_dp * dt / 86400.0_dp
    n_uptake = min(par%n_uptake_max * dt / 86400.0_dp * state%beta, state%Nlab * 0.1_dp)
    state%Nlab = max(state%Nlab - n_demand + n_uptake, 0.1_dp)
    state%nu_stress = min(1.0_dp, state%Nlab / 5.0_dp)

    if (cfg%icnp >= CNP_CNP) then
       p_demand = flux%GPP * 1.0e-7_dp * dt / 86400.0_dp
       p_uptake = min(par%p_uptake_max * dt / 86400.0_dp * state%beta, state%Plab * 0.1_dp)
       state%Plab = max(state%Plab - p_demand + p_uptake, 0.05_dp)
       state%np_stress = min(1.0_dp, state%Plab / 0.5_dp)
    else
       state%np_stress = 1.0_dp
    end if
  end subroutine update_cnp

end module mod_cnp