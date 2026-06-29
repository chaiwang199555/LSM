! Plant hydraulics (simplified PHS, Kennedy et al. 2019 style)
module mod_planthydro
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_state, t_param
  implicit none

  real(dp), parameter :: rho_w = 1000.0_dp  ! liquid water density (kg/m3)

contains

  subroutine init_plant_hydro(state, par)
    type(t_state), intent(inout) :: state
    type(t_param), intent(in)    :: par

    state%psi_leaf  = -0.5_dp
    state%psi_xylem = -0.3_dp
    state%psi_root  = -0.2_dp
    state%k_xylem   = par%kx_max
    state%stress_hydro = 1.0_dp
    state%Vcmax25   = par%Vcmax
    state%Rd25      = par%Rd25_base
    state%T_accl    = tfrz + 25.0_dp
    state%n_accl    = 0.0_dp
  end subroutine init_plant_hydro

  function xylem_conductance(psi, par) result(kx)
    real(dp), intent(in) :: psi
    type(t_param), intent(in) :: par
    real(dp) :: kx
    real(dp) :: vuln      ! embolism vulnerability factor (0-1)

    if (psi >= par%p50_xylem) then
       vuln = 1.0_dp
    else
       vuln = 1.0_dp / (1.0_dp + (abs(psi - par%p50_xylem) / max(abs(par%p50_xylem), 0.1_dp))**par%ck_xylem)
    end if
    kx = par%kx_max * max(vuln, 0.01_dp)
  end function xylem_conductance

  function soil_psi_avg(state) result(psi_avg)
    type(t_state), intent(in) :: state
    real(dp) :: psi_avg    ! root-weighted mean soil water potential (MPa)
    integer :: i, n

    if (.not. allocated(state%psi_soil)) then
       psi_avg = -0.2_dp
       return
    end if
    n = size(state%psi_soil)
    psi_avg = 0.0_dp
    do i = 1, n
       psi_avg = psi_avg + state%root_frac(i) * state%psi_soil(i)
    end do
  end function soil_psi_avg

  subroutine update_plant_hydro(state, par, transpiration, dt)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    real(dp), intent(in) :: transpiration, dt
    real(dp) :: psi_s     ! soil water potential (MPa)
    real(dp) :: kx
    real(dp) :: dpsi      ! water potential drop (MPa)
    real(dp) :: et_m      ! transpiration rate (m/s)
    real(dp) :: k_eff     ! effective hydraulic conductance

    psi_s = soil_psi_avg(state)
    kx = xylem_conductance(state%psi_xylem, par)
    state%k_xylem = kx

    et_m = transpiration / rho_w
    k_eff = max(kx + par%kr_max, 1.0e-12_dp)

    if (et_m > 1.0e-10_dp) then
       dpsi = et_m / k_eff * 1.0e-8_dp
       dpsi = min(dpsi, 0.8_dp)
       state%psi_leaf  = max(psi_s - dpsi, -3.0_dp)
       state%psi_xylem = max(psi_s - 0.6_dp * dpsi, -3.5_dp)
       state%psi_root  = max(psi_s - 0.2_dp * dpsi, -3.0_dp)
    else
       state%psi_leaf  = min(state%psi_leaf + 0.1_dp * dt / 3600.0_dp, -0.05_dp)
       state%psi_xylem = min(state%psi_xylem + 0.08_dp * dt / 3600.0_dp, -0.05_dp)
       state%psi_root  = min(state%psi_root + 0.05_dp * dt / 3600.0_dp, psi_s)
    end if

    call hydraulic_stress(state, par, state%stress_hydro)
  end subroutine update_plant_hydro

  subroutine hydraulic_stress(state, par, stress)
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    real(dp), intent(out) :: stress
    real(dp) :: psi_crit   ! critical leaf water potential (MPa)

    psi_crit = par%psi50_leaf
    if (state%psi_leaf >= psi_crit) then
       stress = 1.0_dp
    else
       stress = max(0.0_dp, (state%psi_leaf - 2.0_dp * psi_crit) / (psi_crit - 2.0_dp * psi_crit))
    end if
  end subroutine hydraulic_stress

  function water_stress_factor(cfg_ihydro, state) result(f)
    use mod_physics, only: HYDRO_BETA, HYDRO_PHS
    integer, intent(in) :: cfg_ihydro
    type(t_state), intent(in) :: state
    real(dp) :: f

    if (cfg_ihydro == HYDRO_PHS) then
       f = state%stress_hydro * state%beta
       f = max(f, 0.3_dp * state%beta)
    else
       f = state%beta
    end if
    f = max(min(f, 1.0_dp), 0.1_dp)
  end function water_stress_factor

end module mod_planthydro