! Soil water: bucket or multi-layer Richards (van Genuchten)
module mod_soil_water
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_state, t_param, t_config
  use mod_physics, only: SWATER_BUCKET, SWATER_RICHARDS, HYDRO_PHS
  implicit none

contains

  subroutine init_root_profile(state, par)
    type(t_state), intent(inout) :: state
    type(t_param), intent(in)    :: par
    integer :: i, n
    real(dp) :: z          ! layer mid-point depth (m)
    real(dp) :: fsum       ! normalization sum for root weights

    n = size(state%theta)
    if (allocated(state%root_frac)) deallocate(state%root_frac, state%psi_soil)
    allocate(state%root_frac(n), state%psi_soil(n))

    fsum = 0.0_dp
    do i = 1, n
       z = state%zmid(i)
       state%root_frac(i) = exp(-2.0_dp * z)
       fsum = fsum + state%root_frac(i)
    end do
    state%root_frac = state%root_frac / fsum
    call update_soil_psi(state, par)
  end subroutine init_root_profile

  subroutine init_soil_water(state, par, cfg)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg

    if (cfg%isoilwater == SWATER_RICHARDS .or. cfg%ihydro == HYDRO_PHS) then
       call init_root_profile(state, par)
    end if
    call update_beta(state, par)
  end subroutine init_soil_water

  function theta_from_psi(psi, par) result(theta)
    real(dp), intent(in) :: psi
    type(t_param), intent(in) :: par
    real(dp) :: theta
    real(dp) :: m          ! van Genuchten m parameter
    real(dp) :: se         ! effective saturation

    m = 1.0_dp - 1.0_dp / par%n_vg
    if (psi >= 0.0_dp) then
       se = 1.0_dp
    else
       se = (1.0_dp + (par%alpha_vg * abs(psi))**par%n_vg)**(-m)
    end if
    theta = par%theta_r + (par%poros - par%theta_r) * se
  end function theta_from_psi

  function psi_from_theta(theta, par) result(psi)
    real(dp), intent(in) :: theta
    type(t_param), intent(in) :: par
    real(dp) :: psi
    real(dp) :: m, se

    m = 1.0_dp - 1.0_dp / par%n_vg
    se = (theta - par%theta_r) / max(par%poros - par%theta_r, 1.0e-6_dp)
    se = max(min(se, 1.0_dp), 1.0e-4_dp)
    psi = -((se**(-1.0_dp / m) - 1.0_dp)**(1.0_dp / par%n_vg)) / par%alpha_vg
  end function psi_from_theta

  function conductivity(theta, par) result(k)
    real(dp), intent(in) :: theta
    type(t_param), intent(in) :: par
    real(dp) :: k
    real(dp) :: m, se

    m = 1.0_dp - 1.0_dp / par%n_vg
    se = (theta - par%theta_r) / max(par%poros - par%theta_r, 1.0e-6_dp)
    se = max(min(se, 1.0_dp), 0.0_dp)
    k = par%Ksat * se**0.5_dp * (1.0_dp - (1.0_dp - se**(1.0_dp / m))**m)**2
    k = max(k, 1.0e-12_dp)
  end function conductivity

  subroutine update_soil_psi(state, par)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    integer :: i

    do i = 1, size(state%theta)
       state%psi_soil(i) = psi_from_theta(state%theta(i), par)
    end do
  end subroutine update_soil_psi

  subroutine update_beta(state, par)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    real(dp) :: frac       ! relative soil wetness
    real(dp) :: theta_avg  ! depth-mean soil moisture

    if (allocated(state%theta)) then
       theta_avg = sum(state%theta * state%dz) / sum(state%dz)
       frac = (theta_avg - par%theta_r) / max(par%poros - par%theta_r, 1.0e-6_dp)
       state%beta = max(min(frac, 1.0_dp), 0.0_dp)
       state%W = theta_avg * sum(state%dz) * 1000.0_dp
    else
       if (state%W >= par%W_field) then
          state%beta = 1.0_dp
       else if (state%W <= par%W_wilt) then
          state%beta = 0.0_dp
       else
          frac = (state%W - par%W_wilt) / (par%W_field - par%W_wilt)
          state%beta = max(min(frac, 1.0_dp), 0.0_dp)
       end if
    end if
  end subroutine update_beta

  subroutine update_soil_water(state, par, cfg, precip, et, psi_root, dt)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    real(dp), intent(in) :: precip, et, psi_root, dt

    if (cfg%isoilwater == SWATER_RICHARDS) then
       call richards_step(state, par, precip, et, psi_root, dt)
    else
       call bucket_step(state, par, precip, et, dt)
    end if
    call update_beta(state, par)
  end subroutine update_soil_water

  subroutine bucket_step(state, par, precip, et, dt)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    real(dp), intent(in) :: precip, et, dt
    real(dp) :: dw          ! change in root-zone storage (mm)

    dw = precip - et * dt / 3600.0_dp
    if (state%W + dw > par%W_field) then
       state%W = par%W_field
    else
       state%W = max(state%W + dw, 0.0_dp)
    end if
  end subroutine bucket_step

  subroutine richards_step(state, par, precip, et, psi_root, dt)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    real(dp), intent(in) :: precip, et, psi_root, dt
    integer :: n, i
    real(dp), allocatable :: q(:)      ! inter-layer water flux (m/s)
    real(dp), allocatable :: sink(:)   ! root water uptake per layer (m/s)
    real(dp) :: infl         ! infiltration flux (m/s)
    real(dp) :: dz_i         ! inter-layer distance (m)
    real(dp) :: k_mid        ! inter-layer hydraulic conductivity (m/s)
    real(dp) :: runoff       ! surface runoff (m)

    n = size(state%theta)
    allocate(q(n+1), sink(n))

    infl = precip / 1000.0_dp
    sink = 0.0_dp
    do i = 1, n
       sink(i) = state%root_frac(i) * max(par%kr_max * (state%psi_soil(i) - psi_root), 0.0_dp) &
               * 1.0e6_dp
    end do
    if (sum(sink) > 0.0_dp) sink = sink * min(et, sum(sink)) / sum(sink)

    q(1) = min(infl, par%Ksat * 10.0_dp)
    do i = 1, n-1
       dz_i = 0.5_dp * (state%dz(i) + state%dz(i+1))
       k_mid = 0.5_dp * (conductivity(state%theta(i), par) + conductivity(state%theta(i+1), par))
       q(i+1) = k_mid * ((state%psi_soil(i+1) - state%psi_soil(i)) / dz_i + 1.0_dp)
    end do
    q(n+1) = conductivity(state%theta(n), par)

    state%theta(1) = state%theta(1) + (q(1) - q(2) - sink(1)) * dt / state%dz(1)
    do i = 2, n-1
       state%theta(i) = state%theta(i) + (q(i) - q(i+1) - sink(i)) * dt / state%dz(i)
    end do
    state%theta(n) = state%theta(n) + (q(n) - q(n+1) - sink(n)) * dt / state%dz(n)

    do i = 1, n
       state%theta(i) = max(min(state%theta(i), par%poros), par%theta_r)
    end do
    if (state%theta(1) >= par%poros - 1.0e-4_dp) then
       runoff = max(0.0_dp, infl - q(2))
    end if

    call update_soil_psi(state, par)
    deallocate(q, sink)
  end subroutine richards_step

end module mod_soil_water