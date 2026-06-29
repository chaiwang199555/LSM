! Soil temperature and ground heat flux (implicit top-layer coupling + skin storage)
module mod_soil_heat
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_state, t_param
  implicit none

contains

  subroutine init_soil_grid(state, par, nsoil, t_init)
    integer, intent(in)  :: nsoil
    real(dp), intent(in)  :: t_init
    type(t_state), intent(inout) :: state
    type(t_param), intent(in)    :: par
    integer :: i
    real(dp) :: zbot       ! total soil column depth (m)
    real(dp) :: dz_total

    if (allocated(state%Tsoil)) deallocate(state%Tsoil, state%theta, state%dz, state%zmid)
    allocate(state%Tsoil(nsoil), state%theta(nsoil), state%dz(nsoil), state%zmid(nsoil))

    zbot = 2.0_dp
    dz_total = zbot
    do i = 1, nsoil
       state%dz(i) = dz_total / real(nsoil, dp) * exp(real(i - 1, dp) * 0.3_dp)
    end do
    state%dz = state%dz * dz_total / sum(state%dz)

    state%zmid(1) = state%dz(1) / 2.0_dp
    do i = 2, nsoil
       state%zmid(i) = state%zmid(i-1) + (state%dz(i-1) + state%dz(i)) / 2.0_dp
    end do

    state%Tsoil = t_init
    state%theta = par%poros * 0.6_dp
    state%Ts = t_init
  end subroutine init_soil_grid

  function soil_ground_resistance(state, par, dt) result(r_eff)
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    real(dp), intent(in) :: dt
    real(dp) :: r_eff, dz_half

    dz_half = 0.5_dp * state%dz(1)
    r_eff = dz_half / par%soil_cond + dt / (par%soil_heat_cap * state%dz(1))
    r_eff = max(r_eff, 1.0e-6_dp)
  end function soil_ground_resistance

  function implicit_soil_heat_flux(ts, state, par, dt) result(g)
    real(dp), intent(in) :: ts
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    real(dp), intent(in) :: dt
    real(dp) :: g, r_eff

    r_eff = soil_ground_resistance(state, par, dt)
    g = (state%Tsoil(1) - ts) / r_eff
  end function implicit_soil_heat_flux

  function dG_dTs_implicit(state, par, dt) result(dg)
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    real(dp), intent(in) :: dt
    real(dp) :: dg, r_eff

    r_eff = soil_ground_resistance(state, par, dt)
    dg = -1.0_dp / r_eff
  end function dG_dTs_implicit

  function skin_storage_flux(ts, ts_prev, par, dt) result(storage)
    real(dp), intent(in) :: ts, ts_prev, dt
    type(t_param), intent(in) :: par
    real(dp) :: storage

    storage = par%skin_heat_cap * (ts - ts_prev) / dt
  end function skin_storage_flux

  function soil_heat_flux_raw(ts, state, par) result(g)
    real(dp), intent(in) :: ts
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    real(dp) :: g

    g = par%soil_cond * (state%Tsoil(1) - ts) / (0.5_dp * state%dz(1))
  end function soil_heat_flux_raw

  function soil_heat_flux(ts, state, par) result(g)
    real(dp), intent(in) :: ts
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    real(dp) :: g

    g = soil_heat_flux_raw(ts, state, par)
  end function soil_heat_flux

  function dG_dTs(state, par) result(dg)
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    real(dp) :: dg

    dg = -par%soil_cond / (0.5_dp * state%dz(1))
  end function dG_dTs

  subroutine update_soil_temperature(state, par, g, dt)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    real(dp), intent(in) :: g, dt
    integer :: i, n
    real(dp) :: cap        ! soil volumetric heat capacity (J/m3/K)
    real(dp) :: relax      ! explicit update coefficient for top layer

    n = size(state%Tsoil)
    cap = par%soil_heat_cap
    relax = dt / (cap * state%dz(1))

    state%Tsoil(1) = state%Tsoil(1) - g * relax
    do i = 2, n
       state%Tsoil(i) = state%Tsoil(i) + 0.05_dp * (state%Tsoil(i-1) - state%Tsoil(i))
    end do
  end subroutine update_soil_temperature

end module mod_soil_heat