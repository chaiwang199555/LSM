! Farquhar C3 photosynthesis + autotrophic respiration
module mod_photosyn
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_forcing, t_state, t_param, t_config, t_flux
  use mod_acclimation, only: vcmax_at_temp, jmax_at_temp, rd_at_temp
  use mod_planthydro,  only: water_stress_factor
  implicit none

  real(dp), parameter :: o2 = 210000.0_dp         ! O2 concentration (umol/mol)
  real(dp), parameter :: kc = 404.9_dp            ! CO2 Michaelis constant (umol/mol)
  real(dp), parameter :: ko = 278400.0_dp         ! O2 Michaelis constant (umol/mol)
  real(dp), parameter :: gamma_star = 42.75_dp    ! CO2 compensation point (umol/mol)
  integer, parameter :: stomatal_inner_max = 8
  real(dp), parameter :: stomatal_gs_tol = 1.0e-7_dp

contains

  subroutine photosynthesis(force, state, par, cfg, flux)
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(in)    :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    type(t_flux),    intent(inout) :: flux
    real(dp) :: vcmax      ! temperature-adjusted Vcmax (umol/m2/s)
    real(dp) :: jmax
    real(dp) :: rd         ! temperature-adjusted dark respiration (umol/m2/s)
    real(dp) :: par_abs    ! absorbed photosynthetically active radiation
    real(dp) :: j          ! electron transport rate (umol/m2/s)
    real(dp) :: wc         ! Rubisco-limited assimilation (umol/m2/s)
    real(dp) :: wj         ! light-limited assimilation (umol/m2/s)
    real(dp) :: a          ! net assimilation rate (umol/m2/s)
    real(dp) :: stress     ! water stress factor
    real(dp) :: nu_p       ! combined nutrient stress factor

    call photosynthesis_tl(force, state, par, cfg, force%Ta, flux)
  end subroutine photosynthesis

  subroutine photosynthesis_tl(force, state, par, cfg, tl, flux)
    type(t_forcing), intent(in)    :: force
    type(t_state),   intent(in)    :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    real(dp), intent(in) :: tl
    type(t_flux), intent(inout) :: flux
    real(dp) :: vcmax, jmax, rd, par_abs, j, wc, wj, a, stress, nu_p

    stress = water_stress_factor(cfg%ihydro, state)
    nu_p = state%nu_stress * state%np_stress

    vcmax = vcmax_at_temp(state%Vcmax25, tl) * stress * nu_p
    jmax  = jmax_at_temp(par%Jmax, tl) * stress * nu_p
    rd    = rd_at_temp(state%Rd25, tl)

    if (flux%PAR_sun + flux%PAR_shade > 0.0_dp) then
       par_abs = flux%PAR_sun + flux%PAR_shade
    else
       par_abs = force%SW * 0.5_dp
    end if
    j = jmax * par_abs / (par_abs + 150.0_dp)

    wc = vcmax * max(flux%Ci - gamma_star, 0.0_dp) / (flux%Ci + kc * (1.0_dp + o2 / ko))
    wj = j * max(flux%Ci - gamma_star, 0.0_dp) / (flux%Ci + 2.0_dp * gamma_star)
    a  = min(wc, wj) - rd

    if (flux%Ci <= 0.0_dp) flux%Ci = force%CO2 * 0.7_dp
    flux%An  = max(a, 0.0_dp)
    flux%GPP = flux%An * state%LAI
    flux%Rleaf = rd * state%LAI
  end subroutine photosynthesis_tl

  subroutine respiration_autotrophic(state, par, cfg, flux)
    type(t_state),  intent(in)    :: state
    type(t_param),  intent(in)    :: par
    type(t_config), intent(in)    :: cfg
    type(t_flux),   intent(inout) :: flux
    real(dp) :: rd

    rd = rd_at_temp(state%Rd25, state%Tsoil(1))
    flux%Rleaf = rd * state%LAI
  end subroutine respiration_autotrophic

  subroutine photosyn_stomatal_coupling(force, state, par, cfg, gs, an, ci)
    type(t_forcing), intent(in)  :: force
    type(t_state),   intent(in)  :: state
    type(t_param),   intent(in)  :: par
    type(t_config),  intent(in)  :: cfg
    real(dp), intent(out) :: gs, an, ci

    call photosyn_stomatal_coupling_tl(force, state, par, cfg, force%Ta, gs, an, ci)
  end subroutine photosyn_stomatal_coupling

  subroutine photosyn_stomatal_coupling_tl(force, state, par, cfg, tl, gs, an, ci)
    type(t_forcing), intent(in)  :: force
    type(t_state),   intent(in)  :: state
    type(t_param),   intent(in)  :: par
    type(t_config),  intent(in)  :: cfg
    real(dp), intent(in)  :: tl
    real(dp), intent(out) :: gs, an, ci
    type(t_flux) :: flux
    real(dp) :: gs_old, an_old
    integer :: k

    flux%Ci = force%CO2 * 0.7_dp
    gs_old = -1.0_dp
    an_old = -1.0_dp
    do k = 1, stomatal_inner_max
       call photosynthesis_tl(force, state, par, cfg, tl, flux)
       an = flux%An
       ci = flux%Ci
       call stomatal_from_scheme(force, state, par, cfg, an, gs)
       flux%Ci = force%CO2 - an * 1.6_dp / max(gs, 1.0e-6_dp) * force%PA &
               / (8.314_dp * max(tl, 200.0_dp))
       flux%Ci = max(flux%Ci, 10.0_dp)
       if (k > 1) then
          if (abs(gs - gs_old) < stomatal_gs_tol .and. abs(an - an_old) < 0.01_dp) exit
       end if
       gs_old = gs
       an_old = an
    end do
    an = flux%An
    ci = flux%Ci
  end subroutine photosyn_stomatal_coupling_tl

  subroutine stomatal_from_scheme(force, state, par, cfg, an, gs)
    use mod_conductance, only: stomatal_conductance
    type(t_forcing), intent(in)  :: force
    type(t_state),   intent(in)  :: state
    type(t_param),   intent(in)  :: par
    type(t_config),  intent(in)  :: cfg
    real(dp), intent(in)  :: an
    real(dp), intent(out) :: gs
    call stomatal_conductance(force, state, par, cfg, an, gs)
  end subroutine stomatal_from_scheme

end module mod_photosyn