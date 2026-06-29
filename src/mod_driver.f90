! Time stepping driver with spin-up and multi-physics coupling
module mod_driver
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_config, t_param, t_state, t_flux, t_forcing, t_couple_boundary, t_soil_init
  use mod_forcing, only: t_forcing_series, load_forcing, get_forcing
  use mod_soil_init, only: load_soil_init, apply_soil_init, print_soil_init_summary
  use mod_soil_heat,    only: init_soil_grid, update_soil_temperature
  use mod_soil_water,   only: init_soil_water, update_soil_water
  use mod_surface,      only: solve_energy_balance
  use mod_photosyn,     only: photosynthesis
  use mod_canopy,       only: canopy_photosynthesis
  use mod_soil_carbon,  only: init_soil_carbon, soil_respiration, update_soil_carbon
  use mod_planthydro,   only: init_plant_hydro, update_plant_hydro
  use mod_acclimation,  only: update_acclimation
  use mod_cnp,          only: init_cnp, update_cnp
  use mod_snow,         only: init_snow, update_snow
  use mod_permafrost,   only: init_permafrost, update_phase_change
  use mod_gcm_coupling, only: forcing_to_couple, couple_in, couple_out
  use mod_conservation, only: check_balances
  use mod_io,           only: init_output, write_output, finalize_output, print_physics_summary
  use mod_physics,      only: CANOPY_TWOLEAF
  implicit none

contains

  subroutine init_state(cfg, par, force0, soil_init, state)
    type(t_config),  intent(in)  :: cfg
    type(t_param),   intent(in)  :: par
    type(t_forcing), intent(in)  :: force0
    type(t_soil_init), intent(in) :: soil_init
    type(t_state),   intent(out) :: state

    call init_soil_grid(state, par, cfg%nsoil, force0%Ta)
    call apply_soil_init(soil_init, state, par)
    state%Tc  = state%Ts
    state%LAI = par%lai
    call init_soil_water(state, par, cfg)
    call init_plant_hydro(state, par)
    call init_soil_carbon(state, par)
    call init_cnp(state, par)
    call init_snow(state)
    call init_permafrost(state, par, cfg)
  end subroutine init_state

  subroutine run_lsm(cfg, par)
    type(t_config), intent(in) :: cfg
    type(t_param),  intent(in) :: par
    type(t_forcing_series) :: series
    type(t_state)  :: state
    type(t_flux)   :: flux
    type(t_forcing) :: force
    type(t_soil_init) :: soil_init
    type(t_couple_boundary) :: bnd
    integer :: ierr, isp, it, t

    call load_forcing(cfg%forcing_file, series, ierr)
    if (ierr /= 0) stop 1

    call load_soil_init(cfg%soil_init_file, cfg, soil_init, ierr)
    if (ierr /= 0) then
       write(*,'(A,A)') 'ERROR: cannot load soil init file: ', trim(cfg%soil_init_file)
       stop 1
    end if
    call print_soil_init_summary(soil_init)

    call get_forcing(series, 1, force)
    call init_state(cfg, par, force, soil_init, state)
    call init_output(cfg%output_file, cfg)
    call print_physics_summary(cfg)

    write(*,'(A,I0,A,I0,A)') 'LSM run: ', series%ntime, ' steps x ', cfg%nspinup + 1, ' cycles'

    t = 0
    do isp = 1, cfg%nspinup + 1
       if (isp <= cfg%nspinup) then
          write(*,'(A,I0,A)') 'Spin-up cycle ', isp, ' ...'
       else
          write(*,'(A)') 'Production run ...'
       end if

       do it = 1, series%ntime
          t = t + 1
          call get_forcing(series, it, force)
          call forcing_to_couple(force, bnd)
          call couple_in(bnd, cfg, force)

          call update_acclimation(force, state, par, cfg, cfg%dt)
          call solve_energy_balance(force, state, par, cfg, flux)
          call update_plant_hydro(state, par, flux%ET, cfg%dt)
          call update_soil_temperature(state, par, flux%G, cfg%dt)
          call update_phase_change(state, par, cfg, cfg%dt)
          call update_snow(state, par, cfg, force%Ta, force%P, flux%Rn, flux%H, flux%G, cfg%dt, flux)

          if (cfg%carbon_on) then
             if (cfg%icanopy == CANOPY_TWOLEAF) then
                call canopy_photosynthesis(force, state, par, cfg, flux)
             else
                call photosynthesis(force, state, par, cfg, flux)
             end if
             call soil_respiration(state, par, cfg, flux)
             call update_soil_carbon(state, par, cfg, flux%GPP, cfg%dt)
             call update_cnp(state, par, cfg, flux, cfg%dt)
          end if

          call update_soil_water(state, par, cfg, force%P, flux%ET, state%psi_root, cfg%dt)
          call couple_out(flux, state, par, bnd)
          call check_balances(cfg, flux, force%P, flux%ET, cfg%dt, t)

          if (isp > cfg%nspinup) then
             call write_output(cfg%output_file, t - cfg%nspinup * series%ntime, &
                  force%SW, force%LW, force%Ta, force%P, force%WS, flux, state)
          end if
       end do
    end do

    call finalize_output()
    write(*,'(A,A)') 'Done. Output written to ', trim(cfg%output_file)
  end subroutine run_lsm

end module mod_driver