! Namelist I/O and output writing (txt or NetCDF)
module mod_io
  use mod_kinds,   only: dp
  use mod_types,   only: t_config, t_param, t_state, t_flux
  use mod_physics
  use mod_ncio, only: is_netcdf_file, init_output_nc, write_output_nc, close_output_nc, &
                      pack_output_values, NVAR_OUT
  implicit none

  logical, save :: output_is_nc = .false.

contains

  subroutine read_namelist(cfg, par, ierr)
    type(t_config), intent(out) :: cfg
    type(t_param),  intent(out) :: par
    integer, intent(out) :: ierr
    integer :: ios         ! namelist read status

    ! config_nml read buffers (field meanings in mod_types::t_config)
    character(len=256) :: forcing_file, soil_init_file, output_file
    character(len=64)  :: site_name
    real(dp) :: dt, tol, accl_days
    integer  :: nspinup, nsoil, max_iter
    integer  :: istomatal, ihydro, isoilwater, isoilcarbon, icnp
    integer  :: icanopy, isnow, ifrost, irad
    logical  :: carbon_on, check_conservation, eeo_on

    ! param_nml read buffers (field meanings in mod_types::t_param)
    real(dp) :: z0, zdisp, albedo, emiss, hc, lai, gs_max, g0, g1, vcmax, jmax
    real(dp) :: rd25_base, ksat, poros, theta_r, alpha_vg, n_vg
    real(dp) :: W_field, W_wilt, soil_heat_cap, soil_cond, skin_heat_cap
    real(dp) :: p50_xylem, ck_xylem, kx_max, kr_max, psi50_leaf, gw_depth
    real(dp) :: cue_micro, k_litter, k_som, litterfall, n_uptake_max, p_uptake_max
    real(dp) :: clumping, ext_coeff, snow_emiss, fresh_snow_alb, rain_snow_thresh, soil_sat_ice

    namelist /config_nml/ forcing_file, soil_init_file, output_file, site_name, dt, &
                         nspinup, nsoil, carbon_on, max_iter, tol, &
                         check_conservation, istomatal, ihydro, isoilwater, &
                         isoilcarbon, icnp, eeo_on, accl_days, &
                         icanopy, isnow, ifrost, irad
    namelist /param_nml/  z0, zdisp, albedo, emiss, hc, lai, gs_max, g0, g1, &
                         vcmax, jmax, rd25_base, ksat, poros, theta_r, alpha_vg, n_vg, &
                         W_field, W_wilt, soil_heat_cap, soil_cond, skin_heat_cap, &
                         p50_xylem, ck_xylem, kx_max, kr_max, psi50_leaf, gw_depth, &
                         cue_micro, k_litter, k_som, litterfall, n_uptake_max, p_uptake_max, &
                         clumping, ext_coeff, snow_emiss, fresh_snow_alb, rain_snow_thresh, soil_sat_ice

    ierr = 0
    open(unit=10, file='namelist.nml', status='old', action='read', iostat=ios)
    if (ios /= 0) then
       ierr = 1
       write(*,*) 'WARNING: namelist.nml not found, using defaults'
       return
    end if

    call copy_config_defaults(cfg, forcing_file, soil_init_file, output_file, site_name, dt, nspinup, &
         nsoil, carbon_on, max_iter, tol, check_conservation, istomatal, ihydro, &
         isoilwater, isoilcarbon, icnp, eeo_on, accl_days, icanopy, isnow, ifrost, irad)
    call copy_param_defaults(par, z0, zdisp, albedo, emiss, hc, lai, gs_max, g0, g1, &
         vcmax, jmax, rd25_base, ksat, poros, theta_r, alpha_vg, n_vg, W_field, W_wilt, &
         soil_heat_cap, soil_cond, skin_heat_cap, p50_xylem, ck_xylem, kx_max, kr_max, psi50_leaf, &
         gw_depth, cue_micro, k_litter, k_som, litterfall, n_uptake_max, p_uptake_max, &
         clumping, ext_coeff, snow_emiss, fresh_snow_alb, rain_snow_thresh, soil_sat_ice)

    read(10, nml=config_nml, iostat=ios)
    if (ios > 0) then
       write(*,*) 'WARNING: config namelist read issue, using defaults'
    else
       call apply_config(cfg, forcing_file, soil_init_file, output_file, site_name, dt, nspinup, &
            nsoil, carbon_on, max_iter, tol, check_conservation, istomatal, ihydro, &
            isoilwater, isoilcarbon, icnp, eeo_on, accl_days, icanopy, isnow, ifrost, irad)
    end if

    rewind(10)
    read(10, nml=param_nml, iostat=ios)
    if (ios > 0) then
       write(*,*) 'WARNING: param namelist read issue, using defaults'
    else
       call apply_param(par, z0, zdisp, albedo, emiss, hc, lai, gs_max, g0, g1, &
            vcmax, jmax, rd25_base, ksat, poros, theta_r, alpha_vg, n_vg, W_field, &
            W_wilt, soil_heat_cap, soil_cond, skin_heat_cap, p50_xylem, ck_xylem, kx_max, kr_max, &
            psi50_leaf, gw_depth, cue_micro, k_litter, k_som, litterfall, &
            n_uptake_max, p_uptake_max, clumping, ext_coeff, snow_emiss, &
            fresh_snow_alb, rain_snow_thresh, soil_sat_ice)
    end if
    close(10)
  end subroutine read_namelist

  subroutine copy_config_defaults(cfg, forcing_file, soil_init_file, output_file, site_name, dt, nspinup, &
       nsoil, carbon_on, max_iter, tol, check_conservation, istomatal, ihydro, &
       isoilwater, isoilcarbon, icnp, eeo_on, accl_days, icanopy, isnow, ifrost, irad)
    type(t_config), intent(in) :: cfg
    character(len=*), intent(out) :: forcing_file, soil_init_file, output_file, site_name
    real(dp), intent(out) :: dt, tol, accl_days
    integer, intent(out) :: nspinup, nsoil, max_iter, istomatal, ihydro, isoilcarbon, icnp
    integer, intent(out) :: isoilwater, icanopy, isnow, ifrost, irad
    logical, intent(out) :: carbon_on, check_conservation, eeo_on

    forcing_file = cfg%forcing_file; soil_init_file = cfg%soil_init_file
    output_file = cfg%output_file
    site_name = cfg%site_name; dt = cfg%dt; nspinup = cfg%nspinup
    nsoil = cfg%nsoil; carbon_on = cfg%carbon_on; max_iter = cfg%max_iter
    tol = cfg%tol; check_conservation = cfg%check_conservation
    istomatal = cfg%istomatal; ihydro = cfg%ihydro
    isoilwater = cfg%isoilwater; isoilcarbon = cfg%isoilcarbon
    icnp = cfg%icnp; eeo_on = cfg%eeo_on; accl_days = cfg%accl_days
    icanopy = cfg%icanopy; isnow = cfg%isnow; ifrost = cfg%ifrost; irad = cfg%irad
  end subroutine copy_config_defaults

  subroutine apply_config(cfg, forcing_file, soil_init_file, output_file, site_name, dt, nspinup, &
       nsoil, carbon_on, max_iter, tol, check_conservation, istomatal, ihydro, &
       isoilwater, isoilcarbon, icnp, eeo_on, accl_days, icanopy, isnow, ifrost, irad)
    type(t_config), intent(out) :: cfg
    character(len=*), intent(in) :: forcing_file, soil_init_file, output_file, site_name
    real(dp), intent(in) :: dt, tol, accl_days
    integer, intent(in) :: nspinup, nsoil, max_iter, istomatal, ihydro, isoilwater, isoilcarbon, icnp
    integer, intent(in) :: icanopy, isnow, ifrost, irad
    logical, intent(in) :: carbon_on, check_conservation, eeo_on

    cfg%forcing_file = forcing_file; cfg%soil_init_file = soil_init_file
    cfg%output_file = output_file
    cfg%site_name = site_name; cfg%dt = dt; cfg%nspinup = nspinup
    cfg%nsoil = nsoil; cfg%carbon_on = carbon_on; cfg%max_iter = max_iter
    cfg%tol = tol; cfg%check_conservation = check_conservation
    cfg%istomatal = istomatal; cfg%ihydro = ihydro
    cfg%isoilwater = isoilwater; cfg%isoilcarbon = isoilcarbon
    cfg%icnp = icnp; cfg%eeo_on = eeo_on; cfg%accl_days = accl_days
    cfg%icanopy = icanopy; cfg%isnow = isnow; cfg%ifrost = ifrost; cfg%irad = irad
  end subroutine apply_config

  subroutine copy_param_defaults(par, z0, zdisp, albedo, emiss, hc, lai, gs_max, g0, g1, &
       vcmax, jmax, rd25_base, ksat, poros, theta_r, alpha_vg, n_vg, W_field, W_wilt, &
       soil_heat_cap, soil_cond, skin_heat_cap, p50_xylem, ck_xylem, kx_max, kr_max, psi50_leaf, &
       gw_depth, cue_micro, k_litter, k_som, litterfall, n_uptake_max, p_uptake_max, &
       clumping, ext_coeff, snow_emiss, fresh_snow_alb, rain_snow_thresh, soil_sat_ice)
    type(t_param), intent(in) :: par
    real(dp), intent(out) :: z0, zdisp, albedo, emiss, hc, lai, gs_max, g0, g1
    real(dp), intent(out) :: vcmax, jmax, rd25_base, ksat, poros, theta_r, alpha_vg, n_vg
    real(dp), intent(out) :: W_field, W_wilt, soil_heat_cap, soil_cond, skin_heat_cap
    real(dp), intent(out) :: p50_xylem, ck_xylem, kx_max, kr_max, psi50_leaf, gw_depth
    real(dp), intent(out) :: cue_micro, k_litter, k_som, litterfall, n_uptake_max, p_uptake_max
    real(dp), intent(out) :: clumping, ext_coeff, snow_emiss, fresh_snow_alb, rain_snow_thresh, soil_sat_ice

    z0=par%z0; zdisp=par%zdisp; albedo=par%albedo; emiss=par%emiss
    hc=par%hc; lai=par%lai; gs_max=par%gs_max; g0=par%g0; g1=par%g1
    vcmax=par%Vcmax; jmax=par%Jmax; rd25_base=par%Rd25_base
    ksat=par%Ksat; poros=par%poros; theta_r=par%theta_r
    alpha_vg=par%alpha_vg; n_vg=par%n_vg
    W_field=par%W_field; W_wilt=par%W_wilt
    soil_heat_cap=par%soil_heat_cap; soil_cond=par%soil_cond
    skin_heat_cap=par%skin_heat_cap
    p50_xylem=par%p50_xylem; ck_xylem=par%ck_xylem
    kx_max=par%kx_max; kr_max=par%kr_max; psi50_leaf=par%psi50_leaf
    gw_depth=par%gw_depth; cue_micro=par%cue_micro
    k_litter=par%k_litter; k_som=par%k_som; litterfall=par%litterfall
    n_uptake_max=par%n_uptake_max; p_uptake_max=par%p_uptake_max
    clumping=par%clumping; ext_coeff=par%ext_coeff
    snow_emiss=par%snow_emiss; fresh_snow_alb=par%fresh_snow_alb
    rain_snow_thresh=par%rain_snow_thresh; soil_sat_ice=par%soil_sat_ice
  end subroutine copy_param_defaults

  subroutine apply_param(par, z0, zdisp, albedo, emiss, hc, lai, gs_max, g0, g1, &
       vcmax, jmax, rd25_base, ksat, poros, theta_r, alpha_vg, n_vg, W_field, &
       W_wilt, soil_heat_cap, soil_cond, skin_heat_cap, p50_xylem, ck_xylem, kx_max, kr_max, &
       psi50_leaf, gw_depth, cue_micro, k_litter, k_som, litterfall, &
       n_uptake_max, p_uptake_max, clumping, ext_coeff, snow_emiss, &
       fresh_snow_alb, rain_snow_thresh, soil_sat_ice)
    type(t_param), intent(out) :: par
    real(dp), intent(in) :: z0, zdisp, albedo, emiss, hc, lai, gs_max, g0, g1
    real(dp), intent(in) :: vcmax, jmax, rd25_base, ksat, poros, theta_r, alpha_vg, n_vg
    real(dp), intent(in) :: W_field, W_wilt, soil_heat_cap, soil_cond, skin_heat_cap
    real(dp), intent(in) :: p50_xylem, ck_xylem, kx_max, kr_max, psi50_leaf, gw_depth
    real(dp), intent(in) :: cue_micro, k_litter, k_som, litterfall, n_uptake_max, p_uptake_max
    real(dp), intent(in) :: clumping, ext_coeff, snow_emiss, fresh_snow_alb, rain_snow_thresh, soil_sat_ice

    par%z0=z0; par%zdisp=zdisp; par%albedo=albedo; par%emiss=emiss
    par%hc=hc; par%lai=lai; par%gs_max=gs_max; par%g0=g0; par%g1=g1
    par%Vcmax=vcmax; par%Jmax=jmax; par%Rd25_base=rd25_base
    par%Ksat=ksat; par%poros=poros; par%theta_r=theta_r
    par%alpha_vg=alpha_vg; par%n_vg=n_vg
    par%W_field=W_field; par%W_wilt=W_wilt
    par%soil_heat_cap=soil_heat_cap; par%soil_cond=soil_cond
    par%skin_heat_cap=skin_heat_cap
    par%p50_xylem=p50_xylem; par%ck_xylem=ck_xylem
    par%kx_max=kx_max; par%kr_max=kr_max; par%psi50_leaf=psi50_leaf
    par%gw_depth=gw_depth; par%cue_micro=cue_micro
    par%k_litter=k_litter; par%k_som=k_som; par%litterfall=litterfall
    par%n_uptake_max=n_uptake_max; par%p_uptake_max=p_uptake_max
    par%clumping=clumping; par%ext_coeff=ext_coeff
    par%snow_emiss=snow_emiss; par%fresh_snow_alb=fresh_snow_alb
    par%rain_snow_thresh=rain_snow_thresh; par%soil_sat_ice=soil_sat_ice
  end subroutine apply_param

  subroutine print_physics_summary(cfg)
    type(t_config), intent(in) :: cfg
    write(*,'(A)') '--- Physics options ---'
    write(*,'(A,I0)') '  Stomatal: ', cfg%istomatal
    write(*,'(A,I0)') '  Plant hydro: ', cfg%ihydro
    write(*,'(A,I0)') '  Soil water: ', cfg%isoilwater
    write(*,'(A,I0)') '  Soil carbon: ', cfg%isoilcarbon
    write(*,'(A,I0)') '  CNP level: ', cfg%icnp
    write(*,'(A,L1)') '  EEO acclimation: ', cfg%eeo_on
    write(*,'(A,I0)') '  Canopy: ', cfg%icanopy
    write(*,'(A,I0)') '  Snow: ', cfg%isnow
    write(*,'(A,I0)') '  Permafrost: ', cfg%ifrost
    write(*,'(A,I0)') '  Radiation/GCM: ', cfg%irad
    write(*,'(A)') '-----------------------'
  end subroutine print_physics_summary

  subroutine init_output(filename, cfg)
    character(len=*), intent(in) :: filename
    type(t_config),  intent(in) :: cfg
    integer :: unit

    output_is_nc = is_netcdf_file(filename)
    if (output_is_nc) then
       call init_output_nc(filename, cfg)
       return
    end if

    unit = 30
    open(unit=unit, file=trim(filename), status='replace', action='write')
    write(unit, '(A)') '# step SW LW Ta P WS Rn H LE G Ts beta W GPP NEE Rleaf Rsoil' // &
         ' psi_leaf stress_hydro snow_swe GPP_sun GPP_shade albedo_eff'
    close(unit)
  end subroutine init_output

  subroutine write_output(filename, step, force_sw, force_lw, force_ta, force_p, force_ws, &
                          flux, state)
    character(len=*), intent(in) :: filename
    integer, intent(in) :: step
    real(dp), intent(in) :: force_sw, force_lw, force_ta, force_p, force_ws
    type(t_flux),  intent(in) :: flux
    type(t_state), intent(in) :: state
    integer :: unit        ! output file unit number
    real(dp) :: vals(NVAR_OUT)

    if (output_is_nc) then
       call write_output_nc(step, force_sw, force_lw, force_ta, force_p, force_ws, flux, state)
       return
    end if

    unit = 30
    call pack_output_values(step, force_sw, force_lw, force_ta, force_p, force_ws, flux, state, vals)
    open(unit=unit, file=trim(filename), status='old', action='write', position='append')
    write(unit, '(I8,23(1X,ES14.6))') int(vals(1)), vals(2:NVAR_OUT)
    close(unit)
  end subroutine write_output

  subroutine finalize_output()
    if (output_is_nc) call close_output_nc()
    output_is_nc = .false.
  end subroutine finalize_output

end module mod_io