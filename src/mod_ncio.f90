! NetCDF I/O for forcing input and model output
module mod_ncio
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_forcing, t_forcing_series, t_soil_init, t_config, t_state, t_flux
  use mod_radiation, only: derive_humidity, fill_sw_geometry
  use netcdf
  implicit none

  integer, parameter :: NVAR_OUT = 23

  integer, save :: nc_out_id = -1
  integer, save :: nc_out_time_varid = -1
  integer, save :: nc_out_varids(NVAR_OUT) = -1
  integer, save :: nc_out_rec = 0

contains

  logical function is_netcdf_file(filename) result(is_nc)
    character(len=*), intent(in) :: filename
    integer :: n

    n = len_trim(filename)
    is_nc = n >= 3 .and. filename(n-2:n) == '.nc'
  end function is_netcdf_file

  subroutine check_nc(status, msg)
    integer, intent(in) :: status
    character(len=*), intent(in) :: msg
    if (status /= nf90_noerr) then
       write(*,'(A,A)') 'ERROR NetCDF: ', trim(msg)
       write(*,'(A)') trim(nf90_strerror(status))
       stop 1
    end if
  end subroutine check_nc

  subroutine load_forcing_nc(filename, series, ierr)
    character(len=*), intent(in)  :: filename
    type(t_forcing_series), intent(out) :: series
    integer, intent(out) :: ierr
    integer :: ncid, time_dimid, varid, istat
    integer :: ntime, iv, it
    real(dp), allocatable :: buf(:)
    character(len=32) :: vname
    logical :: has_cos_sza, has_sw_beam_frac

    ierr = 0
    call check_nc(nf90_open(trim(filename), nf90_nowrite, ncid), 'open forcing nc')

    call check_nc(nf90_inq_dimid(ncid, 'time', time_dimid), 'inq time dim')
    call check_nc(nf90_inquire_dimension(ncid, time_dimid, len=ntime), 'inq time len')

    series%ntime = ntime
    allocate(series%data(ntime))

    allocate(buf(ntime))
    do iv = 1, 8
       select case (iv)
       case (1); vname = 'SW'
       case (2); vname = 'LW'
       case (3); vname = 'Ta'
       case (4); vname = 'P'
       case (5); vname = 'WS'
       case (6); vname = 'PA'
       case (7); vname = 'CO2'
       case (8); vname = 'RH'
       end select
       call check_nc(nf90_inq_varid(ncid, trim(vname), varid), 'inq var '//trim(vname))
       call check_nc(nf90_get_var(ncid, varid, buf), 'get var '//trim(vname))
       select case (iv)
       case (1)
          do it = 1, ntime
             if (buf(it) <= miss + 1.0_dp) buf(it) = 0.0_dp
             series%data(it)%SW = buf(it)
          end do
       case (2)
          do it = 1, ntime
             if (buf(it) <= miss + 1.0_dp) buf(it) = 300.0_dp
             series%data(it)%LW = buf(it)
          end do
       case (3)
          do it = 1, ntime
             if (buf(it) <= miss + 1.0_dp) buf(it) = tfrz + 15.0_dp
             series%data(it)%Ta = buf(it)
          end do
       case (4)
          do it = 1, ntime
             if (buf(it) <= miss + 1.0_dp) buf(it) = 0.0_dp
             series%data(it)%P = buf(it)
          end do
       case (5)
          do it = 1, ntime
             if (buf(it) <= miss + 1.0_dp) buf(it) = 2.0_dp
             series%data(it)%WS = buf(it)
          end do
       case (6)
          do it = 1, ntime
             if (buf(it) <= miss + 1.0_dp) buf(it) = 101325.0_dp
             series%data(it)%PA = buf(it)
          end do
       case (7)
          do it = 1, ntime
             if (buf(it) <= miss + 1.0_dp) buf(it) = 400.0_dp
             series%data(it)%CO2 = buf(it)
          end do
       case (8)
          do it = 1, ntime
             if (buf(it) <= miss + 1.0_dp) buf(it) = 70.0_dp
             series%data(it)%RH = buf(it)
          end do
       end select
    end do

    deallocate(buf)

    has_cos_sza = .false.
    has_sw_beam_frac = .false.
    istat = nf90_inq_varid(ncid, 'cos_sza', varid)
    if (istat == nf90_noerr) has_cos_sza = .true.
    istat = nf90_inq_varid(ncid, 'sw_beam_frac', varid)
    if (istat == nf90_noerr) has_sw_beam_frac = .true.

    if (has_cos_sza) then
       allocate(buf(ntime))
       call check_nc(nf90_inq_varid(ncid, 'cos_sza', varid), 'inq var cos_sza')
       call check_nc(nf90_get_var(ncid, varid, buf), 'get var cos_sza')
       do it = 1, ntime
          if (buf(it) <= miss + 1.0_dp) then
             series%data(it)%cos_sza = miss
          else
             series%data(it)%cos_sza = buf(it)
          end if
       end do
       deallocate(buf)
    else
       do it = 1, ntime
          series%data(it)%cos_sza = miss
       end do
    end if

    if (has_sw_beam_frac) then
       allocate(buf(ntime))
       call check_nc(nf90_inq_varid(ncid, 'sw_beam_frac', varid), 'inq var sw_beam_frac')
       call check_nc(nf90_get_var(ncid, varid, buf), 'get var sw_beam_frac')
       do it = 1, ntime
          if (buf(it) <= miss + 1.0_dp) then
             series%data(it)%sw_beam_frac = miss
          else
             series%data(it)%sw_beam_frac = buf(it)
          end if
       end do
       deallocate(buf)
    else
       do it = 1, ntime
          series%data(it)%sw_beam_frac = miss
       end do
    end if

    do it = 1, ntime
       call derive_humidity(series%data(it))
       call fill_sw_geometry(series%data(it))
    end do
    call check_nc(nf90_close(ncid), 'close forcing nc')
  end subroutine load_forcing_nc

  subroutine load_soil_init_nc(filename, soil, ierr)
    character(len=*), intent(in)  :: filename
    type(t_soil_init), intent(out) :: soil
    integer, intent(out) :: ierr
    integer :: ncid, soil_dimid, varid
    integer :: nlayer

    ierr = 0
    call check_nc(nf90_open(trim(filename), nf90_nowrite, ncid), 'open soil init nc')
    call check_nc(nf90_inq_dimid(ncid, 'soil', soil_dimid), 'inq soil dim')
    call check_nc(nf90_inquire_dimension(ncid, soil_dimid, len=nlayer), 'inq soil len')

    soil%nlayer = nlayer
    if (allocated(soil%Tsoil)) deallocate(soil%Tsoil, soil%theta)
    allocate(soil%Tsoil(nlayer), soil%theta(nlayer))

    call check_nc(nf90_inq_varid(ncid, 'Tsoil', varid), 'inq Tsoil')
    call check_nc(nf90_get_var(ncid, varid, soil%Tsoil), 'get Tsoil')
    call check_nc(nf90_inq_varid(ncid, 'theta', varid), 'inq theta')
    call check_nc(nf90_get_var(ncid, varid, soil%theta), 'get theta')

    call check_nc(nf90_close(ncid), 'close soil init nc')
  end subroutine load_soil_init_nc

  subroutine init_output_nc(filename, cfg)
    character(len=*), intent(in) :: filename
    type(t_config), intent(in)   :: cfg
    integer :: time_dimid
    character(len=64) :: vnames(NVAR_OUT)
    character(len=128) :: units(NVAR_OUT)
    character(len=128) :: lnames(NVAR_OUT)
    integer :: i, icarbon

    vnames = [character(len=64):: &
         'step', 'SW', 'LW', 'Ta', 'P', 'WS', 'Rn', 'H', 'LE', 'G', &
         'Ts', 'beta', 'W', 'GPP', 'NEE', 'Rleaf', 'Rsoil', 'psi_leaf', 'stress_hydro', &
         'snow_swe', 'GPP_sun', 'GPP_shade', 'albedo_eff']
    units = [character(len=128):: &
         '1', 'W m-2', 'W m-2', 'K', 'mm', 'm s-1', 'W m-2', 'W m-2', 'W m-2', 'W m-2', &
         'K', '1', 'mm', 'umol m-2 s-1', 'umol m-2 s-1', 'umol m-2 s-1', 'umol m-2 s-1', &
         'MPa', '1', 'mm', 'umol m-2 s-1', 'umol m-2 s-1', '1']
    lnames = [character(len=128):: &
         'output step number', 'incoming shortwave radiation', 'incoming longwave radiation', &
         'air temperature', 'precipitation', 'wind speed', 'net radiation', &
         'sensible heat flux', 'latent heat flux', 'ground heat flux', &
         'surface temperature', 'soil moisture stress factor', 'root-zone water storage', &
         'gross primary production', 'net ecosystem exchange', &
         'leaf respiration', 'soil respiration', 'leaf water potential', &
         'hydraulic stress factor', 'snow water equivalent', &
         'sunlit gross primary production', 'shaded gross primary production', &
         'effective surface albedo']

    if (nc_out_id >= 0) call close_output_nc()

    call check_nc(nf90_create(trim(filename), nf90_clobber, nc_out_id), 'create output nc')
    call check_nc(nf90_def_dim(nc_out_id, 'time', nf90_unlimited, time_dimid), 'def time dim')

    call check_nc(nf90_def_var(nc_out_id, 'time', nf90_double, (/time_dimid/), &
         nc_out_time_varid), 'def time var')
    call check_nc(nf90_put_att(nc_out_id, nc_out_time_varid, 'long_name', 'time index'), 'time name')
    call check_nc(nf90_put_att(nc_out_id, nc_out_time_varid, 'units', '1'), 'time units')

    do i = 1, NVAR_OUT
       call check_nc(nf90_def_var(nc_out_id, trim(vnames(i)), nf90_double, &
            (/time_dimid/), nc_out_varids(i)), 'def var '//trim(vnames(i)))
       call check_nc(nf90_put_att(nc_out_id, nc_out_varids(i), 'long_name', trim(lnames(i))), 'long_name')
       call check_nc(nf90_put_att(nc_out_id, nc_out_varids(i), 'units', trim(units(i))), 'put units')
    end do

    call check_nc(nf90_put_att(nc_out_id, nf90_global, 'title', 'LSM model output'), 'title')
    call check_nc(nf90_put_att(nc_out_id, nf90_global, 'Conventions', 'CF-1.8'), 'CF')
    call check_nc(nf90_put_att(nc_out_id, nf90_global, 'site_name', trim(cfg%site_name)), 'site')
    call check_nc(nf90_put_att(nc_out_id, nf90_global, 'dt_seconds', cfg%dt), 'dt')
    icarbon = 0
    if (cfg%carbon_on) icarbon = 1
    call check_nc(nf90_put_att(nc_out_id, nf90_global, 'carbon_on', icarbon), 'carbon')
    call check_nc(nf90_put_att(nc_out_id, nf90_global, 'icanopy', cfg%icanopy), 'icanopy')
    call check_nc(nf90_put_att(nc_out_id, nf90_global, 'isnow', cfg%isnow), 'isnow')
    call check_nc(nf90_put_att(nc_out_id, nf90_global, 'ifrost', cfg%ifrost), 'ifrost')
    call check_nc(nf90_put_att(nc_out_id, nf90_global, 'irad', cfg%irad), 'irad')
    call check_nc(nf90_enddef(nc_out_id), 'enddef output nc')

    nc_out_rec = 0
  end subroutine init_output_nc

  subroutine put_scalar(ncid, varid, val, rec)
    integer, intent(in) :: ncid, varid, rec
    real(dp), intent(in) :: val
    real(dp) :: buf(1)
    integer :: start(1), count1(1)

    buf(1) = val
    start = (/rec/)
    count1 = (/1/)
    call check_nc(nf90_put_var(ncid, varid, buf, start=start, count=count1), 'put scalar')
  end subroutine put_scalar

  subroutine pack_output_values(step, force_sw, force_lw, force_ta, force_p, force_ws, &
                                flux, state, vals)
    integer, intent(in) :: step
    real(dp), intent(in) :: force_sw, force_lw, force_ta, force_p, force_ws
    type(t_flux),  intent(in) :: flux
    type(t_state), intent(in) :: state
    real(dp), intent(out) :: vals(NVAR_OUT)

    vals = (/real(step, dp), force_sw, force_lw, force_ta, force_p, force_ws, &
              flux%Rn, flux%H, flux%LE, flux%G, state%Ts, state%beta, state%W, &
              flux%GPP, flux%NEE, flux%Rleaf, flux%Rsoil, state%psi_leaf, state%stress_hydro, &
              state%snow_swe, flux%GPP_sun, flux%GPP_shade, flux%albedo_eff/)
  end subroutine pack_output_values

  subroutine write_output_nc(step, force_sw, force_lw, force_ta, force_p, force_ws, &
                             flux, state)
    integer, intent(in) :: step
    real(dp), intent(in) :: force_sw, force_lw, force_ta, force_p, force_ws
    type(t_flux),  intent(in) :: flux
    type(t_state), intent(in) :: state
    real(dp) :: vals(NVAR_OUT)
    integer :: i

    if (nc_out_id < 0) then
       write(*,*) 'ERROR: NetCDF output not initialized'
       stop 1
    end if

    nc_out_rec = nc_out_rec + 1
    call pack_output_values(step, force_sw, force_lw, force_ta, force_p, force_ws, flux, state, vals)

    call put_scalar(nc_out_id, nc_out_time_varid, real(nc_out_rec, dp), nc_out_rec)
    do i = 1, NVAR_OUT
       call put_scalar(nc_out_id, nc_out_varids(i), vals(i), nc_out_rec)
    end do
  end subroutine write_output_nc

  subroutine close_output_nc()
    if (nc_out_id >= 0) then
       call check_nc(nf90_close(nc_out_id), 'close output nc')
       nc_out_id = -1
       nc_out_time_varid = -1
       nc_out_varids = -1
       nc_out_rec = 0
    end if
  end subroutine close_output_nc

end module mod_ncio