! Read atmospheric forcing time series (txt or NetCDF)
module mod_forcing
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_forcing, t_forcing_series
  use mod_ncio,    only: is_netcdf_file, load_forcing_nc
  use mod_radiation, only: derive_humidity, fill_sw_geometry
  implicit none

contains

  subroutine load_forcing(filename, series, ierr)
    character(len=*), intent(in)  :: filename
    type(t_forcing_series), intent(out) :: series
    integer, intent(out) :: ierr
    integer :: unit        ! file unit number
    integer :: ios         ! I/O status
    integer :: n           ! record count
    integer :: i
    character(len=512) :: line  ! line buffer
    real(dp) :: sw, lw, ta, p, ws, pa, co2, rh  ! column read buffers
    real(dp) :: cos_sza, sw_beam_frac

    ierr = 0

    if (is_netcdf_file(filename)) then
       call load_forcing_nc(filename, series, ierr)
       return
    end if

    unit = 20
    open(unit=unit, file=trim(filename), status='old', action='read', iostat=ios)
    if (ios /= 0) then
       ierr = 1
       write(*,*) 'ERROR: cannot open forcing file: ', trim(filename)
       return
    end if

    n = 0
    do
       read(unit, '(A)', iostat=ios) line
       if (ios /= 0) exit
       if (len_trim(line) == 0) cycle
       if (line(1:1) == '#') cycle
       n = n + 1
    end do
    rewind(unit)

    series%ntime = n
    allocate(series%data(n))

    i = 0
    do
       read(unit, '(A)', iostat=ios) line
       if (ios /= 0) exit
       if (len_trim(line) == 0) cycle
       if (line(1:1) == '#') cycle
       i = i + 1
       cos_sza = miss
       sw_beam_frac = miss
       read(line, *, iostat=ios) sw, lw, ta, p, ws, pa, co2, rh, cos_sza, sw_beam_frac
       if (ios /= 0) then
          read(line, *, iostat=ios) sw, lw, ta, p, ws, pa, co2, rh
          if (ios /= 0) then
             ierr = 2
             deallocate(series%data)
             close(unit)
             return
          end if
          cos_sza = miss
          sw_beam_frac = miss
       end if
       if (sw <= miss + 1.0_dp) sw = 0.0_dp
       if (lw <= miss + 1.0_dp) lw = 300.0_dp
       if (ta <= miss + 1.0_dp) ta = tfrz + 15.0_dp
       if (p  <= miss + 1.0_dp) p  = 0.0_dp
       if (ws <= miss + 1.0_dp) ws = 2.0_dp
       if (pa <= miss + 1.0_dp) pa = 101325.0_dp
       if (co2<= miss + 1.0_dp) co2= 400.0_dp
       if (rh <= miss + 1.0_dp) rh = 70.0_dp

       series%data(i)%SW  = sw
       series%data(i)%LW  = lw
       series%data(i)%Ta  = ta
       series%data(i)%P   = p
       series%data(i)%WS  = ws
       series%data(i)%PA  = pa
       series%data(i)%CO2 = co2
       series%data(i)%RH  = rh
       series%data(i)%cos_sza = cos_sza
       series%data(i)%sw_beam_frac = sw_beam_frac
       call derive_humidity(series%data(i))
       call fill_sw_geometry(series%data(i))
    end do

    close(unit)
  end subroutine load_forcing

  subroutine get_forcing(series, it, force)
    type(t_forcing_series), intent(in) :: series
    integer, intent(in) :: it
    type(t_forcing), intent(out) :: force
    integer :: idx         ! cyclic index (for spin-up)

    idx = modulo(it - 1, series%ntime) + 1
    force = series%data(idx)
    call derive_humidity(force)
    call fill_sw_geometry(force)
  end subroutine get_forcing

end module mod_forcing