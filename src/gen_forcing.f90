! Generate synthetic half-hourly forcing (Fortran, no Python dependency)
program gen_forcing
  use mod_kinds, only: dp
  use mod_constants, only: miss
  implicit none

  character(len=256) :: outfile  ! output file path
  integer :: unit                ! file unit number
  integer :: n                   ! number of timesteps
  integer :: i                   ! loop index
  integer :: ios                 ! I/O status
  real(dp) :: hour               ! hour of day (h)
  real(dp) :: sw                 ! shortwave radiation (W/m2)
  real(dp) :: lw                 ! longwave radiation (W/m2)
  real(dp) :: ta                 ! air temperature (K)
  real(dp) :: p                  ! precipitation (mm)
  real(dp) :: ws                  ! wind speed (m/s)
  real(dp) :: pa                 ! air pressure (Pa)
  real(dp) :: co2                ! CO2 concentration (ppm)
  real(dp) :: rh                  ! relative humidity (%)
  real(dp) :: cos_sza             ! cosine of solar zenith angle
  real(dp) :: sw_beam_frac        ! direct-beam fraction of total SW

  outfile = 'data/txt/sample_forcing.txt'
  if (command_argument_count() >= 1) call get_command_argument(1, outfile)

  n = 48
  unit = 21
  open(unit=unit, file=trim(outfile), status='replace', action='write', iostat=ios)
  if (ios /= 0) then
     write(*,*) 'ERROR: cannot write ', trim(outfile)
     stop 1
  end if

  write(unit, '(A)') '# SW LW Ta P WS PA CO2 RH cos_sza sw_beam_frac'

  do i = 1, n
     hour = real(i - 1, dp) * 0.5_dp
     if (hour >= 6.0_dp .and. hour <= 18.0_dp) then
        cos_sza = max(0.0_dp, sin(3.14159265_dp * (hour - 6.0_dp) / 12.0_dp))
        sw = 800.0_dp * cos_sza
        sw_beam_frac = min(0.85_dp, 0.45_dp + 0.40_dp * cos_sza)
        rh = 40.0_dp + 25.0_dp * sin(3.14159265_dp * (hour - 6.0_dp) / 12.0_dp)
     else
        cos_sza = 0.0_dp
        sw_beam_frac = 0.0_dp
        sw = 0.0_dp
        rh = 75.0_dp + 10.0_dp * sin(2.0_dp * 3.14159265_dp * hour / 24.0_dp)
     end if
     lw  = 320.0_dp + 20.0_dp * sin(2.0_dp * 3.14159265_dp * hour / 24.0_dp)
     ta  = 288.15_dp + 8.0_dp * sin(2.0_dp * 3.14159265_dp * (hour - 14.0_dp) / 24.0_dp)
     p   = 0.5_dp
     if (hour < 14.0_dp .or. hour > 16.0_dp) p = 0.0_dp
     ws  = 2.0_dp + 0.5_dp * sin(2.0_dp * 3.14159265_dp * hour / 24.0_dp)
     pa  = 85000.0_dp
     co2 = 420.0_dp
     write(unit, '(10(F12.4,1X))') sw, lw, ta, p, ws, pa, co2, rh, cos_sza, sw_beam_frac
  end do

  close(unit)
  write(*,'(A,I0,A,A)') 'Generated ', n, ' timesteps -> ', trim(outfile)
end program gen_forcing