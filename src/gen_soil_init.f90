! Generate sample soil initial profile (layer Tsoil theta)
program gen_soil_init
  use mod_kinds, only: dp
  implicit none

  character(len=256) :: outfile
  character(len=64)  :: arg
  integer :: unit, ios, i, nsoil
  real(dp) :: tsoil, theta, zmid

  outfile = 'data/txt/sample_soil_init.txt'
  if (command_argument_count() >= 1) call get_command_argument(1, outfile)
  nsoil = 6
  if (command_argument_count() >= 2) then
     call get_command_argument(2, arg)
     read(arg, *, iostat=ios) nsoil
     if (ios /= 0) nsoil = 6
  end if

  unit = 23
  open(unit=unit, file=trim(outfile), status='replace', action='write', iostat=ios)
  if (ios /= 0) then
     write(*,*) 'ERROR: cannot write ', trim(outfile)
     stop 1
  end if

  write(unit, '(A)') '# layer Tsoil theta'
  write(unit, '(A)') '# layer 1 = surface; Tsoil in K; theta in m3/m3'

  do i = 1, nsoil
     zmid = real(i - 1, dp) * 0.35_dp
     tsoil = 288.15_dp - 0.8_dp * zmid
     theta = 0.30_dp - 0.01_dp * real(i - 1, dp)
     write(unit, '(I4,2(1X,F12.4))') i, tsoil, theta
  end do

  close(unit)
  write(*,'(A,I0,A,A)') 'Generated ', nsoil, ' soil layers -> ', trim(outfile)
end program gen_soil_init