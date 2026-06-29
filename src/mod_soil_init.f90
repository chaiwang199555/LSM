! Soil temperature and moisture initial profile (separate from atmospheric forcing)
module mod_soil_init
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_soil_init, t_state, t_param, t_config
  use mod_ncio,    only: is_netcdf_file, load_soil_init_nc
  implicit none

contains

  subroutine load_soil_init_txt(filename, soil, ierr)
    character(len=*), intent(in)  :: filename
    type(t_soil_init), intent(out) :: soil
    integer, intent(out) :: ierr
    integer :: unit, ios, n, i
    character(len=512) :: line
    real(dp) :: tsoil, theta
    integer :: layer

    ierr = 0
    unit = 22
    open(unit=unit, file=trim(filename), status='old', action='read', iostat=ios)
    if (ios /= 0) then
       ierr = 1
       return
    end if

    read(unit, '(A)', iostat=ios) line

    n = 0
    do
       read(unit, *, iostat=ios) layer, tsoil, theta
       if (ios /= 0) exit
       n = n + 1
    end do

    if (n <= 0) then
       ierr = 2
       close(unit)
       return
    end if

    soil%nlayer = n
    if (allocated(soil%Tsoil)) deallocate(soil%Tsoil, soil%theta)
    allocate(soil%Tsoil(n), soil%theta(n))

    rewind(unit)
    read(unit, '(A)') line
    do i = 1, n
       read(unit, *, iostat=ios) layer, tsoil, theta
       if (ios /= 0) then
          ierr = 2
          deallocate(soil%Tsoil, soil%theta)
          soil%nlayer = 0
          close(unit)
          return
       end if
       if (tsoil <= miss + 1.0_dp) tsoil = tfrz + 15.0_dp
       if (theta <= miss + 1.0_dp) theta = 0.27_dp
       soil%Tsoil(i) = tsoil
       soil%theta(i) = theta
    end do
    close(unit)
  end subroutine load_soil_init_txt

  subroutine load_soil_init(filename, cfg, soil, ierr)
    character(len=*), intent(in) :: filename
    type(t_config), intent(in)   :: cfg
    type(t_soil_init), intent(out) :: soil
    integer, intent(out) :: ierr

    ierr = 0
    soil%nlayer = 0
    if (len_trim(filename) == 0) then
       ierr = 1
       return
    end if

    if (is_netcdf_file(filename)) then
       call load_soil_init_nc(filename, soil, ierr)
    else
       call load_soil_init_txt(filename, soil, ierr)
    end if

    if (ierr /= 0) return
    if (soil%nlayer /= cfg%nsoil) then
       write(*,'(A,I0,A,I0,A)') 'ERROR: soil init has ', soil%nlayer, &
            ' layers but nsoil = ', cfg%nsoil, ' in namelist'
       ierr = 3
    end if
  end subroutine load_soil_init

  subroutine clip_theta_profile(theta, par)
    real(dp), intent(inout) :: theta(:)
    type(t_param), intent(in) :: par
    integer :: i

    do i = 1, size(theta)
       theta(i) = max(min(theta(i), par%poros), par%theta_r)
    end do
  end subroutine clip_theta_profile

  subroutine apply_soil_init(soil, state, par)
    type(t_soil_init), intent(in) :: soil
    type(t_state), intent(inout) :: state
    type(t_param), intent(in)    :: par
    integer :: n

    n = size(state%Tsoil)
    if (soil%nlayer /= n) then
       write(*,*) 'ERROR: soil init layer count mismatch in apply_soil_init'
       stop 1
    end if

    state%Tsoil = soil%Tsoil
    state%theta = soil%theta
    call clip_theta_profile(state%theta, par)
    state%Ts = state%Tsoil(1)
  end subroutine apply_soil_init

  subroutine print_soil_init_summary(soil)
    type(t_soil_init), intent(in) :: soil
    integer :: i

    write(*,'(A)') '--- Soil initial profile ---'
    write(*,'(A,I0)') '  Layers: ', soil%nlayer
    do i = 1, soil%nlayer
       write(*,'(A,I0,A,F8.2,A,F6.3)') '  Layer ', i, ': Tsoil=', soil%Tsoil(i), ' K  theta=', soil%theta(i)
    end do
    write(*,'(A)') '------------------------------'
  end subroutine print_soil_init_summary

end module mod_soil_init