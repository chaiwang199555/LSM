! LSM main program: read namelist and run driver
program lsm_main
  use mod_types,  only: t_config, t_param
  use mod_io,     only: read_namelist
  use mod_driver, only: run_lsm
  implicit none

  type(t_config) :: cfg   ! run configuration
  type(t_param)  :: par    ! site parameters
  integer :: ierr         ! namelist read error code

  write(*,'(A)') '=== Single-Point Land Surface Model (LSM) ==='

  call read_namelist(cfg, par, ierr)
  call run_lsm(cfg, par)

end program lsm_main