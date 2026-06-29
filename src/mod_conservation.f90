! Energy and water balance conservation checks
module mod_conservation
  use mod_kinds,   only: dp
  use mod_types,   only: t_flux, t_config
  implicit none

contains

  subroutine check_balances(cfg, flux, precip, et, dt, step)
    type(t_config), intent(in) :: cfg
    type(t_flux),   intent(inout) :: flux
    real(dp), intent(in) :: precip, et, dt
    integer, intent(in)  :: step

    flux%wbal_res = precip - et * dt / 3600.0_dp

    if (.not. cfg%check_conservation) return
    if (mod(step, 48) == 0) then
       if (abs(flux%ebal_res) > max(5.0_dp, 50.0_dp * cfg%tol)) then
          write(*,'(A,I0,A,ES12.4,A)') 'WARN step ', step, ': energy residual ', flux%ebal_res, ' W/m2'
       end if
    end if
  end subroutine check_balances

end module mod_conservation