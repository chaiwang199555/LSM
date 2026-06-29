! GCM-LSM coupling boundary interface
module mod_gcm_coupling
  use mod_kinds,   only: dp
  use mod_types,   only: t_forcing, t_flux, t_state, t_param, t_config, t_couple_boundary
  use mod_physics, only: RAD_GCM_CPL
  use mod_radiation, only: specific_humidity, derive_humidity_from_qa
  implicit none

contains

  subroutine forcing_to_couple(force, bnd)
    type(t_forcing), intent(in)       :: force
    type(t_couple_boundary), intent(out) :: bnd
    bnd%SW_down = force%SW
    bnd%LW_down = force%LW
    bnd%Ta = force%Ta
    bnd%Pa = force%PA
    bnd%u = force%WS
    bnd%P = force%P
    bnd%CO2 = force%CO2
    bnd%qa = specific_humidity(force)
  end subroutine forcing_to_couple

  subroutine couple_to_forcing(bnd, force)
    type(t_couple_boundary), intent(in) :: bnd
    type(t_forcing), intent(out) :: force
    force%SW = bnd%SW_down
    force%LW = bnd%LW_down
    force%Ta = bnd%Ta
    force%PA = bnd%Pa
    force%WS = bnd%u
    force%P = bnd%P
    force%CO2 = bnd%CO2
    call derive_humidity_from_qa(bnd%Ta, bnd%qa, bnd%Pa, force%RH, force%VPD)
  end subroutine couple_to_forcing

  subroutine couple_in(bnd, cfg, force)
    type(t_couple_boundary), intent(in) :: bnd
    type(t_config), intent(in) :: cfg
    type(t_forcing), intent(inout) :: force
    if (cfg%irad == RAD_GCM_CPL) then
       call couple_to_forcing(bnd, force)
    end if
  end subroutine couple_in

  subroutine couple_out(flux, state, par, bnd)
    type(t_flux), intent(in) :: flux
    type(t_state), intent(in) :: state
    type(t_param), intent(in) :: par
    type(t_couple_boundary), intent(out) :: bnd
    bnd%H_up = flux%H
    bnd%LE_up = flux%LE
    bnd%tau_up = flux%tau_momentum
    bnd%Ts_up = state%Ts
    bnd%albedo_up = flux%albedo_eff
    if (state%snow_present) then
       bnd%emiss_up = par%snow_emiss
    else
       bnd%emiss_up = par%emiss
    end if
  end subroutine couple_out

end module mod_gcm_coupling