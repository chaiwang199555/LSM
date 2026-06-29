! Soil carbon: Q10 baseline or simplified microbial (MIMICS-style)
module mod_soil_carbon
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_state, t_param, t_config, t_flux
  use mod_physics, only: SCARB_Q10, SCARB_MICROBIAL
  implicit none

contains

  subroutine init_soil_carbon(state, par)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par

    state%Clit = 500.0_dp
    state%Cmic = 50.0_dp
    state%Csom = 5000.0_dp
  end subroutine init_soil_carbon

  subroutine soil_respiration(state, par, cfg, flux)
    type(t_state),  intent(in)    :: state
    type(t_param),  intent(in)    :: par
    type(t_config), intent(in)    :: cfg
    type(t_flux),   intent(inout) :: flux
    real(dp) :: t_ref      ! reference temperature (K)
    real(dp) :: f_t        ! Q10 temperature factor
    real(dp) :: decomp     ! litter decomposition rate (umol/m2/s)
    real(dp) :: mic_resp   ! microbial respiration (umol/m2/s)

    t_ref = tfrz + 15.0_dp
    f_t = 2.0_dp**((state%Tsoil(1) - t_ref) / 10.0_dp)

    if (cfg%isoilcarbon == SCARB_MICROBIAL) then
       decomp = par%k_litter * state%Clit * f_t * state%beta
       mic_resp = 0.1_dp * state%Cmic * f_t
       flux%Rsoil = decomp + mic_resp + par%k_som * state%Csom * f_t * 0.1_dp
    else
       flux%Rsoil = 2.0_dp * f_t * state%beta
    end if
    flux%RECO = flux%Rleaf + flux%Rsoil
    flux%NEE  = flux%RECO - flux%GPP
  end subroutine soil_respiration

  subroutine update_soil_carbon(state, par, cfg, gpp, dt)
    type(t_state), intent(inout) :: state
    type(t_param),   intent(in)    :: par
    type(t_config),  intent(in)    :: cfg
    real(dp), intent(in) :: gpp, dt
    real(dp) :: decomp     ! decomposition flux (gC/m2/day)
    real(dp) :: growth     ! microbial growth (gC/m2/day)
    real(dp) :: t_ref
    real(dp) :: f_t

    if (cfg%isoilcarbon /= SCARB_MICROBIAL) return

    t_ref = tfrz + 15.0_dp
    f_t = 2.0_dp**((state%Tsoil(1) - t_ref) / 10.0_dp)
    decomp = par%k_litter * state%Clit * f_t * max(state%beta, 0.1_dp)
    growth = par%cue_micro * decomp
    state%Clit = max(state%Clit - decomp * dt / 86400.0_dp + par%litterfall * dt / 86400.0_dp, 1.0_dp)
    state%Cmic = max(state%Cmic + (growth - 0.1_dp * state%Cmic * f_t) * dt / 86400.0_dp, 1.0_dp)
    state%Csom = max(state%Csom + (1.0_dp - par%cue_micro) * decomp * 0.5_dp * dt / 86400.0_dp, 100.0_dp)
  end subroutine update_soil_carbon

end module mod_soil_carbon