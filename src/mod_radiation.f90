! Net radiation and surface albedo
module mod_radiation
  use mod_kinds,   only: dp
  use mod_constants
  use mod_types,   only: t_forcing, t_param
  implicit none

contains

  function net_radiation(sw_in, lw_in, albedo, emiss, ts) result(rn)
    real(dp), intent(in) :: sw_in, lw_in, albedo, emiss, ts
    real(dp) :: rn      ! net radiation (W/m2)
    real(dp) :: lw_out   ! outgoing longwave radiation (W/m2)

    lw_out = emiss * sigma * ts**4
    rn = sw_in * (1.0_dp - albedo) + lw_in - lw_out
  end function net_radiation

  function dRn_dTs(emiss, ts) result(drn)
    real(dp), intent(in) :: emiss, ts
    real(dp) :: drn      ! Jacobian dRn/dTs (W/m2/K)

    drn = -4.0_dp * emiss * sigma * ts**3
  end function dRn_dTs

  function sat_vapor_pressure(tk) result(es)
    real(dp), intent(in) :: tk
    real(dp) :: es        ! saturation vapor pressure (Pa)
    real(dp) :: tc        ! temperature in Celsius (degC)

    tc = tk - tfrz
    es = 611.2_dp * exp(17.67_dp * tc / (tc + 243.5_dp))
  end function sat_vapor_pressure

  function qs_from_T_P(tk, pa) result(qs)
    real(dp), intent(in) :: tk, pa
    real(dp) :: qs        ! saturation specific humidity (kg/kg)
    real(dp) :: es

    es = sat_vapor_pressure(tk)
    qs = eps * es / (pa - (1.0_dp - eps) * es)
  end function qs_from_T_P

  function vpd_from_rh(ta, rh) result(vpd)
    real(dp), intent(in) :: ta, rh
    real(dp) :: vpd
    real(dp) :: es, ea, rh_clip

    es = sat_vapor_pressure(ta)
    rh_clip = min(max(rh, 0.0_dp), 100.0_dp)
    ea = rh_clip / 100.0_dp * es
    vpd = max(es - ea, 0.0_dp)
  end function vpd_from_rh

  subroutine derive_humidity(force)
    type(t_forcing), intent(inout) :: force
    real(dp) :: rh

    if (force%RH <= miss + 1.0_dp .or. force%RH < 0.0_dp) force%RH = 70.0_dp
    rh = min(max(force%RH, 0.0_dp), 100.0_dp)
    force%RH = rh
    force%VPD = vpd_from_rh(force%Ta, rh)
  end subroutine derive_humidity

  subroutine derive_humidity_from_qa(ta, qa, pa, rh, vpd)
    real(dp), intent(in) :: ta, qa, pa
    real(dp), intent(out) :: rh, vpd
    real(dp) :: es, ea

    es = sat_vapor_pressure(ta)
    ea = qa * pa / (eps + (1.0_dp - eps) * qa)
    ea = min(max(ea, 1.0_dp), es)
    rh = min(max(100.0_dp * ea / es, 0.0_dp), 100.0_dp)
    vpd = max(es - ea, 0.0_dp)
  end subroutine derive_humidity_from_qa

  function qa_from_rh(ta, rh, pa) result(qa)
    real(dp), intent(in) :: ta, rh, pa
    real(dp) :: qa
    real(dp) :: ea
    real(dp) :: es

    es = sat_vapor_pressure(ta)
    ea = rh / 100.0_dp * es
    qa = eps * ea / (pa - (1.0_dp - eps) * ea)
  end function qa_from_rh

  function specific_humidity(force) result(qa)
    type(t_forcing), intent(in) :: force
    real(dp) :: qa

    qa = qa_from_rh(force%Ta, force%RH, force%PA)
  end function specific_humidity

  subroutine infer_sw_geometry(sw, cos_sza, sw_beam_frac)
    real(dp), intent(in)  :: sw
    real(dp), intent(out) :: cos_sza, sw_beam_frac

    if (sw > 1.0_dp) then
       cos_sza = max(0.001_dp, min(1.0_dp, sqrt(sw / 850.0_dp)))
       sw_beam_frac = min(0.85_dp, 0.45_dp + 0.40_dp * cos_sza)
    else
       cos_sza = 0.001_dp
       sw_beam_frac = 0.0_dp
    end if
  end subroutine infer_sw_geometry

  subroutine fill_sw_geometry(force)
    type(t_forcing), intent(inout) :: force

    if (force%cos_sza > miss + 1.0_dp .and. force%sw_beam_frac >= 0.0_dp &
         .and. force%sw_beam_frac <= 1.0_dp) then
       force%cos_sza = max(0.001_dp, min(1.0_dp, force%cos_sza))
       force%sw_beam_frac = max(0.0_dp, min(1.0_dp, force%sw_beam_frac))
    else
       call infer_sw_geometry(force%SW, force%cos_sza, force%sw_beam_frac)
    end if
  end subroutine fill_sw_geometry

end module mod_radiation