! Derived types for LSM state, forcing, parameters, and configuration
module mod_types
  use mod_kinds, only: dp
  use mod_constants, only: miss
  implicit none

  type :: t_forcing
     real(dp) :: SW    = 0.0_dp    ! incoming shortwave radiation (W/m2)
     real(dp) :: LW    = 0.0_dp    ! incoming longwave radiation (W/m2)
     real(dp) :: Ta    = 0.0_dp    ! air temperature (K)
     real(dp) :: P     = 0.0_dp    ! precipitation (mm)
     real(dp) :: WS    = 0.0_dp    ! wind speed (m/s)
     real(dp) :: RH    = 70.0_dp   ! relative humidity (%)
     real(dp) :: PA    = 0.0_dp    ! air pressure (Pa)
     real(dp) :: CO2   = 400.0_dp  ! CO2 concentration (ppm)
     real(dp) :: VPD   = 0.0_dp    ! vapor pressure deficit (Pa); derived from RH
     real(dp) :: cos_sza      = miss  ! cosine of solar zenith angle (1=overhead)
     real(dp) :: sw_beam_frac = miss  ! direct-beam fraction of total SW (0-1)
  end type t_forcing

  type :: t_forcing_series
     integer :: ntime = 0                    ! number of forcing time steps
     type(t_forcing), allocatable :: data(:) ! forcing at each time step
  end type t_forcing_series

  type :: t_soil_init
     integer :: nlayer = 0                       ! number of soil layers in file
     real(dp), allocatable :: Tsoil(:)           ! soil temperature per layer (K)
     real(dp), allocatable :: theta(:)           ! volumetric soil moisture per layer (m3/m3)
  end type t_soil_init

  type :: t_state
     real(dp) :: Ts      = 288.0_dp    ! surface/soil skin temperature (K)
     real(dp) :: Tc      = 288.0_dp    ! canopy temperature (K)
     real(dp) :: LAI     = 2.0_dp      ! leaf area index
     real(dp) :: beta    = 1.0_dp      ! soil moisture stress factor (0-1)
     real(dp) :: W       = 100.0_dp    ! root-zone water storage (mm)
     real(dp) :: stress_hydro = 1.0_dp ! hydraulic stress factor (0-1)
     real(dp) :: psi_leaf  = -0.5_dp   ! leaf water potential (MPa)
     real(dp) :: psi_xylem = -0.3_dp    ! xylem water potential (MPa)
     real(dp) :: psi_root  = -0.2_dp   ! root water potential (MPa)
     real(dp) :: k_xylem   = 1.0_dp    ! xylem hydraulic conductance
     real(dp) :: Vcmax25  = 50.0_dp    ! acclimated Vcmax at 25 C (umol/m2/s)
     real(dp) :: Rd25     = 2.0_dp     ! acclimated dark respiration at 25 C (umol/m2/s)
     real(dp) :: T_accl   = 298.15_dp  ! running-mean temperature for acclimation (K)
     real(dp) :: n_accl   = 0.0_dp     ! accumulated acclimation days
     real(dp) :: Clit     = 500.0_dp   ! litter carbon pool (gC/m2)
     real(dp) :: Cmic     = 50.0_dp    ! microbial carbon pool (gC/m2)
     real(dp) :: Csom     = 5000.0_dp  ! soil organic matter pool (gC/m2)
     real(dp) :: Nlab     = 5.0_dp     ! labile nitrogen pool (gN/m2)
     real(dp) :: Plab     = 0.5_dp     ! labile phosphorus pool (gP/m2)
     real(dp) :: nu_stress = 1.0_dp    ! nitrogen limitation factor (0-1)
     real(dp) :: np_stress = 1.0_dp    ! phosphorus limitation factor (0-1)
     real(dp), allocatable :: Tsoil(:)     ! soil temperature per layer (K)
     real(dp), allocatable :: theta(:)     ! volumetric soil moisture per layer (m3/m3)
     real(dp), allocatable :: psi_soil(:)  ! soil matric potential per layer (MPa)
     real(dp), allocatable :: dz(:)        ! layer thickness (m)
     real(dp), allocatable :: zmid(:)      ! layer mid-point depth (m)
     real(dp), allocatable :: root_frac(:) ! root uptake weight per layer
     real(dp), allocatable :: theta_ice(:) ! volumetric soil ice per layer (m3/m3)
     real(dp) :: LAI_sun   = 0.0_dp       ! sunlit leaf area index
     real(dp) :: LAI_shade = 0.0_dp       ! shaded leaf area index
     real(dp) :: snow_swe  = 0.0_dp       ! snow water equivalent (mm)
     real(dp) :: snow_depth = 0.0_dp      ! snow depth (m)
     real(dp) :: snow_T    = 268.0_dp     ! snow surface temperature (K)
     real(dp) :: snow_albedo = 0.85_dp    ! snow albedo
     real(dp) :: snow_age  = 0.0_dp       ! snow age for albedo decay (days)
     logical  :: snow_present = .false.   ! snow on ground flag
  end type t_state

  type :: t_param
     real(dp) :: z0       = 0.1_dp     ! roughness length (m)
     real(dp) :: zdisp    = 0.5_dp     ! zero-plane displacement height (m)
     real(dp) :: albedo   = 0.2_dp     ! surface albedo
     real(dp) :: emiss    = 0.98_dp    ! surface emissivity
     real(dp) :: hc       = 2.0_dp     ! canopy height (m)
     real(dp) :: lai      = 2.0_dp     ! leaf area index
     real(dp) :: gs_max   = 0.01_dp    ! maximum stomatal conductance, Jarvis (m/s)
     real(dp) :: g0       = 0.01_dp    ! minimum stomatal conductance, Medlyn (m/s)
     real(dp) :: g1       = 2.0_dp     ! Medlyn slope parameter
     real(dp) :: Vcmax    = 50.0_dp    ! baseline maximum carboxylation rate (umol/m2/s)
     real(dp) :: Jmax     = 100.0_dp   ! baseline maximum electron transport rate (umol/m2/s)
     real(dp) :: Rd25_base = 2.0_dp    ! baseline dark respiration at 25 C (umol/m2/s)
     real(dp) :: Ksat     = 1.0e-5_dp  ! saturated hydraulic conductivity (m/s)
     real(dp) :: poros    = 0.45_dp    ! soil porosity
     real(dp) :: theta_r  = 0.05_dp    ! residual soil moisture
     real(dp) :: alpha_vg = 3.5_dp     ! van Genuchten alpha parameter
     real(dp) :: n_vg     = 1.5_dp     ! van Genuchten n parameter
     real(dp) :: W_field  = 150.0_dp   ! field capacity storage (mm)
     real(dp) :: W_wilt   = 30.0_dp    ! wilting-point storage (mm)
     real(dp) :: soil_heat_cap = 2.0e6_dp ! soil volumetric heat capacity (J/m3/K)
     real(dp) :: soil_cond    = 1.5_dp    ! soil thermal conductivity (W/m/K)
     real(dp) :: skin_heat_cap = 2.0e4_dp ! surface skin heat capacity (J/m2/K)
     real(dp) :: p50_xylem = -2.0_dp   ! xylem pressure at 50% conductivity loss (MPa)
     real(dp) :: ck_xylem  = 0.5_dp    ! xylem vulnerability curve shape parameter
     real(dp) :: kx_max    = 1.0e-5_dp ! maximum xylem conductance
     real(dp) :: kr_max    = 1.0e-5_dp ! maximum root-soil interface conductance
     real(dp) :: psi50_leaf = -1.5_dp  ! leaf water potential at 50% stomatal closure (MPa)
     real(dp) :: gw_depth  = 2.0_dp    ! groundwater table depth (m)
     real(dp) :: cue_micro = 0.55_dp   ! microbial carbon use efficiency
     real(dp) :: k_litter  = 0.01_dp   ! litter decomposition rate (/day)
     real(dp) :: k_som     = 1.0e-4_dp ! SOM decomposition rate (/day)
     real(dp) :: litterfall = 0.5_dp   ! litterfall input (gC/m2/day)
     real(dp) :: n_uptake_max = 0.05_dp ! maximum nitrogen uptake (gN/m2/day)
     real(dp) :: p_uptake_max = 0.005_dp ! maximum phosphorus uptake (gP/m2/day)
     real(dp) :: cn_litter = 50.0_dp   ! litter C:N ratio
     real(dp) :: cp_litter = 500.0_dp  ! litter C:P ratio
     real(dp) :: clumping  = 0.7_dp    ! canopy clumping index
     real(dp) :: ext_coeff = 0.5_dp    ! canopy extinction coefficient
     real(dp) :: snow_emiss = 0.98_dp  ! snow emissivity
     real(dp) :: fresh_snow_alb = 0.85_dp ! fresh snow albedo
     real(dp) :: rain_snow_thresh = 273.15_dp ! rain/snow partition (K)
     real(dp) :: soil_sat_ice = 0.4_dp ! max volumetric ice fraction
  end type t_param

  type :: t_config
     character(len=256) :: forcing_file   = 'data/nc/sample_forcing.nc'   ! atmospheric forcing
     character(len=256) :: soil_init_file = 'data/nc/sample_soil_init.nc' ! soil T/theta initial profile
     character(len=256) :: output_file    = 'results/nc/output.nc'      ! model output file path
     character(len=64)  :: site_name    = 'test_site'              ! site name
     real(dp) :: dt       = 1800.0_dp    ! model time step (s)
     integer  :: nspinup  = 1            ! number of spin-up cycles
     integer  :: nsoil    = 6            ! number of soil layers
     logical  :: carbon_on = .false.     ! enable carbon cycle
     integer  :: max_iter  = 20          ! max energy-balance iterations
     real(dp) :: tol       = 0.01_dp     ! energy-balance tolerance (W/m2)
     logical  :: check_conservation = .true. ! check mass/energy conservation
     integer  :: istomatal = 2             ! stomatal scheme (1=Jarvis, 2=Medlyn)
     integer  :: ihydro    = 2             ! water stress scheme (1=beta, 2=PHS)
     integer  :: isoilwater = 2            ! soil water scheme (1=bucket, 2=Richards)
     integer  :: isoilcarbon = 2           ! soil carbon scheme (1=Q10, 2=microbial)
     integer  :: icnp      = 3             ! nutrient coupling (0=off, 1=C, 2=CN, 3=CNP)
     logical  :: eeo_on    = .true.        ! enable EEO photosynthetic acclimation
     real(dp) :: accl_days = 30.0_dp       ! acclimation time scale (days)
     integer  :: icanopy   = 2             ! canopy: 1=big-leaf, 2=two-leaf
     integer  :: isnow     = 1             ! snow: 0=off, 1=on
     integer  :: ifrost    = 1             ! permafrost phase change: 0=off, 1=on
     integer  :: irad      = 1             ! radiation: 0=offline, 1=two-stream, 2=GCM
  end type t_config

  type :: t_flux
     real(dp) :: Rn  = 0.0_dp      ! net radiation (W/m2)
     real(dp) :: H   = 0.0_dp      ! sensible heat flux (W/m2)
     real(dp) :: LE  = 0.0_dp      ! latent heat flux (W/m2)
     real(dp) :: G   = 0.0_dp      ! ground heat flux (W/m2), positive upward
     real(dp) :: ET  = 0.0_dp      ! evapotranspiration (kg/m2/s)
     real(dp) :: ra  = 50.0_dp     ! aerodynamic resistance (s/m)
     real(dp) :: rs  = 100.0_dp    ! canopy/stomatal resistance (s/m)
     real(dp) :: gs  = 0.0_dp      ! stomatal conductance (m/s)
     real(dp) :: GPP = 0.0_dp      ! gross primary production (umol/m2/s)
     real(dp) :: RECO = 0.0_dp     ! ecosystem respiration (umol/m2/s)
     real(dp) :: NEE  = 0.0_dp     ! net ecosystem exchange (umol/m2/s)
     real(dp) :: Rleaf = 0.0_dp    ! autotrophic/leaf respiration (umol/m2/s)
     real(dp) :: Rsoil = 0.0_dp    ! heterotrophic/soil respiration (umol/m2/s)
     real(dp) :: An   = 0.0_dp     ! leaf net assimilation rate (umol/m2/s)
     real(dp) :: Ci   = 0.0_dp     ! intercellular CO2 concentration (ppm)
     real(dp) :: wbal_res = 0.0_dp ! water balance residual (mm)
     real(dp) :: ebal_res = 0.0_dp ! energy balance residual (W/m2)
     real(dp) :: GPP_sun  = 0.0_dp ! sunlit GPP (umol/m2/s)
     real(dp) :: GPP_shade = 0.0_dp ! shaded GPP (umol/m2/s)
     real(dp) :: gs_sun   = 0.0_dp ! sunlit stomatal conductance (m/s)
     real(dp) :: gs_shade = 0.0_dp ! shaded stomatal conductance (m/s)
     real(dp) :: PAR_sun  = 0.0_dp ! PAR absorbed by sun leaves (W/m2)
     real(dp) :: PAR_shade = 0.0_dp ! PAR absorbed by shade leaves (W/m2)
     real(dp) :: Rn_canopy = 0.0_dp ! canopy net radiation (W/m2)
     real(dp) :: Rn_ground = 0.0_dp ! ground/snow net radiation (W/m2)
     real(dp) :: SW_abs_canopy = 0.0_dp ! canopy absorbed SW (W/m2)
     real(dp) :: SW_abs_ground = 0.0_dp ! ground absorbed SW (W/m2)
     real(dp) :: SW_trans_ground = 0.0_dp ! SW transmitted to ground (W/m2)
     real(dp) :: albedo_eff = 0.2_dp ! effective surface albedo
     real(dp) :: tau_momentum = 0.0_dp ! momentum flux (N/m2)
     real(dp) :: LE_soil = 0.0_dp  ! soil evaporation latent heat (W/m2)
     real(dp) :: H_canopy = 0.0_dp ! canopy sensible heat flux (W/m2)
     real(dp) :: LE_canopy = 0.0_dp ! canopy latent heat flux (W/m2)
     real(dp) :: melt_rate = 0.0_dp ! snow melt rate (mm/s)
  end type t_flux

  type :: t_couple_boundary
     real(dp) :: SW_down  = 0.0_dp
     real(dp) :: LW_down  = 0.0_dp
     real(dp) :: Ta       = 0.0_dp
     real(dp) :: qa       = 0.0_dp
     real(dp) :: Pa       = 0.0_dp
     real(dp) :: u        = 0.0_dp
     real(dp) :: P        = 0.0_dp
     real(dp) :: CO2      = 400.0_dp
     real(dp) :: H_up     = 0.0_dp
     real(dp) :: LE_up    = 0.0_dp
     real(dp) :: tau_up   = 0.0_dp
     real(dp) :: Ts_up    = 0.0_dp
     real(dp) :: albedo_up = 0.2_dp
     real(dp) :: emiss_up = 0.98_dp
  end type t_couple_boundary

end module mod_types