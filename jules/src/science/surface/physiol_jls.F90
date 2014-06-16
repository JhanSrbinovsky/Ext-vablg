! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Description:
! Subroutine to calculate gridbox mean values of surface conductance
! and carbon fluxes. Also returns net primary productivity, leaf
! turnover and wood respiration of each plant functional type for
! driving TRIFFID.

!**********************************************************************
SUBROUTINE physiol (land_pts,land_index                           &
,                   nshyd,ntiles,tile_pts,tile_index,l_aggregate  &
,                   dim_cs1, dim_cs2                              &
,                   co2,co2_3d,co2_dim_len                        &
,                   co2_dim_row,l_co2_interactive                 &
,                   l_triffid, l_q10                              &
,                   can_model,cs,frac,fland,ht,ipar,lai,pstar,q1  &
,                   sthu,tsoil,tstar_tile                         &
,                   v_crit,v_sat,v_wilt,wind,z0_tile,z1,o3        &
,                   canhc_tile,vfrac_tile,emis_tile,emis_soil     &
,                   flake,g_leaf,gs,gs_tile                       &
,                   gpp,gpp_ft,npp,npp_ft,resp_p,resp_p_ft        &
,                   resp_s,resp_w_ft,smct,wt_ext_tile,fsmc,wt_ext &
,                   ra,albsoil,cos_zenith_angle                   &
,                   can_rad_mod,ilayers,flux_o3_ft,fo3_ft)

USE atm_fields_bounds_mod
USE theta_field_sizes, ONLY : t_i_length, t_j_length

USE c_densty

USE nstypes, ONLY :                                               &
!      imported scalars with intent(in)
   lake, npft, ntype, soil, ice, urban_canyon, urban_roof

USE nvegparm

USE soil_param, ONLY : dzsoil
USE surf_param, ONLY : diff_frac

USE pftparm

USE urban_param, ONLY : emisr, emisw, hwr, emiss,                 &
   diffus_road, diffus_wall, diffus_roof, cap_road, cap_wall,     &
   cap_roof, dz_roof_p, omega_day, gs_c, gs_rf, ch_c, ch_rf,      &
   vf_c, vf_rf, emis_c, emis_rf

USE switches, ONLY : l_spec_albedo, l_albedo_obs 

USE switches_urban, ONLY :                                        &
   l_urban2t, l_moruses_emissivity, l_moruses_storage,            &
   l_moruses_storage_thin

USE bvoc_vars, ONLY : isoprene, isoprene_ft,                      &
                      terpene,  terpene_ft,                       &
                      methanol, methanol_ft,                      &
                      acetone,  acetone_ft

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

#if defined(UM_JULES)
USE PrintStatus_mod
#endif
IMPLICIT NONE

INTEGER                                                           &
 land_pts                                                         &
                            ! IN Number of land points to be
!                                 !    processed.
,land_index(land_pts)                                             &
                            ! IN Index of land points on the
!                                 !    P-grid.
,co2_dim_len                                                      &
                            ! IN Length of a CO2 field row.
,co2_dim_row                                                      &
                            ! IN Number of CO2 field rows.
,nshyd                                                            &
                            ! IN Number of soil moisture
!                                 !    levels.
,ntiles                                                           &
                            ! IN Number of surface tiles.
,tile_pts(ntype)                                                  &
                            ! IN Number of land points which
!                                 !    include the nth surface type.
,tile_index(land_pts,ntype)                                       &
                            ! IN Indices of land points which
!                                 !    include the nth surface type.
,can_model                                                        &
                            ! IN Swith for thermal vegetation
!                                 !    canopy
,dim_cs1, dim_cs2           ! soil carbon dimensions

LOGICAL                                                           &
        l_co2_interactive                                         &
                            ! switch for 3D CO2 field
,       l_aggregate                                               &
                            ! IN Logical to set aggregate
                            !    surface scheme
,       l_triffid                                                 &
                            ! TRUE if using TRIFFID
,       l_q10               ! TRUE if using Q10 for soil resp

LOGICAL ::                  &
   firstcall = .TRUE.

INTEGER                                                           &
 can_rad_mod                                                      &
!                                !Switch for canopy radiation model
,ilayers                                                          &
!                                !No of layers in canopy radiation model
,albpft_call                     ! Flag for albpft, scaling to obs 

REAL                                                              &
 alb_type_dummy(land_pts,ntype,4)                                 &
!                                 ! WORK Dummy argument for albedo
!                                 ! subroutine
,fapar_dir(land_pts,npft,ilayers)                                 &
!                                 ! WORK Profile of absorbed PAR -
!                                 ! Direct beam
,fapar_dif(land_pts,npft,ilayers)                                 &
!                                 ! WORK Profile of absorbed PAR -
!                                 ! Diffuse beam
,fapar_dir_tot(land_pts,npft)                                     &
!                                 ! WORK Total absorbed PAR -
!                                 ! Direct beam
,fapar_dif_tot(land_pts,npft)                                     &
!                                 ! WORK Total absorbed PAR -
!                                 ! Diffuse beam
,faparv(land_pts,ilayers)                                         &
!                                 ! WORK Profile of absorbed PAR.
,fapar_dif2dif(land_pts,npft,ilayers)                             &
!                                 ! WORK Profile of absorbed PAR
!                                 ! - Diffuse (fraction/LAI)
,fapar_dir2dif(land_pts,npft,ilayers)                             &
!                                 ! WORK Profile of scattered PAR
!                                 ! -direct to diffuse (fraction/LAI)
,fapar_dir2dir(land_pts,npft,ilayers)                             &
!                                 ! WORK Profile of absorbed only
!                                 ! direct PAR (fraction/LAI)
,fapar_shd(land_pts,ilayers)                                      &
!                                 ! WORK fraction of total absorbed PAR
!                                 ! by shaded leaves
,fapar_sun(land_pts,ilayers)                                      &
!                                 ! WORK fraction of total absorbed PAR
!                                 ! by sunlit leaves
,fsun(land_pts,npft,ilayers)
!                                 ! WORK fraction of leaves that are
!                                 ! sunlit



REAL                                                              &
 co2                                                              &
                            ! IN Atmospheric CO2 concentration
,co2_3d(co2_dim_len,co2_dim_row)                                  &
!                                 ! IN 3D atmos CO2 concentration
!                                 !    (kg CO2/kg air).
,cs(land_pts,dim_cs1)                                             &
                           ! IN Soil carbon (kg C/m2).
,veg_frac(dim_cs2)                                                &
                           ! WORK vegetated fraction of gridbox
,frac(land_pts,ntype)                                             &
                            ! IN Surface type fractions.
,fland(land_pts)                                                  &
                            ! IN Land fraction on land tiles
,ht(land_pts,npft)                                                &
                            ! IN Canopy height (m).
,ipar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)        &
                            ! IN Incident PAR (W/m2).
,lai(land_pts,npft)                                               &
                            ! IN Leaf area index.
,pstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                            ! IN Surface pressure (Pa).
,q1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)          &
                            ! IN Specific humidity at level 1
!                                 !    (kg H2O/kg air).
,sthu(land_pts,nshyd)                                             &
                            ! IN Soil moisture content in each
!                                 !    layer as a fraction of saturation
,tsoil(land_pts)                                                  &
                            ! IN Soil temperature (K).
,tstar_tile(land_pts,ntiles)                                      &
                            ! IN Tile surface temperatures (K).
,v_crit(land_pts,nshyd)                                           &
                            ! IN Volumetric soil moisture
!                                 !    concentration above which
!                                 !    stomata are not sensitive
!                                 !    to soil water (m3 H2O/m3 soil).
,v_sat(land_pts,nshyd)                                            &
                            ! IN Volumetric soil moisture
!                                 !    concentration at saturation
!                                 !    (m3 H2O/m3 soil).
,v_wilt(land_pts,nshyd)                                           &
                            ! IN Volumetric soil moisture
!                                 !    concentration below which
!                                 !    stomata close (m3 H2O/m3 soil).

,wind(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)        &
                            ! IN Windspeed (m/s).
,z0_tile(land_pts,ntiles)                                         &
                            ! IN Tile roughness lengths (m).
,z1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)          &
                            ! IN Windspeed reference height(m).
,o3(land_pts)                                                     &
                            ! IN Surface ozone concentration (ppb).

,gs(land_pts)                                                     &
                            ! INOUT Gridbox mean surface
!                                 !       conductance (m/s).
,wt_ext(land_pts,nshyd)                                           &
                            ! OUT Gridbox-mean WT_EXT.
!                                       NB This is only non-zero if
!                                          l_aggregate=TRUE.
,ra(land_pts)                                                     &
                            ! OUT Aerodynamic resistance (s/m).
,albsoil(land_pts)                                                &
!                                  ! Soil albedo.
,cos_zenith_angle(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
!                                  ! Cosine of the zenith angle

REAL                                                              &
 canhc_tile(land_pts,ntiles)                                      &
                            ! OUT Areal heat capacity of canopy
!                                 !     for land tiles (J/K/m2).
,flake(land_pts,ntiles)                                           &
                            ! OUT Lake fraction.
!CABLE: kdcorbin, 11/10 - changed from NPFT
,g_leaf(land_pts,ntiles)                                               &
                            ! OUT Leaf turnover rate (/360days).
,gs_tile(land_pts,ntiles)                                         &
                            ! OUT Surface conductance for
!                                 !     land tiles (m/s).
,gpp(land_pts)                                                    &
                            ! OUT Gridbox mean gross primary
!                                 !     productivity (kg C/m2/s).
,gpp_ft(land_pts,ntiles)                                               &
                            ! OUT Gross primary productivity
!                                 !     (kg C/m2/s).
,npp(land_pts)                                                    &
                            ! OUT Gridbox mean net primary
!                                 !     productivity (kg C/m2/s).
,npp_ft(land_pts,ntiles)                                               &
                            ! OUT Net primary productivity
!                                 !     (kg C/m2/s).
,resp_p(land_pts)                                                 &
                            ! OUT Gridbox mean plant respiration
!                                 !     (kg C/m2/s).
,resp_p_ft(land_pts,ntiles)                                            &
                            ! OUT Plant respiration (kg C/m2/s).
,resp_s(land_pts,dim_cs1)                                         &
                           ! OUT Soil respiration (kg C/m2/s).
,resp_w_ft(land_pts,npft)                                         &
                            ! OUT Wood maintenance respiration
!                                 !     (kg C/m2/s).
,smct(land_pts)                                                   &
                            ! OUT Available moisture in the
!                                 !     soil profile (mm).
,vfrac_tile(land_pts,ntiles)                                      &
                            ! OUT Fractional canopy coverage for
!                                 !     land tiles.
,emis_tile(land_pts,ntiles)                                       &
                            ! OUT Emissivity for land tiles
,emis_soil(land_pts)                                              &
                            ! OUT Emissivity of underlying soil
,wt_ext_tile(land_pts,nshyd,ntiles)                               &
!                                 ! OUT Fraction of evapotranspiration
!                                 !     which is extracted from each
!                                 !     soil layer by each tile.
,fsmc(land_pts,npft)                                              &
                            ! OUT Moisture availability factor.
,flux_o3_ft(land_pts,npft)                                        &
                            ! OUT Flux of O3 to stomata (nmol O3/m2/s).
,fo3_ft(land_pts,npft)      ! OUT Ozone exposure factor.


!  External routines called :-
EXTERNAL root_frac,smc_ext,raero,sf_stom,soil_evap,               &
 leaf_lit,cancap,microbe


REAL                                                              &
 canhc(land_pts)                                                  &
                            ! WORK Canopy heat capacity (J/K/m2).
,ch_type(land_pts,ntype)                                          &
                            ! WORK CANHC for surface types.
,f_root(nshyd)                                                    &
                            ! WORK Fraction of roots in each soil
!                                 !      layer.
,gsoil(land_pts)                                                  &
                            ! WORK Bare soil conductance.
,gs_type(land_pts,ntype)                                          &
                            ! WORK Conductance for surface types.
,pstar_land(land_pts)                                             &
                            ! WORK Surface pressure (Pa).
,rib(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)         &
                            ! WORK Bulk Richardson Number.
,tstar(land_pts)                                                  &
                            ! WORK Surface temperature (K).
,vfrac(land_pts)                                                  &
                            ! WORK Fractional canopy coverage.
,vf_type(land_pts,ntype)                                          &
                            ! WORK VFRAC for surface types.
,wt_ext_type(land_pts,nshyd,ntype)                                &
!                                 ! WORK WT_EXT for surface types.
,fsoil(land_pts,npft)                                             &
                            ! WORK Fraction of ground below canopy
!                                 !      contributing to evaporation.
,fsoil_tot(land_pts)                                              &
                            ! WORK Total fraction of soil
!                                 !      contributing to evaporation
!                                 !      ( = bare soil fraction
!                                 !    + fraction of ground below canopy
,z0(land_pts)                                                     &
                            ! WORK Roughness length (m).
,emisc(land_pts)                                                  &
!                                 ! WORK Emissivity of canyon
,dz_wall                                                          &
!                                 ! WORK Damping depth: wall
,dz_road                                                          &
!                                 ! WORK Damping depth: road
,dz_roof                                                          &
!                                 ! WORK Damping depth: roof
,dz_roof_c                  ! WORK Damping depth: roof
!                                 ! Calculated rather than physical

INTEGER                                                           &
 i,j,k,l,m,n                ! Loop indices

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook('PHYSIOL',zhook_in,zhook_handle)

!-----------------------------------------------------------------------
! Initialisations
!-----------------------------------------------------------------------
f_root(:)             = 0.0
gpp(:)                = 0.0
npp(:)                = 0.0
resp_p(:)             = 0.0
smct(:)               = 0.0
ra(:)                 = 0.0
canhc(:)              = 0.0
vfrac(:)              = 0.0
veg_frac(:)           = 0.0
wt_ext(:,:)           = 0.0
rib(:,:)              = 0.0
g_leaf(:,:)           = 0.0
gpp_ft(:,:)           = 0.0
npp_ft(:,:)           = 0.0
resp_p_ft(:,:)        = 0.0
resp_w_ft(:,:)        = 0.0
fsmc(:,:)             = 0.0
resp_s(:,:)           = 0.0
faparv(:,:)           = 0.0
wt_ext_type(:,:,:)    = 0.0
alb_type_dummy(:,:,:) = 0.0
fapar_dir(:,:,:)      = 0.0
fapar_dif(:,:,:)      = 0.0

isoprene(:)           = 0.0
isoprene_ft(:,:)      = 0.0
terpene(:)            = 0.0
terpene_ft(:,:)       = 0.0
methanol(:)           = 0.0
methanol_ft(:,:)      = 0.0
acetone(:)            = 0.0
acetone_ft(:,:)       = 0.0


DO l=1,land_pts
  j=(land_index(l)-1)/t_i_length + 1
  i = land_index(l) - (j-1)*t_i_length
  pstar_land(l) = pstar(i,j)
END DO

DO n=1,ntype
  DO l=1,land_pts
    gs_type(l,n)=gs(l)
  END DO
END DO
gs(:)=0.0

DO l=1,land_pts
  fsoil_tot(l) = frac(l,soil)
  gsoil(l) = 0.
  IF (v_crit(l,1) > 0.)                                           &
    gsoil(l) = gs_nvg(soil - npft) *                              &
               (sthu(l,1) * v_sat(l,1) / v_crit(l,1))**2
END DO

!-----------------------------------------------------------------------
! Calculate light absorption by the plant canopy
!-----------------------------------------------------------------------
IF ( can_rad_mod /= 1 ) THEN
! The logical argument (getProfile in albpft) is TRUE to indicate
! that profiles through the canopy should be calculated.
  IF (l_albedo_obs .AND. l_spec_albedo) THEN 
! scale the input PFT scattering and reflectivity parameters to match 
! albedo obs, using pre-corrected scalings: 
    albpft_call=2 
  ELSE 
! do not scale to match albedo obs: 
    albpft_call=0 
  END IF 
! DEPENDS ON: albpft
  CALL albpft     (t_i_length * t_j_length,land_pts,land_index,   &
                   tile_index,tile_pts,ilayers,.TRUE.,albpft_call,&
                   albsoil,cos_zenith_angle,lai,alb_type_dummy,   &
                   fapar_dir,fapar_dif,fapar_dir2dif,             &
                   fapar_dif2dif,fapar_dir2dir,fsun)
END IF



!-----------------------------------------------------------------------
! Loop over Plant Functional Types to calculate the available moisture
! and the values of canopy conductance, the carbon fluxes and the leaf
! turnover rate
!-----------------------------------------------------------------------
DO n=1,npft

  IF ( l_aggregate ) THEN
    DO l=1,land_pts
      tstar(l) = tstar_tile(l,1)
      z0(l) = z0_tile(l,1)
    END DO
  ELSE
    DO l=1,land_pts
      tstar(l) = tstar_tile(l,n)
      z0(l) = z0_tile(l,n)
    END DO
  END IF

! DEPENDS ON: root_frac
  CALL root_frac(nshyd,dzsoil,rootd_ft(n),f_root)

! DEPENDS ON: smc_ext
  CALL smc_ext (land_pts,nshyd,tile_pts(n),tile_index(:,n)        &
,               f_root,sthu,v_crit,v_sat,v_wilt                   &
,               wt_ext_type(:,:,n),fsmc(:,n))

! DEPENDS ON: raero
  CALL raero (land_pts,land_index,tile_pts(n),tile_index(:,n)     &
,             rib,wind,z0,z0,z1,ra)
!-----------------------------------------------------------------------
! Calculate light absorption by the plant canopy
!-----------------------------------------------------------------------

  IF (can_rad_mod /= 1) THEN

    DO l=1,land_pts
      fapar_dif_tot(l,n)=0.0
      fapar_dir_tot(l,n)=0.0
    END DO

    DO m=1,ilayers
      DO k=1,tile_pts(n)
        l = tile_index(k,n)
        i = land_index(l)

        fapar_dir_tot(l,n) = fapar_dir_tot(l,n)                   &
                           + fapar_dir(l,n,m)
        fapar_dif_tot(l,n) = fapar_dif_tot(l,n)                   &
                           + fapar_dif(l,n,m)

        faparv(l,m) = (1.-diff_frac(i)) * fapar_dir(l,n,m)        &
                    + diff_frac(i) * fapar_dif(l,n,m)
      END DO !  points
    END DO  !  layer

    IF ( can_rad_mod == 5 ) THEN
      DO m=1,ilayers
        DO k=1,tile_pts(n)
          l = tile_index(k,n)
          i = land_index(l)

          fapar_shd(l,m) = diff_frac(i) * fapar_dif2dif(l,n,m)    &
               + ( 1.0 - diff_frac(i) ) * fapar_dir2dif(l,n,m)

          IF ( fsun(l,n,m) > EPSILON(fsun) ) THEN
            fapar_sun(l,m) = fapar_shd(l,m)                       &
                + ( 1. - diff_frac(i) ) * fapar_dir2dir(l,n,m )   &
                / fsun(l,n,m)
          ELSE
            fapar_sun(l,m) = 0.0
          END IF

        END DO !  points
      END DO  !  layer
    END IF  !  can_rad_mod=5

  END IF  !  can_rad_mod



! DEPENDS ON: sf_stom
  CALL sf_stom (land_pts,land_index                               &
,               tile_pts(n),tile_index(:,n),n                     &
,               co2,co2_3d,co2_dim_len                            &
,               co2_dim_row,l_co2_interactive                     &
,               fsmc(:,n),ht(:,n),ipar,lai(:,n),pstar_land        &
,               q1,ra,tstar,o3                                    &
,               can_rad_mod,ilayers,faparv                        &
,               gpp_ft(:,n),npp_ft(:,n),resp_p_ft(:,n)            &
,               resp_w_ft(:,n),gs_type(:,n)                       &
,               fapar_sun,fapar_shd,fsun(:,n,:)                   &
,               flux_o3_ft(:,n),fo3_ft(:,n)                       &
,               isoprene_ft(:,n),terpene_ft(:,n)                  &
,               methanol_ft(:,n),acetone_ft(:,n))

! DEPENDS ON: soil_evap
  CALL soil_evap (land_pts,nshyd,tile_pts(n),tile_index(:,n)      &
,                 gsoil,lai(:,n),gs_type(:,n),wt_ext_type(:,:,n)  &
,                 fsoil(:,n))

! DEPENDS ON: leaf_lit
  CALL leaf_lit (land_pts,tile_pts(n),tile_index(:,n)             &
,                n,fsmc(:,n),tstar,g_leaf(:,n))

! DEPENDS ON: cancap
  CALL cancap (land_pts,tile_pts(n),tile_index(:,n),can_model,n   &
,              ht(:,n),lai(:,n),ch_type(:,n),vf_type(:,n))

  DO l=1,land_pts
    fsoil_tot(l) = fsoil_tot(l) + frac(l,n)*fsoil(l,n)
  END DO

END DO

!----------------------------------------------------------------------
! Non-vegetated surface types
!----------------------------------------------------------------------
DO n=npft+1,ntype
  DO m=1,tile_pts(n)
    l=tile_index(m,n)
    gs_type(l,n) = gs_nvg(n-npft)
  END DO
END DO

! URBAN-2T over-write gs_type with urban values. Set canyon & roof at the same
! time to avoid affecting land-ice points.
IF ( l_urban2t ) THEN
  n = urban_canyon
  DO m = 1, tile_pts(n)
    l = tile_index(m,n)
    gs_type(l,n) = gs_c
    gs_type(l,urban_roof) = gs_rf
  END DO
END IF

! Copy soil conductance and add bare soil fraction to extraction from
! surface layer
n = soil
DO m=1,tile_pts(n)
  l=tile_index(m,n)
  gs_type(l,n) = gsoil(l)
  wt_ext_type(l,1,n) = 1.
END DO

!----------------------------------------------------------------------
! Canopy heat capacity and coverage for non-vegetated surfaces
!----------------------------------------------------------------------
DO n=npft+1,ntype
  DO m=1,tile_pts(n)
    l=tile_index(m,n)
    ch_type(l,n) = ch_nvg(n-npft)
    vf_type(l,n) = vf_nvg(n-npft)
  END DO
END DO

! MORUSES - Update ch_type (ch_nvg) with calculated values for urban fabric.
! Roof is done at the same time as canyon; cannot have one without the other.
! Avoids over-writing land-ice points when hijacking ice tile. Differs from
! original coding for efficiency reasons; taken out of previous loop. Also set
! vf_type = 0.0 here for urban tiles when l_moruses_storage = TRUE.
IF ( l_moruses_storage ) THEN

  dz_wall   = ( 2.0 * diffus_wall / omega_day )**( 1.0 / 2.0 )
  dz_road   = ( 2.0 * diffus_road / omega_day )**( 1.0 / 2.0 )
  dz_roof_c = ( 2.0 * diffus_roof / omega_day )**( 1.0 / 2.0 )
  ! dz_roof is very thin to represent insulation
  IF ( l_moruses_storage_thin ) THEN
    dz_roof = MIN( dz_roof_c, dz_roof_p )
  ELSE
    dz_roof=dz_roof_c
  END IF

  n = urban_canyon
  DO m = 1, tile_pts(n)
    l = tile_index(m,n)

    ! Canyon
    ch_type(l,n) = cap_road * dz_road +                                 &
       ( 2.0 * hwr(l) * cap_wall * dz_wall )

    ! Where there's a roof there's a canyon
    ch_type(l,urban_roof) = cap_roof * dz_roof

    IF ( l_aggregate ) THEN
      ! Use original coupling to soil not MORUSES coupling
      vf_type(l,n)          = 1.0
      vf_type(l,urban_roof) = 1.0
    ELSE
      vf_type(l,n)          = 0.0
      vf_type(l,urban_roof) = 0.0
    END IF

  END DO
ELSE IF  ( l_urban2t .AND. .NOT. l_moruses_storage ) THEN
! URBAN-2T Set canyon & roof at same time to avoid affecting land-ice points
  n = urban_canyon
  DO m = 1, tile_pts(n)
    l = tile_index(m,n)
    ch_type(l,n)          = ch_c
    ch_type(l,urban_roof) = ch_rf
    vf_type(l,n)          = vf_c
    vf_type(l,urban_roof) = vf_rf

  END DO

END IF

!----------------------------------------------------------------------
! Surface emissivity
!----------------------------------------------------------------------

! Initialise emis_tile
emis_tile(:,:) = 0.0

! URBAN-2T & MORUSES: Set canyon emissivity

IF ( l_moruses_emissivity ) THEN
  n = urban_canyon
#if defined(UM_JULES)
  IF ( printstatus > PrStatus_Normal .AND. firstcall) THEN
    WRITE(6,*) 'MORUSES canyon emissivity calculated'
  END IF
#else
  IF ( firstcall ) PRINT *, 'MORUSES canyon emissivity calculated'
#endif
  DO m = 1, tile_pts(n)
    l = tile_index(m,n)
    ! DEPENDS ON: urbanemis
    CALL urbanemis(hwr(l), emisr(l), emiss, emisw(l),               &
       emisc(l))
  END DO
ELSE IF ( l_urban2t .AND. .NOT. l_moruses_emissivity ) THEN
#if defined(UM_JULES)
  IF ( printstatus > PrStatus_Normal .AND. firstcall ) THEN
    WRITE(6,*) 'URBAN-2T canyon emissivity used'
  END IF
#else
  IF ( firstcall ) PRINT *, 'URBAN-2T canyon emissivity used'
#endif
  n = urban_canyon
  DO m = 1, tile_pts(n)
    l = tile_index(m,n)
    emisc(l) = emis_c
  END DO
END IF

! Calculate EMIS_TILE

IF ( l_aggregate ) THEN

  DO n=1,npft
    DO m=1,tile_pts(n)
      l = tile_index(m,n)
      emis_tile(l,1) = emis_tile(l,1) + frac(l,n) * emis_pft(n)
    END DO
  END DO

! URBAN-2T & MORUSES: Use emisc and emis_rf instead of EMIS_NVG if canyon tile.
! Apply roof at same time as cannot have one without the other.

! Implementation inelegant. If emis_nvg becomes dependent on land_pts then
! could overwrite emis_nvg with SUBROUTINE urbanemis and do away with the
! if statements. Alternatively could create a 2d work array e.g.
! emis_nvg_wk(land_pts,ntype) or re-write to have just
! emis_tile(land_pts,ntype) then copy aggregated value to
! emis_tile(land_pts,1). Similar to albedo in tile_albedo.

  IF ( l_urban2t ) THEN

! URBAN-2T & MORUSES: Use v_sat to test for land ice
! This will have to be re-done for flexible tiles.

    DO n=npft+1,ntype
      IF ( n == urban_canyon ) THEN
        DO m=1,tile_pts(n)
          l = tile_index(m,n)
#if defined(UM_JULES)
          IF ( printstatus > PrStatus_Normal .AND. firstcall ) THEN
            WRITE(6,*) 'EMIS_TILE: Emissivity of canyon used', n,m,l
          END IF
#else
          IF ( firstcall )                                            &
             PRINT *, 'EMIS_TILE: Emissivity of canyon used', n,m,l
#endif
          emis_tile(l,1) = emis_tile(l,1)                             &
             + frac(l,n) * emisc(l)
        END DO
      ELSE IF ( n == urban_roof ) THEN
        DO m=1,tile_pts(n)
          l = tile_index(m,n)
          IF ( v_sat(l,1) > 0.0 ) THEN ! Check for ice-tile
#if defined(UM_JULES)
            IF ( printstatus > PrStatus_Normal .AND. firstcall ) THEN
              WRITE(6,*) 'EMIS_TILE: Emissivity of roof used', n,m,l
            END IF
#else
            IF ( firstcall )                                          &
               PRINT *, 'EMIS_TILE: Emissivity of roof used', n, m, l
#endif
            emis_tile(l,1) = emis_tile(l,1)                           &
               + frac(l,n) * emis_rf
          ELSE ! Land-ice
            emis_tile(l,1) = emis_tile(l,1)                           &
               + frac(l,n) * emis_nvg(n - npft)
          END IF
        END DO
      ELSE
        DO m=1,tile_pts(n)
          l = tile_index(m,n)
          emis_tile(l,1) = emis_tile(l,1)                             &
             + frac(l,n) * emis_nvg(n - npft)
        END DO
      END IF
    END DO

    firstcall = .FALSE.

  ELSE ! .NOT. l_urban2T
    DO n=npft+1,ntype
      DO m=1,tile_pts(n)
        l = tile_index(m,n)
        emis_tile(l,1) = emis_tile(l,1)                             &
           + frac(l,n) * emis_nvg(n - npft)
      END DO
    END DO
  END IF

ELSE ! .NOT. L_AGGREGATE

  DO n=1,npft
    DO m=1,tile_pts(n)
      l = tile_index(m,n)
      emis_tile(l,n) = emis_pft(n)
    END DO
  END DO

  DO n=npft+1,ntype
    DO m=1,tile_pts(n)
      l = tile_index(m,n)
      emis_tile(l,n) = emis_nvg(n - npft)
    END DO
  END DO

! URBAN-2T & MORUSES: Overwrite EMIS_TILE for urban canyon & roof
  IF ( l_urban2t ) THEN
    n = urban_canyon
    DO m = 1, tile_pts(n)
      l = tile_index(m,n)
      emis_tile(l,n) = emisc(l)
      emis_tile(l,urban_roof) = emis_rf
    END DO
    firstcall = .FALSE.

  END IF
END IF ! L_AGGREGATE

DO l=1,land_pts
  emis_soil(l) = emis_nvg(soil - npft)
END DO

!----------------------------------------------------------------------
! Calculate the rate of soil respiration
!----------------------------------------------------------------------
! set VEG_FRAC according to whether it is full or dummy field
IF (l_triffid) THEN
  DO l=1,land_pts
    veg_frac(l) = SUM(frac(l,1:npft))
  END DO
END IF
! DEPENDS ON: microbe
CALL microbe (land_pts,dim_cs1,dim_cs2,l_triffid,l_q10,cs,        &
              sthu(:,1),v_sat(:,1),v_wilt(:,1),tsoil,resp_s,      &
              veg_frac)

!----------------------------------------------------------------------
! Form gridbox mean values
!----------------------------------------------------------------------

DO n=1,ntype
  DO m=1,tile_pts(n)
    l=tile_index(m,n)
    gs(l) = gs(l) + frac(l,n)*gs_type(l,n)
  END DO
END DO

IF ( l_aggregate ) THEN
  DO n=1,ntype
    DO m=1,tile_pts(n)
      l=tile_index(m,n)
      canhc(l) = canhc(l) + frac(l,n)*ch_type(l,n)
      vfrac(l) = vfrac(l) + frac(l,n)*vf_type(l,n)
      DO k=1,nshyd
        wt_ext(l,k) = wt_ext(l,k) + frac(l,n)*wt_ext_type(l,k,n)
      END DO
    END DO
  END DO
  DO l=1,land_pts
    IF ( lake > 0 ) THEN
      flake(l,1) = frac(l,lake)
    ELSE
      flake(l,1) = 0.
    END IF
    gs_tile(l,1) = 0.
    IF (flake(l,1) <  1.)                                         &
      gs_tile(l,1) = gs(l) / (1. - flake(l,1))
    canhc_tile(l,1) = canhc(l)
    vfrac_tile(l,1) = vfrac(l)
    DO k=1,nshyd
      wt_ext_tile(l,k,1) = wt_ext(l,k)
    END DO
  END DO
ELSE
  gs_tile(:,:)=0.0
  canhc_tile(:,:)=0.0
  vfrac_tile(:,:)=0.0
  DO n=1,ntype
    DO m=1,tile_pts(n)
      l=tile_index(m,n)
      flake(l,n) = 0.
      gs_tile(l,n) = gs_type(l,n)
      canhc_tile(l,n) = ch_type(l,n)
      vfrac_tile(l,n) = vf_type(l,n)
      DO k=1,nshyd
        wt_ext(l,k) = wt_ext(l,k) + frac(l,n)*wt_ext_type(l,k,n)
        wt_ext_tile(l,k,n) = wt_ext_type(l,k,n)
      END DO
    END DO
  END DO
  IF ( lake > 0 ) THEN
    n = lake    ! Lake tile
    DO m=1,tile_pts(n)
      l=tile_index(m,n)
      flake(l,n) = 1.
    END DO
  END IF
END IF

DO n=1,npft
  DO m=1,tile_pts(n)
    l=tile_index(m,n)

    gpp(l)      = gpp(l) + frac(l,n) * gpp_ft(l,n)
    npp(l)      = npp(l) + frac(l,n) * npp_ft(l,n)
    resp_p(l)   = resp_p(l) + frac(l,n) * resp_p_ft(l,n)

    isoprene(l) = isoprene(l) + frac(l,n) * fland(l) * isoprene_ft(l,n)
    terpene(l)  = terpene(l)  + frac(l,n) * fland(l) * terpene_ft(l,n)
    methanol(l) = methanol(l) + frac(l,n) * fland(l) * methanol_ft(l,n)
    acetone(l)  = acetone(l)  + frac(l,n) * fland(l) * acetone_ft(l,n)

  END DO
END DO

!----------------------------------------------------------------------
! Diagnose the available moisture in the soil profile
!----------------------------------------------------------------------
! Available water for plant transpiration
DO n=1,nshyd
  DO l=1,land_pts
    smct(l) = smct(l) + MAX( 0. ,                                 &
         wt_ext(l,n)*rho_water*dzsoil(n)*                         &
               (sthu(l,n)*v_sat(l,n)-v_wilt(l,n)))
  END DO
END DO

! Add available water for evaporation from bare soil
DO l=1,land_pts
   smct(l) = (1.0-fsoil_tot(l))*smct(l) +                         &
                   fsoil_tot(l)*rho_water*dzsoil(1)*              &
                       MAX(0.0,sthu(l,1))*v_sat(l,1)
END DO

IF (lhook) CALL dr_hook('PHYSIOL',zhook_out,zhook_handle)
RETURN
END SUBROUTINE physiol
