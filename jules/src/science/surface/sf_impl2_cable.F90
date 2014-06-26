! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  SUBROUTINE SF_IMPL2-----------------------------------------------
!
!  Purpose: Calculate implicit correction to surface fluxes of heat,
!           moisture and momentum to be used by the unconditionally
!           stable and non-oscillatory BL numerical solver.  Also
!           calculates screen level temperature and humidity as well
!           as 10 m winds.
!
!--------------------------------------------------------------------
!    Arguments :-
SUBROUTINE sf_impl2_cable (                                             &
! IN values defining field dimensions and subset to be processed :
 land_pts,land_index,nice,nice_use,ntiles,tile_index,tile_pts,    &
 sm_levels,canhc_tile,canopy,flake,smc,tile_frac,wt_ext_tile,     &
 fland,flandg,lq_mix_bl,l_flux_bc,                                &
! IN sea/sea-ice data :
 ice_fract,ice_fract_ncat,k_sice,u_0,v_0,                         &
! IN everything not covered so far :
 pstar,lw_down,sw_tile,                                           &
 t_soil,qw_1,tl_1,u_1,v_1,rhokm_u_1,rhokm_v_1,GAMMA,              &
 gamma1,gamma2,alpha1,alpha1_sice,ashtf_prime,ashtf_prime_tile,   &
 dtrdz_charney_grid_1,du_1,dv_1,                                  &
 fraca,resfs,resft,rhokh,rhokh_tile,rhokh_sice,z1,                &
 z0hssi,z0mssi,z0h_tile,z0m_tile,cdr10m_u,cdr10m_v,               &
 chr1p5m,chr1p5m_sice,ctctq1,                                     &
 dqw1_1,dtl1_1,du_star1,dv_star1,cq_cm_u_1,cq_cm_v_1,             &
 l_correct,flandg_u,flandg_v,                         &
 emis_tile,ti,tstar_sea,snow_tile,                                &
! IN variables used to calculate cooling at the screen level
 l_co2_interactive, co2_mmr, co2_3d,rho1, f3_at_p, ustargbm,      &
! IN STASH flags :-
 simlt,smlt,slh,sq1p5,st1p5,su10,sv10,                            &
! INOUT data :
 epot_tile,fqw_ice,ftl_ice,dtstar_tile,dtstar,                    &
 tstar_sice_cat,tstar_ssi,tstar_tile,radnet_sice,fqw_tile,        &
 fqw_1,ftl_1,ftl_tile,olr,taux_land,taux_ssi,tauy_land,tauy_ssi,  &
 TScrnDcl_SSI,TScrnDcl_TILE,tStbTrans,                            &
! OUT Diagnostic not requiring STASH flags :
 ecan,ei_tile,esoil_tile,sea_ice_htf,surf_ht_flux,                &
 surf_ht_flux_land,surf_ht_flux_sice,surf_htf_tile,               &
! OUT diagnostic requiring STASH flags :
 sice_mlt_htf,snomlt_surf_htf,latent_heat,                        &
 q1p5m,q1p5m_tile,t1p5m,t1p5m_tile,u10m,v10m,                     &
! OUT data required elsewhere in UM system :
 tstar,tstar_land,tstar_sice,le_tile,radnet_tile,e_sea,h_sea,     &
 taux_1,tauy_1,taux_land_star,tauy_land_star,taux_ssi_star,       &
 tauy_ssi_star,ecan_tile,ei,ei_sice,esoil,ext,snowmelt,melt_tile, &
 rhokh_mix,error                                                  &
 )

USE atm_fields_bounds_mod
USE theta_field_sizes, ONLY : t_i_length, t_j_length

USE csigma
USE c_lheat
USE c_r_cp
USE c_0_dg_c
USE surf_param, ONLY : ls, ip_scrndecpl2, emis_sea, emis_sice
USE switches  , ONLY : l_aggregate                                &
                      ,l_flake_model, l_tstar_sice_new            &
                      ,l_dolr_land_black
USE nstypes   , ONLY : lake
USE lake_mod  , ONLY : surf_ht_flux_lake                          &
                      ,lake_h_ice
#if defined(UM_JULES)
USE timestep_mod, ONLY: timestep
USE science_fixes_mod, ONLY: l_emis_ssi_full
#else
USE model_time_mod, ONLY: timestep_len
#endif
USE fluxes, ONLY : anthrop_heat, surf_ht_store, sw_sice

USE ancil_info, ONLY: nsmax,ssi_pts,sea_pts,sice_pts,             &
    sice_pts_ncat,ssi_index,sea_index,sice_index,sice_index_ncat, &
    fssi,sea_frac,sice_frac,sice_frac_ncat
USE switches,    ONLY: iscrntdiag                                 &
                      ,can_model                                  &
                      ,l_snowdep_surf,                            &
                      l_neg_tstar
USE prognostics, ONLY: nsnow                                      &
                      ,snowdepth
USE jules_mod,   ONLY: snowdep_surf
USE snow_param,  ONLY: rho_snow_const                             &
                      ,cansnowtile
USE c_perma,     ONLY: rho_ice
USE solinc_data, ONLY: sky, l_skyview

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
use cable_data_mod, only : cable, cable_control7
!jhan : make switchable? already switched subr
IMPLICIT NONE
!--------------------------------------------------------------------
!  Inputs :-
! (a) Defining horizontal grid and subset thereof to be processed.
!    Checked for consistency.
!--------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                            &
 land_pts    ! IN No of land points

! (c) Soil/vegetation/land surface parameters (mostly constant).
INTEGER, INTENT(IN) ::                                            &
 land_index(land_pts)        ! IN LAND_INDEX(I)=J => the Jth
!                                  !    point in ROW_LENGTH,ROWS is the
!                                  !    Ith land point.

INTEGER, INTENT(IN) ::                                            &
 sm_levels                                                        &
                             ! IN No. of soil moisture levels
,ntiles                                                           &
                             ! IN No. of land tiles
,tile_index(land_pts,ntiles)                                      &
                             ! IN Index of tile points
,tile_pts(ntiles)                                                 &
                             ! IN Number of tile points
,nice                                                             &
                             ! IN Number of sea ice categories
,nice_use                    ! IN Number of sea ice categories used 
                             !    fully in surface exchange

REAL, INTENT(IN) ::                                               &
 canhc_tile(land_pts,ntiles)                                      &
                             ! IN Areal heat capacity of canopy
!                                  !    for land tiles (J/K/m2).
,canopy(land_pts,ntiles)                                          &
                             ! IN Surface/canopy water for
!                                  !    snow-free land tiles (kg/m2)
,flake(land_pts,ntiles)                                           &
                             ! IN Lake fraction.
,smc(land_pts)                                                    &
                             ! IN Available soil moisture (kg/m2).
,tile_frac(land_pts,ntiles)                                       &
                             ! IN Tile fractions including
!                                  ! snow cover in the ice tile.
,wt_ext_tile(land_pts,sm_levels,ntiles)                           &
!                                  ! IN Fraction of evapotranspiration
!                                  !    extracted from each soil layer
!                                  !    by each tile.
,fland(land_pts)                                                  &
                             ! IN Land fraction on land pts.
,flandg(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)      &
!                                  ! IN Land fraction on all pts.
,flandg_u(udims%i_start:udims%i_end,udims%j_start:udims%j_end)    &
                             ! IN Land fraction on U grid.
,flandg_v(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)    &
                             ! IN Land fraction on V grid.
,emis_tile(land_pts,ntiles)                                       &
                             ! IN Emissivity for land tiles
,ti(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice)     &
                             ! IN   Sea-ice surface layer
                             !       temperature (K).
,tstar_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)   &
                             ! IN    Open sea sfc temperature (K).
,snow_tile(land_pts,ntiles)
                             ! IN Lying snow on tiles (kg/m2)

! (d) Sea/sea-ice data.
REAL, INTENT(IN) ::                                               &
 ice_fract(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)   &
                             ! IN Fraction of gridbox covered by
!                            !     sea-ice (decimal fraction).
,ice_fract_ncat(tdims%i_start:tdims%i_end,                        &
                tdims%j_start:tdims%j_end,nice)                   &
                             ! IN Fraction of gridbox
!                            !  covered by sea-ice on categories.
,k_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice) &
                             ! IN sea ice effective conductivity in
!                             !     sfc layer on categories (W/m2/k)      
,u_0(udims%i_start:udims%i_end,udims%j_start:udims%j_end)         &
                             ! IN W'ly component of surface
!                                  !    current (m/s).
,v_0(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
                             ! IN S'ly component of surface
!                                  !    current (m/s).

! (f) Atmospheric + any other data not covered so far, incl control.

REAL, INTENT(IN) ::                                               &
 pstar(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)       &
                             ! IN Surface pressure (Pascals).
,lw_down(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)     &
                             ! IN Surface downward LW radiation
!                                  !    (W/m2).
,sw_tile(land_pts,ntiles)                                         &
                             ! IN Surface net SW radiation on
!                                  !    land tiles (W/m2).
,t_soil(land_pts,sm_levels)                                       &
                             ! IN Soil temperatures (K).
,qw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)        &
                             ! IN Total water content
,tl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)        &
                             ! IN Ice/liquid water temperature
,u_1(udims_s%i_start:udims_s%i_end,udims_s%j_start:udims_s%j_end) &
                             ! IN W'ly wind component (m/s)
,v_1(vdims_s%i_start:vdims_s%i_end,vdims_s%j_start:vdims_s%j_end) &
                             ! IN S'ly wind component (m/s)
,rhokm_u_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)   &
                             ! IN Exchange coefficients for
!                                  !    momentum (on U-grid, with 1st
!                                  !    and last rows undefined or, at
!                                  !    present, set to "missing data")
,rhokm_v_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
                             ! IN Exchange coefficients for
!                                  !    momentum (on V-grid, with 1st
!                                  !    and last rows undefined or, at
!                                  !    present, set to "missing data")

REAL, INTENT(IN) :: GAMMA          ! IN implicit weight in level 1

REAL, INTENT(IN) ::                                               &
 gamma1(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)      &
                             ! weights for new BL solver
,gamma2(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)

REAL, INTENT(IN) ::                                               &
 alpha1(land_pts,ntiles)                                          &
                             ! IN Mean gradient of saturated
!                                  !    specific humidity with respect
!                                  !    to temperature between the
!                                  !    bottom model layer and tile
!                                  !    surfaces
,alpha1_sice(tdims%i_start:tdims%i_end,                           &
             tdims%j_start:tdims%j_end,nice_use)                  &
                             ! IN ALPHA1 for sea-ice.
,ashtf_prime(tdims%i_start:tdims%i_end,                           &
             tdims%j_start:tdims%j_end,nice_use)                  &
                             ! IN Adjusted SEB coefficient for
                             !    sea-ice
,ashtf_prime_tile(land_pts,ntiles)                                &
                             ! IN Adjusted SEB coefficient for
                             !    land tiles.
,dtrdz_charney_grid_1(pdims%i_start:pdims%i_end,                  &
                      pdims%j_start:pdims%j_end)                  &
!                                  ! IN -g.dt/dp for model layers.
,fraca(land_pts,ntiles)                                           &
                             ! IN Fraction of surface moisture
!                                  !    flux with only aerodynamic
!                                  !    resistance for snow-free land
!                                  !    tiles.
,resfs(land_pts,ntiles)                                           &
                             ! IN Combined soil, stomatal
!                                  !    and aerodynamic resistance
!                                  !    factor for fraction (1-FRACA) of
!                                  !    snow-free land tiles.
,resft(land_pts,ntiles)                                           &
                             ! IN Total resistance factor.
!                                  !    FRACA+(1-FRACA)*RESFS for
!                                  !    snow-free land, 1 for snow.
,rhokh(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! IN Grid-box surface exchange
!                                  !     coefficients
!                                  !    (not used for JULES)
,rhokh_tile(land_pts,ntiles)                                      &
                             ! IN Surface exchange coefficients
!                                  !    for land tiles
,rhokh_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Surface exchange coefficients
!                                  !    for sea and sea-ice
 REAL, INTENT(IN) ::                                              &
 z1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)          &
                             ! IN Height of lowest level (i.e.
!                                  !    height of middle of lowest
!                                  !    layer).
,z0hssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)      &
,z0mssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)      &
                             ! IN Roughness lengths over sea (m)
,z0h_tile(land_pts,ntiles)                                        &
                             ! IN Tile roughness lengths for heat
!                                  !    and moisture (m).
,z0m_tile(land_pts,ntiles)                                        &
                             ! IN Tile roughness lengths for
!                                  !    momentum.
,cdr10m_u(udims%i_start:udims%i_end,udims%j_start:udims%j_end)    &
                             ! IN Ratio of CD's reqd for
!                                  !    calculation of 10 m wind. On
!                                  !    U-grid; comments as per RHOKM.
,cdr10m_v(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)    &
                             ! IN Ratio of CD's reqd for
!                                  !    calculation of 10 m wind. On
!                                  !    V-grid; comments as per RHOKM.
,chr1p5m(land_pts,ntiles)                                         &
                             ! IN Ratio of coefffs for calculation
!                                  !    of 1.5m temp for land tiles.
,chr1p5m_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)&
!                                  ! IN CHR1P5M for sea and sea-ice
!                                  !    (leads ignored).
,cq_cm_u_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)   &
                             ! IN Coefficient in U tri-diagonal
!                                  !    implicit matrix
,cq_cm_v_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)   &
                             ! IN Coefficient in V tri-diagonal
!                                  !    implicit matrix
,du_1(udims_s%i_start:udims_s%i_end,udims_s%j_start:udims_s%j_end)&
                             ! IN Level 1 increment to u wind
!                                  !    field
,dv_1(vdims_s%i_start:vdims_s%i_end,vdims_s%j_start:vdims_s%j_end)&
                             ! IN Level 1 increment to v wind
!                                  !    field
,ctctq1(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)      &
,dqw1_1(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)      &
,dtl1_1(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)      &
,du_star1(udims_s%i_start:udims_s%i_end,                          &
          udims_s%j_start:udims_s%j_end)                          &
,dv_star1(vdims_s%i_start:vdims_s%i_end,                          &
          vdims_s%j_start:vdims_s%j_end)
!                                  ! IN Additional arrays needed by the
!                                  !    uncond stable BL numerical solver
! IN Additional variables for screen-level diagnostics
LOGICAL, INTENT(IN) :: l_co2_interactive
                                ! Flag for interactive 3-D CO2
REAL, INTENT(IN)    :: co2_mmr
                                ! Initial or fixed mass mixing ratio
                                ! of CO2
REAL, INTENT(IN)    :: co2_3d                                         &
                      (tdims_s%i_start:tdims_s%i_end,tdims_s%j_start:tdims_s%j_end)
                                ! 3-D field of CO2
REAL, INTENT(IN)    :: rho1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                                ! Density on lowest level
REAL, INTENT(IN)    :: f3_at_p(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                                ! Coriolis parameter
REAL, INTENT(IN)    :: uStarGBM(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                                ! GBM surface friction velocity

LOGICAL, INTENT(IN) ::                                            &
 l_flux_bc                                                        &
                             ! IN SCM logical for prescribed
                             !    surface flux forcing
,l_correct
                             ! flag used by the new BL solver

LOGICAL, INTENT(IN) ::                                            &
 lq_mix_bl              ! TRUE if mixing ratios used in
!                             ! boundary layer code
!  STASH flags :-

LOGICAL, INTENT(IN) ::                                            &
 simlt                                                            &
         ! IN Flag for SICE_MLT_HTF (q.v.)
,smlt                                                             &
         ! IN Flag for SNOMLT_SURF_HTF (q.v.)
,slh                                                              &
         ! IN Flag for LATENT_HEAT (q.v.)
,sq1p5                                                            &
         ! IN Flag for Q1P5M (q.v.)
,st1p5                                                            &
         ! IN Flag for T1P5M (q.v.)
,su10                                                             &
         ! IN Flag for U10M (q.v.)
,sv10    ! IN Flag for V10M (q.v.)
!--------------------------------------------------------------------
!  In/outs :-
!--------------------------------------------------------------------
REAL, INTENT(INOUT) ::                                            &
 epot_tile(land_pts,ntiles)                                       &
                             ! INOUT surface tile potential
!                                  !    evaporation
,fqw_ice(tdims%i_start:tdims%i_end,                               &
         tdims%j_start:tdims%j_end,nice_use)                      &
                             ! INOUT Surface FQW for sea-ice
,ftl_ice(tdims%i_start:tdims%i_end,                               &
         tdims%j_start:tdims%j_end,nice_use)                      &
                             ! INOUT Surface FTL for sea-ice
,dtstar_tile(land_pts,ntiles)                                     &
                             ! INOUT Change in TSTAR over timestep
!                                  !     for land tiles
,dtstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice_use)   &
                             ! INOUT Change is TSTAR over timestep
!                                  !     for sea-ice
,tstar_sice_cat(tdims%i_start:tdims%i_end,                        &
                tdims%j_start:tdims%j_end,nice_use)               &
                             ! INOUT   Sea-ice sfc temperature (K).
,tstar_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)   &
                             ! INOUT Sea mean sfc temperature (K).
,tstar_tile(land_pts,ntiles)                                      &
                             ! INOUT Surface tile temperatures
,radnet_sice(tdims%i_start:tdims%i_end,                           &
             tdims%j_start:tdims%j_end,nice_use)                  &
                             ! INOUT Surface net radiation on
!                                  !       sea-ice (W/m2)
,fqw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! INOUT Moisture flux between layers
!                                  !       (kg per square metre per sec)
!                                  !       FQW(,1) is total water flux
!                                  !       from surface, 'E'.
,ftl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! INOUT FTL(,K) contains net
!                                  !       turbulent sensible heat flux
!                                  !       into layer K from below; so
!                                  !       FTL(,1) is the surface
!                                  !       sensible heat, H.(W/m2)
,ftl_tile(land_pts,ntiles)                                        &
                             ! INOUT Surface FTL for land tiles
,fqw_tile(land_pts,ntiles)                                        &
                             ! INOUT Surface FQW for land tiles
,olr(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)         &
                             ! IN    TOA - surface upward LW on
!                                  !       last radiation timestep
!                                  ! OUT   Corrected TOA outward LW
,taux_land(udims%i_start:udims%i_end,udims%j_start:udims%j_end)   &
                             ! INOUT W'ly component of surface
!                                  !       wind stress over land
!                                  !       (N/sq m). (On
!                                  !       UV-grid with first and last
!                                  !       rows undefined or, at
!                                  !       present, set to missing data
,taux_ssi(udims%i_start:udims%i_end,udims%j_start:udims%j_end)    &
                             ! INOUT W'ly component of surface
!                                  !       wind stress over mean sea
!                                  !       (N/sq m). (On
!                                  !       UV-grid with first and last
!                                  !       rows undefined or, at
!                                  !       present, set to missing data
,tauy_land(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)   &
                             ! INOUT S'ly component of land sfc
!                                  !       wind stress (N/sq m).  On
!                                  !       UV-grid; comments as per TAUX
,tauy_ssi(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
!                                  !       wind stress (N/sq m).  On
!                                  !       UV-grid; comments as per TAUX

REAL, INTENT(INOUT) :: TScrnDcl_SSI(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                            !    Decoupled screen-level temperature
                            !    over sea or sea-ice
REAL, INTENT(INOUT) :: TScrnDcl_TILE(land_pts,ntiles)
                            !    Decoupled screen-level temperature
                            !    over land tiles
REAL, INTENT(INOUT) :: tStbTrans(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                            !    Time since the transition to stable
                            !    conditions
!--------------------------------------------------------------------
!  Outputs :-
!-1 Diagnostic (or effectively so - includes coupled model requisites):-

!  (a) Calculated anyway (use STASH space from higher level) :-
!--------------------------------------------------------------------
REAL, INTENT(OUT) ::                                              &
 ecan(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)        &
                             ! OUT Gridbox mean evaporation from
!                                  !     canopy/surface store (kg/m2/s).
!                                  !     Zero over sea.
,esoil_tile(land_pts,ntiles)                                      &
                             ! OUT ESOIL for snow-free land tiles
,sea_ice_htf(tdims%i_start:tdims%i_end,                           &
             tdims%j_start:tdims%j_end,nice)                      &
                             ! OUT Heat flux through sea-ice
!                                  !     (W/m2, positive downwards).
!                                  !     (Not used for JULES)
,surf_ht_flux(tdims%i_start:tdims%i_end,                          &
              tdims%j_start:tdims%j_end)                          &
!                                  ! OUT Net downward heat flux at
!                                  !     surface over land and sea-ice
!                                  !     fraction of gridbox (W/m2).
,surf_ht_flux_land(tdims%i_start:tdims%i_end,                     &
                   tdims%j_start:tdims%j_end)                     &
!                                  ! OUT Net downward heat flux at
!                                  !     surface over land
!                                  !     fraction of gridbox (W/m2).
,surf_ht_flux_sice(tdims%i_start:tdims%i_end,                     &
                   tdims%j_start:tdims%j_end,nice)                &
!                                  ! OUT Net category downward heat flux at
!                                  !     surface over sea-ice
!                                  !     fraction of gridbox (W/m2).
,surf_htf_tile(land_pts,ntiles)
!                                  ! OUT Net downward surface heat flux
!                                  !     on tiles (W/m2)

!  (b) Not passed between lower-level routines (not in workspace at this
!      level) :-

REAL, INTENT(OUT) ::                                              &
 sice_mlt_htf(tdims%i_start:tdims%i_end,                          &
              tdims%j_start:tdims%j_end,nice)                     &
!                                  ! OUT Heat flux due to melting of
!                                  !     sea-ice (Watts per sq metre).
,snomlt_surf_htf(tdims%i_start:tdims%i_end,                       &
                 tdims%j_start:tdims%j_end)                       &
!                                  ! OUT Heat flux required for surface
!                                  !     melting of snow (W/m2).
,latent_heat(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end) &
                             ! OUT Surface latent heat flux, +ve
!                                  !     upwards (Watts per sq m).
,q1p5m(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT Q at 1.5 m (kg water / kg air).
,q1p5m_tile(land_pts,ntiles)                                      &
                             ! OUT Q1P5M over land tiles.
,t1p5m(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT T at 1.5 m (K).
,u10m(udims%i_start:udims%i_end,udims%j_start:udims%j_end)        &
                             ! OUT U at 10 m (m per s).
,t1p5m_tile(land_pts,ntiles)                                      &
                             ! OUT T1P5M over land tiles.
,v10m(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
                             ! OUT V at 10 m (m per s).

!-2 Genuinely output, needed by other atmospheric routines :-

REAL, INTENT(OUT) ::                                              &
 tstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT   GBM surface temperature (K).
,tstar_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)  &
                             ! OUT   Land mean sfc temperature (K)
,tstar_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)  &
                             ! OUT Ice area mean sea ice surface temperature
,le_tile(land_pts,ntiles)                                         &
                             ! OUT Surface latent heat flux for
!                                  !     land tiles
,radnet_tile(land_pts,ntiles)                                     &
                             ! OUT Surface net radiation on
!                                  !       land tiles (W/m2)
,e_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT Evaporation from sea times
!                                  !       leads fraction. Zero over
!                                  !       land. (kg per square metre
!                                  !       per sec).
,h_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT Surface sensible heat flux
!                                  !       over sea times leads fraction
!                                  !       (W/m2)
,taux_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)      &
                             ! OUT   W'ly component of surface
!                                  !       wind stress (N/sq m). (On
!                                  !       UV-grid with first and last
!                                  !       rows undefined or, at
!                                  !       present, set to missing data
,tauy_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)      &
                             ! OUT   S'ly component of surface
!                                  !       wind stress (N/sq m).  On
!                                  !       UV-grid; comments as per TAUX
,taux_land_star(udims%i_start:udims%i_end,                        &
                udims%j_start:udims%j_end)                        &
,tauy_land_star(vdims%i_start:vdims%i_end,                        &
                vdims%j_start:vdims%j_end)                        &
,taux_ssi_star(udims%i_start:udims%i_end,                         &
               udims%j_start:udims%j_end)                         &
,tauy_ssi_star(vdims%i_start:vdims%i_end,                         &
               vdims%j_start:vdims%j_end)                         &
,ei(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)          &
                             ! OUT Sublimation from lying snow or
!                                  !     sea-ice (kg/m2/s).
,ei_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice_use)&
                             ! OUT Sublimation from sea-ice
!                                  !     (kg/m2/s).
,ei_tile(land_pts,ntiles)                                         &
                             ! OUT EI for land tiles.
,ecan_tile(land_pts,ntiles)                                       &
                             ! OUT ECAN for snow-free land tiles
,esoil(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT Surface evapotranspiration
!                                  !     from soil moisture store
!                                  !     (kg/m2/s).
,ext(land_pts,sm_levels)                                          &
                             ! OUT Extraction of water from each
!                                  !     soil layer (kg/m2/s).
,snowmelt(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)    &
                             ! OUT Snowmelt (kg/m2/s).
,melt_tile(land_pts,ntiles)                                       &
                             ! OUT Snowmelt on land tiles (kg/m2/s
,rhokh_mix(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT Exchange coeffs for moisture.
                             !     (Not used for JULES)

INTEGER, INTENT(OUT) ::                                           &
 error          ! OUT 0 - AOK;
!                     !     1 to 7  - bad grid definition detected;
!--------------------------------------------------------------------
!  Workspace :-
!--------------------------------------------------------------------
REAL                                                              &
 elake_tile(land_pts,ntiles)                                      &
                             ! Lake evaporation.
,melt_ice_tile(land_pts,ntiles)                                   &
                             ! Ice melt on FLake lake tile (kg/m2/s)
,lake_ice_mass(land_pts)                                          &
                             ! areal density equivalent to
                             ! lake ice of a given depth (kg/m2)
,qim_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! Implicit value of first model level
!                                  ! humidity
,tim_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! Implicit value of first model level
!                                  ! temperature
,tstar_rad4(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)  &
                             ! Effective surface radiative
!                                  ! temperature for land and sea-ice
,tstar_tile_old(land_pts,ntiles)                                  &
!                                  ! Tile surface temperatures at
!                                  ! beginning of timestep.
,tstar_sice_cat_old(tdims%i_start:tdims%i_end,                    &
           tdims%j_start:tdims%j_end,nice_use)                    &
                             ! Sea ice surface T at beginning of timestep
,tsurf(land_pts)                                                  &
                             ! Soil or snow surface layer temp
,sice_melt(tdims%i_start:tdims%i_end,                             &
           tdims%j_start:tdims%j_end,nice)                        &
                             !Melt at surface sea-ice category
,non_lake_frac(tdims%i_start:tdims%i_end,                         &
               tdims%j_start:tdims%j_end)                         &
                             ! total tile fraction for surface types
                             ! other than inland water

,dftl_sice_ncat(tdims%i_start:tdims%i_end,                        &
                tdims%j_start:tdims%j_end)                        &
                             ! Increment for ftl_ice from sea-ice 
                             ! melt calculated for each category,
                             ! but un-weighted by category fractions
,dfqw_sice_ncat(tdims%i_start:tdims%i_end,                        &
                tdims%j_start:tdims%j_end)                        &
                             ! Increment for fqw_ice from sea-ice 
                             ! melt calculated for each category,
                             ! but un-weighted by category fractions
,dei_sice_ncat(tdims%i_start:tdims%i_end,                         &
               tdims%j_start:tdims%j_end)                         &
                             ! Increment for ei_sice from sea-ice 
                             ! melt calculated for each category,
                             ! but un-weighted by category fractions
,ice_fract_cat_use(tdims%i_start:tdims%i_end,                      &
                   tdims%j_start:tdims%j_end,nice_use)             &  
                             ! Sea ice category fractions
                             ! If nice_use=1, this is the total ice 
                             ! fraction
,ei_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT Sublimation from lying snow
!                                  !     (kg/m2/s).

REAL, ALLOCATABLE :: tstar_ssi_old(:,:)
                                   ! Sea and sea-ice surface temperature
                                   ! at beginning of timestep --
                                   ! Required only for decoupled diagnosis,
                                   ! so allocatable, and local since it is
                                   ! used only on the predictor step

REAL, ALLOCATABLE :: tstar_sic(:,:,:)
                             !Ice category surface temperature
                             ! Only used if nice_use EQ 1

! dummy arrays required for sea and se-ice to create universal
! routines for all surfaces
REAL                                                              &
 array_one(t_i_length*t_j_length)                                 &
                             ! Array of ones
,array_one_e_six(t_i_length*t_j_length)
                             ! Array of 1.0E6

REAL ::                                                           &
 surf_ht_flux_sice_sm(tdims%i_start:tdims%i_end,                  &
                      tdims%j_start:tdims%j_end)                  &
                             ! Sea area mean seaice surface heat flux
,ei_sice_sm(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! Sea area mean sea ice sublimation


!  Local scalars :-

REAL                                                              &
 lat_ht     ! Latent heat of evaporation for snow-free land
!                 ! or sublimation for snow-covered land and ice.

INTEGER                                                           &
 i,j                                                              &
            ! LOCAL Loop counter (horizontal field index).
,k                                                                &
            ! LOCAL Tile pointer
,l                                                                &
            ! LOCAL Land pointer
,n          ! LOCAL Loop counter (tile index).

LOGICAL                                                           &
 l_sice_new_code   ! Controls the sea ice temperature calculation
                   ! (See comments in sea ice section below)

! temporary timestep variable needed because JULES and UM have different
! names and types for the variable
#if !defined(UM_JULES)
REAL :: timestep
#endif

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!CABLE: == sxy
! local variables in the subroutine sf_evap
      REAL                                                              &
     & DFQW(LAND_PTS)                                                   &
                             ! Increment in GBM moisture flux.
     &,DFTL(LAND_PTS)                                                   &
                             ! Increment in GBM sensible heat flux.
     &,E_TILE_OLD(LAND_PTS,NTILES)                                      &
!                            ! Surface moisture flux before adjustment.
     &,LE_TILE_OLD(LAND_PTS,NTILES)
!                            ! Surf latent heat flux before adjustment.
      REAL                                                              &
     & DIFF_LAT_HTF                                                     &
                             ! Increment in local latent heat flux.
     &,DIFF_SENS_HTF                                                    &
                             ! Increment in local sensible heat flux.
     &,fDTSTAR                                                           &
                             ! Increment in local surface temperature.
     &,EDT                   ! Moisture flux x timestep

      INTEGER                                                           &
     & M                    ! Loop counter (soil level index).          
! local variables in the subroutine sf_melt
      REAL                                                              &
     & DFQW_M                                                             &
                            ! Moisture flux increment.
     &,DFTL_M                                                             &
                            ! Sensible heat flux increment.
!     &,DTSTAR                                                           &
                            ! Surface temperature increment.
     &,LCMELT                                                           &
                            ! Temporary in melt calculations.
     &,LSMELT                                                           &
                            ! Temporary in melt calculations.
     &,RHOKH1_PRIME                                                     &
                            ! Modified forward time-weighted
!                           ! transfer coefficient.
     &,SNOW_MAX                                                         &
                            ! Snow available for melting.
     &,TSTARMAX             ! Maximum gridbox mean surface temperature
!                                                       

IF (lhook) CALL dr_hook('SF_IMPL2',zhook_in,zhook_handle)

!CABLE: satisfy intent(inout) nature
radnet_tile=0.
! temporary timestep variable needed because JULES and UM have different
! names and types for the variable
#if !defined(UM_JULES)
timestep = REAL(timestep_len)
#endif

error = 0

array_one(:)=1.0
array_one_e_six(:)=1.0e6

! Set up sea ice field depending on nice_use
IF (nice_use > 1) THEN
  ! Use all categories fully in surface exchange
  ice_fract_cat_use(:,:,:) = ice_fract_ncat(:,:,:)
ELSE  ! nice_use=1
  ice_fract_cat_use(:,:,1) = ice_fract(:,:)
END IF


! DEPENDS ON: im_sf_pt2
CALL im_sf_pt2 (                                                  &
 land_pts,land_index,ntiles,tile_index,tile_pts                   &
,flandg,tile_frac,snow_tile,nice_use,ice_fract,ice_fract_cat_use  &
,GAMMA,gamma1,gamma2,alpha1,alpha1_sice                           &
,ashtf_prime,ashtf_prime_tile                                     &
,resft,dtstar_tile,dtstar                                         &
,rhokm_u_1,rhokm_v_1,rhokh_tile,rhokh_sice                        &
,ctctq1,dqw1_1,dtl1_1                                             &
,cq_cm_u_1,cq_cm_v_1,du_1,dv_1,du_star1,dv_star1                  &
,flandg_u,flandg_v                                                &
,fqw_1,ftl_1                                                      &
,taux_1,taux_land,taux_land_star,taux_ssi,taux_ssi_star,tauy_1    &
,tauy_land,tauy_land_star,tauy_ssi,tauy_ssi_star                  &
,fqw_tile,epot_tile,ftl_tile,fqw_ice,ftl_ice,e_sea,h_sea          &
,l_correct,l_flux_bc                                              &
)


!-----------------------------------------------------------------------

! Calculate surface scalar fluxes, temperatures only at the 1st call
! of the subroutine (first stage of the new BL solver) using standard
! MOSES2 physics and equations. These are the final values for this
! timestep and there is no need to repeat the calculation.
!-----------------------------------------------------------------------

IF ( .NOT. l_correct ) THEN
!-----------------------------------------------------------------------
!! 6.1 Convert FTL to sensible heat flux in Watts per square metre.
!-----------------------------------------------------------------------

!Cfpp$ Select(CONCUR)
  DO j=tdims%j_start,tdims%j_end
    DO i=tdims%i_start,tdims%i_end
      ftl_1(i,j) = ftl_1(i,j)*cp
    END DO
  END DO

  DO j=tdims%j_start,tdims%j_end
    DO i=tdims%i_start,tdims%i_end
      h_sea(i,j) = cp*h_sea(i,j)
    END DO
  END DO

  DO n=1,nice_use
    DO j=tdims%j_start,tdims%j_end
      DO i=tdims%i_start,tdims%i_end
        ftl_ice(i,j,n) = cp*ftl_ice(i,j,n)
      END DO
    END DO
  END DO


  DO n=1,ntiles
    DO k=1,tile_pts(n)
      l = tile_index(k,n)
      ftl_tile(l,n) = cp*ftl_tile(l,n)
    END DO
  END DO

!CABLE{
  DO N=1,NTILES
    DO L=1,LAND_PTS
      MELT_TILE(L,N) = 0.
    ENDDO
  ENDDO
call cable_control7(                      &
                     dtl1_1, &
                     dqw1_1, &
                     T_SOIL, &
                      FTL_1,&
                      FQW_1,  &
                     SURF_HT_FLUX_LAND, &
                     ECAN_TILE,&
                     ESOIL_TILE,&
                     EI_TILE,&
                     T1P5M_TILE, &
                     Q1P5M_TILE, &
                     MELT_TILE &
                  )
 
! DEPENDS ON:cable_implicit_driver
  call cable_implicit_driver( cable% um% LS_RAIN, cable% um% CONV_RAIN, &
                  cable% um% LS_SNOW, cable% um% CONV_SNOW, cable% im% dtl_1,  &
                  cable% im% dqw_1, cable% im% T_SOIL, cable%cable% TSOIL_TILE,&
                  cable% um% SMCL, cable% cable% SMCL_TILE,                    &
                  cable% mp% timestep_width, cable% um%SMVCST, cable% um% STHF,&
                  cable% cable% STHF_TILE, cable% um% STHU,&
                  cable% cable% STHU_TILE, cable% um% snow_tile,               &
                  cable% cable% SNOW_RHO1L, cable% cable% SNOW_FLG3L,          &
                  cable% cable% SNOW_DEPTH3L, cable% cable% SNOW_MASS3L,       &
                  cable% cable% SNOW_RHO3L, cable% cable% SNOW_TMP3L,          &
                  cable% cable% SNOW_COND, cable% im% FTL_1,                   &
                  cable% um% FTL_TILE, cable% im% FQW_1, cable% um% FQW_TILE,  &
                  cable% um% TSTAR_TILE, cable% im% SURF_HT_FLUX_LAND,         &
                  cable% im% ECAN_TILE, cable% im% ESOIL_TILE,                 &
                  cable% im% EI_TILE, cable% um% RADNET_TILE,                  &
                  cable% um% TOT_ALB,  cable% cable% SNow_AGE,                 &
                  cable% um% CANOPY, cable% um% GS,                            &
                  cable% im% T1P5M_TILE, cable% im% Q1P5M_TILE,                &
                  cable% um% CANOPY_GB, cable% um% Fland,                      &
                  cable% im% MELT_TILE, cable% um% DIM_CS1,                    &
                  cable% um% DIM_CS2, cable% um% NPP,                          &
                  cable% um% NPP_FT, cable% um% GPP,                           &
                  cable% um% GPP_FT,cable% um% RESP_S,                         &
                  cable% um% RESP_S_TOT,cable% um% RESP_S_TILE,                &
                  cable% um% RESP_P,cable% um% RESP_P_FT,                      &
                  cable% um% G_LEAF, &
                  cable% hyd% sub_surf_roff, &
                  cable% hyd% surf_roff, &
                  cable% hyd% tot_tfall, &
                  cable% hyd% LYING_SNOW &
                  )
!CABLE}


!-----------------------------------------------------------------------
! Land surface calculations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Optional error check : test for negative top soil layer temperature
!-----------------------------------------------------------------------
  IF (l_neg_tstar) THEN
    DO l=1,land_pts
      IF (t_soil(l,1) < 0) THEN
        error = 1
        WRITE(6,*) '*** ERROR DETECTED BY ROUTINE SF_IMPL2 ***'
        WRITE(6,*) 'NEGATIVE TEMPERATURE IN TOP SOIL LAYER AT '
        WRITE(6,*) 'LAND POINT ',l
      END IF
    END DO
  END IF

!-----------------------------------------------------------------------
!!   Diagnose the land surface temperature
!-----------------------------------------------------------------------

  DO n=1,ntiles
    DO k=1,tile_pts(n)
      l = tile_index(k,n)
      tstar_tile_old(l,n) = tstar_tile(l,n)
!CABLE: use CABLE's tstar_tile
!      tstar_tile(l,n) = tstar_tile_old(l,n) + dtstar_tile(l,n)
    END DO
  END DO


!-----------------------------------------------------------------------
!! 7.  Surface evaporation components and updating of surface
!!     temperature (P245, routine SF_EVAP).
!-----------------------------------------------------------------------
  IF(  cable% um% l_cable ) THEN
    DO N=1,NTILES
      DO L=1,LAND_PTS
        ELAKE_TILE(L,N) = 0.
        EI_TILE(L,N) = 0.
      ENDDO
    ENDDO

    DO J=1,cable% mp% rows
     DO I=1,cable% mp% row_length
      ECAN(I,J) = 0.
      ESOIL(I,J) = 0.
     ENDDO
    ENDDO

    DO N=1,NTILES
     DO K=1,TILE_PTS(N)
       L = TILE_INDEX(K,N)
       J=(LAND_INDEX(L)-1)/cable% mp% row_length + 1
       I = LAND_INDEX(L) - (J-1)*cable% mp% row_length
       ECAN(I,J) = ECAN(I,J) + TILE_FRAC(L,N)*ECAN_TILE(L,N)
       ESOIL(I,J) = ESOIL(I,J) + TILE_FRAC(L,N)*ESOIL_TILE(L,N)          
     ENDDO
    ENDDO   
    EXT = 0. ! MRD

  ELSE ! ( cable% um% l_cable )

! DEPENDS ON: sf_evap
  CALL sf_evap (                                                  &
    land_pts,ntiles,                                              &
    land_index,tile_index,tile_pts,sm_levels,fland,               &
    ashtf_prime_tile,canopy,dtrdz_charney_grid_1,flake,fraca,     &
    snow_tile,resfs,resft,rhokh_tile,tile_frac,smc,wt_ext_tile,   &
    timestep,GAMMA,fqw_1,fqw_tile,ftl_1,ftl_tile,tstar_tile,      &
    ecan,ecan_tile,elake_tile,esoil,esoil_tile,ei_tile,ext        &
    )
  ENDIF

!-----------------------------------------------------------------------
!!     Surface melting of sea-ice and snow on land tiles.
!-----------------------------------------------------------------------

  ei_land(:,:)=0.0
  snowmelt(:,:)=0.0

! Lake initialisation
  melt_ice_tile(:,:)=0.0

  DO n=1,ntiles
!CABLE:!
!! DEPENDS ON: sf_melt
!    CALL sf_melt (                                                &
!      land_pts,land_index,                                        &
!      tile_index(:,n),tile_pts(n),flandg,                         &
!      alpha1(:,n),ashtf_prime_tile(:,n),dtrdz_charney_grid_1,     &
!      resft(:,n),rhokh_tile(:,n),tile_frac(:,n),timestep,GAMMA,   &
!      ei_tile(:,n),fqw_1,ftl_1,fqw_tile(:,n),ftl_tile(:,n),       &
!      tstar_tile(:,n),snow_tile(:,n),snowdep_surf(:,n),           &
!      melt_tile(:,n)                                              &
!      )

!-----------------------------------------------------------------------
! thermodynamic, flux contribution of melting ice on the FLake lake tile
!-----------------------------------------------------------------------
!    IF (     (l_flake_model   ) &
!        .AND.(.NOT.l_aggregate) &
!        .AND.(n == lake       ) ) THEN

! lake_h_ice is only initialised if FLake is on.
!  lake_ice_mass=lake_h_ice * rho_ice

!! DEPENDS ON: sf_melt
!    CALL sf_melt (                                                &
!      land_pts,land_index,                                        &
!      tile_index(:,n),tile_pts(n),flandg,                         &
!      alpha1(:,n),ashtf_prime_tile(:,n),dtrdz_charney_grid_1,     &
!      resft(:,n),rhokh_tile(:,n),tile_frac(:,n),timestep,GAMMA,   &
!      ei_tile(:,n),fqw_1,ftl_1,fqw_tile(:,n),ftl_tile(:,n),       &
!      tstar_tile(:,n),lake_ice_mass,lake_ice_mass/rho_snow_const, &
!      melt_ice_tile(:,n)                                          &
!        )
!    END IF

!-----------------------------------------------------------------------
!  Increment snow by sublimation and melt
!-----------------------------------------------------------------------
    DO k=1,tile_pts(n)
      l = tile_index(k,n)
      j=(land_index(l)-1)/t_i_length + 1
      i = land_index(l) - (j-1)*t_i_length
      ei_land(i,j) = ei_land(i,j) + tile_frac(l,n)*ei_tile(l,n)
      snowmelt(i,j) = snowmelt(i,j) +                             &
                      tile_frac(l,n)*melt_tile(l,n)
    END DO

  END DO

  IF (smlt) THEN
    DO j=tdims%j_start,tdims%j_end
      DO i=tdims%i_start,tdims%i_end
        snomlt_surf_htf(i,j) = lf*snowmelt(i,j)
      END DO
    END DO
  END IF

  DO j=tdims%j_start,tdims%j_end
    DO i=tdims%i_start,tdims%i_end
    surf_ht_flux_land(i,j) = 0.
   END DO
  END DO

!!CABLE{
!!CABLE: taken out as vars unknown, howevermay not need
!       IF(NICE  ==  1)THEN
!       DO J=1,cable% mp% rows
!        DO I=1,cable% mp% row_length
!          DTSTAR=0.0
!         IF ( FLANDG(I,J) <  1.0 .AND. ICE_FRACT(I,J) >  0.0 ) THEN
!!-----------------------------------------------------------------------
!!   Melt sea-ice if TSTAR > TSTARMAX
!!-----------------------------------------------------------------------
!           EI_SICE(I,J,:) = FQW_ICE(I,J,:)
!           TSTARMAX = ICE_FRACT(I,J)*TM                                  &
!     &         + (1.0 - ICE_FRACT(I,J))*TSTAR_SEA(I,J)
!           !CABLE: taken out as vars unknown, howevermay not need
!           !IF ( TSTAR_SSI0(I,J)  >   TSTARMAX ) THEN
!           !  RHOKH1_PRIME = 1. / ( 1. / RHOKH_SICE(I,J)                &
!     &     !                  + ICE_FRACT(I,J)*GAMMA*DTRDZ_CHARNEY_GRID_1(I,J) )
!           !  DTSTAR = TSTARMAX - TSTAR_SSI0(I,J)
!           !  LSMELT = (CP + (LC + LF)*ALPHA1_SICE(I,J))*RHOKH1_PRIME     &
!     &     !                                            + ASHTF(I,J)
!           !  DFTL_M = CP * RHOKH1_PRIME * DTSTAR
!           !  DFQW_M = ALPHA1_SICE(I,J) * RHOKH1_PRIME * DTSTAR
!           !  TSTAR_SSI0(I,J) = TSTARMAX
!           !  SICE_MELT0(I,J) = - LSMELT * DTSTAR
!           !  IF (SIMLT) SICE_MLT_HTF(I,J,1) = SICE_MELT0(I,J)
!           !  FTL_1(I,J) = FTL_1(I,J) + (1.0-FLANDG(I,J))*DFTL_M
!           !  FQW_1(I,J) = FQW_1(I,J) + (1.0-FLANDG(I,J))*DFQW_M
!           !  EI_SICE(I,J,:) = EI_SICE(I,J,:) + DFQW_M
!           !  FTL_ICE(I,J,:) = FTL_ICE(I,J,:) + DFTL_M
!           !  FQW_ICE(I,J,:) = FQW_ICE(I,J,:) + DFQW_M
!
!           !ENDIF
!
!         ENDIF
!        ENDDO
!       ENDDO
!
!       ELSE
!
!        DO J=1,cable% mp% rows
!         DO I=1,cable% mp% row_length
!          IF ( FLANDG(I,J) <  1.0 .AND. ICE_FRACT(I,J) >  0.0 ) THEN   
!!-----------------------------------------------------------------------
!!   Melt sea-ice if TSTAR > TSTARMAX
!!-----------------------------------------------------------------------
!            EI_SICE(I,J,:) = FQW_ICE(I,J,:)
!            DTSTAR=0.0
!            DO N=1,NICE
!              IF (ICE_FRACT_NCAT(I,J,N) >  0.0) THEN
!                TSTARMAX = ICE_FRACT_NCAT(I,J,N)*TM
!                IF (TSTAR_SIC(I,J,N) >  TSTARMAX) THEN
!                  RHOKH1_PRIME = 1. / ( 1. / RHOKH_SICE(I,J)           &
!     &                 + ICE_FRACT_NCAT(I,J,N)*GAMMA*DTRDZ_CHARNEY_GRID_1(I,J) )
!                  DTSTAR = TSTARMAX - TSTAR_SIC(I,J,N)
!                  LSMELT = (CP + (LC + LF)*ALPHA1_SICE(I,J))*RHOKH1_PRIME&
!     &                                                 + ASHTF(I,J)
!                  DFTL_M = CP * RHOKH1_PRIME * DTSTAR
!                  DFQW_M = ALPHA1_SICE(I,J) * RHOKH1_PRIME * DTSTAR
!                  TSTAR_SIC(I,J,N) = TSTARMAX
!                  SICE_MELT(I,J,N) = - LSMELT * DTSTAR
!                  IF (SIMLT) SICE_MLT_HTF(I,J,N) = SICE_MELT(I,J,N)
!                  FTL_1(I,J)   = FTL_1(I,J) + (1.0-FLANDG(I,J))*DFTL_M
!                  FQW_1(I,J)   = FQW_1(I,J) + (1.0-FLANDG(I,J))*DFQW_M
!                  EI_SICE(I,J,:) = EI_SICE(I,J,:) + DFQW_M
!                  FTL_ICE(I,J,:) = FTL_ICE(I,J,:) + DFTL_M
!                  FQW_ICE(I,J,:) = FQW_ICE(I,J,:) + DFQW_M
!
!                ENDIF
!              ENDIF
!            ENDDO
!          ENDIF
!         ENDDO
!        ENDDO
!       ENDIF
!
!!CABLE}

  IF (     (l_flake_model   ) &
      .AND.(.NOT.l_aggregate) ) THEN
    DO j=tdims%j_start,tdims%j_end
     DO i=tdims%i_start,tdims%i_end
      surf_ht_flux_lake(i,j) = 0.0
! initialise the non-lake fraction to one, not zero,
! in case there should ever be more than one lake tile, see below
      non_lake_frac(    i,j) = 1.0
     END DO
    END DO
  ENDIF

  DO l=1,land_pts
    j=(land_index(l)-1)/t_i_length + 1
    i = land_index(l) - (j-1)*t_i_length
    tstar_land(i,j) = 0.
  END DO

  IF (l_skyview) THEN
    DO n=1,ntiles
      DO k=1,tile_pts(n)
        l = tile_index(k,n)
        j=(land_index(l)-1)/tdims%i_end + 1
        i = land_index(l) - (j-1)*tdims%i_end
        !CABLE
        !radnet_tile(l,n) = sw_tile(l,n) +   emis_tile(l,n)*       &
        !  sky(i,j)*( lw_down(i,j) - sbcon*tstar_tile(l,n)**4 )
      END DO
    END DO
  ELSE
    DO n=1,ntiles
      DO k=1,tile_pts(n)
        l = tile_index(k,n)
        j=(land_index(l)-1)/tdims%i_end + 1
        i = land_index(l) - (j-1)*tdims%i_end
        !CABLE
        !radnet_tile(l,n) = sw_tile(l,n) +   emis_tile(l,n)*       &
        !           ( lw_down(i,j) - sbcon*tstar_tile(l,n)**4 )
      END DO
    END DO
  END IF

  DO n=1,ntiles
    DO k=1,tile_pts(n)
      l = tile_index(k,n)
      j=(land_index(l)-1)/t_i_length + 1
      i = land_index(l) - (j-1)*t_i_length
      le_tile(l,n) = lc*ecan_tile(l,n) + lc*esoil_tile(l,n) +     &
                     lc*elake_tile(l,n) + ls*ei_tile(l,n)
      surf_ht_store(l,n) = (canhc_tile(l,n)/timestep) *           &
                           (tstar_tile(l,n) - tstar_tile_old(l,n))
      surf_htf_tile(l,n) = radnet_tile(l,n) + anthrop_heat(l,n) - &
                          ftl_tile(l,n) -                         &
                          le_tile(l,n) -                          &
                          lf*(melt_tile(l,n)+melt_ice_tile(l,n))- &
                          surf_ht_store(l,n)
! separate out the lake heat flux for FLake
! and replace the snow-melt and ice-melt heat flux
! so Flake can do its melting
      IF (     (l_flake_model   ) &
          .AND.(.NOT.l_aggregate) &
          .AND.(n == lake       ) ) THEN
        surf_ht_flux_lake(i,j) = surf_htf_tile(l,n)               &
                      + lf * (melt_tile(l,n)+melt_ice_tile(l,n))
        non_lake_frac(    i,j) = non_lake_frac(i,j) - tile_frac(l,n)
      ELSE
        !CABLE
        !surf_ht_flux_land(i,j) = surf_ht_flux_land(i,j)           &
        !                  + tile_frac(l,n) * surf_htf_tile(l,n)
      END IF
      tstar_land(i,j) = tstar_land(i,j)                           &
                 + tile_frac(l,n)*tstar_tile(l,n)
    END DO
  END DO

! normalise the non-lake surface heat flux
!CABLE
!  IF (     (l_flake_model   ) &
!      .AND.(.NOT.l_aggregate) ) THEN
!    DO j=tdims%j_start,tdims%j_end
!      DO i=tdims%i_start,tdims%i_end
!! be careful about gridboxes that are all lake
!        !IF (non_lake_frac(i,j) > EPSILON(0.0)) THEN
!        !CABLE
!        !  surf_ht_flux_land(i,j) =   surf_ht_flux_land(i,j)       &
!        !                           / non_lake_frac(i,j)
!        !ENDIF
!      END DO
!    END DO
!  END IF


!-----------------------------------------------------------------------
! Optional error check : test for negative surface temperature
!-----------------------------------------------------------------------
  IF (l_neg_tstar) THEN
    DO l=1,land_pts
      j=(land_index(l)-1)/t_i_length + 1
      i = land_index(l) - (j-1)*t_i_length
      IF (tstar_land(i,j) < 0) THEN
        error = 1
        WRITE(6,*) '*** ERROR DETECTED BY ROUTINE SF_IMPL2 ***'
        WRITE(6,*) 'NEGATIVE SURFACE TEMPERATURE AT LAND POINT ',l
      END IF
    END DO
  END IF


!-----------------------------------------------------------------------
! Sea and sea-ice surface calculations
!-----------------------------------------------------------------------

! Set control logical (l_sice_new_code) to determine how to calculate the 
! sea ice surface temperature.  l_sice_new_code = T is the method that is 
! compatible with using the sea ice categories fully in the radiation and 
! surface exchange code (so nice_use=nice).  l_sice_new_code = F is the 
! old method that only uses the categories in the implicit surface exchange 
! code (so nice_use = 1).  

! NOTE that nice_use=nice=1 can use either scheme and the choice is
! determined by the logical l_tstar_sice_new set in switches.F90.  The 
! difference between the 2 methods is small but causes differences in the 
! results that are larger than bit level, hence the need to control which 
! method is used.

  IF (nice_use == 1 .AND. nice == 1) THEN
    ! If nice_use=nice=1 : Choice method determined by logical 
    ! l_tstar_sice_new
    l_sice_new_code = l_tstar_sice_new

  ELSE IF (nice_use /= nice) THEN
    ! Old calculation must be used
    l_sice_new_code = .FALSE.

  ELSE IF (nice > 1) THEN
    ! New calculation must be used
    l_sice_new_code = .TRUE.
  END IF

  IF (.not.l_sice_new_code) THEN
    ALLOCATE(tstar_sic(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice))
    tstar_sic(:,:,:)= 0.0
  END IF
  surf_ht_flux_sice_sm(:,:)=0.0
  sice_melt(:,:,:)=0.0
  ei_sice(:,:,:)=fqw_ice(:,:,:)
  sice_mlt_htf(:,:,:)=0.0
  sea_ice_htf(:,:,:)=0.0

!-----------------------------------------------------------------------
! Store old surface temperature for sea and sea-ice if using the
! decoupled diagnostic.
!-----------------------------------------------------------------------
  IF (IScrnTDiag == IP_ScrnDecpl2) THEN
    ALLOCATE(tstar_ssi_old(tdims%i_start:tdims%i_end,                    &
                           tdims%j_start:tdims%j_end))
    tstar_ssi_old(:,:) = tstar_ssi(:,:)
  ELSE
    ALLOCATE(tstar_ssi_old(1,1))
  END IF

!-----------------------------------------------------------------------
! Diagnose the surface temperature for points with sea-ice
! Note that k_sice = 2.0*thermal conductivity/surface layer thickness
!-----------------------------------------------------------------------

  IF (l_sice_new_code) THEN       

    ! Update tstar_sice_cat using dtstar
    DO n=1,nice_use
      DO j=tdims%j_start,tdims%j_end
        DO i=tdims%i_start,tdims%i_end
          IF ( flandg(i,j) < 1.0 .AND. ice_fract_ncat(i,j,n) > 0 ) THEN
            tstar_sice_cat_old(i,j,n) = tstar_sice_cat(i,j,n)
            tstar_sice_cat(i,j,n) = tstar_sice_cat_old(i,j,n) + dtstar(i,j,n)
          ENDIF
        END DO
      END DO
    END DO

  ELSE  ! Use old code

    ! Update tstar_sic using the surface heat equation
    DO j=tdims%j_start,tdims%j_end
      DO i=tdims%i_start,tdims%i_end
        IF ( flandg(i,j) < 1.0 .AND. ice_fract(i,j) > 0. ) THEN
          surf_ht_flux_sice_sm(i,j) = radnet_sice(i,j,1) -          &
             4.0*emis_sice*sbcon*(tstar_sice_cat(i,j,1)**3.0)*      & 
                 dtstar(i,j,1) - ftl_ice(i,j,1) - ls*fqw_ice(i,j,1) 
          DO n=1,nice
            IF (ice_fract_ncat(i,j,n) > 0.) THEN
              tstar_sic(i,j,n) = ti(i,j,n) +                        &
                       surf_ht_flux_sice_sm(i,j)/k_sice(i,j,n)
            END IF
          END DO
        END IF
      END DO
    END DO
  END IF

  IF (l_sice_new_code) THEN

    DO n=1, nice_use
! DEPENDS ON: sf_melt
      CALL sf_melt (                                                &
        ssi_pts,ssi_index,                                          &
        sice_index_ncat(:,n),sice_pts_ncat(n),fssi,                 &
        alpha1_sice,ashtf_prime,dtrdz_charney_grid_1,               &
        array_one,rhokh_sice,sice_frac_ncat(:,n),timestep,GAMMA,    &
        ei_sice(:,:,n),fqw_1,ftl_1,fqw_ice(:,:,n),ftl_ice(:,:,n),   &
        tstar_sice_cat(:,:,n),array_one_e_six,                      &
        array_one_e_six/rho_snow_const,                             &
        sice_melt(:,:,n)                                            &
        )

      DO k=1,sice_pts_ncat(n)
        l = sice_index_ncat(k,n)
        j=(ssi_index(l)-1)/t_i_length + 1
        i = ssi_index(l) - (j-1)*t_i_length
        IF (simlt) sice_mlt_htf(i,j,n) = lf * sice_melt(i,j,n)
      END DO

    END DO

  ELSE ! Use old code

    DO n=1,nice

! Since sea-ice categories are not actually tiled for their surface
! fluxes here, then the increment to ftl_ice, fqw_ice and
! ei_sice are not correctly weighted in sf_melt. Hence need to keep
! the increments and update ftl_ice, fqw_ice and ei_sice with
! weighted contributions below
      dftl_sice_ncat(:,:)=0.0
      dfqw_sice_ncat(:,:)=0.0
      dei_sice_ncat(:,:)=0.0

! DEPENDS ON: sf_melt
      CALL sf_melt (                                                &
        ssi_pts,ssi_index,                                          &
        sice_index_ncat(:,n),sice_pts_ncat(n),fssi,                 &
        alpha1_sice,ashtf_prime,dtrdz_charney_grid_1,               &
        array_one,rhokh_sice,sice_frac_ncat(:,n),timestep,GAMMA,    &
        dei_sice_ncat,fqw_1,ftl_1,dfqw_sice_ncat,dftl_sice_ncat,    &
        tstar_sic(:,:,n),array_one_e_six,                           &
        array_one_e_six/rho_snow_const,                             &
        sice_melt(:,:,n)                                            &
        )

      DO k=1,sice_pts_ncat(n)
        l = sice_index_ncat(k,n)
        j=(ssi_index(l)-1)/t_i_length + 1
        i = ssi_index(l) - (j-1)*t_i_length
        IF (simlt) sice_mlt_htf(i,j,n) = lf * sice_melt(i,j,n)
! Add weighted increments to ftl_ice, fqw_ice and ei_sice
        ftl_ice(i,j,1)=ftl_ice(i,j,1)                               &
           +( sice_frac_ncat(l,n)/sice_frac(l) )*dftl_sice_ncat(i,j)
        fqw_ice(i,j,1)=fqw_ice(i,j,1)                               &
           +( sice_frac_ncat(l,n)/sice_frac(l) )*dfqw_sice_ncat(i,j)
        ei_sice(i,j,1)=ei_sice(i,j,1)                               &
           +( sice_frac_ncat(l,n)/sice_frac(l) )*dei_sice_ncat(i,j)
      END DO

    END DO

  END IF

!-----------------------------------------------------------------------
!!     Gridbox-mean surface temperature and net surface heat fluxes
!-----------------------------------------------------------------------

! Assign SW flux on categories to radnet_sice to avoid cumbersome
! indexing in the following loops.
  radnet_sice(:,:,:) = 0.0
  IF  (nice_use == nice) THEN
!   Radiative fluxes are on all categories in use, so use the
!   full arrays.
    DO n=1,nice
      DO k=1,sice_pts_ncat(n)
        l = sice_index_ncat(k,n)
        j=(ssi_index(l)-1)/t_i_length + 1
        i = ssi_index(l) - (j-1)*t_i_length
        radnet_sice(i,j,n) = sw_sice(l,n)
#if defined(UM_JULES)
        IF (.NOT.l_emis_ssi_full) THEN
!         Match old calculation to ensure bit-comparison.
          radnet_sice(i,j,n) = ice_fract_ncat(i,j,n)*sw_sice(l,n)
        ENDIF
#endif
      ENDDO
    ENDDO
  ELSE
!   In this case n_ice_use must be 1, so indexing is over all sea-ice
!   points.
    DO n=1,nice_use
      DO k=1,sice_pts
        l = sice_index(k)
        j=(ssi_index(l)-1)/t_i_length + 1
        i = ssi_index(l) - (j-1)*t_i_length
        radnet_sice(i,j,n) = sw_sice(l,n)
#if defined(UM_JULES)
        IF (.NOT.l_emis_ssi_full) THEN
!         Match old calculation to ensure bit-comparison.
          radnet_sice(i,j,n) = ice_fract_cat_use(i,j,n)*sw_sice(l,n)
        ENDIF
#endif
      ENDDO
    ENDDO
  ENDIF

  IF (l_sice_new_code) THEN

    DO j=tdims%j_start,tdims%j_end
      DO i=tdims%i_start,tdims%i_end
        surf_ht_flux_sice_sm(i,j) = 0.
        DO n=1,nice
          surf_ht_flux_sice(i,j,n)= 0.
        END DO
        IF (flandg(i,j) < 1.0 .AND. ice_fract(i,j) > 0.) THEN
          tstar_sice(i,j)= 0.0
          DO n=1,nice
            IF (ice_fract_ncat(i,j,n) > 0.) THEN
              tstar_sice(i,j) = tstar_sice(i,j) +                   &
                                (ice_fract_ncat(i,j,n)/             &
                               ice_fract(i,j)) * tstar_sice_cat(i,j,n)
            ELSE
              tstar_sice_cat(i,j,n) = tfs   ! copy setting in sice_htf
            END IF
          END DO
          tstar_ssi(i,j)=(1.-ice_fract(i,j))*tstar_sea(i,j) +       &
                          ice_fract(i,j)*tstar_sice(i,j)


          DO n=1,nice
            IF (ice_fract_ncat(i,j,n) > 0.) THEN

#if defined(UM_JULES)
              IF (.NOT.l_emis_ssi_full) THEN
!               Replicate initial implementation with omission of 
!               emissivity at this point.
                radnet_sice(i,j,n) = ( (radnet_sice(i,j,n) +        &
                  ice_fract_cat_use(i,j,n)*lw_down(i,j) ) /         &
                  ice_fract_cat_use(i,j,n) ) - emis_sice *          &
                  sbcon*tstar_sice_cat(i,j,n)**4
              ELSE
                radnet_sice(i,j,n) = radnet_sice(i,j,n) + emis_sice * &
                  ( lw_down(i,j) - sbcon*tstar_sice_cat(i,j,n)**4 )
              ENDIF
#else
              radnet_sice(i,j,n) = radnet_sice(i,j,n) + emis_sice * &
                ( lw_down(i,j) - sbcon*tstar_sice_cat(i,j,n)**4 )
#endif

              surf_ht_flux_sice(i,j,n) = radnet_sice(i,j,n) -       &  
                               ftl_ice(i,j,n) - ls*fqw_ice(i,j,n) - &
                               lf*sice_melt(i,j,n)
              surf_ht_flux_sice_sm(i,j) = surf_ht_flux_sice_sm(i,j)+&
                           (ice_fract_ncat(i,j,n)/ice_fract(i,j))*  &
                                surf_ht_flux_sice(i,j,n)
            END IF
          END DO

        ELSE IF (flandg(i,j) < 1.0) THEN  ! non-icy ocean point
          tstar_sice_cat(i,j,:) = tfs  
        END IF
        IF (NICE .EQ. 1) THEN 
          tstar_sice(i,j) = tstar_sice_cat(i,j,1)  ! Ensure these are the
                                                   ! same at all points
        END IF
      END DO
    END DO

  ELSE   ! Use old code

    DO j=tdims%j_start,tdims%j_end
      DO i=tdims%i_start,tdims%i_end
        surf_ht_flux_sice_sm(i,j) = 0.
        DO n=1,nice
          surf_ht_flux_sice(i,j,n)=0.
        END DO
        IF (flandg(i,j) < 1.0 .AND. ice_fract(i,j) > 0.) THEN
          tstar_sice(i,j)= 0.0
          DO n=1,nice
            IF (ice_fract_ncat(i,j,n) > 0.) THEN
              tstar_sice(i,j) = tstar_sice(i,j) +                   &
                                (ice_fract_ncat(i,j,n)/             &
                                 ice_fract(i,j)) * tstar_sic(i,j,n)
            END IF
          END DO
          tstar_ssi(i,j)=(1.-ice_fract(i,j))*tstar_sea(i,j) +       &
                          ice_fract(i,j)*tstar_sice(i,j)
#if defined(UM_JULES)
          IF (.NOT.l_emis_ssi_full) THEN
!           Replicate initial implementation matching calculations
!           exactly to give bit comparison.
            radnet_sice(i,j,1) = radnet_sice(i,j,1) +               &
              ice_fract_cat_use(i,j,1)*lw_down(i,j)
            radnet_sice(i,j,1) = radnet_sice(i,j,1) /               &
              ice_fract_cat_use(i,j,1)
            radnet_sice(i,j,1) = radnet_sice(i,j,1) -               &
              emis_sice * sbcon*tstar_sice(i,j)**4
          ELSE
            radnet_sice(i,j,1) = radnet_sice(i,j,1) + emis_sice *   &
              ( lw_down(i,j) - sbcon*tstar_sice(i,j)**4 )
          ENDIF
#else
          radnet_sice(i,j,1) = radnet_sice(i,j,1) + emis_sice *     &
            ( lw_down(i,j) - sbcon*tstar_sice(i,j)**4 )
#endif

          DO n=1,nice
            IF (ice_fract_ncat(i,j,n) > 0.) THEN
              surf_ht_flux_sice(i,j,n) = radnet_sice(i,j,1) -       &
                               4.0*emis_sice*sbcon*                 & 
                               tstar_sice(i,j)**3 *                 &
                               (tstar_sic(i,j,n)-tstar_sice(i,j)) - &
                               ftl_ice(i,j,1) - ls*fqw_ice(i,j,1) - &
                               lf*sice_melt(i,j,n)
              surf_ht_flux_sice_sm(i,j) = surf_ht_flux_sice_sm(i,j)+&
                           (ice_fract_ncat(i,j,n)/ice_fract(i,j))*  &
                            surf_ht_flux_sice(i,j,n)

            END IF
          END DO

        ELSE IF (flandg(i,j) < 1.0) THEN  ! non-icy ocean point
          tstar_sice(i,j) = tfs  
        END IF

        tstar_sice_cat(i,j,1) = tstar_sice(i,j)  ! Ensure these are the
                                                 ! same at all points
      END DO
    END DO

  END IF

! Convert sea and sea-ice fluxes to be fraction of grid-box
! (as required by sea and sea-ice modellers)
  DO j=tdims%j_start,tdims%j_end
    DO i=tdims%i_start,tdims%i_end
      h_sea(i,j)=(1.0-ice_fract(i,j))*h_sea(i,j)
      e_sea(i,j)=(1.0-ice_fract(i,j))*e_sea(i,j)
      surf_ht_flux_sice_sm(i,j)=ice_fract(i,j)*surf_ht_flux_sice_sm(i,j)
      ei_sice_sm(i,j) = 0.0
      DO n=1,nice_use
        ei_sice(i,j,n)=ice_fract_cat_use(i,j,n)*ei_sice(i,j,n)
        ei_sice_sm(i,j)= ei_sice_sm(i,j) + ei_sice(i,j,n)
        ftl_ice(i,j,n)=ice_fract_cat_use(i,j,n)*ftl_ice(i,j,n)
        fqw_ice(i,j,n)=ice_fract_cat_use(i,j,n)*fqw_ice(i,j,n)
        radnet_sice(i,j,n)=ice_fract_cat_use(i,j,n)*radnet_sice(i,j,n)
      END DO
    END DO
  END DO

!-----------------------------------------------------------------------
! GBM diagnostic calculations
!-----------------------------------------------------------------------

  DO j=tdims%j_start,tdims%j_end
    DO i=tdims%i_start,tdims%i_end
      qim_1(i,j)=qw_1(i,j) + dqw1_1(i,j)-ctctq1(i,j)*fqw_1(i,j)
      tim_1(i,j)=tl_1(i,j) + dtl1_1(i,j)-ctctq1(i,j)*ftl_1(i,j)/cp
   END DO
  END DO

  DO j=tdims%j_start,tdims%j_end
    DO i=tdims%i_start,tdims%i_end
      tstar(i,j)=flandg(i,j)*tstar_land(i,j)                      &
        +(1.-flandg(i,j))*tstar_ssi(i,j)
      ei(i,j)=flandg(i,j)*ei_land(i,j)                            &
        +(1.-flandg(i,j))*ei_sice_sm(i,j)
      surf_ht_flux(i,j)=flandg(i,j)*surf_ht_flux_land(i,j)        &
        +(1.-flandg(i,j))*surf_ht_flux_sice_sm(i,j)
      rhokh_mix(i,j)=rhokh(i,j)
    END DO
  END DO

! TOA outward LW radiation after boundary layer

  tstar_rad4(:,:)=0.0

  DO j=tdims%j_start,tdims%j_end
    DO i=tdims%i_start,tdims%i_end
!     The contribution from the sea is removed in UM imp_solver to keep it   
!     consistent with the UM radiation scheme (see olr comment there)
      tstar_rad4(i,j) = tstar_rad4(i,j) + (1.0-flandg(i,j))*      &
                        (1.0-ice_fract(i,j))*emis_sea*            &
                        tstar_sea(i,j)**4
      DO n=1,nice_use
        tstar_rad4(i,j) = tstar_rad4(i,j) + (1.0-flandg(i,j))*    &
                   ice_fract_cat_use(i,j,n)*emis_sice*            & 
                   tstar_sice_cat(i,j,n)**4

      END DO
    END DO
  END DO

  DO n=1,ntiles
    DO k=1,tile_pts(n)
      l = tile_index(k,n)
      j=(land_index(l)-1)/t_i_length + 1
      i = land_index(l) - (j-1)*t_i_length
!     For historical compatibility, the addjustment of the OLR
!     may be made with or without the surface emissivity.
      IF (l_dolr_land_black) THEN
        tstar_rad4(i,j) = tstar_rad4(i,j) + flandg(i,j)*          &
                          tile_frac(l,n)*tstar_tile(l,n)**4
      ELSE
        tstar_rad4(i,j) = tstar_rad4(i,j) + flandg(i,j)*          &
                          tile_frac(l,n)*emis_tile(l,n)*          &
                          tstar_tile(l,n)**4
      END IF
    END DO
  END DO

  DO j=tdims%j_start,tdims%j_end
    DO i=tdims%i_start,tdims%i_end
      olr(i,j) = olr(i,j) + sbcon*tstar_rad4(i,j)
    END DO
  END DO


!-----------------------------------------------------------------------
!!     Specific humidity and temperature at 1.5 metres.
!-----------------------------------------------------------------------
! DEPENDS ON: screen_tq
  CALL screen_tq (                                                &
    land_pts,ntiles,                                              &
    land_index,tile_index,tile_pts,flandg,                        &
    sq1p5,st1p5,chr1p5m,chr1p5m_sice,pstar,qim_1,resft,           &
    tile_frac,tim_1,tstar_ssi,tstar_tile,                         &
    z0hssi,z0h_tile,z0mssi,z0m_tile,z1,                           &
    timestep,tstar_ssi_old,tstar_tile_old,                        &
    l_co2_interactive, co2_mmr, co2_3d,                           &
    f3_at_p, uStarGBM, rho1,                                      &
    TScrnDcl_SSI,TScrnDcl_TILE,tStbTrans,                         &
    q1p5m,q1p5m_tile,t1p5m,t1p5m_tile,                            &
    lq_mix_bl                                                     &
    )

! Release space allocated for the transitional diagnostic.
  DEALLOCATE(tstar_ssi_old)
  if (.not.l_sice_new_code) DEALLOCATE(tstar_sic)

!-----------------------------------------------------------------------
!! 9.  Calculate surface latent heat flux.
!-----------------------------------------------------------------------

  IF (slh) THEN
    DO j=tdims%j_start,tdims%j_end
      DO i=tdims%i_start,tdims%i_end
        latent_heat(i,j) = lc*fqw_1(i,j)                           &
                          + lf*(flandg(i,j)*ei_land(i,j) +         &
                             (1.-flandg(i,j))*ei_sice_sm(i,j))
      END DO
    END DO
  END IF


!-----------------------------------------------------------------------
! Rescale FTL_1 as it should be used to update the botom row of the
! discrete equation handled by the new BL solver at the next (2nd)
! stage of the scheme.
!-----------------------------------------------------------------------
!fpp$ Select(CONCUR)
  DO j=tdims%j_start,tdims%j_end
    DO i=tdims%i_start,tdims%i_end
      ftl_1(i,j) = ftl_1(i,j)/cp
    END DO
  END DO

ELSE ! L_correct = true: 2nd stage of the scheme

!-----------------------------------------------------------------------
! Rescale to Watts/m^2 as this is the final call to the imp BL solver
! and FTL_1 will be used by stash diagnostics
!-----------------------------------------------------------------------
!fpp$ Select(CONCUR)
  DO j=tdims%j_start,tdims%j_end
    DO i=tdims%i_start,tdims%i_end
      ftl_1(i,j) = cp*ftl_1(i,j)
    END DO
  END DO
!-----------------------------------------------------------------------
!  U_V will be updated at 2nd stage of the scheme as the equations
!  providing the implicit surface stresses have been modified
!  consistently with the new scheme.
!-----------------------------------------------------------------------
! U component of 10m wind
  IF (su10) THEN
    DO j=udims%j_start,udims%j_end
      DO i=udims%i_start,udims%i_end
        u10m(i,j) = (u_1(i,j) + du_star1(i,j) + (du_1(i,j) -      &
                     cq_cm_u_1(i,j)*taux_1(i,j)) -                &
                     u_0(i,j))*cdr10m_u(i,j) + u_0(i,j)
      END DO
    END DO
  END IF

! V component of 10m wind
  IF (sv10) THEN
    DO j=vdims%j_start,vdims%j_end
      DO i=vdims%i_start,vdims%i_end
        v10m(i,j) = (v_1(i,j) + dv_star1(i,j) + (dv_1(i,j) -      &
                     cq_cm_v_1(i,j)*tauy_1(i,j)) -                &
                     v_0(i,j))*cdr10m_v(i,j) + v_0(i,j)
      END DO
    END DO
  END IF

! Correct surface stress diagnostics

  DO j=udims%j_start,udims%j_end
    DO i=udims%i_start,udims%i_end
      taux_land(i,j) = taux_land(i,j) + taux_land_star(i,j)
      taux_ssi(i,j)  = taux_ssi(i,j)  + taux_ssi_star(i,j)
    END DO
  END DO

  DO j=vdims%j_start,vdims%j_end
    DO i=vdims%i_start,vdims%i_end
      tauy_land(i,j) = tauy_land(i,j) + tauy_land_star(i,j)
      tauy_ssi(i,j)  = tauy_ssi(i,j)  + tauy_ssi_star(i,j)
    END DO
  END DO

END IF ! IF .NOT. L_correct

IF (lhook) CALL dr_hook('SF_IMPL2',zhook_out,zhook_handle)
RETURN
END SUBROUTINE sf_impl2_cable
