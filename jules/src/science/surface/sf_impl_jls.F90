! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  SUBROUTINE SF_IMPL-----------------------------------------------
!
!  Purpose: Calculate implicit correction to surface fluxes of heat,
!           moisture and momentum. Also calculates screen level
!           temperature and humidity as well as 10 m winds.
!
!  Documentation: UMDP 24.
!
!--------------------------------------------------------------------

!    Arguments :-
SUBROUTINE sf_impl (                                              &
! IN values defining field dimensions and subset to be processed :
 off_x,off_y,row_length,rows,n_rows,land_pts,                     &
! IN soil/vegetation/land surface data :
 land_index,land_mask,nice,                                       &
 ntiles,tile_index,tile_pts,sm_levels,                            &
 canhc_tile,canopy,flake,smc,                                     &
 tile_frac,wt_ext_tile,                                           &
 fland,flandg,                                                    &
! IN sea/sea-ice data :
 di,ice_fract,di_ncat,ice_fract_ncat,u_0,v_0,                     &
! IN everything not covered so far :
 pstar,lw_down,sw_tile,timestep,                                  &
 t_soil,qw_1,tl_1,u_1,v_1,rhokm_u_1,rhokm_v_1,GAMMA,              &
 alpha1,alpha1_sice,ashtf_prime,ashtf_prime_tile,                 &
 dtrdz_charney_grid_1,du_1,dv_1,                                  &
 fqw_tile,epot_tile,fqw_ice,ftl_ice,                              &
 fraca,resfs,resft,rhokh,rhokh_tile,rhokh_sice,                   &
 rhokpm,rhokpm_pot,rhokpm_sice,                                   &
 dtstar_tile,dtstar,z1,                                           &
 z0hssi,z0mssi,z0h_tile,z0m_tile,cdr10m_u,cdr10m_v,               &
 chr1p5m,chr1p5m_sice,ct_ctq_1,dqw_1,dtl_1,cq_cm_u_1,cq_cm_v_1,   &
 flandg_u,flandg_v,l_sice_heatflux,                               &
 emis_tile,emis_soil,                                             &
! IN additional variables used to calculate atmospheric cooling
!    at the screen level
 l_co2_interactive, co2_mmr, co2_3d,                              &
 rho1, f3_at_p, ustargbm,                                         &
! IN STASH flags :-
 simlt,smlt,slh,sq1p5,st1p5,su10,sv10,                            &
! INOUT data :
 ti,ti_gb,tstar,                                                  &
 tstar_land,tstar_sea,tstar_sice,tstar_ssi,                       &
 tstar_tile,snow_tile,                                            &
 le_tile,radnet_sice,radnet_tile,                                 &
 e_sea,fqw_1,ftl_1,ftl_tile,h_sea,olr,taux_1,tauy_1,              &
 taux_land,taux_ssi,tauy_land,tauy_ssi,                           &
 tscrndcl_ssi,tscrndcl_tile,tstbtrans,                            &
! OUT Diagnostic not requiring STASH flags :
 ecan,ei_tile,esoil_tile,                                         &
 sea_ice_htf,surf_ht_flux,surf_ht_flux_land,surf_ht_flux_sice,    &
 surf_htf_tile,                                                   &
! OUT diagnostic requiring STASH flags :
 sice_mlt_htf,snomlt_surf_htf,latent_heat,                        &
 q1p5m,q1p5m_tile,t1p5m,t1p5m_tile,u10m,v10m,                     &
! OUT data required elsewhere in UM system :
 ecan_tile,ei,esoil,ext,snowmelt,melt_tile,rhokh_mix,             &
 error,                                                           &
! LOGICAL LTIMER
 lq_mix_bl,                                                       &
 l_flux_bc,                                                       &
 ltimer                                                           &
 )

USE atm_fields_bounds_mod
USE theta_field_sizes, ONLY : t_i_length, t_j_length

USE c_mdi
USE csigma
USE c_lheat
USE c_kappai
USE c_r_cp
USE c_0_dg_c
USE surf_param, ONLY : ls, ip_scrndecpl2, emis_sea, emis_sice

USE fluxes, ONLY : anthrop_heat, surf_ht_store, sw_sice

USE ancil_info, ONLY: nsmax,ssi_pts,sea_pts,sice_pts,             &
    sice_pts_ncat,ssi_index,sea_index,sice_index,sice_index_ncat, &
    fssi,sea_frac,sice_frac,sice_frac_ncat
USE switches,    ONLY: iscrntdiag                                 &
                      ,can_model                                  &
                      ,l_snowdep_surf                             &
                      ,l_dolr_land_black,                         &
                      l_neg_tstar
USE prognostics, ONLY: nsnow                                      &
                      ,snowdepth
USE jules_mod,   ONLY: snowdep_surf
USE snow_param,  ONLY: rho_snow_const                             &
                      ,cansnowtile
USE solinc_data, ONLY: sky                                        &
                      ,l_skyview

#if defined(UM_JULES)
USE science_fixes_mod, ONLY: l_emis_ssi_full
#endif

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

!  Inputs :-
! (a) Defining horizontal grid and subset thereof to be processed.
!    Checked for consistency.

INTEGER                                                           &
  row_length                                                      &
             ! Local number of points on a row
, rows                                                            &
             ! Local number of rows in a theta field
, n_rows                                                          &
             ! Local number of rows in a v field
, off_x                                                           &
             ! Size of small halo in i
, off_y                                                           &
             ! Size of small halo in j.
,land_pts    ! IN No of land points

! (c) Soil/vegetation/land surface parameters (mostly constant).

LOGICAL                                                           &
 land_mask(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN T if land, F elsewhere.

INTEGER                                                           &
 land_index(land_pts)        ! IN LAND_INDEX(I)=J => the Jth
!                                  !    point in ROW_LENGTH,ROWS is the
!                                  !    Ith land point.

INTEGER                                                           &
 sm_levels                                                        &
                             ! IN No. of soil moisture levels
,ntiles                                                           &
                             ! IN No. of land tiles
,tile_index(land_pts,ntiles)                                      &
                             ! IN Index of tile points
,tile_pts(ntiles)                                                 &
                             ! IN Number of tile points
,nice                        ! IN Number of sea ice catagories

REAL                                                              &
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
,emis_soil(land_pts)         ! IN Emissivity of underlying soil

! (d) Sea/sea-ice data.

REAL                                                              &
 di(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)          &
                             ! IN "Equivalent thickness" of
!                                  !     sea-ice GBM agregate (m).
,di_ncat(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice)&
                               ! IN "Equivalent thickness" of
!                                  !     sea-ice catagories (m).
,ice_fract(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)   &
                             ! IN Fraction of gridbox covered by
!                                  !     sea-ice (decimal fraction).
,ice_fract_ncat(tdims%i_start:tdims%i_end,                        &
                tdims%j_start:tdims%j_end,nice)                   &
                                       ! IN Fraction of gridbox
!                                  !  covered by sea-ice on catagories.

,u_0(udims%i_start:udims%i_end,udims%j_start:udims%j_end)         &
                             ! IN W'ly component of surface
!                                  !    current (m/s).
,v_0(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
                             ! IN S'ly component of surface
!                                  !    current (m/s).

! (f) Atmospheric + any other data not covered so far, incl control.

REAL                                                              &
 pstar(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)       &
                             ! IN Surface pressure (Pascals).
,lw_down(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)     &
                             ! IN Surface downward LW radiation
!                                  !    (W/m2).
,sw_tile(land_pts,ntiles)                                         &
                             ! IN Surface net SW radiation on
!                                  !    land tiles (W/m2).
,timestep                                                         &
                             ! IN Timestep (seconds).
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
,rhokm_u_land(udims%i_start:udims%i_end,udims%j_start:udims%j_end)&
!                                  ! IN Exchange coefficients for
!                                  !    land momentum
!                                  !    (on U-grid, with 1st
!                                  !    and last rows undefined or, at
!                                  !    present, set to "missing data")
,rhokm_u_ssi(udims%i_start:udims%i_end,udims%j_start:udims%j_end) &
                             ! IN Exchange coefficients for
!                                  !    mean sea momentum
!                                  !    (on U-grid, with 1st
!                                  !    and last rows undefined or, at
!                                  !    present, set to "missing data")
,rhokm_v_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)   &
                             ! IN Exchange coefficients for
!                                  !    momentum (on V-grid, with 1st
!                                  !    and last rows undefined or, at
!                                  !    present, set to "missing data")
,rhokm_v_land(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)&
!                                  ! IN Exchange coefficients for
!                                  !    land momentum
!                                  !    (on V-grid, with 1st
!                                  !    and last rows undefined or, at
!                                  !    present, set to "missing data")
,rhokm_v_ssi(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
!                                  ! IN Exchange coefficients for
!                                  !    mean sea momentum
!                                  !    (on V-grid, with 1st
!                                  !    and last rows undefined or, at
!                                  !    present, set to "missing data")

REAL GAMMA                   ! IN implicit weight in level 1
REAL                                                              &
 alpha1(land_pts,ntiles)                                          &
                             ! IN Mean gradient of saturated
!                                  !    specific humidity with respect
!                                  !    to temperature between the
!                                  !    bottom model layer and tile
!                                  !    surfaces
,alpha1_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end) &
                             ! IN ALPHA1 for sea-ice.
,ashtf_prime(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end) &
                             ! IN Adjusted SEB coefficient for
                             !    sea-ice
,ashtf_prime_tile(land_pts,ntiles)                                &
                             ! IN Adjusted SEB coefficient for
                             !    land tiles.
,ashtf_tile(land_pts,ntiles)                                      &
                             !IN Coefficient to calculate
!                                  !    surface heat flux into land
!                                  !    tiles.
,dtrdz_charney_grid_1(tdims%i_start:tdims%i_end,                  &
                      tdims%j_start:tdims%j_end)                  &
!                                  ! IN -g.dt/dp for model layers.
,fqw_tile(land_pts,ntiles)                                        &
                             ! IN Surface FQW for land tiles
,epot_tile(land_pts,ntiles)                                       &
                             ! IN surface tile potential
!                                  !    evaporation
,fqw_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)     &
                             ! IN Surface FQW for sea-ice
,ftl_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)     &
                             ! IN Surface FTL for sea-ice
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
,rhokh_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)  &
                             ! IN Surface exchange coefficients
!                                  !    for sea and sea-ice
,rhokpm(land_pts,ntiles)                                          &
                             ! IN Land surface exchange coeff.
!                                  !    (not used for JULES)
,rhokpm_pot(land_pts,ntiles)                                      &
                             ! IN Land surface exchange coeff.
!                                  !    for potential evaporation.
!                                  !    (not used for JULES)
,rhokpm_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end) &
                             ! IN Sea-ice surface exchange coeff.
!                                  !    (not used for JULES)
,dtstar_tile(land_pts,ntiles)                                     &
                             ! IN Change in TSTAR over timestep
!                                  !     for land tiles
,dtstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Change is TSTAR over timestep
!                                  !     for sea-ice

 REAL                                                             &
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
,ct_ctq_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)    &
                             ! IN Coefficient in T and q
!                                  !    tri-diagonal implicit matrix
,cq_cm_u_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)   &
                             ! IN Coefficient in U tri-diagonal
!                                  !    implicit matrix
,cq_cm_v_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)   &
                             ! IN Coefficient in V tri-diagonal
!                                  !    implicit matrix
,dqw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! IN Level 1 increment to q field
,dtl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! IN Level 1 increment to T field
,du_1(udims_s%i_start:udims_s%i_end,udims_s%j_start:udims_s%j_end)&
                             ! IN Level 1 increment to u wind
!                                  !    field
,dv_1(vdims_s%i_start:vdims_s%i_end,vdims_s%j_start:vdims_s%j_end)
                             ! IN Level 1 increment to v wind
!                                  !    field

! IN Additional variables for screen-level diagnostics
LOGICAL, INTENT(IN) :: l_co2_interactive
                             ! Flag for interactive 3-D CO2
REAL, INTENT(IN)    :: co2_mmr
                             ! Initial or fixed mass mixing ratio
                             ! of CO2
REAL, INTENT(IN)    :: co2_3d                                     &
                      (tdims_s%i_start:tdims_s%i_end,tdims_s%j_start:tdims_s%j_end)
                             ! 3-D field of CO2
REAL, INTENT(IN)    :: rho1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! Density on lowest level
REAL, INTENT(IN)    :: f3_at_p(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! Coriolis parameter
REAL, INTENT(IN)    :: uStargbm(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! GBM surface friction velocity

LOGICAL                                                           &
 ltimer                                                           &
                             ! IN Logical switch for TIMER diags
,l_flux_bc                                                        &
                             ! IN SCM logical for prescribed
                             !    surface flux forcing
,l_sice_heatflux             ! IN T: semi-implicit update of TI

LOGICAL                                                           &
 lq_mix_bl              ! TRUE if mixing ratios used in
!                             ! boundary layer code
!  STASH flags :-

LOGICAL                                                           &
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

!  In/outs :-

REAL                                                              &
 ti(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice)     &
                             ! INOUT Sea-ice surface layer
!                                  !       temperature (K).
,ti_gb(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT GBM ice surface temperature (K)
                             !     (Not used for JULES)
,tstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT   GBM surface temperature (K).
,tstar_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)  &
                             ! OUT   Land mean sfc temperature (K)
,tstar_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)   &
                             ! IN    Open sea sfc temperature (K).
,tstar_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)  &
                             ! OUT   Sea-ice sfc temperature (K).
,tstar_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)   &
                             ! INOUT Sea mean sfc temperature (K).
,tstar_tile(land_pts,ntiles)                                      &
                             ! INOUT Surface tile temperatures
,snow_tile(land_pts,ntiles)                                       &
                             ! INOUT Lying snow on tiles (kg/m2)
,le_tile(land_pts,ntiles)                                         &
                             ! INOUT Surface latent heat flux for
!                                  !     land tiles
,radnet_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end) &
                             ! INOUT Surface net radiation on
!                                  !       sea-ice (W/m2)
,radnet_tile(land_pts,ntiles)                                     &
                             ! INOUT Surface net radiation on
!                                  !       land tiles (W/m2)
,e_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! INOUT Evaporation from sea times
!                                  !       leads fraction. Zero over
!                                  !       land. (kg per square metre
!                                  !       per sec).
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
,h_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! INOUT Surface sensible heat flux
!                                  !       over sea times leads fraction
!                                  !       (W/m2)
,olr(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)         &
                             ! IN    TOA - surface upward LW on
!                                  !       last radiation timestep
!                                  ! OUT   Corrected TOA outward LW
,taux_1(udims%i_start:udims%i_end,udims%j_start:udims%j_end)      &
                             ! OUT   W'ly component of surface
!                                  !       wind stress (N/sq m). (On
!                                  !       UV-grid with first and last
!                                  !       rows undefined or, at
!                                  !       present, set to missing data
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
,tauy_1(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)      &
                             ! OUT   S'ly component of surface
!                                  !       wind stress (N/sq m).  On
!                                  !       UV-grid; comments as per TAUX
,tauy_land(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)   &
                             ! INOUT S'ly component of land sfc
!                                  !       wind stress (N/sq m).  On
!                                  !       UV-grid; comments as per TAUX
,tauy_ssi(vdims%i_start:vdims%i_end,vdims%j_start:vdims%j_end)
!                                  !       wind stress (N/sq m).  On
!                                  !       UV-grid; comments as per TAUX

REAL, INTENT(INOUT) :: tscrndcl_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                                   !    Decoupled screen-level temperature
                                   !    over sea or sea-ice
REAL, INTENT(INOUT) :: tscrndcl_tile(land_pts,ntiles)
                                   !    Decoupled screen-level temperature
                                   !    over land tiles
REAL, INTENT(INOUT) :: tstbtrans(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                                   !    Time since the transition


!  Outputs :-
!-1 Diagnostic (or effectively so - includes coupled model requisites):-

!  (a) Calculated anyway (use STASH space from higher level) :-
!
REAL                                                              &
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
                   tdims%j_start:tdims%j_end)                     &
!                                  ! OUT Net downward heat flux at
!                                  !     surface over sea-ice
!                                  !     fraction of gridbox (W/m2).
,surf_htf_tile(land_pts,ntiles)
!                                  ! OUT Net downward surface heat flux
!                                  !     on tiles (W/m2)

!  (b) Not passed between lower-level routines (not in workspace at this
!      level) :-

REAL                                                              &
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

REAL                                                              &
 ei(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)          &
                             ! OUT Sublimation from lying snow or
!                                  !     sea-ice (kg/m2/s).
,ei_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)     &
                             ! OUT Sublimation from lying snow
!                                  !     (kg/m2/s).
,ei_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)     &
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

INTEGER                                                           &
 error          ! OUT 0 - AOK;
!                     !     1 to 7  - bad grid definition detected;

!  Workspace :-

REAL                                                              &
 elake_tile(land_pts,ntiles)                                      &
                             ! Lake evaporation.
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
,tsurf(land_pts)                                                  &
                             ! Soil or snow surface layer temp
,sice_melt(tdims%i_start:tdims%i_end,                             &
           tdims%j_start:tdims%j_end,nice)                        &
                             !Melt at surface sea-ice catagory
,tstar_sic(tdims%i_start:tdims%i_end,                             &
           tdims%j_start:tdims%j_end,nice)                        &
                             !Ice catagory surface temperature
,surf_ht_flux_sice_ncat(tdims%i_start:tdims%i_end,                &
                        tdims%j_start:tdims%j_end,nice)           &
                             ! Heat flux by ice catagory
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
               tdims%j_start:tdims%j_end)
                             ! Increment for ei_sice from sea-ice
                             ! melt calculated for each category,
                             ! but un-weighted by category fractions

REAL, ALLOCATABLE :: tstar_ssi_old(:,:)
                             ! Sea and ice surface temperatures at
                             ! beginning of timestep.


! dummy arrays required for sea and se-ice to create universal
! routines for all surfaces
REAL                                                              &
 array_one(t_i_length*t_j_length)                                 &
                             ! Array of ones
,array_one_e_six(t_i_length*t_j_length)
                             ! Array of 1.0E6



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

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook('SF_IMPL',zhook_in,zhook_handle)

error = 0

array_one(:)=1.0
array_one_e_six(:)=1.0e6

! DEPENDS ON: im_sf_pt
CALL im_sf_pt (                                                   &
 land_pts,land_index,ntiles,tile_index,tile_pts                   &
,flandg,tile_frac,snow_tile,ice_fract                             &
,GAMMA,alpha1,alpha1_sice,ashtf_prime,ashtf_prime_tile            &
,resft,dtstar_tile,dtstar                                         &
,rhokm_u_1,rhokm_v_1,rhokh_tile,rhokh_sice                        &
,ct_ctq_1,dqw_1,dtl_1,cq_cm_u_1,cq_cm_v_1,du_1,dv_1               &
,flandg_u,flandg_v                                                &
,fqw_1,ftl_1                                                      &
,taux_1,taux_land,taux_ssi,tauy_1,tauy_land,tauy_ssi              &
,fqw_tile,epot_tile,ftl_tile,fqw_ice,ftl_ice,e_sea,h_sea          &
,l_flux_bc,ltimer                                                 &
)

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
  ftl_ice(i,j) = cp*ftl_ice(i,j)
 END DO
END DO

DO n=1,ntiles
  DO k=1,tile_pts(n)
    l = tile_index(k,n)
    ftl_tile(l,n) = cp*ftl_tile(l,n)
  END DO
END DO



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
      WRITE(6,*) '*** ERROR DETECTED BY ROUTINE SF_IMPL ***'
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
    tstar_tile(l,n) = tstar_tile_old(l,n) + dtstar_tile(l,n)
  END DO
END DO


!-----------------------------------------------------------------------
!! 7.  Surface evaporation components and updating of surface
!!     temperature (P245, routine SF_EVAP).
!-----------------------------------------------------------------------
! DEPENDS ON: sf_evap
CALL sf_evap (                                                    &
 land_pts,ntiles,                                                 &
 land_index,tile_index,tile_pts,sm_levels,ltimer,fland,           &
 ashtf_prime_tile,canopy,dtrdz_charney_grid_1,flake,fraca,        &
 snow_tile,resfs,resft,rhokh_tile,tile_frac,smc,wt_ext_tile,      &
 timestep,GAMMA,fqw_1,fqw_tile,ftl_1,ftl_tile,tstar_tile,         &
 ecan,ecan_tile,elake_tile,esoil,esoil_tile,ei_tile,ext           &
 )

!-----------------------------------------------------------------------
!!     Surface melting of sea-ice and snow on land tiles.
!-----------------------------------------------------------------------

ei_land(:,:)=0.0
snowmelt(:,:)=0.0

DO n=1,ntiles
! DEPENDS ON: sf_melt
  CALL sf_melt (                                                  &
   land_pts,land_index,                                           &
   tile_index(:,n),tile_pts(n),ltimer,flandg,                     &
   alpha1(:,n),ashtf_prime_tile(:,n),dtrdz_charney_grid_1,        &
   resft(:,n),rhokh_tile(:,n),tile_frac(:,n),timestep,GAMMA,      &
   ei_tile(:,n),fqw_1,ftl_1,fqw_tile(:,n),ftl_tile(:,n),          &
   tstar_tile(:,n),snow_tile(:,n),snowdep_surf(:,n),              &
   melt_tile(:,n)                                                 &
   )

!-----------------------------------------------------------------------
!  Increment gridbox snow sublimation and melt.
!-----------------------------------------------------------------------
  DO k=1,tile_pts(n)
    l = tile_index(k,n)
    j=(land_index(l)-1)/t_i_length + 1
    i = land_index(l) - (j-1)*t_i_length
    ei_land(i,j) = ei_land(i,j) + tile_frac(l,n)*ei_tile(l,n)
    snowmelt(i,j) = snowmelt(i,j) + tile_frac(l,n)*melt_tile(l,n)
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

DO l=1,land_pts
  j=(land_index(l)-1)/t_i_length + 1
  i = land_index(l) - (j-1)*t_i_length
  tstar_land(i,j) = 0.
END DO

IF (l_skyview) THEN
  DO n=1,ntiles
    DO k=1,tile_pts(n)
      l = tile_index(k,n)
      j=(land_index(l)-1)/t_i_length + 1
      i = land_index(l) - (j-1)*t_i_length
      radnet_tile(l,n) = sw_tile(l,n) +   emis_tile(l,n)*         &
        sky(i,j)*( lw_down(i,j) - sbcon*tstar_tile(l,n)**4 )
    END DO
  END DO
ELSE
  DO n=1,ntiles
    DO k=1,tile_pts(n)
      l = tile_index(k,n)
      j=(land_index(l)-1)/t_i_length + 1
      i = land_index(l) - (j-1)*t_i_length
      radnet_tile(l,n) = sw_tile(l,n) +   emis_tile(l,n)*         &
                 ( lw_down(i,j) - sbcon*tstar_tile(l,n)**4 )
    END DO
  END DO
END IF

DO n=1,ntiles
  DO k=1,tile_pts(n)
    l = tile_index(k,n)
    j=(land_index(l)-1)/t_i_length + 1
    i = land_index(l) - (j-1)*t_i_length
    le_tile(l,n) = lc*ecan_tile(l,n) + lc*esoil_tile(l,n) +       &
                   lc*elake_tile(l,n) + ls*ei_tile(l,n)
    surf_ht_store(l,n) = (canhc_tile(l,n)/timestep) *             &
                       (tstar_tile(l,n) - tstar_tile_old(l,n))
    surf_htf_tile(l,n) = radnet_tile(l,n) + anthrop_heat(l,n) -   &
                         ftl_tile(l,n) -                          &
                         le_tile(l,n) - lf*melt_tile(l,n) -       &
                         surf_ht_store(l,n)
    surf_ht_flux_land(i,j) = surf_ht_flux_land(i,j)               &
                      + tile_frac(l,n) * surf_htf_tile(l,n)
    tstar_land(i,j) = tstar_land(i,j)                             &
               + tile_frac(l,n)*tstar_tile(l,n)
  END DO
END DO

!-----------------------------------------------------------------------
! Optional error check : test for negative surface temperature
!-----------------------------------------------------------------------
IF (l_neg_tstar) THEN
  DO l=1,land_pts
    j=(land_index(l)-1)/t_i_length + 1
    i = land_index(l) - (j-1)*t_i_length
    IF (tstar_land(i,j) < 0) THEN
      error = 1
      WRITE(6,*) '*** ERROR DETECTED BY ROUTINE SF_IMPL ***'
      WRITE(6,*) 'NEGATIVE SURFACE TEMPERATURE AT LAND POINT ',l
    END IF
  END DO
END IF

!-----------------------------------------------------------------------
! Sea and sea-ice surface calculations
!-----------------------------------------------------------------------

tstar_sic(:,:,:)= 0.0
surf_ht_flux_sice(:,:)=0.0
sice_melt(:,:,:)=0.0
ei_sice(:,:)=fqw_ice(:,:)
sice_mlt_htf(:,:,:)=0.0
sea_ice_htf(:,:,:)=0.0

!-----------------------------------------------------------------------
! Store old surface temperature for sea and sea-ice if using the
! decoupled diagnostic.
!-----------------------------------------------------------------------
IF (iscrntdiag == ip_scrndecpl2) THEN
  ALLOCATE(tstar_ssi_old(t_i_length,t_j_length))
  tstar_ssi_old(:,:) = tstar_ssi(:,:)
ELSE
  ALLOCATE(tstar_ssi_old(1,1))
END IF

!-----------------------------------------------------------------------
! Diagnose the surface temperature for points with sea-ice
!-----------------------------------------------------------------------
DO j=tdims%j_start,tdims%j_end
 DO i=tdims%i_start,tdims%i_end
  IF ( flandg(i,j) < 1.0 .AND. ice_fract(i,j) > 0. ) THEN
    surf_ht_flux_sice(i,j) = radnet_sice(i,j) -                   &
              4.0*emis_sice*sbcon*(tstar_sice(i,j)**3.0)*         & 
                  dtstar(i,j) - ftl_ice(i,j) - ls*fqw_ice(i,j)
    DO n=1,nice
      tstar_sic(i,j,n) = ti(i,j,n) +                              &
                   surf_ht_flux_sice(i,j)/(2.0*kappai/de)
    END DO
  END IF
 END DO
END DO


DO n=1,nice

! Since sea-ice categories are not actually tiled for their surface
! fluxes at present, then the increment to ftl_ice, fqw_ice and
! ei_sice are not correctly weighted in sf_melt. Hence need to keep
! the increments and update ftl_ice, fqw_ice and ei_sice with
! weighted contributions below
  dftl_sice_ncat(:,:)=0.0
  dfqw_sice_ncat(:,:)=0.0
  dei_sice_ncat(:,:)=0.0

! DEPENDS ON: sf_melt
  CALL sf_melt (                                                  &
   ssi_pts,ssi_index,                                             &
   sice_index_ncat(:,n),sice_pts_ncat(n),ltimer,fssi,             &
   alpha1_sice,ashtf_prime,dtrdz_charney_grid_1,                  &
   array_one,rhokh_sice,sice_frac_ncat(:,n),timestep,GAMMA,       &
   dei_sice_ncat,fqw_1,ftl_1,dfqw_sice_ncat,dftl_sice_ncat,       &
   tstar_sic(:,:,n),array_one_e_six,                              &
   array_one_e_six/rho_snow_const,                                &
   sice_melt(:,:,n)                                               &
   )

  DO k=1,sice_pts_ncat(n)
    l = sice_index_ncat(k,n)
    j=(ssi_index(l)-1)/t_i_length + 1
    i = ssi_index(l) - (j-1)*t_i_length
    IF (simlt) THEN 
      sice_mlt_htf(i,j,n) = lf * sice_melt(i,j,n)
    END IF
! Add weighted increments to ftl_ice, fqw_ice and ei_sice
    ftl_ice(i,j)=ftl_ice(i,j)                                   &
       +( sice_frac_ncat(l,n)/sice_frac(l) )*dftl_sice_ncat(i,j)
    fqw_ice(i,j)=fqw_ice(i,j)                                   &
       +( sice_frac_ncat(l,n)/sice_frac(l) )*dfqw_sice_ncat(i,j)
    ei_sice(i,j)=ei_sice(i,j)                                   &
       +( sice_frac_ncat(l,n)/sice_frac(l) )*dei_sice_ncat(i,j)
  END DO

END DO


!-----------------------------------------------------------------------
! Gridbox-mean surface temperature and net surface heat fluxes
!-----------------------------------------------------------------------

! Accumulate SW fluxes over categories first as sw_sice is not
! indexed as a 2D array.
radnet_sice(:,:) = 0.0
DO n=1,nice
  DO k=1,sice_pts_ncat(n)
    l = sice_index_ncat(k,n)
    j=(ssi_index(l)-1)/t_i_length + 1
    i = ssi_index(l) - (j-1)*t_i_length
    radnet_sice(i,j) = radnet_sice(i,j) + sw_sice(l,n) *          &
      (ice_fract_ncat(i,j,n)/ice_fract(i,j))
  ENDDO
ENDDO

DO j=tdims%j_start,tdims%j_end
  DO i=tdims%i_start,tdims%i_end
    surf_ht_flux_sice(i,j) = 0.0
    DO n=1,nice
      surf_ht_flux_sice_ncat(i,j,n)=0.0
    END DO
    IF (flandg(i,j) < 1.0 .AND. ice_fract(i,j) > 0.0) THEN
      tstar_sice(i,j)= 0.0
      DO n=1,nice
        tstar_sice(i,j) = tstar_sice(i,j) +                       &
                          (ice_fract_ncat(i,j,n)/                 &
                           ice_fract(i,j)) * tstar_sic(i,j,n)
      END DO
      tstar_ssi(i,j)=(1.-ice_fract(i,j))*tstar_sea(i,j)           &
                    +ice_fract(i,j)*tstar_sice(i,j)
      radnet_sice(i,j) = radnet_sice(i,j) + emis_sice *           &
                ( lw_down(i,j) - sbcon*tstar_sice(i,j)**4 )
#if defined(UM_JULES)
      IF (.NOT.l_emis_ssi_full) THEN
!       Replicate initial implementation with omission of
!       emissivity at this point.
        radnet_sice(i,j) = radnet_sice(i,j) +                     &
          (1.0 - emis_sice) *  lw_down(i,j)
      ENDIF
#endif
      DO n=1,nice
        surf_ht_flux_sice_ncat(i,j,n) = radnet_sice(i,j) -        &
                             4.0*emis_sice*sbcon*                 & 
                             tstar_sice(i,j)**3 *                 & 
                             (tstar_sic(i,j,n)-tstar_sice(i,j)) - &
                             ftl_ice(i,j) - ls*fqw_ice(i,j) -     &
                               lf*sice_melt(i,j,n)
        surf_ht_flux_sice(i,j) = surf_ht_flux_sice(i,j) +         &
                         (ice_fract_ncat(i,j,n)/ice_fract(i,j))*  &
                               surf_ht_flux_sice_ncat(i,j,n)
      END DO

    END IF
  END DO
END DO


! Convert sea and sea-ice fluxes to be fraction of grid-box
! (as required by sea and sea-ice modellers)
DO j=tdims%j_start,tdims%j_end
  DO i=tdims%i_start,tdims%i_end
    h_sea(i,j)=(1.0-ice_fract(i,j))*h_sea(i,j)
    e_sea(i,j)=(1.0-ice_fract(i,j))*e_sea(i,j)
    ftl_ice(i,j)=ice_fract(i,j)*ftl_ice(i,j)
    fqw_ice(i,j)=ice_fract(i,j)*fqw_ice(i,j)
    ei_sice(i,j)=ice_fract(i,j)*ei_sice(i,j)
    radnet_sice(i,j)=ice_fract(i,j)*radnet_sice(i,j)
    surf_ht_flux_sice(i,j)=ice_fract(i,j)*surf_ht_flux_sice(i,j)
  END DO
END DO



!-----------------------------------------------------------------------
! GBM diagnostic calculations
!-----------------------------------------------------------------------

DO j=tdims%j_start,tdims%j_end
  DO i=tdims%i_start,tdims%i_end
    qim_1(i,j)=qw_1(i,j) + dqw_1(i,j)-ct_ctq_1(i,j)*fqw_1(i,j)
    tim_1(i,j)=tl_1(i,j) + dtl_1(i,j)-ct_ctq_1(i,j)*ftl_1(i,j)/cp
  END DO
END DO

DO j=tdims%j_start,tdims%j_end
  DO i=tdims%i_start,tdims%i_end
    tstar(i,j)=flandg(i,j)*tstar_land(i,j)                        &
      +(1.-flandg(i,j))*tstar_ssi(i,j)
    ei(i,j)=flandg(i,j)*ei_land(i,j)                              &
      +(1.-flandg(i,j))*ei_sice(i,j)
    surf_ht_flux(i,j)=flandg(i,j)*surf_ht_flux_land(i,j)          &
      +(1.-flandg(i,j))*surf_ht_flux_sice(i,j)
    rhokh_mix(i,j)=rhokh(i,j)
  END DO
END DO


! TOA outward LW radiation after boundary layer

tstar_rad4(:,:)=0.0

DO j=tdims%j_start,tdims%j_end
  DO i=tdims%i_start,tdims%i_end
!   The contribution from the sea is removed in UM imp_solver to keep it   
!   consistent with the UM radiation scheme (see olr comment there) 
    tstar_rad4(i,j) = tstar_rad4(i,j) + (1.0-flandg(i,j))*        &
                     ( (1.0-ice_fract(i,j))*emis_sea*             & 
                      tstar_sea(i,j)**4 +   &
                      ice_fract(i,j)*emis_sice*tstar_sice(i,j)**4 ) 
  END DO
END DO

DO n=1,ntiles
  DO k=1,tile_pts(n)
    l = tile_index(k,n)
    j=(land_index(l)-1)/t_i_length + 1
    i = land_index(l) - (j-1)*t_i_length
    IF (l_dolr_land_black) THEN
      tstar_rad4(i,j) = tstar_rad4(i,j) + flandg(i,j)*            &
                        tile_frac(l,n)*tstar_tile(l,n)**4
    ELSE
      tstar_rad4(i,j) = tstar_rad4(i,j) + flandg(i,j)*            &
                        tile_frac(l,n)*emis_tile(l,n)*            &
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
CALL screen_tq (                                                  &
 land_pts,ntiles,                                                 &
 land_index,tile_index,tile_pts,flandg,                           &
 sq1p5,st1p5,chr1p5m,chr1p5m_sice,pstar,qim_1,resft,              &
 tile_frac,tim_1,tstar_ssi,tstar_tile,                            &
 z0hssi,z0h_tile,z0mssi,z0m_tile,z1,                              &
 timestep,tstar_ssi_old,tstar_tile_old,                           &
 l_co2_interactive, co2_mmr, co2_3d,                              &
 f3_at_p, ustargbm, rho1,                                         &
 tscrndcl_ssi,tscrndcl_tile,tstbtrans,                            &
 q1p5m,q1p5m_tile,t1p5m,t1p5m_tile,                               &
 lq_mix_bl                                                        &
 )

! Release space allocated for the transitional diagnostic.
DEALLOCATE(tstar_ssi_old)

!----------------------------------------------------------------------
!! 8.1 Update U_V.
!----------------------------------------------------------------------

! U component of 10m wind
IF (su10)THEN
  DO j=udims%j_start,udims%j_end
    DO i=udims%i_start,udims%i_end
      u10m(i,j) = (u_1(i,j) + (du_1(i,j) -                        &
                   cq_cm_u_1(i,j)*taux_1(i,j)) -                  &
                      u_0(i,j))*cdr10m_u(i,j) + u_0(i,j)
    END DO
  END DO
END IF

! V component of 10m wind
IF (sv10)THEN
  DO j=vdims%j_start,vdims%j_end
    DO i=vdims%i_start,vdims%i_end
      v10m(i,j) = (v_1(i,j) + (dv_1(i,j) -                        &
                   cq_cm_v_1(i,j)*tauy_1(i,j)) -                  &
                      v_0(i,j))*cdr10m_v(i,j) + v_0(i,j)
    END DO
  END DO
END IF

!-----------------------------------------------------------------------
!! 9.  Calculate surface latent heat flux.
!-----------------------------------------------------------------------

IF (slh) THEN
  DO j=tdims%j_start,tdims%j_end
    DO i=tdims%i_start,tdims%i_end
      latent_heat(i,j) = lc*fqw_1(i,j)                            &
       + lf*(flandg(i,j)*ei_land(i,j)+(1.-flandg(i,j))*ei_sice(i,j))
    END DO
  END DO
END IF

IF (lhook) CALL dr_hook('SF_IMPL',zhook_out,zhook_handle)
RETURN
END SUBROUTINE sf_impl
