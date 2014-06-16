! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  SUBROUTINE SF_EXPL------------------------------------------------
!
!  Purpose: Calculate explicit surface fluxes of heat, moisture and
!           momentum. Also calculates surface exchange coefficients
!           required for implicit update of surface fluxes and surface
!           information required by the explicit boundary layer
!           routine
!
!
!  Documentation: UMDP 24.
!
!---------------------------------------------------------------------
!    Arguments :-
SUBROUTINE sf_expl_l (                                            &
! IN values defining field dimensions and subset to be processed :
 land_pts, nice_use,                                              &
! IN  parameters for iterative SISL scheme
 numcycles, cycleno,                                              &
! IN parameters required from boundary-layer scheme :
 bq_1,bt_1,z1_uv,z1_uv_top,z1_tq,z1_tq_top,qw_1,tl_1,             &
! IN soil/vegetation/land surface data :
 land_index,ntiles,sm_levels,canopy,catch,catch_snow,hcon,        &
 ho2r2_orog, fland,flandg,                                        &
 snow_tile,sil_orog_land,smvccl,smvcst,smvcwt,sthf,sthu,z0_tile,  &
 z0h_tile_bare,                                                   &
! IN sea/sea-ice data :
 ice_fract_cat,k_sice,                                            &
! IN everything not covered so far :
 pstar,lw_down,sw_tile,zh,ddmfx,                                  &
 co2_mmr,co2_3d,l_co2_interactive,l_phenol,l_triffid,             &
 asteps_since_triffid,cs,frac,canht_ft,photosynth_act_rad,lai_ft, &
 lq_mix_bl,t_soil,ti,tstar,tstar_sea,tstar_sice_cat,              &
 tstar_tile,z_land,albsoil,cos_zenith_angle,                      &
 l_aero_classic,l_dust,l_dust_diag,soil_clay,o3,                  &
! IN idealised and SCM things
 l_spec_z0, z0m_scm, z0h_scm,                                     &
! IN variables for message passing
 u_1_px, v_1_px, u_0_px, v_0_px,                                  &
! IN STASH flags :-
 sfme,sq1p5,st1p5,su10,sv10,sz0heff,                              &
! INOUT data :
 l_q10,z0msea,gs,g_leaf_acc,npp_ft_acc,resp_w_ft_acc,resp_s_acc,  &
! OUT Diagnostic not requiring STASH flags :
 recip_l_mo_sea,e_sea,fqw_1,ftl_1,ftl_tile,le_tile,h_sea,         &
 radnet_sice,radnet_tile,rhokm_1,rib,rib_tile,                    &
! OUT variables for message passing
 flandfac, fseafac, rhokm_land, rhokm_ssi, cdr10m,                &
! OUT diagnostic requiring STASH flags :
 fme,                                                             &
! OUT diagnostics required for soil moisture nudging scheme :
 wt_ext,ra,                                                       &
! OUT data required for tracer mixing :
 rho_aresist,aresist,resist_b,                                    &
 rho_aresist_tile,aresist_tile,resist_b_tile,                     &
! OUT data required for mineral dust scheme
 r_b_dust,cd_std_dust,u_s_std_tile,                               &
! OUT data required elsewhere in UM system :
 fb_surf,u_s,t1_sd,q1_sd,                                         &
! OUT data required elsewhere in boundary layer or surface code
 alpha1,alpha1_sice,ashtf_prime,ashtf_prime_tile,fqw_tile,        &
 epot_tile,fqw_ice,ftl_ice,fraca,rhostar,resfs,resft,             &
 rhokh,rhokh_tile,rhokh_sice,rhokh_mix,dtstar_tile,dtstar,        &
 h_blend_orog,z0hssi,z0h_tile,z0h_eff,z0m_gb,z0mssi,z0m_tile,     &
 z0m_eff,chr1p5m,chr1p5m_sice,smc,hcons,vshr,vshr_land,vshr_ssi,  &
 gpp,npp,resp_p,g_leaf,gpp_ft,npp_ft,                             &
 resp_p_ft,resp_s,resp_s_tot,resp_w_ft,                           &
 gc,canhc_tile,wt_ext_tile,flake,                                 &
 tile_index,tile_pts,tile_frac,fsmc,emis_tile,emis_soil           &
 )

USE theta_field_sizes, ONLY : t_i_length

USE dust_param, ONLY: ndiv
USE c_0_dg_c
USE c_r_cp
USE c_g
USE csigma
USE c_mdi

USE soil_param, ONLY : dzsoil

USE snow_param, ONLY : ds

USE surf_param, ONLY : emis_sice 

USE fluxes, ONLY : anthrop_heat,                                  &
                   sw_sice

USE nstypes, ONLY :                                               &
!      imported scalars with intent(in)
   npft,ntype

USE veg_param, ONLY : secs_per_360days

USE switches, ONLY: l_aggregate, can_model, can_rad_mod, &
                    buddy_sea, l_ctile

#if defined(UM_JULES)
USE land_surf_mod, ONLY: ilayers
USE atm_step_local, ONLY: land_pts_trif, npft_trif,dim_cs1, dim_cs2,    &
     co2_dim_len,co2_dim_row
USE timestep_mod, ONLY: timestep
USE science_fixes_mod, ONLY: l_emis_ssi_full
#else
USE switches, ONLY: ilayers
USE ancil_info, ONLY: land_pts_trif, npft_trif,dim_cs1, dim_cs2,        &
     co2_dim_len,co2_dim_row
USE model_time_mod, ONLY: timestep_len
#endif
                    
USE ancil_info, ONLY: nsmax, sice_pts_ncat, sice_index_ncat,            &
     ssi_index

USE prognostics, ONLY: nsnow,sice,sliq,snowdepth,tsnow
USE c_elevate, ONLY: surf_hgt

USE bl_option_mod, ONLY: fd_stab_dep, formdrag, on,      &
                  orog_drag_param, charnock,seasalinityfactor,    &
                  l_quick_ap2

USE solinc_data, ONLY: sky, l_skyview

USE atm_fields_bounds_mod, ONLY:                                  &
   pdims_s, pdims, tdims

USE ozone_vars, ONLY : flux_o3_ft, fo3_ft

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE
!-----------------------------------------------------------------------
!  Inputs :-
!-----------------------------------------------------------------------
! (a) Defining horizontal grid and subset thereof to be processed.
!    Checked for consistency.
INTEGER, INTENT(IN) ::                                            &
 land_pts,                                                        &
            ! IN No of land points being processed.
 numcycles,                                                       &
            ! Number of cycles (iterations) for iterative SISL.
 cycleno,                                                         &
            ! Iteration no
 nice_use   ! No. of sea ice categories used fully in surface calculations

! Defining vertical grid of model atmosphere.
REAL, INTENT(IN) ::                                               &
 bq_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)        &
                             ! IN A buoyancy parameter
!                                  !    (beta q tilde).
,bt_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)        &
                             ! IN A buoyancy parameter
!                                  !    (beta T tilde).
,z1_uv(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! IN Height of lowest uv level (m).
,z1_tq(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! IN Height of lowest tq level (m).
!                                  !    Note, if the grid used is
!                                  !    staggered in the vertical,
!                                  !    Z1_UV and Z1_TQ can be
!                                  !    different.
,qw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)        &
                             ! IN Total water content
,tl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Ice/liquid water temperature

 REAL, INTENT(IN) ::                                                    &
  u_1_px(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),  &
  v_1_px(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),  &
  u_0_px(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),  &
  v_0_px(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end)

REAL, INTENT(IN) :: z1_uv_top(tdims%i_start:tdims%i_end,          &
                              tdims%j_start:tdims%j_end)
                             ! Height of top of lowest uv-layer
REAL, INTENT(IN) :: z1_tq_top(tdims%i_start:tdims%i_end,          &
                              tdims%j_start:tdims%j_end)
                             ! Height of top of lowest Tq-layer

! (c) Soil/vegetation/land surface parameters (mostly constant).
LOGICAL, INTENT(IN) ::                                            &
 l_co2_interactive                                                &
                             ! IN Switch for 3D CO2 field
,l_phenol                                                         &
                             ! IN Indicates whether phenology
!                                  !    in use
,l_triffid                                                        &
                             ! IN Indicates whether TRIFFID
!                                  !    in use.
,l_spec_z0
                             ! IN T if using prescribed
!                                  !    sea surface roughness lengths

INTEGER, INTENT(IN) ::                                            &
 land_index(land_pts)        ! IN LAND_INDEX(I)=J => the Jth
                                  !    point in ROW_LENGTH,ROWS is the
                                  !    land point.

INTEGER, INTENT(IN) ::                                            &
 sm_levels                                                        &
                             ! IN No. of soil moisture levels
,ntiles                                                           &
                             ! IN No. of land-surface tiles
,asteps_since_triffid                                              
                             ! IN Number of atmospheric
                                  !    timesteps since last call
                                  !    to TRIFFID.

REAL, INTENT(IN) ::                                               &
 canopy(land_pts,ntiles)                                          &
                             ! IN Surface/canopy water for
!                                  !    snow-free land tiles (kg/m2)
,catch(land_pts,ntiles)                                           &
                             ! IN Surface/canopy water capacity
!                                  !    of snow-free land tiles (kg/m2).
,catch_snow(land_pts,ntiles)                                      &
                             ! IN Snow interception capacity of
!                                  !    tiles (kg/m2).
,hcon(land_pts)                                                   &
                             ! IN Soil thermal conductivity
!                                  !    (W/m/K).
,snow_tile(land_pts,ntiles)                                       &
                             ! IN Lying snow on tiles (kg/m2)
,smvccl(land_pts,sm_levels)                                       &
                             ! IN Critical volumetric SMC
!                                  !    (cubic m per cubic m of soil).
,smvcst(land_pts,sm_levels)                                       &
                             ! IN Volumetric saturation point
!                                  !    (m3/m3 of soil).
,smvcwt(land_pts,sm_levels)                                       &
                             ! IN Volumetric wilting point
!                                  !    (cubic m per cubic m of soil).
,sthf(land_pts,sm_levels)                                         &
                             ! IN Frozen soil moisture content of
!                                  !    each layer as a fraction of
!                                  !    saturation.
,sthu(land_pts,sm_levels)                                         &
                             ! IN Unfrozen soil moisture content
!                                  !    of each layer as a fraction of
!                                  !    saturation.
,z0_tile(land_pts,ntiles)                                         &
                             ! IN Tile roughness lengths (m).
,z0h_tile_bare(land_pts,ntiles)                                   &
                             ! IN Tile thermal roughness lengths 
                             ! without snow cover(m).
,sil_orog_land(land_pts)                                          &
                             ! IN Silhouette area of unresolved
!                                  !    orography per unit horizontal
!                                  !    area on land points only.
,ho2r2_orog(land_pts)                                             &
                             ! IN Standard Deviation of orography.
!                                  !    equivilent to peak to trough
!                                  !    height of unresolved orography
,fland(land_pts)                                                  &
                             ! IN Land fraction on land tiles.
,flandg(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end)
!                                  ! IN Land fraction on all tiles.
!                                  !    divided by 2SQRT(2) on land
!                                  !    points only (m)
! (d) Sea/sea-ice data.
REAL, INTENT(IN) ::                                               &
 ice_fract_cat(tdims%i_start:tdims%i_end,                         &
               tdims%j_start:tdims%j_end,nice_use)                &
                       ! IN Fraction of gridbox covered by
!                      ! category sea-ice (decimal fraction).
                       ! If nice_use=1, this is the sum of the categories
,k_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice_use)
                       ! IN 2 * Sea ice thermal conductivity divided
                       !  by surface layer thickness  (in coupled mode,
                       !  this is passed in from the sea ice model) (W/m2/K)

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
,zh(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)          &
                             ! IN Height above surface of top of
!                            !    boundary layer (metres).
,ddmfx(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
!                            ! IN Convective downdraught
!                            !    mass-flux at cloud base
,co2_mmr                                                          &
                             ! IN CO2 Mass Mixing Ratio
,co2_3d(co2_dim_len,co2_dim_row)                                  &
!                                  ! IN 3D CO2 field if required.
,cs(land_pts,dim_cs1)                                             &
                        ! IN Soil carbon (kg C/m2).
,frac(land_pts,ntype)                                             &
                             ! IN Fractions of surface types.
,canht_ft(land_pts,npft)                                          &
                             ! IN Canopy height (m)
,photosynth_act_rad(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)    &
!                                  ! IN Net downward shortwave radiation
!                                  !    in band 1 (w/m2).
,lai_ft(land_pts,npft)                                            &
                             ! IN Leaf area index
,t_soil(land_pts,sm_levels)                                       &
                             ! IN Soil temperatures (K).
,ti(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)          &
                             ! IN Sea-ice surface layer
                             !    temperature (K).
,tstar_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)   &
                             ! IN Open sea surface temperature (K)
,tstar_sice_cat(tdims%i_start:tdims%i_end,                        &
                tdims%j_start:tdims%j_end,nice_use)               &
                             ! IN Sea-ice surface temperature (K).
,tstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! IN GBM surface temperature (K).
,tstar_tile(land_pts,ntiles)                                      &
                             ! IN Surface tile temperatures
,z_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)      &
                             ! IN Land height (m).
,albsoil(land_pts)                                                &
!                                  ! Soil albedo.
, cos_zenith_angle(tdims%i_start:tdims%i_end,                     &
                   tdims%j_start:tdims%j_end)                     &
!                                  ! Cosine of the zenith angle
,soil_clay (tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)  &
                             ! IN Soil clay fraction
,o3(land_pts)                                                     &
                            ! IN Surface ozone concentration (ppb).
,z0m_scm(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)     &
                             ! IN Fixed Sea-surface roughness
!                                  !    length for momentum (m).(SCM)
,z0h_scm(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Fixed Sea-surface roughness
!                                  !    length for heat (m). (SCM)

LOGICAL, INTENT(IN) ::                                            &
 l_aero_classic                                                   &
                             ! IN switch for using CLASSIC aerosol 
                             !    scheme
,l_dust                                                           &
                             ! IN switch for mineral dust
,l_dust_diag                                                      &
                             ! IN Switch for diagnostic mineral dust
                             !    lifting
,lq_mix_bl                                                        &
,sfme                                                             &
         ! IN Flag for FME (q.v.).
,sz0heff                                                          &
         ! IN Flag for Z0H_EFF
,sq1p5                                                            &
         ! IN Flag for Q1P5M (q.v.)
,st1p5                                                            &
         ! IN Flag for T1P5M (q.v.)
,su10                                                             &
         ! IN Flag for U10M (q.v.)
,sv10    ! IN Flag for V10M (q.v.)
!-----------------------------------------------------------------------
!  In/outs :-
!-----------------------------------------------------------------------
LOGICAL, INTENT(INOUT) :: l_q10  ! INOUT True if using Q10 for soil resp

REAL, INTENT(INOUT) ::                                            &
 z0msea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)      &
                             ! INOUT Sea-surface roughness
!                                  !       length for momentum (m).
,gs(land_pts)                                                     &
                             ! INOUT "Stomatal" conductance to
!                                  !        evaporation (m/s).
,g_leaf_acc(land_pts,npft)                                        &
                             ! INOUT Accumulated G_LEAF
,npp_ft_acc(land_pts_trif,npft_trif)                              &
!                                  ! INOUT Accumulated NPP_FT
,resp_w_ft_acc(land_pts_trif,npft_trif)                           &
!                                  ! INOUT Accum RESP_W_FT
,resp_s_acc(land_pts_trif,dim_cs1) ! INOUT Accumulated RESP_S
!-----------------------------------------------------------------------
!  Outputs :-
!-----------------------------------------------------------------------
!-1 Diagnostic (or effectively so - includes coupled model requisites):-
INTEGER, INTENT(OUT) ::                                           &
 tile_index(land_pts,ntype)                                       &
                             ! OUT Index of tile points
,tile_pts(ntype)             ! OUT Number of tile points

!  (a) Calculated anyway (use STASH space from higher level) :-

REAL, INTENT(OUT) ::                                              &
 recip_l_mo_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)        &
!                                  ! OUT Reciprocal of the surface
!                                  !     Obukhov  length at sea
!                                  !     points. (m-1).
,e_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT Evaporation from sea times
!                                  !     leads fraction. Zero over land.
!                                  !     (kg per square metre per sec).
,fqw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT Moisture flux between layers
!                                  !     (kg per square metre per sec).
!                                  !     FQW(,1) is total water flux
!                                  !     from surface, 'E'.
,ftl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT FTL(,K) contains net turbulent
!                                  !     sensible heat flux into layer K
!                                  !     from below; so FTL(,1) is the
!                                  !     surface sensible heat, H.(W/m2)
,ftl_tile(land_pts,ntiles)                                        &
                             ! OUT Surface FTL for land tiles
,le_tile(land_pts,ntiles)                                         &
                             ! OUT Surface latent heat flux for
!                                  !     land tiles
,h_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT Surface sensible heat flux over
!                                  !     sea times leads fraction (W/m2)
,radnet_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end, &
             nice_use)                                            &
                             ! OUT Surface net radiation on
!                                  !     sea-ice (W/m2)
,radnet_tile(land_pts,ntiles)                                     &
                             ! OUT Surface net radiation on
,rhokm_1(pdims_s%i_start:pdims_s%i_end,                           &
         pdims_s%j_start:pdims_s%j_end)                           &
!                                  ! OUT Exchange coefficients for
!                                  !     momentum on P-grid
,rib(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)         &
                             ! OUT Mean bulk Richardson number for
!                                  !     lowest layer.
,rib_tile(land_pts,ntiles)                                        &
                                   ! OUT RIB for land tiles.
,rho_aresist(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end) &
                                   ! OUT RHOSTAR*CD_STD*VSHR
!                                  ! for CLASSIC aerosol scheme
,aresist(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)     &
                                   ! OUT 1/(CD_STD*VSHR)
!                                  ! for CLASSIC aerosol scheme
,resist_b(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)    &
                                   ! OUT (1/CH-1/(CD_STD)/VSHR
!                                  ! for CLASSIC aerosol scheme
,rho_aresist_tile(land_pts,ntiles)                                &
!                                  ! OUT RHOSTAR*CD_STD*VSHR on land
!                                  ! tiles for CLASSIC aerosol scheme
,aresist_tile(land_pts,ntiles)                                    &
!                                  ! OUT 1/(CD_STD*VSHR) on land tiles
!                                  ! for CLASSIC aerosol scheme
,resist_b_tile(land_pts,ntiles)                                   &
!                                  ! OUT (1/CH-1/CD_STD)/VSHR on land
!                                  ! tiles for CLASSIC aerosol scheme
, r_b_dust(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,ndiv)&
                                   ! OUT surf layer res for dust
, cd_std_dust(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)&
                                   ! OUT Bulk transfer coef. for
!                                  ! momentum, excluding orographic 
!                                  ! effects for mineral dust
, u_s_std_tile(land_pts,ntiles)                                   &
                                   ! OUT Surface friction velocity
!                                  ! (standard value) 
!                                  ! for mineral dust
,emis_tile(land_pts,ntiles)                                       &
                             ! OUT Emissivity for land tiles
,emis_soil(land_pts)                                              &
                             ! OUT Emissivity of underlying soil
,wt_ext(land_pts,sm_levels)                                       &
                             ! OUT cumulative fraction of transp'n
,ra(land_pts)                ! OUT Aerodynamic resistance (s/m).

 REAL, INTENT(OUT) ::                                                   &
  flandfac(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),&
  fseafac(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end), &
  rhokm_land(pdims_s%i_start:pdims_s%i_end,                             &
             pdims_s%j_start:pdims_s%j_end),                            &
  rhokm_ssi(pdims_s%i_start:pdims_s%i_end,                              &
            pdims_s%j_start:pdims_s%j_end),                             &
  cdr10m(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end)

!  (b) Not passed between lower-level routines (not in workspace at this
!      level) :-

REAL, INTENT(OUT) ::                                                    &
 fme(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT Wind mixing "power" (W/m2).

!-2 Genuinely output, needed by other atmospheric routines :-
REAL, INTENT(OUT) ::                                              &
 fb_surf(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)     &
                             ! OUT Surface flux buoyancy over
!                                  !     density (m^2/s^3)
,u_s(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)         &
                             ! OUT Surface friction velocity (m/s)
,t1_sd(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT Standard deviation of turbulent
!                                  !     fluctuations of layer 1 temp;
!                                  !     used in initiating convection.
,q1_sd(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT Standard deviation of turbulent
!                                  !     flucs of layer 1 humidity;
!                                  !     used in initiating convection.

REAL, INTENT(OUT) ::                                              &
 alpha1(land_pts,ntiles)                                          &
                             ! OUT Mean gradient of saturated
!                                  !     specific humidity with respect
!                                  !     to temperature between the
!                                  !     bottom model layer and tile
!                                  !     surfaces
,alpha1_sice(tdims%i_start:tdims%i_end,                           &
             tdims%j_start:tdims%j_end,nice_use)                  &
                             ! OUT ALPHA1 for sea-ice.
,ashtf_prime(tdims%i_start:tdims%i_end,                           &
             tdims%j_start:tdims%j_end,nice_use)                  &
                             ! OUT Coefficient to calculate
!                                  !     surface heat flux into sea-ice.
,ashtf_prime_tile(land_pts,ntiles)                                &
                             ! OUT Coefficient to calculate
!                                  !     surface heat flux into land
!                                  !     tiles.
,fqw_tile(land_pts,ntiles)                                        &
                             ! OUT Surface FQW for land tiles
,epot_tile(land_pts,ntiles)                                       &
                             ! OUT Local EPOT for land tiles.
,fqw_ice(tdims%i_start:tdims%i_end,                               &
         tdims%j_start:tdims%j_end,nice_use)                      &
                             ! OUT Surface FQW for sea-ice
,ftl_ice(tdims%i_start:tdims%i_end,                               &
         tdims%j_start:tdims%j_end,nice_use)                      &
                             ! OUT Surface FTL for sea-ice
,fraca(land_pts,ntiles)                                           &
                             ! OUT Fraction of surface moisture
!                                  !     flux with only aerodynamic
!                                  !     resistance for snow-free land
!                                  !     tiles.
,rhostar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)     &
                             ! OUT Surface air density
,resfs(land_pts,ntiles)                                           &
                             ! OUT Combined soil, stomatal
!                                  !     and aerodynamic resistance
!                                  !     factor for fraction (1-FRACA)
!                                  !     of snow-free land tiles.
,resft(land_pts,ntiles)                                           &
                             ! OUT Total resistance factor.
!                                  !     FRACA+(1-FRACA)*RESFS for
!                                  !     snow-free land, 1 for snow.
,rhokh(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                             ! OUT Grid-box surface exchange
!                                  !     coefficients
,rhokh_tile(land_pts,ntiles)                                      &
                             ! OUT Surface exchange coefficients
!                                  !     for land tiles
,rhokh_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)  &
                             ! OUT Surface exchange coefficients
!                                  !     for sea and sea-ice
,rhokh_mix(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)   &
                             ! OUT Exchange coeffs for moisture.
,dtstar_tile(land_pts,ntiles)                                     &
                             ! OUT Change in TSTAR over timestep
!                                  !     for land tiles
,dtstar(tdims%i_start:tdims%i_end,                                &
        tdims%j_start:tdims%j_end,nice_use)                       &
                             ! OUT Change is TSTAR over timestep
!                                  !     for sea-ice
,h_blend_orog(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)&
!                                  ! OUT Blending height used as part of
!                                  !     effective roughness scheme
,z0hssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)      &
                             ! OUT Roughness length for heat and
!                                  !     moisture over sea (m).
,z0mssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)      &
                             ! OUT Roughness length for momentum
!                                  !     over sea (m).
,z0h_tile(land_pts,ntiles)                                        &
                             ! OUT Tile roughness lengths for heat
!                                  !     and moisture (m).
,z0h_eff(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)     &
                             ! OUT Effective grid-box roughness
!                                  !     length for heat, moisture (m)
,z0m_gb(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)      &
                             ! OUT Gridbox mean roughness length
!                                  !     for momentum (m).
,z0m_tile(land_pts,ntiles)                                        &
                             ! OUT Tile roughness lengths for
!                                  !     momentum.
,z0m_eff(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)     &
                             ! OUT Effective grid-box roughness
!                                  !     length for momentum
,chr1p5m(land_pts,ntiles)                                         &
                             ! OUT Ratio of coefffs for
!                                  !     calculation of 1.5m temp for
!                                  !     land tiles.
,chr1p5m_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)&
!                                  ! OUT CHR1P5M for sea and sea-ice
!                                  !     (leads ignored).
,smc(land_pts)                                                    &
                             ! OUT Available moisture in the
!                                  !     soil profile (mm).
,hcons(land_pts)                                                  &
                             ! OUT Soil thermal conductivity
!                                  !     including water and ice
,vshr(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)        &
                             ! OUT Magnitude of surface-to-lowest
!                                  !     atm level wind shear (m per s).
,vshr_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)   &
                             ! OUT Magnitude of surface-to-lowest
!                                  !     atm level wind shear (m per s).
,vshr_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)    &
                             ! OUT Magnitude of surface-to-lowest
!                                  !     atm level wind shear (m per s).
,gpp(land_pts)                                                    &
                             ! OUT Gross primary productivity
!                                  !     (kg C/m2/s).
,npp(land_pts)                                                    &
                             ! OUT Net primary productivity
!                                  !     (kg C/m2/s).
,resp_p(land_pts)                                                 &
                             ! OUT Plant respiration (kg C/m2/s).
,g_leaf(land_pts,npft)                                            &
                             ! OUT Leaf turnover rate (/360days).
,gpp_ft(land_pts,npft)                                            &
                             ! OUT Gross primary productivity
!                                  !     on PFTs (kg C/m2/s).
,npp_ft(land_pts,npft)                                            &
                             ! OUT Net primary productivity
!                                  !     (kg C/m2/s).
,resp_p_ft(land_pts,npft)                                         &
                             ! OUT Plant respiration on PFTs
!                                  !     (kg C/m2/s).
,resp_s(land_pts,dim_cs1)                                         &
                          ! OUT Soil respiration (kg C/m2/s).
,resp_s_tot(dim_cs2)                                              &
                            ! OUT Total soil respiration
                            ! (kg C/m2/s).
,resp_w_ft(land_pts,npft)                                         &
                             ! OUT Wood maintenance respiration
!                                  !     (kg C/m2/s).
,gc(land_pts,ntiles)                                              &
                             ! OUT "Stomatal" conductance to
!                                  !      evaporation for land tiles
!                                  !      (m/s).
,canhc_tile(land_pts,ntiles)                                      &
                             ! IN Areal heat capacity of canopy
!                                  !    for land tiles (J/K/m2).
,wt_ext_tile(land_pts,sm_levels,ntiles)                           &
!                                  ! IN Fraction of evapotranspiration
!                                  !    which is extracted from each
!                                  !    soil layer by each tile.
,flake(land_pts,ntiles)                                           &
                             ! IN Lake fraction.
,tile_frac(land_pts,ntiles)                                       &
                             ! OUT Tile fractions including
!                                  !     snow cover in the ice tile.
,fsmc(land_pts,npft)         ! OUT Moisture availability factor.
!-----------------------------------------------------------------------
! LOCAL variables
!-----------------------------------------------------------------------
!  Workspace :-
REAL :: work_clay         ! working variable

REAL                                                              &
 vfrac_tile(land_pts,ntiles)                                      &
                          ! Fractional canopy coverage for
                          ! land tiles.
,csnow(land_pts,nsmax)                                            &
                          ! Areal heat capacity of snow (J/K/m2)
,ksnow(land_pts,nsmax)                                            &
                          ! Thermal conductivity of snow (W/m/K)
,hcons_snow(land_pts,ntiles)                                      &
                          ! Snow thermal conductivity
,resp_frac(dim_cs2)                                               &
                          ! respired fraction of RESP_S
,clay_land(dim_cs2)       ! Clay fraction on land points

!  Local scalars :-

INTEGER                                                           &
 i,j,k,l,n                                                        &
            ! LOCAL Loop counter (horizontal field index).
,is,js                                                            &
            ! Loop counter for coastal point stencil
,COUNT      ! Counter for average wind speed

REAL                                                              &
 ushear                                                           &
              ! U-component of surface-to-lowest-level wind shear.
,vshear                                                           &
              ! V-component of surface-to-lowest-level wind shear.
,vshr2        ! Square of magnitude of surface-to-lowest-level
!                   ! wind shear.

REAL seawind  ! average wind speed adjacent to coast
REAL fseamax  ! Maximum factor to apply to coast wind speed

     ! Minimum factor allowed to convert coastal wind speed to land part
REAL flandmin
PARAMETER(flandmin=0.2)

! temporary timestep variable needed because JULES and UM have different
! names and types for the variable
#if !defined(UM_JULES)
REAL :: timestep
#endif

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook('SF_EXPL_L',zhook_in,zhook_handle)

! temporary timestep variable needed because JULES and UM have different
! names and types for the variable
#if !defined(UM_JULES)
timestep = REAL(timestep_len)
#endif
!-----------------------------------------------------------------------
! Call TILEPTS to calculate TILE_PTS and TILE_INDEX for surface types
!-----------------------------------------------------------------------
! DEPENDS ON: tilepts
CALL tilepts(land_pts,frac,tile_pts,tile_index)

!-----------------------------------------------------------------------
! Calculate wind shear between level 1 and the surface
!-----------------------------------------------------------------------

DO j=tdims%j_start,tdims%j_end
 DO i=tdims%i_start,tdims%i_end
  IF(flandg(i,j) <  1.0)THEN
    ushear = u_1_px(i,j) - u_0_px(i,j)
    vshear = v_1_px(i,j) - v_0_px(i,j)
    vshr2 = MAX (1.0e-6 , ushear*ushear + vshear*vshear)
    vshr_ssi(i,j) = SQRT(vshr2)
  ELSE
    vshr_ssi(i,j) = 0.0
  END IF

  IF(flandg(i,j) >  0.0)THEN
  vshr2 = MAX (1.0e-6 , u_1_px(i,j)*u_1_px(i,j)                   &
    + v_1_px(i,j)*v_1_px(i,j))
  vshr_land(i,j) = SQRT(vshr2)
  ELSE
    vshr_land(i,j) = 0.0
  END IF

  vshr(i,j)= flandg(i,j)*vshr_land(i,j)                           &
    + (1.0 - flandg(i,j))*vshr_ssi(i,j)
  END DO
END DO

#if !defined(SCMA)

IF (l_ctile .AND. buddy_sea == on) THEN

  DO j=tdims%j_start,tdims%j_end
   DO i=tdims%i_start,tdims%i_end
    fseafac(i,j)  = 1.0
    flandfac(i,j) = 1.0

    IF ( flandg(i,j) > 0.01 .AND. flandg(i,j) < 0.99 ) THEN
!           !-----------------------------------------------------
!           ! Calculate average windspeed over adjacent sea points
!           !-----------------------------------------------------
      seawind=0.0
      COUNT = 0
      DO is=i-1,i+1
      DO js=j-1,j+1
        IF( flandg(is,js) < 0.001 )THEN
!               ! ie. this is basically a sea point
          ushear = u_1_px(is,js) - u_0_px(is,js)
          vshear = v_1_px(is,js) - v_0_px(is,js)
          vshr2 = MAX (1.0e-10 , ushear*ushear + vshear*vshear)
          seawind = seawind + SQRT( vshr2 )
          COUNT = COUNT + 1
        END IF
      END DO
      END DO
!           !-----------------------------------------------------
!           ! Calculate multiplicative factor, FSEAFAC, to convert
!           ! from the GBM VSHR to an appropriate marine VSHR
!           !-----------------------------------------------------
      IF (COUNT > 0) THEN
        seawind = seawind/FLOAT(COUNT)
!             ! Restrict FSEAFAC so FLANDFAC>FLANDMIN
        fseamax = MIN( 1./flandmin,                               &
                      (1.-flandmin*flandg(i,j))/(1.-flandg(i,j)) )
!             ! First limit is to keep fseamax sensible as FLANDG -> 1
!             ! Second limit is to keep fland > flandmin, remembering
!             !   that the we want FLANDG-weighted sum of factors =1
!             !   to preserve the gridbox mean VSHR
        fseafac(i,j) = MAX(1.0,                                   &
                       MIN( fseamax, seawind/vshr(i,j) ))
      END IF

      vshr_ssi(i,j) = vshr(i,j) * fseafac(i,j)

      flandfac(i,j) = ( 1.0 - fseafac(i,j)*(1.0-flandg(i,j)) )    &
                    / flandg(i,j)
      vshr_land(i,j) = vshr(i,j) * flandfac(i,j)


      vshr(i,j)= flandg(i,j)*vshr_land(i,j)                       &
        + (1.0 - flandg(i,j))*vshr_ssi(i,j)
      END IF
  END DO
  END DO

END IF  ! test on buddy_sea switch
#endif


! This IF-test is a fix, but is it the right fix?
IF (l_triffid) THEN
   DO l = 1,land_pts
     j = (land_index(l)-1)/t_i_length + 1
     i = land_index(l) - (j-1)*t_i_length
     clay_land(l) = soil_clay(i,j)
! Set default value for missing data points
     IF (clay_land(l) == rmdi) clay_land(l)=0.23
   END DO !LAND_PTS
END IF



!-----------------------------------------------------------------------
! Call MOSES II physiology routine to calculate surface conductances
! and carbon fluxes.
!-----------------------------------------------------------------------

! DEPENDS ON: physiol
CALL physiol (                                                    &
 land_pts,land_index,                                             &
 sm_levels,ntiles,tile_pts,tile_index,l_aggregate,                &
 dim_cs1, dim_cs2,                                                &
 co2_mmr,co2_3d,co2_dim_len,co2_dim_row,l_co2_interactive,        &
 l_triffid, l_q10,                                                &
 can_model,cs,frac,fland,canht_ft,photosynth_act_rad,             &
 lai_ft,pstar,qw_1,sthu,t_soil(:,1),tstar_tile,                   &
 smvccl,smvcst,smvcwt,vshr,z0_tile,z1_uv,o3,                      &
 canhc_tile,vfrac_tile,emis_tile,emis_soil,flake,                 &
 g_leaf,gs,gc,gpp,gpp_ft,npp,npp_ft,                              &
 resp_p,resp_p_ft,resp_s,resp_w_ft,smc,wt_ext_tile,fsmc,          &
 wt_ext,ra,albsoil,cos_zenith_angle                               &
,can_rad_mod,ilayers,flux_o3_ft,fo3_ft)


!----------------------------------------------------------------------
! If TRIFFID is being used apply any correction to the land-atmosphere
! fluxes on the first timestep after the last TRIFFID call. Such a
! correction will typically be associated with a total depletion of
! carbon or with maintanence of the seed fraction. The corrections
! are stored in the accumulation variables after the call to TRIFFID.
! The correction is added to the instantaneous land-atmosphere fluxes
! (so that the atmospheric carbon budget is corrected) but is not
! included in the accumulation variables which drive TRIFFID, since
! this has already been dealt with during the last TRIFFID call.
!----------------------------------------------------------------------
IF (l_triffid .AND.(asteps_since_triffid==1)                      &
    .AND. ( cycleno==numcycles .OR. l_quick_ap2) ) THEN
  DO n=1,npft
    DO l=1,land_pts
      npp_ft(l,n)=npp_ft(l,n)+npp_ft_acc(l,n)/timestep
      resp_p_ft(l,n)=resp_p_ft(l,n)-npp_ft_acc(l,n)/timestep
      npp_ft_acc(l,n)=-npp_ft_acc(l,n)
    END DO
  END DO
  DO n=1,dim_cs1
    DO l=1,land_pts
      resp_s(l,n)=resp_s(l,n)+resp_s_acc(l,n)/timestep
      resp_s_acc(l,n)=-resp_s_acc(l,n)
    END DO
  END DO
END IF

!----------------------------------------------------------------------
! Increment accumulation of leaf turnover rate.
! This is required for leaf phenology and/or TRIFFID, either of
! which can be enabled independently of the other.
!----------------------------------------------------------------------
IF ( cycleno == numcycles .OR. l_quick_ap2 ) THEN
IF (l_phenol.OR.l_triffid) THEN
  DO n=1,npft
    DO l=1,land_pts
      g_leaf_acc(l,n) = g_leaf_acc(l,n) +                         &
      g_leaf(l,n)*(timestep/secs_per_360days)
    END DO
  END DO
END IF

!----------------------------------------------------------------------
! Increment accumulation prognostics for TRIFFID
!----------------------------------------------------------------------
IF (l_triffid) THEN
  DO n=1,npft
    DO l=1,land_pts
      npp_ft_acc(l,n) = npp_ft_acc(l,n) + npp_ft(l,n)*timestep
      resp_w_ft_acc(l,n) = resp_w_ft_acc(l,n)                     &
                                      + resp_w_ft(l,n)*timestep
    END DO
  END DO
  DO n=1,dim_cs1
    DO l=1,land_pts
      resp_s_acc(l,n)=resp_s_acc(l,n)+resp_s(l,n)*timestep
    END DO
  END DO
END IF
END IF ! CycleNo == NumCycles

!-----------------------------------------------------------------------
! calculate CO2:(BIO+HUM) ratio, dependent on soil clay content, and
! sum soil respiration components
! (RESP_FRAC here then contains the fraction of soil respiration which
! is respired to the atmos. the rest is re-partitioned into BIO+HUM)

! RESP_S_ACC contains the full amount, and this is carried forward to
! VEG_CTL for use in updating soil carbon pools. RESP_S_TOT calculated
! here is passed to BL_TRMIX as the fraction which is respired as CO2
! to the atmosphere. RESP_S_TOT, and RESP_S are also passed out for
! storage in diagnostics 3293, and 3467-470.

!-----------------------------------------------------------------------
IF (l_triffid) THEN
  DO i=1,land_pts
    work_clay = EXP(-0.0786 * 100.0*clay_land(i))
    resp_frac(i) = (3.0895 + 2.672*work_clay) /                &
                   (4.0895 + 2.672*work_clay)
    resp_s(i,1)  = resp_s(i,1) * resp_frac(i)
    resp_s(i,2)  = resp_s(i,2) * resp_frac(i)
    resp_s(i,3)  = resp_s(i,3) * resp_frac(i)
    resp_s(i,4)  = resp_s(i,4) * resp_frac(i)
    resp_s_tot(i) = resp_s(i,1) + resp_s(i,2) +                   &
                    resp_s(i,3) + resp_s(i,4)
  END DO
END IF
!-----------------------------------------------------------------------
! Reset TILE_PTS and TILE_INDEX and set tile fractions to 1 if aggregate
! tiles are used (L_AGGREGATE=.T.).
! Otherwise, set tile fractions to surface type fractions.
!-----------------------------------------------------------------------
IF (l_aggregate) THEN
  tile_pts(1) = land_pts
  DO l=1,land_pts
    tile_frac(l,1) = 1.
    tile_index(l,1) = l
  END DO
ELSE
  DO n=1,ntype
    DO l = 1, land_pts
      tile_frac(l,n) = frac(l,n)
    END DO
  END DO
END IF

IF (land_pts >  0) THEN    ! Omit if no land points

!-----------------------------------------------------------------------
! Calculate the thermal conductivity of the top soil layer.
!-----------------------------------------------------------------------
! DEPENDS ON: heat_con
  CALL heat_con (land_pts,hcon,sthu(:,1),sthf(:,1),               &
                 smvcst(:,1),hcons)

! Thermal conductvity of top snow layer if nsmax > 0
  IF(nsmax > 0) THEN
    DO n=1,ntiles
! DEPENDS ON: snowtherm
      CALL snowtherm(land_pts,tile_pts(n),nsnow(:,n),             &
                     tile_index(:,n),ds(:,n,:),sice(:,n,:),       &
                     sliq(:,n,:),csnow,ksnow)
      DO l=1,land_pts
        hcons_snow(l,n) = ksnow(l,1)
      END DO
    END DO
  END IF

END IF                     ! End test on land points

!-----------------------------------------------------------------------
!! Calculate net radiation on land tiles and sea-ice
!-----------------------------------------------------------------------

    radnet_tile(:,:) = 0.
    le_tile(:,:) = 0.

IF (l_skyview) THEN
  DO n=1,ntiles
    DO k=1,tile_pts(n)
      l = tile_index(k,n)
      j=(land_index(l)-1)/pdims%i_end + 1
      i = land_index(l) - (j-1)*pdims%i_end
      radnet_tile(l,n) = sw_tile(l,n) + emis_tile(l,n)*           &
        sky(i,j)*( lw_down(i,j) - sbcon*tstar_tile(l,n)**4 )
    END DO
  END DO
ELSE
  DO n=1,ntiles
    DO k=1,tile_pts(n)
      l = tile_index(k,n)
      j=(land_index(l)-1)/pdims%i_end + 1
      i = land_index(l) - (j-1)*pdims%i_end
      radnet_tile(l,n) = sw_tile(l,n) + emis_tile(l,n)*           &
                 ( lw_down(i,j) - sbcon*tstar_tile(l,n)**4 )
    END DO
  END DO
END IF

radnet_sice(:,:,:) = 0.0

DO n = 1, nice_use
  DO k = 1, sice_pts_ncat(n)
    l = sice_index_ncat(k,n)
    j=(ssi_index(l)-1)/t_i_length + 1
    i = ssi_index(l) - (j-1)*t_i_length
    radnet_sice(i,j,n) = sw_sice(l,n) + emis_sice *                      &
               ( lw_down(i,j) - sbcon*tstar_sice_cat(i,j,n)**4 )
#if defined(UM_JULES)
    IF (.NOT.l_emis_ssi_full) THEN
!     Replicate initial implementation with omission of emissivity
!     at this point. The calculation is written in two stages 
!     to match old calculation exactly at the bit level.
      IF (flandg(i,j) <  1.0 .AND. ice_fract_cat(i,j,n) >  0.)           &
        radnet_sice(i,j,n) = ice_fract_cat(i,j,n)*sw_sice(l,n)
        radnet_sice(i,j,n) = radnet_sice(i,j,n) +                        &
          ice_fract_cat(i,j,n) * lw_down(i,j)
        radnet_sice(i,j,n) = radnet_sice(i,j,n) / ice_fract_cat(i,j,n)
        radnet_sice(i,j,n) = radnet_sice(i,j,n) -                        &
          sbcon*tstar_sice_cat(i,j,n)**4
    ENDIF
#endif
  END DO
END DO

!-----------------------------------------------------------------------
!! 4.  Surface turbulent exchange coefficients and "explicit" fluxes
!!     (P243a, routine SF_EXCH).
!!     Wind mixing "power" and some values required for other, later,
!!     diagnostic calculations, are also evaluated if requested.
!-----------------------------------------------------------------------

! DEPENDS ON: sf_exch_cable
CALL sf_exch_cable (                                                    &
 land_pts,ntiles,land_index,                                      &
 tile_index,tile_pts,fland,                                       &
 flandg(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end),     &
 nice_use,                                                        &
 nsnow,ds,hcons_snow,k_sice,                                      &
 bq_1,bt_1,canhc_tile,canopy,catch,dzsoil(1),flake,gc,hcons,      &
 can_model,catch_snow,lq_mix_bl,                                  &
 ho2r2_orog,ice_fract_cat,snowdepth,snow_tile,pstar,qw_1,         &
 radnet_sice,                                                     &
 radnet_tile,sil_orog_land,smvcst(:,1),tile_frac,timestep,        &
 surf_hgt,emis_tile,emis_soil,                                    &
 tl_1,ti,t_soil(:,1),                                             &
 tsnow,                                                           &
 tstar_tile,tstar_sea,tstar_sice_cat,z_land,                      &
 l_ctile,seasalinityfactor,                                       &
 tstar,l_aggregate,l_spec_z0,z0m_scm,z0h_scm,                     &
 l_aero_classic,l_dust,l_dust_diag,                               &
 vfrac_tile,vshr_land,vshr_ssi,zh,ddmfx,                          &
 z0_tile,z0h_tile_bare,z1_uv,z1_uv_top,z1_tq,z1_tq_top,           &
 su10,sv10,sq1p5,st1p5,sfme,sz0heff,formdrag,fd_stab_dep,         &
 orog_drag_param,z0msea,                                          &
 alpha1,alpha1_sice,ashtf_prime,ashtf_prime_tile,                 &
 recip_l_mo_sea,cdr10m,chr1p5m,                                   &
 chr1p5m_sice,e_sea,fme,fqw_1,fqw_tile,epot_tile,fqw_ice,         &
 ftl_1,ftl_tile,ftl_ice,fraca,h_blend_orog,h_sea,charnock,        &
 rhostar,resfs,resft,rib,rib_tile,                                &
 fb_surf,u_s,q1_sd,t1_sd,z0hssi,z0h_tile,z0h_eff,                 &
 z0m_gb,z0mssi,z0m_tile,z0m_eff,rho_aresist,aresist,resist_b,     &
 rho_aresist_tile,aresist_tile,resist_b_tile,                     &
 r_b_dust,cd_std_dust,u_s_std_tile,                               &
 rhokh_tile,rhokh_sice,rhokm_1,rhokm_land,rhokm_ssi,              &
 dtstar_tile,dtstar,rhokh,rhokh_mix,anthrop_heat                  &
 )


IF (lhook) CALL dr_hook('SF_EXPL_L',zhook_out,zhook_handle)

RETURN
END SUBROUTINE sf_expl_l
