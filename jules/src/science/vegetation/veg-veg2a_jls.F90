#if defined(L19_2A)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Version 2A of vegetation section: models leaf phenology and vegetation
! competition

! Subroutine Interface:
#if defined(UM_JULES)
SUBROUTINE veg(                                                   &
#else
SUBROUTINE veg2(                                                  &
#endif
               land_pts,land_index,ntiles,can_model               &
,              a_step,asteps_since_triffid                        &
,              phenol_period,triffid_period                       &
,              l_phenol,l_triffid,l_trif_eq                       &
,              atimestep,fraca,satcon                             &
,              g_leaf_ac,g_leaf_phen_ac,npp_ac                    &
,              resp_s_ac,resp_w_ac                                &
,              cs,frac,lai,clay_frac,ht                           &
,              catch_s,catch_t,infil_t,z0_t,z0h_t                 &
,              c_veg,cv,lit_c,lit_c_mn,g_leaf_day,g_leaf_phen     &
,              lai_phen,g_leaf_dr_out,npp_dr_out,resp_w_dr_out    &
,              resp_s_dr_out                                      &
               )

USE atm_fields_bounds_mod
USE theta_field_sizes, ONLY : t_i_length
USE nstypes, ONLY :                                               &
!      imported scalars with intent(in)
  npft,ntype,soil

USE descent
USE seed
USE veg_param, ONLY : agric
USE c_mdi, ONLY : rmdi

USE switches, ONLY : l_aggregate

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

! Description:
!   Updates Leaf Area Index for Plant Functional Types (PFTs) and uses
!   this to derive new vegetation parameters for PFTs along with gridbox
!   mean values where appropriate.

! Method:
!   Calls PHENOL which models phenolgy and updates Leaf Area Index
!   (LAI), then calls TRIFFID to update vegetation and soil fractions,
!   LAI, canopy height, veg and soil carbon and carbon fluxes.  Passes
!   fractions, LAI and canopy height to SPARM which derives the
!   vegetation parameters for each PFT and also the gridbox means where
!   this is required.

INTEGER                                                           &
 land_pts                                                         &
                       ! IN Number of land points.
,land_index(land_pts)                                             &
                       ! IN Index of land points
,ntiles                                                           &
                       ! IN Number of land-surface tiles.
,can_model                                                        &
                              ! IN Swith for thermal vegetation
,a_step                                                           &
                       ! IN Atmospheric timestep number.
,asteps_since_triffid                                             &
                       ! INOUT Number of atmosphere
!                                    timesteps since last call
!                                           to TRIFFID.
,phenol_period                                                    &
                       ! IN Phenology period (days).
,triffid_period        ! IN TRIFFID period (days).

INTEGER                                                           &
 i,j,k,l,n                                                        &
                              ! WORK loop counters.
,kiter                        ! WORK Number of TRIFFID iterations.

LOGICAL                                                           &
 l_phenol                                                         &
                              ! IN .T. for interactive leaf
!                                   !    phenology.
,l_triffid                                                        &
                              ! IN .T. for interactive vegetation.
,l_trif_eq                    ! IN .T. for vegetation equilibrium.

REAL                                                              &
 atimestep                                                        &
                              ! IN Atmospheric timestep (s).
,fraca(land_pts)                                                  &
                              ! IN Fraction of agriculture.
,satcon(land_pts)                                                 &
                              ! IN Saturated hydraulic
!                                   !    conductivity (kg/m2/s).
,g_leaf_ac(land_pts,npft)                                         &
                              ! INOUT Accumulated leaf turnover
!                                   !       rate.
,g_leaf_phen_ac(land_pts,npft)                                    &
                              ! INOUT Accumulated leaf turnover
!                                   !       rate including phenology.
,npp_ac(land_pts,npft)                                            &
                              ! INOUT Accumulated NPP (kg C/m2).
,resp_w_ac(land_pts,npft)                                         &
                              ! INOUT Accumulated wood respiration
!                                   !       (kg C/m2).
,resp_s_ac(land_pts,4)                                            &
                           ! INOUT Accumulated soil respiration
!                                       !       (kg C/m2).
,cs(land_pts,4)                                                   &
                           ! INOUT Soil carbon content
,cs_tot(land_pts)                                                 &
                           ! INOUT Soil carbon content
!                                   !       (kg C/m2).
,frac(land_pts,ntype)                                             &
                              ! INOUT Fractions of surface types.
,lai(land_pts,npft)                                               &
                              ! INOUT LAI of plant functional
!                                   !       types.
,ht(land_pts,npft)                                                &
                              ! INOUT Height of plant functional
!                                   !       types (m).
,catch_s(land_pts,ntiles)                                         &
                              ! OUT Snow capacity for tiles
!                                   !     (kg/m2).
,catch_t(land_pts,ntiles)                                         &
                              ! OUT Canopy capacity for tiles
!                                   !     (kg/m2).
,infil_t(land_pts,ntiles)                                         &
                              ! OUT Maximum surface infiltration
!                                   !     rate for tiles (kg/m2/s).
,z0_t(land_pts,ntiles)                                            &
                              ! OUT Roughness length for tiles (m)
,z0h_t(land_pts,ntiles)                                            &
                              ! OUT Thermal roughness length for 
                              !     tiles (m)
,c_veg(land_pts,npft)                                             &
                              ! OUT Total carbon content of
!                                   !     the vegetation (kg C/m2).
,cv(land_pts)                                                     &
                              ! OUT Gridbox mean vegetation
!                                   !     carbon (kg C/m2).
,g_leaf_day(land_pts,npft)                                        &
                              ! OUT Mean leaf turnover rate for
!                                   !      input to PHENOL (/360days).
,g_leaf_dr_out(land_pts,npft)                                     &
                              ! OUT Mean leaf turnover rate for
!                                   !       driving TRIFFID (/360days).
,lai_phen(land_pts,npft)                                          &
                              ! OUT LAI of PFTs after phenology.
,lit_c(land_pts,npft)                                             &
                              ! OUT Carbon Litter
!                                   !     (kg C/m2/360days).
,lit_c_mn(land_pts)                                               &
                              ! OUT Gridbox mean carbon litter
!                                   !     (kg C/m2/360days).
,npp_dr_out(land_pts,npft)                                        &
                              ! OUT Mean NPP for driving TRIFFID
!                                   !     (kg C/m2/360days).
,resp_w_dr_out(land_pts,npft)                                     &
                              ! OUT Mean wood respiration for
!                                   !       driving TRIFFID
!                                   !       (kg C/m2/360days).
,resp_s_dr_out(land_pts,5)                                        &
                           ! OUT Mean soil resp for
!                                   ! NB 5=dim_cs1+1. The 5th element
!                                   ! is used as workspace.
,clay_frac(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)   &
                                  ! IN  Clay fraction of soil
,clay_land(land_pts)                                              &
                                  ! IN  clay on land points
,resp_frac(land_pts)            ! respired fraction of RESP_S
!                                   !     driving TRIFFID
!                                   !     (kg C/m2/360days).

INTEGER                                                           &
 nstep_phen                                                       &
                              ! WORK Number of atmospheric
!                                   !      timesteps between calls to
!                                   !      PHENOL.
,nstep_trif                                                       &
                              ! WORK Number of atmospheric
!                                   !      timesteps between calls to
!                                   !      TRIFFID.
,tile_pts(ntype)                                                  &
                              ! WORK Number of land points which
!                                   !      include the nth surface type.
,tile_index(land_pts,ntype)                                       &
                              ! WORK Indices of land points which
!                                   !      include the nth surface type.
,trif_pts                                                         &
                              ! WORK Number of points on which
!                                   !      TRIFFID may operate
,trif_index(land_pts)         ! WORK Indices of land points on
!                                   !      which TRIFFID may operate

REAL                                                              &
 dtime_phen                                                       &
                              ! WORK The phenology timestep (yr).
,forw                                                             &
                              ! WORK Forward timestep weighting
!                                   !      for TRIFFID.
,GAMMA                                                            &
                              ! WORK Inverse TRIFFID timestep
!                                   !      (/360days).
,gam_trif                                                         &
                              ! WORK Inverse TRIFFID coupling
!                                   !      timestep (/360days).
,ratio                                                            &
                              ! WORK Ratio of fractional
!                                   !      coverage before to that
!                                   !      after TRIFFID.
,dcs(land_pts)                                                    &
                              ! WORK Change in soil carbon
!                                   !      (kg C/m2).
,frac_agric(land_pts)                                             &
                              ! WORK Fraction of agriculture as
!                                   !      seen by TRIFFID.
,frac_old(land_pts,ntype)                                         &
                              ! WORK Fractions of surface types
!                                   !      before the call to TRIFFID.
,frac_vs(land_pts)                                                &
                              ! WORK Total fraction of gridbox
!                                   !      covered by veg or soil.
,g_leaf_phen(land_pts,npft)                                       &
                              ! WORK Mean leaf turnover rate over
!                                   !      phenology period (/360days).
,g_leaf_dr(land_pts,npft)                                         &
                              ! WORK Mean leaf turnover rate
!                                   !      for driving TRIFFID
!                                   !      (/360days).
,npp_dr(land_pts,npft)                                            &
                              ! WORK Mean NPP for driving
!                                   !      TRIFFID (kg C/m2/360days).
,resp_w_dr(land_pts,npft)                                         &
                              ! WORK Mean wood respiration for
!                                   !      driving TRIFFID
!                                   !      (kg C/m2/360days).
,resp_s_dr(land_pts,5)     ! WORK Mean soil resp for
!                                   !      driving TRIFFID
!                                   !      (kg C/m2/360days).
!                                   ! NB 5=dim_cs1+1. The 5th element
!                                   ! is used as workspace. Not sure it
!                                   ! is even needed.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook('VEG',zhook_in,zhook_handle)

!-----------------------------------------------------------------------
! Initialisations
!-----------------------------------------------------------------------
DO n=1,npft
  DO l=1,land_pts
    g_leaf_phen(l,n)=0.0
    g_leaf_day(l,n)=0.0
    g_leaf_dr(l,n)=0.0
    npp_dr(l,n)=0.0
    resp_w_dr(l,n)=0.0
    c_veg(l,n)=0.0
    lit_c(l,n)=0.0
  END DO
END DO

DO n=1,ntiles
  DO l=1,land_pts
    catch_t(l,n)=0.0
    infil_t(l,n)=0.0
    z0_t(l,n)=0.0
  END DO
END DO

IF (can_model == 4) THEN
  DO n=1,ntiles
    DO l=1,land_pts
      catch_s(l,n)=0.0
    END DO
  END DO
END IF

DO l=1,land_pts
  cv(l)=0.0
  lit_c_mn(l)=0.0
  frac_vs(l) = 0.0
  frac_agric(l) = 0.0
END DO
DO n=1,5
  DO l=1,land_pts
    resp_s_dr(l,n)=0.0
  END DO
END DO

!-----------------------------------------------------------------------
! Calculate the number of atmospheric timesteps between calls to PHENOL
! and TRIFFID.
!-----------------------------------------------------------------------
nstep_phen=INT(86400.0*phenol_period/atimestep)
nstep_trif=INT(86400.0*triffid_period/atimestep)


!-----------------------------------------------------------------------
! Find total fraction of gridbox covered by vegetation and soil, and use
! this to set indices of land points on which TRIFFID may operate
!-----------------------------------------------------------------------
trif_pts = 0
DO l=1,land_pts
  DO n=1,npft
    frac_vs(l) = frac_vs(l) + frac(l,n)
  END DO
  n=soil
  frac_vs(l) = frac_vs(l) + frac(l,n)
  IF (frac_vs(l) >= (npft*frac_min)) THEN
    trif_pts = trif_pts + 1
    trif_index(trif_pts) = l
  END IF
END DO

!-----------------------------------------------------------------------
! Create the TILE_INDEX array of land points with each surface type
!-----------------------------------------------------------------------
! DEPENDS ON: tilepts
CALL tilepts(land_pts,frac,tile_pts,tile_index)

IF (l_phenol .AND. MOD(a_step,nstep_phen) == 0) THEN

!-----------------------------------------------------------------------
! Calculate the phenology timestep in years.
!-----------------------------------------------------------------------
  dtime_phen=FLOAT(phenol_period)/360.0

  DO n=1,npft

!-----------------------------------------------------------------------
! Calculate the mean turnover rate and update the leaf phenological
! state, and take copy of updated LAI field for output as diagnostic.
!-----------------------------------------------------------------------
    DO j=1,tile_pts(n)
      l=tile_index(j,n)
      g_leaf_day(l,n)=g_leaf_ac(l,n)/dtime_phen
    END DO

    WRITE(6,*) 'Calling phenology'

! DEPENDS ON: phenol
    CALL phenol (land_pts,tile_pts(n),tile_index(:,n),n,          &
                 g_leaf_day(:,n),ht(:,n),dtime_phen,              &
                 g_leaf_phen(:,n),lai(:,n))

    WRITE(6,*) 'Phenology completed normally'

    DO l=1,land_pts
      lai_phen(l,n)=lai(l,n)
    END DO

!-----------------------------------------------------------------------
! Increment the leaf turnover rate for driving TRIFFID and reset the
! accumulation over atmospheric model timesteps to zero.
!-----------------------------------------------------------------------
    DO j=1,tile_pts(n)
      l=tile_index(j,n)
      g_leaf_phen_ac(l,n)=g_leaf_phen_ac(l,n)                     &
                    +g_leaf_phen(l,n)*dtime_phen
    END DO

    DO l=1,land_pts
      g_leaf_ac(l,n)=0.0
    END DO

  END DO
END IF

!-----------------------------------------------------------------------
! Call TRIFFID vegetation model to update vegetation and terrestrial
! carbon storage.
!-----------------------------------------------------------------------
IF (l_triffid .AND.                                               &
   (asteps_since_triffid == nstep_trif)) THEN

!-----------------------------------------------------------------------
! Calculate the TRIFFID inverse coupling timestep.
!-----------------------------------------------------------------------
  gam_trif=360.0/FLOAT(triffid_period)

!-----------------------------------------------------------------------
! Diagnose the mean fluxes over the coupling period.
!-----------------------------------------------------------------------
  DO l=1,land_pts
    resp_s_dr(l,1)=resp_s_ac(l,1)*gam_trif
    resp_s_dr(l,2)=resp_s_ac(l,2)*gam_trif
    resp_s_dr(l,3)=resp_s_ac(l,3)*gam_trif
    resp_s_dr(l,4)=resp_s_ac(l,4)*gam_trif
  END DO

  DO n=1,npft
    DO j=1,tile_pts(n)
      l=tile_index(j,n)
      g_leaf_dr(l,n)=g_leaf_phen_ac(l,n)*gam_trif
      npp_dr(l,n)=npp_ac(l,n)*gam_trif
      resp_w_dr(l,n)=resp_w_ac(l,n)*gam_trif
    END DO
  END DO

!-----------------------------------------------------------------------
! Diagnose the mean leaf turnover rates over the coupling period.
!-----------------------------------------------------------------------
  IF (l_phenol) THEN
    DO n=1,npft
      DO j=1,tile_pts(n)
        l=tile_index(j,n)
        g_leaf_dr(l,n)=g_leaf_phen_ac(l,n)*gam_trif
      END DO
    END DO
  ELSE
    DO n=1,npft
      DO j=1,tile_pts(n)
        l=tile_index(j,n)
        g_leaf_dr(l,n)=g_leaf_ac(l,n)*gam_trif
      END DO
    END DO
  END IF

!-----------------------------------------------------------------------
! Define the agricultural regions.
!-----------------------------------------------------------------------
  IF (agric) THEN
  DO l=1,land_pts
      frac_agric(l)=fraca(l)
  END DO
  END IF

!-----------------------------------------------------------------------
! Take copies of TRIFFID input variables for output as diagnostics.
!-----------------------------------------------------------------------
  DO n=1,npft
    DO l=1,land_pts
      g_leaf_dr_out(l,n)=g_leaf_dr(l,n)
      npp_dr_out(l,n)=npp_dr(l,n)
      resp_w_dr_out(l,n)=resp_w_dr(l,n)
      frac_old(l,n)=frac(l,n)
    END DO
  END DO
  DO n=1,5
    DO l=1,land_pts
      resp_s_dr_out(l,n)=resp_s_dr(l,n)
    END DO
  END DO
  DO l=1,land_pts
    dcs(l)=cs(l,1)+cs(l,2)+cs(l,3)+cs(l,4)
    resp_s_dr_out(l,5) = resp_s_dr_out(l,1) + resp_s_dr_out(l,2)  &
                       + resp_s_dr_out(l,3) + resp_s_dr_out(l,4)
  END DO

!-----------------------------------------------------------------------
! Select timestep and forward timestep weighting parameters for
! equilibrium or dynamic vegetation and call TRIFFID.
!-----------------------------------------------------------------------

! calculate RESP_FRAC.
! NOTE - here we want to use (1-X) - i.e. the fraction of
!        respiration NOT released to the atmos, so RESP_FRAC
!        here is 1-RESP_FRAC as calc'd in BL_CTL.

DO l=1,land_pts
  j=(land_index(l)-1)/t_i_length + 1
  i=land_index(l) - (j-1)*t_i_length
  clay_land(l) = clay_frac(i,j)
! Set default value for missing data points
  IF (clay_land(l)  == rmdi ) clay_land(l)=0.23
  resp_frac(l) = 1.0 / (4.0895 + 2.672*                           &
                   EXP(-0.0786 * 100.0*clay_land(l)))
END DO

  IF (l_trif_eq) THEN
    forw=1.0
    GAMMA=gamma_eq
    kiter=iter_eq
  ELSE
    forw=0.0
    GAMMA=gam_trif
    kiter=1
  END IF

  DO k=1,kiter

    WRITE(6,*) 'Calling TRIFFID'

! DEPENDS ON: triffid
    CALL triffid (land_pts,trif_pts,trif_index,forw,GAMMA         &
,                 frac_vs,frac_agric,g_leaf_dr,npp_dr,resp_s_dr   &
,                 resp_w_dr,cs,frac,ht,lai,resp_frac              &
,                 c_veg,cv,lit_c,lit_c_mn)

    WRITE(6,*) 'TRIFFID completed normally'

  END DO

!-----------------------------------------------------------------------
! Update TILE_INDEX for new surface type fractions.
!-----------------------------------------------------------------------
! DEPENDS ON: tilepts
  CALL tilepts(land_pts,frac,tile_pts,tile_index)


!-----------------------------------------------------------------------
! Reset the accumulation fluxes to zero in equilibrium mode.
!-----------------------------------------------------------------------
  IF (l_trif_eq) THEN

    DO n=1,4
      DO l=1,land_pts
        resp_s_ac(l,n)=0.0
      END DO
    END DO

    DO n=1,npft
      DO l=1,land_pts
        npp_ac(l,n)=0.0
        resp_w_ac(l,n)=0.0
      END DO
    END DO

!-----------------------------------------------------------------------
! Reset the accumulation fluxes to the TRIFFID-diagnosed corrections
! in dynamic mode. Such corrections will be typically associated with
! the total depletion of a carbon reservoir or with the maintenance
! of the plant seed fraction. Any residual for the 4 component pools is
! assumed to be in proportion to the pool size itself.

! (in the case of zero total soil carbon, use 1e-10 to prevent zero
!  divide. This will have no impact because it is multiplied by the
!  constituent stores which are also zero...)
!-----------------------------------------------------------------------
  ELSE

    DO l=1,land_pts
      cs_tot(l)=MAX(1e-10,cs(l,1)+cs(l,2)+cs(l,3)+cs(l,4))
      dcs(l)=cs_tot(l)-dcs(l)
      resp_s_dr(l,1)=(1-resp_frac(l)) * (resp_s_dr_out(l,5))
      resp_s_ac(l,1)=(lit_c_mn(l)-GAMMA*dcs(l)-resp_s_dr(l,1)) *  &
                       cs(l,1) / (GAMMA*cs_tot(l))
      resp_s_ac(l,2)=(lit_c_mn(l)-GAMMA*dcs(l)-resp_s_dr(l,1)) *  &
                       cs(l,2) / (GAMMA*cs_tot(l))
      resp_s_ac(l,3)=(lit_c_mn(l)-GAMMA*dcs(l)-resp_s_dr(l,1)) *  &
                       cs(l,3) / (GAMMA*cs_tot(l))
      resp_s_ac(l,4)=(lit_c_mn(l)-GAMMA*dcs(l)-resp_s_dr(l,1)) *  &
                       cs(l,4) / (GAMMA*cs_tot(l))
    END DO

    DO n=1,npft
      DO j=1,tile_pts(n)
        l=tile_index(j,n)
        ratio=frac_old(l,n)/frac(l,n)
        npp_ac(l,n)=ratio*(npp_dr(l,n)-npp_dr_out(l,n))/gam_trif
        resp_w_ac(l,n)=ratio*(resp_w_dr(l,n)-resp_w_dr_out(l,n))  &
                             /gam_trif
      END DO
    END DO

  END IF

!-----------------------------------------------------------------------
! Reset the accumulated leaf turnover rates to zero.
!-----------------------------------------------------------------------
  IF (l_phenol) THEN
    DO n=1,npft
      DO l=1,land_pts
        g_leaf_phen_ac(l,n)=0.0
      END DO
    END DO
  ELSE
    DO n=1,npft
      DO l=1,land_pts
        g_leaf_ac(l,n)=0.0
      END DO
    END DO
  END IF

  asteps_since_triffid=0

END IF

!-----------------------------------------------------------------------
! Calculate gridbox mean vegetation parameters from fractions of
! surface functional types
!-----------------------------------------------------------------------
! DEPENDS ON: sparm
CALL sparm (land_pts,ntiles,can_model,l_aggregate                 &
,           tile_pts,tile_index                                   &
,           frac,ht,lai,satcon,catch_s,catch_t,infil_t,z0_t,z0h_t)

IF (lhook) CALL dr_hook('VEG',zhook_out,zhook_handle)
RETURN

#if defined(UM_JULES)
END SUBROUTINE veg
#else
END SUBROUTINE veg2
#endif

#endif
