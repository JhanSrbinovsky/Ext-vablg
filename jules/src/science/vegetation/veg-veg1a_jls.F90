#if defined(L19_1A)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Version 1A of vegetation section: models leaf phenology

! Subroutine Interface:
#if defined(UM_JULES)
SUBROUTINE veg(                                                   &
#else
SUBROUTINE veg1(                                                  &
#endif
               land_pts,ntiles,can_model                          &
,              a_step,phenol_period,l_phenol                      &
,              atimestep,satcon                                   &
,              g_leaf_ac,frac,lai,ht                              &
,              catch_s,catch_t,infil_t,z0_t,z0h_t                 &
,              g_leaf_day,g_leaf_phen,lai_phen                    &
               )

USE nstypes, ONLY :                                               &
!      imported scalars with intent(in)
  npft,ntype

USE switches, ONLY : l_aggregate

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

! Description:
!   Updates Leaf Area Index for Plant Functional Types (PFTs) and uses
!   this to derive new vegetation parameters for PFTs along with gridbox
!   mean values where appropriate.

! Method:
!   Calls PHENOL which models phenology and updates Leaf Area Index
!   (LAI), then passes new LAI into SPARM along with canopy height
!   and fractional cover of Plant Functional Types.  SPARM uses this to
!   derive the vegetation parameters for each PFT, and also derives
!   gridbox means where this is required.

INTEGER                                                           &
 land_pts                                                         &
                       ! IN Number of land points to be processed.
,ntiles                                                           &
                       ! IN Number of land-surface tiles.
,can_model                                                        &
                              ! IN Swith for thermal vegetation
,a_step                                                           &
                       ! IN Atmospheric timestep number.
,phenol_period         ! IN Phenology period (days).

INTEGER                                                           &
 j,l,n                      ! WORK loop counters.

LOGICAL                                                           &
 l_phenol                     ! IN .T. for interactive leaf
!                                   !    phenology.

REAL                                                              &
 atimestep                                                        &
                              ! IN Atmospheric timestep (s).
,satcon(land_pts)                                                 &
                              ! IN Saturated hydraulic
!                                   !    conductivity (kg/m2/s).
,g_leaf_ac(land_pts,npft)                                         &
                              ! INOUT Accumulated leaf turnover
!                                   !       rate.
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
,lai_phen(land_pts,npft)                                          &
                              ! OUT LAI of PFTs after phenology.
!                                   !     Required as separate variable
!                                   !     for top-level argument list
!                                   !     matching with VEG_IC2A.
,z0_t(land_pts,ntiles)       &! OUT Roughness length for tiles (m)
,z0h_t(land_pts,ntiles)       ! OUT Thermal roughness length for 
                              !     tiles (m)

INTEGER                                                           &
 nstep_phen                                                       &
                              ! WORK Number of atmospheric
!                                   !      timesteps between calls to
!                                   !      PHENOL.
,tile_pts(ntype)                                                  &
                              ! WORK Number of land points which
!                                   !      include the nth surface type.
,tile_index(land_pts,ntype)   ! WORK Indices of land points which
!                                   !      include the nth surface type.

REAL                                                              &
 dtime_phen                                                       &
                              ! WORK The phenology timestep (yr).
,g_leaf_day(land_pts,npft)                                        &
                              ! WORK Mean leaf turnover rate for
!                                   !      input to PHENOL (/360days).
,g_leaf_phen(land_pts,npft)   ! WORK Mean leaf turnover rate over
!                                   !      phenology period (/360days).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------
! Initialisations
!-----------------------------------------------------------------------
IF (lhook) CALL dr_hook('VEG',zhook_in,zhook_handle)

DO n=1,ntiles
  DO l=1,land_pts
    catch_s(l,n)=0.0
    catch_t(l,n)=0.0
    infil_t(l,n)=0.0
    z0_t(l,n)=0.0
  END DO
END DO

DO n=1,npft
  DO l=1,land_pts
    g_leaf_phen(l,n)=0.0
    g_leaf_day(l,n)=0.0
  END DO
END DO

!-----------------------------------------------------------------------
! Calculate the number of atmospheric timesteps between calls to PHENOL
! and TRIFFID.
!-----------------------------------------------------------------------
nstep_phen=INT(86400.0*phenol_period/atimestep)


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
! state.
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
! Reset the accumulation over atmospheric model timesteps to zero.
!-----------------------------------------------------------------------
    DO l=1,land_pts
      g_leaf_ac(l,n)=0.0
    END DO
  END DO
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
END SUBROUTINE veg1
#endif

#endif
