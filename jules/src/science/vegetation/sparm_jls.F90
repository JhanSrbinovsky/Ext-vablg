#if defined(L19_1A) || defined(L19_2A)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!**********************************************************************
! SUBROUTINE sparm
!
! Routine to calculate the gridbox mean land surface parameters from
! the areal fractions of the surface types and the structural
! properties of the plant functional types.
!
!**********************************************************************
SUBROUTINE sparm (land_pts,ntiles,can_model,l_aggregate           &
,                 tile_pts,tile_index,frac,ht,lai,satcon          &
,                 catch_snow,catch_tile,infil_tile                &
,                 z0_tile,z0h_tile_bare)

USE nstypes, ONLY :                                               &
!      imported scalars with intent(in)
  lake,npft,ntype,urban_canyon,urban_roof,soil

USE blend_h
USE nvegparm
USE pftparm
USE switches, ONLY : i_aggregate_opt
USE snow_param, ONLY : cansnowtile,snowloadlai
USE c_z0h_z0m, ONLY : z0h_z0m, z0h_z0m_classic
USE urban_param, ONLY : catch_c, catch_rf, infil_c, infil_rf,     &
   z0_c, z0_rf, ztm
USE switches_urban, ONLY : l_urban2t, l_moruses
USE dust_param, ONLY : z0_soil
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

LOGICAL                                                           &
 l_aggregate       ! IN Logical to set aggregate surface scheme

INTEGER                                                           &
 land_pts                                                         &
                       ! IN Number of land points to be processed.
,ntiles                                                           &
                       ! IN Number of surface tiles.
,can_model                                                        &
                              ! IN Swith for thermal vegetation
,tile_pts(ntype)                                                  &
                              ! IN Number of land points which
!                                   !    include the nth surface type.
,tile_index(land_pts,ntype)   ! IN Indices of land points which
!                                   !    include the nth surface type.

REAL                                                              &
 frac(land_pts,ntype)                                             &
                            ! IN Fractional cover of each
!                                 !    surface type.
,ht(land_pts,npft)                                                &
                            ! IN Vegetation height (m).
,lai(land_pts,npft)                                               &
                            ! IN Leaf area index.
,satcon(land_pts)           ! IN Saturated hydraulic
!                                 !    conductivity (kg/m2/s).

REAL                                                              &
 catch_tile(land_pts,ntiles)                                      &
                              ! OUT Canopy capacity for each tile
!                                   !     (kg/m2).
,infil_tile(land_pts,ntiles)                                      &
                              ! OUT Maximum surface infiltration
!                                   !     rate for each tile (kg/m2/s).
,z0_tile(land_pts,ntiles)                                         &
                              ! OUT Roughness length for each
,z0h_tile_bare(land_pts,ntiles)                                   &
                              ! OUT Snow-free thermal roughness length
!                                   !     for each tile (m).
,catch_snow(land_pts,ntiles)  ! OUT Snow capacity for tile (kg/m2)

REAL                                                              &
 catch(land_pts)                                                  &
                            ! WORK GBM canopy capacity (kg/m2).
,catch_t(land_pts,ntype)                                          &
                            ! WORK Capacities for types.
,fz0(land_pts)                                                    &
                            ! WORK Aggregation function of Z0.
,fz0h(land_pts)                                                   &
                            ! WORK Aggregation function of Z0H.
,infil(land_pts)                                                  &
                            ! WORK GBM infiltration rate(kg/m2/s).
,infil_t(land_pts,ntype)                                          &
                            ! WORK Infiltration rates for types.
,z0(land_pts)                                                     &
                            ! WORK GBM roughness length (m).
,z0h(land_pts)                                                     &
                            ! WORK GBM thermal roughness length (m).
,z0_t(land_pts,ntype)       ! WORK Roughness lengths for types.

INTEGER                                                           &
 j,l,n                      ! WORK Loop counters

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle


!----------------------------------------------------------------------
! Set parameters for vegetated surface types
!----------------------------------------------------------------------
IF (lhook) CALL dr_hook('SPARM',zhook_in,zhook_handle)
DO n=1,npft
! DEPENDS ON: pft_sparm
  CALL pft_sparm (land_pts,n,tile_index(:,n),tile_pts(n)          &
,                 ht(:,n),lai(:,n),satcon                         &
,                 catch_t(:,n),infil_t(:,n),z0_t(:,n))
END DO

! cansnowtile is only used when l_aggregate is .FALSE. - needs to be
! consistent with logic where cansnowtile is set.
IF (can_model  ==  4 .AND. .NOT. l_aggregate) THEN
  DO n=1,npft
    IF ( cansnowtile(n) ) THEN
      DO j=1,tile_pts(n)
        l = tile_index(j,n)
        catch_snow(l,n) = snowloadlai*lai(l,n)
      END DO
    END IF
  END DO
END IF

!----------------------------------------------------------------------
! Set parameters for non-vegetated surface types
!----------------------------------------------------------------------
DO n=npft+1,ntype
  DO j=1,tile_pts(n)
    l = tile_index(j,n)
    catch_t(l,n) = catch_nvg(n-npft)
    infil_t(l,n) = infil_nvg(n-npft)*satcon(l)
    z0_t(l,n) = z0_nvg(n-npft)
  END DO
END DO

! URBAN-2T Set canyon & roof parameters here. Canyon & roof done at the same
! time to avoid overwriting land-ice points.
IF ( l_urban2t ) THEN
  n = urban_canyon
  DO j=1,tile_pts(n)
    l = tile_index(j,n)
    catch_t(l,n)          = catch_c
    catch_t(l,urban_roof) = catch_rf
    infil_t(l,n)          = infil_c  * satcon(l)
    infil_t(l,urban_roof) = infil_rf * satcon(l)
    IF ( l_moruses ) THEN
      z0_t(l,n)          = ztm(l)
      z0_t(l,urban_roof) = ztm(l)
    ELSE
      z0_t(l,n)          = z0_c
      z0_t(l,urban_roof) = z0_rf
    END IF
  END DO
END IF

IF ( l_aggregate ) THEN
!----------------------------------------------------------------------
! Form means and copy to tile arrays if required for aggregate tiles
!----------------------------------------------------------------------
  DO l=1,land_pts
    catch(l) = 0.0
    fz0(l) = 0.0
    fz0h(l) = 0.0
    infil(l) = 0.0
    z0(l) = 0.0
  END DO

  DO n=1,ntype
    DO j=1,tile_pts(n)
      l = tile_index(j,n)
      fz0(l)  = fz0(l)  + frac(l,n) / (LOG(lb / z0_t(l,n)))**2
!     Explicit aggregation of z0h if required.
      IF (i_aggregate_opt == 1)                                         &
        fz0h(l) = fz0h(l) + frac(l,n) /                                 &
          ( LOG(lb / z0_t(l,n)) * LOG(lb / (z0h_z0m(n) * z0_t(l,n))) )
    END DO
  END DO

  DO l=1,land_pts
    z0(l)  = lb * EXP(-SQRT(1. / fz0(l)))
    !     Explicit aggregation of z0h if required.
    IF (i_aggregate_opt == 1)                                           &
      z0h(l) = lb * EXP(-1.0 / (fz0h(l) * LOG(lb / z0(l))) )
  END DO

  DO n=1,ntype
    DO j=1,tile_pts(n)
      l = tile_index(j,n)
      catch(l) = catch(l) + frac(l,n) * catch_t(l,n)
      infil(l) = infil(l) + frac(l,n) * infil_t(l,n)
    END DO
  END DO

  DO l=1,land_pts
!         Canopy capacity is average over non-lake surface types
    catch_tile(l,1) = 0.
    IF ( lake > 0 ) THEN
      IF ( frac(l,lake) < 1. )                                    &
        catch_tile(l,1) = catch(l) / (1. - frac(l,lake))
    END IF
    infil_tile(l,1) = infil(l)
    z0_tile(l,1) = z0(l)
    IF (i_aggregate_opt == 1) z0h_tile_bare(l,1) = z0h(l)
  END DO

ELSE
!----------------------------------------------------------------------
! Copy surface-type arrays to tiles if separate tiles used
!----------------------------------------------------------------------
  DO n=1,ntype
    DO j=1,tile_pts(n)
      l = tile_index(j,n)
      catch_tile(l,n) = catch_t(l,n)
      infil_tile(l,n) = infil_t(l,n)
      z0_tile(l,n) = z0_t(l,n)
    END DO
  END DO

END IF
!----------------------------------------------------------------------
! Set bare soil roughness for use in 1 tile dust scheme
!----------------------------------------------------------------------
! The following line has been added to readlsta so to fix CRUNs
! and is now a duplication.

z0_soil=z0_nvg(soil-npft)

IF (lhook) CALL dr_hook('SPARM',zhook_out,zhook_handle)
RETURN
END SUBROUTINE sparm
#endif
