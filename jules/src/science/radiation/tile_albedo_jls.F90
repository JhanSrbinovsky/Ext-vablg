! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Routine to calculate albedos of land-surface tiles and gridbox-mean
! albedo for MOSES II.


SUBROUTINE tile_albedo (                                          &
 p_field,land_field,land_index,ntiles,tile_pts,                   &
 tile_index,l_aggregate,l_snow_albedo,albsoil,                    &
 albobs_sw, albobs_vis, albobs_nir,                               &
 cosz,frac,lai_in,rgrain,snow_tile,soot,tstar_tile,               &
 z0_tile,alb_tile,land_albedo,albobs_sc,can_rad_mod               &
 )

USE nstypes, ONLY : npft, ntype, lake, soil, ice                  &
                   ,urban_canyon,urban_roof
USE c_0_dg_c
USE nvegparm
USE pftparm
USE rad_param,  ONLY : kland,maskd,tcland
USE snow_param, ONLY : rho_snow_const                             &
                      ,cansnowtile
USE switches,   ONLY : l_point_data                               &
                      ,can_model                                  &
                      ,l_snowdep_surf                             &
                      ,l_flake_model                              &
                      ,l_spec_albedo                              &
                      ,l_albedo_obs
USE switches_urban, ONLY : l_urban2t, l_moruses_albedo
USE urban_param, ONLY : albwl, albrd, hwr, ztm, albsnc_c,         &
                        albsnc_rf, albsnf_c, albsnf_rf
USE lake_mod,    ONLY : albedo_whiteice_ref                       &
                       ,albedo_blueice_ref                        &
                       ,c_albice_MR                               &
                       ,lake_h_ice

#if defined(UM_JULES)
USE jules_mod,   ONLY : smvcst_levs                               &
                       ,snowdep_surf                              &
                       ,albobs_scaling
USE ereport_mod, ONLY : ereport
#else
USE p_s_parms, ONLY : smvcst_levs => smvcst
USE jules_mod, ONLY : snowdep_surf, albobs_scaling
#endif
USE prognostics, ONLY : snowdepth

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE c_mdi, ONLY : rmdi
USE cable_data_mod, only : cable   
IMPLICIT NONE

! Subroutine arguments
!   Scalar arguments with intent(in):
INTEGER                                                           &
 p_field                                                          &
                             ! Total number of grid points.
,land_field                                                       &
                             ! No. of land points.
,ntiles                      ! Number of surface tiles.

LOGICAL                                                           &
 l_aggregate                                                      &
                             ! IN Logical to set aggregate
!                                  !    surface scheme
,l_snow_albedo               ! .TRUE. for prognostic snow albedo.

!   Array arguments with intent(in):
INTEGER                                                           &
 land_index(land_field)                                           &
                             ! Index of land points.
,tile_pts(ntype)                                                  &
                             ! Number of land points which
!                                  ! include the nth surface type.
,tile_index(land_field,ntype) 
                             ! Indices of land points which
!                                  ! include the nth surface type.

REAL                                                              &
 albsoil(land_field)                                              &
                             ! Soil albedo.
,albobs_sw(land_field)                                            &
                             ! Observed snow-free sw albedo.
,albobs_vis(land_field)                                           &
                             ! Observed snow-free vis albedo.
,albobs_nir(land_field)                                           &
                             ! Observed snow-free nir albedo.
,albsfm_sw(land_field)                                            &
                             ! Model grid-box mean sf sw albedo 
,albsfm_vis(land_field)                                           &
                             ! Model grid-box mean sf vis albedo 
,albsfm_nir(land_field)                                           &
                             ! Model grid-box mean sf nir albedo 
,cosz(p_field)                                                    &
                             ! Cosine of the zenith angle.
,frac(land_field,ntype)                                           &
                             ! Fractional cover of each
!                                  ! surface type.
,lai_in(land_field,npft)                                          &
                             ! Leaf area index.
,rgrain(land_field,ntiles)                                        &
                             ! Snow grain size on tiles
!                                  ! (microns).
,snow_tile(land_field,ntiles)                                     &
                             ! Canopy snow on tiles (kg/m2)
,soot(p_field)                                                    &
                             ! Snow soot content (kg/kg).
,tstar_tile(land_field,ntiles)                                    &
                             ! Tile surface temperatures (K).
,z0_tile(land_field,ntiles)  ! Surface roughness on tiles (m).

!   Array arguments with intent(out):
REAL                                                              &
 alb_tile(land_field,ntiles,4)                                    &
                              !Albedos for surface tiles.
!                                  !   (*,*,1) - Direct beam visible
!                                  !   (*,*,2) - Diffuse visible
!                                  !   (*,*,3) - Direct beam near-IR
!                                  !   (*,*,4) - Diffuse near-IR
,land_albedo(p_field,4)                                            &
                              ! GBM albedos.
,albobs_sc(p_field,ntiles,2)  ! albedo scaling to obs in VIS and NIR
                              ! for diagnostics output by the UM

! Local arrays:
REAL                                                              &
 albsnc(land_field,ntype)                                         &
                             ! Snow-covered albedo of surf types.
,albsnf(land_field,ntype)                                         &
                             ! Snow-free albedo of surf types.
,albsfsc(land_field,2)                                            &
                             ! local scaling factor to match obs
,albobs_tile(land_field,ntype)                                    &
                             ! Albedo of the tiles (full veg) after
                             ! being scaled to obs
,alb_type(land_field,ntype,4)                                     &
                             ! Albedos of surface types.
,alb_snow(land_field,ntype,4)                                     &
                             ! Snow albedos.
,fsnow(land_field)                                                &
                             ! Weighting factor for albedo.
,lai(land_field,npft)                                             &
                             ! Adjusted leaf area index.
,snowd(land_field)                                                &
                             ! Work variable (snow depth).
,tstar(land_field)                                                &
                             ! Copy of TSTAR_TILE.
,z0(land_field)              ! Copy of Z0_TILE.

INTEGER, PARAMETER ::       ilayers_dummy=1


! This variable is not used in this routine, but it is required in
! the subroutine argument list for the UM implementation of JULES
INTEGER                                                           &
 can_rad_mod                       ! Which canopy radiation model
                                   ! we're using



REAL                                                              &
 fapar_dir_dummy(land_field,npft,ilayers_dummy)                   &
!                                 ! Profile of absorbed PAR -
!                                 ! Direct beam - DUMMY
,fapar_dif_dummy(land_field,npft,ilayers_dummy)                   &
!                                 ! Profile of absorbed PAR -
!                                 ! Diffuse beam -DUMMY
,fapar_dir2dif_dummy(land_field,npft,ilayers_dummy)               &
!                                 ! DUMMY
,fapar_dif2dif_dummy(land_field,npft,ilayers_dummy)               &
!                                 ! DUMMY
,fapar_dir2dir_dummy(land_field,npft,ilayers_dummy)               &
!                                 ! DUMMY
,fsun_dummy(land_field,npft,ilayers_dummy)
!                                 ! DUMMY

! Local scalars:
REAL                                                              &
 dsa                                                              &
                             ! Deep-snow albedo.
,flit
                             ! Weighting factor for albedo.
INTEGER                                                           &
 band,i,j,l,n                ! Loop counters

LOGICAL                                                           &
 pointflag                   ! Switch for treatment of snow

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

INTEGER ::              errcode            ! Error code
CHARACTER (LEN = 80) :: errmsg             ! Error message text

! declare parameters used in ccontrol
!#include "chsunits.h"
!#include "csubmmax.h"
! We need this file for CAN_MODEL
!#include "ccontrol.h"


IF (lhook) CALL dr_hook('TILE_ALBEDO',zhook_in,zhook_handle)
DO n=1,ntiles
  DO band=1,4
    DO l=1,land_field
      alb_tile(l,n,band) = 0.
    END DO
  END DO
END DO
DO n=1,ntype
  DO band=1,4
    DO l=1,land_field
      alb_type(l,n,band) = 0.
      alb_snow(l,n,band) = 0.
    END DO
  END DO
END DO

! Impose minimum LAI for bare vegetation
DO n=1,npft
  DO j=1,tile_pts(n)
    l = tile_index(j,n)
    lai(l,n) = MAX( lai_in(l,n), 0.5 )
  END DO
END DO

! Equivalent snowdepth for surface calculations.
snowdep_surf(:,:) = snowdepth(:,:)
DO n=1,ntiles
  IF ( (can_model==4).AND.cansnowtile(n).AND.l_snowdep_surf ) THEN
    DO l=1,land_field
      snowdep_surf(l,n)=snow_tile(l,n)/rho_snow_const
    END DO
  END IF
END DO


! scaling factor to get the model albedo to agree with the obs
! (initialise it as it goes into the module for use elsewhere)
IF (l_albedo_obs) THEN
  albobs_scaling = 1.0
END IF
albobs_sc = rmdi

IF (l_spec_albedo) THEN
!----------------------------------------------------------------------
! Spectral albedo scheme, can have prognostic or diagnostic snow albedo
!----------------------------------------------------------------------

! Set albedos of vegetated surface types
! The logical argument (getProfile in albpft) is FALSE to indicate
! that profiles through the canopy should not be calculated.
! The 0 indicates that no scaling to obs is required
! DEPENDS ON: albpft
  CALL albpft       (p_field,land_field,                          &
                     land_index,tile_index,tile_pts,              &
                     ilayers_dummy,.FALSE.,0,                     &
                     albsoil,cosz,lai,alb_type,                   &
                     fapar_dir_dummy,fapar_dif_dummy,             &
                     fapar_dir2dif_dummy,fapar_dif2dif_dummy,     &
                     fapar_dir2dir_dummy,fsun_dummy )

! Set albedos of non-vegetated surface types
  DO band=1,4
    DO n=npft+1,ntype
      DO j=1,tile_pts(n)
        l = tile_index(j,n)
        alb_type(l,n,band) = albsnf_nvg(n-npft)
        IF ( albsnf_nvg(n-npft) <  0. )                           &
                                               ! Soil tile
          alb_type(l,n,band) = albsoil(l)
      END DO
    END DO
  END DO

! Set snow-free albedos for urban_2T. Cannot have a canyon without a roof so
! these are done at the same time to avoid overwriting ice tiles. These are
! over-written if l_moruses_albedo.
  IF ( l_urban2t ) THEN
    n = urban_canyon
    DO j=1,tile_pts(n)
      l = tile_index(j,n)
      alb_type(l,n,:)          = albsnf_c
      alb_type(l,urban_roof,:) = albsnf_rf
    END DO
  END IF

  IF ( l_albedo_obs ) THEN
! Average the model snow-free diffuse albedos (2 for VIS and 4 for NIR)
! over the grid box, by the tile fraction:
    albsfm_vis(:) = 0.0
    albsfm_nir(:) = 0.0
    DO n=1,ntype
      DO j=1,tile_pts(n)
        l = tile_index(j,n)
        albsfm_vis(l) = albsfm_vis(l) + ( alb_type(l,n,2) * frac(l,n) )
        albsfm_nir(l) = albsfm_nir(l) + ( alb_type(l,n,4) * frac(l,n) )
      END DO
    END DO

! Work out the scaling factor that needs to be applied to make the model
! snow-free albedo agree with the obs:
    DO l=1,land_field
      albsfsc(l,1) = albobs_vis(l) / albsfm_vis(l)
      albsfsc(l,2) = albobs_nir(l) / albsfm_nir(l)
      ! If point is land ice then do not do anything:
      IF ( frac(l,ice) > 0.5 ) albsfsc(l,:) = 1.0
    END DO

! Recalculate the above albedos, but with scaled input albedos, within limits
! and store the scaling:
!
! starting with the non-veg (only need to do calculations twice, as the the 
! diffuse and direct albedos are the same):
    DO band=1,2
      DO n=npft+1,ntype
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
          IF ( n == soil ) THEN
            alb_type(l,n,2*band-1) =MIN( MAX(albsoil(l) * albsfsc(l,band),     &
                             albsnf_nvgl(n-npft)), albsnf_nvgu(n-npft))
            albobs_scaling(l,n,band) = alb_type(l,n,2*band-1)/ albsoil(l)
          ELSE
            alb_type(l,n,2*band-1) =MIN(MAX(albsnf_nvg(n-npft)*albsfsc(l,band),&
                           albsnf_nvgl(n-npft)), albsnf_nvgu(n-npft)) 
            albobs_scaling(l,n,band) = alb_type(l,n,2*band-1)/albsnf_nvg(n-npft)
          END IF
        END DO
      END DO
    END DO
! now fill in the diffuse albedo from the direct value:
    DO band=1,2
      DO n=npft+1,ntype
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
          alb_type(l,n,2*band) = alb_type(l,n,2*band-1)
        END DO
      END DO
    END DO

! two-tile urban:
    IF ( l_urban2t ) THEN
      DO band=1,2
        n = urban_canyon
        !DO l=1,land_field
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
! scale and apply limits:
          alb_type(l,n,2*band-1) = MIN( MAX(albsnf_c * albsfsc(l,band),        &
                           albsnf_nvgl(n-npft)), albsnf_nvgu(n-npft)) 
          albobs_scaling(l,n,band) =  alb_type(l,n,2*band-1) / albsnf_c
        END DO
        n = urban_roof
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
! scale and apply limits:
          alb_type(l,n,2*band-1) = MIN( MAX(albsnf_rf * albsfsc(l,band),       &
                           albsnf_nvgl(n-npft)), albsnf_nvgu(n-npft)) 
          albobs_scaling(l,n,band) =  alb_type(l,n,2*band-1) / albsnf_rf
        END DO
      END DO
! now fill in the diffuse albedo from the direct value:
      DO band=1,2
        n=urban_canyon
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
          alb_type(l,n,2*band) = alb_type(l,n,2*band-1)
        END DO
        n=urban_roof
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
          alb_type(l,n,2*band) = alb_type(l,n,2*band-1)
        END DO
      END DO
    END IF

! now do the veg tiles, by calling albpft again and telling it to
! scale the vegetation scattering and reflectivity parameters:
!
! Put the intended scaling into albobs_scaling for each PFT
! and then albpft will correct it for the non-linearity
! (which is what the 1 indicates): 
  DO band=1,2
    DO n=1,npft
      DO l=1,land_field
        albobs_scaling(l,n,band) = albsfsc(l,band)
      END DO
   END DO
  END DO
! DEPENDS ON: albpft
  CALL albpft       (p_field,land_field,                          &
                     land_index,tile_index,tile_pts,              &
                     ilayers_dummy,.FALSE.,1,                     &
                     albsoil,cosz,lai,alb_type,                   &
                     fapar_dir_dummy,fapar_dif_dummy,             &
                     fapar_dir2dif_dummy,fapar_dif2dif_dummy,     &
                     fapar_dir2dir_dummy,fsun_dummy )

! ! Check on 0 > albedo > 1
    DO band=1,4
      DO n=1,ntype
      !DO l=1,land_field
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
          IF ( (alb_type(l,n,band) < 0.) .OR. (alb_type(l,n,band) > 1.) ) THEN
            WRITE(6,'(a,F24.12)') 'ERROR: Albedo < 0.0 or > 1.0: ',            &
                              alb_type(l,n,band)
            WRITE(6,'(a,I3,a,I3)') 'Tile number', n, ', Band', band
            WRITE(6,'(a,F24.12)') 'Scaling', albsfsc(l,band)
            IF (band <= 2) THEN
              WRITE(6,'(a,F24.12,F24.12,F24.12)') 'obs, model, soil:',         &
                               albobs_vis(l), albsfm_vis(l), albsoil(l)
            ELSE
              WRITE(6,'(a,F24.12,F24.12,F24.12)') 'obs, model, soil:',         &
                               albobs_nir(l), albsfm_nir(l), albsoil(l)
            END IF
#if defined(UM_JULES)
            errcode = 1
            errmsg  = 'Unphysical albedos being created'
            CALL ereport ('tile_albedo',errcode,errmsg)
#else
            WRITE(6,'(/,a)') 'ERROR: in tile_albedo'
            STOP
#endif
         END IF
        END DO
      END DO
    END DO
  
  END IF ! ends l_albedo_obs for the spectral albedo scheme

! Re-set albedos of frozen lakes if FLake is used 
! using the algorithm from the flake interface program 
! (reference Mironov & Ritter 2004). 
  IF ( l_flake_model.and.(.not.l_aggregate) ) THEN 
    n = lake 
    DO j=1,tile_pts(n) 
      l = tile_index(j,n) 
      IF ((lake_h_ice(l) > 0.0).AND.(tstar_tile(l,n) <= tm)) THEN 
        alb_type(l,n,:) =   albedo_whiteice_ref                           & 
                          + EXP( -c_albice_MR*(tm-tstar_tile(l,n)) / tm ) & 
                          * (albedo_blueice_ref - albedo_whiteice_ref) 
      END IF 
    END DO 
  END IF 

  IF (l_snow_albedo) THEN
!----------------------------------------------------------------------
! Spectral albedo scheme with prognostic snow albedo
!----------------------------------------------------------------------
! Calculate snow albedos
! DEPENDS ON: albsnow
    CALL albsnow(p_field,land_field,land_index,                   &
                 ntiles,tile_index,tile_pts,l_aggregate,          &
                 cosz,rgrain,snowdep_surf,soot,alb_snow)

! Adjust surface type albedos for snow cover
    DO l=1,land_field
      snowd(l) = snowdep_surf(l,1)
      z0(l) = z0_tile(l,1)
    END DO
    DO n=1,ntype
      IF (.NOT. l_aggregate) THEN
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
          snowd(l) = snowdep_surf(l,n)
          z0(l) = z0_tile(l,n)
        END DO
      END IF
! Calculate snow albedo weighting factor.
      fsnow(:) = 0.0
      IF ( l_point_data ) THEN
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
          IF ( snowd(l) .gt. 0.) fsnow(l) = 1.0 - EXP( -50.0*snowd(l) )
        END DO
      ELSE
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
          IF ( snowd(l) .gt. 0.) fsnow(l) = snowd(l) / ( snowd(l) + 10.*z0(l) )
        END DO
      END IF
! Calculate weighted tile albedo.
      DO j=1,tile_pts(n)
        l = tile_index(j,n)
        DO band=1,4
          alb_type(l,n,band) = fsnow(l)*alb_snow(l,n,band)        &
                              + (1. - fsnow(l))*alb_type(l,n,band)
        END DO

! MORUSES: overwrite alb_type with values from canyonalb or albsnf_rf. Use
! test on smvcst_levs to check for land-ice points. Loop around band could be
! taken out as no snow involved at the moment.
        IF ( l_moruses_albedo ) THEN
          IF ( n == urban_canyon ) THEN
            i = land_index(l)
            DO band = 1,4
! DEPENDS ON: canyonalb
             CALL canyonalb(cosz(i),hwr(l),albwl(l),albrd(l),      &
                 alb_type(l,n,band))
            END DO
          ELSE IF ( n == urban_roof                                 &
                  .AND. smvcst_levs(l,1) > 0.0 ) THEN
! MORUSES here removes the effects of snow; snow scheme for MORUSES needs
! to be added
            alb_type(l,n,:) = albsnf_rf
          END IF
        END IF

      END DO
    END DO   !  ntype

  ELSE
!----------------------------------------------------------------------
! spectral albedo scheme with diagnosed snow albedo
!----------------------------------------------------------------------
! Adjust surface type albedos for snow cover
    DO l=1,land_field
      tstar(l) = tstar_tile(l,1)
      snowd(l) = snowdep_surf(l,1)
    END DO
! Set albedos of snow covered vegetated surface types:
    DO n=1,npft
      DO j=1,tile_pts(n)
        l = tile_index(j,n)
        flit = 1.0 - EXP(-kext(n)*lai(l,n))
        albsnc(l,n) = albsnc_min(n)*(1 - flit) + albsnc_max(n)*flit
      END DO
    END DO
! and the snow covered non-veg types:
    DO n=npft+1,ntype
      DO j=1,tile_pts(n)
        l = tile_index(j,n)
        albsnc(l,n) = albsnc_nvg(n-npft)
      END DO
    END DO
! now apply the snow covered albedos to the snow free ones in alb_type
    DO band=1,4
      DO n=1,ntype
        IF (.NOT. l_aggregate) THEN
          DO j=1,tile_pts(n)
            l = tile_index(j,n)
            tstar(l) = tstar_tile(l,n)
            snowd(l) = snowdep_surf(l,n)
          END DO
        END IF
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
          IF ( tstar(l)  <   tcland ) THEN
            dsa = albsnc(l,n)
          ELSE IF ( tstar(l)  <   tm ) THEN
            dsa = albsnc(l,n) + kland*(alb_type(l,n,band) - albsnc(l,n)) *     &
                                   (tstar(l) - tcland)
          ELSE
            dsa = albsnc(l,n) + kland*(alb_type(l,n,band) - albsnc(l,n)) *     &
                                   (tm - tcland)
          END IF
          alb_type(l,n,band) = alb_type(l,n,band) + (dsa-alb_type(l,n,band)) * &
                                     ( 1. - EXP(-maskd*snowd(l)) )
        END DO
      END DO
    END DO

    IF ( l_moruses_albedo ) THEN
      n = urban_canyon
      DO j = 1,tile_pts(n)
        l = tile_index(j,n)
        i = land_index(l)
        ! DEPENDS ON: canyonalb
        CALL canyonalb( cosz(i), hwr(l), albwl(l), albrd(l),        &
           alb_type(l,n,1) )
! MORUSES here removes the effects of snow; snow scheme for MORUSES needs
! to be added
        alb_type(l,urban_roof,:) = albsnf_rf
      END DO
    END IF

  END IF ! ends test on snow scheme for spectral albedo

ELSE
!----------------------------------------------------------------------
! Non-spectral albedo scheme with diagnosed snow albedo
!----------------------------------------------------------------------
  IF (l_snow_albedo) THEN
#if defined(UM_JULES)
    errcode = 1
    errmsg  = 'l_snow_albedo is dependent on l_spec_albedo'
    CALL ereport ('tile_albedo',errcode,errmsg)
#else
    WRITE(6,'(/,a)') 'ERROR: l_snow_albedo is dependent on l_spec_albedo'
    STOP
#endif
  END IF

! Set albedos of vegetated surface types
  DO n=1,npft
    DO j=1,tile_pts(n)
      l = tile_index(j,n)
      flit = 1.0 - EXP(-kext(n)*lai(l,n))
      albsnc(l,n) = albsnc_min(n)*(1 - flit) + albsnc_max(n)*flit
      albsnf(l,n) = albsoil(l)*(1 - flit) + albsnf_max(n)*flit
    END DO
  END DO

! Set albedos of non-vegetated surface types
  DO n=npft+1,ntype
    DO j=1,tile_pts(n)
      l = tile_index(j,n)
      albsnc(l,n) = albsnc_nvg(n-npft)
      albsnf(l,n) = albsnf_nvg(n-npft)
      IF ( albsnf_nvg(n-npft) <  0. ) albsnf(l,n) = albsoil(l)
    END DO
  END DO

! Set canyon & roof albedos for two-tile urban at the same time to avoid
! over-writing land-ice points. Note alb_type is over-written if
! l_moruses_albedo.
  IF ( l_urban2t ) THEN
    n = urban_canyon
    DO j=1,tile_pts(n)
      l = tile_index(j,n)
      albsnc(l,n) = albsnc_c
      albsnf(l,n) = albsnf_c
      albsnc(l,urban_roof) = albsnc_rf
      albsnf(l,urban_roof) = albsnf_rf
    END DO
  END IF

  IF ( l_albedo_obs ) THEN
! Average the model snow-free albedo over the grid box, by the tile fraction:
    albsfm_sw(:) = 0.0
    DO n=1,ntype
      DO j=1,tile_pts(n)
        l = tile_index(j,n)
        albsfm_sw(l) = albsfm_sw(l) + ( albsnf(l,n) * frac(l,n) )
      END DO
    END DO

! Work out the scaling factor that needs to be applied to make the model
! albedo agree with the obs:
    DO l=1,land_field
      albsfsc(l,1) = albobs_sw(l) / albsfm_sw(l)
      ! If point is land ice then do not do anything:
      IF ( frac(l,ice) > 0.5 ) albsfsc(l,1) = 1.0
    END DO

! Recalculate the above albedos, but with scaled input albedos, within limits:
! scale the bare soil albedo first:
    DO l=1,land_field
      albobs_tile(l,soil)=albsoil(l)*albsfsc(l,1)
    END DO
! apply the limits:
    DO l=1,land_field
      albobs_tile(l,soil) =MIN(MAX(albobs_tile(l,soil),albsnf_nvgl(soil-npft)),&
               albsnf_nvgu(soil-npft))
    END DO
! and make a note of what the scaling ended up being, after the limits:
    DO l=1,land_field
      albobs_scaling(l,soil,1) = albobs_tile(l,soil) / albsoil(l)
    END DO

! now do the veg tiles
    DO n=1,npft
       !DO l=1,land_field
       DO j=1,tile_pts(n)
         l = tile_index(j,n)
! scale the fully veg albedo, and apply limits:
         albobs_tile(l,n) =MIN( MAX(albsnf_max(n)*albsfsc(l,1),albsnf_maxl(n)),&
                 albsnf_maxu(n))
! work out the albedo of the tile, with bare soil under the actual partial veg:
         flit = 1.0 - EXP(-kext(n)*lai(l,n))
         albsnf(l,n) = (1 - flit)*albobs_tile(l,soil) + flit*albobs_tile(l,n)
! and store the scaling:
         albobs_scaling(l,n,1) =  albobs_tile(l,n) / albsnf_max(n)    
      END DO
    END DO

! non-vegetated surface types:
    DO n=npft+1,ntype
      IF ( n == soil ) THEN
        !DO l=1,land_field
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
          albsnf(l,n) = albobs_tile(l,soil)
        END DO
      ELSE
        !DO l=1,land_field
        DO j=1,tile_pts(n)
          l = tile_index(j,n)
! apply scaling and limits:
          albsnf(l,n) = MIN( MAX(albsnf_nvg(n-npft)*albsfsc(l,1),              &
                         albsnf_nvgl(n-npft)), albsnf_nvgu(n-npft))
          albobs_tile(l,n)= albsnf(l,n)
! and store the scaling
          albobs_scaling(l,n,1) =  albobs_tile(l,n) / albsnf_nvg(n-npft)
        END DO
      END IF
    END DO

! two-tile urban:
    IF ( l_urban2t ) THEN
      n = urban_canyon
      !DO l=1,land_field
      DO j=1,tile_pts(n)
        l = tile_index(j,n)
! scale and apply limits:
        albsnf(l,n) =  MIN( MAX(albsnf_c*albsfsc(l,1),albsnf_nvgl(n-npft)),    &
                       albsnf_nvgu(n-npft))
        albsnf(l,urban_roof) = MIN( MAX(albsnf_rf*albsfsc(l,1),                &
                   albsnf_nvgl(urban_roof-npft)), albsnf_nvgu(urban_roof-npft))
        albobs_tile(l,n)= albsnf(l,n)
        albobs_tile(l,urban_roof)= albsnf(l,urban_roof)
! and store the scaling
        albobs_scaling(l,n,1) =  albobs_tile(l,n) / albsnf_c
        albobs_scaling(l,urban_roof,1) =  albobs_tile(l,urban_roof)/albsnf_rf
      END DO
    END IF

! ! Check on 0 > albedo > 1
   DO n=1,ntype
    !DO l=1,land_field
      DO j=1,tile_pts(n)
        l = tile_index(j,n)
        IF ( (albsnf(l,n) < 0.) .OR. (albsnf(l,n) > 1.) ) THEN
          WRITE(6,'(a,F24.12)') 'ERROR: Albedo < 0.0 or > 1.0: ', albsnf(l,n)
          WRITE(6,'(a,I3)') 'Tile number', n
          WRITE(6,'(a,F24.12)') 'Scaling', albsfsc(l,1)
          WRITE(6,'(a,F24.12,F24.12,F24.12)') 'obs, model, soil:',             &
                  albobs_sw(l), albsfm_sw(l), albsoil(l)
#if defined(UM_JULES)
          errcode = 1
          errmsg  = 'Unphysical albedos being created'
          CALL ereport ('tile_albedo',errcode,errmsg)
#else
          WRITE(6,'(/,a)') 'ERROR: in tile_albedo'
          STOP
#endif
        END IF
      END DO
    END DO

  END IF ! ends l_albedo_obs for the non-spectral albedo scheme

! Re-set albedos of frozen lakes if FLake is used 
! using the algorithm from the flake interface program 
! (reference Mironov & Ritter 2004). 
  IF ( l_flake_model.and.(.not.l_aggregate) ) THEN 
    n = lake 
    DO j=1,tile_pts(n) 
      l = tile_index(j,n) 
      IF ((lake_h_ice(l) > 0.0).AND.(tstar_tile(l,n) <= tm)) THEN 
        albsnf(l,n) =   albedo_whiteice_ref                           & 
                      + EXP( -c_albice_MR*(tm-tstar_tile(l,n)) / tm ) & 
                      * (albedo_blueice_ref - albedo_whiteice_ref) 
      END IF 
    END DO 
  END IF 

! Adjust surface type albedos for snow cover
  DO l=1,land_field
    tstar(l) = tstar_tile(l,1)
    snowd(l) = snowdep_surf(l,1)
  END DO
  DO n=1,ntype
    IF (.NOT. l_aggregate) THEN
      DO j=1,tile_pts(n)
        l = tile_index(j,n)
        tstar(l) = tstar_tile(l,n)
        snowd(l) = snowdep_surf(l,n)
      END DO
    END IF
    DO j=1,tile_pts(n)
      l = tile_index(j,n)
      IF ( tstar(l)  <   tcland ) THEN
        dsa = albsnc(l,n)
      ELSE IF ( tstar(l)  <   tm ) THEN
        dsa = albsnc(l,n) + kland*(albsnf(l,n) - albsnc(l,n))     &
                                 *(tstar(l) - tcland)
      ELSE
        dsa = albsnc(l,n) + kland*(albsnf(l,n) - albsnc(l,n))     &
                                 *(tm - tcland)
      END IF
      alb_type(l,n,1) = albsnf(l,n) + (dsa - albsnf(l,n)) *       &
                                    ( 1. - EXP(-maskd*snowd(l)) )
    END DO
  END DO

  IF ( l_moruses_albedo ) THEN
    n = urban_canyon
    DO j = 1,tile_pts(n)
      l = tile_index(j,n)
      i = land_index(l)
      ! DEPENDS ON: canyonalb
      CALL canyonalb( cosz(i), hwr(l), albwl(l), albrd(l),        &
         alb_type(l,n,1) )
! MORUSES here removes the effects of snow; snow scheme for MORUSES needs
! to be added
      alb_type(l,urban_roof,1) = albsnf_rf
    END DO
  END IF

! Copy albedo to all bands
  DO band=2,4
    DO n=1,ntype
      DO j=1,tile_pts(n)
        l = tile_index(j,n)
        alb_type(l,n,band) = alb_type(l,n,1)
      END DO
    END DO
  END DO

END IF       ! Spectral or non-spectral albedo schemes

!----------------------------------------------------------------------
! Calculate GBM surface albedo
!----------------------------------------------------------------------

DO band=1,4
  DO i=1,p_field
    land_albedo(i,band) = 0.
  END DO
  DO n=1,ntype
    DO j=1,tile_pts(n)
      l = tile_index(j,n)
      i = land_index(l)
      land_albedo(i,band) = land_albedo(i,band) +                 &
                            frac(l,n)*alb_type(l,n,band)
    END DO
  END DO
END DO

!----------------------------------------------------------------------
! Copy albedos as required for aggregate or distinct tiles
!----------------------------------------------------------------------

IF (l_aggregate) THEN
  DO band=1,4
    DO l=1,land_field
      i = land_index(l)
      alb_tile(l,1,band) = land_albedo(i,band)
    END DO
  END DO
ELSE
  DO band=1,4
    DO n=1,ntype
      DO j=1,tile_pts(n)
        l = tile_index(j,n)
        alb_tile(l,n,band) = alb_type(l,n,band)
      END DO
    END DO
  END DO
END IF

!----------------------------------------------------------------------
! Copy the albedo scaling to obs for output as diagnostic.
!----------------------------------------------------------------------
IF (l_albedo_obs) THEN
  IF (l_aggregate) THEN
! Output the unmodified sclaing factor albsfsc
    IF (l_spec_albedo) THEN
! There are 2 scalings, one for VIS and NIR
       DO band=1,2
         DO l=1,land_field
           i = land_index(l)
           albobs_sc(i,1,band) = albsfsc(l,band)
         END DO
       END DO
    ELSE
! Both diagnostics will see the same scaling
       DO l=1,land_field
         i = land_index(l)
         albobs_sc(i,1,1) = albsfsc(l,1)
         albobs_sc(i,1,2) = albsfsc(l,1)
       END DO
    END IF
  ELSE
! Output the modified sclaing factor on tiles, albobs_scaling
    IF (l_spec_albedo) THEN
! There are 2 scalings, one for VIS and NIR
       DO band=1,2
         DO n=1,ntiles
           DO l=1,land_field
             i = land_index(l)
             albobs_sc(i,n,band) = albobs_scaling(l,n,band)
           END DO
         END DO
       END DO
    ELSE
! Both diagnostics will see the same scaling
       DO n=1,ntiles
         DO l=1,land_field
           i = land_index(l)
           albobs_sc(i,n,1) = albobs_scaling(l,n,1)
           albobs_sc(i,n,2) = albobs_scaling(l,n,1)
         END DO
       END DO
    END IF
  END IF
ELSE
! no obs scaling used, so the scaling is 1.0
  albobs_sc(:,:,:)=1.0
END IF

IF ( cable% um% l_cable ) then
  if(cable% mp% timestep_number .gt. 1) then
print *, "jhan:tile_albedo:PRE _rad_driver"
! DEPENDS ON: cable_rad_driver.o
   CALL cable_rad_driver( cable% forcing% ShortWave,                           &
          cable% um% cos_zenith_angle, cable% um% SNOW_TILE,                   &
          cable% cable% SNOW_TMP3L, cable% cable% SNOW_RHO1L,                  &
          cable% cable% TSOIL_TILE, cable% cable% SNOW_FLG3L,                  &
          cable% um% albsoil,                            &
          ! the next 2 same vars are available here anyway
          cable% um% LAND_ALBEDO, cable% um% ALB_TILE, &
          cable% um% LAND_ALB )
print *, "jhan:tile_albedo:POST _rad_driver"
   endif  
endif

IF (lhook) CALL dr_hook('TILE_ALBEDO',zhook_out,zhook_handle)
RETURN
END SUBROUTINE tile_albedo
