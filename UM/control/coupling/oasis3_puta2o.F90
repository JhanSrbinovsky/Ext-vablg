#if defined(OASIS3)
! *****************************COPYRIGHT****************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt which you should
! have received as part of this distribution.
! *****************************COPYRIGHT****************************************
SUBROUTINE oasis3_puta2o(                                         &
#include "argd1.h"
#include "arg_atm_fields.h"
  put_step)

! needed by typ_atm_fields.h:
  USE atm_fields_bounds_mod

  USE water_constants_mod, ONLY: lc, lf
  USE oasis_atm_data_mod
  USE ancil_info, ONLY: nsmax
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim
  USE UM_ParVars
  USE Control_Max_Sizes
  USE Decomp_DB
  USE domain_params
  USE lbc_mod
  USE um_types 
  USE um_input_control_mod,  ONLY: l_couple_master, l_oasis_icecalve
  IMPLICIT NONE


  !
  ! Description:
  ! Put data from atmosphere to be got by the Nemo or other Ocean model.
  !  
  ! Code Owner: See Unified Model Code Owners HTML page
  ! This file belongs in section: Coupling
  ! 
  !==================================================================


#include "atm_lsm.h"
#include "typsize.h"
#include "typd1.h"
#include "typptra.h"
#include "typcona.h"
#include "typlndm.h"

#include "c_mdi.h"
#include "ctracera.h"
#include "typ_atm_fields.h"

  !     Subroutine arguments
  LOGICAL :: put_step       ! Proper data is to be put

  !     Local variables
  ! Some of the following constants really should
  ! come from somewhere central rather than being defined here 
  REAL :: latentHeatOfCond=lc
  REAL :: latentHeatOfFus=lf

  INTEGER (KIND=integer64) :: i              ! Loop counter 
  INTEGER (KIND=integer64) :: j              ! Loop counter 
  INTEGER (KIND=integer64) :: k              ! Ice category counter 
  INTEGER (KIND=integer64) :: tc              ! Loop counter 
  INTEGER (KIND=integer32) :: oasis_info     ! OASIS 32 bit return code 
  INTEGER (KIND=integer64) :: oasis_jmt_this ! OASIS jmt size by grid cell type
  INTEGER (KIND=integer64) :: ft             ! Field type 
  INTEGER (KIND=integer64) :: s_cols, s_rows ! Send buffer sizes 
 
  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  ! Perform necessary processing as described, for example,
  ! in 'Operations 1' on  HadGEM3 coupling diagram. (See HadGEM3 
  ! coupling documentation)

  IF (lhook) CALL dr_hook('OASIS3_PUTA2O',zhook_in,zhook_handle)

  IF (put_step) THEN
    ! We set up coupling data based on prognostic contents,
    ! Prepare fields for putting to the coupler
    ! by copying to our temporary arrays.

    IF (l_bluerad) THEN
      DO j=1,oasis_jmt
        DO i=1,oasis_imt
          ! Blue band radiation
          blue2d(i,j)=c_blue(i,j)
        END DO
      END DO
    END IF

    IF (l_evap2d) THEN
      DO j=1,oasis_jmt
        DO i=1,oasis_imt
          ! Evap
          evap2d(i,j)=c_evap(i,j)
        END DO
      END DO
    END IF

    IF (l_heatflux) THEN
      ! Note here the dependence of heatflux on evap2d and solar2d
      DO j=1,oasis_jmt
        DO i=1,oasis_imt
          ! Copy various heat flux components
          solar2d(i,j)=c_solar(i,j)
          longwave2d(i,j)=c_longwave(i,j)
          sensible2d(i,j)=c_sensible(i,j)

          IF (fland_loc(I,J) == 1.0) THEN
            heatflux(i,j)=0.0
          ELSE
             heatflux(i,j)=longwave2d(i,j)                       &
                 -(sensible2d(i,j)+latentHeatOfCond*evap2d(i,j))
             IF (l_bluerad) THEN
               heatflux(i,j)=solar2d(i,j)+heatflux(i,j)
             END IF
          END IF
        END DO
      END DO
    END IF

    IF (l_w10) THEN
      DO j=1,oasis_jmt
        DO i=1,oasis_imt
          ! 10m wind
          w10(i,j) = c_w10(i,j)
        END DO
      END DO
    END IF

    IF (l_train) THEN
      DO j=1,oasis_jmt
        DO i=1,oasis_imt
          rainls(i,j) = c_lsrain(i,j)
          rainconv(i,j) = c_cvrain(i,j)
          IF (fland_loc(I,J) == 1.0) THEN
            totalrain(i,j)=0.0
          ELSE
            totalrain(i,j)=rainls(i,j)+rainconv(i,j)
          END IF
        END DO
      END DO
    END IF

    IF (l_tsnow) THEN
      DO j=1,oasis_jmt
        DO i=1,oasis_imt
          snowls(i,j) = c_lssnow(i,j)
          snowconv(i,j) = c_cvsnow(i,j)
          IF (fland_loc(I,J) == 1.0) THEN
            totalsnow(i,j)=0.0
          ELSE
            totalsnow(i,j)=snowls(i,j)+snowconv(i,j)
          END IF
        END DO
      END DO
    END IF

    IF (l_runoff) THEN
      IF (l_oasis_icecalve) THEN
        DO j=1,oasis_jmt
          DO i=1,oasis_imt
            ! River runoff
            riverout(i,j) = c_riverout(i,j)
            calving(i,j)= c_calving(i,j)
            ! Riverout needs scaling to take take account of 
            ! coastal tiling points
            ! Iceberg calving field also needs scaling
            IF (fland_loc(i,j) /= 1.0) THEN
              riverout(i,j)=riverout(i,j)/(1.0-fland_loc(i,j))
              calving(i,j)=calving(i,j)/(1.0-fland_loc(i,j))
              ! Add calving into runoff for the moment
              riverout(i,j)=riverout(i,j)+calving(i,j)
              ! Also add latent heat flux term
              heatflux(i,j)=heatflux(i,j)-latentHeatOfFus*calving(i,j)
            END IF
          END DO
        END DO
      ELSE
        DO j=1,oasis_jmt
          DO i=1,oasis_imt
            ! River runoff
            riverout(i,j) = c_riverout(i,j)
            ! Riverout needs scaling to take take account of 
            ! coastal tiling points
            IF (fland_loc(i,j) /= 1.0) THEN
              riverout(i,j)=riverout(i,j)/(1.0-fland_loc(i,j))
            END IF
          END DO
        END DO
      END IF
    END IF

    IF (l_sublim(1)) THEN
      ! Sublimation (single or multi-category)
      DO k=1,nice_use
        DO j=1,oasis_jmt
          DO i=1,oasis_imt
            sublim(i,j,k) = c_sublim(i,j,k)
          END DO
        END DO
      END DO
    END IF

    IF (l_topmeltn(1)) THEN
      ! Topmelt (category)
      DO k=1,nice
        DO j=1,oasis_jmt
          DO i=1,oasis_imt
            topmeltn(i,j,k) = c_topmeltn(i,j,k)
          END DO
        END DO
      END DO
    END IF

    IF (l_fcondtopn(1)) THEN
      ! Fcondtop (category)
      DO k=1,nice
        DO j=1,oasis_jmt
          DO i=1,oasis_imt
            fcondtopn(i,j,k) = c_fcondtopn(i,j,k)
          END DO
        END DO
      END DO
    END IF

    IF (l_taux) THEN
      ! Set up fields from the U grid
      DO j=1,oasis_jmt
        DO i=1,oasis_imt
          taux(i,j)=c_taux(i,j)
        END DO
      END DO
    END IF

    IF (l_tauy) THEN
      ! Set up fields from the V grid
      DO j=1,oasis_jmt_v
        DO i=1,oasis_imt
          tauy(i,j)=c_tauy(i,j)
        END DO
      END DO
    END IF

    ! Currently we have to observe very strict call sequences - i.e.
    ! the sequence of puts and fields referred to must be in the same
    ! order as the sequence of gets in the receiving component.
    ! If it isn't, OASIS3 will usually just hang without issuing any warnings!
    !
    ! Clearly this is not a flexible long term FLUME option since it needs
    ! each component to "know" what sequence of puts/gets to use.  
    ! In the long term we might use the non SEQMODE option for better
    ! flexibility and FLUMEification. 
    !===============================================================

    DO tc = 1, max_transients

      IF (transient_out(tc)%indx > 0) THEN

        ! This is an expexted outgoing transient, what grid type is it on
        IF (transient_out(tc)%grid == "V") THEN
          ft = fld_type_v
          oasis_jmt_this = oasis_jmt_v
        ELSE
          IF (transient_out(tc)%grid == "U") THEN
            ft = fld_type_u
            oasis_jmt_this = oasis_jmt_u
          ELSE
            ft = fld_type_p
            oasis_jmt_this = oasis_jmt
          END IF
        END IF

        IF (l_couple_master) THEN
          s_cols = glsize(1,ft)
          s_rows = glsize(2,ft)
        ELSE
          s_cols = lasize(1,ft,halo_type_no_halo)
          s_rows = lasize(2,ft,halo_type_no_halo)
        END IF


        ! DEPENDS ON: oasis3_put
        CALL oasis3_put(transient_out(tc)%field                            &
                        ,transient_out(tc)%oasis_id                        & 
                        ,oasis_imt,oasis_jmt_this                          &
                        ,transient_out(tc)%oasis_info                      &
                        ,ft                                                &
                        ,s_cols,s_rows,put_step                            &
                        )

        ! We don't actually check the return code because we can't  
        ! really do anything about it at this stage. If it has failed, 
        ! then the receiving component will handle things 
        ! in a more informative way.

      END IF

    END DO  ! For each potential transient

  END IF  ! put_step=true

  IF (lhook) CALL dr_hook('OASIS3_PUTA2O',zhook_out,zhook_handle)
  RETURN

END SUBROUTINE oasis3_puta2o
#endif
