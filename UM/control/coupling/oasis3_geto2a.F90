#if defined(OASIS3)
! *****************************COPYRIGHT****************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt which you should
! have received as part of this distribution.
! *****************************COPYRIGHT****************************************
SUBROUTINE oasis3_geto2a(                                         &
#include "argd1.h"
#include "arg_atm_fields.h"
    get_step)


! needed by typ_atm_fields.h:
  USE atm_fields_bounds_mod

#if defined(MCT)
  ! OASIS3-MCT case
  USE mod_prism, ONLY: prism_recvout,     &
                       prism_fromrestout, &
                       prism_recvd,       &
                       prism_fromrest
#else
  ! Standard OASIS3 case
  USE mod_prism_proto, ONLY: prism_recvout, &
                       prism_fromrestout,   &
                       prism_recvd,         &
                       prism_fromrest
#endif

  USE water_constants_mod, ONLY: tfs,tm
  USE oasis_atm_data_mod
  USE ancil_info, ONLY: nsmax
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim
  USE UM_ParVars
  USE Control_Max_Sizes
  USE Decomp_DB
  USE domain_params
  USE lbc_mod
  USE UM_types
  USE c_kappai, ONLY : kappai,kappai_snow,rhosnow
  USE um_input_control_mod,  ONLY: model_domain, l_couple_master, &
                                   ltleads
  USE switches, ONLY: l_sice_multilayers, l_ctile

  IMPLICIT NONE


  !
  ! Description:
  ! Receive incoming coupling data from the ocean (NEMO) to be used
  ! to drive the atmosphere. 
  !
  !
  ! Code Owner: See Unified Model Code Owners HTML page
  ! This file belongs in section: Coupling
  !
  !=======================================================================

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
  LOGICAL :: get_step       ! Whether to transfer incoming
  ! data to UM atmos D1 or just ignore it.

  !     Local variables
  INTEGER (KIND=integer64) :: i, j, k, n, tc ! Loop counters 
  INTEGER (KIND=integer64) :: r_cols, r_rows ! Receive buffer dimensions 

  INTEGER (KIND=integer64) :: ft             ! Field type index 
  INTEGER (KIND=integer64) :: info           ! Dummy arg for GCOM calls 
 
  ! OASIS3 32 bit return codes:  
  INTEGER (KIND=integer32) :: infosst, infou, infov
  INTEGER (KIND=integer32) :: infofreezen, infosnowthickn, infohicen   
  INTEGER (KIND=integer32) :: infoicetn, infoicekn

  INTEGER (KIND=integer64) :: oasis_jmt_this   ! J dim dependent on field type

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle
  REAL(KIND=jprb)               :: aicenmin

  CHARACTER(LEN=*) :: RoutineName
  PARAMETER (RoutineName='oasis3_geto2a')

  ! Initialise potential incoming fields
  IF (lhook) CALL dr_hook('OASIS3_GETO2A',zhook_in,zhook_handle)
  ocn_sst(:,:) = 0.0
  ocn_u(:,:) = 0.0
  ocn_v(:,:) = 0.0
  ocn_freeze(:,:) = 0.0
  ocn_freezen(:,:,:) = 0.0
  ocn_hice(:,:) = 0.0
  ocn_hicen(:,:,:) = 0.0
  ocn_snowthick(:,:) = 0.0
  ocn_snowthickn(:,:,:) = 0.0
  ocn_icetn(:,:,:) = 0.0
  ocn_icet_gb(:,:) = 0.0
  ocn_icekn(:,:,:) = 0.0

  tstar_local(:,:) = 0.0
  tstar_ssi(:,:) = 0.0
  tstar_sice_local(:,:) = 0.0
  tstar_sice_cat_local(:,:,:) = 0.0
  tstar_land_local(:,:) = 0.0

  ! This saves us having to do an explicit mask because
  ! OASIS will just overlay the sea points
  IF (L_CTILE) THEN
    DO j = 1, oasis_jmt
      DO i = 1, oasis_imt
        ocn_sst(i,j)=tstar_sea(i,j)
        ocn_sst_orig(i,j)=tstar_sea(i,j)
      END DO
    END DO
  ELSE
    DO j = 1, oasis_jmt
      DO i = 1, oasis_imt
        ocn_sst(i,j)=tstar(i,j)
        ocn_sst_orig(i,j)=tstar(i,j)
      END DO
    END DO
  END IF


  DO j = 1, oasis_jmt
    DO i = 1, oasis_imt
      tstar_sice_local(i,j)=tstar_sice(i,j,1)
      tstar_land_local(i,j)=tstar_land(i,j)
    END DO
  END DO

  IF (nice_use .GT. 1) THEN
    DO k = 1, nice_use
      DO j = 1, oasis_jmt
        DO i = 1, oasis_imt
          tstar_sice_cat_local(i,j,k)=tstar_sice_cat(i,j,k)
        END DO
      END DO
    END DO
  ELSE    ! TSTAR_SICE_CAT doesn't exist
    DO j = 1, oasis_jmt
      DO i = 1, oasis_imt
        tstar_sice_cat_local(i,j,1)=tstar_sice(i,j,1)
      END DO
    END DO
  END IF

  DO k=1,nice
    DO j = 1, oasis_jmt
      DO i = 1, oasis_imt
        ocn_freezen(i,j,k)=ice_fract_cat(i,j,k)
        ocn_hicen(i,j,k)=ice_thick_cat(i,j,k)
        ocn_snowthickn(i,j,k)=snodep_sea_cat(i,j,k)
      END DO
    END DO
  END DO

  IF (l_sice_multilayers) then
    DO k=1,nice
      DO j = 1, oasis_jmt
        DO i = 1, oasis_imt
          ocn_icetn(i,j,k)=ti_cat(i,j,k)
          ocn_icekn(i,j,k)=ice_k_cat(i,j,k)
        END DO
      END DO
    END DO
  END IF

  ! If this is a timestep when we are genuinely expecting to 
  ! recieve incoming fields, then check each potential field. 
  IF (get_step) THEN

    DO tc = 1, max_transients

      ! It is usually important to check fields in the right seuquece. 
      ! For a "few" fields, we can get away with a random order, but 
      ! but usually for the numbers of fields involved in realistic models
      ! the MPI buffers will clog up and deadlock us.   
      IF (transient_in(tc)%indx > 0) THEN

          ! This is an expexted incoming transient, what grid type is it on
          IF (transient_in(tc)%grid == "V") THEN
             ft = fld_type_v
             oasis_jmt_this = oasis_jmt_v
          ELSE
             IF (transient_in(tc)%grid == "U") THEN
                ft = fld_type_u
                oasis_jmt_this = oasis_jmt_u
             ELSE
                ft = fld_type_p
                oasis_jmt_this = oasis_jmt
             END IF
          END IF

          IF (l_couple_master) THEN
             r_cols = glsize(1,ft)
             r_rows = glsize(2,ft)
          ELSE
             r_cols = oasis_imt        !lasize(1,ft,halo_type_no_halo)
             r_rows = oasis_jmt_this   !lasize(2,ft,halo_type_no_halo)
          END IF

          ! DEPENDS ON: oasis3_get
          CALL oasis3_get(transient_in(tc)%field                           &
                         ,transient_in(tc)%oasis_id                        &
                         ,oasis_imt,oasis_jmt_this                         &
                         ,transient_in(tc)%oasis_info,ft                   &
                         ,r_cols,r_rows,get_step                           &
                         )

          IF (l_couple_master) THEN
             ! Ensure all PEs know the return codes under all
             ! conditions - PE 0 should definitely have them
             CALL GC_IBCAST(805,1,0,nproc,info,     &
                                      transient_in(tc)%oasis_info  ) 

          END IF

          IF (transient_in(tc)%oasis_info == PRISM_Recvd.OR.               &
              transient_in(tc)%oasis_info == PRISM_FromRest.OR.            &
              transient_in(tc)%oasis_info == PRISM_RecvOut.OR.             &
              transient_in(tc)%oasis_info == PRISM_FromRestOut) THEN

             ! If this PE deals with the North polar row we may need
             ! to set a uniform value in all grid points along that
             ! row to prevent crashes in the atmos dynamics.
             ! That's all jolly well in a 1xn decomposition, but a
             ! nx1 or nxm composition means we have to do some gathering
             ! and scattering.
             ! There shouldn't be any implications for conservation.
             ! Note: MEAN_POLAR_ROW must be called for all fields which
             ! are defined at 90 degrees North (or South! but currently 
             ! there aren't any of these.)

             ! This is a special adjustment to get sensible
             ! numbers in the polar row. One might regard this as a fix
             ! to make up for shortcomings in OASIS3 remapping where
             ! it fails to deal with the pole as a singularity.

             ! If our polar row is not a V row, then we need to 
             ! ensure all T fields are consistent at the singularity
             IF (transient_in(tc)%polar_mean) THEN

               DO i = 1, oasis_imt
                 transient_in(tc)%field(i,oasis_jmt) =          &
                               transient_in(tc)%field(i,oasis_jmt-1)
               END DO

               ! DEPENDS ON: mean_polar_row
               CALL MEAN_POLAR_ROW(transient_in(tc)%field)
             END IF

             ! Check if we need some standard processing on ice fields
             ! to impose meaningful minima. 
             IF (transient_in(tc)%ice_min) THEN

               DO j = 1, oasis_jmt
                 DO i = 1, oasis_imt
                   transient_in(tc)%field(i,j) =      &
                              MAX(0.0,transient_in(tc)%field(i,j))
                 END DO
               END DO

             END IF


             IF (transient_in(tc)%field_id == vind_ocn_sst) &
                              infosst =  transient_in(tc)%oasis_info

             IF (transient_in(tc)%field_id == vind_ocn_freezen(1)) &
                              infofreezen =  transient_in(tc)%oasis_info

             IF (transient_in(tc)%field_id == vind_ocn_hicen(1)) &
                              infohicen =  transient_in(tc)%oasis_info

             IF (transient_in(tc)%field_id == vind_ocn_snowthickn(1)) &
                              infosnowthickn =  transient_in(tc)%oasis_info

             IF (transient_in(tc)%field_id == vind_ocn_u) &
                              infou =  transient_in(tc)%oasis_info

             IF (transient_in(tc)%field_id == vind_ocn_v) &
                              infov =  transient_in(tc)%oasis_info

       END IF ! Info indicates data received

      END IF ! Transient in use

    END DO ! Over tc 


    ! Perform necessary processing as described, for example,
    ! in 'Operations 5' on  HadGEM3 coupling diagram. (See HadGEM3 
    ! coupling documentation)

    IF ((infofreezen == infosst).AND.          &
        (infohicen == infosst).AND.            &
        (infosnowthickn == infosst).AND.       &
        (infosst == PRISM_Recvd.OR.            &
         infosst == PRISM_FromRest.OR.         &
         infosst == PRISM_RecvOut.OR.          &
         infosst == PRISM_FromRestOut)) THEN


       DO k=1,nice
        DO j = 1, oasis_jmt
          DO i = 1, oasis_imt

            IF (l_sice_multilayers) THEN
              ! Ensure ice temperature (in K) and effective conductivity are 
              ! always positive.
 
              ! Ensure consistency between location of ice and ice temperature
              ! and conductivity fields

              IF (ocn_freezen(i,j,k) >  0.0 .AND.                  & 
                (ocn_icetn(i,j,k) == 0.0 .OR. ocn_icekn(i,j,k) == 0.0))THEN
                WRITE(6,'(A)') 'PROBLEM IN OASIS3_GETO2O'
                WRITE(6,'(A)') 'Missing ice T or K data'
                WRITE(6,'(A,1X,I4,1X,I4,1X,I4)') 'i,j,k=',i,j,k
                WRITE(6,'(A,1X,I4,1X,I4,1X,I4)')                       &
                  'ocn_freezen,ocn_icetn,ocn_icekn=',                  &
                  ocn_freezen(i,j,k),ocn_icetn(i,j,k),ocn_icekn(i,j,k)
                !!!! Remove ice for now 
                ocn_freezen(i,j,k) = 0.0 ! other fields will be zeroed below
              END IF

            END IF

            ! Here we ensure that regardless of what has happened to the ice 
            ! fraction and GBM ice depth during the coupling,
            ! the minimum local ice depth is 1cm (as in CICE). This is done by
            ! squashing the ice to a smaller area whilst conserving volume (so
            ! the GBM ice depth is unchanged).
            !
            ! Note that freezen is unchanged unless the local ice depth (i.e.
            ! hicen/freezen is less than 1.0e-02, in which case freezen is
            ! decreased so that hicen/freezen=1.0e-02

! If using multilayers....
! **CHANGE the way min local ice depth is enforced and increase min ice depth
! to 0.1m.  Get rid of ice below that value - do not squash 
! **INCREASE min ice concentration

            IF (l_sice_multilayers) THEN            
              IF (ocn_freezen(i,j,k) > 0.0) THEN
                IF (ocn_hicen(i,j,k)/ocn_freezen(i,j,k) < 1.0e-01) THEN
                   ocn_freezen(i,j,k) = 0.0  ! other fields will be zeroed next
                END IF
              END IF
              aicenmin=1.0e-02
            ELSE  ! old method
              ocn_freezen(i,j,k)=MIN(ocn_freezen(i,j,k),            &
               (ocn_hicen(i,j,k)/1.0e-02))
              aicenmin=2.0e-04
            END IF
            
            ! Also impose a minimum ice fraction
            IF (ocn_freezen(i,j,k) < aicenmin) THEN

              ocn_freezen(i,j,k)=0.0
              ocn_hicen(i,j,k)=0.0
              ocn_snowthickn(i,j,k)=0.0
              IF (l_sice_multilayers) THEN
                ocn_icetn(i,j,k)=0.0
                ocn_icekn(i,j,k)=0.0
              END IF
            END IF

            ocn_freeze(i,j)=ocn_freeze(i,j)+ocn_freezen(i,j,k)
 
         END DO
        END DO
      END DO

      DO K=1,nice
        DO J = 1, oasis_jmt
          DO I = 1, oasis_imt

            ! Also reduce category ice fractions if aggregate > 1
            IF (ocn_freeze(I,J) > 1.0)  THEN
              ocn_freezen(I,J,K)=ocn_freezen(I,J,K)/ocn_freeze(I,J)
            END IF

            ocn_hicen(i,j,k)=ocn_hicen(i,j,k)+                    &
               ocn_snowthickn(i,j,k)*(kappai/kappai_snow)
            ocn_hice(i,j)=ocn_hice(i,j)+ocn_hicen(i,j,k)
            ocn_snowthick(i,j)=ocn_snowthick(i,j)+                &
               ocn_snowthickn(i,j,k)

            IF (ocn_freezen(i,j,k) >  0.0) THEN
              ocn_hicen(i,j,k)=ocn_hicen(i,j,k)/                 &
                 ocn_freezen(i,j,k)
              ocn_snowthickn(i,j,k)=ocn_snowthickn(i,j,k)*       &
                 rhosnow/ocn_freezen(i,j,k)

            END IF

            IF (l_sice_multilayers) THEN
              ! Ice T and K fields are passed as GBM (i.e. multiplied by
              ! category ice concentration).  Convert them back to local
              ! calues here.
              IF (ocn_freezen(i,j,k) >  0.0) THEN
                ocn_icetn(i,j,k)=ocn_icetn(i,j,k)/ocn_freezen(i,j,k)
                ocn_icekn(i,j,k)=ocn_icekn(i,j,k)/ocn_freezen(i,j,k)
              ! ELSE not needed as fields will have been zeroed above
              END IF
            END IF

          END DO
        END DO
      END DO

      DO j = 1, oasis_jmt
        DO i = 1, oasis_imt
          IF (ocn_freeze(i,j) >  0.0) THEN
            ocn_freeze(i,j)=MIN(ocn_freeze(i,j),1.0)
            ocn_snowthick(i,j)=ocn_snowthick(i,j)*                &
               rhosnow/ocn_freeze(i,j)
            ocn_hice(i,j)=ocn_hice(i,j)/ocn_freeze(i,j)
          END IF
        END DO
      END DO

      IF (l_sice_multilayers) THEN

        ! Calculate GBM icet and icek - ignoring open water
        ! Don't need to do this if nice=1 as the pointer for TI_GB and TI will
        ! be indentical and likewise for ICE_K and ICE_K_CAT
        IF (nice > 1) THEN
          DO j = 1, oasis_jmt
            DO i = 1, oasis_imt
              IF (ocn_freeze(i,j).GT.0.0) THEN 
                ocn_icet_gb(i,j) = 0.0 
                DO k = 1,nice  
                  ocn_icet_gb(i,j) = ocn_icet_gb(i,j) +                   &
                              (ocn_freezen(i,j,k)/ocn_freeze(i,j) ) *     &
                               ocn_icetn(i,j,k)
                END DO
              ELSE
                ocn_icet_gb(i,j)=ocn_icetn(i,j,1)   ! zero (open ocean) or land
              END IF
            END DO
          END DO
        END IF
      END IF
               
      ! The incoming SST is expected in K NOT degrees C
      ! Any necessary transforms will be done in the PSMILE
      ! controlled via the SMIOC, namcouple or in the sending
      ! model component.
      DO j = 1, oasis_jmt
        DO i = 1, oasis_imt

          ! Special form of masking for T points - if
          ! value returned from coupling is approx abs zero
          ! we assume this is an unchanged point masked during
          ! the coupling process and we reinstate
          ! our original value.
          IF (ocn_sst(i,j) <  1.0) THEN
            ocn_sst(i,j)=ocn_sst_orig(i,j)
          END IF
        END DO
      END DO

      ! Process surface temperature fields here (as used to be done
      ! in SWAPA20 pre UM version 7.0).

      IF (nice_use .GT. 1) THEN
        DO j = 1, oasis_jmt
          DO i = 1, oasis_imt
            IF (fland_loc(i,j) <  1.0 .AND. ocn_freeze(i,j) > 0.0 ) THEN
              tstar_sice_local(i,j) = 0.0
              DO k = 1, nice_use
                tstar_sice_cat_local(i,j,k) =                             &
                                MIN(tstar_sice_cat_local(i,j,k),TM)
                tstar_sice_local(i,j) = tstar_sice_local(i,j) +           &
                                (ocn_freezen(i,j,k)/ocn_freeze(i,j) ) *   &
                                tstar_sice_cat_local(i,j,k)
              END DO
            END IF
          END DO
        END DO
      ELSE 
        DO j = 1, oasis_jmt
          DO i = 1, oasis_imt
            IF (fland_loc(i,j) <  1.0 .AND. ocn_freeze(i,j) > 0.0 ) THEN
              tstar_sice_cat_local(i,j,1) =                             &
                                MIN(tstar_sice_cat_local(i,j,1),TM)
              tstar_sice_local(i,j) = tstar_sice_cat_local(i,j,1)
            END IF
          END DO
        END DO
      END IF

      DO j = 1, oasis_jmt
        DO i = 1, oasis_imt
          tstar_ssi(i,j)=ocn_sst(i,j)

          IF (fland_loc(i,j) <  1.0) THEN

            IF (ltleads.EQV..FALSE.) THEN
              IF(ocn_freeze(i,j) >  0.0) ocn_sst(i,j)=TFS
            END IF

            tstar_ssi(i,j)=ocn_freeze(i,j)*tstar_sice_local(i,j)+  &
               (1.0-ocn_freeze(i,j))*ocn_sst(i,j)
          END IF

          tstar_local(i,j)=fland_loc(i,j)*tstar_land_local(i,j)+    &
             (1.0-fland_loc(i,j))*tstar_ssi(i,j)
        END DO
      END DO

      ! Having got our new values we need to move them
      ! to the appropriate arrays where they'll be employed next 
      ! time the appropriate calculation is called.
      DO j = 1, oasis_jmt
        DO i = 1, oasis_imt

          IF (l_ctile) THEN
            tstar_sea(i,j)=ocn_sst(i,j)
          END IF

          tstar(i,j)=tstar_local(i,j)

          tstar_sice(i,j,1)=tstar_sice_local(i,j)
          ice_fraction(i,j,1)=ocn_freeze(i,j)
          ice_thickness(i,j,1)=ocn_hice(i,j)
          snodep_sea(i,j,1)=ocn_snowthick(i,j)
        END DO
      END DO

      IF (nice_use .GT. 1) THEN
        DO k = 1, nice_use
          DO j = 1, oasis_jmt
            DO i = 1, oasis_imt
              tstar_sice_cat(i,j,k)=tstar_sice_cat_local(i,j,k)
            END DO
          END DO
        END DO
      END IF

      DO k = 1,nice
        DO j = 1, oasis_jmt
          DO i = 1, oasis_imt
            ice_fract_cat(i,j,k)=ocn_freezen(i,j,k)
            ice_thick_cat(i,j,k)=ocn_hicen(i,j,k)
            snodep_sea_cat(i,j,k)=ocn_snowthickn(i,j,k)
          END DO
        END DO
      END DO

      IF (l_sice_multilayers) THEN
        DO k = 1,nice
          DO j = 1, oasis_jmt
            DO i = 1, oasis_imt
              ti_cat(i,j,k)=ocn_icetn(i,j,k)
              ice_k_cat(i,j,k)=ocn_icekn(i,j,k)
            END DO
          END DO
        END DO
        DO j = 1, oasis_jmt
          DO i = 1, oasis_imt
            ti(i,j,1)=ocn_icet_gb(i,j)
          END DO
        END DO
      END IF

    END IF ! Our multi cat ice field return codes were the same as our
           ! sst return code.  

    ! Update our surface currents (U,V)
    IF (( infou == PRISM_Recvd        .OR.                             &
          infou == PRISM_FromRest     .OR.                             &
          infou == PRISM_RecvOut      .OR.                             &
          infou == PRISM_FromRestOut ).AND.                            &
        ( infov == PRISM_Recvd        .OR.                             &
          infov == PRISM_FromRest     .OR.                             &
          infov == PRISM_RecvOut      .OR.                             &
          infov == PRISM_FromRestOut )) THEN
  
      ! Now we have to make sure that things are 
      ! consistent on the North polar row. (We shouldn't care
      ! about the South because there's no L-S coupling
      ! over Antarctica!) To do this, under ND, we discard
      ! our incoming N polar U values and calculate new ones based
      ! on our "top row" of V values. We need to do this because
      ! remapped values from OASIS3 over the pole are junk.
      ! Of course we only need to do this for a global model 
      ! (or at least a model which covers the N pole!) 
      ! For Endgame/VaP, positions of V and U are reversed so
      ! we must calculate V based on U. 
       
      IF (model_domain  ==  mt_Global ) THEN
    
        ! If we have more U rows than V rows then we have U points at the N pole
        ! i.e. we're using ND and consequently need to calculate U values based 
        ! on V (half a row below the pole) 
        IF (glsize(2,fld_type_u) > glsize(2,fld_type_v)) THEN

           ! ND version
! DEPENDS ON: Correct_Polar_UV
           CALL Correct_Polar_UV(                 &
                delta_lambda, global_row_length,  &
                ocn_v,ocn_u,                      &
                oasis_jmt_v,oasis_jmt_u)

        ELSE

           ! EG version
! DEPENDS ON: Correct_Polar_UV
           CALL Correct_Polar_UV(                 &
                delta_lambda, global_row_length,  &
                ocn_u,ocn_v,                      &
                oasis_jmt_u,oasis_jmt_v)

        END IF

      END IF


      ! Incoming currents expected in m/s
      DO J = 1, oasis_jmt_u
        DO I = 1, oasis_imt
          U_SEA(I,J)=ocn_u(I,J)
        END DO
      END DO

      ! Incoming currents expected in m/s
      DO j = 1, oasis_jmt_v
        DO i = 1, oasis_imt
          V_SEA(i,j)=ocn_v(i,j)
        END DO
      END DO

    END IF ! We received incoming u and v 

  END IF ! If GET_STEP = true
  IF (lhook) CALL dr_hook('OASIS3_GETO2A',zhook_out,zhook_handle)
  RETURN

END SUBROUTINE oasis3_geto2a
#endif
