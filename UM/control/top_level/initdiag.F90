! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!+ Calculate diagnostic quantities from the initial atmosphere dump
!
! Subroutine Interface:

      SUBROUTINE InitDiag(                                              &
#include "argd1.h"
#include "argduma.h"
#include "argsts.h"
#include "arg_atm_fields.h"
#include "arglndm.h"
       Dummy)

! needed by typ_atm_fields.h:
      USE atm_fields_bounds_mod
      USE trignometric_mod, Only: sin_v_latitude
      USE wet_to_dry_n_calc_mod

      USE ancil_info, only: nsmax

      USE yomhook, ONLY: lhook, dr_hook
      USE parkind1, ONLY: jprb, jpim
      USE ereport_mod, ONLY : ereport
      USE UM_ParVars
      USE Control_Max_Sizes
      USE lbc_mod
      USE Submodel_Mod
      IMPLICIT NONE

!
! Description:
!   InitDiag processes diagnostic requests from the initial atmosphere
!   dump, including both prognostic variables resident in D1 - UM
!   STASH section 0 - and fields derived from physics and dynamics
!   variables, as calculated in UM STASH sections 15 and 16.
!
! Method:
!   1. Call STASH to process diagnostics requests for sections 0,33,34
!   2. Process dynamics-derived diagnostics (section 15):
!    2a. Check validity of diagnostic requests (avoids re-checking
!        within every later call of St_diag1).
!    2b. Call St_diag1 as interface to: Dyn_diag calculations of
!        dynamics derived variables and STASH (section 15).
!   3. Process physics-derived diagnostics (section 16):
!    3a. Check validity of diagnostic requests (avoids re-checking
!        within every later call of St_diag2).
!    3b. Call St_diag2 as interface to: Phy_diag calculations of
!        physics derived variables and STASH (section 16).
!   4. Process climate diagnostics (section 30):
!    4a. Check validity of diagnostic requests (avoids re-checking
!        within every later call of St_diag3).
!    4b. Call St_diag3 as interface to: EOT_diag calculations of
!        climate variables and STASH (section 30).
!
! Code Owner: See Unified Model Code Owners HTML page
! This file belongs in section: Top Level
!
! Code Description:
!   Language: FORTRAN 77 + common extensions.
!   This code is written to UMDP3 v6 programming standards.
!
! Declarations:
!
! Global variables (*CALLed COMDECKs etc...):
#include "typsize.h"
#include "typd1.h"
#include "typduma.h"
#include "typsts.h"
#include "ctracera.h"
#include "typ_atm_fields.h"
#include "typcona.h"
#include "typlndm.h"

! Subroutine arguments
!   Scalar arguments with intent(in):
      INTEGER :: Dummy            ! Not used, needed to end arg list

!   Array  arguments with intent(in):

!   Scalar arguments with intent(InOut):

!   Array  arguments with intent(InOut):

!   Scalar arguments with intent(out):

!   Array  arguments with intent(out):

! Local parameters:
      CHARACTER(LEN=*), PARAMETER :: RoutineName='InitDiag'

! Local scalars:
!   ErrorStatus
      INTEGER :: ErrorStatus          ! Error flag (0 = OK)
      CHARACTER(LEN=256) :: CMessage  ! Error message if return code >0

      INTEGER ::    &
       im_index,    & !  Internal Model Index for stash arrays
       i,j,k          !  Loop indices

      INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
      INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
      REAL(KIND=jprb)               :: zhook_handle

! Local dynamic arrays:
      REAL, ALLOCATABLE :: t_incr_diagnostic(:,:,:)
      REAL, ALLOCATABLE :: q_incr_diagnostic(:,:,:)
      REAL, ALLOCATABLE :: qcl_incr_diagnostic(:,:,:)

! Local dynamic arrays for PC2 
      REAL, ALLOCATABLE :: t_inc_pres(:,:,:)
      REAL, ALLOCATABLE :: q_inc_pres(:,:,:)
      REAL, ALLOCATABLE :: qcl_inc_pres(:,:,:)
      REAL, ALLOCATABLE :: qcf_inc_pres(:,:,:)
      REAL, ALLOCATABLE :: cf_inc_pres(:,:,:)
      REAL, ALLOCATABLE :: cfl_inc_pres(:,:,:)
      REAL, ALLOCATABLE :: cff_inc_pres(:,:,:)
      REAL, ALLOCATABLE :: t_dini(:,:,:)
      REAL, ALLOCATABLE :: q_dini(:,:,:)
      REAL, ALLOCATABLE :: qcl_dini(:,:,:)
      REAL, ALLOCATABLE :: qcf_dini(:,:,:)
      REAL, ALLOCATABLE :: cf_dini(:,:,:)
      REAL, ALLOCATABLE :: cfl_dini(:,:,:)
      REAL, ALLOCATABLE :: cff_dini (:,:,:)

! Local dynamic arrays for section 30
      REAL :: energy_corr_now
      REAL, ALLOCATABLE :: STASHwork30(:)
      REAL, ALLOCATABLE :: inc_u(:,:,:)
      REAL, ALLOCATABLE :: inc_v(:,:,:)
      REAL, ALLOCATABLE :: inc_w(:,:,:)
      REAL, ALLOCATABLE :: inc_t(:,:,:)
      REAL, ALLOCATABLE :: inc_q(:,:,:)
      REAL, ALLOCATABLE :: inc_qcl(:,:,:)
      REAL, ALLOCATABLE :: inc_qcf(:,:,:)
      REAL, ALLOCATABLE :: inc_rho(:,:,:)
      REAL, ALLOCATABLE :: inc_qrain(:,:,:)
      REAL, ALLOCATABLE :: inc_qgraup(:,:,:)
      REAL, ALLOCATABLE :: inc_qcf2(:,:,:)

!- End of header

!   0. Initialisation

      IF (lhook) CALL dr_hook('INITDIAG',zhook_in,zhook_handle)
      ErrorStatus = 0
      Cmessage=''
      im_index = internal_model_index(atmos_im)

!----------------------------------------------------------------------
!   1. Call STASH to process diagnostics requests for section 0.

! DEPENDS ON: stash
         CALL STASH(a_sm,a_im,0,D1,                                     &
#include "argd1.h"
#include "argdumga.h"
#include "argsts.h"
          ErrorStatus,Cmessage)

! -------------------------------------------------------------------
!   1a. Call STASH to process diagnostics requests for section 33.

      IF ( SF(0,33) )                                                    &
! DEPENDS ON: stash
         CALL STASH(a_sm,a_im,33,D1,                                     &
#include "argd1.h"
#include "argdumga.h"
#include "argsts.h"
          ErrorStatus,Cmessage)

! -------------------------------------------------------------------
!   1b. Call STASH to process diagnostics requests for section 34.

      IF ( SF(0,34) )                                                    &
! DEPENDS ON: stash
         CALL STASH(a_sm,a_im,34,D1,                                     &
#include "argd1.h"
#include "argdumga.h"
#include "argsts.h"
          ErrorStatus,Cmessage)

!----------------------------------------------------------------------
!   2. Process dynamics-derived diagnostics (section 15):
!    2a. Check validity of diagnostic requests (avoids re-checking
!        within every later call of St_diag1).
!    2b. Call St_diag1 as interface to: Dyn_diag calculations of
!        dynamics derived variables and STASH (section 15).

      IF( SF(0,15) .AND. ErrorStatus == 0) THEN

! ----------------------------------------------------------------------
! Add ability to get increments from qt_bal_cld call and output in section 15
! This is only valid from atm_step; here zeroed to avoid potential errors.
! ----------------------------------------------------------------------

        IF (sf(181,15) ) THEN
          ALLOCATE ( T_incr_diagnostic(row_length,rows,model_levels) )
          DO k=1,model_levels
            DO j=1,rows
              DO i=1,row_length
                T_incr_diagnostic(i,j,k) = 0.0
              END DO ! i
            END DO ! j
          END DO ! k
        ELSE
          ALLOCATE ( T_incr_diagnostic(1,1,1) )
        END IF

        IF (sf(182,15) ) THEN
          ALLOCATE ( q_incr_diagnostic(row_length,rows,wet_levels) )
          DO k=1,wet_levels
            DO j=1,rows
              DO i=1,row_length
                q_incr_diagnostic(i,j,k) = 0.0
              END DO ! i
            END DO ! j
          END DO ! k
        ELSE
          ALLOCATE ( q_incr_diagnostic(1,1,1) )
        END IF

        IF (sf(183,15) ) THEN
          ALLOCATE ( qcl_incr_diagnostic(row_length,rows,wet_levels) )
          DO k=1,wet_levels
            DO j=1,rows
              DO i=1,row_length
                qcl_incr_diagnostic(i,j,k) = 0.0
              END DO ! i
            END DO ! j
          END DO ! k
        ELSE
          ALLOCATE ( qcl_incr_diagnostic(1,1,1) )
        END IF

! DEPENDS ON: st_diag1
        CALL St_diag1( STASH_MAXLEN(15,im_index),                       &
#include "argd1.h"
#include "argduma.h"
#include "argsts.h"
#include "arg_atm_fields.h"
#include "arglndm.h"
          t_incr_diagnostic,q_incr_diagnostic,qcl_incr_diagnostic,       &
          ErrorStatus,Cmessage)

        DEALLOCATE ( t_incr_diagnostic)
        DEALLOCATE ( q_incr_diagnostic)
        DEALLOCATE ( qcl_incr_diagnostic)

      END IF      ! Diagnostics required for this section

!----------------------------------------------------------------------
!   3. Process physics-derived diagnostics (section 16):
!    3a. Check validity of diagnostic requests (avoids re-checking
!        within every later call of St_diag2).
!    3b. Call St_diag2 as interface to: Phy_diag calculations of
!        physics derived variables and STASH (section 16).

      IF( SF(0,16) .AND. ErrorStatus == 0) THEN

        ALLOCATE(t_inc_pres  (row_length,rows,wet_levels)) 
        ALLOCATE(q_inc_pres  (row_length,rows,wet_levels)) 
        ALLOCATE(qcl_inc_pres(row_length,rows,wet_levels)) 
        ALLOCATE(qcf_inc_pres(row_length,rows,wet_levels)) 
        ALLOCATE(cf_inc_pres (row_length,rows,wet_levels)) 
        ALLOCATE(cfl_inc_pres(row_length,rows,wet_levels)) 
        ALLOCATE(cff_inc_pres(row_length,rows,wet_levels)) 
        ALLOCATE(t_dini      (row_length,rows,wet_levels)) 
        ALLOCATE(q_dini      (row_length,rows,wet_levels)) 
        ALLOCATE(qcl_dini    (row_length,rows,wet_levels)) 
        ALLOCATE(qcf_dini    (row_length,rows,wet_levels)) 
        ALLOCATE(cf_dini     (row_length,rows,wet_levels)) 
        ALLOCATE(cfl_dini    (row_length,rows,wet_levels)) 
        ALLOCATE(cff_dini    (row_length,rows,wet_levels)) 

! DEPENDS ON: st_diag2
        CALL St_diag2( STASH_MAXLEN(16,im_index),                       &
#include "argd1.h"
#include "argduma.h"
#include "argsts.h"
#include "arg_atm_fields.h"
#include "arglndm.h"
       t_inc_pres, q_inc_pres, qcl_inc_pres, qcf_inc_pres,              &  
       cf_inc_pres, cfl_inc_pres, cff_inc_pres,                         &  
       t_dini, q_dini, qcl_dini, qcf_dini,                              &  
       cf_dini, cfl_dini, cff_dini,                                     & 
          ErrorStatus,Cmessage)

        DEALLOCATE(t_inc_pres) 
        DEALLOCATE(q_inc_pres) 
        DEALLOCATE(qcl_inc_pres) 
        DEALLOCATE(qcf_inc_pres) 
        DEALLOCATE(cf_inc_pres) 
        DEALLOCATE(cfl_inc_pres) 
        DEALLOCATE(cff_inc_pres) 
        DEALLOCATE(t_dini) 
        DEALLOCATE(q_dini) 
        DEALLOCATE(qcl_dini) 
        DEALLOCATE(qcf_dini) 
        DEALLOCATE(cf_dini) 
        DEALLOCATE(cfl_dini) 
        DEALLOCATE(cff_dini) 

      END IF      ! Diagnostics required for this section

!----------------------------------------------------------------------
!   4. Process climate diagnostics (section 30):
!    4a. Check validity of diagnostic requests (avoids re-checking
!        within every later call of St_diag3).
!    4b. Call St_diag3 as interface to: EOT_diag calculations of
!        climate variables and STASH (section 30).

      IF( SF(0,30) .AND. ErrorStatus == 0) THEN

        energy_corr_now=0.0
        ALLOCATE(inc_u(udims_s%i_start:udims_s%i_end,                  &
                       udims_s%j_start:udims_s%j_end,                  &
                       udims_s%k_start:udims_s%k_end))
        ALLOCATE(inc_v(vdims_s%i_start:vdims_s%i_end,                  &
                       vdims_s%j_start:vdims_s%j_end,                  &
                       vdims_s%k_start:vdims_s%k_end))
        ALLOCATE(inc_w(wdims%i_start:wdims%i_end,                      &
                       wdims%j_start:wdims%j_end,                      &
                       wdims%k_start:wdims%k_end))
        ALLOCATE(inc_t(tdims_s%i_start:tdims_s%i_end,                  &
                       tdims_s%j_start:tdims_s%j_end,                  &
                       tdims_s%k_start:tdims_s%k_end))
        ALLOCATE(inc_q(qdims_l%i_start:qdims_l%i_end,                  &
                       qdims_l%j_start:qdims_l%j_end,                  &
                       1:qdims_l%k_end))
        ALLOCATE(inc_qcl(qdims_l%i_start:qdims_l%i_end,                &
                         qdims_l%j_start:qdims_l%j_end,                &
                         1:qdims_l%k_end))
        ALLOCATE(inc_qcf(qdims_l%i_start:qdims_l%i_end,                &
                         qdims_l%j_start:qdims_l%j_end,                &
                         1:qdims_l%k_end))
        ALLOCATE(inc_qrain(qdims_l%i_start:qdims_l%i_end,              &
                           qdims_l%j_start:qdims_l%j_end,              &
                           1:qdims_l%k_end))
        ALLOCATE(inc_qgraup(qdims_l%i_start:qdims_l%i_end,             &
                            qdims_l%j_start:qdims_l%j_end,             &
                            1:qdims_l%k_end))
        ALLOCATE(inc_qcf2(qdims_l%i_start:qdims_l%i_end,               &
                          qdims_l%j_start:qdims_l%j_end,               &
                          1:qdims_l%k_end))
        ALLOCATE(inc_rho(pdims_s%i_start:pdims_s%i_end,                &
                         pdims_s%j_start:pdims_s%j_end,                &
                         pdims_s%k_start:pdims_s%k_end))

        ! Initialise arrays as they will end up with rubbish in them otherwise
        inc_u(:,:,:)      = 0.0
        inc_v(:,:,:)      = 0.0
        inc_w(:,:,:)      = 0.0
        inc_t(:,:,:)      = 0.0
        inc_q(:,:,:)      = 0.0
        inc_qcl(:,:,:)    = 0.0
        inc_qcf(:,:,:)    = 0.0
        inc_rho(:,:,:)    = 0.0
        inc_qrain(:,:,:)  = 0.0
        inc_qgraup(:,:,:) = 0.0
        inc_qcf2(:,:,:)   = 0.0

        ! Calculate wet_to_dry_n for total column integrals
        CALL wet_to_dry_n_calc(q,qcl,qcf,qcf2,qrain,qgraup)

! size of diagnostic space
        ALLOCATE (STASHwork30(STASH_maxlen(30,A_im)))
! DEPENDS ON: st_diag3
        CALL St_diag3(STASHwork30,STASH_MAXLEN(30,im_index),            &
#include "argd1.h"
#include "argduma.h"
#include "argsts.h"
#include "arg_atm_fields.h"
          energy_corr_now,                                               &
          inc_u, inc_v, inc_w, inc_t,                                    &
          inc_q, inc_qcl, inc_qcf,                                       &
          inc_rho,sin_v_latitude,                                        &
          inc_qrain, inc_qgraup, inc_qcf2,                               &
          wet_to_dry_n,                                                  &
          ErrorStatus,Cmessage)

        DEALLOCATE(inc_u)
        DEALLOCATE(inc_v)
        DEALLOCATE(inc_w)
        DEALLOCATE(inc_t)
        DEALLOCATE(inc_q)
        DEALLOCATE(inc_qcl)
        DEALLOCATE(inc_qcf)
        DEALLOCATE(inc_rho)
        DEALLOCATE(inc_qrain)
        DEALLOCATE(inc_qgraup)
        DEALLOCATE(inc_qcf2)
        
        CALL destroy_wet_to_dry_n()

      END IF      ! Diagnostics required for this section

!----------------------------------------------------------------------

! Check error condition
      IF(ErrorStatus >  0) THEN

         CALL Ereport(RoutineName,ErrorStatus,Cmessage)
      END IF

      IF (lhook) CALL dr_hook('INITDIAG',zhook_out,zhook_handle)
      RETURN
      END SUBROUTINE InitDiag
