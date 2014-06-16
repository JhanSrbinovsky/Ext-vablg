! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine: JOBCTL ------------------------------------------------
!
!  Purpose: Outputs job release request to output processing server
!           task at selected timesteps.
!
!  External documentation:
!    On-line UM document C1 - The top-level control system
!    On-line UM document Y1 - Automated output processing system
!
!Code Owner: See Unified Model Code Owners HTML page
!This file belongs in section: Top Level

SUBROUTINE JOBCTL(I_AO,ICODE,CMESSAGE)

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE filenamelength_mod, ONLY :                                    & 
      filenamelength
USE MPPIO_job_control, ONLY : jobCntrl

USE PrintStatus_mod
USE UM_ParVars
USE Control_Max_Sizes
USE nlstgen_mod, ONLY: jobrel_stepim, jobrel_offsetim
USE Submodel_Mod


USE chsunits_mod, ONLY : nunits

IMPLICIT NONE

INTEGER I_AO               ! Internal model indicator
                           ! 1 - Atmosphere; 2 - Ocean
INTEGER ICODE              ! Error return code
CHARACTER(LEN=80) CMESSAGE    ! Error return message
!
!  Common blocks
!
#include "chistory.h"
#include "ccontrol.h"
#include "ctime.h"

!
!  Local variables
!

INTEGER      I                  ! Loop counter
INTEGER :: MODJOBREL            ! modulus of jobrel_step
CHARACTER(LEN=2)  CDIGIT(10)         ! Character array of digits
CHARACTER(LEN=80) CREQUEST           ! Template processing request
CHARACTER(LEN=filenamelength) :: filename

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

DATA CDIGIT  / "1 ","2 ","3 ","4 ","5 ","6 ","7 ","8 ","9 ","10" /

DATA CREQUEST  / "%%%  _JOB_MEMBER_   REL NET" /

IF (lhook) CALL dr_hook('JOBCTL',zhook_in,zhook_handle)

! 1. Set character for internal model type

IF (I_AO == 1) THEN
  CREQUEST(5:5) = "A"
ELSE IF (I_AO == 2) THEN
  CREQUEST(5:5) = "O"
ELSE
  ICODE = I_AO
  CMESSAGE = 'JOBCTL1: invalid internal model indicator'
  GO TO 9999
END IF

! 2. Job Release - loop over release timesteps

  DO I=1,10
    IF (jobrel_stepim(I,I_AO) <  0) THEN
      MODJOBREL=ABS(jobrel_stepim(I,I_AO))
      IF (MOD(STEPim(I_AO),MODJOBREL) == jobrel_offsetim) THEN
        CREQUEST(18:19) = CDIGIT(I)
        IF (mype == 0) THEN
          IF (PrintStatus >= PrStatus_Oper) THEN
            WRITE(6,*) 'JobCtl : Timestep ',STEPim(I_AO),         &
                ' Job No ',I,' released. CREQUEST : ',            &
                  CREQUEST (1:Len_Trim(CREQUEST))
          END IF
        END IF
        CALL jobCntrl(CREQUEST)
      ENDIF
    ELSE
      IF (jobrel_stepim(I,I_AO) == STEPim(I_AO)) THEN
        CREQUEST(18:19) = CDIGIT(I)
        IF (mype == 0.AND.PrintStatus >= PrStatus_Oper) THEN
          WRITE(6,*) 'JobCtl : Timestep ',STEPim(I_AO),  &
              ' Job No ',I,' released. CREQUEST : ',     &
              CREQUEST (1:Len_Trim(CREQUEST))
        END IF
        CALL jobCntrl(CREQUEST)
      ENDIF
    ENDIF
  ENDDO

9999 CONTINUE

  IF (lhook) CALL dr_hook('JOBCTL',zhook_out,zhook_handle)
  RETURN

END SUBROUTINE JOBCTL
