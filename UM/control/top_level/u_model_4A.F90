! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!LL  Subroutine: U_MODEL_4A (ENDGAME VERSION) -------------------------
!LL
!LL  Purpose: High level control program for the Unified Model
!LL           (master routine).  Calls lower level control routines
!LL           according to top level switch settings. Called by
!LL           top level routine UMSHELL which provides dimension sizes
!LL           for dynamic allocation of data arrays.
!LL
!LL  Programming standard: UM Doc Paper 3, version 8.3
!LL
!LL  External documentation: On-line UM document C0 - The top-level
!LL                          control system
!LL
!LL  -------------------------------------------------------------------
!
!    Code Owner: See Unified Model Code Owners HTML page
!    This file belongs in section: Top Level

      SUBROUTINE U_MODEL_4A(                                            &
     &       NFT,NFTU,                                                  &
#include "argszsp.h"
#include "argszspa.h"
#include "argszspc.h"
     ) 

! D1 replacement module

use atm_fields_mod
use earth_constants_mod
use atm_fields_bounds_mod
use integrity_mod

! OASIS Modules
USE oasis_atm_data_mod
  

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE IO, ONLY             : io_timestep
USE model_file, ONLY : storeAllLookups, SynchroniseAll
USE um_types
USE filenamelength_mod, ONLY : filenamelength

! ensure that module variables cannot go out of scope
USE horiz_grid_mod
USE ref_pro_mod
USE departure_pts_mod
USE fields_rhs_mod
USE metric_terms_mod
USE helmholtz_const_matrix_mod
USE coriolis_mod
USE gravity_mod
USE eg_parameters_mod
USE wet_to_dry_n_calc_mod, ONLY : wet_to_dry_n

USE Control_Max_Sizes
USE decomp_DB
USE UM_ParVars
USE ereport_mod, ONLY : ereport
USE PrintStatus_mod

! For managing dump deletion
USE MPPIO_job_control,        ONLY : jobcntrl     
USE MPPIO_job_control_common, ONLY : jc_delete

USE um_input_control_mod,  ONLY:                   &
     l_oasis,                                      &
     oasis_couple_freq
USE river_inputs_mod, ONLY: l_rivers
USE ppxlook_mod
USE acp_namel_mod, ONLY: l_ac
USE ukca_option_mod, ONLY: l_ukca

USE Submodel_Mod
USE nlstgen_mod, ONLY: steps_per_periodim, secs_per_periodim


USE nlstcall_mod, ONLY : model_basis_time, &
                         LPP, &
                         ldump, &
                         lmean, &
                         lprint, &
                         linterface, &
                         lexit, &
                         ljobrelease, &
                         lancillary, &
                         lboundary, &
                         ltimer, &
                         ft_select

  USE chsunits_mod, ONLY : nunits

  use cable_data_mod, ONLY : set_endstep_umodel

IMPLICIT NONE

!*L  Interface and arguments: ------------------------------------------
!L       Sizes of super arrays
#include "typszsp.h"
#include "typszspa.h"
#include "typszspc.h"
!L
!L       Model sizes
#include "typsize.h"
!L
!L       Addresses of component arrays within super arrays
#include "spindex.h"
!L
!*----------------------------------------------------------------------
!
!  Common blocks
!
#include "chistory.h"
#include "ccontrol.h"
#include "ctime.h"
#include "cintfa.h"
#include "c_global.h"
!L
! Data kind parameters
!
!L
!L  DYNAMIC ALLOCATION OF SUPER ARRAYS:
!L
!L       Main D1 data array
#include "typspd1.h"
#if defined(NECSX6)
!dir$ cache_align spd1
#endif
!L
!L       STASH related arrays
#include "typspst.h"
!L
!L       Dump headers and lookups
#include "typspdua.h"
!L
!L       Pointers (addresses) of model variables and constants
#include "typsppta.h"
!L Maximum sizes of fields limited by User Interface
!L
!L       Model derived constants arrays
#include "typspcoa.h"
!L
!L       Generation of output interface fields
#include "typspina.h"
!L
!L       Updating of model from ancillary files
#include "typspana.h"
!L
!L       Boundary updating for Limited Area Models
#include "typspbo.h"
#include "typspboa.h"
!L
!L       Coupled model arrays (atmosphere-river routing)
#include "typspcpl.h"
!L
!  Sizes for allocation of AC scheme arrays
#include "csizeobs.h"
      INTEGER :: obs_flag_len,obs_len
      INTEGER, ALLOCATABLE, DIMENSION(:) :: OBS_FLAG
      REAL,    ALLOCATABLE, DIMENSION(:) :: OBS
!
!  Local variables
!
      INTEGER internal_model    ! Work - Internal model identifier
      INTEGER internal_model_prev!Work - Previous internal model ident
      INTEGER submodel          ! Work - Submodel id for dump partition
      INTEGER submodel_prev     ! Work - Previous submodel dump id
      INTEGER NGROUP            ! Work - Number of steps in "group"
      INTEGER MEANLEV           ! Work - Mean level indicator
      INTEGER IABORT            ! Work - Internal return code
      INTEGER I_STEP            ! Work - Loop counter over timesteps
      INTEGER G_THETA_FIELD_SIZE                                        &
                                   ! Sizes for MPP dynamic allocation
     &       ,G_IMTJMT          ! in A-O coupling routines

! Number of dust bins in LBC generation, disabled if not using MakeBC 
      INTEGER :: ndustbin_in, ndustbin_out  
!
! River routing
      INTEGER G_RIVER_FIELD_SIZE   ! Sizes for MPP dynamic allocation
!
      LOGICAL lexitNOW          ! Work - Immediate exit indicator
      INTEGER NFT           ! Unit no. for standard STASHmaster files
      INTEGER NFTU          ! Do. user STASH files (for GET_FILE)
      INTEGER RowNumber     ! Row no. counter for PPXI, PPXC arrays
      INTEGER I,J,K,IDX     ! Loop counters
      INTEGER CO2_DIMA,                                                 &
                                   ! CO2 array dimensions
     &        CO2_DIMO,                                                 &
     &        CO2_DIMO2
      INTEGER DMS_DIMA,                                                 &
                                   ! DMS array dimensions
     &        DMS_DIMO,                                                 &
     &        DMS_DIMO2
      Integer info   ! Return code from GCom routines

      integer len_runid       !  No of chars in RUNID

      CHARACTER(LEN=4) runtype_char  !  Run Type (ie. NRUN, CRUN)

! 3-D fields of species to be passed down to radiation
      INTEGER, PARAMETER :: ngrgas = 8
      INTEGER, SAVE :: grgas_addr(ngrgas)
      
      LOGICAL :: put_step ! True when we're going to do a put
                      ! for OASIS purposes, false if we use dummy data.
      LOGICAL :: get_step ! True if we need to get data from OASIS, false
                      ! if we perform a get but discard results.
      LOGICAL :: cpl_update_step ! True when we need to make sure
                             ! the D1 prognostic data is updated
                             ! in preparation for dump creation or
                             ! coupling actions.


#if defined(IBM)
      INTEGER                      :: POS               ! string position
      INTEGER                      :: ICODE_ENV         ! error code
      CHARACTER (LEN=256)          :: C_XLFRTEOPTS      ! XLFRTEOPTS content
      CHARACTER (LEN=*), PARAMETER :: C_DISABLE='buffering=disable_all'
#endif
! Error reporting
      INTEGER       ICODE       ! =0 normal exit; >0 error exit
      CHARACTER(LEN=256) Cmessage    ! Error message
      CHARACTER(LEN=*) RoutineName

      INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
      INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
      REAL(KIND=jprb)               :: zhook_handle
      PARAMETER (   RoutineName='U_MODEL_4A')

! ENDGAME-only declarations
#include "ctracera.h"

!
! ENDGame prognostic variables (not included in the start dump)
!



      REAL, parameter ::   inv_r_squared = 1. / Earth_Radius**2
! End of ENDGAME-only declarations

      IF (lhook) CALL dr_hook('U_MODEL_4A',zhook_in,zhook_handle)


      ALLOCATE(PPXI(ppxRecs,PPXREF_CODELEN))
      ALLOCATE(PPXPTR                                                    &
     & (N_INTERNAL_MODEL    ,0:PPXREF_SECTIONS ,PPXREF_ITEMS))


      ICODE=0
      CMESSAGE=''

!L----------------------------------------------------------------------
!L 0. Start Timer call for U_MODEL_4A (NB: not conditional on ltimer)
!L
! DEPENDS ON: timer
      IF (ltimer) CALL TIMER('U_MODEL_4A ',5)

! DEPENDS ON: lbc_coup_setup
      CALL lbc_coup_setup(ltimer)

      ICODE=0

!  Routine GETPPX_PART reads those ppxref records which correspond to
!  entries in the stash list into the ppx look-up arrays PPXI, PPXC.
!  It also sets the ppx pointer array PPXPTR. The lengths of PPXI, PPXC
!  have been dynamically allocated to the value of ppxRecs.

! Initialise row number in PPXI, PPXC arrays
      RowNumber = 1

! Initialise lookup and pointer array
      DO I=1,ppxRecs
        DO J=1,PPXREF_CODELEN
          PPXI(I,J)=0
        END DO
        DO J=1,PPXREF_CHARLEN
          PPXC(I,J) = ' '
        END DO
      END DO
      DO I = 1,N_INTERNAL_MODEL
        DO J   = 0,PPXREF_SECTIONS
          DO K = 1,PPXREF_ITEMS
            PPXPTR(I,J,K)=0
          END DO
        END DO
      END DO

! Read in STASHmaster records
      IF (INTERNAL_MODEL_INDEX(A_IM) >  0) THEN
! DEPENDS ON: getppx_part
      CALL GETPPX_PART(NFT,NFTU,'STASHmaster_A',A_IM,RowNumber,         &
     &                        ICODE,CMESSAGE)
      IF (ICODE  /=  0) CALL Ereport(RoutineName,ICODE,Cmessage)
      END IF

!L----------------------------------------------------------------------
!L 1. General initialisation of control and physical data blocks
!L

      ICODE=0

! DEPENDS ON: timer
      CALL timer('INITIAL ',5)
      CALL init_coriolis()

! DEPENDS ON: initial_4A
      CALL INITIAL_4A(                                                  &
#include "argszsp.h"
#include "argszspa.h"
#include "argszspc.h"
          ,                                                             &
#include "argsp.h"
#include "argspa.h"
#include "argspc.h"
           ngrgas,grgas_addr,                                           &
           internal_model,submodel,ngroup,meanlev)
      IF (ICODE  /=  0) CALL Ereport(RoutineName,ICODE,Cmessage)

! DEPENDS ON: timer
      CALL timer('INITIAL ',6)

! DEPENDS ON: icopenoutput
      CALL ICOPENOUTPUT(runtype_char)

!  Allocate AC scheme arrays using sizes from AC_INIT
      IF (L_AC) THEN
        obs_flag_len = A_MAX_NO_OBS
        ALLOCATE (OBS_FLAG(obs_flag_len))
! 2048 gives enough space in WORK in RDOBS2 when no or very few obs.
        obs_len = A_MAX_OBS_SIZE+2048
        ALLOCATE (OBS(obs_len))
      WRITE(6,*)'U_MODEL_4A - OBS arrays allocated with sizes ',        &
     & A_MAX_NO_OBS,A_MAX_OBS_SIZE+2048
      ELSE
        A_MAX_NO_OBS = 1
        A_MAX_OBS_SIZE = 1
        obs_flag_len = A_MAX_NO_OBS
        ALLOCATE (OBS_FLAG(obs_flag_len))
        obs_len = A_MAX_OBS_SIZE
        ALLOCATE (OBS(obs_len))
      WRITE(6,*)'U_MODEL_4A - OBS arrays allocated with length 1'
      END IF

!L----------------------------------------------------------------------
!L 2. Check for nothing-to-do
!L

! DEPENDS ON: exitchek
      CALL EXITCHEK( internal_model, lexitNOW)

      IF (LEXITNOW) GO TO 9999

!L----------------------------------------------------------------------
!L 3. Start group of timesteps

      IF (l_oasis) THEN

        put_step=.FALSE.
        get_step=.FALSE.
        cpl_update_step=.FALSE.

      END IF

      WRITE(6,'(A,F10.2,A)') 'Model running with timestep ',           & 
                             secs_per_stepim(a_im),' seconds' 

#if defined(IBM)
! On IBM turn off buffering of Fortran I/O once initialisation complete
! provided the disabling of buffering has been asked for. This is done
! by reading  the XLFRTEOPTS environment variable and checking is for the 
! 'buffering=disable_all' substring (note case and space requirements)
       CALL FORT_GET_ENV('XLFRTEOPTS', 10, C_XLFRTEOPTS, 256, ICODE_ENV)

       ! If ICODE_ENV is 0, the env var has been read
       IF (ICODE_ENV == 0) THEN
         POS = INDEX(C_XLFRTEOPTS, C_DISABLE)

         IF (POS /= 0) THEN    ! Substring found
           IF (PRINTSTATUS  >=  PRSTATUS_OPER .AND. MYPE == 0) THEN
             WRITE (6,*) 'Disabling Fortran I/O Buffering'
           END IF
          
           CALL SETRTEOPTS(C_DISABLE)
         END IF
       END IF
#endif
!L----------------------------------------------------------------------
!L 3. Start group of timesteps
!L
   1  CONTINUE
!L----------------------------------------------------------------------
!L 3.1. Start main timestep loop
!L
!L 3.1.1 Increment model time ..

! DEPENDS ON: incrtime
       CALL INCRTIME (                                                   &
#include "artduma.h"
           internal_model,ICODE,CMESSAGE)
       IF (ICODE  /=  0) CALL Ereport(RoutineName,ICODE,Cmessage)


! Keep tabs on PRISM PUT/GET timesteps.
! At the moment we say a put and get timestep are one and the same.
      IF (l_oasis) THEN

! Is this a genuine exchange timestep.
        put_step = (MOD(stepim(atmos_im),oasis_couple_ts).eq.1)
        get_step = (MOD(stepim(atmos_im),oasis_couple_ts).eq.1)
        cpl_update_step=(MOD(stepim(atmos_im),oasis_couple_ts).eq.0)

! Perform coupling exchanges relating to TS-1

! DEPENDS ON: oasis3_geto2a
        CALL oasis3_geto2a(                                         &
#include "artd1.h"
#include "arg_atm_fields.h"
           get_step)

! DEPENDS ON: oasis3_puta2o
        CALL oasis3_puta2o(                                         &
#include "artd1.h"
#include "arg_atm_fields.h"
           put_step)

! Advance date ready for next timestep (if there is one)
! DEPENDS ON: OASIS3_ADVANCE_DATE
          CALL oasis3_advance_date()

      END IF ! l_oasis=true

!L 3.1.2 .. set timestep control switches
! DEPENDS ON: settsctl
      CALL SETTSCTL (                                                   &
#include "artduma.h"
#include "artsts.h"
#include "artinfa.h"
               internal_model,.FALSE.,meanlev,icode,cmessage)
      IF (ICODE  /=  0) CALL Ereport(RoutineName,ICODE,Cmessage)

!L 3.1.3 If PPfile initialisation time call PP control routine
!L          for instantaneous data (MEANLEV=0)
      IF (LPP) THEN

! DEPENDS ON: ppctl_reinit
        CALL PPCTL_REINIT(                                              &
#include "artduma.h"
#include "artinfa.h"
           internal_model,ICODE,CMESSAGE)
      IF (ICODE  /=  0) CALL Ereport(RoutineName,ICODE,Cmessage)

      END IF

! Send an EOT message to IOS
! He may or may not act on this to purge outstanding items
      CALL IO_TIMESTEP()


!L       Integrate atmosphere or ocean by 1 timestep
          IF (internal_model == atmos_im) THEN

! Synchronize before the timestep starts
      CALL GC_GSYNC(nproc,info)

! River routing
      IF (L_RIVERS) THEN
! Get 'global' atmos horizontal domain sizes from database
! in DECOMPDB to set dynamic allocation in ATM_STEP_4A for River routing
! on PE 0                                                   .
        G_THETA_FIELD_SIZE=                                             &
     &  decompDB(decomp_standard_atmos)%glsize(1,fld_type_p) *          &
     &  decompDB(decomp_standard_atmos)%glsize(2,fld_type_p)
        G_RIVER_FIELD_SIZE=                                             &
     &  decompDB(decomp_standard_atmos)%glsize(1,fld_type_r) *          &
     &  decompDB(decomp_standard_atmos)%glsize(2,fld_type_r)
      ELSE
        G_THETA_FIELD_SIZE=1
        G_RIVER_FIELD_SIZE=1
      END IF

!CABLE:
   call set_endstep_umodel( TARGET_END_STEPim(a_im) )

! DEPENDS ON: atm_step_4A
         CALL ATM_STEP_4A (                                             &
#include "artd1.h"
#include "artduma.h"
#include "artlndm.h"
#include "arg_atm_fields.h"
#include "artbnd.h"
#include "artsts.h"
! ENDGame prognostic variables 
    exner,                                                              &
! River routing
#include "artatcpl.h"
     & G_THETA_FIELD_SIZE,                                              &
     & G_RIVER_FIELD_SIZE,                                              &
     & OBS_FLAG,OBS,obs_flag_len,obs_len,                               &
     & ngrgas,grgas_addr)
         
      IF (L_ukca) THEN
! DEPENDS ON: timer
        IF (ltimer) CALL TIMER('UKCA_MAIN1',5)
! DEPENDS ON: ukca_main1
        CALL UKCA_MAIN1(                                                &
#include "artd1.h"
#include "artduma.h"
#include "artsts.h"
#include "artlndm.h"
#include "artptra.h"
     &   I)                  ! dummy to terminate call
! DEPENDS ON: timer
        IF (ltimer) CALL TIMER('UKCA_MAIN1',6)
      END IF

!L Generate Atmosphere lateral boundary values

      IF (linterface) THEN

! DEPENDS ON: timer
        IF (ltimer) CALL TIMER ('GEN_INTF',3)

        ndustbin_in  = 6 
        ndustbin_out = 6 
! DEPENDS ON: gen_intf
        CALL GEN_INTF (                                                 &
#include "artd1.h"
#include "artduma.h"
#include "artsts.h"
#include "artptra.h"
#include "artinfa.h"
     &       submodel, ndustbin_in, ndustbin_out, ICode, CMessage) 

! DEPENDS ON: timer
        IF (ltimer) CALL TIMER ('GEN_INTF',4)

        IF (ICode /= 0) THEN
          CALL Ereport(RoutineName, ICode ,Cmessage)
        END IF

      END IF  ! linterface

         END IF        ! internal_model = atmos_im

         IF (l_oasis) THEN

! Applicable to OASIS3 and OASIS3-MCT

           IF (cpl_update_step) THEN
!---------------------------------------------------------------------
! Ensure atmos coupling data in prognostic areas of D1 are up to date.
! The logic here is that cpl_update_step will be TRUE on the timestep
! BEFORE coupling is due to take place.

! Newly generated coupling data is intercepted after ATM_STEP and
! copied to  D1 prognostics.

! On the next timestep i.e. a coupling timestep, the prognostic
! contents will be sent to the other components in the coupling process.

! If there is no subsequent timestep (i.e. if this is the last model
! timestep then the D1 contents will be written to the dump
! ready for any future restart). There is no "end-of-model"
! coupling exchange.
!---------------------------------------------------------------------

! DEPENDS ON: oasis_updatecpl
             CALL oasis_updatecpl(                              &
#include "artd1.h"
#include "arg_atm_fields.h"
              cmessage)

           END IF
         END IF


!L 3.1.4 If dump time, call dump control routine
          IF (ldump) THEN
            IF (ltimer) THEN
! DEPENDS ON: timer
              CALL TIMER('DUMPCTL',5)

            END IF
! DEPENDS ON: dumpctl
            CALL DUMPCTL (                                              &
#include "artd1.h"
#include "artduma.h"
#include "artlndm.h"
#include "artptra.h"
#include "artsts.h"
     &          submodel,MEANLEV,.false.,'           ',0,               &
     &          ICODE,CMESSAGE)
            IF (ICODE  /=  0) CALL Ereport(RoutineName,ICODE,Cmessage)

            IF (ltimer) THEN
! DEPENDS ON: timer
              CALL TIMER('DUMPCTL ',6)
            END IF
          END IF
!L 3.1.5 If printed output time, call print control routine
          IF (lprint) THEN

          IF (PrintStatus >= PrStatus_Oper) THEN
         WRITE(6,*) RoutineName,':Warning, Printing of climate global ' &
     &             ,'and zonal diagnostics no longer supported'
          END IF  ! PrintStatus test

          END IF
!L 3.1.6.1 Release job to process output created so far, if selected
          IF (ljobrelease) THEN
! Write all pp files' cached lookups to ensure that current data is 
! is in the files output stream
            CALL StoreAllLookups()

! Flush/Sync all pp files' units to ensure that data is committed from the 
! application
            CALL SynchroniseAll()

! DEPENDS ON: jobctl
            CALL JOBCTL(internal_model,ICODE,CMESSAGE)

            IF (icode  /=  0) CALL Ereport(RoutineName,icode,Cmessage)
          END IF
!L 3.1.7 If partial sum/mean creation time, call means control routine
!L       (calls mean PPfield and diagnostic print routines internally)
          IF (lmean) THEN

! DEPENDS ON: meanctl
            CALL MEANCTL (                                              &
#include "artd1.h"
#include "artduma.h"
#include "artptra.h"
#include "artsts.h"
#include "artlndm.h"
#include "artinfa.h"
     &                  submodel,MEANLEV,ICODE,CMESSAGE)

            IF (ICODE >  0) THEN
              CALL Ereport(RoutineName,ICODE,Cmessage)
            END IF
          END IF

! 3.1.8 Update history file once dumping and any meaning is complete
  IF (ldump) THEN
! checkpoint_dump_im now successfully written. Update history file to restart
! from this instead of superseded dump stored in save_dumpname. Only delete 
! superseded dump when this has been done.

! DEPENDS ON: set_history_values
    CALL set_history_values
    ! Write history to backup location before overwriting main file
    IF (mype  ==  0) THEN
! DEPENDS ON: temphist
      CALL temphist(thist_unit,icode,cmessage)
      IF (icode /= 0) THEN
        WRITE(6,*)routinename,':Failure writing temporary restart file'
        WRITE(6,*)'Check for problems and restart from main file'
        CALL ereport(routinename,icode,cmessage)
      END IF
      CALL temphist(xhist_unit,icode,cmessage)
      IF (icode /= 0) THEN
        WRITE(6,*)routinename,':Failure writing main restart file'
        WRITE(6,*)'Check for problems and restart from temporary file'
        WRITE(6,*)'by overwriting main file with temporary file'
        CALL ereport(routinename,icode,cmessage)
      ELSE
        ! Main restart file successfully written, so delete backup
! DEPENDS ON: del_hist
        CALL del_hist(thist_unit)
      END IF

      IF (ft_select(22) == "Y") THEN
        ! History files written so can delete superseded dump.
        IF (save_dumpname_im(a_im) /= blank_file_name) THEN
          CALL jobcntrl(jc_delete,save_dumpname_im(a_im))
        END IF
        save_dumpname_im(a_im)=checkpoint_dump_im(a_im)
      END IF ! ft_select(22) == "Y"
    END IF ! IF (mype == 0)
  END IF ! IF (ldump)

!L 3.1.9 If exit check time, check for immediate exit
  IF (lexit) THEN

! DEPENDS ON: exitchek
    CALL EXITCHEK(internal_model, lexitNOW)

    IF (lexitNOW) THEN
      IF (.NOT.ldump) THEN

        WRITE(6,*)routinename,                                         &
           ': Warning: exiting at a period that is not a dump period'
        WRITE(6,*)'Therefore continuing the run will rerun preceding timesteps'
        WRITE(6,*)'This is inefficient and can cause restart problems'

      END IF
      GO TO 9999
    END IF
  END IF
!L 3.1.10 Update ancillary fields if necessary
          IF (lancillary) THEN

! DEPENDS ON: up_ancil
         CALL UP_ANCIL (                                                &
#include "artd1.h"
#include "artduma.h"
#include "artptra.h"
#include "artanc.h"
     &                   submodel,                                      &
     &                   ICODE,CMESSAGE)

      IF (ICODE  /=  0) CALL Ereport(RoutineName,ICODE,Cmessage)
          END IF
!L 3.1.11 Update boundary fields if necessary
          IF (lboundary) THEN
             ! DEPENDS ON: lbc_coup_update
             CALL lbc_coup_update( &
#include "artd1.h"
#include "artduma.h"
#include "artptra.h"
#include "artbnd.h"
                  submodel,ICODE,CMESSAGE)
          END IF
!L
!L      End main timestep loop
!L----------------------------------------------------------------------

      GO TO 1
!
 9999 CONTINUE

!L----------------------------------------------------------------------
!L 4. Exit processing: Output error messages and perform tidy-up
!L

      IF (l_oasis) THEN
! DEPENDS ON: oasis_tidy
        CALL oasis_tidy(                       &
#include "artd1.h"
#include "artsts.h"
#include "artduma.h"
#include "artptra.h"
        icode,cmessage)
      END IF


! DEPENDS ON: iccloseoutput
      CALL ICCLOSEOUTPUT()

!L 4.1 Exit processing: If abnormal completion, output error message
      IABORT = ICODE
      IF (ICODE /= 0) THEN

        CALL Ereport(RoutineName,ICODE,Cmessage)

      END IF
!L 4.2 Exit processing: Perform tidy-up

! DEPENDS ON: exitproc
      CALL EXITPROC(ICODE,CMESSAGE)

!L 4.3 Exit processing: If error in exit processing, output error mess
      IF (ICODE /= 0) THEN

        CALL Ereport(RoutineName,ICODE,Cmessage)

      END IF

      DEALLOCATE(PPXI)
      DEALLOCATE(PPXPTR) 

!L----------------------------------------------------------------------
!L 5. Complete Timer call and return
!L
      ICODE=IABORT
! DEPENDS ON: timer
      IF (ltimer) CALL TIMER('U_MODEL_4A ',6)

      IF (lhook) CALL dr_hook('U_MODEL_4A',zhook_out,zhook_handle)

      RETURN
      END SUBROUTINE U_MODEL_4A
