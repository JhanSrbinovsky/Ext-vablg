! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

!    Routine: SETTSCTL -------------------------------------------------
!
!    Purpose: Sets timestep loop control switches and STASHflags.
!             Note STEP on entry is the values at the N+1 (ie. updated)
!             timelevel; control switches are therefore set using
!             A_STEP/O_STEP, whereas physical switches are set using
!             A_STEP-1/O_STEP-1 to ensure correct synchronisation.
!             Note also that step on entry is N (i.e. not updated) when
!             called from INITIAL.
!
!
!    Programming standard: UM Doc Paper 3, version 8.2
!
!    Project task: C0
!
!    External documentation: On-line UM document C0 - The top-level
!                            control system
!
!     ------------------------------------------------------------------
!    Interface and arguments: ------------------------------------------
!
!    Code Owner: See Unified Model Code Owners HTML page
!    This file belongs in section: Top Level

SUBROUTINE settsctl (                                             &
#include "argduma.h"
#include "argsts.h"
#include "arginfa.h"
       internal_model,linitial,meanlev,icode,cmessage)

USE rad_input_mod, ONLY:                                          &
    A_SW_RadStep,                                                 &
    A_LW_RadStep,                                                 &
    A_SW_RadStep_diag,                                            &
    A_SW_Radstep_prog,                                            &
    A_LW_RadStep_diag,                                            &
    A_LW_Radstep_prog,                                            &
    l_sw_radiate,      l_lw_radiate,                              &
    l_sw_radiate_diag, l_lw_radiate_diag,                         &
    l_sw_radiate_prog, l_lw_radiate_prog

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE IAU_mod, ONLY : L_IAU
USE UM_ParParams
USE Control_Max_Sizes
USE um_input_control_mod, ONLY: lcal360
USE eng_corr_inputs_mod, ONLY: a_energysteps, lenergy, lflux_reset

USE lookup_addresses
USE nlstgen_mod, ONLY: steps_per_periodim, secs_per_periodim,     &
     dumpfreqim, dumptimesim, dumptimes_len1,                     &
     meanfreqim, meanfreq_len1, jobrel_stepim, jobrel_len1,       &
     jobrel_offsetim   

USE Submodel_Mod     

USE nlstcall_mod, ONLY : model_basis_time, &
                         ancil_reftime, &
                         ft_steps, &
                         ft_firststep, &
                         ft_laststep, &
                         LPP, &
                         lpp_select, &
                         ldump, &
                         lmean, &
                         lprint, &
                         linterface, &
                         lexit, &
                         ljobrelease, &
                         lancillary, &
                         lboundary, &
                         lassimilation, &
                         model_assim_mode, &
                         type_letter_1, &
                         type_letter_2, &
                         ft_output, &
                         run_assim_mode

USE chsunits_mod, ONLY : nunits

IMPLICIT NONE

INTEGER internal_model   ! IN  - internal model identifier
LOGICAL linitial         ! IN  - true in called from INITIAL
INTEGER meanlev          ! OUT - Mean level indicator
! Needed for N_INTERNAL_MODEL
#include "typsize.h"
#include "typduma.h"
! Contains *CALL CPPXREF
#include "typsts.h"
#include "typinfa.h"
INTEGER icode            ! Out - Return code
CHARACTER(LEN=80) cmessage  ! Out - Return error message

!*----------------------------------------------------------------------
!  Common blocks

#include "chistory.h"
#include "ccontrol.h"
#include "ctime.h"
#include "stparam.h"

!  Local variables

INTEGER i,nftunit        ! Loop counters
INTEGER itime            ! Loop index over selected dump times
INTEGER step             ! A_STEP or O_STEP for atmos/ocean
INTEGER is,ii,il,im,ntab,it,ie  ! STASH variables
INTEGER modl             ! Int model no, read from STASH list arra
INTEGER modjobrel        ! modulus of JOBREL_STEP()
INTEGER step_assim       ! model step rel. to assim start

INTEGER jintf            ! Interface area index
INTEGER ancil_ref_days,ancil_ref_secs
INTEGER ancil_offset_steps ! offset of ref. from basis time
INTEGER secs_per_step    ! seconds per timestep
INTEGER months_in        ! Number of months into forecast
INTEGER secs_per_period
INTEGER steps_per_period
INTEGER dumpfreq
INTEGER offset_dumps
INTEGER exitfreq
INTEGER target_end_step
INTEGER ancillary_steps
INTEGER boundary_steps
INTEGER bndary_offset
INTEGER dumptimes(dumptimes_len1)
INTEGER meanfreq (meanfreq_len1)
INTEGER jobrel_step(jobrel_len1)

LOGICAL iau_resetdt ! If .TRUE., reset data time.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook('SETTSCTL',zhook_in,zhook_handle)

icode=0

! Initialise internal_model
internal_model = 1 ! Atmosphere

!  1. Set timestep loop top-level control switches
!
!  1.0  Initialise control switches which are shared in coupled runs
!

step=                        stepim(internal_model)
secs_per_period=  secs_per_periodim(internal_model)
steps_per_period=steps_per_periodim(internal_model)
secs_per_step=secs_per_period/steps_per_period
dumpfreq=                dumpfreqim(internal_model)
offset_dumps=        offset_dumpsim(internal_model)
target_end_step=  target_end_stepim(internal_model)
ancillary_steps=  ancillary_stepsim(internal_model)
boundary_steps =   boundary_stepsim(internal_model)
bndary_offset  =   bndary_offsetim(internal_model)

exitfreq=dumpfreq

DO i=1,dumptimes_len1
  dumptimes(i)=       dumptimesim(i,internal_model)
END DO ! i
DO i=1,meanfreq_len1
  meanfreq(i)=         meanfreqim(i,internal_model)
END DO ! i
DO i=1,jobrel_len1
  jobrel_step(i)=   jobrel_stepim(i,internal_model)
END DO ! i
meanlev=0

lassimilation=.FALSE.
ldump=        .FALSE.
lexit=        .FALSE.
lmean=        .FALSE.
lprint=       .FALSE.
lancillary=   .FALSE.
lboundary=    .FALSE.
linterface=   .FALSE.
ljobrelease = .FALSE.
!
!  1.1  Set up PPfile switches for the timestep
!
lpp =.FALSE.
  DO nftunit=20,nunits
    IF (ft_output(nftunit) == 'Y') THEN

      ! Initialise:
      lpp_select(nftunit) = .FALSE.

      ! Allow mix of real-month & regular reinit. periods on
      ! different units:
      IF (ft_steps(nftunit) <  0) THEN ! Real-month reinit.

        ! Select files to be reinitialised on this timestep:
        ! First initialisation may be on any timestep, not just 0
        IF ((type_letter_1(nftunit) == 'c' .AND.                   &
             step-1 == ft_firststep(nftunit))                      &
        .OR.(type_letter_1(nftunit) /= 'c' .AND.                   &
             step == ft_firststep(nftunit))) THEN

          lpp_select(nftunit) =.TRUE.

        ELSE IF (.NOT.lcal360                                                 &
                 .AND.(nftunit >= 60 .AND. nftunit <  70 .OR. nftunit == 151) &
                 .AND.(i_day     ==  1)                                       &
                 .AND.(i_hour    ==  secs_per_step/3600           )           &
                 .AND.(i_minute  ==  (MOD(secs_per_step,3600))/60 )           &
                 .AND.(i_second  ==   MOD(secs_per_step,60)       )           &
                 .AND.(step /= 1)) THEN

          ! Gregorian calendar: 
          ! Reinitialisation can only be selected when day=1 and
          ! hours, minutes and seconds equals exactly one time-step
          ! This code relies on the rule that the time-step length
          ! divides into a day.

          ! Additionally initialisation only takes place on 1-month,
          ! 3-month (season) or 12-month boundaries. So calculate months
          ! since start of job, and request reinitialisation at the 
          ! appropriate time according to which option selected.
          months_in = i_month - model_basis_time(2) +             &
             12 * (i_year - model_basis_time(1))

          IF (months_in >= ft_firststep(nftunit) .AND.             &
             (ft_laststep(nftunit) <  0 .OR.                       &
              months_in <= ft_laststep(nftunit))) THEN

            IF (ft_steps(nftunit) == -1) THEN
              lpp_select(nftunit) = .TRUE. ! Months
            ELSE IF (ft_steps(nftunit) == -3 .AND.                 &
                     MOD(months_in -                               &
                      ft_firststep(nftunit),3)  == 0) THEN
              lpp_select(nftunit) = .TRUE. ! Seasons
            ELSE IF (ft_steps(nftunit) == -12 .AND.                &
                     MOD(months_in -                               &
                      ft_firststep(nftunit),12) == 0) THEN
              lpp_select(nftunit) = .TRUE. ! Years
            END IF ! Of ft_steps(NFTUNIT) = reinit period

          END IF ! Of MONTHS_IN within reinitialisation limits

        END IF  ! of lcal360 and nftunit etc.

      ELSE IF (ft_steps(nftunit) >  0) THEN ! Regular reinit.

        ! Select files to be reinitialised on this timestep:
        ! First initialisation may be on any timestep, not just 0
        IF ((type_letter_1(nftunit) == 'c' .AND.                   &
             step-1 == ft_firststep(nftunit))                      &
        .OR.(type_letter_1(nftunit) /= 'c' .AND.                   &
             step == ft_firststep(nftunit))) THEN

          lpp_select(nftunit) = .TRUE.

        ! Deal with subsequent reinitialisation after end of period
        ELSE IF ((step-1) > ft_firststep(nftunit) .AND.            &
                (ft_laststep(nftunit) <  0 .OR.                    &
                (step-1) <= ft_laststep(nftunit))) THEN

          IF (MOD((step-1) -                                       &
              ft_firststep(nftunit),ft_steps(nftunit)) == 0)       &
            lpp_select(nftunit) = .TRUE.

          ! Do not reinitialise files on step 1 if they were
          ! initialised at step 0:
          IF (step == 1 .AND. ft_firststep(nftunit) == 0)          &
            lpp_select(nftunit) = .FALSE.

          ! Sub model id must correspond with TYPE_LETTER_2:
          lpp_select(nftunit) =  lpp_select(nftunit)               &
              .AND. internal_model == atmos_im                     &                 
              .AND. type_letter_2(nftunit) == 'a'

        END IF
!
!  1.1.1  Set up PPfile switches for boundary output files
!
        IF (type_letter_1(nftunit) == 'b') THEN ! Boundary File

!               Get interface area number
! DEPENDS ON: intf_area
          CALL intf_area( internal_model, nftunit, jintf)

          lpp_select(nftunit) = ( lpp_select(nftunit)              &
          .AND. .NOT.                                              &
!               Do not reinitialise first file on timestep ft_firststep
          (step-1-ft_firststep(nftunit) == 0) )                    &
          .OR.                                                     &
!               Initialise first file if start of sequence is offset
!               from beginning of run
          (step-1+interface_stepsim(jintf,internal_model) -        &
           ft_firststep(nftunit)  ==  0)                           &
          .OR.                                                     &
!               Select boundary file if incomplete on continuation run
          (linitial                                                &
          .AND.                                                    &
           step+interface_stepsim(jintf,internal_model) >          &
                                         ft_firststep(nftunit)     &
          .AND.                                                    &
           step <= interface_lstepim(jintf,internal_model)         &
          .AND.                                                    &
          (step-ft_firststep(nftunit) == 0 .OR.                    &
           MOD(step-ft_firststep(nftunit),ft_steps(nftunit)) /= 0) &
            )

        END IF

      ELSE  ! for files not reinitialised, ie. ft_steps(nftunit)=0

!           Initialise at step 0
        lpp_select(nftunit) = step == 0 .AND.                      &
        (ft_steps(nftunit) == 0.OR.ft_firststep(nftunit) == 0)

!           Select boundary file if incomplete on continuation run
        IF (linitial .AND.                                         &
          type_letter_1(nftunit) == 'b') THEN ! Boundary File

!             Get interface area number
! DEPENDS ON: intf_area
          CALL intf_area( internal_model, nftunit, jintf)

          lpp_select(nftunit) = lpp_select(nftunit) .OR.           &
          (step >  0.AND.step <=                                   &
                       interface_lstepim(jintf,internal_model))

        END IF

      END IF  ! of ft_steps(NFTUNIT) lt, gt or =0, ie. reinit. type
    ELSE ! of ft_output(NFTUNIT) =Y
      lpp_select(nftunit)=.FALSE.
    END IF
    lpp = lpp .OR. lpp_select(nftunit)
  END DO  ! of loop over nftunit from 20 to nunits

!
!  1.2   Set switches for general internal models.
!        For coupled models dump related switches can only be set when
!        the last internal model in a submodel has completed its group
!        of timesteps. For coupled models the only safe restart point
!        is at the completion of all groups within a model timestep.

IF(n_internal_model == 1.OR.(                                     &
                                     ! if not coupled model, or
   last_im_in_sm(internal_model).AND.                             &
                                      ! last model in submodel
   MOD(step,steps_per_period) == 0))                              &
                                      ! and last step in group
   THEN

!  ldump   : Write-up dump on this timestep

  IF (dumpfreq >  0) THEN
    ldump=       (MOD(step,dumpfreq)    == 0)
  ELSE
    ldump=.FALSE.
    DO itime=1,dumptimes_len1
      ldump=ldump.OR.(step == dumptimes(itime))
    END DO
  END IF

!  LMEAN   : Perform climate-meaning from dumps on this timestep
  lmean = .FALSE.
  IF (dumpfreq >  0.AND.meanfreq(1) >  0) THEN
    lmean=     (MOD(step,dumpfreq)       == 0)
  END IF

!  LEXIT   : Check for exit condition on this timestep
  IF (exitfreq >  0) THEN ! Implies climate meaning

    lexit=  ( (MOD(step,exitfreq)    == 0)  .OR.                  &
              (step  >=  target_end_step) )

  ELSE                    ! No climate meaning

    lexit=    (step  >=  target_end_step)

  END IF

!  lancillary: Update ancillary fields on this timestep

!   Convert ancillary reference time to days & secs
! DEPENDS ON: time2sec
  CALL time2sec(ancil_reftime(1),ancil_reftime(2),                &
                ancil_reftime(3),ancil_reftime(4),                &
                ancil_reftime(5),ancil_reftime(6),                &
                0,0,ancil_ref_days,ancil_ref_secs,lcal360)

!   Compute offset in timesteps of basis time from ancillary ref.time
! DEPENDS ON: tim2step
  CALL tim2step(basis_time_days-ancil_ref_days,                   &
                basis_time_secs-ancil_ref_secs,                   &
                steps_per_period,secs_per_period,ancil_offset_steps)

  IF (ancillary_steps >  0.AND.step >  0)                         &
    lancillary=(MOD(step+ancil_offset_steps,ancillary_steps) == 0)

END IF     ! Test for non-coupled or coupled + last step in group

!  lboundary    : Update boundary fields on this timestep

IF (boundary_steps >  0)                                          &
    lboundary= (MOD(step+bndary_offset,boundary_steps)  == 0)

!  ljobrelease  : Release user jobs on this timestep

DO i=1,jobrel_len1
  modjobrel=ABS(jobrel_step(i))
  IF (modjobrel /= 0) THEN
    ljobrelease = ljobrelease .OR.                                &
    (step == jobrel_step(i) .AND. jobrel_step(i) >  0).OR.        &
    (MOD(step,modjobrel) == jobrel_offsetim .AND. jobrel_step(i) <  0)
  END IF
END DO
!
!  1.2.1  Set switches for atmosphere timestep
!
IF (internal_model == atmos_im) THEN
!  1.2.2 Set switches for all cases

! Energy correction switches
! Set on the assumption that the energy correction is evaluated at the
! end of a timestep
  IF (a_energysteps >  0) THEN
! true if this is the step on which to do energy calculation
    lenergy =  (MOD(step,a_energysteps) == 0)
! True if this is the step after last energy calculation
    lflux_reset = (MOD(step-1,a_energysteps) == 0)
  END IF


!  lassimilation: Perform data assimilation on this timestep

  lassimilation= (step >  assim_firststepim(a_im) .AND.           &
   (step-assim_firststepim(a_im)  <=                              &
         assim_stepsim(a_im)+assim_extrastepsim(a_im)))
  lassimilation=lassimilation.AND.model_assim_mode == 'Atmosphere'
!
!       Reset Data Time fields in dump header at new analysis time
!       ( also in history block )
!       ( This now includes resetting prognostic field LOOKUP headers )
!       NB: done even if assimilation suppressed by run_assim_mode
!       Set MEANLEV to -1  and ldump to TRUE to force dump of analysis
!       - otherwise set MEANLEV to 0 (ie. instantaneous)
!
  iau_resetdt = .FALSE.
  IF (l_iau .OR. run_assim_mode  ==  "NoIAU     ") THEN
    IF (step  ==  iau_dtresetstep) iau_resetdt = .TRUE.
  END IF
  IF ( (step == assim_firststepim(a_im)+assim_stepsim(a_im)       &
        .AND.lassimilation) .OR. iau_resetdt) THEN

    DO i=21,27
      a_fixhd(i)=a_fixhd(i+7)
    END DO
    DO i=1,6
      model_data_time(i)=a_fixhd(20+i)
    END DO
    DO i=1,a_prog_lookup
      a_lookup(lbyrd ,i)=a_fixhd(21)
      a_lookup(lbmond,i)=a_fixhd(22)
      a_lookup(lbdatd,i)=a_fixhd(23)
      a_lookup(lbhrd ,i)=a_fixhd(24)
      a_lookup(lbmind,i)=a_fixhd(25)
      a_lookup(lbsecd,i)=a_fixhd(26)
    END DO
    meanlev=-1
  ELSE
    meanlev=0
  END IF
!
!       Suppress assimilation using run_assim_mode if necessary
  lassimilation=lassimilation .AND. run_assim_mode == 'Atmosphere'

!  L_SW_RADIATE : Perform SW-radiation on this timestep
!  L_LW_RADIATE : Perform LW-radiation on this timestep

  IF (a_sw_radstep >  0)                                          &
    l_sw_radiate=(MOD(step-1,a_sw_radstep)  == 0)
  IF (a_lw_radstep >  0)                                          &
    l_lw_radiate=(MOD(step-1,a_lw_radstep)  == 0)
  IF (a_sw_radstep_diag >  0)                                     &
    l_sw_radiate_diag=(MOD(step-1,a_sw_radstep_diag)  == 0)
  IF (a_lw_radstep_diag >  0)                                     &
    l_lw_radiate_diag=(MOD(step-1,a_lw_radstep_diag)  == 0)

  IF (a_sw_radstep_prog >  0)                                     &
    l_sw_radiate_prog=(MOD(step-1,a_sw_radstep_prog)  == 0)
  IF (a_lw_radstep_prog >  0)                                     &
    l_lw_radiate_prog=(MOD(step-1,a_lw_radstep_prog)  == 0)

!  linterface: Output interface fields on this timestep (>1 file poss.)

  DO jintf=1,n_intf_a
    IF (interface_stepsim(jintf,atmos_im) >  0) THEN
      linterface = linterface .OR.                                &
      (MOD(step-interface_fstepim(jintf,atmos_im),                &
           interface_stepsim(jintf,atmos_im)) == 0                &
      .AND. step >= interface_fstepim(jintf,atmos_im)             &
      .AND. step <= interface_lstepim(jintf,atmos_im) )
! To allow output of interface files from model
      IF (linitial) THEN
        lpp_select(jintf+139) = .TRUE.
      END IF
    END IF
  END DO
END IF ! Test for atmos_im
!
! ----------------------------------------------------------------------
!  2. Set STASHflags to activate diagnostics this timestep
!
!     IS is section number
!     IE is item number within section
!     IL is item number within STASHlist
!     II is counter within given section/item sublist for repeated items
!     IM is cumulative active item number within section

!   Clear all STASHflags

DO is=0,nsects
  DO im=0,nitems
    sf     (im,is) = .FALSE.
    sf_calc(im,is) = .FALSE.
  END DO
END DO

!   Loop over all items in STASHlist, enabling flags for diagnostics
!   which are active this step --
!     note that atmosphere/ocean diagnostics must be discriminated

DO il =1,totitems
  modl=stlist(st_model_code  ,il)
  is  =stlist(st_sect_no_code,il)
  im  =stlist(st_item_code   ,il)
! Skip diagnostics which are not active
  IF(stlist(st_proc_no_code,il) == 0) GO TO 200
! Skip diagnostics which don't correspond to the submodel this timestep
  IF(internal_model /= modl) GO TO 200

!     STASHflag is off by default.
!     But reset ...

!       ... if required every timestep between start and end

  IF (stlist(st_freq_code,il) == 1) THEN
    IF (step >= stlist(st_start_time_code,il).AND.                &
       (step <= stlist(st_end_time_code,il).OR.                   &
        stlist(st_end_time_code,il) == st_infinite_time))         &
    THEN
      sf(im,is)=.TRUE.
      sf(0 ,is)=.TRUE.
    END IF

!       ... if required at specified times and this is one of them

  ELSE IF(stlist(st_freq_code,il) <  0) THEN
    ntab=-stlist(st_freq_code,il)
    DO it=1,nsttims
      IF(sttabl(it,ntab) == st_end_of_list) EXIT
      IF(step == sttabl(it,ntab)) THEN
        sf(im,is)=.TRUE.
        sf(0 ,is)=.TRUE.
      END IF
    END DO

!       ... if required every N timesteps and this is one of them

  ELSE IF (stlist(st_freq_code,il) >  0) THEN
    IF  (MOD((step-stlist(st_start_time_code,il)),                &
              stlist(st_freq_code,il)) == 0.AND.                  &
         step >= stlist(st_start_time_code,il).AND.               &
        (step <= stlist(st_end_time_code,il).OR.                  &
         stlist(st_end_time_code,il) == st_infinite_time))        &
    THEN
      sf(im,is)=.TRUE.
      sf(0 ,is)=.TRUE.
    END IF
  END IF

! We now want just a subset of the above where we only want to calculate
  IF (sf(im,is)) THEN
    ! See whether we are performing a replace. 
    IF ( stlist(st_proc_no_code,il) == st_replace_code ) THEN
      ! We only want to know about replaces which are not dependent on other
      ! records.  If positive this STASH request requires a calculation.
      IF ( stlist(st_input_code,il) > 0 ) THEN
        sf_calc(im,is)  = .TRUE.
        sf_calc(0 ,is)  = .TRUE.
      END IF
    ! Everything not a replace we just let through.
    ELSE
      sf_calc(im,is)  = .TRUE.
      sf_calc(0 ,is)  = .TRUE.
    END IF

  END IF

!     Next item

  200 CONTINUE
END DO
! ----------------------------------------------------------------------
!  3. If errors occurred in setting STASHflags, set error return code
IF (icode == 1) cmessage='SETTSCTL: STASH code error'
 9999 CONTINUE
IF (lhook) CALL dr_hook('SETTSCTL',zhook_out,zhook_handle)
RETURN
! ----------------------------------------------------------------------
END SUBROUTINE settsctl
