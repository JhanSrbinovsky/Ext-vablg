! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Checks version mask and option code in a ppx record
!
! Code Owner: See Unified Model Code Owners HTML page
! This file belongs in section: Top Level

! Subroutine Interface:

SUBROUTINE tstmsk(modl,isec,lmask,ladres,errorstatus,cmessage)

#if defined(RECON)
USE rcf_address_vars_mod, ONLY :                                  &
    iopn,                                                         &
    ispace,                                                       &
    vmsk

USE rcf_model_mod, ONLY :                                         &
    mean_number,         h_global,                                &
    aasset,                                                       &
    a_max_trvars,        tracer_a,                                &
    a_max_ukcavars,      tr_ukca_a,                               &
    tca_lbc,             tc_lbc_ukca,                             &
    h_vers,                                                       &
    orogr,                                                        &
    h_orog_grad,         h_strat,                                 &
    h_floor

USE rcf_o3intp_mod

USE rcf_recon_mod, ONLY :                                         &
    var_recon

USE nlsizes_namelist_mod, ONLY :                                  &
    tr_vars,                                                      &
    tr_ukca,                                                      &
    nice,                                                         &
    nice_use,                                                     &
    nsmax
#else
! JULES
USE ancil_info, ONLY: nsmax
#endif

USE rimtypes
USE lbc_mod

USE g_wave_input_mod, ONLY:                                       &
    l_gwd,l_use_ussp

USE dust_parameters_mod, ONLY: l_dust, l_dust_diag, l_twobin_dust
USE um_input_control_mod, ONLY:                                            &
    l_use_arclocff,                                                        &
    l_soot   ,l_sulpc_so2   ,l_so2_natem  ,l_use_arcldust    ,             &
    l_nh3_em ,l_dms_em   ,l_sulpc_ozone ,l_sulpc_nh3  ,l_use_arclsulp    , &
    l_biomass,l_use_biogenic            ,              l_use_arclsslt    , &
    l_ocff   ,l_dms_ointer  ,l_sulpc_dms  ,l_use_arclblck    ,             &
    l_use_arclbiom,                                                        &
    l_nitrate,l_so2_surfem ,l_use_arcldlta    ,                            &
    l_oasis, l_oasis_icecalve, l_soot_surem,                               &
              l_triffid  ,               l_soot_hilem ,l_use_seasalt_pm  , &
                          l_bmass_surem ,l_bmass_hilem,                    &
                          l_so2_hilem  ,                                   &
    l_veg_fracs,l_ocff_surem  ,l_ocff_hilem ,l_co2_interactive ,           &
                          l_use_sulpc_indirect_sw,l_use_seasalt_direct   , &
                          l_sulpc_online_oxidants,l_use_seasalt_indirect , &
    l_endgame
USE mphys_inputs_mod, ONLY: l_mcr_qcf2, l_mcr_qgraup, l_mcr_qrain
USE cloud_inputs_mod, ONLY: l_cld_area, l_pc2, l_rhcpt
USE river_inputs_mod, ONLY: l_rivers, l_inland
USE cv_run_mod, ONLY: l_ccrad, l_3d_cca, l_conv_hist, l_param_conv
USE murk_inputs_mod,  ONLY: l_murk, l_murk_source

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE ancilcta_namelist_mod, ONLY: l_sstanom
USE umsections_mod, ONLY: atmos_sr
USE bl_option_mod,  ONLY: isrfexcnvgust
USE rad_input_mod, ONLY: l_use_cariolle, l_use_tpps_ozone, l_use_grad_corr,&
     l_orog_unfilt, i_ozone_int

! JULES
USE switches, ONLY: l_flake_model, l_spec_albedo, l_albedo_obs, iscrntdiag,&
                    l_snow_albedo, l_sice_multilayers, l_top, can_model,   &
                    l_ctile, l_aggregate, i_aggregate_opt
USE switches_urban, ONLY : l_urban2t

USE PrintStatus_mod
USE UM_ParParams
USE domain_params
USE o3intp_mod, ONLY: io3_3dspec, io3_2dspec, io3_2dmasscon,      &
                      io3_trop_map, io3_trop_map_masscon
! version_mod items required by cstash.h and model.h
USE version_mod, ONLY: nproftp, nprofdp, nprofup, ndiagpm,        &
                       ntimep, NTimSerP, nlevp, npslevp,          &
                       npslistp, outfile_s, outfile_e, nsectp


USE Submodel_Mod
IMPLICIT NONE

! Description:
!   Determines whether a diagnostic is available to a particular
!   (internal model,section) version. Also checks the option code IOPN
!   Called by INACTR, PRELIM.

! Method:

! The decimal value of the version mask, VMSK, was read from the
! ppxref file by GETPPX, each (model, section, item). The version
! number of the (model, section) - NMASK - is obtained from the
! H_VERS array.

! The procedure for checking diagnostic availability is as follows:
! (1) Check whether the relevant internal model is included in the
!     submodel configuration, by examining the INTERNAL_MODEL_LIST
!     array; if not, return.
! (2) Check whether NMASK=0 - this implies that the diag is
!      unavailable to any version. If so, return.
! (3) Check whether the diag is available to the specified version.
!     The 'version mask' binary code in the STASHmaster specifies
!     which versions the diag is available to. Eg., if it is available
!     to vns. 1 and 3, but not vn.2, the version mask is 101. VMSK
!     is the decimal equivalent of the version mask (5 in this example).
!     TSTMSK therefore calculates the quantity

!             IMOD = MOD(VMSK,2**NMASK)/2**(NMASK-1)

!       If IMOD=1, the diag is available; if IMOD=0, it isn't.

!   The option code is checked in accordance with the ppxref option
!   code definitions.

!   Note for code developers adding new tests: LMASK is initialised to
!   .TRUE. at top of deck. A series of tests is made and if any one
!   test fails LMASK is reset to .FALSE. Therefore do not reinitialise
!   LMASK to .TRUE. anywhere otherwise you may inadvertently overwrite
!   a preceding .FALSE. setting or you may mislead future code writers
!   who may do so.
!    For this reason, all unnecessary LMASK=.TRUE. were removed at 5.1

!  Code description:
!  Language: Fortran 90.
!  This code is written to UM programming standards version 8.3.

!  System component covered:
!  System task:               Sub-Models Project

! Global variables:
#if !defined(RECON)
#include "cstash.h"
#include "typsize.h"
#include "model.h"
#endif

! Subroutine arguments:
!   Scalar arguments with intent(in):
INTEGER modl    ! Internal model number
INTEGER isec    ! Section number

!   Scalar arguments with intent(out):
LOGICAL lmask   ! T if diag is available to specified version
LOGICAL ladres  ! T if diag is available for addressing primary
CHARACTER(LEN=80) cmessage

! Local scalars
INTEGER nmask   ! Version number for (model,section)
INTEGER imod    ! Determines whether diag is available
INTEGER twonm   ! Used in calculation of IMOD
INTEGER twonm1  ! Used in calculation of IMOD
INTEGER count
INTEGER i
INTEGER n0,n1,n2,n2n1,n3,n4,n5,n6,n7,n8,n9,n10
INTEGER n11,n12,n13,n14,n15,n16,n17,n18,n19,n20
INTEGER n21,n22,n23,n24,n25,n26,n27,n28,n29,n30
INTEGER n10n9
INTEGER sum_iopn

#if !defined(RECON)
! Hard-wire var_recon to false if not the reconfiguration
LOGICAL, PARAMETER :: var_recon=.FALSE.
#endif
! ErrorStatus
INTEGER errorstatus

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!- End of Header ---------------------------------------------------

IF (lhook) CALL dr_hook('TSTMSK',zhook_in,zhook_handle)
sum_iopn=iopn(1)+iopn(2)+iopn(3)+iopn(4)+iopn(5)+iopn(6)
!--------------------------------------------------------------------
! Check whether internal model is included in submodel configuration
!--------------------------------------------------------------------
count = 0
DO i = 1,n_internal_model_max
  IF (internal_model_list(i) == modl) THEN
    lmask =.TRUE.
    ladres=.TRUE.
    count = count + 1
  END IF
END DO
IF (count == 0) THEN
  lmask =.FALSE.
  ladres=.FALSE.
  GO TO 9999
END IF

!--------------------------------------------------------------------
! Check whether diagnostic is unavailable to any version
!--------------------------------------------------------------------
nmask=h_vers(modl,isec)
IF (nmask == 0) THEN
  lmask =.FALSE.
  ladres=.FALSE.
  GO TO 9999
END IF

! Determine whether the diag is available to the specified version
twonm  = 2**nmask
twonm1 = 2**(nmask-1)
imod   = MOD(vmsk,twonm)/twonm1
IF(imod == 1) THEN
  lmask =.TRUE.
ELSE IF(imod == 0) THEN
  lmask =.FALSE.
  ladres=.FALSE.
  GO TO 9999
ELSE
  WRITE(6,*)'S: TSTMSK INVALID DECODING OF VMSK',vmsk
  WRITE(6,*)'s: ... imod=',imod,'     NMASK=',nmask
END IF

!-------------------------------------------------------------------
! Check option codes
!-------------------------------------------------------------------
lmask=.TRUE.
!-------------------------------------------------------------------
! Var reconfiguration now is not restricted to primary fields.
! Therefore need to perform on all sections.
!-------------------------------------------------------------------
IF ( var_recon ) THEN
  n17 = MOD((iopn(4)/10),10)
  IF (n17 /= 1) THEN
    lmask = .FALSE.
  END IF
END IF

IF((isec == 0) .OR. (isec == 31) .OR.                           &
   (isec == 32) .OR. (isec == 33) .OR.                          &
   (isec == 34) .OR. (isec == 36) .OR. (isec == 37)) THEN
! Atmosphere primary field
  IF(sum_iopn /= 0) THEN
    n2n1=MOD (iopn(1),100)
! Up to A_MAX_TRVARS=150 free tracers now allowed in section 33
! Up to A_MAX_UKCAVARS=150 UKCA tracers now allowed in section 34
! Up to 150 free tracer lbcs in section 36
! Up to 150 UKCA tracer lbcs in section 37
    IF (isec == 33 .OR. isec == 34 .OR. isec == 36              &
   .OR. isec == 37) THEN
      n2n1=MOD (iopn(1),1000)
    END IF
    n3  =MOD((iopn(1)/100),10)
    n4  =MOD((iopn(1)/1000),10)
    n5  =MOD((iopn(1)/10000),10)
    n6  =MOD( iopn(2),10)
    n7  =MOD((iopn(2)/10),10)
    n8  =MOD((iopn(2)/100),10)
! n10n9 is a 2 digit option code for CLASSIC aerosols
    n10n9 =MOD((iopn(2)/1000),100)
    n11 =MOD( iopn(3),10)
    n12 =MOD((iopn(3)/10),10)
    n13 =MOD((iopn(3)/100),10)
    n14 =MOD((iopn(3)/1000),10)
    n15 =MOD((iopn(3)/10000),10)
    n16 =MOD( iopn(4),10)
    n17 =MOD((iopn(4)/10),10)
    n18 =MOD((iopn(4)/100),10)
    n19 =MOD((iopn(4)/1000),10)
    n20 =MOD((iopn(4)/10000),10)
    n21 =MOD( iopn(5),10)
    n22 =MOD((iopn(5)/10),10)
    n23 =MOD((iopn(5)/100),10)
    n24 =MOD((iopn(5)/1000),10)
    n25 =MOD((iopn(5)/10000),10)
    n26 =MOD( iopn(6),10)
    n27 =MOD((iopn(6)/10),10)
    n28 =MOD((iopn(6)/100),10)
    n29 =MOD((iopn(6)/1000),10)
    n30 =MOD((iopn(6)/10000),10)
    IF ((n2n1 == 99) .AND.                                    &
        (isec /= 33) .AND. (isec /= 34) .AND.                 &
        (isec /= 36) .AND. (isec /= 37)) THEN
      lmask=.FALSE.

    ELSE IF (isec == 33 .OR. isec == 36) THEN
      IF (n2n1 > 0 .AND. n2n1 <= a_max_trvars) THEN
        IF ((isec == 33 .AND. .NOT. tracer_a(n2n1)) .OR.      &
            (isec == 36 .AND. tca_lbc(n2n1) == 0)) THEN
          lmask=.FALSE.
        END IF
      END IF

    ELSE IF (isec == 34 .OR. isec == 37) THEN
      IF (n2n1 > 0 .AND. n2n1 <= a_max_ukcavars) THEN
        IF ((isec == 34 .AND. .NOT. tr_ukca_a(n2n1)) .OR.     &
            (isec == 37 .AND. tc_lbc_ukca(n2n1) == 0)) THEN
          lmask=.FALSE.
        END IF
      END IF

! Make n30 a metacharacter to allow more option codes
    ELSE IF (n30 == 0) THEN

!             n3=1 not to be used because of tracers
!             n3=2 not to be used
      IF((n3 == 5).AND.(.NOT.l_top)) THEN
        lmask=.FALSE.   ! TOPMODEL hydrology
      ELSE IF((n4 == 1).AND.(.NOT.l_use_biogenic)) THEN
        lmask=.FALSE.   ! biogenic aerosol
      ELSE IF((n4 == 2).AND.(.NOT.l_use_arclbiom)) THEN
        lmask=.FALSE.   ! biomass burning aerosol
      ELSE IF((n4 == 3).AND.(.NOT.l_use_arclblck)) THEN
        lmask=.FALSE.   ! black carbon aerosol
      ELSE IF((n4 == 4).AND.(.NOT.l_use_arclsslt)) THEN
        lmask=.FALSE.   ! sea salt aerosol
      ELSE IF((n4 == 5).AND.(.NOT.l_use_arclsulp)) THEN
        lmask=.FALSE.   ! sulphate aerosol
      ELSE IF((n4 == 6).AND.(.NOT.l_use_arcldust)) THEN
        lmask=.FALSE.   ! dust aerosol
      ELSE IF((n4 == 7).AND.(.NOT.l_use_arclocff)) THEN
        lmask=.FALSE.   ! organic carbon fossil fuel
      ELSE IF((n4 == 8).AND.(.NOT.l_use_arcldlta)) THEN
        lmask=.FALSE.   ! delta aerosol
      ELSE IF((n4 > 8)) THEN
        lmask=.FALSE.  ! these are not yet defined
      ELSE IF((n5 == 1).AND.(orogr == 'N')) THEN
        lmask=.FALSE.   ! orographic roughness
      ELSE IF((n5 == 2).AND.(.NOT.h_orog_grad)) THEN
        lmask=.FALSE.   ! orographic gradient
      ELSE IF((n5 == 3).AND.(.NOT.l_use_grad_corr)) THEN
        lmask=.FALSE.   ! gradient correction for SW radiation
      ELSE IF((n5 == 4).AND.(.NOT.l_orog_unfilt)) THEN
        lmask=.FALSE.   ! unfiltered orography for horizon angles
      ELSE IF((n6 == 1) .AND.                                   &
          (rimwidtha(rima_type_norm) == 0 .OR. h_global(a_im) == 'Y')) THEN
        lmask=.FALSE.   ! limited area model boundary condition
      ELSE IF((n6 == 2).AND.(h_floor == 'N')) THEN
        lmask=.FALSE.   ! lower boundary - growing orography (redundant)
      ELSE IF((n7 == 1).AND.(.NOT.l_oasis)) THEN
        lmask=.FALSE.   ! OASIS coupling to ocean model
      ELSE IF((n7 == 2)) THEN
        lmask=.FALSE.   ! other coupling fields currently excluded
      ELSE IF((n7 == 3).AND.(.NOT.(l_oasis.AND.l_oasis_icecalve))) THEN
        lmask=.FALSE.   ! oasis iceberg calving
      ELSE IF((n7 == 7).AND.(.NOT.(l_dms_ointer))) THEN
        lmask=.FALSE.   ! coupling for DMS ocean flux
      ELSE IF((n8 == 1).AND.(.NOT.l_sstanom)) THEN
        lmask=.FALSE.   ! SST anomaly
      ELSE IF((n8 == 2).AND.(iscrntdiag /= 2)) THEN
        lmask=.FALSE.
      ELSE IF((n8 == 3).AND.(isrfexcnvgust == 0)) THEN
        lmask=.FALSE.   ! effect of convective downdraughts on surface exchange
      ELSE IF((n8 == 4).AND.(.NOT.l_murk)) THEN
        lmask=.FALSE.   ! total aerosol (murk) for visibility
      ELSE IF((n8 == 5).AND.(.NOT.l_murk_source)) THEN
        lmask=.FALSE.   ! total aerosol emissions
      ELSE IF((n8 == 6).AND.(.NOT.l_snow_albedo)                &
                       .AND.(nsmax == 0)) THEN
        lmask=.FALSE.   ! snow albedo
      ELSE IF((n8 == 7).AND.(atmos_sr(3) /= '1A')) THEN
        lmask=.FALSE.   ! tke closure
      ELSE IF ( ( n8  ==  8) .AND. (atmos_sr(14) == '0A') ) THEN
        lmask=.FALSE.   ! energy adjustment scheme

!
! n10n9 is used for all CLASSIC aerosol related prognostics
! Sulphur cycle (1-19)
      ELSE IF((n10n9 == 1).AND.(.NOT.l_sulpc_so2)) THEN
        lmask=.FALSE.   ! sulphur dioxide cycle
      ELSE IF((n10n9 == 2).AND.((.NOT.l_sulpc_so2)                &
                        .OR.  (.NOT.l_so2_surfem)) ) THEN
        lmask=.FALSE.   ! surface SO2 emissions
      ELSE IF((n10n9 == 3).AND.((.NOT.l_sulpc_so2)                &
                        .OR.  (.NOT.l_so2_hilem)) ) THEN
        lmask=.FALSE.   ! high level SO2 emissions
      ELSE IF((n10n9 == 4).AND.((.NOT.l_sulpc_so2)                &
                        .OR.  (.NOT.l_so2_natem)) ) THEN
        lmask=.FALSE.   ! natural SO2 emissions
      ELSE IF((n10n9 == 5).AND.((.NOT.l_sulpc_so2)                &
                        .OR.  (.NOT.l_sulpc_dms)) ) THEN
        lmask=.FALSE.   ! dimethyl sulphide in SO2 cycle
      ELSE IF((n10n9 == 6).AND.((.NOT.l_sulpc_so2)                &
                        .OR.  (.NOT.l_sulpc_dms)                  &
                        .OR.  (.NOT.l_dms_em)) ) THEN
        lmask=.FALSE.   ! dimethyl sulphide emissions
      ELSE IF((n10n9 == 7).AND.((.NOT.l_sulpc_so2)                &
                        .OR.  (l_sulpc_online_oxidants)           &
                        .OR.  (.NOT.l_sulpc_ozone)) ) THEN
        lmask=.FALSE.   ! offline ozone oxidant in SO2 cycle
      ELSE IF((n10n9 == 8).AND.((.NOT.l_sulpc_so2)                &
                        .OR.  (.NOT.l_sulpc_ozone)                &
                        .OR.  (.NOT.l_sulpc_nh3)) )  THEN
        lmask=.FALSE.   ! ozone and ammonia in SO2 cycle
      ELSE IF((n10n9 == 9).AND.((.NOT.l_sulpc_so2)                &
                        .OR.  (.NOT.l_sulpc_ozone)                &
                        .OR.  (.NOT.l_sulpc_nh3)                  &
                        .OR.  (.NOT.l_nh3_em)) )  THEN
        lmask=.FALSE.   ! ammonia emissions and O3 in SO2 cycle
      ELSE IF((n10n9 == 10).AND.((.NOT.l_sulpc_so2)               &
                        .OR.  (l_sulpc_online_oxidants)) )  THEN
        lmask=.FALSE.   ! offline oxidants (not ozone)
! Soot (21 - 23)
      ELSE IF((n10n9 == 21).AND.(.NOT.l_soot))  THEN
        lmask=.FALSE.   ! soot scheme
      ELSE IF((n10n9 == 22).AND.((.NOT.l_soot)                    &
                        .OR.  (.NOT.l_soot_surem)) )  THEN
        lmask=.FALSE.   ! soot scheme with surface emissions
      ELSE IF((n10n9 == 23).AND.((.NOT.l_soot)                    &
                       .OR.  (.NOT.l_soot_hilem)) )  THEN
        lmask=.FALSE.   ! soot scheme with high level emissions
! Biomass (24 - 26)
      ELSE IF ((n10n9 == 24).AND.(.NOT.l_biomass)) THEN
        lmask=.FALSE.   ! biomass scheme
      ELSE IF ((n10n9 == 25).AND.((.NOT.l_biomass)                 &
                       .OR. (.NOT.l_bmass_surem)) ) THEN
        lmask=.FALSE.   ! biomass scheme with surface emissions
      ELSE IF ((n10n9 == 26).AND.((.NOT.l_biomass)                 &
                         .OR. (.NOT.l_bmass_hilem)) ) THEN
        lmask=.FALSE.   ! biomass scheme with high level emissions
! 2 bin and 6 bin Mineral dust
      ELSE IF ((n10n9 == 27).AND.(.NOT.l_dust)) THEN
        lmask=.FALSE.   ! mineral dust scheme (prognostic) 
! 6 bin (and currently also 2 bin) mineral dust diagnosis
      ELSE IF ((n10n9 == 28).AND.(.NOT.l_dust).AND.(.NOT.l_dust_diag)) THEN  
        lmask=.FALSE.   ! mineral dust scheme (diagnostic lifting only) 
! Nitrate (31)
      ELSE IF ((n10n9 == 31).AND.(.NOT.l_nitrate)) THEN
        lmask=.FALSE.   ! nitrate scheme
! OCFF (35-37)
      ELSE IF ((n10n9 == 35).AND.(.NOT.l_ocff)) THEN
        lmask=.FALSE.   ! organic carbon fossil fuel scheme
      ELSE IF ((n10n9 == 36).AND.((.NOT.l_ocff)                    &
                         .OR. (.NOT.l_ocff_surem)) ) THEN
        lmask=.FALSE.   ! OCFF with surface emissions
      ELSE IF ((n10n9 == 37).AND.((.NOT.l_ocff)                    &
                         .OR. (.NOT.l_ocff_hilem)) ) THEN
        lmask=.FALSE.   ! OCFF with high level emissions
! 6 bin only Mineral dust
      ELSE IF ((n10n9 == 38).AND.(l_twobin_dust                    &
                         .OR. (.NOT.l_dust)) ) THEN
        lmask=.FALSE.   ! mineral dust scheme (prognostic) 
! End of classic aerosol section

      ELSE IF (n11 == 1) THEN
        lmask=.FALSE.   ! basic vegetation scheme, no longer supported
      ELSE IF((n11 == 2).AND.(.NOT.l_veg_fracs)) THEN
        lmask=.FALSE.   ! fractional vegetation scheme
      ELSE IF((n11 == 3).AND.                                   &
              (.NOT.l_veg_fracs.OR..NOT.l_triffid) ) THEN
        lmask=.FALSE.   ! TRIFFID vegetation scheme
      ELSE IF((n11 == 4).AND.((.NOT.(l_veg_fracs.AND.l_albedo_obs)).OR.      &
              l_spec_albedo)) THEN
        lmask=.FALSE.   ! SW albedo_obs
      ELSE IF((n11 == 5).AND.(.NOT.(l_veg_fracs.AND.l_albedo_obs.AND.        &
              l_spec_albedo))) THEN
        lmask=.FALSE.   ! VIS and NIR albedo_obs
      ELSE IF( (n11 == 6) .AND.                                              &
        ( .NOT.(l_aggregate.AND.(i_aggregate_opt==1)) ) ) THEN                
        lmask=.FALSE.   ! No separate prognostic for thermal roughness
      ELSE IF((n12 == 3) .AND. (.NOT.l_mcr_qcf2)) THEN
        lmask=.FALSE.   ! QCF2 is prognostic
      ELSE IF((n12 == 4) .AND. (.NOT.l_mcr_qrain)) THEN
        lmask=.FALSE.   ! QRAIN is prognostic
      ELSE IF((n12 == 5) .AND. (.NOT.l_mcr_qgraup)) THEN
        lmask=.FALSE.   ! QGRAUP is prognostic
      ELSE IF((n12 == 6) .AND. (.NOT.l_pc2)) THEN
        lmask=.FALSE.   ! PC2 clod fraction boundary values out (secn.32)
      ELSE IF ((n13 == 1).AND.(l_3d_cca)) THEN
        lmask=.FALSE.    !  convective cloud amount is 2D
      ELSE IF ((n13 == 2).AND.(.NOT.l_3d_cca)) THEN
        lmask=.FALSE.    !  convective cloud amount is 3D
      ELSE IF ((n13 == 3).AND.(.NOT.l_ccrad)) THEN
        lmask=.FALSE.   ! CCRad scheme
      ELSE IF ((n13 == 5).AND.(.NOT.(l_conv_hist))) THEN
        lmask=.FALSE.   ! fields for convection history
      ELSE IF ((n15 == 3).AND.(.NOT.l_co2_interactive)) THEN
        lmask=.FALSE.   ! carbon cycle scheme
      ELSE IF ((n16 == 2).AND.(nsmax==0)) THEN
        lmask=.FALSE.   ! JULES snow scheme with multiple snow layers
      ELSE IF ((n16 == 3).AND.(.NOT.l_snow_albedo)) THEN
        lmask=.FALSE.   ! snow soot
      ELSE IF ((n16 == 4).AND.(.NOT.l_urban2t)) THEN
        lmask=.FALSE.   ! URBAN-2T schemes including MORUSES
      ELSE IF ((n16 == 5).AND.(.NOT.l_flake_model)) THEN
        lmask=.FALSE.   ! FLake lake scheme
! n17 is left out here, as it is for all sections not just prognostics.
      ELSE IF ((n18 == 1).AND.(.NOT.l_ctile)) THEN
        lmask=.FALSE.   ! coastal tiling scheme
      ELSE IF ((n18 == 3).AND.(.NOT.l_rivers)) THEN
        lmask=.FALSE.   ! river routing scheme
      ELSE IF ((n18 == 4).AND.(.NOT.l_inland)) THEN
        lmask=.FALSE.   ! inland re-routing scheme
      ELSE IF ((n18 == 6).AND.(nice_use > 1)) THEN
        lmask=.FALSE.   ! single sea ice category
      ELSE IF ((n18 == 7).AND.(nice == 1)) THEN
        lmask=.FALSE.   ! sea ice categories
      ELSE IF ((n18 == 8).AND.(nice_use == 1)) THEN
        lmask=.FALSE.   ! sea ice categories used fully 
      ELSE IF ((n18 == 9).AND.(.NOT.l_sice_multilayers)) THEN
        lmask=.FALSE.   ! multilayer sea ice scheme
      ELSE IF ((n19 == 1).AND.(.NOT.l_use_tpps_ozone)) THEN
        lmask=.FALSE.   ! tropopause ozone scheme
      ELSE IF ((n20 == 1).AND.(can_model /= 4)                  &
                         .AND.(nsmax == 0)) THEN
        lmask=.FALSE.   ! snow canopy scheme
      ELSE IF (n25 == 3) THEN
        lmask=.FALSE.   ! Direct PAR prognostic not currently used by any scheme
      ELSE IF ((n28 == 1).AND.(.NOT.l_use_cariolle)) THEN
        lmask=.FALSE.   ! cariolle ozone scheme
      ELSE IF ((n29 == 1).AND.(.NOT.l_endgame)) THEN
        lmask=.FALSE.   ! Disable ENDGame prognostics in ND run
      END IF

    ELSE IF (n30 == 1) THEN
! Can be used for a new set of option codes with n30=1
! when those with n30=0 are fully used up.
    END IF ! n30
  END IF ! SUM_IOPN

END IF ! isec

! Print out details of any tracer boundary conditions.
IF ( (isec == 36) .AND. printstatus >= prstatus_diag) THEN
  WRITE(6,*) 'TSTMSK:',isec,n2n1,tca_lbc(n2n1),lmask
END IF
IF ( (isec == 37)  .AND. printstatus >= prstatus_diag) THEN
  WRITE(6,*) 'TSTMSK:',isec,n2n1,tc_lbc_ukca(n2n1),lmask
END IF
! End of atmos primary block

! Atmos diagnostics
IF (isec == 1) THEN
! Shortwave radiation
  IF (sum_iopn /= 0) THEN
    n1 = MOD (iopn(1)    ,10)
    n2 = MOD((iopn(1)/10),10)
    n3 = MOD((iopn(1)/100),10)
    n4 = MOD((iopn(1)/1000),10)
    n5 = MOD((iopn(1)/10000),10)
    n6 = MOD( iopn(2),10)
    IF ((n1 == 1).AND.(h_global(a_im) /= 'Y')) THEN
      lmask=.FALSE.
    ELSE IF ((n4  ==  1).AND.(.NOT. l_use_sulpc_indirect_sw)) THEN
      lmask = .FALSE.
    ELSE IF ((n5  ==  1).AND.                                     &
            (.NOT.(l_use_seasalt_direct                           &
              .OR. l_use_seasalt_indirect))) THEN
      lmask = .FALSE.
    ELSE IF ( ( n6  ==  1 ) .AND. ( .NOT. l_pc2 ) ) THEN
      lmask = .FALSE.
    END IF
  END IF

ELSE IF (isec == 2) THEN
! Longwave radiation
  IF (sum_iopn /= 0) THEN
    n1 = MOD(iopn(1)   ,10)
    IF ((n1 == 1).AND.                                            &
        (i_ozone_int /= io3_trop_map_masscon)) THEN
      lmask=.FALSE.
    END IF
    n6 = MOD( iopn(2),10)
    IF ( ( n6  ==  1 ) .AND. ( .NOT. l_pc2 ) ) THEN
      lmask = .FALSE.
    END IF
  END IF

ELSE IF (isec == 3) THEN
! Boundary layer
  IF (sum_iopn /= 0) THEN
    n1 = MOD (iopn(1)    ,10)
    n2 = MOD((iopn(1)/10),10)
    n3 = MOD((iopn(1)/100),10)
    n4 = MOD((iopn(1)/1000),10)
    n6 = MOD( iopn(2),10)
    n7  =MOD((iopn(2)/10),10)
    IF ((n1 == 1).AND.(orogr == 'N')) THEN
      lmask=.FALSE.
    END IF
    IF ((n2 == 1).AND.(.NOT.l_sulpc_so2)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 2).AND.(.NOT.l_sulpc_nh3)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 3).AND.(.NOT.l_soot)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 4).AND.(.NOT.l_biomass)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 5).AND.(.NOT.l_dust)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 6).AND.(.NOT.l_ocff)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 7).AND.(.NOT.l_nitrate)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 8).AND.(.NOT.l_dust).AND.(.NOT.l_dust_diag)) THEN
      lmask=.FALSE.
    END IF
    IF ((n3 == 1).AND.(.NOT.l_co2_interactive)) THEN
      lmask=.FALSE.
    ELSE IF ( ( n6  ==  1 ) .AND. ( .NOT. l_pc2 ) ) THEN
      lmask = .FALSE.
    END IF
    IF ((n4 == 1) .AND. (nice == 1))THEN
      lmask = .FALSE.
    ELSEIF ((n4 == 2) .AND. (nice_use == 1)) THEN
      lmask = .FALSE.
    END IF
    ! 2 and 6 bin dust fields
    IF ((n7 == 1) .AND. (.NOT. l_dust)) THEN
      lmask = .FALSE.
    ! 6 bin dust only fields
    ELSE IF ((n7 == 2) .AND. (l_twobin_dust              &
                        .OR. (.NOT.l_dust)) ) THEN
      lmask = .FALSE.
    END IF
  END IF

ELSE IF(isec == 4) THEN
! Large-scale precipitation
  IF (sum_iopn /= 0) THEN
    n2 = MOD((iopn(1)/10),10)
    n6 = MOD( iopn(2),10)
    n7  =MOD((iopn(2)/10),10)
    IF ((n2 == 1).AND.(.NOT.l_sulpc_so2)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 2).AND.(.NOT.l_sulpc_nh3)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 3).AND.(.NOT.l_soot)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 4).AND.(.NOT.l_biomass)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 5).AND.(.NOT.l_dust)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 6).AND.(.NOT.l_ocff)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 7).AND.(.NOT.l_nitrate)) THEN
      lmask=.FALSE.
    END IF
    IF ( ( n6  ==  1 ) .AND. ( .NOT. l_pc2 ) ) THEN
      lmask = .FALSE.
    END IF
    ! 2 and 6 bin dust fields
    IF ((n7 == 1) .AND. (.NOT. l_dust)) THEN
      lmask = .FALSE.
    ! 6 bin dust only fields
    ELSE IF ((n7 == 2) .AND. (l_twobin_dust              &
                        .OR. (.NOT.l_dust)) ) THEN
      lmask = .FALSE.
    END IF
  END IF

ELSE IF (isec == 5) THEN
! Convection
  IF (sum_iopn /= 0) THEN
    n2 = MOD((iopn(1)/10),10)
    n3 = MOD((iopn(1)/100),10)
    n6 = MOD( iopn(2),10)
    n7  =MOD((iopn(2)/10),10)
    IF ((n2 == 1).AND.(.NOT.l_sulpc_so2)) THEN
      lmask=.FALSE.
    ELSE IF ( (n3 == 1).AND.(l_3d_cca) ) THEN
      lmask=.FALSE.
    ELSE IF ( (n3 == 2).AND.(.NOT.l_3d_cca) ) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 2).AND.(.NOT.l_sulpc_nh3)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 3).AND.(.NOT.l_soot)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 4).AND.(.NOT.l_biomass)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 5).AND.(.NOT.l_dust)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 6).AND.(.NOT.l_ocff)) THEN
      lmask=.FALSE.
    ELSE IF ((n2 == 7).AND.(.NOT.l_nitrate)) THEN
      lmask=.FALSE.
    END IF
    IF ( ( n6  ==  1 ) .AND. ( .NOT. l_pc2 ) ) THEN
      lmask = .FALSE.
    END IF
    ! 2 and 6 bin dust fields
    IF ((n7 == 1) .AND. (.NOT. l_dust)) THEN
      lmask = .FALSE.
    ! 6 bin dust only fields
    ELSE IF ((n7 == 2) .AND. (l_twobin_dust              &
                        .OR. (.NOT.l_dust)) ) THEN
      lmask = .FALSE.
    END IF
  END IF
  n4 = MOD((iopn(1)/1000),10) 
  IF ( (n4 /= 1).AND.(.NOT. l_param_conv) ) THEN
    lmask=.FALSE.
  ENDIF

ELSE IF (isec == 6) THEN
! Gravity Wave Drag parametrization
  IF (sum_iopn /= 0) THEN
    n2 = MOD((iopn(1)/10),10)
    n3 = MOD((iopn(1)/100),10)
    IF ((n2 == 1).AND.(.NOT.l_gwd)) THEN
      lmask=.FALSE.
    ELSE IF ((n3 == 1).AND.(.NOT.l_use_ussp)) THEN
      lmask=.FALSE.
    END IF
  END IF

ELSE IF(isec == 8) THEN
! Hydrology
  IF (sum_iopn /= 0) THEN
    n1 = MOD (iopn(1),10)
    n22 = MOD((iopn(5)/10),10)
    IF ((n1 == 1).AND.(.NOT.l_top)) THEN
      lmask=.FALSE.
    ELSE IF ((n22 == 1).AND.(.NOT.l_rivers)) THEN
          lmask=.FALSE.    !River routing switched off
    ELSE IF ((n22 == 2).AND.(.NOT.l_inland)) THEN
          lmask=.FALSE.    !Inland re-routing switched off
    END IF
  END IF

ELSE IF(isec == 9) THEN
! Cloud parametrization
  IF (sum_iopn /= 0) THEN
    n2 = MOD((iopn(1)/10),10)
    n3 = MOD((iopn(1)/100),10)
    n6 = MOD( iopn(2),10)
    IF ((n2 == 1).AND.(.NOT.l_cld_area)) THEN
      lmask=.FALSE.
    ELSE IF ((n3 == 1).AND.(.NOT.l_rhcpt)) THEN
      lmask=.FALSE.
    ELSE IF ( ( n6  ==  1 ) .AND. ( .NOT. l_pc2 ) ) THEN
      lmask = .FALSE.
    END IF
  END IF

ELSE IF(isec == 12) THEN
! Dynamics Advection
  IF (sum_iopn /= 0) THEN
    n6 = MOD( iopn(2),10)
    IF ( ( n6  ==  1 ) .AND. ( .NOT. l_pc2 ) ) THEN
      lmask = .FALSE.
    END IF
  END IF

ELSE IF(isec == 16) THEN
! Extra physics
  IF (sum_iopn /= 0) THEN
    n2n1 = MOD(iopn(1),100)
    n6 = MOD( iopn(2),10)
    IF ((n2n1 /= 0).AND.(.NOT.tracer_a(n2n1))) THEN
      lmask=.FALSE.
    END IF
    IF ( ( n6  ==  1 ) .AND. ( .NOT. l_pc2 ) ) THEN
      lmask = .FALSE.
    END IF
  END IF

ELSE IF (isec == 17) THEN
! CLASSIC Aerosol section
  IF (sum_iopn /= 0) THEN
    n1 = MOD(iopn(1),     10)
    n2 = MOD((iopn(1)/10),10)
    ! Sulphur cycle diagnostics
    IF ((n1 == 1).AND.(.NOT.l_sulpc_so2)) THEN
      lmask=.FALSE.
    ! Soot diagnostics
    ELSE IF ((n1 == 2).AND.(.NOT.l_soot)) THEN
      lmask=.FALSE.
    ! Biomass aerosol diagnostics
    ELSE IF ((n1 == 3).AND.(.NOT.l_biomass)) THEN
      lmask=.FALSE.
    ! Dust aerosol diagnostics
    ELSE IF ((n1 == 4).AND.(.NOT.l_dust)) THEN
      lmask=.FALSE.
    ! OCFF aerosol diagnostics
    ELSE IF ((n1 == 5).AND.(.NOT.l_ocff)) THEN
      lmask=.FALSE.
    ! SOA aerosol diagnostics
    ELSE IF ((n1 == 6).AND.(.NOT.l_use_biogenic)) THEN
      lmask=.FALSE.
    ! Sea-salt PM diagnostics
    ELSE IF ((n1 == 7).AND.(.NOT.l_use_seasalt_pm)) THEN
      lmask=.FALSE.
    ! Nitrate aerosol diagnostics
    ELSE IF ((n1 == 8).AND.(.NOT.l_nitrate)) THEN
      lmask=.FALSE.
    ! aerosol PM10/PM2.5 diagnostics - any of the above mean it is available
    ELSE IF ((n1 == 9).AND.(.NOT.                                   &
           (l_sulpc_so2      .OR. l_soot .OR. l_biomass      .OR.   &
            l_dust           .OR. l_ocff .OR. l_use_biogenic .OR.   &
            l_use_seasalt_pm .OR. l_nitrate) ) ) THEN
      lmask=.FALSE.
    END IF
    ! The following option codes are usually used 
    ! in conjunection with n1 ==1
    ! DMS diagnostics 
    IF ((n2 == 1).AND.(.NOT.l_sulpc_dms)) THEN
      lmask=.FALSE.
    ! Diagnostics which depend on oxidation of sulphur by ozone
    ELSE IF ((n2 == 2).AND.(.NOT.l_sulpc_ozone)) THEN
      lmask=.FALSE.
    END IF
  END IF

ELSE IF(isec == 18) THEN
! Data assimilation
  IF (sum_iopn /= 0) THEN
    n1 = MOD (iopn(1),10)
    n2 = MOD((iopn(1)/10),10)
  END IF

ELSE IF ((isec >= 1).AND.(isec <= 20)) THEN  ! BUT NOT 1,3,18
  IF (sum_iopn /= 0) THEN
    WRITE(6,*)                                                    &
   'MESSAGE FROM ROUTINE TSTMSK: UNEXPECTED OPTION CODE',         &
    iopn(1)
    WRITE(6,*)'IN ATMOSPHERE SECTION ',isec
  END IF

ELSE IF ((isec >= 21).AND.(isec <= 24)) THEN
! Atmos climate mean diagnostics - redundant

ELSE IF (isec  ==  26) THEN
!  River Routing Diagnostics
  IF (sum_iopn /= 0) THEN
    n22 = MOD((iopn(5)/10),10)
    IF ((n22 == 1).AND.(.NOT.l_rivers)) THEN
      lmask=.FALSE.    !  River routing switched off
    ELSE IF ((n22 == 2).AND.(.NOT.l_inland)) THEN
      lmask=.FALSE.    !Inland re-routing switched off
    END IF
  END IF
END IF
! End of atmos diagnostic block

IF(lmask) THEN
  ladres=.TRUE.
  IF ((ispace == 3).OR.(ispace == 5)) THEN
    lmask=.FALSE.
  END IF
ELSE
  ladres=.FALSE.
END IF

 9999 CONTINUE

IF (lhook) CALL dr_hook('TSTMSK',zhook_out,zhook_handle)
RETURN

END SUBROUTINE tstmsk
