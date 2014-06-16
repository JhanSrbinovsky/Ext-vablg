! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!LL  SUBROUTINE SET_ATM_POINTERS ---------------------------------------
!LL
!LL  Set pointers for primary atmosphere fields
!LL
!LL Programming Standard: Unified Model DP NO. 3, Version 8.3
!LL
!LL  Purpose:   Sets integer pointers to atmospheric
!LL             variables from STASHIN addresses.
!LL
!LL  External documentation: UMDP NO. C4 
!LL
!LLEND-------------------------------------------------------------
!
!  Code Owner: See Unified Model Code Owners HTML page
!  This file belongs in section: Top Level

      SUBROUTINE SET_ATM_POINTERS(                                      &
#include "argduma.h"
#include "argsts.h"
#include "argptra.h"
                        ICODE,CMESSAGE)

      use cable_data_mod, only : cable_set_atm_pointers
      USE yomhook, ONLY: lhook, dr_hook
      USE parkind1, ONLY: jprb, jpim
      USE ukca_d1_defs, ONLY: ukca_item_sulpc, ukca_sect
      USE clmchfcg_scenario_mod, ONLY: nsulpat
      USE atm_fields_bounds_mod
      USE UM_ParVars
      USE Control_Max_Sizes
      USE um_input_control_mod, ONLY:                                  &
           l_sulpc_online_oxidants
      USE rad_input_mod, ONLY: lexpand_ozone, lexpand_tpps_ozone
      USE lookup_addresses
      USE cv_run_mod, ONLY: l_3d_cca, l_ccrad
      USE ukca_option_mod, ONLY: l_ukca
      USE Submodel_Mod


      USE chsunits_mod, ONLY : nunits

      IMPLICIT NONE
!L
!*L Arguments
!L
#include "typsize.h"
#include "typduma.h"
#include "typsts.h"
#include "typptra.h"
      INTEGER                                                           &
          ICODE                  ! OUT: Error return code
!
      CHARACTER(LEN=80)                                                      &
          CMESSAGE               ! OUT: Error return message
#include "ccontrol.h"
#include "c_mdi.h"
#include "ctracera.h"
#include "cruntimc.h"

! local variables

      INTEGER                                                           &
              IVAR,                                                     &
                                 ! Loop counts
              JVAR,                                                     &
                                 ! Loop counts
              IFLD,                                                     &
              LEV                                                       &
             ,im_ident                                                  &
                            !  Internal Model Identifier
             ,im_index                                                  &
                            !  Internal Model Index in Stash arrays
             ,Sect_No       !  Stash section number

      INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
      INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
      REAL(KIND=jprb)               :: zhook_handle

!     Set to atmosphere internal model
      IF (lhook) CALL dr_hook('SET_ATM_POINTERS',zhook_in,zhook_handle)
      im_ident  = atmos_im
      im_index  = internal_model_index(im_ident)
      Sect_No   = 0


! Set pointers for atmospheric primary variables from STASH :-

      JU(udims%k_start)                = SI(  2,Sect_No,im_index)
      JV(vdims%k_start)                = SI(  3,Sect_No,im_index)
      JTHETA(tdims%k_start)            = SI(  4,Sect_No,im_index)
      JQ    (qdims%k_start)            = SI( 10,Sect_No,im_index)
      JQCF  (qdims%k_start)            = SI( 12,Sect_No,im_index)
      JTSTAR                           = SI( 24,Sect_No,im_index)
      JLAND                            = SI( 30,Sect_No,im_index)
      JOROG                            = SI( 33,Sect_No,im_index)
      JW(wdims_s%k_start)              = SI(150,Sect_No,im_index)
      JRHO   (pdims%k_start)           = SI(253,Sect_No,im_index)
      JQCL   (qdims%k_start)           = SI(254,Sect_No,im_index)
      JEXNER_RHO_LEVELS(pdims%k_start) = SI(255,Sect_No,im_index)
      JQCF2  (qdims%k_start)           = SI(271,Sect_No,im_index)
      JQRAIN (qdims%k_start)           = SI(272,Sect_No,im_index)
      JQGRAUP(qdims%k_start)           = SI(273,Sect_No,im_index)

!     ENDGame prognostics
      JTHETAV(tdims%k_start)           = SI(388,Sect_No,im_index)
      JDRYRHO(pdims%k_start)           = SI(389,Sect_No,im_index)
      JETADOT(tdims%k_start)           = SI(387,Sect_No,im_index)
      JPSIWS                           = SI(390,Sect_No,im_index)
      JPSIWL                           = SI(397,Sect_No,im_index)
      JEXNERSURF                       = SI(398,Sect_No,im_index)
      JMV(qdims%k_start)               = SI(391,Sect_No,im_index)
      JMCL(qdims%k_start)              = SI(392,Sect_No,im_index)
      JMCF(qdims%k_start)              = SI(393,Sect_No,im_index)
      JMCF2(qdims%k_start)             = SI(396,Sect_No,im_index)
      JMRAIN(qdims%k_start)            = SI(394,Sect_No,im_index)
      JMGRAUP(qdims%k_start)           = SI(395,Sect_No,im_index)

!     for TKE based turbulence scheme
      JE_TRB  (tdims%k_start)          = SI(70,Sect_No,im_index)
      JTSQ_TRB(tdims%k_start)          = SI(71,Sect_No,im_index)
      JQSQ_TRB(tdims%k_start)          = SI(72,Sect_No,im_index)
      JCOV_TRB(tdims%k_start)          = SI(73,Sect_No,im_index)
      JZHPAR_SHCU                      = SI(74,Sect_No,im_index)



! Set extra pointers for coastal tiling.
      JFRAC_LAND     = SI(505,Sect_No,im_index)
      JTSTAR_LAND    = SI(506,Sect_No,im_index)
      JTSTAR_SEA     = SI(507,Sect_No,im_index)
      JTSTAR_SICE    = SI(508,Sect_No,im_index)
      JTSTAR_SICE_CAT= SI(441,Sect_No,im_index)
! Set pointers for seaice and land albedos
      JSICE_ALB      = SI(509,Sect_No,im_index)
      JLAND_ALB      = SI(510,Sect_No,im_index)
!
! Set extra pointers for large-scale hydrology.
      JTI_MEAN       = SI(274,Sect_No,im_index)
      JTI_SIG        = SI(275,Sect_No,im_index)
      JFEXP          = SI(276,Sect_No,im_index)
      JGAMMA_INT     = SI(277,Sect_No,im_index)
      JWATER_TABLE   = SI(278,Sect_No,im_index)
      JFSFC_SAT      = SI(279,Sect_No,im_index)
      JF_WETLAND     = SI(280,Sect_No,im_index)
      JSTHZW         = SI(281,Sect_No,im_index)
      JA_FSAT        = SI(282,Sect_No,im_index)
      JC_FSAT        = SI(283,Sect_No,im_index)
      JA_FWET        = SI(284,Sect_No,im_index)
      JC_FWET        = SI(285,Sect_No,im_index)
      JACC_LAKE_EVAP = SI(290,Sect_No,im_index) 


      DO LEV= 1, wdims_s%k_end
        JW(LEV)=JW(LEV-1)+theta_off_size
      END DO

      DO LEV= udims%k_start+1, udims%k_end
        JU(LEV)    =JU(LEV-1)     + u_off_size
      END DO

      DO LEV= vdims%k_start+1, vdims%k_end
        JV(LEV)    =JV(LEV-1)     + v_off_size
      END DO

      DO LEV= pdims%k_start+1, pdims%k_end
        JRHO(LEV)    =JRHO(LEV-1)      + theta_off_size
        JDRYRHO(LEV) =JDRYRHO(LEV-1)   + theta_off_size
      END DO

      DO LEV= tdims%k_start+1, tdims%k_end
        JTHETA(LEV) =JTHETA(LEV-1)  + theta_off_size
        JTHETAV(LEV)=JTHETAV(LEV-1) + theta_off_size
        JETADOT(LEV)=JETADOT(LEV-1) + theta_off_size
      END DO

      DO LEV= pdims%k_start+1, pdims%k_end+1
        JEXNER_RHO_LEVELS(LEV)=JEXNER_RHO_LEVELS(LEV-1)+theta_off_size
      END DO

      DO LEV= qdims%k_start+1, qdims%k_end
        JQ(LEV)    =JQ(LEV-1)     + theta_halo_size
        JQCL(LEV)  =JQCL(LEV-1)   + theta_halo_size
        JQCF(LEV)  =JQCF(LEV-1)   + theta_halo_size
        JQCF2(LEV)   =JQCF2(LEV-1)   + theta_halo_size
        JQRAIN(LEV)  =JQRAIN(LEV-1)  + theta_halo_size
        JQGRAUP(LEV) =JQGRAUP(LEV-1) + theta_halo_size
        JMV(LEV)     =JMV(LEV-1)     + theta_off_size
        JMCL(LEV)    =JMCL(LEV-1)    + theta_off_size
        JMCF(LEV)    =JMCF(LEV-1)    + theta_off_size
        JMCF2(LEV)   =JMCF2(LEV-1)   + theta_off_size
        JMRAIN(LEV)  =JMRAIN(LEV-1)  + theta_off_size
        JMGRAUP(LEV) =JMGRAUP(LEV-1) + theta_off_size
      END DO

      DO LEV= tdims%k_start+1, tdims%k_end
        JE_TRB(LEV) = JE_TRB(LEV-1) + theta_halo_size
        JTSQ_TRB(LEV) = JTSQ_TRB(LEV-1) + theta_halo_size
        JQSQ_TRB(LEV) = JQSQ_TRB(LEV-1) + theta_halo_size
        JCOV_TRB(LEV) = JCOV_TRB(LEV-1) + theta_halo_size
      ENDDO
! 
!     Convective downdraught mass-flux at cloud base
      jddmfx         = SI(493,Sect_No,im_index) 

      ! Pointers required to save coupling fields as
      ! prognostics when employing OASIS as the coupler.
      ! In non-coupled models these pointers will
      ! simply end up with a value of 1.
      JC_SOLAR = SI(171,Sect_No,im_index)
      JC_BLUE =  SI(172,Sect_No,im_index)
      JC_LONGWAVE =  SI(174,Sect_No,im_index)
      JC_TAUX =  SI(176,Sect_No,im_index)
      JC_TAUY =  SI(177,Sect_No,im_index)
      JC_SENSIBLE =  SI(179,Sect_No,im_index)
      IF (NICE_USE .EQ. 1) THEN
        JC_SUBLIM =  SI(180,Sect_No,im_index)  ! Single cat field
      ELSE
        JC_SUBLIM =  SI(182,Sect_No,im_index)  ! Multi cat field
      ENDIF
      JC_EVAP =  SI(181,Sect_No,im_index)
      JC_FCONDTOPN =  SI(184,Sect_No,im_index)
      JC_TOPMELTN =  SI(185,Sect_No,im_index)
      JC_LSRAIN =  SI(186,Sect_No,im_index)
      JC_LSSNOW =  SI(187,Sect_No,im_index)
      JC_CVRAIN =  SI(188,Sect_No,im_index)
      JC_CVSNOW =  SI(189,Sect_No,im_index)
      JC_CALVING =  SI(190,Sect_No,im_index)
      JC_W10 =  SI(191,Sect_No,im_index)
      JC_RIVEROUT =  SI(192,Sect_No,im_index)


! Set pointers for optional atmospheric primary variables.
        JZH                         = SI( 25,Sect_No,im_index)
        JU_ADV(udims%k_start)       = SI(256,Sect_No,im_index)
        JV_ADV(vdims%k_start)       = SI(257,Sect_No,im_index)
        JW_ADV(wdims_s%k_start)     = SI(258,Sect_No,im_index)
        JNTML                       = SI(259,Sect_No,im_index)
        JNBDSC                      = SI(260,Sect_No,im_index)
        JNTDSC                      = SI(261,Sect_No,im_index)
        JCUMULUS                    = SI(262,Sect_No,im_index)
        JT1_SD                      = SI(263,Sect_No,im_index)
        JQ1_SD                      = SI(264,Sect_No,im_index)
        JCF_AREA  (1)               = SI(265,Sect_No,im_index)
        JCF_BULK  (qdims%k_start)   = SI(266,Sect_No,im_index)
        JCF_LIQUID(qdims%k_start)   = SI(267,Sect_No,im_index)
        JCF_FROZEN(qdims%k_start)   = SI(268,Sect_No,im_index)

        IF (L_3D_CCA .OR. L_CCRAD) THEN
          JCCA(1) = SI(211,Sect_No,im_index)
          DO LEV=2, N_CCA_LEV      ! n_cca_lev set in dervsize 
            JCCA(LEV)=JCCA(LEV-1)+THETA_FIELD_SIZE
          ENDDO
        ELSE
          JCCA(1)      = SI( 13,Sect_No,im_index)
        ENDIF

        JCCB           = SI( 14,Sect_No,im_index)
        JCCT           = SI( 15,Sect_No,im_index)
        JCCLWP         = SI( 16,Sect_No,im_index)
        jdeepflag      = SI(342,Sect_No,im_index)
        jpastprecip    = SI(343,Sect_No,im_index)
        jpastconvht    = SI(344,Sect_No,im_index)
        JLCBASE        = SI( 21,Sect_No,im_index)
        JCANOPY_WATER  = SI( 22,Sect_No,im_index)
        JCCW_RAD(1)    = SI(212,Sect_No,im_index)

        DO LEV= 2, qdims%k_end
          JCCW_RAD(LEV) =JCCW_RAD(LEV-1)+THETA_FIELD_SIZE
          JCF_AREA(LEV) =JCF_AREA(LEV-1)+THETA_FIELD_SIZE
        ENDDO

      DO LEV= wdims_s%k_start+1, wdims_s%k_end
        JW_ADV(LEV)=JW_ADV(LEV-1)+theta_halo_size
      END DO

      DO LEV= udims%k_start+1, udims%k_end
        JU_ADV(LEV)=JU_ADV(LEV-1)+u_halo_size
      END DO

      DO LEV= vdims%k_start+1, vdims%k_end
        JV_ADV(LEV)=JV_ADV(LEV-1)+v_halo_size
      END DO

      DO LEV= qdims%k_start+1, qdims%k_end
        JCF_BULK(LEV)  =JCF_BULK(LEV-1)+THETA_halo_SIZE
        JCF_LIQUID(LEV)=JCF_LIQUID(LEV-1)+THETA_halo_SIZE
        JCF_FROZEN(LEV)=JCF_FROZEN(LEV-1)+THETA_halo_SIZE
      END DO

!  Set pointers for secondary fields in D1
      JEXNER_THETA_LEVELS(tdims%k_start) = SI(406,Sect_No,im_index)
      JP                 (pdims%k_start) = SI(407,Sect_No,im_index)
      JP_THETA_LEVELS    (tdims%k_start) = SI(408,Sect_No,im_index)
      JPSTAR                             = SI(409,Sect_No,im_index)
      JSW_INCS(0)                        = SI(410,Sect_No,im_index)
      JLW_INCS(0)                        = SI(411,Sect_No,im_index)

! This code should work for both ND and V-AT-POLES
      DO LEV= 1, model_levels+1 
        JSW_INCS(LEV)=JSW_INCS(LEV-1)+THETA_FIELD_SIZE
      END DO
      DO LEV= 1, model_levels
        JLW_INCS(LEV)=JLW_INCS(LEV-1)+THETA_FIELD_SIZE
      END DO

! Direct PAR flux 
      JDIRPAR               = SI(460,Sect_no,im_index)

      DO LEV= tdims%k_start+1, tdims%k_end
        JEXNER_THETA_LEVELS(LEV)=JEXNER_THETA_LEVELS(LEV-1)+            &
                                   theta_off_size
        JP_THETA_LEVELS(LEV)    =JP_THETA_LEVELS(LEV-1)+theta_off_size
      END DO

      DO LEV= pdims%k_start+1, pdims%k_end+1
        JP(LEV)                 =JP(LEV-1)+theta_off_size
      END DO

!  Set pointers for ancillary fields in D1 from STASH
!     Soil fields
      JSMCL(1)            = SI(  9,Sect_No,im_index)
      J_DEEP_SOIL_TEMP(1) = SI( 20,Sect_No,im_index)
      JVOL_SMC_WILT       = SI( 40,Sect_No,im_index)
      JVOL_SMC_CRIT       = SI( 41,Sect_No,im_index)
      JVOL_SMC_SAT        = SI( 43,Sect_No,im_index)
      JSAT_SOIL_COND      = SI( 44,Sect_No,im_index)
      JTHERM_CAP          = SI( 46,Sect_No,im_index)
      JTHERM_COND         = SI( 47,Sect_No,im_index)
      JSAT_SOILW_SUCTION  = SI( 48,Sect_No,im_index)
      JCLAPP_HORN         = SI(207,Sect_No,im_index)
      JSTHU(1)            = SI(214,Sect_No,im_index)
      JSTHF(1)            = SI(215,Sect_No,im_index)

      DO LEV=2,ST_LEVELS
        J_DEEP_SOIL_TEMP(LEV)=J_DEEP_SOIL_TEMP(LEV-1)+LAND_FIELD
      ENDDO

      DO LEV=2,SM_LEVELS
        JSMCL(LEV)=JSMCL(LEV-1)+LAND_FIELD
        JSTHU(LEV)=JSTHU(LEV-1)+LAND_FIELD
        JSTHF(LEV)=JSTHF(LEV-1)+LAND_FIELD
      END DO

!     Other surface fields
      JZ0          = SI( 26,Sect_No,im_index) ! roughness length (used for sea)
      JGS          = SI(213,Sect_No,im_index) ! stomatal conductance

! Orography fields
      JOROG_SIL      = SI(17,Sect_No,im_index)   ! Silhouette area
      JOROG_HO2      = SI(18,Sect_No,im_index)   ! Peak to trough ht.
      JOROG_SD       = SI(34,Sect_No,im_index)
      JOROG_GRAD_X   = SI( 5,Sect_No,im_index)
      JOROG_GRAD_Y   = SI( 6,Sect_No,im_index)
      JOROG_UNFILT   = SI( 7,Sect_No,im_index)
      JOROG_GRAD_XX  = SI(35,Sect_No,im_index)
      JOROG_GRAD_XY  = SI(36,Sect_No,im_index)
      JOROG_GRAD_YY  = SI(37,Sect_No,im_index)

! Sea/Sea Ice fields
      JU_SEA         = SI( 28,Sect_No,im_index)
      JV_SEA         = SI( 29,Sect_No,im_index)
      JICE_FRACTION  = SI( 31,Sect_No,im_index)
      JICE_THICKNESS = SI( 32,Sect_No,im_index)
      JTI            = SI( 49,Sect_No,im_index)
      JICE_FRACT_CAT = SI(413,Sect_No,im_index)
      JICE_THICK_CAT = SI(414,Sect_No,im_index)
      JTI_CAT        = SI(415,Sect_No,im_index)
      JICE_K_CAT     = SI(440,Sect_No,im_index)
      JU_0_P         = SI(269,Sect_No,im_index)
      JV_0_P         = SI(270,Sect_No,im_index)

! Snow fields
      JSNODEP        = SI( 23,Sect_No,im_index) ! Snow depth over land
      JSNODEP_SEA    = SI( 95,Sect_No,im_index) ! Snow depth on sea ice
      JSNODEP_SEA_CAT= SI(416,Sect_No,im_index) ! Snow depth on ice cats
      JSNSOOT        = SI(221,Sect_No,im_index) ! Snow soot content
      JCATCH_SNOW    = SI(241,Sect_No,im_index)
      JSNOW_GRND     = SI(242,Sect_No,im_index)

! Decoupled screen temperatures
      JTScrnDcl_TILE = SI(490,Sect_No,im_index) ! Decoupled screen-level
                                                ! temperature on tiles
      JTScrnDcl_SSI  = SI(491,Sect_No,im_index) ! Decoupled screen-level
                                                ! temperature on sea/s.ice
      JtStbTrans     = SI(492,Sect_No,im_index) ! Time since the transition

! Ozone
      JOZONE(o3dims2%k_start)     = SI(60,Sect_No,im_index)
! Check for zonal ozone and calculate pointers accordingly
      LEXPAND_OZONE=.FALSE.
      IF (A_LOOKUP(LBNPT,PPINDEX(60,im_index)) == 1) THEN
        LEXPAND_OZONE = .TRUE.
      ENDIF

      DO LEV= o3dims2%k_start+1, o3dims2%k_end
        IF(LEXPAND_OZONE) THEN
!         Ozone held as zonal averages, i.e. one value per row
          JOZONE(LEV)=JOZONE(LEV-1)+ROWS
        ELSE
          JOZONE(LEV)=JOZONE(LEV-1)+THETA_FIELD_SIZE
        END IF
      END DO

!! Tropopause-based Ozone
      IF (tpps_ozone_levels >  0) THEN
        JTPPSOZONE(1)     = SI(341,Sect_No,im_index)

        !Check for zonal tpps_ozone and calculate pointers accordingly
        LEXPAND_TPPS_OZONE=.FALSE.
        IF (A_LOOKUP(LBNPT,PPINDEX(341,im_index)) == 1) THEN
          LEXPAND_TPPS_OZONE = .TRUE.
        ENDIF
      END IF

      DO LEV=2,TPPS_OZONE_LEVELS
        IF(LEXPAND_TPPS_OZONE) THEN
          JTPPSOZONE(LEV)=JTPPSOZONE(LEV-1)+ROWS
        ELSE
          JTPPSOZONE(LEV)=JTPPSOZONE(LEV-1)+THETA_FIELD_SIZE
        END IF
      END DO

! Add prognostic ozone tracer and cariolle parameters to section 0
! 
        JOZONE_TRACER = SI(480,sect_no,im_index)
        JO3_PROD_LOSS = SI(481,sect_no,im_index)
        JO3_P_L_VMR   = SI(482,sect_no,im_index)
        JO3_VMR       = SI(483,sect_no,im_index)
        JO3_P_L_TEMP  = SI(484,sect_no,im_index)
        JO3_TEMP      = SI(485,sect_no,im_index)
        JO3_P_L_COLO3 = SI(486,sect_no,im_index)
        JO3_COLO3     = SI(487,sect_no,im_index)

        DO  LEV = tdims%k_start+1, tdims%k_end
          JOZONE_TRACER(LEV) = JOZONE_TRACER(LEV-1) + THETA_OFF_SIZE
          JO3_PROD_LOSS(LEV) = JO3_PROD_LOSS(LEV-1) + ROWS
          JO3_P_L_VMR(LEV)   = JO3_P_L_VMR(LEV-1) + ROWS
          JO3_VMR(LEV)       = JO3_VMR(LEV-1) + ROWS
          JO3_P_L_TEMP(LEV)  = JO3_P_L_TEMP(LEV-1) + ROWS 
          JO3_TEMP(LEV)      = JO3_TEMP(LEV-1) + ROWS
          JO3_P_L_COLO3(LEV) = JO3_P_L_COLO3(LEV-1) + ROWS
          JO3_COLO3(LEV)     = JO3_COLO3(LEV-1) + ROWS
        END DO


! STOCHEM fields - removed

! Add sources and aerosol ancillaries

      JMURK_SOURCE(tdims%k_start)= SI(57,Sect_No,im_index) !Murk source
      JSO2_EM                    = SI(58,Sect_No,im_index) !Sulphur dioxide emiss.
      JDMS_EM                    = SI(59,Sect_No,im_index) !Dimethyl sulphide emiss.
      JMURK       (tdims%k_start)= SI(90,Sect_No,im_index) !Murk concentration

! Add for Sulphur Cycle
      JSO2       (tdims%k_start)= SI(101,Sect_No,im_index) !Sulphur dioxide gas
      JDMS       (tdims%k_start)= SI(102,Sect_No,im_index) !Dimethyl sulphide gas
      JSO4_AITKEN(tdims%k_start)= SI(103,Sect_No,im_index) !Aitken mode SO4 aerosol
      JSO4_ACCU  (tdims%k_start)= SI(104,Sect_No,im_index) !Accumulation mode SO4 aer
      JSO4_DISS  (tdims%k_start)= SI(105,Sect_No,im_index) !Dissolved SO4 aerosol
      JH2O2      (tdims%k_start)= SI(106,Sect_No,im_index) !Hydrogen peroxide mmr
      JNH3       (tdims%k_start)= SI(107,Sect_No,im_index) !Ammonia gas
      JSOOT_NEW  (tdims%k_start)= SI(108,Sect_No,im_index) !Fresh soot
      JSOOT_AGD  (tdims%k_start)= SI(109,Sect_No,im_index) !Aged soot
      JSOOT_CLD  (tdims%k_start)= SI(110,Sect_No,im_index) !Soot in cloud
      JBMASS_NEW (tdims%k_start)= SI(111,Sect_No,im_index) !Fresh biomass smoke
      JBMASS_AGD (tdims%k_start)= SI(112,Sect_No,im_index) !Aged biomass smoke
      JBMASS_CLD (tdims%k_start)= SI(113,Sect_No,im_index) !Cloud biomass smoke
      JOCFF_NEW  (tdims%k_start)= SI(114,Sect_No,im_index) !Fresh ocff
      JOCFF_AGD  (tdims%k_start)= SI(115,Sect_No,im_index) !Aged socff
      JOCFF_CLD  (tdims%k_start)= SI(116,Sect_No,im_index) !Ocff in cloud
      JSO2_NATEM (tdims%k_start)= SI(121,Sect_No,im_index) !Natural SO2 emissions
      JOH        (tdims%k_start)= SI(122,Sect_No,im_index) !OH 3_D ancillary
      JHO2       (tdims%k_start)= SI(123,Sect_No,im_index) !HO2 3_D ancillary
      JH2O2_LIMIT(tdims%k_start)= SI(124,Sect_No,im_index) !H2O2 LIMIT 3_D ancillary
      JO3_CHEM   (tdims%k_start)= SI(125,Sect_No,im_index) !O3 for chemistry 3_D anc
      JSO2_HILEM    =SI(126,Sect_No,im_index)  !High level SO2 emissions
      JNH3_EM       =SI(127,Sect_No,im_index)  !Ammonia surface emiss
      JSOOT_EM      =SI(128,Sect_No,im_index)  !Fresh soot surf emiss
      JSOOT_HILEM   =SI(129,Sect_No,im_index)  !Fresh soot high emiss
      JBMASS_EM     =SI(130,Sect_No,im_index)  !Fresh bmass surf emiss
      JBMASS_HILEM  =SI(131,Sect_No,im_index)  !Elevated bmass emiss
      JDMS_CONC     =SI(132,Sect_No,im_index)  !DMS conc in seawater
      JDMS_OFLUX    =SI(133,Sect_No,im_index)  !DMS flux from ocean
      JOCFF_EM      =SI(134,Sect_No,im_index)  !Fresh OCFF surf emiss
      JOCFF_HILEM   =SI(135,Sect_No,im_index)  !Fresh OCFF high emiss

! Aerosol climatologies
      JARCLBIOG_BG  =SI(351,Sect_No,im_index)  ! Biogenic aerosol climatology
      JARCLBIOM_FR  =SI(352,Sect_No,im_index)  ! Biomass burning (fresh) aerosol clim
      JARCLBIOM_AG  =SI(353,Sect_No,im_index)  ! Biomass burning (aged) aerosol clim
      JARCLBIOM_IC  =SI(354,Sect_No,im_index)  ! Biomass burning (in-cloud) aerosol clim
      JARCLBLCK_FR  =SI(355,Sect_No,im_index)  ! Black carbon (fresh) aerosol clim
      JARCLBLCK_AG  =SI(356,Sect_No,im_index)  ! Black carbon (aged) aerosol clim
      JARCLSSLT_FI  =SI(357,Sect_No,im_index)  ! Sea salt (film mode) aerosol clim 
      JARCLSSLT_JT  =SI(358,Sect_No,im_index)  ! Sea salt (jet mode) aerosol clim
      JARCLSULP_AC  =SI(359,Sect_No,im_index)  ! Sulphate (accumulation mode) aero clim
      JARCLSULP_AK  =SI(360,Sect_No,im_index)  ! Sulphate (Aitken mode) aerosol clim 
      JARCLSULP_DI  =SI(361,Sect_No,im_index)  ! Sulphate (dissolved) aerosol clim
      JARCLDUST_B1  =SI(362,Sect_No,im_index)  ! Dust (bin 1) aerosol climatology 
      JARCLDUST_B2  =SI(363,Sect_No,im_index)  ! Dust (bin 2) aerosol climatology 
      JARCLDUST_B3  =SI(364,Sect_No,im_index)  ! Dust (bin 3) aerosol climatology 
      JARCLDUST_B4  =SI(365,Sect_No,im_index)  ! Dust (bin 4) aerosol climatology 
      JARCLDUST_B5  =SI(366,Sect_No,im_index)  ! Dust (bin 5) aerosol climatology 
      JARCLDUST_B6  =SI(367,Sect_No,im_index)  ! Dust (bin 6) aerosol climatology 
      JARCLOCFF_FR  =SI(368,Sect_No,im_index)  ! Org carbon fossil fuel (fresh) aero clim
      JARCLOCFF_AG  =SI(369,Sect_No,im_index)  ! Org carbon fossil fuel (aged) aero clim
      JARCLOCFF_IC  =SI(370,Sect_No,im_index)  ! Org carbon fossil fuel (in-cloud) aero clim
      JARCLDLTA_DL  =SI(371,Sect_No,im_index)  ! Delta aerosol climatology
      
      DO LEV= tdims%k_start+1, tdims%k_end
        JARCLBIOG_BG(LEV) = JARCLBIOG_BG(LEV-1)+THETA_FIELD_SIZE
        JARCLBIOM_FR(LEV) = JARCLBIOM_FR(LEV-1)+THETA_FIELD_SIZE
        JARCLBIOM_AG(LEV) = JARCLBIOM_AG(LEV-1)+THETA_FIELD_SIZE
        JARCLBIOM_IC(LEV) = JARCLBIOM_IC(LEV-1)+THETA_FIELD_SIZE
        JARCLBLCK_FR(LEV) = JARCLBLCK_FR(LEV-1)+THETA_FIELD_SIZE
        JARCLBLCK_AG(LEV) = JARCLBLCK_AG(LEV-1)+THETA_FIELD_SIZE
        JARCLSSLT_FI(LEV) = JARCLSSLT_FI(LEV-1)+THETA_FIELD_SIZE
        JARCLSSLT_JT(LEV) = JARCLSSLT_JT(LEV-1)+THETA_FIELD_SIZE
        JARCLSULP_AC(LEV) = JARCLSULP_AC(LEV-1)+THETA_FIELD_SIZE
        JARCLSULP_AK(LEV) = JARCLSULP_AK(LEV-1)+THETA_FIELD_SIZE
        JARCLSULP_DI(LEV) = JARCLSULP_DI(LEV-1)+THETA_FIELD_SIZE
        JARCLDUST_B1(LEV) = JARCLDUST_B1(LEV-1)+THETA_FIELD_SIZE
        JARCLDUST_B2(LEV) = JARCLDUST_B2(LEV-1)+THETA_FIELD_SIZE
        JARCLDUST_B3(LEV) = JARCLDUST_B3(LEV-1)+THETA_FIELD_SIZE
        JARCLDUST_B4(LEV) = JARCLDUST_B4(LEV-1)+THETA_FIELD_SIZE
        JARCLDUST_B5(LEV) = JARCLDUST_B5(LEV-1)+THETA_FIELD_SIZE
        JARCLDUST_B6(LEV) = JARCLDUST_B6(LEV-1)+THETA_FIELD_SIZE
        JARCLOCFF_FR(LEV) = JARCLOCFF_FR(LEV-1)+THETA_FIELD_SIZE
        JARCLOCFF_AG(LEV) = JARCLOCFF_AG(LEV-1)+THETA_FIELD_SIZE
        JARCLOCFF_IC(LEV) = JARCLOCFF_IC(LEV-1)+THETA_FIELD_SIZE
        JARCLDLTA_DL(LEV) = JARCLDLTA_DL(LEV-1)+THETA_FIELD_SIZE
      END DO

! Mineral dust scheme

      JSOIL_CLAY   =SI(418,Sect_No,im_index)  ! soil clay fraction
      JSOIL_SILT   =SI(419,Sect_No,im_index)  ! soil silt fraction
      JSOIL_SAND   =SI(420,Sect_No,im_index)  ! soil sand fraction

      JDUST_MREL1=SI(421,Sect_No,im_index) !relative soil mass in div1
      JDUST_MREL2=SI(422,Sect_No,im_index) !relative soil mass in div2
      JDUST_MREL3=SI(423,Sect_No,im_index) !relative soil mass in div3
      JDUST_MREL4=SI(424,Sect_No,im_index) !relative soil mass in div4
      JDUST_MREL5=SI(425,Sect_No,im_index) !relative soil mass in div5
      JDUST_MREL6=SI(426,Sect_No,im_index) !relative soil mass in div6


      JDUST_DIV1(tdims%k_start)=SI(431,Sect_No,im_index)  ! dust mmr, division 1
      JDUST_DIV2(tdims%k_start)=SI(432,Sect_No,im_index)  ! dust mmr, division 2
      JDUST_DIV3(tdims%k_start)=SI(433,Sect_No,im_index)  ! dust mmr, division 3
      JDUST_DIV4(tdims%k_start)=SI(434,Sect_No,im_index)  ! dust mmr, division 4
      JDUST_DIV5(tdims%k_start)=SI(435,Sect_No,im_index)  ! dust mmr, division 5
      JDUST_DIV6(tdims%k_start)=SI(436,Sect_No,im_index)  ! dust mmr, division 6

      DO LEV = tdims%k_start+1, tdims%k_end
       JDUST_DIV1(LEV)=JDUST_DIV1(LEV-1)+THETA_OFF_SIZE
       JDUST_DIV2(LEV)=JDUST_DIV2(LEV-1)+THETA_OFF_SIZE
       JDUST_DIV3(LEV)=JDUST_DIV3(LEV-1)+THETA_OFF_SIZE
       JDUST_DIV4(LEV)=JDUST_DIV4(LEV-1)+THETA_OFF_SIZE
       JDUST_DIV5(LEV)=JDUST_DIV5(LEV-1)+THETA_OFF_SIZE
       JDUST_DIV6(LEV)=JDUST_DIV6(LEV-1)+THETA_OFF_SIZE
      ENDDO

! Ammonium nitrate scheme
      JNITR_ACC (tdims%k_start)= SI(117,Sect_No,im_index) !Accumulation nitrate MMR
      JNITR_DISS(tdims%k_start)= SI(118,Sect_No,im_index) !Dissolved nitrate MMR 
      DO LEV = tdims%k_start+1, tdims%k_end 
        JNITR_ACC(LEV) = JNITR_ACC(LEV-1)+THETA_OFF_SIZE  
        JNITR_DISS(LEV) = JNITR_DISS(LEV-1)+THETA_OFF_SIZE  
      ENDDO 

! HadCM2 sulphate loading patterns
      JHadCM2_SO4(1)= SI(160,Sect_No,im_index)
      DO LEV=2, NSULPAT  ! nsulpat hard wired to 2 in clmchfcg_scenario_mod
        JHadCM2_SO4(LEV)=JHadCM2_SO4(LEV-1)+THETA_FIELD_SIZE
      ENDDO

! Add for Carbon cycle
      J_CO2FLUX = SI(250,Sect_No,im_index)
      J_CO2_EMITS  = SI(251,Sect_No,im_index)
      JCO2(tdims%k_start)      = SI(252,Sect_No,im_index)
      DO LEV= tdims%k_start+1, tdims%k_end
        JMURK_SOURCE(LEV) = JMURK_SOURCE(LEV-1)+THETA_FIELD_SIZE
        JMURK(LEV) = JMURK(LEV-1)+THETA_OFF_SIZE

! For Sulphur Cycle variables
        JSO2(LEV)=JSO2(LEV-1)+THETA_OFF_SIZE
        JDMS(LEV)=JDMS(LEV-1)+THETA_OFF_SIZE
        JSO4_AITKEN(LEV)=JSO4_AITKEN(LEV-1)+THETA_OFF_SIZE
        JSO4_ACCU(LEV)=JSO4_ACCU(LEV-1)+THETA_OFF_SIZE
        JSO4_DISS(LEV)=JSO4_DISS(LEV-1)+THETA_OFF_SIZE
        JH2O2(LEV)=JH2O2(LEV-1)+THETA_OFF_SIZE
        JSO2_NATEM(LEV)=JSO2_NATEM(LEV-1)+THETA_FIELD_SIZE
        JOH(LEV) = JOH(LEV-1)+THETA_FIELD_SIZE
        JHO2(LEV) = JHO2(LEV-1)+THETA_FIELD_SIZE
        JNH3(LEV)      = JNH3(LEV-1)+THETA_OFF_SIZE
        JSOOT_NEW(LEV) = JSOOT_NEW(LEV-1)+THETA_OFF_SIZE
        JSOOT_AGD(LEV) = JSOOT_AGD(LEV-1)+THETA_OFF_SIZE
        JSOOT_CLD(LEV) = JSOOT_CLD(LEV-1)+THETA_OFF_SIZE
        JBMASS_NEW(LEV) = JBMASS_NEW(LEV-1)+THETA_OFF_SIZE
        JBMASS_AGD(LEV) = JBMASS_AGD(LEV-1)+THETA_OFF_SIZE
        JBMASS_CLD(LEV) = JBMASS_CLD(LEV-1)+THETA_OFF_SIZE
        JOCFF_NEW(LEV) = JOCFF_NEW(LEV-1)+THETA_OFF_SIZE
        JOCFF_AGD(LEV) = JOCFF_AGD(LEV-1)+THETA_OFF_SIZE
        JOCFF_CLD(LEV) = JOCFF_CLD(LEV-1)+THETA_OFF_SIZE
        JH2O2_LIMIT(LEV)=JH2O2_LIMIT(LEV-1)+THETA_FIELD_SIZE
        JO3_CHEM(LEV)=JO3_CHEM(LEV-1)+THETA_FIELD_SIZE

! For Carbon Cycle variables
        JCO2(LEV)=JCO2(LEV-1)+THETA_OFF_SIZE
      END DO
      !CABLE: pointers into d1 (e.g. jTSOIL_TILE) included via argptra.h etc
      call cable_set_atm_pointers( SI, NITEMS, NSECTS, N_INTERNAL_MODEL, &
                                 Sect_No,im_index, & 
                                 jTSOIL_TILE, jSMCL_TILE, jSTHF_TILE,            &
                                 jSNOW_DEPTH3L, jSNOW_MASS3L, jSNOW_TMP3L,       &
                                 jSNOW_RHO3L, jSNOW_RHO1L, jSNOW_AGE, jSNOW_FLG3L )

      JUSER_ANC19 = SI(319,Sect_No,im_index)
      JUSER_ANC20 = SI(320,Sect_No,im_index)
      JUSER_MULT1(1)  = SI(321,Sect_No,im_index)
      JUSER_MULT2(1)  = SI(322,Sect_No,im_index)
      JUSER_MULT18(1) = SI(338,Sect_No,im_index)
      JUSER_MULT19(1) = SI(339,Sect_No,im_index)
      JUSER_MULT20(1) = SI(340,Sect_No,im_index)

! Set for multi-level user ancillaries
      DO LEV=2,MODEL_LEVELS
        JUSER_MULT1(LEV)  = JUSER_MULT1(LEV-1)+THETA_FIELD_SIZE
        JUSER_MULT2(LEV)  = JUSER_MULT2(LEV-1)+THETA_FIELD_SIZE
        JUSER_MULT18(LEV) = JUSER_MULT18(LEV-1)+THETA_FIELD_SIZE
        JUSER_MULT19(LEV) = JUSER_MULT19(LEV-1)+THETA_FIELD_SIZE
        JUSER_MULT20(LEV) = JUSER_MULT20(LEV-1)+THETA_FIELD_SIZE
      END DO

! Tiled vegetation and triffid
      JFRAC_TYP     = SI(216,Sect_No,im_index) ! surface type fractions
      JFRAC_CON1    = SI(442,Sect_No,im_index) ! surface type fractions
      JFRAC_CON2    = SI(443,Sect_No,im_index) ! surface type fractions
      JFRAC_CON3    = SI(444,Sect_No,im_index) ! surface type fractions
      JFRAC_CON4    = SI(445,Sect_No,im_index) ! surface type fractions
      JFRAC_CON5    = SI(446,Sect_No,im_index) ! surface type fractions
      JFRAC_CON6    = SI(447,Sect_No,im_index) ! surface type fractions
      JFRAC_CON7    = SI(448,Sect_No,im_index) ! surface type fractions
      JFRAC_CON8    = SI(449,Sect_No,im_index) ! surface type fractions
      JFRAC_CON9    = SI(450,Sect_No,im_index) ! surface type fractions
      JLAI_PFT      = SI(217,Sect_No,im_index) ! leaf area index of PFTs
      JCANHT_PFT    = SI(218,Sect_No,im_index) ! canopy height of PFTs
      JDISTURB      = SI(219,Sect_No,im_index) ! Veg disturbed fraction
      JSOIL_ALB     = SI(220,Sect_No,im_index) ! Snow-free soil albedo
      JOBS_ALB_SW   = SI(243,Sect_No,im_index) ! Observed snow-free SW albedo 
      JOBS_ALB_VIS  = SI(244,Sect_No,im_index) ! Observed snow-free VIS albedo 
      JOBS_ALB_NIR  = SI(245,Sect_No,im_index) ! Observed snow-free NIR albedo 
      JSOIL_CARB    = SI(223,Sect_No,im_index) ! Soil carbon content
      JSOIL_CARB1   = SI(466,Sect_No,im_index) ! Soil carbon content DPM
      JSOIL_CARB2   = SI(467,Sect_No,im_index) ! Soil carbon content RPM
      JSOIL_CARB3   = SI(468,Sect_No,im_index) ! Soil carbon content BIO
      JSOIL_CARB4   = SI(469,Sect_No,im_index) ! Soil carbon content HUM
      JNPP_PFT_ACC  = SI(224,Sect_No,im_index) ! Accumulated NPP on PFTs
      JG_LF_PFT_ACC = SI(225,Sect_No,im_index) ! Accumulated leaf
!                                              ! turnover rate on PFTs
      JG_PHLF_PFT_ACC=SI(226,Sect_No,im_index) ! Accumulat. phenological
!                                              ! leaf turnover rate PFTs
      JRSP_W_PFT_ACC= SI(227,Sect_No,im_index) ! Accum. wood resp PFTs
      JRSP_S_ACC    = SI(228,Sect_No,im_index) ! Accumulated soil resp
      JRSP_S_ACC1 = SI(470,Sect_No,im_index)  ! Soil respiration DPM
      JRSP_S_ACC2 = SI(471,Sect_No,im_index)  ! Soil respiration RPM
      JRSP_S_ACC3 = SI(472,Sect_No,im_index)  ! Soil respiration BIO
      JRSP_S_ACC4 = SI(473,Sect_No,im_index)  ! Soil respiration HUM
      JCAN_WATER_TILE=SI(229,Sect_No,im_index) ! Canopy water content
!                                              ! on tiles
      JCATCH_TILE   = SI(230,Sect_No,im_index) ! Canopy capacity on
!                                              ! tiles
      JRGRAIN_TILE  = SI(231,Sect_No,im_index) ! Snow grain size on
!                                              ! tiles
      JTSTAR_TILE   = SI(233,Sect_No,im_index) ! Tiled surface temp
      JZ0_TILE      = SI(234,Sect_No,im_index) ! Tiled surface roughness
      JZ0H_TILE     = SI(246,Sect_No,im_index) ! Tiled surface thermal 
!                                              ! roughness
! Stash number for snow tile not finalised yet
      JSNODEP_TILE  = SI(240,Sect_No,im_index) ! Tiled snow depth
      JINFIL_TILE   = SI(236,Sect_No,im_index) ! Max tile infilt rate
! Stash codes for DORL, LW_DOWN, SW_TILE not finalised yet
      JDOLR         = SI(239,Sect_No,im_index) ! TOA surface up LW
      JLW_DOWN      = SI(238,Sect_No,im_index) ! Surface down LW
      JSW_TILE      = SI(237,Sect_No,im_index) ! Surface net SW on tiles

! MORUSES urban scheme
      JURBHGT             = SI(494,Sect_No,im_index) ! Building height
      JURBHWR             = SI(495,Sect_No,im_index) ! Height to width
      JURBWRR             = SI(496,Sect_No,im_index) ! Width ratio
      JURBDISP            = SI(497,Sect_No,im_index) ! Displacement height
      JURBZTM             = SI(498,Sect_No,im_index) ! Effective roughness
                                                     ! length for momentum
      JURBALBWL           = SI(499,Sect_No,im_index) ! Wall albedo
      JURBALBRD           = SI(500,Sect_No,im_index) ! Road albedo
      JURBEMISW           = SI(501,Sect_No,im_index) ! Wall emmissivity
      JURBEMISR           = SI(502,Sect_No,im_index) ! Road emmissivity

! River routing fields
      JRIV_SEQUENCE  = SI(151,Sect_No,im_index) ! River sequence
      JRIV_DIRECTION = SI(152,Sect_No,im_index) ! River Direction
      JRIV_STORAGE   = SI(153,Sect_No,im_index) ! River Water Storage
      JTOT_SURFROFF  = SI(155,Sect_No,im_index) ! Acc. surface runoff
      JTOT_SUBROFF   = SI(156,Sect_No,im_index) ! Acc. sub-surf runoff
! Set pointer for inland basin outflow
      JRIV_INLANDATM    = SI(511,Sect_No,im_index)
       !Inland basin outflow



! required for energy correction
      JNET_FLUX=SI(222,Sect_No,im_index)    ! store for energy flux
      JNET_MFLUX=SI(235,Sect_No,im_index)   ! store for moisture flux

!Fields carried forward from previous version.
      JTSTAR_ANOM    = SI(39,Sect_No,im_index)

! JULES version 2 prognostics
      JSNOWDEPTH      = SI(376,Sect_No,im_index) ! Snow depth on ground on tiles (m)
      JRHO_SNOW_GRND  = SI(377,Sect_No,im_index) ! Snowpack bulk density (kg/m3)
      JNSNOW          = SI(380,Sect_No,im_index) ! Number of snow layers on ground on tiles
      JDS             = SI(381,Sect_No,im_index) ! Snow layer thickness (m)
      JSICE           = SI(382,Sect_No,im_index) ! Snow layer ice mass on tiles (Kg/m2)
      JSLIQ           = SI(383,Sect_No,im_index) ! Snow layer liquid mass on tiles (Kg/m2)
      JTSNOWLAYER     = SI(384,Sect_No,im_index) ! Snow layer temperature (K)
      JRHO_SNOW       = SI(385,Sect_No,im_index) ! Snow layer densities (kg/m3)
      JRGRAINL        = SI(386,Sect_No,im_index) ! Snow layer grain size on tiles (microns)

! FLake lake scheme prognostics
      JLAKE_DEPTH  = SI(291,Sect_No,im_index) ! lake depth (m)
      JLAKE_FETCH  = SI(292,Sect_No,im_index) ! typical wind fetch (m)
      JLAKE_T_MEAN = SI(293,Sect_No,im_index) ! lake mean temperature (K)
      JLAKE_T_MXL  = SI(294,Sect_No,im_index) ! lake mixed-layer temperature (K)
      JLAKE_T_ICE  = SI(295,Sect_No,im_index) ! temperature at upper boundary of lake ice (K)
      JLAKE_H_MXL  = SI(296,Sect_No,im_index) ! lake mixed-layer depth (m)
      JLAKE_H_ICE  = SI(297,Sect_No,im_index) ! lake ice thickness (m)
      JLAKE_SHAPE  = SI(298,Sect_No,im_index) ! thermocline shape factor
      JLAKE_G_DT   = SI(299,Sect_No,im_index) ! lake ht.flx / dT (W m-2 K-1)

! Set all pointers referencing Lateral Boundary Conditions

      Sect_No=31  ! LBC section

      JOROG_LBC     = SI(1,Sect_No,im_index)
      JU_LBC        = SI(2,Sect_No,im_index)
      JV_LBC        = SI(3,Sect_No,im_index)
      JW_LBC        = SI(4,Sect_No,im_index)
      JRHO_LBC      = SI(5,Sect_No,im_index)
      JTHETA_LBC    = SI(6,Sect_No,im_index)
      JQ_LBC        = SI(7,Sect_No,im_index)
      JQCL_LBC      = SI(8,Sect_No,im_index)
      JQCF_LBC      = SI(9,Sect_No,im_index)
      JEXNER_LBC    = SI(10,Sect_No,im_index)
      JU_ADV_LBC    = SI(11,Sect_No,im_index)
      JV_ADV_LBC    = SI(12,Sect_No,im_index)
      JW_ADV_LBC    = SI(13,Sect_No,im_index)
      JQCF2_LBC     = SI(14,Sect_No,im_index)
      JQRAIN_LBC    = SI(15,Sect_No,im_index)
      JQGRAUP_LBC   = SI(16,Sect_No,im_index)
      JCF_BULK_LBC  = SI(17,Sect_No,im_index)
      JCF_LIQUID_LBC= SI(18,Sect_No,im_index)
      JCF_FROZEN_LBC= SI(19,Sect_No,im_index)
      JMURK_LBC     = SI(20,Sect_No,im_index)

      JDUST_DIV1_LBC = SI(23,Sect_No,im_index)
      JDUST_DIV2_LBC = SI(24,Sect_No,im_index)
      JDUST_DIV3_LBC = SI(25,Sect_No,im_index)
      JDUST_DIV4_LBC = SI(26,Sect_No,im_index)
      JDUST_DIV5_LBC = SI(27,Sect_No,im_index)
      JDUST_DIV6_LBC = SI(28,Sect_No,im_index)
      JSO2_LBC       = SI(29,Sect_No,im_index)
      JDMS_LBC       = SI(30,Sect_No,im_index)
      JSO4_AITKEN_LBC= SI(31,Sect_No,im_index)
      JSO4_ACCU_LBC  = SI(32,Sect_No,im_index)
      JSO4_DISS_LBC  = SI(33,Sect_No,im_index)
      JNH3_LBC       = SI(35,Sect_No,im_index)
      JSOOT_NEW_LBC  = SI(36,Sect_No,im_index)
      JSOOT_AGD_LBC  = SI(37,Sect_No,im_index)
      JSOOT_CLD_LBC  = SI(38,Sect_No,im_index)
      JBMASS_NEW_LBC = SI(39,Sect_No,im_index)
      JBMASS_AGD_LBC = SI(40,Sect_No,im_index)
      JBMASS_CLD_LBC = SI(41,Sect_No,im_index)
      JOCFF_NEW_LBC  = SI(42,Sect_No,im_index)
      JOCFF_AGD_LBC  = SI(43,Sect_No,im_index)
      JOCFF_CLD_LBC  = SI(44,Sect_No,im_index)
      JNITR_ACC_LBC  = SI(45,Sect_No,im_index)
      JNITR_DISS_LBC = SI(46,Sect_No,im_index)

      JU_LBC_TEND     = SI(257,Sect_No,im_index)
      JV_LBC_TEND     = SI(258,Sect_No,im_index)
      JW_LBC_TEND     = SI(259,Sect_No,im_index)
      JRHO_LBC_TEND   = SI(260,Sect_No,im_index)
      JTHETA_LBC_TEND = SI(261,Sect_No,im_index)
      JQ_LBC_TEND     = SI(262,Sect_No,im_index)
      JQCL_LBC_TEND   = SI(263,Sect_No,im_index)
      JQCF_LBC_TEND   = SI(264,Sect_No,im_index)
      JEXNER_LBC_TEND = SI(265,Sect_No,im_index)
      JU_ADV_LBC_TEND = SI(266,Sect_No,im_index)
      JV_ADV_LBC_TEND = SI(267,Sect_No,im_index)
      JW_ADV_LBC_TEND = SI(268,Sect_No,im_index)
      JQCF2_LBC_TEND   = SI(269,Sect_No,im_index)
      JQRAIN_LBC_TEND  = SI(270,Sect_No,im_index)
      JQGRAUP_LBC_TEND = SI(271,Sect_No,im_index)
      JCF_BULK_LBC_TEND  = SI(272,Sect_No,im_index)
      JCF_LIQUID_LBC_TEND= SI(273,Sect_No,im_index)
      JCF_FROZEN_LBC_TEND= SI(274,Sect_No,im_index)
      JMURK_LBC_TEND   = SI(275,Sect_No,im_index)

      JDUST_DIV1_LBC_TEND = SI(276,Sect_No,im_index)
      JDUST_DIV2_LBC_TEND = SI(277,Sect_No,im_index)
      JDUST_DIV3_LBC_TEND = SI(278,Sect_No,im_index)
      JDUST_DIV4_LBC_TEND = SI(279,Sect_No,im_index)
      JDUST_DIV5_LBC_TEND = SI(280,Sect_No,im_index)
      JDUST_DIV6_LBC_TEND = SI(281,Sect_No,im_index)
      JSO2_LBC_TEND       = SI(282,Sect_No,im_index)
      JDMS_LBC_TEND       = SI(283,Sect_No,im_index)
      JSO4_AITKEN_LBC_TEND= SI(284,Sect_No,im_index)
      JSO4_ACCU_LBC_TEND  = SI(285,Sect_No,im_index)
      JSO4_DISS_LBC_TEND  = SI(286,Sect_No,im_index)
      JNH3_LBC_TEND       = SI(288,Sect_No,im_index)
      JSOOT_NEW_LBC_TEND  = SI(289,Sect_No,im_index)
      JSOOT_AGD_LBC_TEND  = SI(290,Sect_No,im_index)
      JSOOT_CLD_LBC_TEND  = SI(291,Sect_No,im_index)
      JBMASS_NEW_LBC_TEND = SI(292,Sect_No,im_index)
      JBMASS_AGD_LBC_TEND = SI(293,Sect_No,im_index)
      JBMASS_CLD_LBC_TEND = SI(294,Sect_No,im_index)
      JOCFF_NEW_LBC_TEND  = SI(295,Sect_No,im_index)
      JOCFF_AGD_LBC_TEND  = SI(296,Sect_No,im_index)
      JOCFF_CLD_LBC_TEND  = SI(297,Sect_No,im_index)
      JNITR_ACC_LBC_TEND  = SI(298,Sect_No,im_index)
      JNITR_DISS_LBC_TEND = SI(299,Sect_No,im_index)

! Set pointers for Tracer prognostics
! Tracer prognostics are now in section 33, not section 0
      sect_no = 33   ! tracers section
      JVAR=0         ! JVAR+1 is the current tracer to be found
      IF (TR_VARS >  0) THEN
        DO IVAR=A_TRACER_FIRST,A_TRACER_LAST
          IF(SI(IVAR,sect_no,im_index) /= 1) THEN ! tracer in use
            JVAR=JVAR+1
            JTRACER(trdims_xstl%k_start,JVAR) = SI(IVAR,sect_no,im_index)
            ! Set up array containing stash item codes for active 
            ! tracer prognostics (1:TR_VARS)
            A_TR_StashItem(JVAR) = IVAR
            DO LEV = trdims_xstl%k_start+1, trdims_xstl % k_end
              JTRACER(LEV,JVAR)=JTRACER(LEV-1,JVAR)+THETA_OFF_SIZE
            END DO
            A_TR_INDEX(IVAR-A_TRACER_FIRST+1)=JVAR
          ELSE
            ! If tracer not active, set value to -1
            A_TR_INDEX(IVAR-A_TRACER_FIRST+1) = -1
          END IF
        END DO
      ELSE   
        ! Ensure a sensible address even if no tracers
        JTRACER(trdims_xstl%k_start,1)=1
      ENDIF
      IF(JVAR /= TR_VARS) THEN
        WRITE(6,*) 'STATMPT: TR_VARS and SI are inconsistent'
        WRITE(6,*) 'TR_VARS=',TR_VARS,' .     But, SI implies :',JVAR
        CMESSAGE=  'STATMPT: TR_VARS and SI  inconsistent, see output'
        ICODE=100
        GOTO 9999 ! error return
      END IF

! UKCA tracer prognostics are in section 34.
      sect_no = ukca_sect   ! UKCA tracers section 
      JVAR=0         ! JVAR+1 is the current tracer to be found
      IF (TR_UKCA >  0) THEN
        DO IVAR=A_UKCA_FIRST,A_UKCA_LAST
          IF(SI(IVAR,sect_no,im_index) /= 1) THEN ! tracer in use
            JVAR=JVAR+1
            JTR_UKCA(trdims_xstl%k_start,JVAR) = SI(IVAR,sect_no,im_index)
            ! Set up array containing stash item codes for active
            ! tracer prognostics (1:TR_UKCA)
            UKCA_TR_StashItem(JVAR) = IVAR
            DO LEV = trdims_xstl%k_start+1, trdims_xstl%k_end
              JTR_UKCA(LEV,JVAR)=JTR_UKCA(LEV-1,JVAR)+THETA_OFF_SIZE
            END DO
            A_UKCA_INDEX(IVAR-A_UKCA_FIRST+1)=JVAR
          ELSE
            ! If tracer not active, set value to -1
            A_UKCA_INDEX(IVAR-A_UKCA_FIRST+1) = -1
          END IF
        END DO
      ELSE
        ! Ensure a sensible address when no tracers
        JTR_UKCA(trdims_xstl%k_start,1)=1
      ENDIF

      IF(JVAR /= TR_UKCA) THEN
        WRITE(6,*) 'STATMPT: TR_UKCA and SI are inconsistent'
        WRITE(6,*) 'TR_UKCA=',TR_UKCA,' .     But, SI implies :',JVAR
        CMESSAGE=  'STATMPT: TR_UKCA and SI  inconsistent, see output'
        ICODE=100
        GOTO 9999 ! error return
      END IF

      IF(L_SULPC_ONLINE_OXIDANTS .AND. L_UKCA) THEN 
         JO3_UKCA(tdims%k_start)   =SI(ukca_item_sulpc(1),sect_no,im_index)
         JHNO3_UKCA(tdims%k_start) =SI(ukca_item_sulpc(2),sect_no,im_index)
         JH2O2_UKCA(tdims%k_start) =SI(ukca_item_sulpc(3),sect_no,im_index)
         JOH_UKCA(tdims%k_start)   =SI(ukca_item_sulpc(4),sect_no,im_index) 
         JHO2_UKCA(tdims%k_start)  =SI(ukca_item_sulpc(5),sect_no,im_index) 
      END IF
      
! Set pointers for Tracer lateral boundary data

      ! Free tracer lbc data is in section 36
      ! The tracer lbc stash item codes in A_TR_LBC_StashItem
      ! are set up in INBOUNDA
 
      sect_no = 36   ! tracer lbcs section
      jvar = 0
      If (TR_LBC_VARS > 0) Then                                         
        Do ivar= A_TRACER_FIRST,A_TRACER_LAST
          If (SI(ivar,sect_no,im_index) /= 1) Then 
            jvar = jvar + 1
            JTRACER_LBC(jvar)        = SI(ivar,sect_no,im_index)
            JTRACER_LBC_TEND(jvar)   = SI(256+ivar,sect_no,im_index)
            A_TR_LBC_StashItem(jvar) = ivar
          End If
        End Do                                          
      Else   
        ! Ensure a sensible address even if no tracer lbcs   
        JTRACER_LBC(1)=1                                           
        JTRACER_LBC_TEND(1)=1 
      End If ! IF (TR_LBC_VARS > 0)                                     

      ! Set up array (1:TR_VARS) pointing to location in TRACER_LBC 
      ! array for each prognostic tracer (A_tr_active_lbc_index)
      ! This allows tracer prognostics to be active even if
      ! there are no lbcs, and lbc data to be ignored if the 
      ! tracer prognostic is not active. If there is no lbc data
      ! for a particular tracer, the array is set to -1.
       
      DO ivar= 1,TR_VARS
        A_tr_active_lbc_index(ivar) = -1 
        DO jvar= 1,TR_LBC_VARS
          IF (A_TR_LBC_StashItem(jvar) == A_TR_StashItem(ivar)) THEN
            A_tr_active_lbc_index(ivar) = jvar                  
            Write(6,*) 'A_tr_active_lbc_index:',ivar,jvar,              &
                  A_TR_LBC_StashItem(jvar),A_TR_StashItem(ivar),        &
                  A_tr_active_lbc_index(ivar)     
          END IF
        END DO
      END DO


      ! UKCA tracer lbc data is in section 37
      ! The tracer lbc stash item codes in UKCA_TR_LBC_StashItem
      ! are set up in INBOUNDA
 
      sect_no = 37   ! UKCA tracer lbcs section
      jvar = 0
      IF (TR_LBC_UKCA > 0) THEN                                         
        DO ivar= A_UKCA_FIRST,A_UKCA_LAST
          IF (SI(ivar,sect_no,im_index) /= 1) THEN 
            jvar = jvar + 1
            JTR_UKCA_LBC(jvar)        = SI(ivar,sect_no,im_index)
            JTR_UKCA_LBC_TEND(jvar)   = SI(256+ivar,sect_no,im_index)
            UKCA_TR_LBC_StashItem(jvar) = ivar
          END IF
        END DO                                          
      ELSE   
        ! Ensure a sensible address even if no tracer lbcs   
        JTR_UKCA_LBC(1)=1                                           
        JTR_UKCA_LBC_TEND(1)=1 
      END IF ! IF (TR_LBC_UKCA > 0)                                     

     ! Set up array (1:TR_UKCA) pointing to location in TRACER_LBC_UKCA 
     ! array for each prognostic tracer (UKCA_tr_active_lbc_index)
     ! This allows tracer prognostics to be active even if
     ! there are no lbcs, and lbc data to be ignored if the 
     ! tracer prognostic is not active. If there is no lbc data
     ! for a particular tracer, the array is set to -1.
       
      Do ivar= 1,TR_UKCA
        UKCA_tr_active_lbc_index(ivar) = -1 
        Do jvar= 1,TR_LBC_UKCA
          If (UKCA_TR_LBC_StashItem(jvar) == UKCA_TR_StashItem(ivar))   &
          Then
            UKCA_tr_active_lbc_index(ivar) = jvar                  
            Write(6,*) 'UKCA_tr_active_lbc_index:',ivar,jvar,           &
                  UKCA_TR_LBC_StashItem(jvar),UKCA_TR_StashItem(ivar),  &
                  UKCA_tr_active_lbc_index(ivar)     
          End If
        End Do
      End Do

! Set pointers to level dependent constants for atmosphere.
      JETATHETA      =1              ! eta_theta_levels(0:model_levels)
      JETARHO        =JETATHETA+model_levels+1 ! eta_rho_levels
      JRHCRIT        =JETARHO+model_levels+1   ! rhcrit
      JSOIL_THICKNESS=JRHCRIT+model_levels+1   ! soil level depths
! For height definition z(i,j,k) = zsea(k) + C(k)*zorog(i,j)
      Jzseak_theta   =JSOIL_THICKNESS+model_levels+1 ! zsea (theta levs)
      JCk_theta      =Jzseak_theta   +model_levels+1 ! C    (theta levs)
      Jzseak_rho     =JCk_theta      +model_levels+1 ! zsea (rho levs)
      JCk_rho        =Jzseak_rho     +model_levels+1 ! C    (rho levs)
      
      IF (A_LEN2_ROWDEPC > 0 .AND. A_LEN2_COLDEPC > 0) THEN
! Set pointers to Row dependent constants for atmosphere.
        JPHI_INPUT_P       =1
        JPHI_INPUT_V       =JPHI_INPUT_P + A_LEN1_ROWDEPC
! Set pointers to Col dependent constants for atmosphere.      
        JLAMBDA_INPUT_P    =1
        JLAMBDA_INPUT_U    =JLAMBDA_INPUT_P + A_LEN1_COLDEPC
      ELSE
        JPHI_INPUT_P       =1
        JPHI_INPUT_V       =1
        JLAMBDA_INPUT_P    =1
        JLAMBDA_INPUT_U    =1
      END IF

! The following if block should only be required for test purposes
! during the transition of vn5.2. This ensures old values for
! setting blev/bulev/brlev on old style dumps (before 'smooth'
! algorithm introduced and new definitions introduced).
      if(A_LEN2_LEVDEPC  <=  4) then ! ie before 5.2 change
         JCk_rho=jetarho
         Jck_theta=jetatheta
         Jzseak_rho=jetarho
         Jzseak_theta=jetatheta
         write(6,*) 'SETCONA_CTL: WARNING. This dump has not been '//   &
         'reconfigured with new 5.2 set of level dependent constants:'
         write(6,*) 'PP headers will revert to 5.0/5.1 style '//        &
         'blev/bulev/brlev definitions'
      endif

 9999 CONTINUE ! ERROR GOTO point.
      IF (lhook) CALL dr_hook('SET_ATM_POINTERS',zhook_out,zhook_handle)
      RETURN
      END SUBROUTINE SET_ATM_POINTERS
