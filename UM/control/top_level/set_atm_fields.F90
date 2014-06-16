! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!+ Points atmosphere fields to the appropriate sections of D1
!
! Subroutine Interface: 
SUBROUTINE Set_Atm_Fields ( &
! "argptra.h" contains jpointers
#include "argptra.h"
! "argsts.h" contains SI (STASH index array); used to check tracers
#include "argsts.h" 
      D1, LD1, ID1 )

       USE atm_fields_mod   ! atmosphere fields
       USE field_length_mod ! field_length function 
       USE atm_fields_bounds_mod
       USE ancil_info, only: nsmax

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE UM_ParVars
USE rimtypes
USE lbc_mod
USE um_input_control_mod,  ONLY: l_sulpc_online_oxidants
USE rad_input_mod, ONLY: lexpand_ozone, lexpand_tpps_ozone
USE ukca_option_mod, ONLY: l_ukca
USE Submodel_Mod


       USE chsunits_mod, ONLY : nunits

   use cable_data_mod, only : TSOIL_TILE, SMCL_TILE, STHF_TILE,            &
                              SNOW_DEPTH3L, SNOW_MASS3L, SNOW_TMP3L,       &
                              SNOW_RHO3L, SNOW_RHO1L, SNOW_AGE, SNOW_FLG3L

   use cable_data_mod, only : cable_set_atm_fields
IMPLICIT NONE
!
! Description: 
!   Routine to point atmosphere fields to the appropriate sections of D1.
!   After calling this subroutine, the fields can be used directly without
!   referring to D1
!
! Method: 
!   Assuming SET_ATM_POINTERS has been called beforehand, this subroutine
!   points each field to an area of D1 starting at the corresponding
!   "jpointer" (at the first level) and ending at the "jpointer" (at the 
!   last level) plus the size of a single level of that field. 
!
!   Tracers are dealt with differently:   First the number of active tracers
!   is computed so that the correct sections of the corresponding tracer
!   jpointers can be used in pointing the tracer fields to D1.  If no tracers
!   are active then the fields are pointed to a dummy array
!
! Code Owner: See Unified Model Code Owners HTML page
! This file belongs in section: Top Level
!
! Code description: 
!   Language:  Fortran 90.
!   This code is written to UM programming standards UMDP3 vn 8.3.
!

! Subroutine arguments

#include "typsize.h"
#include "typptra.h"
#include "typsts.h"
! constants (NUNITS) in "chsunits.h" are needed by "ccontrol.h"
! constants (L_3D_CCA) in "ccontrol.h" are needed to determine field sizes below
#include "ccontrol.h" 
! constants (A_TRACER_FIRST, etc.) in "ctracera.h" are needed for tracers
#include "ctracera.h"

      REAL,    TARGET, INTENT(IN) :: D1(LEN_TOT)
      LOGICAL, TARGET, INTENT(IN) :: LD1(LEN_TOT)
      INTEGER, TARGET, INTENT(IN) :: ID1(LEN_TOT)

! Local variables

      INTEGER :: nTracer ! loop counter over available tracers
      INTEGER :: nActiveTracers ! number of tracers actually being used

      INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
      INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
      REAL(KIND=jprb)               :: zhook_handle

! End of header

! 1.0 Start of subroutine code; point fields to D1

!    atmospheric primary variables
      IF (lhook) CALL dr_hook('SET_ATM_FIELDS',zhook_in,zhook_handle)

     Exner      => D1(JEXNER_RHO_LEVELS(pdims%k_start) :                       &
                      JEXNER_RHO_LEVELS(pdims%k_start) +                       &
                      field_length(theta_points,single_halo,pdims%k_end+1) -1)

     Exner_Surf => D1(JEXNERSURF : JEXNERSURF +                                &
                      field_length(theta_points,single_halo,1) -1)

     DryRho     => D1(JDRYRHO(pdims%k_start) : JDRYRHO(pdims%k_start) +        &
                      field_length(theta_points,single_halo,                   &
                                   pdims%k_end-pdims%k_start+1) -1)

     Etadot     => D1(JETADOT(wdims%k_start) : JETADOT(wdims%k_start) +        &
                      field_length(theta_points,single_halo,                   &
                                   wdims%k_end-wdims%k_start+1) -1)

     THETAV     => D1(JTHETAV(tdims%k_start) : JTHETAV(tdims%k_start) +        &
                      field_length(theta_points,single_halo,                   &
                                   tdims%k_end-tdims%k_start+1) -1)

     PSI_W_SURF => D1(JPSIWS : JPSIWS + field_length(theta_points,no_halo,1) -1)

     PSI_W_LID  => D1(JPSIWL : JPSIWL + field_length(theta_points,no_halo,1) -1)

     m_v        => D1(JMV(qdims%k_start) : JMV(qdims%k_start) +                &
                      field_length(theta_points,single_halo,                   &
                              qdims%k_end-qdims%k_start+1) -1)

     m_cl       => D1(JMCL(qdims%k_start) : JMCL(qdims%k_start) +              &
                      field_length(theta_points,single_halo,                   &
                                   qdims%k_end-qdims%k_start+1) -1)

     m_cf       => D1(JMCF(qdims%k_start) : JMCF(qdims%k_start) +              &
                      field_length(theta_points,single_halo,                   &
                                   qdims%k_end-qdims%k_start+1) -1)

     m_cf2      => D1(JMCF2(qdims%k_start) : JMCF2(qdims%k_start) +            &
                      field_length(theta_points,single_halo,                   &
                                   qdims%k_end-qdims%k_start+1) -1)

     m_gr       => D1(JMGRAUP(qdims%k_start) : JMGRAUP(qdims%k_start) +        &
                      field_length(theta_points,single_halo,                   &
                              qdims%k_end-qdims%k_start+1) -1)

     m_r        => D1(JMRAIN(qdims%k_start) : JMRAIN(qdims%k_start) +          &
                      field_length(theta_points,single_halo,                   &
                                   qdims%k_end-qdims%k_start+1) -1)
    
     

     U         => D1(JU(udims%k_start) : JU(udims%k_start) + &
      field_length(u_points,single_halo,udims%k_end-udims%k_start+1) -1)

     V         => D1(JV(udims%k_start) : JV(udims%k_start) + &
      field_length(v_points,single_halo,vdims%k_end-vdims%k_start+1) -1)

     THETA     => D1(JTHETA(tdims%k_start) : JTHETA(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)

     Q         => D1(JQ(qdims%k_start) : JQ(qdims%k_start)+ &
      field_length(theta_points,extended_halo,qdims%k_end-qdims%k_start+1) -1)

     QCF       => D1(JQCF(qdims%k_start) : JQCF(qdims%k_start)+ &
      field_length(theta_points,extended_halo,qdims%k_end-qdims%k_start+1) -1)

     TSTAR     => D1(JTSTAR : JTSTAR+field_length(theta_points,no_halo,1) -1)
     LAND      => D1(JLAND  : JLAND +field_length(theta_points,no_halo,1) -1)
     OROGRAPHY => D1(JOROG  : JOROG +field_length(theta_points,no_halo,1) -1)

     W         => D1(JW(wdims_s%k_start) : JW(wdims_s%k_start) + &
      field_length(theta_points,single_halo,wdims%k_end-wdims%k_start+1) -1)

     RHO       => D1(JRHO(pdims%k_start) : JRHO(pdims%k_start)+ &
      field_length(theta_points,single_halo,pdims%k_end) -1)

     QCL       => D1(JQCL(qdims%k_start) : JQCL(qdims%k_start)+ &
      field_length(theta_points,extended_halo,qdims%k_end-qdims%k_start+1) -1)

     QCF2      => D1(JQCF2(qdims%k_start) : JQCF2(qdims%k_start)+ &
      field_length(theta_points,extended_halo,qdims%k_end-qdims%k_start+1) -1)

     QRAIN     => D1(JQRAIN(qdims%k_start) : JQRAIN(qdims%k_start)+ &
      field_length(theta_points,extended_halo,qdims%k_end-qdims%k_start+1) -1)

     QGRAUP    => D1(JQGRAUP(qdims%k_start) : JQGRAUP(qdims%k_start)+ &
      field_length(theta_points,extended_halo,qdims%k_end-qdims%k_start+1) -1)

     EXNER_RHO_LEVELS => D1(JEXNER_RHO_LEVELS(pdims%k_start) : &
                            JEXNER_RHO_LEVELS(pdims%k_start) + &
                      field_length(theta_points,single_halo,pdims%k_end+1) -1)

     E_TRB     => D1(JE_TRB(tdims%k_start) : JE_TRB(tdims%k_start)+ &
      field_length(theta_points,extended_halo,tdims%k_end-tdims%k_start+1) -1)
     TSQ_TRB   => D1(JTSQ_TRB(tdims%k_start) : JTSQ_TRB(tdims%k_start)+ &
      field_length(theta_points,extended_halo,tdims%k_end-tdims%k_start+1) -1)
     QSQ_TRB   => D1(JQSQ_TRB(tdims%k_start) : JQSQ_TRB(tdims%k_start)+ &
      field_length(theta_points,extended_halo,tdims%k_end-tdims%k_start+1) -1)
     COV_TRB   => D1(JCOV_TRB(tdims%k_start) : JCOV_TRB(tdims%k_start)+ &
      field_length(theta_points,extended_halo,tdims%k_end-tdims%k_start+1) -1)
     ZHPAR_SHCU=> D1(JZHPAR_SHCU : JZHPAR_SHCU +                        &
      field_length(theta_points,no_halo,1) -1)

!    Coastal Tiling
     FRAC_LAND  => D1(JFRAC_LAND:JFRAC_LAND  + &
                                         field_length(land_points,no_halo,1) -1)
     TSTAR_LAND => D1(JTSTAR_LAND:JTSTAR_LAND+ &
                                         field_length(theta_points,no_halo,1)-1)
     TSTAR_SEA  => D1(JTSTAR_SEA:JTSTAR_SEA  + &
                                         field_length(theta_points,no_halo,1)-1)
     TSTAR_SICE => D1(JTSTAR_SICE:JTSTAR_SICE+ &
                                         field_length(theta_points,no_halo,1)-1)
     TSTAR_SICE_CAT => D1(JTSTAR_SICE_CAT:JTSTAR_SICE_CAT+ &
                                  field_length(theta_points,no_halo,nice_use)-1)

!    SeaIce and Land albedos
     SICE_ALB => D1(JSICE_ALB : JSICE_ALB + &
                                         field_length(theta_points,no_halo,1)-1)
     LAND_ALB => D1(JLAND_ALB : JLAND_ALB + &
                                         field_length(theta_points,no_halo,1)-1)

!    Large-Scale hydrology
     TI_MEAN   => D1(JTI_MEAN:JTI_MEAN+field_length(land_points,no_halo,1) -1)
     TI_SIG    => D1(JTI_SIG:JTI_SIG  +field_length(land_points,no_halo,1) -1)
     FEXP      => D1(JFEXP:JFEXP      +field_length(land_points,no_halo,1) -1)
     GAMMA_INT => D1(JGAMMA_INT:JGAMMA_INT + &
                                       field_length(land_points,no_halo,1) -1)
     FSFC_SAT  => D1(JFSFC_SAT:JFSFC_SAT   + &
                                       field_length(land_points,no_halo,1) -1)
     F_WETLAND => D1(JF_WETLAND:JF_WETLAND + &
                                       field_length(land_points,no_halo,1) -1)
     WATER_TABLE => D1(JWATER_TABLE:JWATER_TABLE+ &
                                       field_length(land_points,no_halo,1) -1)

     STHZW   => D1(JSTHZW  : JSTHZW  + field_length(land_points,no_halo,1) -1)
     A_FSAT  => D1(JA_FSAT : JA_FSAT + field_length(land_points,no_halo,1) -1)
     C_FSAT  => D1(JC_FSAT : JC_FSAT + field_length(land_points,no_halo,1) -1)
     A_FWET  => D1(JA_FWET : JA_FWET + field_length(land_points,no_halo,1) -1)
     C_FWET  => D1(JC_FWET : JC_FWET + field_length(land_points,no_halo,1) -1)

!    Optional atmospheric primary variables

     ZH        => D1(JZH    : JZH    +field_length(theta_points,no_halo,1) -1)
     ddmfx     => D1(jddmfx : jddmfx +field_length(theta_points,no_halo,1) -1)

     U_ADV     => D1(JU_ADV(udims%k_start) : JU_ADV(udims%k_start)+ &
                          field_length(u_points,extended_halo,udims%k_end) -1)
     V_ADV     => D1(JV_ADV(vdims%k_start) : JV_ADV(vdims%k_start)+ &
                          field_length(v_points,extended_halo,vdims%k_end) -1)

     W_ADV     => D1(JW_ADV(wdims_s%k_start) : JW_ADV(wdims_s%k_start)+ &
                  field_length(theta_points,extended_halo,wdims_s%k_end+1) -1)

     NTML      => D1(JNTML  : JNTML +field_length(theta_points,no_halo,1) -1)
     NBDSC     => D1(JNBDSC : JNBDSC+field_length(theta_points,no_halo,1) -1)
     NTDSC     => D1(JNTDSC : JNTDSC+field_length(theta_points,no_halo,1) -1)
     CUMULUS   => D1(JCUMULUS : JCUMULUS+field_length(theta_points,no_halo,1)-1)

     T1_SD     => D1(JT1_SD : JT1_SD+field_length(theta_points,no_halo,1) -1)
     Q1_SD     => D1(JQ1_SD : JQ1_SD+field_length(theta_points,no_halo,1) -1)

     CF_AREA   => D1(JCF_AREA(1) : JCF_AREA(1)+ &
      field_length(theta_points,no_halo      ,qdims%k_end) -1)

     CF_BULK   => D1(JCF_BULK(qdims%k_start) : JCF_BULK(qdims%k_start)+ &
      field_length(theta_points,extended_halo,qdims%k_end-qdims%k_start+1) -1)

     CF_LIQUID => D1(JCF_LIQUID(qdims%k_start) : JCF_LIQUID(qdims%k_start)+ &
      field_length(theta_points,extended_halo,qdims%k_end-qdims%k_start+1) -1)

     CF_FROZEN => D1(JCF_FROZEN(qdims%k_start) : JCF_FROZEN(qdims%k_start)+ &
      field_length(theta_points,extended_halo,qdims%k_end-qdims%k_start+1) -1)

     ! size of cca varies according to L_3D_CCA - n_cca_lev set in dervsize
     CCA => D1(JCCA(1):JCCA(1) + &
                              field_length(theta_points,no_halo,n_cca_lev) -1)

     CCB          => D1(JCCB   : JCCB  +field_length(theta_points,no_halo,1) -1)

     CCT          => D1(JCCT   : JCCT  +field_length(theta_points,no_halo,1) -1)

     CCLWP        => D1(JCCLWP : JCCLWP+field_length(theta_points,no_halo,1) -1)

     DEEP_FLAG    => D1(JDEEPFLAG : JDEEPFLAG+ &
                                    field_length(theta_points,no_halo,1) -1)
     Past_precip  => D1(JPASTPRECIP : JPASTPRECIP + &
                                    field_length(theta_points,no_halo,1) -1)
     Past_conv_ht => D1(JPASTCONVHT : JPASTCONVHT + &
                                    field_length(theta_points,no_halo,1) -1)
     CANOPY_WATER => D1(JCANOPY_WATER : JCANOPY_WATER+ &
                                     field_length(land_points,no_halo,1) -1)

     LCBASE       => D1(JLCBASE : JLCBASE + &
                                    field_length(theta_points,no_halo,1) -1)

     CCW_RAD      => D1(JCCW_RAD(1) : JCCW_RAD(1) + &
                              field_length(theta_points,no_halo,qdims%k_end) -1)

!    Secondary Fields in D1

     EXNER_THETA_LEVELS => D1(JEXNER_THETA_LEVELS(tdims%k_start):     &
                              JEXNER_THETA_LEVELS(tdims%k_start)+     &
                      field_length(theta_points,single_halo,          &
                                   tdims%k_end-tdims%k_start+1) -1)

     P => D1(JP(pdims%k_start):JP(pdims%k_start) +                    &
                    field_length(theta_points,single_halo,            &
                                  pdims%k_end+1) -1)

     P_THETA_LEVELS => D1(JP_THETA_LEVELS(tdims%k_start):             &
                          JP_THETA_LEVELS(tdims%k_start)+             &
                      field_length(theta_points,single_halo,          &
                                   tdims%k_end-tdims%k_start+1) -1)

     PSTAR =>   D1(JPSTAR : JPSTAR +field_length(theta_points,no_halo,1) -1)

     SW_INCS => D1(JSW_INCS(0) : JSW_INCS(0) + &
                       field_length(theta_points, no_halo, model_levels+1)-1)

     LW_INCS => D1(JLW_INCS(0) : JLW_INCS(0) + &
                       field_length(theta_points, no_halo, model_levels  )-1)

!    Direct PAR flux for STOCHEM
     DIRPAR => D1(JDIRPAR : JDIRPAR+field_length(theta_points,no_halo,1) -1)

!    Soil Fields
     SMCL => D1(JSMCL(1):JSMCL(1)+ &
                              field_length(land_points,no_halo,sm_levels)-1)
     DEEP_SOIL_TEMP => D1(J_DEEP_SOIL_TEMP(1):J_DEEP_SOIL_TEMP(1)+ &
                              field_length(land_points,no_halo,sm_levels)-1)
     VOL_SMC_WILT   => D1(JVOL_SMC_WILT:JVOL_SMC_WILT+ &
                                      field_length(land_points,no_halo,1)-1)
     VOL_SMC_CRIT   => D1(JVOL_SMC_CRIT:JVOL_SMC_CRIT+ &
                                      field_length(land_points,no_halo,1)-1)
     VOL_SMC_SAT    => D1(JVOL_SMC_SAT:JVOL_SMC_SAT+ &
                                      field_length(land_points,no_halo,1)-1)
     SAT_SOIL_COND  => D1(JSAT_SOIL_COND:JSAT_SOIL_COND+ &
                                      field_length(land_points,no_halo,1)-1)
     THERM_CAP  =>D1(JTHERM_CAP:JTHERM_CAP + &
                                     field_length(land_points,no_halo,1) -1)
     THERM_COND =>D1(JTHERM_COND:JTHERM_COND+ &
                                      field_length(land_points,no_halo,1)-1)
     CLAPP_HORN =>D1(JCLAPP_HORN:JCLAPP_HORN+ &
                                      field_length(land_points,no_halo,1)-1)
     SAT_SOILW_SUCTION => D1(JSAT_SOILW_SUCTION:JSAT_SOILW_SUCTION+ &
                                      field_length(land_points,no_halo,1)-1)
     STHU => D1(JSTHU(1):JSTHU(1)+ &
                              field_length(land_points,no_halo,sm_levels)-1)
     STHF => D1(JSTHF(1):JSTHF(1)+ &
                              field_length(land_points,no_halo,sm_levels)-1)

!    Roughness lenght of sea points
     Z0         => D1(JZ0:JZ0           +field_length(theta_points,no_halo,1)-1)
     GS         => D1(JGS:JGS           +field_length(land_points,no_halo,1)-1)

      call cable_set_atm_fields( D1, LEN_TOT, land_points,no_halo,sm_levels,ntiles, &
                                 jTSOIL_TILE, jSMCL_TILE, jSTHF_TILE,            &
                                 jSNOW_DEPTH3L, jSNOW_MASS3L, jSNOW_TMP3L,       &
                                 jSNOW_RHO3L, jSNOW_RHO1L, jSNOW_AGE, jSNOW_FLG3L )
!    Orography Fields
     OROG_SIL => D1(JOROG_SIL : JOROG_SIL+field_length(land_points,no_halo,1)-1)
     OROG_HO2 => D1(JOROG_HO2 : JOROG_HO2+field_length(land_points,no_halo,1)-1)
     OROG_SD  => D1(JOROG_SD  : JOROG_SD +field_length(land_points,no_halo,1)-1)
     OROG_GRAD_X  => D1(JOROG_GRAD_X : JOROG_GRAD_X+ &
                                          field_length(land_points,no_halo,1)-1)
     OROG_GRAD_Y  => D1(JOROG_GRAD_Y : JOROG_GRAD_Y+ &
                                          field_length(land_points,no_halo,1)-1)
     OROG_UNFILT  => D1(JOROG_UNFILT : JOROG_UNFILT+ &
                                          field_length(land_points,no_halo,1)-1)
     OROG_GRAD_XX => D1(JOROG_GRAD_XX : JOROG_GRAD_XX+ &
                                          field_length(land_points,no_halo,1)-1)
     OROG_GRAD_XY => D1(JOROG_GRAD_XY : JOROG_GRAD_XY+ &
                                          field_length(land_points,no_halo,1)-1)
     OROG_GRAD_YY => D1(JOROG_GRAD_YY : JOROG_GRAD_YY+ &
                                          field_length(land_points,no_halo,1)-1)

!    Sea/Sea Ice Fields
     U_SEA => D1(JU_SEA : JU_SEA    +field_length(u_points,no_halo,1) -1)
     V_SEA => D1(JV_SEA : JV_SEA    +field_length(v_points,no_halo,1) -1)
     ICE_FRACTION  => D1(JICE_FRACTION  : JICE_FRACTION + &
                        field_length(theta_points_sea_only,no_halo,1) -1)
     ICE_THICKNESS => D1(JICE_THICKNESS : JICE_THICKNESS+ &
                        field_length(theta_points_sea_only,no_halo,1) -1)
     TI => D1(JTI : JTI+field_length(theta_points_sea_only,no_halo,1) -1)
     ICE_FRACT_CAT => D1(JICE_FRACT_CAT : JICE_FRACT_CAT+ &
                              field_length(theta_points,no_halo,nice) -1)
     ICE_THICK_CAT => D1(JICE_THICK_CAT : JICE_THICK_CAT+ &
                              field_length(theta_points,no_halo,nice) -1)
     TI_CAT => D1(JTI_CAT : JTI_CAT+field_length(theta_points,no_halo,nice) -1)
     ICE_K_CAT => D1(JICE_K_CAT : JICE_K_CAT + &
                              field_length(theta_points,no_halo,nice) -1)
     U_0_P => D1(JU_0_P : JU_0_P+field_length(theta_points,no_halo,1) -1)
     V_0_P => D1(JV_0_P : JV_0_P+field_length(theta_points,no_halo,1) -1)

!    Snow Fields
     SNODEP => D1(JSNODEP : JSNODEP+field_length(theta_points,no_halo,1) -1)
     SNODEP_SEA => D1(JSNODEP_SEA : JSNODEP_SEA+ &
                           field_length(theta_points_sea_only,no_halo,1) -1)
     SNODEP_SEA_CAT => D1(JSNODEP_SEA_CAT : JSNODEP_SEA_CAT+ &
                                 field_length(theta_points,no_halo,nice) -1)
! SNSOOT may not be used as of vn6.6
     SNSOOT => D1(JSNSOOT : JSNSOOT+field_length(theta_points,no_halo,1) -1)
     CATCH_SNOW => D1(JCATCH_SNOW : JCATCH_SNOW+ &
                                field_length(land_points,no_halo,ntiles) -1)
     SNOW_GRND => D1(JSNOW_GRND : JSNOW_GRND+ &
                                field_length(land_points,no_halo,ntiles) -1)

!    Decoupled screen temperatures
     TScrnDcl_TILE => D1(JTScrnDcl_TILE : JTScrnDcl_TILE+ &
                                     field_length(land_points,no_halo,ntiles) -1)
     TScrnDcl_SSI => D1(JTScrnDcl_SSI : JTScrnDcl_SSI+ &
                                    field_length(theta_points,no_halo,1) -1)
     tStbTrans => D1(JtStbTrans : JtStbTrans+ &
                                    field_length(theta_points,no_halo,1) -1)

!    OZONE (has extra surface level for V-AT-POLES)
     IF (lexpand_ozone) THEN
!      Ozone held as zonal averages, i.e. one value per row
       o3 => d1(jozone(o3dims2%k_start):jozone(o3dims2%k_start)+ &
                                  rows * (o3dims2%k_end-o3dims2%k_start+1) -1)
     ELSE
       o3 => d1(jozone(o3dims2%k_start):jozone(o3dims2%k_start)+ &
        field_length(ozone_points,no_halo,o3dims2%k_end-o3dims2%k_start+1) -1)
     END IF

!    Tropopause-based Ozone 
     IF (tpps_ozone_levels > 0) THEN
       IF (lexpand_tpps_ozone) THEN
         tppsozone => d1(jtppsozone(o3dims2%k_start): &
                       jtppsozone(o3dims2%k_start) + rows*tpps_ozone_levels -1)
       ELSE
         tppsozone => d1(jtppsozone(o3dims2%k_start): &
                                               jtppsozone(o3dims2%k_start)+ &
                       field_length(ozone_points,no_halo,tpps_ozone_levels) -1)
       END IF
     ELSE
       tppsozone => dummy_field
     END IF

!    Ozone tracer field and cariolle parameters
     OZONE_TRACER => &
               D1(JOZONE_TRACER(tdims%k_start):JOZONE_TRACER(tdims%k_start)+ &
       field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     O3_PROD_LOSS => &
               D1(JO3_PROD_LOSS(tdims%k_start):JO3_PROD_LOSS(tdims%k_start)+ &
                             (ROWS*(tdims%k_end-tdims%k_start+1) ) -1)
     O3_P_L_VMR   => &
               D1(JO3_P_L_VMR(tdims%k_start)  :JO3_P_L_VMR(tdims%k_start)+   &
                             (ROWS*(tdims%k_end-tdims%k_start+1) ) -1)
     O3_VMR       => &
               D1(JO3_VMR (tdims%k_start)     :JO3_VMR(tdims%k_start)+       &
                             (ROWS*(tdims%k_end-tdims%k_start+1) ) -1)
     O3_P_L_TEMP  => &
               D1(JO3_P_L_TEMP(tdims%k_start) :JO3_P_L_TEMP(tdims%k_start)+  &
                             (ROWS*(tdims%k_end-tdims%k_start+1) ) -1)
     O3_TEMP      => &
               D1(JO3_TEMP(tdims%k_start)     :JO3_TEMP(tdims%k_start)+      &
                             (ROWS*(tdims%k_end-tdims%k_start+1) ) -1)    
     O3_P_L_COLO3 => &
               D1(JO3_P_L_COLO3(tdims%k_start):JO3_P_L_COLO3(tdims%k_start)+ &
                             (ROWS*(tdims%k_end-tdims%k_start+1) ) -1)
     O3_COLO3     => &
               D1(JO3_COLO3(tdims%k_start)    :JO3_COLO3(tdims%k_start)+     &
                             (ROWS*(tdims%k_end-tdims%k_start+1) ) -1)

!    Sources and Aerosol Ancillaries
     MURK_SOURCE => D1(JMURK_SOURCE(tdims%k_start) : &
                                               JMURK_SOURCE(tdims%k_start)+ &
            field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)
     SO2_EM      => D1(JSO2_EM : JSO2_EM + &
                                      field_length(theta_points,no_halo,1) -1)
     DMS_EM      => D1(JDMS_EM : JDMS_EM + &
                                      field_length(theta_points,no_halo,1) -1)
     MURK        => D1(JMURK(tdims%k_start) : JMURK(tdims%k_start)+ &
        field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)

!    Sulphur cycle
     SO2         => D1(JSO2(tdims%k_start) : JSO2(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     DMS         => D1(JDMS(tdims%k_start) : JDMS(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     SO4_AITKEN  => D1(JSO4_AITKEN(tdims%k_start) : &
                                                JSO4_AITKEN(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     SO4_ACCU    => D1(JSO4_ACCU(tdims%k_start) : JSO4_ACCU(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     SO4_DISS    => D1(JSO4_DISS(tdims%k_start) : JSO4_DISS(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     H2O2        => D1(JH2O2(tdims%k_start) : JH2O2(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     NH3         => D1(JNH3(tdims%k_start) : JNH3(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     SOOT_NEW    => D1(JSOOT_NEW(tdims%k_start) : JSOOT_NEW(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     SOOT_AGD    => D1(JSOOT_AGD(tdims%k_start) : JSOOT_AGD(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     SOOT_CLD    => D1(JSOOT_CLD(tdims%k_start) : JSOOT_CLD(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     BMASS_NEW   => D1(JBMASS_NEW(tdims%k_start) : &
                                                 JBMASS_NEW(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     BMASS_AGD   => D1(JBMASS_AGD(tdims%k_start) : &
                                                 JBMASS_AGD(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     BMASS_CLD   => D1(JBMASS_CLD(tdims%k_start) : &
                                                 JBMASS_CLD(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     OCFF_NEW    => D1(JOCFF_NEW(tdims%k_start) : JOCFF_NEW(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     OCFF_AGD    => D1(JOCFF_AGD(tdims%k_start) : JOCFF_AGD(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     OCFF_CLD    => D1(JOCFF_CLD(tdims%k_start) : JOCFF_CLD(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     SO2_NATEM   => D1(JSO2_NATEM(tdims%k_start) : &
                                                 JSO2_NATEM(tdims%k_start)+ &
      field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)
     OH          => D1(JOH(tdims%k_start) : JOH(tdims%k_start)+ &
      field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)
     HO2         => D1(JHO2(tdims%k_start) : JHO2(tdims%k_start)+ &
      field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)
     H2O2_LIMIT  => D1(JH2O2_LIMIT(tdims%k_start) :&
                                              JH2O2_LIMIT(tdims%k_start)+ &
      field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)
     O3_CHEM     => D1(JO3_CHEM(tdims%k_start) : JO3_CHEM(tdims%k_start)+ &
      field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)
     SO2_HILEM   => D1(JSO2_HILEM : JSO2_HILEM+ &
      field_length(theta_points,no_halo,1) -1)
     NH3_EM      => D1(JNH3_EM : JNH3_EM+ &
      field_length(theta_points,no_halo,1) -1)
     SOOT_EM     => D1(JSOOT_EM : JSOOT_EM+ &
      field_length(theta_points,no_halo,1) -1)
     SOOT_HILEM  => D1(JSOOT_HILEM : JSOOT_HILEM+ &
      field_length(theta_points,no_halo,1) -1)
     BMASS_EM    => D1(JBMASS_EM : JBMASS_EM+ &
      field_length(theta_points,no_halo,1) -1)
     BMASS_HILEM => D1(JBMASS_HILEM : JBMASS_HILEM+ &
      field_length(theta_points,no_halo,1) -1)
     OCFF_EM     => D1(JOCFF_EM : JOCFF_EM+ &
      field_length(theta_points,no_halo,1) -1)
     OCFF_HILEM  => D1(JOCFF_HILEM : JOCFF_HILEM+ &
      field_length(theta_points,no_halo,1) -1)
     DMS_CONC    => D1(JDMS_CONC : JDMS_CONC+ &
      field_length(theta_points,no_halo,1) -1)
     DMS_OFLUX   => D1(JDMS_OFLUX : JDMS_OFLUX+ &
      field_length(theta_points,no_halo,1) -1)

! 
! Ammonium nitrate scheme: 
     NITR_ACC  => D1(JNITR_ACC(tdims%k_start) : JNITR_ACC(tdims%k_start)+ &  
       field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)  
     NITR_DISS => D1(JNITR_DISS(tdims%k_start) : JNITR_DISS(tdims%k_start)+ &  
       field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)  

! 
! Aerosol climatologies
     ARCLBIOG_BG => D1(JARCLBIOG_BG(tdims%k_start) : &
                           JARCLBIOG_BG(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLBIOM_FR => D1(JARCLBIOM_FR(tdims%k_start) : & 
                           JARCLBIOM_FR(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLBIOM_AG => D1(JARCLBIOM_AG(tdims%k_start) : & 
                           JARCLBIOM_AG(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLBIOM_IC => D1(JARCLBIOM_IC(tdims%k_start) : & 
                           JARCLBIOM_IC(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLBLCK_FR => D1(JARCLBLCK_FR(tdims%k_start) : & 
                           JARCLBLCK_FR(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLBLCK_AG => D1(JARCLBLCK_AG(tdims%k_start) : & 
                           JARCLBLCK_AG(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLSSLT_FI => D1(JARCLSSLT_FI(tdims%k_start) : & 
                           JARCLSSLT_FI(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLSSLT_JT => D1(JARCLSSLT_JT(tdims%k_start) : & 
                           JARCLSSLT_JT(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLSULP_AC => D1(JARCLSULP_AC(tdims%k_start) : & 
                           JARCLSULP_AC(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLSULP_AK => D1(JARCLSULP_AK(tdims%k_start) : & 
                           JARCLSULP_AK(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLSULP_DI => D1(JARCLSULP_DI(tdims%k_start) : & 
                           JARCLSULP_DI(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLDUST_B1 => D1(JARCLDUST_B1(tdims%k_start) : & 
                           JARCLDUST_B1(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLDUST_B2 => D1(JARCLDUST_B2(tdims%k_start) : & 
                           JARCLDUST_B2(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLDUST_B3 => D1(JARCLDUST_B3(tdims%k_start) : & 
                           JARCLDUST_B3(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLDUST_B4 => D1(JARCLDUST_B4(tdims%k_start) : & 
                           JARCLDUST_B4(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLDUST_B5 => D1(JARCLDUST_B5(tdims%k_start) : & 
                           JARCLDUST_B5(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLDUST_B6 => D1(JARCLDUST_B6(tdims%k_start) : & 
                           JARCLDUST_B6(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLOCFF_FR => D1(JARCLOCFF_FR(tdims%k_start) : & 
                           JARCLOCFF_FR(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLOCFF_AG => D1(JARCLOCFF_AG(tdims%k_start) : & 
                           JARCLOCFF_AG(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLOCFF_IC => D1(JARCLOCFF_IC(tdims%k_start) : & 
                           JARCLOCFF_IC(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)

     ARCLDLTA_DL => D1(JARCLDLTA_DL(tdims%k_start) : & 
                           JARCLDLTA_DL(tdims%k_start) + &
        field_length(theta_points,no_halo,tdims%k_end-tdims%k_start+1) -1)
     
!    Mineral Dust Scheme
     SOIL_CLAY  => D1(JSOIL_CLAY : JSOIL_CLAY+ &
      field_length(theta_points,no_halo,1) -1)
     SOIL_SILT  => D1(JSOIL_SILT : JSOIL_SILT+ &
      field_length(theta_points,no_halo,1) -1)
     SOIL_SAND  => D1(JSOIL_SAND : JSOIL_SAND+ &
      field_length(theta_points,no_halo,1) -1)
     DUST_MREL1 => D1(JDUST_MREL1 : JDUST_MREL1+ &
      field_length(theta_points,no_halo,1) -1)
     DUST_MREL2 => D1(JDUST_MREL2 : JDUST_MREL2+ &
      field_length(theta_points,no_halo,1) -1)
     DUST_MREL3 => D1(JDUST_MREL3 : JDUST_MREL3+ &
      field_length(theta_points,no_halo,1) -1)
     DUST_MREL4 => D1(JDUST_MREL4 : JDUST_MREL4+ &
      field_length(theta_points,no_halo,1) -1)
     DUST_MREL5 => D1(JDUST_MREL5 : JDUST_MREL5+ &
      field_length(theta_points,no_halo,1) -1)
     DUST_MREL6 => D1(JDUST_MREL6 : JDUST_MREL6+ &
      field_length(theta_points,no_halo,1) -1)
     DUST_DIV1  => D1(JDUST_DIV1(tdims%k_start) : JDUST_DIV1(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     DUST_DIV2  => D1(JDUST_DIV2(tdims%k_start) : JDUST_DIV2(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     DUST_DIV3  => D1(JDUST_DIV3(tdims%k_start) : JDUST_DIV3(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     DUST_DIV4  => D1(JDUST_DIV4(tdims%k_start) : JDUST_DIV4(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     DUST_DIV5  => D1(JDUST_DIV5(tdims%k_start) : JDUST_DIV5(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)
     DUST_DIV6  => D1(JDUST_DIV6(tdims%k_start) : JDUST_DIV6(tdims%k_start)+ &
      field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)

!    Carbon Cycle
     CO2FLUX   => D1(J_CO2FLUX : J_CO2FLUX+ &
         field_length(theta_points,no_halo,1) -1)

     CO2_EMITS => D1(J_CO2_EMITS : J_CO2_EMITS+ &
         field_length(theta_points,no_halo,1) -1)

     CO2       => D1(JCO2(tdims%k_start):JCO2(tdims%k_start) + &
         field_length(theta_points,single_halo,tdims%k_end-tdims%k_start+1) -1)

!    level dependent constants
     zseak_theta => D1(JZSEAK_THETA : JZSEAK_THETA+(model_levels+1) -1)
     Ck_theta    => D1(JCK_THETA    : JCK_THETA   +(model_levels+1) -1)
     zseak_rho   => D1(JZSEAK_RHO   : JZSEAK_RHO  +(model_levels+1) -1)
     Ck_rho      => D1(JCK_RHO      : JCK_RHO     +(model_levels+1) -1)

!    User ancillaries
     USER_ANC1   => D1(JUSER_ANC1  : JUSER_ANC1 + &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC2   => D1(JUSER_ANC2  : JUSER_ANC2 + &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC3   => D1(JUSER_ANC3  : JUSER_ANC3 + &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC4   => D1(JUSER_ANC4  : JUSER_ANC4 + &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC5   => D1(JUSER_ANC5  : JUSER_ANC5 + &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC6   => D1(JUSER_ANC6  : JUSER_ANC6 + &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC7   => D1(JUSER_ANC7  : JUSER_ANC7 + &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC8   => D1(JUSER_ANC8  : JUSER_ANC8 + &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC9   => D1(JUSER_ANC9  : JUSER_ANC9 + &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC10  => D1(JUSER_ANC10 : JUSER_ANC10+ &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC11  => D1(JUSER_ANC11 : JUSER_ANC11+ &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC12  => D1(JUSER_ANC12 : JUSER_ANC12+ &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC13  => D1(JUSER_ANC13 : JUSER_ANC13+ &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC14  => D1(JUSER_ANC14 : JUSER_ANC14+ &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC15  => D1(JUSER_ANC15 : JUSER_ANC15+ &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC16  => D1(JUSER_ANC16 : JUSER_ANC16+ &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC17  => D1(JUSER_ANC17 : JUSER_ANC17+ &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC18  => D1(JUSER_ANC18 : JUSER_ANC18+ &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC19  => D1(JUSER_ANC19 : JUSER_ANC19+ &
      field_length(theta_points,no_halo,1) -1)
     USER_ANC20  => D1(JUSER_ANC20 : JUSER_ANC20+ &
      field_length(theta_points,no_halo,1) -1)
     USER_MULT1  => D1(JUSER_MULT1(1)  : JUSER_MULT1(1) + &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT2  => D1(JUSER_MULT2(1)  : JUSER_MULT2(1) + &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT3  => D1(JUSER_MULT3(1)  : JUSER_MULT3(1) + &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT4  => D1(JUSER_MULT4(1)  : JUSER_MULT4(1) + &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT5  => D1(JUSER_MULT5(1)  : JUSER_MULT5(1) + &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT6  => D1(JUSER_MULT6(1)  : JUSER_MULT6(1) + &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT7  => D1(JUSER_MULT7(1)  : JUSER_MULT7(1) + &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT8  => D1(JUSER_MULT8(1)  : JUSER_MULT8(1) + &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT9  => D1(JUSER_MULT9(1)  : JUSER_MULT9(1) + &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT10 => D1(JUSER_MULT10(1) : JUSER_MULT10(1)+ &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT11 => D1(JUSER_MULT11(1) : JUSER_MULT11(1)+ &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT12 => D1(JUSER_MULT12(1) : JUSER_MULT12(1)+ &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT13 => D1(JUSER_MULT13(1) : JUSER_MULT13(1)+ &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT14 => D1(JUSER_MULT14(1) : JUSER_MULT14(1)+ &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT15 => D1(JUSER_MULT15(1) : JUSER_MULT15(1)+ &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT16 => D1(JUSER_MULT16(1) : JUSER_MULT16(1)+ &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT17 => D1(JUSER_MULT17(1) : JUSER_MULT17(1)+ &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT18 => D1(JUSER_MULT18(1) : JUSER_MULT18(1)+ &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT19 => D1(JUSER_MULT19(1) : JUSER_MULT19(1)+ &
      field_length(theta_points,no_halo,model_levels) -1)
     USER_MULT20 => D1(JUSER_MULT20(1) : JUSER_MULT20(1)+ &
      field_length(theta_points,no_halo,model_levels) -1)

!    Tiled vegetation and triffid
     FRAC_TYP =>D1(JFRAC_TYP:JFRAC_TYP  + & 
                                       field_length(land_points,no_halo,1)-1)
     FRAC_CON1=>D1(JFRAC_CON1:JFRAC_CON1+ & 
                                       field_length(land_points,no_halo,1)-1)
     FRAC_CON2=>D1(JFRAC_CON2:JFRAC_CON2+ & 
                                       field_length(land_points,no_halo,1)-1)
     FRAC_CON3=>D1(JFRAC_CON3:JFRAC_CON3+ & 
                                       field_length(land_points,no_halo,1)-1)
     FRAC_CON4=>D1(JFRAC_CON4:JFRAC_CON4+ & 
                                       field_length(land_points,no_halo,1)-1)
     FRAC_CON5=>D1(JFRAC_CON5:JFRAC_CON5+ & 
                                       field_length(land_points,no_halo,1)-1)
     FRAC_CON6=>D1(JFRAC_CON6:JFRAC_CON6+ & 
                                       field_length(land_points,no_halo,1)-1)
     FRAC_CON7=>D1(JFRAC_CON7:JFRAC_CON7+ & 
                                       field_length(land_points,no_halo,1)-1)
     FRAC_CON8=>D1(JFRAC_CON8:JFRAC_CON8+ & 
                                       field_length(land_points,no_halo,1)-1)
     FRAC_CON9=>D1(JFRAC_CON9:JFRAC_CON9+ & 
                                       field_length(land_points,no_halo,1)-1)
     LAI_PFT  =>D1(JLAI_PFT:JLAI_PFT    + & 
                                       field_length(land_points,no_halo,1) -1)
     CANHT_PFT =>D1(JCANHT_PFT:JCANHT_PFT+ & 
                                       field_length(land_points,no_halo,1)-1)
     DISTURB_VEG =>  D1(JDISTURB:JDISTURB+ & 
                                       field_length(land_points,no_halo,1)-1)
     SOIL_ALB  =>  D1(JSOIL_ALB:JSOIL_ALB+ & 
                                       field_length(land_points,no_halo,1)-1)
     OBS_ALB_SW  =>  D1(JOBS_ALB_SW:JOBS_ALB_SW+ &  
                                       field_length(land_points,no_halo,1)-1) 
     OBS_ALB_VIS  =>  D1(JOBS_ALB_VIS:JOBS_ALB_VIS+ &  
                                       field_length(land_points,no_halo,1)-1) 
     OBS_ALB_NIR  =>  D1(JOBS_ALB_NIR:JOBS_ALB_NIR+ &  
                                       field_length(land_points,no_halo,1)-1) 
     SOIL_CARB =>D1(JSOIL_CARB:JSOIL_CARB+ & 
                                       field_length(land_points,no_halo,1)-1)
     SOIL_CARB1 => D1(JSOIL_CARB1:JSOIL_CARB1+ &
                                       field_length(land_points,no_halo,1) -1)
     SOIL_CARB2 => D1(JSOIL_CARB2:JSOIL_CARB2+ &
                                       field_length(land_points,no_halo,1) -1)
     SOIL_CARB3 => D1(JSOIL_CARB3:JSOIL_CARB3+ &
                                       field_length(land_points,no_halo,1) -1)
     SOIL_CARB4 => D1(JSOIL_CARB4:JSOIL_CARB4+ &
                                       field_length(land_points,no_halo,1) -1)
     NPP_PFT_ACC    => D1(JNPP_PFT_ACC:JNPP_PFT_ACC+ &
                                       field_length(land_points,no_halo,1) -1)
     G_LF_PFT_ACC   => D1(JG_LF_PFT_ACC:JG_LF_PFT_ACC+ &
                                       field_length(land_points,no_halo,1) -1)
     G_PHLF_PFT_ACC => D1(JG_PHLF_PFT_ACC:JG_PHLF_PFT_ACC+ &
                                       field_length(land_points,no_halo,1) -1)
     RSP_W_PFT_ACC  => D1(JRSP_W_PFT_ACC:JRSP_W_PFT_ACC+ &
                                       field_length(land_points,no_halo,1) -1)
     RSP_S_ACC  => D1(JRSP_S_ACC:JRSP_S_ACC+ &
                                       field_length(land_points,no_halo,1) -1)
     RSP_S_ACC1 => D1(JRSP_S_ACC1:JRSP_S_ACC1+ &
                                       field_length(land_points,no_halo,1) -1)
     RSP_S_ACC2 => D1(JRSP_S_ACC2:JRSP_S_ACC2+ &
                                       field_length(land_points,no_halo,1) -1)
     RSP_S_ACC3 => D1(JRSP_S_ACC3:JRSP_S_ACC3+ &
                                       field_length(land_points,no_halo,1) -1)
     RSP_S_ACC4 => D1(JRSP_S_ACC4:JRSP_S_ACC4+ &
                                       field_length(land_points,no_halo,1) -1)
     CAN_WATER_TILE => D1(JCAN_WATER_TILE:JCAN_WATER_TILE+ &
                                       field_length(land_points,no_halo,1) -1)
     CATCH_TILE  => D1(JCATCH_TILE:JCATCH_TILE+ &
                                       field_length(land_points,no_halo,1) -1)
     RGRAIN_TILE => D1(JRGRAIN_TILE:JRGRAIN_TILE+ &
                                       field_length(land_points,no_halo,1) -1)
     TSTAR_TILE  => D1(JTSTAR_TILE:JTSTAR_TILE+ &
                                         field_length(land_points,no_halo,1) -1)
     Z0_TILE     => D1(JZ0_TILE:JZ0_TILE+field_length(land_points,no_halo,1) -1)
     Z0H_TILE    => D1(JZ0H_TILE:JZ0H_TILE+field_length(land_points,no_halo,1) &
                      -1)
     SNODEP_TILE => D1(JSNODEP_TILE:JSNODEP_TILE+ &
                                         field_length(land_points,no_halo,1) -1)
     INFIL_TILE  => D1(JINFIL_TILE:JINFIL_TILE+ &
                                         field_length(land_points,no_halo, 1)-1)
     DOLR_FIELD  => D1(JDOLR:JDOLR      +field_length(theta_points,no_halo,1)-1)
     LW_DOWN     => D1(JLW_DOWN:JLW_DOWN+field_length(theta_points,no_halo,1)-1)
     SW_TILE_RTS => D1(JSW_TILE:JSW_TILE+field_length(land_points,no_halo, 1)-1)

! MORUSES - new urban two-tile scheme
      hgt   => d1(jurbhgt  :jurbhgt   +field_length(land_points,no_halo,1) -1)
      ! building height
      hwr   => d1(jurbhwr  :jurbhwr   +field_length(land_points,no_halo,1) -1)
      ! height to width
      wrr   => d1(jurbwrr  :jurbwrr   +field_length(land_points,no_halo,1) -1)
      ! width ratio
      disp  => d1(jurbdisp :jurbdisp  +field_length(land_points,no_halo,1) -1)
      ! displacement height
      ztm   => d1(jurbztm  :jurbztm   +field_length(land_points,no_halo,1) -1)
      !
      albwl => d1(jurbalbwl:jurbalbwl +field_length(land_points,no_halo,1) -1)
      ! wall albedo
      albrd => d1(jurbalbrd:jurbalbrd +field_length(land_points,no_halo,1) -1)
      ! road albedo
      emisw => d1(jurbemisw:jurbemisw +field_length(land_points,no_halo,1) -1)
      ! wall emissivity
      emisr => d1(jurbemisr:jurbemisr +field_length(land_points,no_halo,1) -1)
      ! road emissivity

!    River routing fields
     RIV_SEQUENCE  => D1(JRIV_SEQUENCE : JRIV_SEQUENCE+ &
                                     field_length(river_points,no_halo,1) -1)
     RIV_DIRECTION => D1(JRIV_DIRECTION : JRIV_DIRECTION+ &
                                     field_length(river_points,no_halo,1) -1)
     RIV_STORAGE   => D1(JRIV_STORAGE : JRIV_STORAGE+ &
                                     field_length(river_points,no_halo,1) -1)
     TOT_SURFROFF  => D1(JTOT_SURFROFF : JTOT_SURFROFF+ &
                                     field_length(land_points,no_halo,1) -1)
     TOT_SUBROFF   => D1(JTOT_SUBROFF : JTOT_SUBROFF+ &
                                     field_length(land_points,no_halo,1) -1)
     RIV_INLANDATM => D1(JRIV_INLANDATM : JRIV_INLANDATM+ &
                                     field_length(land_points,no_halo,1)  -1)
     ! these are uninitialised upon entering ATM_STEP
     RIV_IAREA     => dummy_field !D1(1:1+row_length*rows)
     RIV_SLOPE     => dummy_field !D1(1:1+row_length*rows)
     RIV_FLOWOBS1  => dummy_field !D1(1:1+row_length*rows)
     RIV_INEXT     => dummy_field !D1(1:1+row_length*rows)
     RIV_JNEXT     => dummy_field !D1(1:1+row_length*rows)
     RIV_LAND      => dummy_field !D1(1:1+row_length*rows)
     RIV_SUBSTORE  => dummy_field !D1(1:1+row_length*rows)
     RIV_SURFSTORE => dummy_field !D1(1:1+row_length*rows)
     RIV_FLOWIN    => dummy_field !D1(1:1+row_length*rows)
     RIV_BFLOWIN   => dummy_field !D1(1:1+row_length*rows)
 
!    Required for water conservation correction due to lake evaporation 
     ACC_LAKE_EVAP => D1(JACC_LAKE_EVAP:JACC_LAKE_EVAP                  & 
                   +field_length(theta_points,no_halo,1) -1)

!    Fields to be retained in dumps for coupled models using OASIS    
     C_SOLAR => D1(JC_SOLAR : JC_SOLAR + &
                    field_length(theta_points,no_halo,1) -1)

     C_BLUE =>  D1(JC_BLUE : JC_BLUE + &
                    field_length(theta_points,no_halo,1) -1)

     C_LONGWAVE => D1(JC_LONGWAVE : JC_LONGWAVE + &
                    field_length(theta_points,no_halo,1) -1)

     C_TAUX => D1(JC_TAUX : JC_TAUX + &
                    field_length(u_points,no_halo,1) -1)

     C_TAUY => D1(JC_TAUY : JC_TAUY + &
                    field_length(v_points,no_halo,1) -1)

     C_W10 => D1(JC_W10 : JC_W10 + &
                    field_length(theta_points,no_halo,1) -1)

     C_SENSIBLE => D1(JC_SENSIBLE : JC_SENSIBLE + &
                    field_length(theta_points,no_halo,1) -1)

     C_SUBLIM =>  D1(JC_SUBLIM : JC_SUBLIM + &
                    field_length(theta_points,no_halo,nice_use) -1)

     C_EVAP =>  D1(JC_EVAP : JC_EVAP + &
                    field_length(theta_points,no_halo,1) -1)

     C_FCONDTOPN => D1(JC_FCONDTOPN : JC_FCONDTOPN + &
                    field_length(theta_points,no_halo,nice) -1)

     C_TOPMELTN => D1(JC_TOPMELTN : JC_TOPMELTN + &
                    field_length(theta_points,no_halo,nice) -1)

     C_LSRAIN =>  D1(JC_LSRAIN : JC_LSRAIN + &
                    field_length(theta_points,no_halo,1) -1)

     C_LSSNOW =>  D1(JC_LSSNOW : JC_LSSNOW + &
                    field_length(theta_points,no_halo,1) -1)

     C_CVRAIN =>  D1(JC_CVRAIN : JC_CVRAIN + &
                    field_length(theta_points,no_halo,1) -1)

     C_CVSNOW =>  D1(JC_CVSNOW : JC_CVSNOW + &
                    field_length(theta_points,no_halo,1) -1)

     C_RIVEROUT => D1(JC_RIVEROUT : JC_RIVEROUT + &
                    field_length(theta_points,no_halo,1) -1)

     C_CALVING => D1(JC_CALVING : JC_CALVING + &
                    field_length(theta_points,no_halo,1) -1)
     
! JULES 2 prognostics
      SNOWDEPTH      =>  D1(JSNOWDEPTH     :JSNOWDEPTH     + &
        field_length(land_points,no_halo,ntiles) -1)
      RHO_SNOW_GRND  =>  D1(JRHO_SNOW_GRND :JRHO_SNOW_GRND + &
        field_length(land_points,no_halo,ntiles) -1)
      NSNOW          =>  D1(JNSNOW         :JNSNOW         + &
        field_length(land_points,no_halo,ntiles) -1)
      DS             =>  D1(JDS            :JDS            + &
        field_length(land_points,no_halo,ntiles*nsmax) -1)
      SICE           =>  D1(JSICE          :JSICE          + &
        field_length(land_points,no_halo,ntiles*nsmax) -1)
      SLIQ           =>  D1(JSLIQ          :JSLIQ          + &
        field_length(land_points,no_halo,ntiles*nsmax) -1)
      TSNOWLAYER     =>  D1(JTSNOWLAYER    :JTSNOWLAYER    + &
        field_length(land_points,no_halo,ntiles*nsmax) -1)
      RHO_SNOW       =>  D1(JRHO_SNOW      :JRHO_SNOW      + &
        field_length(land_points,no_halo,ntiles*nsmax) -1)
      RGRAINL        =>  D1(JRGRAINL       :JRGRAINL       + &
        field_length(land_points,no_halo,ntiles*nsmax) -1)

! FLake lake scheme prognostics
      LAKE_DEPTH     =>  D1(JLAKE_DEPTH     :JLAKE_DEPTH     + &
        field_length(land_points,no_halo,1) -1)
      LAKE_FETCH     =>  D1(JLAKE_FETCH     :JLAKE_FETCH     + &
        field_length(land_points,no_halo,1) -1)
      LAKE_T_MEAN    =>  D1(JLAKE_T_MEAN    :JLAKE_T_MEAN    + &
        field_length(land_points,no_halo,1) -1)
      LAKE_T_MXL     =>  D1(JLAKE_T_MXL     :JLAKE_T_MXL     + &
        field_length(land_points,no_halo,1) -1)
      LAKE_T_ICE     =>  D1(JLAKE_T_ICE     :JLAKE_T_ICE     + &
        field_length(land_points,no_halo,1) -1)
      LAKE_H_MXL     =>  D1(JLAKE_H_MXL     :JLAKE_H_MXL     + &
        field_length(land_points,no_halo,1) -1)
      LAKE_H_ICE     =>  D1(JLAKE_H_ICE     :JLAKE_H_ICE     + &
        field_length(land_points,no_halo,1) -1)
      LAKE_SHAPE     =>  D1(JLAKE_SHAPE     :JLAKE_SHAPE     + &
        field_length(land_points,no_halo,1))
      LAKE_G_DT      =>  D1(JLAKE_G_DT      :JLAKE_G_DT      + &
        field_length(land_points,no_halo,1))

!    Required for energy correction
     NET_FLUX  => D1(JNET_FLUX:JNET_FLUX  + &
                                     field_length(theta_points,no_halo,1) -1)
     NET_MFLUX => D1(JNET_MFLUX:JNET_MFLUX+&
                                     field_length(theta_points,no_halo,1) -1)

!    Fields carried forward from previous version
     TSTAR_ANOM => D1(JTSTAR_ANOM : JTSTAR_ANOM + &
                                     field_length(theta_points,no_halo,1) -1)

!    lateral boundary conditions

     OROG_LBC  => D1(JOROG_LBC : JOROG_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)*1  -1)

     U_LBC     => D1(JU_LBC : JU_LBC + &
       LENRIMA(fld_type_u,halo_type_extended,1)* &
                                       (udims_l%k_end-udims_l%k_start+1) -1)

     V_LBC     => D1(JV_LBC : JV_LBC +  &
       LENRIMA(fld_type_v,halo_type_extended,1)* &
                                       (vdims_l%k_end-vdims_l%k_start+1) -1)

     ! model_levels+1
     W_LBC     => D1(JW_LBC : JW_LBC + &
      LENRIMA(fld_type_p,halo_type_extended,rima_type_norm)* &
                                       (wdims_l%k_end-wdims_l%k_start+1) -1)

     RHO_LBC   => D1(JRHO_LBC : JRHO_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (pdims_l%k_end-pdims_l%k_start+1) -1)

     THETA_LBC => D1(JTHETA_LBC : JTHETA_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (tdims_l%k_end-tdims_l%k_start+1) -1)

     Q_LBC     => D1(JQ_LBC : JQ_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (qdims_l%k_end-qdims_l%k_start+1) -1)

     QCL_LBC   => D1(JQCL_LBC : JQCL_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (qdims_l%k_end-qdims_l%k_start+1) -1)

     QCF_LBC   => D1(JQCF_LBC : JQCF_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (qdims_l%k_end-qdims_l%k_start+1) -1)

     ! model_levels+1
     EXNER_LBC => D1(JEXNER_LBC : JEXNER_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)*(pdims_l%k_end+1) -1) 

     U_ADV_LBC => D1(JU_ADV_LBC : JU_ADV_LBC + &
       LENRIMA(fld_type_u,halo_type_extended,1)* &
                                        (udims_l%k_end-udims_l%k_start+1) -1)

     V_ADV_LBC => D1(JV_ADV_LBC : JV_ADV_LBC + &
       LENRIMA(fld_type_v,halo_type_extended,1)* &
                                        (vdims_l%k_end-vdims_l%k_start+1) -1)

     ! model_levels+1
     W_ADV_LBC => D1(JW_ADV_LBC : JW_ADV_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                        (wdims_l%k_end-wdims_l%k_start+1) -1) 

     QCF2_LBC  => D1(JQCF2_LBC : JQCF2_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (qdims_l%k_end-qdims_l%k_start+1) -1)

     QRAIN_LBC => D1(JQRAIN_LBC : JQRAIN_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (qdims_l%k_end-qdims_l%k_start+1) -1)

     QGRAUP_LBC  => D1(JQGRAUP_LBC : JQGRAUP_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (qdims_l%k_end-qdims_l%k_start+1) -1)

     CF_BULK_LBC => D1(JCF_BULK_LBC : JCF_BULK_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (qdims_l%k_end-qdims_l%k_start+1) -1)

     CF_LIQUID_LBC => D1(JCF_LIQUID_LBC : JCF_LIQUID_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (qdims_l%k_end-qdims_l%k_start+1) -1)

     CF_FROZEN_LBC => D1(JCF_FROZEN_LBC : JCF_FROZEN_LBC + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (qdims_l%k_end-qdims_l%k_start+1) -1)

     MURK_LBC  => D1(JMURK_LBC : JMURK_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     DUST_DIV1_LBC => D1(JDUST_DIV1_LBC : JDUST_DIV1_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     DUST_DIV2_LBC => D1(JDUST_DIV2_LBC : JDUST_DIV2_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     DUST_DIV3_LBC => D1(JDUST_DIV3_LBC : JDUST_DIV3_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     DUST_DIV4_LBC => D1(JDUST_DIV4_LBC : JDUST_DIV4_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     DUST_DIV5_LBC => D1(JDUST_DIV5_LBC : JDUST_DIV5_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     DUST_DIV6_LBC => D1(JDUST_DIV6_LBC : JDUST_DIV6_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     SO2_LBC  => D1(JSO2_LBC : JSO2_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     DMS_LBC => D1(JDMS_LBC : JDMS_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     SO4_AITKEN_LBC => D1(JSO4_AITKEN_LBC : JSO4_AITKEN_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     SO4_ACCU_LBC => D1(JSO4_ACCU_LBC : JSO4_ACCU_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     SO4_DISS_LBC => D1(JSO4_DISS_LBC : JSO4_DISS_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     NH3_LBC => D1(JNH3_LBC : JNH3_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     SOOT_NEW_LBC => D1(JSOOT_NEW_LBC : JSOOT_NEW_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     SOOT_AGD_LBC => D1(JSOOT_AGD_LBC : JSOOT_AGD_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     SOOT_CLD_LBC => D1(JSOOT_CLD_LBC : JSOOT_CLD_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     BMASS_NEW_LBC => D1(JBMASS_NEW_LBC : JBMASS_NEW_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     BMASS_AGD_LBC => D1(JBMASS_AGD_LBC : JBMASS_AGD_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     BMASS_CLD_LBC => D1(JBMASS_CLD_LBC : JBMASS_CLD_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     OCFF_NEW_LBC => D1(JOCFF_NEW_LBC : JOCFF_NEW_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     OCFF_AGD_LBC => D1(JOCFF_AGD_LBC : JOCFF_AGD_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     OCFF_CLD_LBC => D1(JOCFF_CLD_LBC : JOCFF_CLD_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     NITR_ACC_LBC => D1(JNITR_ACC_LBC : JNITR_ACC_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     NITR_DISS_LBC => D1(JNITR_DISS_LBC : JNITR_DISS_LBC + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                       (tdims_s%k_end-tdims_s%k_start+1) -1)

     U_LBC_TEND     => D1(JU_LBC_TEND : JU_LBC_TEND + &
       LENRIMA(fld_type_u,halo_type_extended,1)* &
                                       (udims_l%k_end-udims_l%k_start+1) -1)

     V_LBC_TEND     => D1(JV_LBC_TEND : JV_LBC_TEND + &
       LENRIMA(fld_type_v,halo_type_extended,1)* &
                                       (vdims_l%k_end-vdims_l%k_start+1) -1)

     ! model_levels+1
     W_LBC_TEND     => D1(JW_LBC_TEND : JW_LBC_TEND + &
      LENRIMA(fld_type_p,halo_type_extended,rima_type_norm)* &
                                       (wdims_l%k_end-wdims_l%k_start+1) -1)

     RHO_LBC_TEND   => D1(JRHO_LBC_TEND : JRHO_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (pdims_l%k_end-pdims_l%k_start+1) -1)

     THETA_LBC_TEND => D1(JTHETA_LBC_TEND : JTHETA_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (tdims_l%k_end-tdims_l%k_start+1) -1)

     Q_LBC_TEND     => D1(JQ_LBC_TEND : JQ_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (qdims_l%k_end-qdims_l%k_start+1) -1)

     QCL_LBC_TEND   => D1(JQCL_LBC_TEND : JQCL_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (qdims_l%k_end-qdims_l%k_start+1) -1)

     QCF_LBC_TEND   => D1(JQCF_LBC_TEND : JQCF_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                       (qdims_l%k_end-qdims_l%k_start+1) -1)

     ! model_levels+1
     EXNER_LBC_TEND => D1(JEXNER_LBC_TEND : JEXNER_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)*(pdims_l%k_end+1) -1)

     U_ADV_LBC_TEND => D1(JU_ADV_LBC_TEND : JU_ADV_LBC_TEND + &
       LENRIMA(fld_type_u,halo_type_extended,1)* &
                                        (udims_l%k_end-udims_l%k_start+1) -1)

     V_ADV_LBC_TEND => D1(JV_ADV_LBC_TEND : JV_ADV_LBC_TEND + &
       LENRIMA(fld_type_v,halo_type_extended,1)* &
                                        (vdims_l%k_end-vdims_l%k_start+1) -1)

     ! model_levels+1
     W_ADV_LBC_TEND => D1(JW_ADV_LBC_TEND : JW_ADV_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                        (wdims_l%k_end-wdims_l%k_start+1) -1)

     QCF2_LBC_TEND => D1(JQCF2_LBC_TEND : JQCF2_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                        (qdims_l%k_end-qdims_l%k_start+1) -1)

     QRAIN_LBC_TEND => D1(JQRAIN_LBC_TEND : JQRAIN_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                        (qdims_l%k_end-qdims_l%k_start+1) -1)

     QGRAUP_LBC_TEND => D1(JQGRAUP_LBC_TEND : JQGRAUP_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                        (qdims_l%k_end-qdims_l%k_start+1) -1)

     CF_BULK_LBC_TEND => D1(JCF_BULK_LBC_TEND : JCF_BULK_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                        (qdims_l%k_end-qdims_l%k_start+1) -1)

     CF_LIQUID_LBC_TEND => D1(JCF_LIQUID_LBC_TEND : JCF_LIQUID_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                        (qdims_l%k_end-qdims_l%k_start+1) -1)

     CF_FROZEN_LBC_TEND => D1(JCF_FROZEN_LBC_TEND : JCF_FROZEN_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_extended,1)* &
                                        (qdims_l%k_end-qdims_l%k_start+1) -1)

     MURK_LBC_TEND => D1(JMURK_LBC_TEND : JMURK_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     DUST_DIV1_LBC_TEND => D1(JDUST_DIV1_LBC_TEND : JDUST_DIV1_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     DUST_DIV2_LBC_TEND => D1(JDUST_DIV2_LBC_TEND : JDUST_DIV2_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     DUST_DIV3_LBC_TEND => D1(JDUST_DIV3_LBC_TEND : JDUST_DIV3_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     DUST_DIV4_LBC_TEND => D1(JDUST_DIV4_LBC_TEND : JDUST_DIV4_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     DUST_DIV5_LBC_TEND => D1(JDUST_DIV5_LBC_TEND : JDUST_DIV5_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     DUST_DIV6_LBC_TEND => D1(JDUST_DIV6_LBC_TEND : JDUST_DIV6_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     SO2_LBC_TEND  => D1(JSO2_LBC_TEND : JSO2_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     DMS_LBC_TEND => D1(JDMS_LBC_TEND : JDMS_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     SO4_AITKEN_LBC_TEND => D1(JSO4_AITKEN_LBC_TEND : JSO4_AITKEN_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     SO4_ACCU_LBC_TEND => D1(JSO4_ACCU_LBC_TEND : JSO4_ACCU_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     SO4_DISS_LBC_TEND => D1(JSO4_DISS_LBC_TEND : JSO4_DISS_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     NH3_LBC_TEND => D1(JNH3_LBC_TEND : JNH3_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     SOOT_NEW_LBC_TEND => D1(JSOOT_NEW_LBC_TEND : JSOOT_NEW_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     SOOT_AGD_LBC_TEND => D1(JSOOT_AGD_LBC_TEND : JSOOT_AGD_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     SOOT_CLD_LBC_TEND => D1(JSOOT_CLD_LBC_TEND : JSOOT_CLD_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     BMASS_NEW_LBC_TEND => D1(JBMASS_NEW_LBC_TEND : JBMASS_NEW_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     BMASS_AGD_LBC_TEND => D1(JBMASS_AGD_LBC_TEND : JBMASS_AGD_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     BMASS_CLD_LBC_TEND => D1(JBMASS_CLD_LBC_TEND : JBMASS_CLD_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     OCFF_NEW_LBC_TEND => D1(JOCFF_NEW_LBC_TEND : JOCFF_NEW_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     OCFF_AGD_LBC_TEND => D1(JOCFF_AGD_LBC_TEND : JOCFF_AGD_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     OCFF_CLD_LBC_TEND => D1(JOCFF_CLD_LBC_TEND : JOCFF_CLD_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     NITR_ACC_LBC_TEND => D1(JNITR_ACC_LBC_TEND : JNITR_ACC_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)

     NITR_DISS_LBC_TEND => D1(JNITR_DISS_LBC_TEND : JNITR_DISS_LBC_TEND + &
       LENRIMA(fld_type_p,halo_type_single,1)* &
                                        (tdims_s%k_end-tdims_s%k_start+1) -1)


! Oxidant concentrations from UKCA for use in HadGEM sulphur
! cycle and ammonium nitrate scheme (these are in Section 33):
      IF(L_SULPC_ONLINE_OXIDANTS .AND. L_UKCA) THEN 
         OH_UKCA   => D1(JOH_UKCA(tdims%k_start) : JOH_UKCA(tdims%k_start)+ &
                       field_length(theta_points,no_halo,     &
                                            tdims%k_end-tdims%k_start+1) -1)
         H2O2_UKCA => D1(JH2O2_UKCA(tdims%k_start): JH2O2_UKCA(tdims%k_start)+ &
                       field_length(theta_points,single_halo, &
                                            tdims%k_end-tdims_s%k_start+1) -1)
         HO2_UKCA  => D1(JHO2_UKCA(tdims%k_start): JHO2_UKCA(tdims%k_start)+ &
                       field_length(theta_points,no_halo,     &
                                            tdims%k_end-tdims%k_start+1) -1)
         O3_UKCA   => D1(JO3_UKCA(tdims%k_start)   : JO3_UKCA(tdims%k_start)+ &
                       field_length(theta_points,single_halo, &
                                            tdims%k_end-tdims_s%k_start+1) -1)
         HNO3_UKCA => D1(JHNO3_UKCA(tdims%k_start):JHNO3_UKCA(tdims%k_start)+ &
                       field_length(theta_points,single_halo, &
                                            tdims%k_end-tdims_s%k_start+1) -1)
      ELSE 
        OH_UKCA   => dummy_field
        H2O2_UKCA => dummy_field
        HO2_UKCA  => dummy_field
        O3_UKCA   => dummy_field
        HNO3_UKCA => dummy_field
      END IF  

! 1.1 point tracer fields to D1

      ! find out how many tracers are active
      nActiveTracers=0
      DO nTracer=A_TRACER_FIRST,A_TRACER_LAST
        IF (SI(nTracer,33,atmos_im) /= 1) THEN
          nActiveTracers = nActiveTracers+1
        END IF
      END DO ! nTracer

      IF (nActiveTracers /= 0) THEN

        ! set the pointer to the appropriate section of D1
        TRACER => D1( JTRACER(tdims%k_start,A_TRACER_FIRST) : &
                      JTRACER(tdims%k_start,A_TRACER_FIRST) + &
          field_length(theta_points,single_halo,tr_levels)*nActiveTracers -1)

      ELSE
        ! or set it to something non-null if there are no active tracers
        TRACER => dummy_field
      END IF

      ! do the same for section 34 (UKCA) tracers     
      nActiveTracers=0
      DO nTracer=A_UKCA_FIRST,A_UKCA_LAST
        IF (SI(nTracer,34,atmos_im) /= 1) THEN
          nActiveTracers = nActiveTracers+1
        END IF
      END DO ! nTracer 

      IF (nActiveTracers /= 0) THEN

        TRACER_UKCA => D1( JTR_UKCA(tdims%k_start,A_UKCA_FIRST) : &
                           JTR_UKCA(tdims%k_start,A_UKCA_FIRST) + &
          field_length(theta_points,single_halo,tr_levels)*nActiveTracers -1)

      ELSE
        TRACER_UKCA => dummy_field
      END IF

     ! find out how many free tracer LBCs are active
      nActiveTracers=0
      DO nTracer=1,TR_LBC_VARS
        IF (SI(nTracer,36,atmos_im) /= 1) THEN
          nActiveTracers = nActiveTracers+1
        END IF
      END DO ! nTracer

      IF (nActiveTracers /= 0) THEN
        ! set the pointer to the appropriate section of D1
        TRACER_LBC => D1(JTRACER_LBC(1) : &
         JTRACER_LBC(nActiveTracers) + &
         (LENRIMA(fld_type_p,halo_type_extended,1)*(tr_levels-tdims%k_start+1)))
        TRACER_LBC_TEND => D1(JTRACER_LBC_TEND(1) : &
         JTRACER_LBC_TEND(nActiveTracers) + &
         (LENRIMA(fld_type_p,halo_type_extended,1)*(tr_levels-tdims%k_start+1)))
      ELSE
        ! or set it to something non-null if there are no active tracer LBCs
        TRACER_LBC => dummy_field
        TRACER_LBC_TEND => dummy_field
      END IF

      ! find out how many UKCA tracer LBCs are active
      nActiveTracers=0
      DO nTracer=1,TR_LBC_UKCA
        IF (SI(nTracer,37,atmos_im) /= 1) THEN
          nActiveTracers = nActiveTracers+1
        END IF
      END DO ! nTracer

      IF (nActiveTracers /= 0) THEN
        ! set the pointer to the appropriate section of D1
        TRACER_UKCA_LBC => D1(JTR_UKCA_LBC(1) : &
         JTR_UKCA_LBC(nActiveTracers) + &
         (LENRIMA(fld_type_p,halo_type_extended,1)*(tr_levels-tdims%k_start+1)))
        TRACER_UKCA_LBC_TEND => D1(JTR_UKCA_LBC_TEND(1) : &
         JTR_UKCA_LBC_TEND(nActiveTracers) + &
         (LENRIMA(fld_type_p,halo_type_extended,1)*(tr_levels-tdims%k_start+1)))
      ELSE
        ! set to something non-null if there are no active ukca tracer LBCs
        TRACER_UKCA_LBC => dummy_field
        TRACER_UKCA_LBC_TEND => dummy_field
      END IF
      IF (lhook) CALL dr_hook('SET_ATM_FIELDS',zhook_out,zhook_handle)
      RETURN

END SUBROUTINE Set_Atm_Fields
