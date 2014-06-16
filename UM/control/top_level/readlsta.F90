! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!+ Read run-time control information from namelists for atmos model
!
! Subroutine Interface:
      SUBROUTINE Readlsta()

      USE umsections_mod, ONLY: atmos_sr
      USE switches, ONLY: l_flake_model, l_ssice_albedo
      USE rad_input_mod
      USE sw_rad_input_mod
      USE lw_rad_input_mod
      USE MAX_CALLS, ONLY : npd_swcall, npd_lwcall
      USE clmchfcg_scenario_mod, ONLY: L_clmchfcg, clim_fcg_nyears,     &
                   clim_fcg_rates, nclmfcgs, clmchfcg
      USE CORADOCA

      USE um_input_control_mod, ONLY:                                   &
           l_oasis
      USE cv_run_mod                         ! Access to all variables
      USE cv_param_mod                       ! Access to all variables
      USE check_iostat_mod
      
      USE turb_diff_mod, ONLY: run_diffusion,                                 &
          l_print_shear, l_print_max_wind, l_print_theta1, l_diag_l2norms,    &
          l_diag_l2helm, l_print_lapse, l_diag_print, l_flush6, l_print_pe,   &
          l_diag_print_ops, l_print_div, l_print_wmax, l_print_w,             &
          diag_interval, print_step, first_norm_print, up_diff_scale,         &
          polar_filter_north_lat_limit, polar_filter_step_per_sweep,          &
          polar_filter_lat_limit, polar_filter_south_lat_limit, norm_lev_end, &
          norm_lev_start, w_print_limit, l_diag_wind, dom_n_in, dom_s_in,     &
          dom_e_in, dom_w_in, blocky_in, blockx_in, l_diag_noise, vdiffuv_end,&
          vdiffuv_start, l_vdiff_uv, l_adjust_theta, adjust_lapse_min,        &
          adjust_theta_end, adjust_theta_start, vdiffuv_test, l_filter_incs,  &
          l_filter, ramp_lat_radians, l_diff_auto, diff_coeff_ref,            &
          ref_lat_deg, max_sweeps, vdiffuv_timescale, tar_horizontal,         &
          sponge_power, sponge_ns, q_pos_method, qpos_diag_limit,             &
          l_qpos_diag_pr, q_pos_tracer_method, sponge_ew, top_filt_end,       &
          top_filt_start, l_upper_ramp, top_diff, l_sponge, l_pofil_new 
      USE Submodel_Mod
      USE missing_data_mod, ONLY: rmdi, imdi
      
      USE sl_input_mod

! module for RUN_PRECIP namelist
      USE mphys_inputs_mod, ONLY:                                       &
      ! Logicals block 1
        l_cry_agg_dep, l_it_melting, l_autoc_3b, l_warm_new,            &
        l_autolim_3b,  l_psd, l_psd_global, l_autoconv_murk,            &
      ! cloud rain correlation coefficient
        c_r_correl,                                                     &
      ! Drop-size distribution parameters
        x1r, x2r,                                                       &
        ai,  bi,  aic, bic,                                             &
       lsp_eic, lsp_fic,                                                &
      ! Number of iterations of microphysics
        lsiter,                                                         &
      ! Droplet taper parameters
        z_peak_nd, ndrop_surf,                                          &
      ! Logicals block 2
        l_droplet_tpr,   l_rainfall_as,                                 &
        l_mcr_iter,      l_mcr_qrain,      l_mcr_qgraup,                &
        l_mcr_qrain_lbc, l_mcr_qgraup_lbc, l_rain,                      &
        RUN_PRECIP

! module for RUN_CLOUD namelist
      USE cloud_inputs_mod, ONLY: pc2_falliceshear_method,              &
       cloud_fraction_method, i_fixbug_pc2_checks,                      &
       i_pc2_conv_coupling, i_pc2_erosion_method, L_eacf,               &
       l_ensure_min_in_cloud_qcf, l_fixbug_pc2_qcl_incr,                &
       l_fixbug_pc2_mixph, l_micro_eros,                                &
       dbsdtbs_turb_0,                                                  &
       starticeTKelvin, alliceTdegC, cff_spread_rate, ice_width, rhcrit,& 
       l_pc2, l_rhcpt, RUN_CLOUD, check_run_cloud,                      &
       l_filter_cloud, tau_thresh

! module for RUN_RIVERS namelist
      USE river_inputs_mod, ONLY: RUN_RIVERS

! module for RUN_Eng_Corr namelist
      USE eng_corr_inputs_mod, ONLY: RUN_Eng_Corr

! module for RUN_Murk namelist
      USE murk_inputs_mod, ONLY: RUN_Murk

! module for RUN_LAND, RUN_BLVEG and RUN_PFT namelists
      USE land_surf_mod

! module for mineral dust scheme options
      USE dust_parameters_mod, ONLY:                                    &
         RUN_Dust, dust_parameters_check, dust_parameters_load,         &
         dust_size_dist_initialise

! module for aerosol emissions options
      USE run_aerosol_mod, ONLY: RUN_Aerosol

! module for Boundary Layer options (run_BL name list)
      USE bl_option_mod, ONLY: run_bl, cor_mo_iter, ishear_bl,          &
            alpha_cd_batches, alpha_cd_items, alpha_cd_vals, alpha_cd,  &
            l_lambdam2, l_full_lambdas, nl_bl_levels, seasalinityfactor,&
            iseaz0t, puns, pstb, sbl_op, variable_ric, idyndiag,        &
            isrfexcnvgust, fric_heating, subs_couple_fix, l_us_blsol,   &
            bl_option_off=>off, check_run_bl

! module for UKCA options
      USE ukca_option_mod, ONLY: run_ukca,                        &
         l_ukca, l_ukca_aie1, l_ukca_aie2,                        &
         i_ukca_chem, L_ukca_achem,                               & 
         L_ukca_mode, L_ukca_dust, L_ukca_ageair,                 &
         L_ukca_qch4inter,                                        &
         L_ukca_useumuivals,                                      &
         L_ukca_het_psc, L_ukca_sa_clim,                          &
         L_ukca_h2o_feedback,                                     &
         L_ukca_rado3, L_ukca_radch4, L_ukca_radn2o,              &
         L_ukca_radf11, L_ukca_radf12, L_ukca_radf113,            &
         L_ukca_radf22,                                           &
         L_ukca_intdd, L_ukca_trophet, L_ukca_prescribech4,       &
         L_ukca_set_trace_gases, L_ukca_use_background_aerosol,   &
         L_ukca_primsu, L_ukca_primss,                            &
         L_ukca_primbcoc, L_ukca_primdu, L_ukca_use_2dtop,        &
         L_bcoc_ff, L_bcoc_bf, L_bcoc_bm, L_mode_bhn_on,          &
         L_mode_bln_on, L_ukca_arg_act,                           &
         L_ukca_sfix, i_mode_setup, i_mode_nzts,                  &
         i_mode_bln_param_method, mode_parfrac, dts0, nit,        &
         jvspec_dir, jvspec_file, jvscat_file, phot2d_dir,        &
         strat2d_dir, fastjx_numwl, fastjx_mode,                  &
         fastjx_prescutoff, dir_strat_aer, file_strat_aer,        &
         ukca_MeBrmmr, ukca_MeClmmr, ukca_CH2Br2mmr, ukca_H2mmr,  &
         ukca_N2mmr, ukca_CFC115mmr, ukca_CCl4mmr,                &
         ukca_MeCCl3mmr, ukca_HCFC141bmmr, ukca_HCFC142bmmr,      &
         ukca_H1211mmr, ukca_H1202mmr, ukca_H1301mmr,             &
         ukca_H2402mmr, ukca_COSmmr

      USE ukca_init_mod, ONLY: ukca_init

! module for TKE schemes options
      USE mym_option_mod, ONLY:                                         &
       bdy_tke, tke_dlen, l_tke_dlen_blackadar, tke_cm_mx, tke_cm_fa,   &
       l_my3_improved_closure, my_lowest_pd_surf,                       &
       l_my_condense, l_shcu_buoy, l_adv_turb_field,                    &
       l_my_extra_level, my_z_extra_fact, l_my_prod_adj,                &
       my_prod_adj_fact, tke_levels,                                    &
       l_my_initialize, l_my_ini_zero, l_local_above_tkelvs,            &
       none, my_length, no_pd_surf,                                     &
       high_order_scheme_adv_turb, monotone_scheme_adv_turb,            &
       l_high_adv_turb, l_mono_adv_turb, l_conserv_adv_turb,            &
       l_print_max_tke, l_my_lowest_pd_surf_tqc, my_ini_dbdz_min,       &
       my_z_limit_elb, wb_ng_max, shcu_levels


! Module for stochastic physics UM section 35 : SKEB2
      USE stochastic_physics_run_mod, ONLY:                             &
       l_skeb2, l_rp2, run_stochastic,                                  &
      ! Stochastic parameters for ls precip
        m_ci, m_ci_max, m_ci_min, rhcrit_max,  rhcrit_min,              &
      ! Logic control subroutine
        check_run_stochastic

! Module for IAU scheme:
      USE IAU_mod, ONLY : &
          L_IAU,          &
          IAU_nl

! Module for Nudging scheme:
      USE nudging_input_mod, ONLY : Run_Nudging

! Module for GWD scheme:
      USE g_wave_input_mod         

      USE conversions_mod, ONLY: pi
      
      USE q_pos_method_mod, ONLY : &
          q_pos_local

! Modules for reading JULES namelists:
      USE read_jules_namelists_mod, ONLY:                                     &
          read_jules_switches,   read_jules_nvegparm,   read_jules_pftparm,   &
          read_jules_triffid,    read_jules_snow_param, read_jules_soil_param,&
          read_jules_surf_param, read_jules_elevate,    read_jules_rad_param, &
          read_jules_csmin,      read_jules_seed,       read_jules_sigm,      &
          read_urban_switches,   read_urban2t_param

      USE parkind1, ONLY: jprb, jpim
      USE yomhook, ONLY: lhook, dr_hook
      USE ereport_mod, ONLY : ereport
      USE PrintStatus_mod
      USE UM_ParVars
      USE Control_Max_Sizes

      USE science_fixes_mod

! Module for COSP
      USE cosp_input_mod, ONLY: run_cosp

      USE c_gwave_mod, ONLY: nsigma, amplitude_saturation,              &
                             stress_saturation, beta_fix, frac_wl,      &
                             lambdaz_min, lambdaz_max, nsq_neutral,     &
                             zav_converge, zav_iterate

      USE dynamics_input_mod
      USE dynamics_testing_mod
      USE blopt8a, ONLY : Limit_ObukhovL

      USE nlstcall_mod, ONLY : run_assim_mode

      USE chsunits_mod, ONLY : nunits

      IMPLICIT NONE
!
! Description: Read run-time control information passed as namelists
!   from the UMUI, as required to set up parametrization constants and
!   logical switches needed by physics and dynamics schemes for the
!   Atmosphere model.
! Method:  Sequential read of namelists. Note that defaults are set to
!   missing data indicators. 
!   Namelist variables/declarations which are NOT in modules listed above 
!   are held in cruntimc.h include file.
!
! Code Owner: See Unified Model Code Owners HTML page
! This file belongs in section: Top Level
!
! Code Description:
!   Language: FORTRAN 77 + common extensions.
!   This code is written to UMDP3 v6 programming standards.
!
! System component: Control Atmos
!
! Declarations:
!
! Global variables (*CALLed COMDECKs etc...):
#include "typsize.h"
#include "typcona.h" 
#include "typlndm.h"
#include "ccontrol.h"
#include "cruntimc.h"

!
! Subroutine arguments
!   Scalar arguments with intent(in):
!   Array  arguments with intent(in):
!   Scalar arguments with intent(InOut):
!   Array  arguments with intent(InOut):
!   Scalar arguments with intent(out):
!   Array  arguments with intent(out):

! Local parameters:
      CHARACTER(LEN=*) RoutineName
      PARAMETER (   RoutineName='Readlsta')

! Local scalars:
      INTEGER                                                           &
     & ErrorStatus      ! Return code : 0 Normal Exit : >0 Error
      CHARACTER(LEN=256)                                                     &
     & CMessage         ! Error message if Errorstatus >0

      INTEGER :: level ,i, j, k, jj

      INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
      INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
      REAL(KIND=jprb)               :: zhook_handle

                             
      IF (lhook) CALL dr_hook('READLSTA',zhook_in,zhook_handle)
      ErrorStatus = 0


! Read atmosphere run consts. control data into COMMON


!     **********    Dynamics defaults   *********

! Dynamics MDI defaults
      ramp_lat_radians = RMDI
! Diffusion defaults
      L_filter = .false.
      L_filter_incs = .false.
      L_diff_auto = .false.
      max_sweeps = 8
      ref_lat_deg = 0.0
      diff_coeff_ref = 0.25
      vdiffuv_test = 100.0
      L_vdiff_uv = .false.
      vdiffuv_start = imdi
      vdiffuv_end = imdi
      L_adjust_theta = .false.
      adjust_theta_start = imdi
      adjust_theta_end = imdi
      adjust_lapse_min = 0.0
      vdiffuv_timescale = 1
      L_upper_ramp = .false.
      top_filt_start = imdi
      top_filt_end = imdi
      top_diff = 0.1
      up_diff_scale = 0.5
      L_pofil_new = .false.
      L_sponge = .false.
      sponge_ew = 0
      sponge_ns = 0
      sponge_power = 1

      tar_horizontal = 0


!    QPOS defaults
      q_pos_method           = q_pos_local   ! original with local
      q_pos_tracer_method    = q_pos_local   ! original with local
      l_qpos_diag_pr         = .FALSE.       ! QPOS diagnostics off
      qpos_diag_limit        = -1.0e-8


!     **********    Dynamics defaults   *********

!   Diagnostic printing defaults
      L_print_pe = .false.
      L_flush6 = .false.
      L_diag_print = .false.
      L_diag_print_ops = .false.
      L_print_w = .false.
      L_print_wmax = .false.
      L_print_div = .false.
      L_print_lapse = .false.
      L_print_theta1 = .false.
      L_print_max_wind = .false.
      L_print_shear = .false.
      L_diag_L2norms = .false.
      L_diag_L2helm = .false.
      print_step = 1
      diag_interval = 1
      first_norm_print = 1
      w_print_limit = 1000.0
      norm_lev_start = 1
      norm_lev_end = imdi
      L_print_pe = .false.
      L_diag_wind = .false.
      L_diag_noise = .false.
      dom_w_in = 0
      dom_e_in = 0
      dom_s_in = 0
      dom_n_in = 0
      blockx_in = 0
      blocky_in = 0

!     In common block CDERIVED (in comdeck CCONSTS called in TYPCONA)

! Default settings for h_split (can be overridden by UMUI-supplied
!  values if this is required at a future release, when rmdi defaults
!  should be reinstated):
!  low:middle:high cloud model levels =(1)->(2):(2)->(3):(3)->(4)
      h_split(1) =   111.        ! ICAO 1000mb height (m)
      h_split(2) =  1949.        ! ICAO  800mb height (m)
      h_split(3) =  5574.        ! ICAO  500mb height (m)
      h_split(4) = 13608.        ! ICAO  150mb height (m)

      DO J = 1,MAX_REQ_THPV_LEVS
        REQ_THETA_PV_LEVS(J) = RMDI
      END DO

      REWIND(UNIT=5)
! -----------------------------------------
! read in scientific fixes namelist
! controls what fixes are applied 
! each of these fixes are anticipated 
! to become the default code in the future
! -----------------------------------------

      READ (UNIT=5, NML=temp_fixes, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist temp_fixes")
      
      CALL warn_temp_fixes()
 
! UKCA Sub-model
      READ (UNIT=5, NML=run_ukca, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_UKCA")

! Read in Physics/Dynamics namelists

! Gravity wave drag physics
      READ (UNIT=5, NML=RUN_GWD, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_GWD")

! Murk aerosol physics
      READ (UNIT=5, NML=RUN_Murk, IOSTAT=ErrorStatus) 
      CALL check_iostat(errorstatus, "namelist RUN_Murk")

! Convection physics
      READ (UNIT=5, NML=RUN_Convection, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_Convection")

! Set other dependent convective switches valid for whole run
! DEPENDS ON: cv_set_dependent_switches
      CALL cv_set_dependent_switches

! Boundary layer physics
      READ (UNIT=5, NML=RUN_BL, IOSTAT=ErrorStatus) 
      CALL check_iostat(errorstatus, "namelist RUN_BL")
      CALL check_run_bl()

! River routing
      READ (UNIT=5, NML=RUN_RIVERS, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_RIVERS")

! Large scale precipitation physics 
      READ (UNIT=5, NML=RUN_Precip, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_Precip")
      
! Radiation physics
      READ (UNIT=5, NML=RUN_Radiation, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_Radiation")
      CALL check_run_radiation()

! Mineral dust modelling
! Initialise sizes in case not set in namelist
      CALL dust_size_dist_initialise
      READ (UNIT=5, NML=RUN_Dust, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_Dust")
      CALL dust_parameters_load
      CALL dust_parameters_check

! Large scale cloud physics
      READ (UNIT=5, NML=RUN_Cloud, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_Cloud")
      CALL check_run_cloud()
    
! Surface type characteristics
      READ (UNIT=5, NML=RUN_BLVEG, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_BLVEG")
      
! Energy correction physics
      READ (UNIT=5, NML=RUN_Eng_Corr, IOSTAT=ErrorStatus) 
      CALL check_iostat(errorstatus, "namelist RUN_Eng_Corr")

! Stochastic physics
      READ (UNIT=5, NML=RUN_Stochastic, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_Stochastic")
      CALL check_run_stochastic()
     
! Aerosol Modelling
      READ (UNIT=5, NML=RUN_Aerosol, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_Aerosol")

! Check UKCA logicals are consistent and 
! set internal UKCA values from UKCA namelists
! Needs to be after CLASSIC aerosol namelist is read as 
! well as after UKCA
      CALL ukca_init()
      
! UM nudging
      READ (UNIT=5, NML=RUN_Nudging, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_Nudging")
      
! Generalised integration and GCR dynamics
      READ (UNIT=5, NML=RUN_Dyn, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_Dyn")
      CALL check_run_dyn()

! Generalised integration and GCR dynamics
      READ (UNIT=5, NML=RUN_Dyntest, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_Dyntest")
      CALL check_run_dyntest()

! Semi-Lagrangian advection dynamics
      READ (UNIT=5, NML=RUN_SL, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_SL")
      CALL check_run_sl()

! Diffusion, divergence damping and filtering dynamics
      READ (UNIT=5, NML=RUN_Diffusion, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_Diffusion")
   
! Call to COSP
      READ (UNIT=5, NML=RUN_COSP, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RUN_COSP")

! Diagnostic double call to radiation
      READ (UNIT=5, NML=RADFCDIA, IOSTAT=ErrorStatus)
      CALL check_iostat(errorstatus, "namelist RADFCDIA")


!------------------------------------------------------- 
! Check that if L_CCRAD is selected certain other
! switches are not set so that they conflict.
!------------------------------------------------------- 
! Error capture 
!------------------------------------------------------- 
                 
      If (L_ccrad) Then 

        If (.NOT. L_3D_CCA) Then 
          ErrorStatus = 100 
          CMessage    = '**ERROR**: CCRad is not yet available without'// &
                                  ' the anvil scheme (L_3D_CCA = .True.)'
 
          CALL ereport(RoutineName, ErrorStatus, CMessage) 
        End If

        If (l_fix_udfactor) Then 
          ErrorStatus = 101 
          CMessage    = '**ERROR**: L_CCRad and l_fix_udfactor'//         &
                                  ' should not be both set to true.'
 
          CALL ereport(RoutineName, ErrorStatus, CMessage) 
        End If

        If (l_pc2_diag_sh) Then 
          ErrorStatus = 102 
          CMessage    = '**ERROR**: L_CCRad and l_pc2_diag_sh'//          &
                                  ' should not be both set to true.'
 
          CALL ereport(RoutineName, ErrorStatus, CMessage) 
        End If

      End If       ! l_ccrad 

      IF (l_pc2) THEN 
        ! So that PC2 does NOT affect Section 5 diagnostics
        l_dcpl_cld4pc2=.TRUE.
      END IF

!------------------------------------------------------- 
! End error capture 
!-------------------------------------------------------

      IF (L_Backwards) L_Physics = .false.

      IF(PrintStatus >= PrStatus_Normal) THEN

        WRITE(6,'(/,A,A,A)') '******************** ',RoutineName,         &
                    ': Atmosphere run-time constants *******************'
                    
        WRITE(6,temp_fixes)            
        WRITE(6,RUN_Cloud)
        WRITE(6,RUN_BLVEG)
        WRITE(6,RUN_BL)
        WRITE(6,RUN_Eng_Corr)
        WRITE(6,RUN_Murk)
        WRITE(6,RUN_Precip)
        WRITE(6,RUN_Convection)
        WRITE(6,RUN_Stochastic)
        WRITE(6,RUN_Radiation)
        WRITE(6,RUN_GWD)
        WRITE(6,RUN_Aerosol)
        WRITE(6,RUN_Dust)
        WRITE(6,RUN_UKCA)
        WRITE(6,RUN_Nudging)
        WRITE(6,RUN_Dyn)
        WRITE(6,RUN_Dyntest)
        WRITE(6,RUN_SL)
        WRITE(6,RUN_Diffusion)
        WRITE(6,RUN_RIVERS)
        WRITE(6,RUN_COSP)
        WRITE(6,RADFCDIA)

      END IF ! PrintStatus



! Convert from degrees to radians
      polar_filter_north_lat_limit= polar_filter_north_lat_limit*pi/180.
      polar_filter_south_lat_limit= polar_filter_south_lat_limit*pi/180.
      polar_filter_lat_limit      = polar_filter_lat_limit      *pi/180.
      polar_filter_step_per_sweep = polar_filter_step_per_sweep *pi/180.
      ramp_lat_radians = ramp_lat_radians *pi/180.



!  Multiply input req_theta_pv_levs by 100. to convert mb to pascals.
      DO LEVEL = 1,MAX_REQ_THPV_LEVS
        REQ_THETA_PV_LEVS(LEVEL) = REQ_THETA_PV_LEVS(LEVEL)*100.
      END DO

! Set radiation aerosol switches
      IF (cusack_aero==2 .OR.  cusack_aero==3    ) l_climat_aerosol    = .TRUE.
      IF (cusack_aero==3 .AND. cusack_aero_hgt==1) l_clim_aero_hgt     = .TRUE.
      IF (cusack_aero==2)                          l_HadGEM1_clim_aero = .TRUE.



!
!     Read controlling information for version 3C/Z of the SW or LW
!     radiation schemes.
!
      READ (UNIT=5, NML=R2SWNCAL, IOSTAT=ErrorStatus)   
      CALL check_iostat(errorstatus, "namelist R2SWNCAL")
      
      IF (n_swcall > npd_swcall) THEN
        ErrorStatus = 100 
        CMessage    = '**ERROR**: Too many calls to SW radiation:'//  &
                      ' n_swcall > npd_swcall. Increase npd_swcall'// &
                      ' in max_calls.F90 and recompile.'
 
        CALL ereport(RoutineName, ErrorStatus, CMessage) 
      END IF

! Options for the shortwave
      CALL sw_input
        
      READ (UNIT=5, NML=R2LWNCAL, IOSTAT=ErrorStatus)   
      CALL check_iostat(errorstatus, "namelist R2LWNCAL")
      IF (n_lwcall > npd_lwcall) THEN
        ErrorStatus = 100 
        CMessage    = '**ERROR**: Too many calls to LW radiation:'//  &
                      ' n_lwcall > npd_lwcall. Increase npd_lwcall'// &
                      ' in max_calls.F90 and recompile.'
 
        CALL ereport(RoutineName, ErrorStatus, CMessage) 
      END IF

!       Options for the longwave 
      CALL lw_input
          
      READ (UNIT=5, NML=clmchfcg, IOSTAT=ErrorStatus)   
      CALL check_iostat(errorstatus, "namelist CLMCHFCG")
      
      IF ( L_clmchfcg ) THEN
         !  Convert rates from percent to multiplicative factors:
         DO j = 1, nclmfcgs
!          !  This is a null loop, as it should be, if clim_fcg_nyears=0
           DO jj = 1, clim_fcg_nyears(j)
             IF ( clim_fcg_rates(jj,j)  >   -100. )  THEN
                clim_fcg_rates(jj,j) = 1. + 0.01 * clim_fcg_rates(jj,j)
             END IF
           END DO
         END DO
      ELSE
!        ! If the namelist is not to be read, set number of designated
!        !   years to zero for all possible forcings, as this may be
!        !   used to test if this system is being used for each forcing.
         DO J = 1, nclmfcgs
           clim_fcg_nyears(J) = 0
         END DO
      END IF !  L_clmchfcg

      CALL coradoca_defaults

! Read the JULES namelists
      REWIND( 5 )

! Unit number is passed as argument
      CALL read_jules_switches( 5 )

      CALL read_urban_switches( 5 )

      CALL read_jules_snow_param( 5 )

      CALL read_jules_soil_param( 5 )

      CALL read_jules_nvegparm( 5 )

      CALL read_jules_pftparm( 5 )

      CALL read_jules_triffid( 5 )

      CALL read_jules_surf_param( 5 )

      CALL read_jules_elevate( 5 )

      CALL read_jules_rad_param( 5 )

      CALL read_jules_csmin( 5 )

      CALL read_jules_seed( 5 )

      CALL read_jules_sigm( 5 )

      CALL read_urban2t_param( 5 )

! In the coupled model case, set alpham/c to ssalpham/c and dtice to ssdtice 
! if l_ssice_albedo == T. Note, ssalpham/c, ssdtice accessed via rad_input_mod
! Do this after JULES namelist reads, as l_ssice_albedo is in JULES_SWITCHES
      IF (l_oasis .AND. l_ssice_albedo) THEN
        alpham = ssalpham
        alphac = ssalphac
        dtice  = ssdtice
      END IF

! ROSE changes for BL_option_mod - logic control moved to after JULES namelist
! reads as l_flake_model now in jules_switches
      IF (l_flake_model) THEN
        cor_mo_iter = Limit_ObukhovL
        CMessage =&
       ': WARNING, cor_mo_iter set to Limit_ObukhovL since l_flake_model on'
        IF(PrintStatus >= PrStatus_Normal) THEN
          ErrorStatus = -100
          CALL ereport(RoutineName, ErrorStatus, CMessage)
        END IF
      END IF

      IF (atmos_sr(3) == '1A') THEN
        ishear_bl = bl_option_off
        CMessage =&
       ': WARNING, ishear_bl set to off since boundary layer version 1A chosen'
        IF(PrintStatus >= PrStatus_Normal) THEN
          ErrorStatus = -100
          CALL ereport(RoutineName, ErrorStatus, CMessage)
        END IF
      END IF

      k = 0
      DO i = 1, alpha_cd_batches
        DO j = 1, alpha_cd_items(i)
          k = k + 1
          alpha_Cd(k) = alpha_cd_vals(i)
        END DO
      END DO
! ROSE changes for BL end

      REWIND (UNIT=5)

      ! Read IAU namelist:
      READ (UNIT=5, NML=IAU_nl, IOSTAT=ErrorStatus)
      IF (ErrorStatus > 0) THEN
        CMessage(1:)   = 'Error reading IAU namelist IAU_nl.'
        CMessage(81:)  = 'Note that the namelist was heavily revised at vn7.6.'
        CMessage(161:) = 'See the UMUI help panel, or section A.2 in '// &
                         'UMDP 31 for details.'
        CALL EReport (RoutineName, ErrorStatus, CMessage)
      END IF

      ! Turn off IAU if in FASTRUN mode:
      IF (run_assim_mode == "FastRun   ") THEN
        IF (L_IAU) THEN
          run_assim_mode = "NoIAU     "
          L_IAU = .FALSE.
        ELSE
          run_assim_mode = "None      "
        END IF
      END IF

      REWIND (UNIT=5)

! Check error condition
      IF(ErrorStatus >  0) THEN

         CALL Ereport(RoutineName,ErrorStatus,Cmessage)
      END IF

      IF (lhook) CALL dr_hook('READLSTA',zhook_out,zhook_handle)
      RETURN
      END SUBROUTINE Readlsta
