! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!+ Perform a 1-timestep integration of the Atmosphere Model
!
! Subroutine Interface:
!
! Code Owner: See Unified Model Code Owners HTML page
! This file belongs in section: Top Level

      SUBROUTINE Atm_Step(                                               &
#include "argd1.h"
#include "argduma.h"
#include "arglndm.h"
#include "arg_atm_fields.h"
#include "argbnd.h"
#include "argsts.h"
! Constant arrays needed for Atmosphere-routing coupling
#include "argatcpl.h"
     & G_P_FIELD,                                                       &
     & G_R_FIELD,                                                       &
     & OBS_FLAG,OBS,obs_flag_len,obs_len,                               &
     & ngrgas,grgas_addr)

      USE Submodel_Mod
      USE atm_fields_bounds_mod
      USE rad_input_mod
      USE switches, ONLY: l_ctile
      USE cv_cntl_mod, ONLY: lcv_3d_ccw
      USE mphys_constants_mod, ONLY: ntot_land, ntot_sea
      USE run_aerosol_mod, ONLY: L_tracer1_non_hydro
      Use level_heights_mod
      Use trignometric_mod
      Use dyn_coriolis_mod
      Use dyn_var_res_mod
      Use diff_coeff_mod
      USE turb_diff_mod
      Use rad_mask_trop_mod
      Use rot_coeff_mod
      USE swapable_field_mod, ONLY : swapable_field_pointer_type
      USE timestep_mod
      USE Atm_Step_local
! Logicals and stash diagnostic types needed for SKEB2
      USE stochastic_physics_run_mod,  ONLY:                             &
          l_skeb2, l_rp2, rhcrit_max, rhcrit_min, m_ci,                  &
          m_ci_max, m_ci_min, par_mezcla_max, par_mezcla_min,            &
          g0_rp, g0_rp_max, g0_rp_min, par_mezcla, charnock_max,         &
     charnock_min, lambda_min_rp, lambda_min_rp_max, lambda_min_rp_min,  &
          ricrit_rp, ricrit_rp_max, ricrit_rp_min,                       &
          a_ent_1_rp, a_ent_1_rp_max, a_ent_1_rp_min, g1_rp, g1_rp_max,  &
     g1_rp_min, gwd_frc_max, gwd_frc_min, kay_gwave_max, kay_gwave_min
      USE ancil_info, ONLY: nsmax
      USE arcl_mod, ONLY: npd_arcl_species, npd_arcl_compnts
      USE IAU_mod, ONLY : L_IAU, IAU_FirstCallTS, IAU_LastCallTS

      USE earth_constants_mod, ONLY: g
      USE yomhook, ONLY: lhook, dr_hook
      USE parkind1, ONLY: jprb, jpim
      USE ukca_radaer_struct_mod
      USE ukca_cdnc_mod, ONLY: ukca_cdnc_struct, ukca_cdnc,             &
                               ukca_cdnc_get,                           &
                               cdnc_dim1, cdnc_dim2, cdnc_dim3
      USE ukca_option_mod, ONLY: L_ukca_chem, L_ukca_useumuivals,       &
                           L_ukca_set_trace_gases, L_ukca_strat,        &
                           L_ukca_strattrop, L_ukca_prescribech4,       &
                           L_ukca_trop, L_ukca_tropisop, L_ukca_raq,    &
                           l_ukca, l_ukca_aie1, l_ukca_aie2,            &
                           l_ukca_radaer
      USE g_wave_input_mod
      USE bl_option_mod
      
      USE nstypes, ONLY: ntype
      USE rimtypes
      USE um_types
      USE ereport_mod, ONLY : ereport
      USE PrintStatus_mod
      USE UM_ParVars
      USE Control_Max_Sizes
      USE Atmos_Max_Sizes, ONLY : model_levels_max
      USE lbc_mod
      USE o3intp_mod, ONLY: io3_3dspec, io3_2dspec, io3_2dmasscon,      &
                            io3_trop_map, io3_trop_map_masscon
      USE problem_mod, ONLY: standard, monsoon, dynamical_core,         &
                             idealised_problem, standard_namelist

      USE eng_mass_diag_mod
      USE diagnostics_adv_mod
      ! Stochastic Physics 
      USE stph_rp2_mod,   ONLY: stph_rp2
      USE stph_setup_mod, ONLY: stph_setup
      USE stph_skeb2_mod, ONLY: stph_skeb2
      
      USE dynamics_input_mod, ONLY:                                     &
          L_polar_reset,                                                &
          polar_reset_timesteps,                                        &
          L_mix_ratio,                                                  &
          NumCycles,L_new_tdisc, L_GCR_cycle_opt,L_fint_theta,          &
          L_thmono_fixed,L_LBC_balance,L_lbc_new,L_transparent,         &
          L_regular,L_qwaterload,n_rims_to_do, alpha_1, alpha_2,        &
          alpha_3,alpha_4,extrp_weight,GCR_use_tol_abs,                 &
          GCR_zero_init_guess,GCR_use_residual_Tol,                     &
          GCR_adi_add_full_soln,L_gcr_fast_x,L_interp_depart,           &
          GCR_max_iterations,GCR_diagnostics,GCR_precon_option,         &
          GCR_n_ADI_pseudo_timesteps,GCR_Restart_value,GCR_tol_res,     &
          GCR_tol_res2,GCR_tol_abs,GCR_ADI_pseudo_timestep,G_term_tol,  &
          GCR_its_avg_step,gcr_tol_abs2,L_lbc_old


      USE dynamics_testing_mod, ONLY:                                   &
          L_Physics,L_Run_With_Physics2,                                &
          L_Backwards, L_dry, L_adjust_wet, L_idealised_data,           &
          L_free_slip,  L_trap_uv,  L_trap_w, L_trap_theta,             &
          trap_option,Cw_test_lev,max_thinc,Cw_max,uv_limit,            &
          lambda_p_end,phi_p_end,lambda_u_end,phi_v_end,                &
          dlambda_p_end,dphi_p_end,dlambda_u_end,dphi_v_end
          
      USE gcr_input_mod, ONLY:                                          &
          GCR_its_switch , GCR_max_its,GCR_min_its,GCR_max_time,        &
          GCR_min_time,GCR_sum_its 
      

      USE sl_input_mod, ONLY:                                           &
          L_conserve_tracers,thmono_levels,halo_lam,halo_phi,           &
          look_lam,look_phi,recip_dlam,recip_dphi,                       &
          L_conserv,L_mono,L_high,L_Ritchie_high,L_Ritchie_mono,        &
          L_2d_sl_geometry,L_sl_halo_reprod,L_2dcomm,                   &
          L_moist_nonhydro_conserve,high_order_scheme,                  &
          monotone_scheme,Ritchie_high_order_scheme,                    &
          Ritchie_monotone_scheme,Depart_scheme,Depart_order,           &
          interp_vertical_search_tol,Instability_diagnostics,           &
          Theta_SL,moist_SL,Wind_SL,rho_SL

      ! Aerosols 
      USE aero_ctl_mod,             ONLY: aero_ctl      
      USE get_sulpc_oxidants_mod,   ONLY: get_sulpc_oxidants
      USE set_arcl_clim_mod,        ONLY: set_arcl_clim
      USE set_arcl_dimensions_mod,  ONLY: set_arcl_dimensions
      USE write_sulpc_oxidants_mod, ONLY: write_sulpc_oxidants

      USE lbc_read_data_mod, ONLY: rimweightsa

      USE dust_parameters_mod, ONLY:                                       &
           l_dust,            l_dust_div1,      l_dust_div2,               &
           l_dust_div3,       l_dust_div4,      l_dust_div5,               &
           l_dust_div6     
      USE um_input_control_mod,  ONLY:                                     &
           model_domain,                        problem_number,            &
           l_int_uvw_lbc,                       l_dust_div1_lbc,           &
                              l_dust_div2_lbc,                             &
           l_dust_div3_lbc,                     l_dust_div4_lbc,           &
                              l_dust_div5_lbc,                             &
           l_dust_div6_lbc,   l_so2,            l_so2_lbc,                 &
           l_dms,             l_dms_lbc,        l_so4_aitken,              &
           l_so4_aitken_lbc,  l_so4_accu,       l_so4_accu_lbc,            &
           l_so4_diss,        l_so4_diss_lbc,   l_nh3,                     &
           l_nh3_lbc,         l_soot_new,       l_soot_new_lbc,            &
           l_soot_agd,        l_soot_agd_lbc,   l_soot_cld,                &
           l_soot_cld_lbc,    l_bmass_new,      l_bmass_new_lbc,           &
           l_bmass_agd,       l_bmass_agd_lbc,  l_bmass_cld,               &
           l_bmass_cld_lbc,   l_ocff_new,       l_ocff_new_lbc,            &
           l_ocff_agd,        l_ocff_agd_lbc,   l_ocff_cld,                &
           l_ocff_cld_lbc,    l_nitr_acc,       l_nitr_acc_lbc,            &
           l_nitr_diss,       l_nitr_diss_lbc,  l_sulpc_so2,               &
           l_sulpc_dms,       l_sulpc_ozone,    l_sulpc_so2_o3_nonbuffered,&
           l_so2_surfem,      l_so2_hilem,      l_so2_natem,               &
           l_dms_em,          l_dms_em_inter,   l_sulpc_2_way_coupling,    &
           l_dms_ointer,      l_dms_wanninkhof, l_sulpc_online_oxidants,   &
           l_dms_nightingale, l_sulpc_nh3,      l_dms_liss_merlivat,       &
           l_nh3_em,          l_soot,           l_soot_surem,              &
           l_soot_hilem,      l_biomass,        l_bmass_surem,             &
           l_bmass_hilem,     l_co2_interactive,l_use_seasalt_autoconv,    &
           l_ocff,            l_ocff_surem,     l_use_seasalt_sulpc,       &
           l_ocff_hilem,      l_use_ocff_sulpc, l_use_sulphate_sulpc,      &
           l_nitrate,         l_use_seasalt_pm, l_use_nitrate_sulpc,       &
           call_chem_freq,    l_use_soot_sulpc, l_use_bmass_sulpc,         &
           l_mr_physics1,     l_mr_physics2,                               &
           super_array_size,                    moisture_array_size,       &
           lcal360

      USE mphys_inputs_mod, ONLY:                                          &
           l_mcr_qcf2,        l_mcr_qcf2_lbc,   l_mcr_qgraup,              &
           l_mcr_qgraup_lbc,  l_mcr_qrain_lbc,  l_mcr_qrain 

      USE cloud_inputs_mod, ONLY:                                          &
           l_cld_area,        l_acf_cusack,     l_pc2,                     &
           l_pc2_reset,       l_pc2_lbc,        l_acf_brooks,              &
           rhcrit

      USE river_inputs_mod, ONLY: river_vel, river_mcoef, i_river_vn

      USE eng_corr_inputs_mod, ONLY:                                       &
           l_emcorr,          lmass_corr,       lemq_print,                &
           a_energysteps,     lqt_corr,         lenergy
      
      USE nudging_input_mod, ONLY: l_nudging

      USE murk_inputs_mod, ONLY: l_murk, l_murk_advect, l_murk_lbc

      USE cosp_input_mod, ONLY: l_cosp

      USE nlstcall_mod, ONLY : ldump, &
                               ltimer
      IMPLICIT NONE
!
! Description: Perform a 1-timestep integration of the Atmosphere Model,
!   including assimilation, physics and dynamics processing.
!
! Method: Sequential execution of code sections below, with control and
!  logical switches determining whether each section is accessed.
!
!!ND ND code section and UM added, tidy and re-number required.
! Section 0.  Initialisation.
! Section 0.1  Filter near polar winds and theta
! Section 1.0  Call Atmospheric Physics1
!     STASH diagnostics for Physics1.
! Section 2. Calculate new time-level estimates to Theta, q, qcl, qcf
! Section 2.1 Calculate advective Momentum increments.
! Section 2.2 Calculate Semi-Lagrangian part of continuity equation.
! Section 2.3 Diagnostics at end of advection.
!     STASH diagnostics for Dynamics advection.
! Section 3.0  Call Atmospheric Physics2
!     STASH diagnostics for Physics2.
! Section 4.  Form and solve Helmholtz equation and update variables.
! Section 4.1 Form and solve Helmholtz equation, return all corrections
!     Set lateral boundaries.
! Section 5.0 Calculate rho at new time level, using flux form.
! Section 6.0 Calculate u, v, w, theta, q, qcl, qcf at new time level.
! Section 7.0 Mean all polar variables on a level to remove deviation.
! Section 8.0 Update pressure to value at new-time level.
!     VAR or AC assimilation.
! Section 9.0 Diagnostics at end of timestep
!     STASH diagnostics - dynamics based (section 15).
!     STASH diagnostics - physics  based (section 16).
!     STASH diagnostics - climate end of timestep (section 30).
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
#include "typd1.h"
#include "typduma.h"
#include "typcona.h"
#include "typlndm.h"
#include "ctracera.h"
#include "typ_atm_fields.h"
#include "typbnd.h"
#include "typsts.h"
! River routing
#include "typatcpl.h"
      INTEGER                                                            &
     &  G_P_FIELD                                                        &
                              ! IN : global horiz domain for atmos
     &, G_R_FIELD             ! IN : global horiz domain for rivers
      INTEGER :: obs_flag_len,obs_len
      INTEGER :: OBS_FLAG(obs_flag_len)
      REAL    :: OBS(obs_len)
!
#include "ccontrol.h"
#include "cruntimc.h"
#include "surface.h"
#include "vgrid.h"
#include "ctime.h"
#include "c_global.h"
#include "c_writd.h"

! Subroutine arguments:
      
! 3-D fields of species to be passed down to radiation       
      INTEGER, INTENT(IN) :: ngrgas 
      INTEGER, INTENT(IN) :: grgas_addr(ngrgas)

! Local parameters:
      CHARACTER(LEN=*) RoutineName
      PARAMETER (   RoutineName='Atm_Step')

! Local Arrays
      REAL, TARGET ::                                                    &
     &  theta_star(tdims_s%i_start:tdims_s%i_end,                        &
     &             tdims_s%j_start:tdims_s%j_end,                        &
     &             tdims_s%k_start:tdims_s%k_end)                        &
     &,     q_star(qdims_s%i_start:qdims_s%i_end,                        &
     &             qdims_s%j_start:qdims_s%j_end,                        &
     &             qdims_s%k_start:qdims_s%k_end)                        &
     &,   qcl_star(qdims_s%i_start:qdims_s%i_end,                        &
     &             qdims_s%j_start:qdims_s%j_end,                        &
     &             qdims_s%k_start:qdims_s%k_end)                        &
     &,   qcf_star(qdims_s%i_start:qdims_s%i_end,                        &
     &             qdims_s%j_start:qdims_s%j_end,                        &
                   qdims_s%k_start:qdims_s%k_end)

      REAL                                                               &
     &    cf_star(qdims_s%i_start:qdims_s%i_end,                         &
     &            qdims_s%j_start:qdims_s%j_end,                         &
     &            qdims_s%k_start:qdims_s%k_end)                         &
     &,  cfl_star(qdims_s%i_start:qdims_s%i_end,                         &
     &            qdims_s%j_start:qdims_s%j_end,                         &
     &            qdims_s%k_start:qdims_s%k_end)                         &
     &,  cff_star(qdims_s%i_start:qdims_s%i_end,                         &
     &            qdims_s%j_start:qdims_s%j_end,                         &
     &            qdims_s%k_start:qdims_s%k_end)                         &
     &,exner_star(edims_s%i_start:edims_s%i_end,                         &
     &            edims_s%j_start:edims_s%j_end,                         &
     &            edims_s%k_start:edims_s%k_end)                         &
     &, frac_control(land_field,ntype)   !Forcing for land surface (3C)


      Real, TARGET ::                                                    &
     &  R_u(udims_s%i_start:udims_s%i_end,                               &
     &      udims_s%j_start:udims_s%j_end,                               &
     &      udims_s%k_start:udims_s%k_end)                               &
     &, R_v(vdims_s%i_start:vdims_s%i_end,                               &
     &      vdims_s%j_start:vdims_s%j_end,                               &
     &      vdims_s%k_start:vdims_s%k_end)                               &
     &, R_w(wdims  %i_start:wdims  %i_end,                               &
     &      wdims  %j_start:wdims  %j_end,                               &
     &      wdims  %k_start:wdims  %k_end)

      Real                                                               &
     &  rho_n (tdims_s%i_start:tdims_s%i_end,                            &
     &         tdims_s%j_start:tdims_s%j_end,                            &
     &         tdims_s%k_start:tdims_s%k_end)

      Real                                                               &
     &   biogenic(row_length, rows, model_levels)

! Local arrays for using the aerosol climatology for NWP
      
      ! Internal model switches
      Logical L_USE_ARCL(NPD_ARCL_SPECIES)

      ! Array index of each component
      Integer i_arcl_compnts(NPD_ARCL_COMPNTS)
      
      Integer cloud_tol

      Real                                                              &
     &  mag_vector_np (model_levels)                                    &
     &, dir_vector_np (model_levels)                                    &
     &, mag_vector_sp (model_levels)                                    &
     &, dir_vector_sp (model_levels)                                    &
     &, lambda_a (row_length) ! delta_lambda term for polar wind
   
! LAM LBC tendency
      REAL                                                               &
     &  U_LBC_REAL_TEND(LENRIMA(fld_type_u,halo_type_extended,           &
     &                    rima_type_norm),MODEL_LEVELS)                  &
     &, V_LBC_REAL_TEND(LENRIMA(fld_type_v,halo_type_extended,           &
     &                    rima_type_norm),MODEL_LEVELS)                  &
     &, W_LBC_REAL_TEND(LENRIMA(fld_type_p,halo_type_extended,           &
     &                    rima_type_norm),wdims_s%k_start:wdims_s%k_end) &
     &, EXNER_LBC_REAL_TEND(LENRIMA(fld_type_p,halo_type_extended,       &
     &                    rima_type_norm),MODEL_LEVELS+1)

! Physics arrays needed by dynamics
      Real                                                               &
     &  rho_km(rkmdims%i_start:rkmdims%i_end,                            &
     &         rkmdims%j_start:rkmdims%j_end,                            &
               rkmdims%k_start:rkmdims%k_end)                            &
     &,     cH(chdims %i_start:chdims %i_end,                            &
     &         chdims %j_start:chdims %j_end,                            &
     &         chdims %k_start:chdims %k_end)                            &
     &, wet_to_dry_n  (tdims_s%i_start:tdims_s%i_end,                    &
     &                 tdims_s%j_start:tdims_s%j_end,                    &
     &                 tdims_s%k_start:tdims_s%k_end)                    &
     &, wet_to_dry_np1(tdims_s%i_start:tdims_s%i_end,                    &
     &                 tdims_s%j_start:tdims_s%j_end,                    &
     &                 tdims_s%k_start:tdims_s%k_end)

! arrays holding information to be passed between physics
! routines.

      Real                                                               &
     &  ls_rain    (row_length, rows)                                    &
     &, ls_snow    (row_length, rows)                                    &
     &, micro_tends(row_length, rows, 2, bl_levels)
!                          ! Tendencies from microphys within BL levels
!                          ! (TL, K/s; QW, kg/kg/s)

! Radiation fields 1. SW & common with LW.
      Real                                                               &
     &  photosynth_act_rad(row_length, rows)                             &
                                             ! Net downward
!                                 shortwave radiation in band 1 (w/m2).
     &, rad_hr(row_length, rows, 2, bl_levels)                           &
!                                 BL (LW,SW) rad heating rates
     &, dolr(row_length,rows)                                            &
!       local field "dolr" is distinguished from "dolr_field" (in atm_fields_mod)
                                   ! TOA - surface upward LW
     &, SW_tile(land_field,ntiles)                                       &
                                   ! Surface net SW on land tiles
     &, cos_zenith_angle(row_length, rows)


! MPP-related Arrays
      INTEGER                                                            &
     & g_row_length(0:nproc-1)                                           &
                               ! Table of number of points on a row
     &,g_rows(0:nproc-1)                                                 &
                               ! Table number of rows in theta field
     &,g_i_pe(1-halo_i:global_row_length+halo_i)       &
                ! processor on my
!               processor-row holding a given value in i direction
     &,g_j_pe(1-halo_j:global_rows+halo_j) ! processor on my
!               processor-row holding a given value in j direction
      
      INTEGER :: size_2dcomm   ! no of procs involved in comms on demand
      INTEGER :: group_2dcomm  ! group ID of procs involved in comms on demand
      INTEGER :: max_comm_size ! error check size for comms on demand

      INTEGER ErrorStatus      ! Return code : 0 Normal Exit : >0 Error
      CHARACTER(LEN=256) CMessage   ! Error message if return code >0

      REAL STASHwork0_dummy(1) ! STASHwork not defined for section 0,
                               !  but required as dummy argument.

! Local arrays for phys1 and phys2 increments for tracers:
      Real :: super_tracer_phys1(tdims_l%i_start:tdims_l%i_end,          &
     &                           tdims_l%j_start:tdims_l%j_end,          &
     &                           tdims_l%k_start:tdims_l%k_end,          &
     &                           super_array_size)

      Real :: super_tracer_phys2(tdims%i_start:tdims%i_end,              &
     &                           tdims%j_start:tdims%j_end,              &
     &                           tdims%k_start:tdims%k_end,              &
     &                           super_array_size)

      Real :: tracer_phys1(trdims_ltl%i_start:trdims_ltl%i_end,          &
     &                     trdims_ltl%j_start:trdims_ltl%j_end,          &
     &                     trdims_ltl%k_start:trdims_ltl%k_end, tr_vars)

      Real :: tracer_phys2(trdims_xstl%i_start:trdims_xstl%i_end,        &
     &                     trdims_xstl%j_start:trdims_xstl%j_end,        &
     &                     trdims_xstl%k_start:trdims_xstl%k_end,        &
     &                     tr_vars)

      Real :: ukca_tracer_phys1(trdims_ltl%i_start:trdims_ltl%i_end,     &
     &                          trdims_ltl%j_start:trdims_ltl%j_end,     &
     &                          trdims_ltl%k_start:trdims_ltl%k_end,     &
     &                          tr_ukca)

      Real :: ukca_tracer_phys2(trdims_xstl%i_start:trdims_xstl%i_end,   &
     &                          trdims_xstl%j_start:trdims_xstl%j_end,   &
     &                          trdims_xstl%k_start:trdims_xstl%k_end,   &
     &                          tr_ukca)

      Real :: unscaled_dry_rho(tdims_s%i_start:tdims_s%i_end,            &
     &                         tdims_s%j_start:tdims_s%j_end,            &
     &                         tdims_s%k_start:tdims_s%k_end)

      REAL GS1(land_field)

! local variable
      REAL                                                               &
     &  tot_dry_mass_final                                               &
                            ! mass at end of energy correction period
     &, tot_energy_final                                                 &
                            ! energy at end of energy correction period
     &, tot_moist_final                                                  &
                            ! moist at end of energy correction period
     &, energy_corr_now                                                 
                            ! instanteous energy correction

! Monsoon variables
      Real                                                               &
     &  lambda_half_width                                                &
     &, phi_half_width                                                   &
     &, p_max                                                            &
     &, p_top                                                            &
     &, p_bottom                                                         &
     &, p_arbitrary                                                      &
     &, lambda_heat_centre                                               &
     &, phi_heat_centre                                                  &
     &, max_heat_per_day                                                 &
     &, Mons_newtonian_timescale

!  Suarez-Held variables now declared in CRUNTIMC

      INTEGER    :: minutos ! LOCAL Store value of timestep in min.
!
! Structure for UKCA/radiation interaction.
!
      TYPE (ukca_radaer_struct), SAVE :: ukca_radaer

      REAL                                                               &
     &  flux_e(row_length, rows)                                         &
                                 ! Surface latent heat flux (W/m^2)
     &, flux_h(row_length, rows)                                         &
                                 ! Surface sensible heat flux (W/m^2)
     &, z0m_scm(row_length, rows)                                        &
                                 ! SCM specified z0m (m)
     &, z0h_scm(row_length, rows)! SCM specified z0m (m)
      REAL, DIMENSION(:,:,:), ALLOCATABLE ::                             &
     &  q_inc_subs, th_inc_subs                                          &
                                  ! subsidence increments
     &, q_inc_ls, th_inc_ls                                              &
                                  ! large scale increments
     &, u_inc_dmp, q_inc_dmp, th_inc_dmp                                 &
                                  !Damping incs
     &, v_inc_dmp
! Tolerance for CycleNo >1
      REAL                                                               &
     &  GCR_run_tol_abs                                                  &
     &, GCR_run_tol_res

      TYPE(swapable_field_pointer_type) :: fields_to_swap(7)  ! multivariate
                                                              ! swapbounds

      Integer exppxi               ! Function to extract ppxref info

! Local parameters for mixing ratio physics
! Mixing ratios for atmos_physics1 and 2 are defined through namelist
      LOGICAL, PARAMETER :: l_mr_qtbalcld=.false. ! Use mr's for qt_bal_cld
      LOGICAL, PARAMETER :: l_mr_iau     =.false. ! Use mr's for tfilt_ctl
      LOGICAL, PARAMETER :: l_mr_pc2     =.false. ! Use mr's for PC2 routines

      INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
      INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
      REAL(KIND=jprb)               :: zhook_handle

! Input variables to be used by COSP (row_length,rows,model_levels)
!  Convective rainfall
      REAL,POINTER,SAVE :: cosp_crain_3d(:,:,:)
!  Convective snowfall
      REAL,POINTER,SAVE :: cosp_csnow_3d(:,:,:)

!- End of header
     
! ----------------------------------------------------------------------
! Section 0.  Initialisation.
! ----------------------------------------------------------------------

      IF (lhook) CALL dr_hook('ATM_STEP',zhook_in,zhook_handle)
! DEPENDS ON: timer
      If (Ltimer) Call timer('Atm_Step (AS)',5)

ErrorStatus = 0

! DEPENDS ON: Atm_Step_Init
CALL Atm_Step_Init (                 &
#include "argduma.h"
     lambda_a, g_row_length, g_rows, g_i_pe, g_j_pe, flux_e, flux_h, &
     z0m_scm, z0h_scm, errorstatus )

     l_2dcomm = .false.
     If (l_2dcomm) Then
       size_2dcomm   = nproc - 1
       group_2dcomm  = gc_all_proc_group
       max_comm_size = model_levels * row_length * rows
     Else
       size_2dcomm   = nproc_x - 1
       group_2dcomm  = gc_proc_row_group
       max_comm_size = model_levels * global_row_length
     End If

! ---------------------------------------------------------------------
! Section 0.1  Initialisation for idealised test problems
!              For standard runs go to end section 0.1
! ---------------------------------------------------------------------

      h_print=0.0

      if(L_initialise_data)then

! DEPENDS ON: idl_ni_init
        Call IDL_NI_Init(                                                &
     & model_domain, row_length, rows, n_rows, model_levels,wet_levels   &
     &,TR_VARS,TR_UKCA,TR_LEVELS,bl_levels,first_constant_r_rho_level    &
     &,cos_theta_latitude, sec_theta_latitude, f3_at_u, f3_at_v          &
     &,timestep, first_atmstep_call, L_regular                           &
     &,delta_x, delta_y, delta_lambda, delta_phi, base_phi, base_lambda  &
     &,A_REALHD(rh_rotlat), A_REALHD(rh_rotlong)                         &
     &, glambda_p(lambda_start), phi_p, glambda_u(lambda_start)          &
     &, phi_v, lambda_p_end, phi_p_end                                   &
     &,r_theta_levels, r_rho_levels, r_at_u, r_at_v, z_orog_print        &
     &,eta_theta_levels, eta_rho_levels                                  &
! Multi-processor
     &,offx,offy,halo_i,halo_j, mype, nproc, at_extremity, datastart     &
     &,gc_all_proc_group, global_row_length, global_rows                 &
     &,g_rows, g_row_length, nproc_x                                     &
! Primary fields
     &,THETA,RHO, EXNER_THETA_LEVELS                                     &
     &,EXNER_RHO_LEVELS,P                                                &
     &,P_THETA_LEVELS, PSTAR                                             &
     &,Q, QCL, QCF,QCF2,QRAIN                                            &
     &,QGRAUP,CF_BULK, CF_LIQUID                                         &
     &,CF_FROZEN,U, V, W                                                 &
     &,U_ADV, V_ADV, W_ADV                                               &
! Lateral boundaries
     &,RIMWIDTHA(rima_type_norm), RIMWEIGHTSA                            &
     &,LENRIMA(1,1,rima_type_norm), LBC_SIZEA(1,1,1,rima_type_norm)      &
     &,LBC_STARTA(1,1,1,rima_type_norm)                                  &
     &,THETA_LBC, THETA_LBC_TEND, EXNER_LBC                              &
     &,EXNER_LBC_TEND, RHO_LBC, RHO_LBC_TEND                             &
     &,Q_LBC, Q_LBC_TEND, QCL_LBC, QCL_LBC_TEND                          &
     &,QCF_LBC, QCF_LBC_TEND, QCF2_LBC                                   &
     &,QCF2_LBC_TEND, QRAIN_LBC, QRAIN_LBC_TEND                          &
     &,QGRAUP_LBC, QGRAUP_LBC_TEND, CF_BULK_LBC                          &
     &,CF_BULK_LBC_TEND, CF_LIQUID_LBC                                   &
     &,CF_LIQUID_LBC_TEND, CF_FROZEN_LBC                                 &
     &,CF_FROZEN_LBC_TEND, U_LBC, U_LBC_TEND                             &
     &,V_LBC, V_LBC_TEND, W_LBC, W_LBC_TEND                              &
     &,U_ADV_LBC, U_ADV_LBC_TEND, V_ADV_LBC                              &
     &,V_ADV_LBC_TEND, W_ADV_LBC, W_ADV_LBC_TEND                         &
! Grid info for idealised
     &,A_REALHD(rh_z_top_theta), height_domain, big_layers               &
     &,transit_layers, mod_layers, surface_type, p_surface               &
! Profile settings
     &,tprofile_number, qprofile_number, uvprofile_number, Brunt_Vaisala &
     &,theta_surface, dtheta_dz1, height_dz1, u_in, v_in, height_u_in    &
     &,ujet_lat, ujet_width, u_ramp_start, u_ramp_end, f_plane, r_plane  &
     &,q1, num_profile_data, zprofile_data, tprofile_data, qprofile_data &
     &,                  num_uvprofile_data, z_uvprofile_data            &
     &,                  uprofile_data, vprofile_data                    &
     &,model_levels_max, max_num_profile_data, max_num_force_times       &
     &,tforce_option, qforce_option, uvforce_option, num_tforce_levels   &
     &,num_tforce_times, num_qforce_levels, num_qforce_times             &
     &,num_uvforce_levels, num_uvforce_times, z_tforce_data, tforce_data &
     &,z_qforce_data, qforce_data, z_uvforce_data, uforce_data           &
     &,vforce_data, tforce_data_modlev, qforce_data_modlev               &
     &,uforce_data_modlev, vforce_data_modlev                            &
! Dynamical core settings
     &,SuHe_pole_equ_deltaT, SuHe_static_stab                            &
     &,base_frictional_timescale, frictional_timescale                   &
     &,SuHe_sigma_cutoff, SuHe_level_weight, L_SH_Williamson             &
!  Horizontal function parameters
     &,                  t_horizfn_number, uv_horizfn_number             &
     &, t_horizfn_data, L_perturb_t, perturb_magnitude_t                 &
     &, L_perturb_q, perturb_magnitude_q, L_perturb_correlate_tq         &
     &, L_perturb_correlate_vert, L_perturb_correlate_time               &
     &, perturb_type, perturb_height                                     &
!  Profiles for fixed lbcs and sponge zones
     &,                  u_ref, v_ref, theta_ref, exner_ref, rho_ref     &
     &,                  q_ref                                           &
     &,L_fix_orog_hgt_lbc, orog_hgt_lbc                                  &
     &,zprofile_orog, idl_interp_option, hf                              &
!  Options
     &,L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup, L_pc2                     &
     &,L_mcr_qcf2_lbc, L_mcr_qrain_lbc, L_mcr_qgraup_lbc, L_pc2_lbc     &
     &,L_constant_dz, L_rotating, L_fixed_lbcs, L_polar_wind_zero       &
     &,L_wind_balance, L_rotate_winds, L_pressure_balance, L_physics    &
     &,L_dry, L_sponge                                                  &
     &,                  L_trivial_trigs, L_perturb, L_code_test        &
     &,L_cyclone, L_baroclinic                                          &
     &,h_print, timestep_number, h_o_actual, grow_steps, h_o_per_step   &
     &,h_o, grid_number, grid_flat, first_theta_height                  &
     &,thin_theta_height, big_factor, mag, lambda_fraction, phi_fraction&
     &,half_width_x, half_width_y, plat_size_x, plat_size_y, Witch_power&
     &,idl_max_num_bubbles, idl_bubble_option, idl_bubble_max           &
     &,idl_bubble_height, idl_bubble_xoffset, idl_bubble_yoffset        &
     &,idl_bubble_width,  idl_bubble_depth, L_idl_bubble_saturate       &
     &,                  a_ixsts,len_a_ixsts, a_spsts,len_a_spsts       &
     &,                  nproc_y, gc_proc_row_group,n_cca_lev           &
     &,IdlSurfFluxSeaOption,IdlSurfFluxSeaParams,L_flux_bc,flux_h,flux_e&
     &,L_spec_z0, z0m_scm, z0h_scm, roughlen_z0m, roughlen_z0h          &
     &,i_hour, i_minute, i_second                                       &
     &,                  problem_number, rad_hr                         &
     &,OROGRAPHY, TSTAR_TILE, ntiles, land_field, land_index            &
     &,CUMULUS, NBDSC, NTDSC, CCA, CCB                                  &
     &,CCT, CCLWP, TSTAR, LAND, SW_INCS                                 &
     &,LW_INCS, T1_SD, Q1_SD, ZH, CF_AREA                               &
     &,TI, Z0, NTML, U_SEA, V_SEA, U_0_P, V_0_P )

        elseif(Problem_number /= standard .and. timestep_number == 1) Then
          IF (.not. L_Physics .and. mype  ==  0)then
           WRITE(6,'(A)') 'Data from dump being used without initialising. '
           WRITE(6,'(A)') 'If source of dump is a full model run with orography'
           WRITE(6,'(A)') 'then there is no guarantee that run will work'
           WRITE(6,'(A)') 'since physics is OFF'
          END IF !.not. L_Physics.and. mype  ==  0

        end if   ! L_initialise_data
! ---------------------------------------------------------------------
! Section 0.1  End of Initialisation for idealised test problems
! ---------------------------------------------------------------------

!----------------------------------------------------------------------
! Section 0.2  Update domain halos for time-dependent fields
! ---------------------------------------------------------------------

! Update domain halos for time-dependent fields
! DEPENDS ON: set_halos
      call set_halos(U, V, W,                                           &
     &               U_ADV, V_ADV, W_ADV,                               &
     &               THETA, Q, QCL, QCF,                                &
     &               QCF2, QRAIN, QGRAUP,                               &
     &               CF_BULK, CF_LIQUID,                                &
     &               CF_FROZEN,                                         &
     &               RHO, P, P_THETA_LEVELS,                            &
     &               EXNER_RHO_LEVELS,                                  &
     &               EXNER_THETA_LEVELS,                                &
     &               MURK,                                              &
     &               DUST_DIV1,DUST_DIV2,                               &
     &               DUST_DIV3,DUST_DIV4,                               &
     &               DUST_DIV5,DUST_DIV6,                               &
     &               SO2, SO4_AITKEN,                                   &
     &               SO4_ACCU, SO4_DISS, DMS,                           &
     &               NH3, SOOT_NEW, SOOT_AGD,                           &
     &               SOOT_CLD, BMASS_NEW,                               &
     &               BMASS_AGD, BMASS_CLD,                              &
     &               OCFF_NEW, OCFF_AGD, OCFF_CLD,                      &
     &               NITR_ACC, NITR_DISS,                               &
     &               CO2, TRACER, tracer_ukca,                          &
     &               row_length, rows, n_rows, model_levels, wet_levels,&
     &               offx, offy, halo_i, halo_j, tr_levels, tr_vars,    &
     &               tr_ukca, L_MURK, L_DUST,                           &
     &               L_SULPC_SO2, L_SULPC_NH3, L_SULPC_DMS,             &
     &               l_soot, l_biomass, l_ocff, l_nitrate,              &
     &               l_co2_interactive,                                 &
     &               L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,             &
     &               OZONE_TRACER, L_USE_CARIOLLE)


! ---------------------------------------------------------------------
!   Section 0.3  Update lbcs for LAMs
! ---------------------------------------------------------------------
      If ((ErrorStatus == 0) .and. (model_domain == mt_lam)) Then
! DEPENDS ON: timer
      If (Ltimer) Call timer('AS LAM_LBCS',5)
      
       If ( L_lbc_new ) THEN
         L_update_lbcs = .false.
       Else If ( first_atmstep_call ) THEN
         L_update_lbcs = .false.
       Else If ( RIM_STEPSA == 0 ) THEN
         L_update_lbcs = .false.
       Else If ( L_Fixed_lbcs ) THEN
         L_update_lbcs = .false.
       Else !  Old lbcs and NOT first_atmstep_call 
         L_update_lbcs = .true.
       end if ! L_lbc_new

       If ( L_update_lbcs ) Then

        If (MOD (bndary_offsetim(atmos_im) + stepim(atmos_im),           &
     &           RIM_STEPSA) /= 1 ) Then

          If (RIM_STEPSA  /=  1) Then

! DEPENDS ON: BOUNDVAL
              Call BOUNDVAL(LENRIMA(1,1,rima_type_norm),                 &
     &                      L_mcr_qcf2_lbc, L_mcr_qrain_lbc,             &
     &                      L_mcr_qgraup_lbc, L_pc2_lbc,                 &
     &                      L_murk_lbc, L_int_uvw_lbc,                   &
     &                      L_dust_div1_lbc,L_dust_div2_lbc,             &
     &                      L_dust_div3_lbc,L_dust_div4_lbc,             &
     &                      L_dust_div5_lbc,L_dust_div6_lbc,             &
     &                      L_so2_lbc,L_dms_lbc,L_so4_aitken_lbc,        &
     &                      L_so4_accu_lbc,L_so4_diss_lbc,               &
     &                      L_nh3_lbc,L_soot_new_lbc,L_soot_agd_lbc,     &
     &                      L_soot_cld_lbc,L_bmass_new_lbc,              &
     &                      L_bmass_agd_lbc,L_bmass_cld_lbc,             &
     &                      L_ocff_new_lbc,                              &
     &                      L_ocff_agd_lbc,L_ocff_cld_lbc,               &
     &                      L_nitr_acc_lbc,L_nitr_diss_lbc,              &
     &                      U_LBC, U_LBC_TEND,                           &
     &                      V_LBC, V_LBC_TEND,                           &
     &                      W_LBC, W_LBC_TEND,                           &
     &                      RHO_LBC, RHO_LBC_TEND,                       &
     &                      THETA_LBC, THETA_LBC_TEND,                   &
     &                      Q_LBC, Q_LBC_TEND,                           &
     &                      QCL_LBC, QCL_LBC_TEND,                       &
     &                      QCF_LBC, QCF_LBC_TEND,                       &
     &                      QCF2_LBC, QCF2_LBC_TEND,                     &
     &                      QRAIN_LBC, QRAIN_LBC_TEND,                   &
     &                      QGRAUP_LBC, QGRAUP_LBC_TEND,                 &
     &                      CF_BULK_LBC, CF_BULK_LBC_TEND,               &
     &                      CF_LIQUID_LBC, CF_LIQUID_LBC_TEND,           &
     &                      CF_FROZEN_LBC, CF_FROZEN_LBC_TEND,           &
     &                      EXNER_LBC, EXNER_LBC_TEND,                   &
     &                      U_ADV_LBC, U_ADV_LBC_TEND,                   &
     &                      V_ADV_LBC, V_ADV_LBC_TEND,                   &
     &                      W_ADV_LBC, W_ADV_LBC_TEND,                   &
     &                      MURK_LBC, MURK_LBC_TEND,                     &
     &                      DUST_DIV1_LBC, DUST_DIV1_LBC_TEND,           &
     &                      DUST_DIV2_LBC, DUST_DIV2_LBC_TEND,           &
     &                      DUST_DIV3_LBC, DUST_DIV3_LBC_TEND,           &
     &                      DUST_DIV4_LBC, DUST_DIV4_LBC_TEND,           &
     &                      DUST_DIV5_LBC, DUST_DIV5_LBC_TEND,           &
     &                      DUST_DIV6_LBC, DUST_DIV6_LBC_TEND,           &
     &                      SO2_LBC, SO2_LBC_TEND,                       &
     &                      DMS_LBC, DMS_LBC_TEND,                       &
     &                      SO4_AITKEN_LBC, SO4_AITKEN_LBC_TEND,         &
     &                      SO4_ACCU_LBC, SO4_ACCU_LBC_TEND,             &
     &                      SO4_DISS_LBC, SO4_DISS_LBC_TEND,             &
     &                      NH3_LBC, NH3_LBC_TEND,                       &
     &                      SOOT_NEW_LBC, SOOT_NEW_LBC_TEND,             &
     &                      SOOT_AGD_LBC, SOOT_AGD_LBC_TEND,             &
     &                      SOOT_CLD_LBC, SOOT_CLD_LBC_TEND,             &
     &                      BMASS_NEW_LBC, BMASS_NEW_LBC_TEND,           &
     &                      BMASS_AGD_LBC, BMASS_AGD_LBC_TEND,           &
     &                      BMASS_CLD_LBC, BMASS_CLD_LBC_TEND,           &
     &                      OCFF_NEW_LBC, OCFF_NEW_LBC_TEND,             &
     &                      OCFF_AGD_LBC, OCFF_AGD_LBC_TEND,             &
     &                      OCFF_CLD_LBC, OCFF_CLD_LBC_TEND,             &
     &                      NITR_ACC_LBC, NITR_ACC_LBC_TEND,             &
     &                      NITR_DISS_LBC, NITR_DISS_LBC_TEND,           &
     &                      TRACER_LBC, TRACER_LBC_TEND,                 &
     &                      TRACER_UKCA_LBC, TRACER_UKCA_LBC_TEND,       &
     &                      0, 1, ErrorStatus, CMESSAGE)

           End If       ! RIM_STEPSA  /=  1
         End If ! MOD(bndary_offsetim+stepim,RIM_STEPSA) /= 1 )

        End If !  L_update_lbcs 


        !--------------------------------------------------------------
        ! Idealised UM LBC forcing
        !  If active, update lateral boundary arrays to contain
        !  idealised namelist profile data interpolated in time.
        !--------------------------------------------------------------
        If (L_initialise_data .and. L_force_lbc) Then

! DEPENDS ON: idl_force_lbc
          Call IDL_Force_LBC (                                          &
     &             row_length, rows, offx, offy                         &
     &,            halo_i, halo_j                                       &
     &,            LENRIMA(1,1,rima_type_norm)                          &
     &,            timestep, timestep_number                            &
     &,            model_levels, wet_levels                             &
     &,            model_levels_max, max_num_force_times                &
     &,            U_LBC, V_LBC                                         &
     &,            THETA_LBC,Q_LBC                                      &
     &,            U_ADV_LBC,V_ADV_LBC                                  &
     &,            EXNER_LBC                                            &
     &,            r_theta_levels, r_rho_levels                         &
     &,            eta_theta_levels, eta_rho_levels                     &
     &,            height_domain, theta_surface                         &
     &,            pforce_option                                        &
     &,            tforce_option, qforce_option, uvforce_option         &
     &,            num_pforce_times                                     &
     &,            num_tforce_times, num_qforce_times                   &
     &,            num_uvforce_times                                    &
     &,            pforce_time_interval                                 &
     &,            tforce_time_interval, qforce_time_interval           &
     &,            uvforce_time_interval                                &
     &,            p_surface_data                                       &
     &,            tforce_data_modlev, qforce_data_modlev               &
     &,            uforce_data_modlev, vforce_data_modlev               &
     &,            newtonian_timescale )

        End If ! on (L_initialise_data .and. L_force_lbc)

       If ( first_atmstep_call ) THEN
         L_apply_lbcs = .true.
       Else If ( L_lbc_new ) THEN
         L_apply_lbcs = .false.
       Else !  Old lbcs  
         L_apply_lbcs = .true.
       end if ! L_lbc_new

       If ( L_apply_lbcs ) THEN

        !--------------------------------------------------------------
        !           Update primary fields with LAM LBC data
        !--------------------------------------------------------------

! DEPENDS ON: UPDATE_LAM_LBCS
        CALL UPDATE_LAM_LBCS(                                           &
     &    r_rho_levels, r_theta_levels,                                 &
     &    ROW_LENGTH,ROWS,N_ROWS,MODEL_LEVELS,WET_LEVELS,               &
     &    TR_VARS,TR_LBC_VARS,TR_LEVELS,                                &
     &    A_MAX_TRVARS,A_tr_active_lbc_index,                           &
     &    TR_UKCA,TR_LBC_UKCA,                                          &
     &    A_MAX_UKCAVARS,UKCA_tr_active_lbc_index,                      &
     &    OFFX,OFFY,HALO_I,HALO_J,AT_EXTREMITY,                         &
     &    L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup, L_pc2,                 &
     &    L_mcr_qcf2_lbc, L_mcr_qrain_lbc, L_mcr_qgraup_lbc, L_pc2_lbc, &
     &    L_murk, L_murk_lbc,                                           &
     &    L_LBC_balance, L_int_uvw_lbc,                                 &
     &    L_dust_div1, L_dust_div1_lbc, L_dust_div2, L_dust_div2_lbc,   &
     &    L_dust_div3, L_dust_div3_lbc, L_dust_div4, L_dust_div4_lbc,   &
     &    L_dust_div5, L_dust_div5_lbc, L_dust_div6, L_dust_div6_lbc,   &
     &    L_so2,L_so2_lbc,L_dms,L_dms_lbc,L_so4_aitken,L_so4_aitken_lbc,&
     &    L_so4_accu, L_so4_accu_lbc, L_so4_diss, L_so4_diss_lbc,       &
     &    L_nh3, L_nh3_lbc,                                             &
     &    L_soot_new, L_soot_new_lbc, L_soot_agd, L_soot_agd_lbc,       &
     &    L_soot_cld, L_soot_cld_lbc, L_bmass_new, L_bmass_new_lbc,     &
     &    L_bmass_agd, L_bmass_agd_lbc, L_bmass_cld, L_bmass_cld_lbc,   &
     &    L_ocff_new, L_ocff_new_lbc,                                   &
     &    L_ocff_agd, L_ocff_agd_lbc, L_ocff_cld, L_ocff_cld_lbc,       &
     &    L_nitr_acc, L_nitr_acc_lbc, L_nitr_diss, L_nitr_diss_lbc,     &
     &    RIMWIDTHA(rima_type_norm),RIMWEIGHTSA,                        &
     &    LENRIMA(1,1,rima_type_norm),                                  &
     &    LBC_SIZEA(1,1,1,rima_type_norm),                              &
     &    LBC_STARTA(1,1,1,rima_type_norm),                             &
     &    THETA_LBC,Q_LBC,QCL_LBC,                                      &
     &    QCF_LBC,QCF2_LBC,QRAIN_LBC,                                   &
     &    QGRAUP_LBC, CF_BULK_LBC,CF_LIQUID_LBC,                        &
     &    CF_FROZEN_LBC, RHO_LBC,EXNER_LBC,                             &
     &    U_LBC,V_LBC,W_LBC,                                            &
     &    U_ADV_LBC,V_ADV_LBC,W_ADV_LBC,                                &
     &    MURK_LBC,                                                     &
     &    DUST_DIV1_LBC, DUST_DIV2_LBC, DUST_DIV3_LBC,                  &
     &    DUST_DIV4_LBC, DUST_DIV5_LBC, DUST_DIV6_LBC,                  &
     &    SO2_LBC, DMS_LBC, SO4_AITKEN_LBC,                             &
     &    SO4_ACCU_LBC,SO4_DISS_LBC,NH3_LBC,                            &
     &    SOOT_NEW_LBC, SOOT_AGD_LBC, SOOT_CLD_LBC,                     &
     &    BMASS_NEW_LBC, BMASS_AGD_LBC, BMASS_CLD_LBC,                  &
     &    OCFF_NEW_LBC, OCFF_AGD_LBC, OCFF_CLD_LBC,                     &
     &    NITR_ACC_LBC, NITR_DISS_LBC,                                  &
     &    TRACER_LBC,TRACER_UKCA_LBC,                                   &
     &    QDIMS_L, THETA,Q,QCL,QCF,                                     &
     &    QCF2,QRAIN,QGRAUP,                                            &
     &    CF_BULK,CF_LIQUID,CF_FROZEN,                                  &
     &    RHO,EXNER_RHO_LEVELS,                                         &
     &    U,V,W,                                                        &
     &    U_ADV,V_ADV,W_ADV,                                            &
     &    MURK,                                                         &
     &    DUST_DIV1, DUST_DIV2, DUST_DIV3,                              &
     &    DUST_DIV4, DUST_DIV5, DUST_DIV6,                              &
     &    SO2, DMS, SO4_AITKEN,SO4_ACCU,                                &
     &    SO4_DISS, NH3,                                                &
     &    SOOT_NEW, SOOT_AGD, SOOT_CLD,                                 &
     &    BMASS_NEW, BMASS_AGD, BMASS_CLD,                              &
     &    OCFF_NEW, OCFF_AGD, OCFF_CLD,                                 &
     &    NITR_ACC, NITR_DISS,                                          &
     &    DELTA_PHI, DELTA_LAMBDA,                                      &
     &    BASE_PHI, BASE_LAMBDA,                                        &
     &    DATASTART, lat_rot_NP,                                        &
     &    GLOBAL_ROW_LENGTH, GLOBAL_ROWS,                               &
     &    TRACER, TRACER_UKCA )

! Must now re-calculate the pressure-based variables, namely pressure
! on both rho and theta levels, exner on theta levels and pstar so that
! they are all consistent with the new LBC-updated values of exner on
! rho levels.
!
! DEPENDS ON: consistent_pressure
        call     Consistent_Pressure ( exner_rho_levels,                &
     &           offx,offy,halo_i,halo_J,                               &
     &           row_length,rows,model_levels,                          &
     &           r_theta_levels, r_rho_levels, rho,                     &
     &           p, pstar, p_theta_levels,exner_theta_levels)

! ----------------------------------------------------------------------
! Now check that cloud is consistent with moisture fields for old lbcs
! Only really needed in lateral boundary zone but done everywhere
! ----------------------------------------------------------------------

        IF (L_lbc_old) THEN

! ----------------------------------------------------------------------
!  Check liquid/ice cloud fraction is zero if qcl/qcf = 0
! ----------------------------------------------------------------------

! DEPENDS ON: cloud_check
        Call Cloud_check(                                                &
     &                 j_start, j_stop, i_start, i_stop                  &
     &,                rows, row_length, wet_levels                      &
     &,                halo_i, halo_j                                    &
     &,                QCL, QCF                                          &
     &,                CF_LIQUID, CF_FROZEN                              &
     &,                CF_AREA, CF_BULK)

        END IF !  L_lbc_old

        End If ! L_apply_lbcs

        if ( L_int_uvw_lbc ) then

! Obtain tendencies in the boundary zone for u, v, w and use to make.
! lbcs for u_adv, v_adv, w_adv, This means that  u_adv, v_adv and w_adv
! not needed in lbc files. Similar code needed for tendencies in the solver.

          CALL Atm_Step_phys_init( &
#include "arg_atm_fields.h"
#include "argsts.h"
            r_v, r_u, theta_star, q_star, qcl_star, qcf_star,  &
            cf_star, cfl_star, cff_star, rho_km, cH, exner_lbc_real_tend, &
            w_lbc_real_tend, errorstatus, 'uvw_btnd')

! DEPENDS ON: set_lateral_boundaries
          CALL SET_LATERAL_BOUNDARIES(ROW_LENGTH, ROWS, HALO_I, HALO_J, &
     &    MODEL_LEVELS+1, fld_type_p,W_ADV,                             &
     &    LENRIMA(fld_type_p,halo_type_extended,rima_type_norm),        &
     &    LBC_SIZEA(1,fld_type_p,halo_type_extended,rima_type_norm),    &
     &    LBC_STARTA(1,fld_type_p,halo_type_extended,rima_type_norm),   &
     &    halo_i, halo_j,                                               &
     &    W_LBC_REAL_TEND,                                              &
     &    RIMWIDTHA(rima_type_norm), RIMWIDTHA(rima_type_norm),         &
     &    RIMWEIGHTSA, AT_EXTREMITY,                                    &
     &    L_do_boundaries, L_do_halos)

          lbc_size=LENRIMA(fld_type_u,halo_type_extended, rima_type_norm)
 
          DO k=1,MODEL_LEVELS
            DO i=1,lbc_size
              U_LBC_REAL_TEND(i,k) = U_LBC(i,k) + 0.5 *increment_factor *&
     &                          ( U_LBC_TEND(i,k) - U_LBC(i,k) )
            END DO
          END DO

! DEPENDS ON: set_lateral_boundaries
          CALL SET_LATERAL_BOUNDARIES(                                  &
     &    ROW_LENGTH, ROWS, HALO_I, HALO_J,                             &
     &    MODEL_LEVELS, fld_type_u, U_ADV,                              &
     &    LENRIMA(fld_type_u,halo_type_extended,rima_type_norm),        &
     &    LBC_SIZEA(1,fld_type_u,halo_type_extended,rima_type_norm),    &
     &    LBC_STARTA(1,fld_type_u,halo_type_extended,rima_type_norm),   &
     &    halo_i, halo_j, U_LBC_REAL_TEND,                              &
     &    RIMWIDTHA(rima_type_norm), RIMWIDTHA(rima_type_norm),         &
     &    RIMWEIGHTSA, AT_EXTREMITY, L_do_boundaries, L_do_halos)
     
          lbc_size=LENRIMA(fld_type_v,halo_type_extended, rima_type_norm)
 
          DO k=1,MODEL_LEVELS
            DO i=1,lbc_size
              V_LBC_REAL_TEND(i,k) = V_LBC(i,k) + 0.5 *increment_factor *&
     &                            ( V_LBC_TEND(i,k) - V_LBC(i,k) )
            END DO
          END DO

! DEPENDS ON: set_lateral_boundaries
          CALL SET_LATERAL_BOUNDARIES( ROW_LENGTH, N_ROWS, HALO_I, HALO_J, &
     &    MODEL_LEVELS, fld_type_v, V_ADV,                              &
     &    LENRIMA(fld_type_v,halo_type_extended,rima_type_norm),        &
     &    LBC_SIZEA(1,fld_type_v,halo_type_extended,rima_type_norm),    &
     &    LBC_STARTA(1,fld_type_v,halo_type_extended,rima_type_norm),   &
     &    halo_i, halo_j, V_LBC_REAL_TEND,                              &
     &    RIMWIDTHA(rima_type_norm), RIMWIDTHA(rima_type_norm),         &
     &    RIMWEIGHTSA, AT_EXTREMITY, L_do_boundaries, L_do_halos)

        endif !  L_int_uvw_lbc
         
! DEPENDS ON: timer
      If (Ltimer) Call timer('AS LAM_LBCS',6)
      ENDIF     !   model_domain  ==  mt_lam
!QAN - No code for mt_cyclic_lam yet

!---------------------------------------------------------------------------
! Allocate and initialise arrays for total timestep increments
! NOTE it is very important this is always done before any process changes
!      the fields at the beginning of the timestep.
!---------------------------------------------------------------------------

! DEPENDS ON: timer
      If (Ltimer) Call timer('AS STASH',5)
! Outside cycle_no loop so no test on cycle_no
! DEPENDS ON: Atm_Step_stash
      CALL Atm_Step_stash( &
#include "argd1.h"
#include "argduma.h"
#include "arg_atm_fields.h"
#include "argsts.h"
           errorstatus, 5)
! DEPENDS ON: timer
      If (Ltimer) Call timer('AS STASH',6)

      IF ( L_trap_uv ) THEN
! DEPENDS ON: trap_uv
          CALL trap_uv( U, V, U_ADV, V_ADV, uv_limit,                   &
     &                  rows, n_rows, row_length, model_levels,         &
     &                  offx, offy, halo_i, halo_j, trap_option )
      END IF ! L_trap_uv
      IF ( L_trap_w ) THEN
! DEPENDS ON: trap_w
        CALL trap_w(W, W_ADV, Cw_max, Cw_test_lev,                      &
     &              rows, row_length, model_levels,                     &
     &              r_theta_levels, r_rho_levels, timestep,             &
     &              offx, offy, halo_i, halo_j, trap_option)
      END IF ! L_trap_w

! ----------------------------------------------------------------------
! Section 0.4  Filter winds and theta near poles if active
!              Do horizontal diffusion as a filter if active
! ----------------------------------------------------------------------
!    diagnostic printing of l2norms
      If( L_print_L2norms ) then
        If( L_print_pe .or. mype ==0 ) then
          write(6,*)' ***   L2 norms start of Timestep ***'
        End If ! L_print_pe .or. mype ==0
! This routine is internal to this subroutine so does not need a DEPENDS ON
! and is within a CONTAINS at the bottom of this subroutine. This reduces the
! need to pass down a lot of subroutine arguments.  The one argument is T if
! prognostic or F for increment.
        CALL prfldincnorm(.TRUE.)
      End If !  L_print_L2norms

      If ( L_filter ) then
! DEPENDS ON: timer
      If (Ltimer) Call timer('AS Filter',5)

! section 13:
        IF( SF(0,13) ) THEN    ! Diagnostics required for this section

! Allocate diagnostic space for STASH
          ALLOCATE (STASHwork13(STASH_maxlen(13,A_im)))
        ELSE
          ALLOCATE (STASHwork13(1))
        ENDIF

! DEPENDS ON: ni_filter_ctl
       Call NI_filter_Ctl(  THETA,                                      &
     &                      U, V, W,                                    &
     &                      EXNER_RHO_LEVELS, RHO,                      &
     &                      row_length, rows, n_rows, model_levels,     &
     &                      r_theta_levels, r_rho_levels,               &
     &                      r_at_u, r_at_v, delta_lambda, delta_phi,    &
     &                      cos_theta_longitude, sin_theta_longitude,   &
     &                      sin_theta_latitude, sin_v_latitude,         &
     &                      cos_theta_latitude, sec_theta_latitude,     &
     &                      cos_v_latitude, sec_v_latitude,             &
     &                      polar_filter_north_lat_limit,               &
     &                      polar_filter_south_lat_limit,               &
     &                      polar_filter_coefficient,                   &
     &                      polar_filter_n_sweeps,                      &
     &                      polar_filter_step_per_sweep,                &
     &                      polar_filter_lat_limit,                     &
     &                      max_121_rows, u_sweeps, v_sweeps,           &
     &                      global_u_filter, global_v_filter,           &
     &                      u_begin, u_end, v_begin, v_end,             &
     &                      diff_coeff_phi, diff_coeff_u, diff_coeff_v, &
     &                      diff_coeff_thermo, diff_coeff_wind,         &
     &                      diff_order_thermo, diff_order_wind,         &
     &                      first_constant_r_rho_level,                 &
     &                      first_constant_r_rho_level_m1,              &
     &                      top_filt_start, top_filt_end,               &
     &                      up_diff, max_updiff_levels,                 &
     &                      horizontal_level, mype,                     &
     &                      global_row_length, global_rows,             &
     &                      offx, offy, halo_i, halo_j,                 &
     &                      nproc, nproc_x, nproc_y, datastart,         &
     &                      neighbour, at_extremity, model_domain,      &
     &                      gc_proc_row_group, gc_proc_col_group,       &
     &                      L_polar_filter, L_pofil_new,                &
     &                      L_pfcomb, L_pftheta, L_pfuv,                &
     &                      L_pfw, L_pfexner, L_diff_exner,             &
     &                      L_diff_thermo, L_diff_wind, L_diff_w,       &
     &                      L_pofil_hadgem2, exner_theta_levels,        &
#include "argsts.h"
     &                      STASHwork13)

      if(L_pfexner .and. L_pofil_new)then
! DEPENDS ON: consistent_pressure
        call     Consistent_Pressure ( exner_rho_levels,                &
     &           offx,offy,halo_i,halo_J,                               &
     &           row_length,rows,model_levels,                          &
     &           r_theta_levels, r_rho_levels, rho,                     &
     &           p, pstar, p_theta_levels,exner_theta_levels)
      endif  !  (L_pfexner .and. l_pofil_new)

! DEPENDS ON: timer
      If (Ltimer) Call timer('AS Filter',6)
      End If    !  L_filter

! ----------------------------------------------------------------------
! Section 0.5  If free-slip then put non-zero w field on bottom boundary
! ----------------------------------------------------------------------

      if ( L_free_slip ) then
! DEPENDS ON: bottom_w_calc
        Call Bottom_w_Calc(r_theta_levels, u, v, w, w_adv,              &
     &                     sec_theta_latitude, delta_lambda, delta_phi, &
     &                     glambda_p(lambda_start), phi_p,              &
     &                     glambda_u(lambda_start), phi_v,              &
     &                     rows, n_rows, row_length, model_levels,      &
     &                     model_domain, L_regular,                     &
     &                     offx, offy, halo_i, halo_j,                  &
     &                     gc_proc_row_group, at_extremity,             &
     &                     global_row_length)

! DEPENDS ON: swap_bounds
        call Swap_Bounds(W, row_length, rows, model_levels+1,           &
     &                   offx, offy, fld_type_p, .false.)

! DEPENDS ON: swap_bounds
        call Swap_Bounds(W_ADV, row_length, rows, model_levels+1,       &
     &                   halo_i, halo_j, fld_type_p, .false.)

      endif !( L_free_slip )

! DEPENDS ON: timer
      If (Ltimer) Call timer('AS Stochastic_Phys',5)
!L --------- UM Section 35---- Stochastic Physics Setup -------------
!  Called only at the first time-step with no arguments
      IF (first_atmstep_call) THEN
        IF (l_rp2 .or. l_skeb2) THEN
          call stph_setup ( )
        END IF
      END IF
!L --------- UM Section 35---- Stochastic Physics Setup END ---------


!L ----- UM Section 35---- Stochastic Physics Random Parameters -----
! Call to the RANDOM PARAMETERS2 (STPH_RP2) subroutine
      IF (l_physics .and. l_rp2) THEN
        minutos=timestep/60
        IF(MOD(i_hour,3) == 0 .and. i_minute == minutos) THEN
          IF (PrintStatus  >=   PrStatus_Normal) THEN
            WRITE(6,*) 'CALLING RANDOM PARAMETERS2'
          END IF
          CALL stph_rp2(model_levels,                                   &
                  rhcrit,rhcrit_max,rhcrit_min,                         &
                  gwd_frc,gwd_frc_max,gwd_frc_min,                      &
                  kay_gwave,kay_gwave_max,kay_gwave_min,                &
                  m_ci,m_ci_max,m_ci_min,                               &
                  charnock)
        ELSE
         IF (printstatus  >=  prstatus_normal) THEN
             WRITE(6,*) 'NOT CALLING RANDOM PARAMETERS2'
             WRITE(6,*) 'This routine is only called every 3hrs'
         END IF
        END IF
      END IF
!L ----- UM Section 35---- Stochastic Physics Random Parameters END ---
! DEPENDS ON: timer
      If (Ltimer) Call timer('AS Stochastic_Phys',6)

! ----------------------------------------------------------------------
!    diagnostic printing of l2norms
      If( L_print_L2norms ) then
        If( L_print_pe .or. mype ==0 ) then
          write(6,*)' ***   L2 norms after Filter_ctl ***'
        End If ! L_print_pe .or. mype ==0
        CALL prfldincnorm(.TRUE.)
      End If !  L_print_L2norms
! ---------------------------------------------------------------

! ---------------------------------------------------------------
!    diagnostic printing at beginning of timestep 1
!    (which is really timestep 0 values so lets record it as such)
! ---------------------------------------------------------------
       if( L_diag_print .and. timestep_number ==1 ) then
! DEPENDS ON: print_diag
         Call Print_diag(U, V, THETA, RHO, W, Q, QCL, QCF,              &
     &                  rows, n_rows, row_length,                       &
     &                  model_levels, wet_levels, model_domain,         &
     &                  global_row_length, global_rows,                 &
     &                  r_theta_levels, r_rho_levels,                   &
     &                  r_at_u, r_at_v ,                                &
     &                  FV_sec_theta_latitude, FV_cos_theta_latitude,   &
     &                  cos_v_latitude,                                 &
     &                  cos_theta_longitude, sin_theta_longitude,       &
     &                  offx, offy, halo_i, halo_j,                     &
     &                  mype, nproc, at_extremity, datastart,           &
     &                  gc_proc_row_group, delta_lambda, delta_phi,     &
     &                  timestep_number-1, print_step, diag_interval,   &
     &                  rpemax, rpemin, ipesum, rpesum, w_print_limit,  &
     &                  L_print_pe, L_print_w,                          &
     &                  L_print_wmax, L_print_max_wind,                 &
     &                  L_print_div, L_print_lapse, L_print_theta1,     &
     &                  L_print_shear, L_diag_wind, L_diag_noise,       &
     &                  max_w_run(1:), max_wind_run, min_theta1_run,    &
     &                  dtheta1_run, max_div_run, min_div_run,          &
     &                  min_lapse_run, max_shear_run, time_max_shear,   &
     &                  time_div_max, time_div_min, time_lapse_min,     &
     &                  time_w_max, time_max_wind, time_theta1_min,     &
     &                  max_KE_run, min_KE_run, max_noise_run,          &
     &                  time_KE_max, time_KE_min, time_noise_max )
! DEPENDS ON: um_fort_flush
         if ( L_flush6 ) call UM_FORT_FLUSH(6,info)
       endif     !  timestep_number ==1

! ---------------------------------------------------------------
! Section 1.0  Call Atmospheric Physics1
! ----------------------------------------------------------------------

! DEPENDS ON: timer
      IF (Ltimer) CALL TIMER('AS Atmos_Phys1 (AP1)',5)
      ! Biogenic aerosol climatology for the climate and NWP models
      IF (L_USE_BIOGENIC) THEN 
        do k=1, model_levels
          do j=1, rows
            do i=1, row_length
              biogenic(i,j,k) = ARCLBIOG_BG(i,j,k)
            end do
          end do
        end do
      END IF
            
! Aerosol climatologies - Model switches and climatologies are
! gathered into bigger arrays.

! First set the internal model switches according to the
! CNTLATM settings, and determine how many components we need.
      CALL set_arcl_dimensions(L_USE_ARCLBIOM, L_USE_ARCLBLCK,          &
     &                         L_USE_ARCLSSLT, L_USE_ARCLSULP,          &
     &                         L_USE_ARCLDUST, L_USE_ARCLOCFF,          &
     &                         L_USE_ARCLDLTA, n_arcl_species,          &
     &                         n_arcl_compnts, L_USE_ARCL )
      
      ! If the aerosol climatology for NWP is used, n_arcl_species
      ! is larger than 0. In that case, allocate the array gathering
      ! component mass-mixing ratio and take the values from the
      ! arrays in arg_atm_fields.h.

      if (n_arcl_species > 0) Then
  
        allocate(arcl(row_length, rows, model_levels, n_arcl_compnts))
      
        call set_arcl_clim(row_length, rows, model_levels,              &
     &                     n_arcl_compnts,                              &
                           ! Internal model switches
     &                     L_USE_ARCL,                                   &
                           ! Climatologies from ancillary files
     &                     arclbiom_fr, arclbiom_ag, arclbiom_ic,        &
     &                     arclblck_fr, arclblck_ag,                     &
     &                     arclsslt_fi, arclsslt_jt,                     &
     &                     arclsulp_ac, arclsulp_ak, arclsulp_di,        &
     &                     arcldust_b1, arcldust_b2, arcldust_b3,        &
     &                     arcldust_b4, arcldust_b5, arcldust_b6,        &
     &                     arclocff_fr, arclocff_ag, arclocff_ic,        &
     &                     arcldlta_dl,                                  &
                           ! Internal climatology array
     &                     arcl,                                         &
                           ! Component array indices
     &                     i_arcl_compnts )
        
      else
         allocate ( arcl(1,1,1,1) )
      end if

!
! UKCA_RADAER: Obtain current UKCA setup and allocate arrays.
!              Most of the work needs only be made for the first
!              Atm_Step() call.
!
      IF (ErrorStatus == 0) THEN
        
        IF (l_ukca_radaer) THEN
      
          IF (first_atmstep_call) THEN

! DEPENDS ON: ukca_radaer_init        
            CALL ukca_radaer_init(                                      &
                                ErrorStatus,                            &
                                Cmessage,                               &
#include "argsts.h"        
                                ukca_radaer                             &
            )
          
            IF (ErrorStatus /= 0) THEN

              CALL Ereport("ATM_STEP", ErrorStatus, Cmessage)
            END IF
          
          END IF
        
          !
          ! Allocate those arrays that will receive UKCA output.
          !
          ALLOCATE(ukca_radaer%mix_ratio(row_length, rows,              &
                             model_levels, ukca_radaer%n_cpnt))
          ALLOCATE(ukca_radaer%comp_vol(row_length, rows,               &
                             model_levels, ukca_radaer%n_cpnt))
     
          ALLOCATE(ukca_radaer%dry_diam(row_length, rows,               &
                             model_levels, ukca_radaer%n_mode))
          ALLOCATE(ukca_radaer%wet_diam(row_length, rows,               &
                             model_levels, ukca_radaer%n_mode))
          ALLOCATE(ukca_radaer%modal_rho(row_length, rows,              &
                             model_levels, ukca_radaer%n_mode))
          ALLOCATE(ukca_radaer%modal_wtv(row_length, rows,              &
                             model_levels, ukca_radaer%n_mode))
          ALLOCATE(ukca_radaer%modal_vol(row_length, rows,              &
                             model_levels, ukca_radaer%n_mode))
          ALLOCATE(ukca_radaer%modal_nbr(row_length, rows,              &
                             model_levels, ukca_radaer%n_mode))
        

          !
          ! Get current UKCA results
          !
! DEPENDS ON: ukca_radaer_get
          CALL ukca_radaer_get(                                         &
                             ErrorStatus,                               &
                             Cmessage,                                  &
                             first_atmstep_call,                        &
#include "argd1.h"
#include "argsts.h"
                             ukca_radaer                                &
          )
     
          IF (ErrorStatus /= 0) THEN
        
            DEALLOCATE(ukca_radaer%mix_ratio)
            DEALLOCATE(ukca_radaer%comp_vol)
            DEALLOCATE(ukca_radaer%dry_diam)
            DEALLOCATE(ukca_radaer%wet_diam)
            DEALLOCATE(ukca_radaer%modal_rho)
            DEALLOCATE(ukca_radaer%modal_wtv)
            DEALLOCATE(ukca_radaer%modal_vol)
            DEALLOCATE(ukca_radaer%modal_nbr)

          
            CALL Ereport("ATM_STEP", ErrorStatus, Cmessage)
        
          END IF
      
        ELSE
      
          ! l_ukca_radaer is false. Set array dimensions
          ! to minimum value and allocate minimum space.
          ukca_radaer%n_mode = 1
          ukca_radaer%n_cpnt = 1
          
          ALLOCATE(ukca_radaer%mix_ratio(1, 1, 1, 1))
          ALLOCATE(ukca_radaer%comp_vol(1, 1, 1, 1))
          ALLOCATE(ukca_radaer%dry_diam(1, 1, 1, 1))
          ALLOCATE(ukca_radaer%wet_diam(1, 1, 1, 1))
          ALLOCATE(ukca_radaer%modal_rho(1, 1, 1, 1))
          ALLOCATE(ukca_radaer%modal_wtv(1, 1, 1, 1))
          ALLOCATE(ukca_radaer%modal_vol(1, 1, 1, 1))
          ALLOCATE(ukca_radaer%modal_nbr(1, 1, 1, 1))
          
        END IF ! l_ukca_radaer
      
      END IF ! ErrorStatus

! UKCA_CDNC: Allocate array and obtain Cloud Droplet No. Concentration from D1

      IF (ErrorStatus == 0) THEN

        IF (L_ukca .AND. (l_ukca_aie1 .OR. l_ukca_aie2)) THEN

! Allocate arrays to receive UKCA output.
          ALLOCATE(ukca_cdnc%cdnc(row_length, rows, model_levels))
          ALLOCATE(ukca_cdnc%cdnc3(row_length, rows, model_levels))

! Get current UKCA results
          CALL ukca_cdnc_get(                                           &
#include "argd1.h"
#include "argsts.h"
                             first_atmstep_call,                        &
                             ErrorStatus,                               &
                             Cmessage)

          IF (ErrorStatus /= 0) THEN
            DEALLOCATE(ukca_cdnc%cdnc)
            DEALLOCATE(ukca_cdnc%cdnc3)
            CALL Ereport("ATM_STEP", ErrorStatus, Cmessage)
          END IF

! Set minimum CDNC value (equivalent to 5 cm-3) 
          WHERE(ukca_cdnc%cdnc < 5.0e06) ukca_cdnc%cdnc = 5.0e06

          cdnc_dim1 = row_length 
          cdnc_dim2 = rows 
          cdnc_dim3 = model_levels

        ELSE

          ! both L_UKCA_AIE1 and L_UKCA_AIE2 are false.
          ALLOCATE(ukca_cdnc%cdnc(1,1,1))
          ukca_cdnc%cdnc(:,:,:) = 0.0
          ALLOCATE(ukca_cdnc%cdnc3(1,1,1))
          ukca_cdnc%cdnc3(:,:,:) = 0.0

          cdnc_dim1 = 1
          cdnc_dim2 = 1
          cdnc_dim3 = 1

        END IF ! L_UKCA_AIE1 or L_UKCA_AIE2

      END IF ! ErrorStatus = 0


      IF (L_Physics .AND. ErrorStatus == 0) THEN

! DEPENDS ON: Atm_Step_phys_init
      CALL Atm_Step_phys_init( &
#include "arg_atm_fields.h"  
#include "argsts.h"
           r_v, r_u, theta_star, q_star, qcl_star, qcf_star,  &
           cf_star, cfl_star, cff_star, rho_km, cH, exner_lbc_real_tend, &
           w_lbc_real_tend, errorstatus, 'tropinit')   

! DEPENDS ON: o3_to_3d
        Call O3_to_3D(lexpand_ozone, i_ozone_int,                       &
     &                rows, row_length, model_levels, ozone_levels,     &
     &                halo_i, halo_j, offx, offy, at_extremity,         &
     &                a_realhd(rh_z_top_theta),                         &
     &                THETA,                                            &
     &                EXNER_THETA_LEVELS,                               &
     &                RHO,                                              &
     &                EXNER_RHO_LEVELS,                                 &
     &                nd_o3, O3,                                        &
     &  min_trop_level, max_trop_level,                                 &
     &  L_O3_trop_level,L_O3_trop_height,                               &
     &  L_T_trop_level,L_T_trop_height,                                 &
     &  O3_trop_level,O3_trop_height,                                   &
     &  T_trop_level,T_trop_height,                                     &
     &  gc_proc_row_group,                                              &
     &  global_row_length, ozone3D, ErrorStatus, cmessage )

! DEPENDS ON: Atm_Step_phys_init
      CALL Atm_Step_phys_init( &
#include "arg_atm_fields.h"  
#include "argsts.h"
            r_v, r_u, theta_star, q_star, qcl_star, qcf_star,  &
            cf_star, cfl_star, cff_star, rho_km, cH, exner_lbc_real_tend, &
            w_lbc_real_tend, errorstatus, 'ozoninit')

      END IF !  L_Physics

! DEPENDS ON: Atm_Step_phys_init
      CALL Atm_Step_phys_init( &
#include "arg_atm_fields.h"
#include "argsts.h"
           r_v, r_u, theta_star, q_star, qcl_star, qcf_star,  &
           cf_star, cfl_star, cff_star, rho_km, cH, exner_lbc_real_tend, &
           w_lbc_real_tend, errorstatus, 'microphy')

      IF (L_Physics .AND. ErrorStatus == 0) THEN

! DEPENDS ON: Atm_Step_diag
        CALL Atm_Step_diag( &
#include "arg_atm_fields.h"
#include "argsts.h"
             r_v, r_u, r_w, q_star, qcl_star, qcf_star,  theta_star, &
             cf_star, cfl_star, cff_star, 1)

! Convert to mixing ratios from specific humidities if needed
! DEPENDS ON: Atm_Step_alloc
       CALL Atm_Step_alloc( &
#include "arg_atm_fields.h"
            cf_star, cfl_star, cff_star, q_star, qcl_star, qcf_star, &
            frac_control, r_u, r_v, r_w, errorstatus,'MixRatio')

       IF (L_cosp) THEN
!        This is done until a prognostic is developed
         IF (timestep_number == 1) THEN
           nullify(cosp_crain_3d,cosp_csnow_3d)
         END IF
         IF (.not. associated(cosp_crain_3d)) THEN
           IF (PrintStatus >= PrStatus_Oper) THEN
             WRITE(6,'(A,I5)')                                          &
               'COSP in ATM_STEP: allocating cosp_crain_3d in tstep ', &
               timestep_number
           END IF
           ALLOCATE(cosp_crain_3d(row_length,rows,model_levels))
           cosp_crain_3d = 0.0
         END IF
         IF (.not. associated(cosp_csnow_3d)) THEN
           IF (PrintStatus >= PrStatus_Oper) THEN
             WRITE(6,'(A,I5)')                                          &
               'COSP in ATM_STEP: allocating cosp_csnow_3d in tstep ', &
               timestep_number
           END IF
           ALLOCATE(cosp_csnow_3d(row_length,rows,model_levels))
           cosp_csnow_3d = 0.0
         END IF
       ELSE
         IF (.not. associated(cosp_crain_3d)) THEN
           ALLOCATE(cosp_crain_3d(1,1,1))
           cosp_crain_3d = 0.0
         END IF
         IF (.not. associated(cosp_csnow_3d)) THEN
           ALLOCATE(cosp_csnow_3d(1,1,1))
           cosp_csnow_3d = 0.0
         END IF
       END IF

! NB if you are changing the argument list to atmos_physics1, please
! do an equivalent change in routine scm_main to keep the single column
! model consistent.

! DEPENDS ON: atmos_physics1
        Call Atmos_Physics1(                                             &
! Parallel variables
     & global_row_length, global_rows, nproc, nproc_x ,nproc_y           &
     &,g_rows, g_row_length                                              &

! model dimensions
     &, row_length, rows, n_rows, land_field, model_levels, wet_levels   &
     &, bl_levels, st_levels, sm_levels, Ozone_levels, cloud_levels      &
     &, land_ice_points, soil_points, n_cca_lev, ntiles, nice_use        &
     &, salt_dim1, salt_dim2, salt_dim3, tr_levels, tr_ukca              &
      , cdnc_dim1, cdnc_dim2, cdnc_dim3                                  &
     &, co2_dim_len, co2_dim_row, co2_dim_lev                            &
     &, n_arcl_species, n_arcl_compnts, i_arcl_compnts                   &

! model switches
     &,     L_regular, L_lbc_old                                         &
     &,     L_Rad_Step, L_Rad_Step_diag, L_Rad_Step_prog                 &
     &,     LCAL360, Ltimer                                              &
     &,     L_mr_physics1                                                &
     &,     L_ukca_chem, L_ukca_useumuivals, L_ukca_set_trace_gases      &
     &,     L_ukca_strat, L_ukca_strattrop                               &
     &,     L_ukca_prescribech4, L_USE_ARCL                              &

! model Parameters
     &,     RHcrit                                                       &
     &,     min_trop_level, max_trop_level                               &


! Pass position of greenhouse gases in tracer_ukca array, for chemical coupling
     &,     ngrgas ,grgas_addr, Ntot_land, Ntot_sea                      &
! parameter for stochastic physics random parameters2
     &,     M_CI                                                         &
! Vertical coordinate levels.
     &, delta_lambda, delta_phi, lat_rot_NP, long_rot_NP                 &
! Time stepping information
     &, I_year, I_day_number, I_hour, I_minute                           &
     &, I_second, PREVIOUS_TIME,                                         &
! diagnostic info
#include "argsts.h"
     &      STASHwork1,STASHwork2,STASHwork4,STASHwork6,STASHwork14      &
! Additional variables for SCM diagnostics
     &,     nSCMDpkgs, L_SCMDiags                                        &
! Data Fields.
     &,     THETA, Q, QCL, QCF, QCF2, QRAIN, QGRAUP, RHO, U, V           &
     &,     P, PSTAR, EXNER_RHO_LEVELS, EXNER_THETA_LEVELS, LAND         &
     &,     P_THETA_LEVELS, FRAC_LAND,frac_control                       &
      ,     ukca_cdnc%cdnc                                               &
! ancillary fields and fields needed to be kept from timestep to
! timestep
     &,land_index, RGRAIN_TILE,SNSOOT,NTML, CUMULUS                      &
     &,ice_fraction,p_ice_fract_rad, p_ice_thick_rad                     &
     &,CCA, CCB, CCT, CCLWP, CCW_RAD, LCBASE                             &
     &,TSTAR,TSTAR_LAND,TSTAR_SEA,p_tstar_sice                           &
     &,SICE_ALB, LAND_ALB, SNODEP,p_snodep_sice                          &
     &,ozone3D,SW_INCS, LW_INCS, DIRPAR                                  &
     &,O3_trop_level, O3_trop_height, T_trop_level, T_trop_height        &
     &,ZH, OROG_SD, OROG_GRAD_XX, OROG_GRAD_XY                           &
     &,OROG_GRAD_YY, CF_AREA, CF_BULK,CF_LIQUID,CF_FROZEN,MURK_SOURCE    &
      ,arcl, soil_alb, obs_alb_sw, obs_alb_vis, obs_alb_nir, lai_pft     & 
      ,snodep_tile, frac_typ,tstar_tile,z0_tile,dolr_field,lw_down       & 
      ,sw_tile_rts,es_space_interp, rad_mask, cos_zenith_angle           & 
! Variables for COSP
      ,cosp_crain_3d,cosp_csnow_3d                                       &
! IN JULES 2 prognostics
      ,snowdepth, lake_h_ice                                             &
! IN/OUT
     &,theta_star, q_star, qcl_star, qcf_star, qcf2_star, qrain_star     &
     &,qgraup_star, cf_star, cfl_star, cff_star, R_u, R_v                &
     &,a_realhd(rh_energy_corr), NET_FLUX, NET_MFLUX                     &
     &,MURK, DUST_DIV1, DUST_DIV2, DUST_DIV3, DUST_DIV4, DUST_DIV5       &
     &,DUST_DIV6, SO2, SO4_AITKEN,SO4_ACCU, SO4_DISS, NH3, SOOT_NEW      &
     &,SOOT_AGD, SOOT_CLD, BMASS_NEW, BMASS_AGD, BMASS_CLD, OCFF_NEW,    &
     & OCFF_AGD, OCFF_CLD, NITR_ACC, NITR_DISS, CO2, TRACER_UKCA         &
     &,biogenic, A_INTHD(23), ukca_radaer                                &
! OUT Fields
     &,     ls_rain, ls_snow, micro_tends, unscaled_dry_rho              &
      ,     photosynth_act_rad, rad_hr, dolr, SW_tile                    &
! error information
     &,     ErrorStatus  )


      
      If (l_mr_physics1) Then

! Convert back from  mixing ratios to spec. humidities if needed
! Allocates mix_*_star variables and copies *_star to mix_*_star
! DEPENDS ON: Atm_Step_phys_init
        CALL Atm_Step_phys_init( &
#include "arg_atm_fields.h"  
#include "argsts.h"   
           r_v, r_u, theta_star, q_star, qcl_star, qcf_star,  &
           cf_star, cfl_star, cff_star, rho_km, cH, exner_lbc_real_tend, &
           w_lbc_real_tend, errorstatus, 'MRtoSpHm')



! Convert the mixing ratio increments (stored in mix_v_star etc.) to 
! spec hum increments and write these back into the q_star variables.
! d1 variables contain the mixing ratios before atmos_physics1.
! q_store variables contain spec hums before atmos_physics1.
! DEPENDS ON: calc_q_star
        call calc_q_star (row_length, rows, wet_levels,                 &
     &           halo_i, halo_j, offx, offy,                            &
     &           Q, QCL, QCF, QCF2, QRAIN, QGRAUP,                      &
     &           mix_v_star, mix_cl_star, mix_cf_star,                  &
     &           mix_cf2_star, mix_rain_star, mix_graup_star,           &
     &           L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,                 &
     &           q_store, qcl_store, qcf_store,                         &
     &           qcf2_store, qrain_store, qgraup_store,                 &
     &           q_star, qcl_star, qcf_star,                            &
     &           qcf2_star, qrain_star, qgraup_star )

! Copy contents of q_store (sp hums before atmos_physics1) back into the 
! d1(q) variables etc. Include the halo points.
! store physics changes for use in Sl_moist_conserve
! DEPENDS ON: Atm_Step_alloc
          CALL Atm_Step_alloc( &
#include "arg_atm_fields.h"
             cf_star, cfl_star, cff_star, q_star, qcl_star, qcf_star, &
             frac_control, r_u, r_v, r_w, errorstatus, 'cpqstore' )


! NOTE converted everything back to specific humidities at this point and 
! deallocated *_store and mix_*_star arrays 

      END IF  ! l_mr_physics1

! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS STASH',5)
! DEPENDS ON: Atm_Step_stash
        CALL Atm_Step_stash( &
#include "argd1.h"
#include "argduma.h"
#include "arg_atm_fields.h"
#include "argsts.h"
             errorstatus, 1)
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS STASH',6)

      ELSE  ! L_physics =.false.

! initialise arrays that hold physics increments to zero
! DEPENDS ON: Atm_Step_phys_init
         CALL Atm_Step_phys_init( &
#include "arg_atm_fields.h"
#include "argsts.h"
           r_v, r_u, theta_star, q_star, qcl_star, qcf_star,  &
           cf_star, cfl_star, cff_star, rho_km, cH, exner_lbc_real_tend, &
           w_lbc_real_tend, errorstatus, 'zeroincs')

      END IF ! L_Physics
      
! Deallocate the arrays of UKCA_RADAER
      DEALLOCATE(ukca_radaer%mix_ratio)
      DEALLOCATE(ukca_radaer%comp_vol)
      DEALLOCATE(ukca_radaer%dry_diam)
      DEALLOCATE(ukca_radaer%wet_diam)
      DEALLOCATE(ukca_radaer%modal_rho)
      DEALLOCATE(ukca_radaer%modal_wtv)
      DEALLOCATE(ukca_radaer%modal_vol)
      DEALLOCATE(ukca_radaer%modal_nbr)

! Deallocate arrays for UKCA_CDNC 
      DEALLOCATE(ukca_cdnc%cdnc) 
      DEALLOCATE(ukca_cdnc%cdnc3)      

! Deallocate the array of the aerosol climatology for NWP
      deallocate(arcl)

      If (L_tracer .and. ErrorStatus == 0) then

! store physics changes for use in Sl_tracer2
! DEPENDS ON: tr_set_phys 
        call TR_Set_Phys(super_array_size, super_tracer_phys1,          &
                         L_CO2_interactive, CO2,                        &
                         L_Murk_advect, murk,                           &
                         L_Soot, soot_new, soot_agd, soot_cld,          &
                         L_SULPC_SO2, SO2, SO4_aitken,                  &
                         so4_accu, so4_diss, L_sulpc_nh3, nh3,          &
                         L_sulpc_dms, dms,                              &
                         L_dust, DUST_DIV1, DUST_DIV2,  DUST_DIV3,      &
                                 DUST_DIV4, DUST_DIV5,  DUST_DIV6,      &
                         L_biomass, bmass_new, bmass_agd, bmass_cld,    &
                         L_ocff, ocff_new, ocff_agd, ocff_cld,          &
                         L_nitrate, nitr_acc, nitr_diss,                &
                         L_USE_CARIOLLE, OZONE_TRACER,                  &
                         tracer_phys1, tracer,                          &
                         ukca_tracer_phys1, tracer_ukca,                &
                         row_length, rows,                              &
                         model_levels, tr_levels, tr_vars, tr_ukca,     &
                         offx, offy, model_domain,                      &
                         .true., tdims_l )

      end if  ! L_tracer and ErrorStatus == 0
     
! store physics changes for use in Sl_moist_conserve
! DEPENDS ON: Atm_Step_alloc
          CALL Atm_Step_alloc( &
#include "arg_atm_fields.h"
               cf_star, cfl_star, cff_star, q_star, qcl_star, qcf_star, &
               frac_control, r_u, r_v, r_w, errorstatus, 'alloc_mr' )

! DEPENDS ON: timer
      IF (Ltimer) CALL TIMER('AS Atmos_Phys1 (AP1)',6)
! ---------------------------------------------------------------
!    diagnostic printing of l2norms
      if( L_print_L2norms ) then
        IF( L_print_pe .or. mype ==0 ) then
          write(6,*)' ***   L2 norms after Atmos_Phys1 ***'
        ENDIF ! L_print_pe .or. mype ==0
        R_w =0.0
        CALL prfldincnorm(.FALSE.)
      endif !  L_print_L2norms
! ---------------------------------------------------------------

! When cycling the following variables need to be reset (at the start
!   of each new cycle) to the value they had on exit from Physics1.

! DEPENDS ON: Atm_Step_phys_reset
        CALL Atm_Step_phys_reset( &
#include "arg_atm_fields.h"
          q_star, qcl_star, qcf_star, r_v, r_u, dolr, theta_star, gs1,&
          cf_star, cfl_star, cff_star, 'cyclrset')

! DEPENDS ON: Atm_Step_alloc
      CALL Atm_Step_alloc(&
#include "arg_atm_fields.h"
          cf_star, cfl_star, cff_star, q_star, qcl_star, qcf_star, &
          frac_control, r_u, r_v, r_w, errorstatus, 'newtdisc') 

! ----------------------------------------------------------------------
! Section 2. Calculate new time-level estimates to Theta, q, qcl, qcf
!            by calling semi-Lagrangian advection routine.
!            Estimated values are returned in the _star variables.
! ----------------------------------------------------------------------

        ALLOCATE (exner_prime( 1-offx:row_length+offx, 1-offy:rows+offy,&
     &                         model_levels) )

        exner_prime(:,:,:) = 0.0

! Iterative SISL: Cycle through dynamics-physics to enable trajectory
! calculations from interpolated winds and utilize improved time
! discretization if requested.

      Do CycleNo = 1, NumCycles

! Restore phys1 variables to be used as predictors
! DEPENDS ON: Atm_Step_phys_reset
        CALL Atm_Step_phys_reset( &
#include "arg_atm_fields.h"
          q_star, qcl_star, qcf_star, r_v, r_u, dolr, theta_star, gs1,&
          cf_star, cfl_star, cff_star, 'phy1rest')

! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS S-L Advect (AA)',5)

! Note: The departure points which are passed into sl_full_wind are
! returned in depart_lambda for lambda, depart_phi for phi and
! depart_r_w for r. depart_r_theta is passed into sl_tracer1.

      If (ErrorStatus  ==  0 ) Then

! DEPENDS ON: Atm_Step_diag
         CALL Atm_Step_diag( &
#include "arg_atm_fields.h"
#include "argsts.h"
              r_v, r_u, r_w, q_star, qcl_star, qcf_star,  theta_star, &
              cf_star, cfl_star, cff_star, 2)

! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AA SL_Thermo',5)
! DEPENDS ON: ni_sl_thermo
        Call NI_SL_Thermo(moisture_array_size,                          &
     &                    theta, q, qcl, qcf, qcf2, qrain, qgraup,      &
     &                    mix_v, mix_cl, mix_cf,                        &
     &                    mix_cf2, mix_rain, mix_graup,                 &
     &                    cf_bulk, cf_liquid, cf_frozen,                &
     &                    e_trb, tsq_trb, qsq_trb, cov_trb,             &
     &                    q_star, qcl_star, qcf_star,                   &
     &                    qcf2_star, qrain_star, qgraup_star,           &
     &                    mix_v_star, mix_cf_star, mix_cl_star,         &
     &                    mix_cf2_star, mix_rain_star, mix_graup_star,  &
     &                    cf_star, cfl_star, cff_star,                  &
     &                    exner_star, theta_star, theta_np1,            &
     &                    w, w_adv, u_adv, v_adv,                       &
     &                    exner_theta_levels,                           &
     &                    pstar, p, p_theta_levels, rho,                &
     &             eta_rho_levels, eta_theta_levels, r_rho_levels,      &
     &             r_theta_levels, row_length, rows, n_rows,            &
     &                    model_levels, wet_levels, bl_levels,          &
     &                    alpha_2, check_bottom_levels,                 &
     &                    interp_vertical_search_tol,                   &
     &                    first_constant_r_rho_level,                   &
     &                    delta_lambda, delta_phi,                      &
     &             glambda_p, phi_p, glambda_u, phi_v,                  &
     &             gdlambda_p, dphi_p, gdlambda_u, dphi_v, grecip_dlamp,&
     &             recip_dphip, grecip_dlamu, recip_dphiv,              &
     &             wt_lambda_p, wt_phi_p, wt_lambda_u, wt_phi_v,        &
     &             lambda_p_rm, lambda_p_rp, lambda_u_rm, lambda_u_rp,  &
     &             phi_p_rm, phi_p_rp, phi_v_rm, phi_v_rp,              &
     &           recip_lambda_p_m, recip_lambda_p_0, recip_lambda_p_p,  &
     &           recip_lambda_p_p2, recip_lambda_u_m, recip_lambda_u_0, &
     &           recip_lambda_u_p, recip_lambda_u_p2, recip_phi_p_m,    &
     &           recip_phi_p_0, recip_phi_p_p, recip_phi_p_p2,          &
     &           recip_phi_v_m, recip_phi_v_0, recip_phi_v_p,           &
     &           recip_phi_v_p2, Base_lambda, base_phi, lambda_p_end,   &
     &           phi_p_end, dlambda_p_end, dphi_p_end, dphi_v_end,      &
     &           recip_dlam, recip_dphi, max_look,                      &
     &           look_lam, look_phi, halo_lam, halo_phi,                &
     &                 timestep, FV_cos_theta_latitude,                 &
     &                 cos_theta_latitude, sec_theta_latitude,          &
     &                 sin_theta_latitude, tan_theta_latitude,          &
     &                 cos_v_latitude, sec_v_latitude,                  &
     &                 sin_v_latitude, tan_v_latitude,                  &
     &                 sin_theta_longitude, cos_theta_longitude,        &
     &                 LAM_max_cfl, THETA_LBC,                          &
     &                 RIMWIDTHA(rima_type_norm),RIMWEIGHTSA,           &
     &                 LENRIMA(fld_type_p,halo_type_extended,           &
     &                         rima_type_norm),                         &
     &                 LBC_SIZEA(1,fld_type_p,halo_type_extended,       &
     &                           rima_type_norm),                       &
     &                 LBC_STARTA(1,fld_type_p,halo_type_extended,      &
     &                            rima_type_norm),                      &
     &                 r_at_u, r_at_v,                                  &
     &                 mype, nproc, nproc_x, nproc_y,                   &
     &                 offx, offy, halo_i, halo_j, datastart,           &
     &                 g_i_pe, g_j_pe, l_2dcomm, size_2dcomm,           &
     &                 group_2dcomm, max_comm_size,at_extremity,        &
     &                 global_row_length, global_rows,                  &
     &                 gc_proc_row_group, gc_proc_col_group,            &
     &                 gc_all_proc_group,                               &
     &                 Depart_scheme, Depart_order,                     &
     &                 high_order_scheme(Theta_SL),                     &
     &                 monotone_scheme(Theta_SL),                       &
     &                 high_order_scheme(moist_SL),                     &
     &                 monotone_scheme(moist_SL),                       &
     &                 L_Ritchie_high, L_Ritchie_mono,                  &
     &                 Ritchie_high_order_scheme,                       &
     &                 Ritchie_monotone_scheme,                         &
     &                 model_domain, L_high(Theta_SL),                  &
     &                 L_mono(Theta_SL), thmono_levels,                 &
     &                 L_thmono_fixed,                                  &
     &                 L_high(moist_SL), L_mono(moist_SL),              &
     &                 L_conserv(moist_SL), L_pc2,                      &
     &                 L_2d_sl_geometry, L_mix_ratio,                   &
     &                 L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,           &
     &                 L_regular, L_sl_halo_reprod,                     &
     &                 L_fint_theta, L_lbc_old,                         &
     &                 L_new_tdisc, CycleNo,                            &
     &                 depart_r_theta,                                  &
     &                 depart_lambda, depart_phi, depart_r_w,           &
     &                 ErrorStatus )
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AA SL_Thermo',6)

        If (.not. L_moist_nonhydro_conserve .and.                        &
     &      .not. L_tracer .and. CycleNo == NumCycles ) Then
          DEALLOCATE (depart_r_theta)
        endif

      End If        !     ErrorStatus  ==  0

! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS S-L Advect (AA)',6)
! ---------------------------------------------------------------
!    diagnostic printing of l2norms
      if( L_print_L2norms ) then
        IF( L_print_pe .or. mype ==0 ) then
          write(6,*)' ***   L2 norms after sl_thermo  ***'
        ENDIF ! L_print_pe .or. mype ==0
        CALL prfldincnorm(.FALSE.)
      endif !  L_print_L2norms

! ----------------------------------------------------------------------
! Section 2.1 Calculate advective Momentum increments.
! ----------------------------------------------------------------------
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS S-L Advect (AA)',5)
      If (ErrorStatus  ==  0 ) Then

! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AA SL_Full_Wind',5)
! DEPENDS ON: ni_sl_full_wind
        Call NI_SL_Full_wind(                                            &
     &                      u, u_np1, v, v_np1,                          &
     &                      w, w_np1,                                    &
     &                      u_adv, v_adv, w_adv,                         &
     &                      theta, theta_np1,                            &
     &                      exner_rho_levels,                            &
     &                      q, qcl, qcf,                                 &
     &                      qcf2, qrain, qgraup,                         &
     &                      q_np1, qcl_np1, qcf_np1, qcf2_np1,           &
     &                      qrain_np1, qgraup_np1,                       &
     &                      mix_v, mix_cl, mix_cf,                       &
     &                      mix_v_np1, mix_cl_np1, mix_cf_np1,           &
     &                      mix_cf2, mix_rain, mix_graup,                &
     &                      mix_cf2_np1, mix_rain_np1, mix_graup_np1,    &
     &                      L_mix_ratio,                                 &
     &                      L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,       &
     &                      depart_lambda, depart_phi, depart_r_w,       &
     &                      r_theta_levels, r_rho_levels,                &
     &                      eta_theta_levels, eta_rho_levels,            &
     &                      cos_theta_latitude, sec_theta_latitude,      &
     &                      sin_theta_latitude, cos_v_latitude,          &
     &                      sec_v_latitude, sin_v_latitude,              &
     &                      tan_theta_latitude, tan_v_latitude,          &
     &                      cos_theta_longitude,                         &
     &                      sin_theta_longitude,                         &
     &                      f1_at_v, f2_at_u, f3_at_u, f3_at_v,          &
     &                      delta_lambda, delta_phi, timestep,           &
     &                      glambda_p, phi_p, glambda_u, phi_v,          &
     &                      gdlambda_p, dphi_p, gdlambda_u, dphi_v,      &
     &            grecip_dlamp, recip_dphip, grecip_dlamu, recip_dphiv,  &
     &            wt_lambda_p, wt_phi_p, wt_lambda_u, wt_phi_v,          &
     &            lambda_p_rm, lambda_p_rp, lambda_u_rm, lambda_u_rp,    &
     &            phi_p_rm, phi_p_rp, phi_v_rm, phi_v_rp,                &
     &            recip_lambda_p_m, recip_lambda_p_0, recip_lambda_p_p,  &
     &            recip_lambda_p_p2, recip_lambda_u_m, recip_lambda_u_0, &
     &            recip_lambda_u_p, recip_lambda_u_p2, recip_phi_p_m,    &
     &            recip_phi_p_0, recip_phi_p_p, recip_phi_p_p2,          &
     &            recip_phi_v_m, recip_phi_v_0, recip_phi_v_p,           &
     &            recip_phi_v_p2, base_lambda, base_phi, lambda_p_end,   &
     &            phi_p_end, dlambda_p_end, dphi_p_end, dphi_v_end,      &
     &            recip_dlam, recip_dphi, max_look,                      &
     &            look_lam, look_phi, halo_lam, halo_phi,                &
     &                      alpha_3, alpha_4, LAM_max_cfl,               &
     &                      n_Y_arrays, n_Yw_arrays,                     &
     &                      n_Yd_arrays, n_Ydw_arrays,                   &
     &                      U_LBC,V_LBC,W_LBC,                           &
     &                      LENRIMA(1,1,rima_type_norm),                 &
     &                      LBC_SIZEA(1,1,1,rima_type_norm),             &
     &                      LBC_STARTA(1,1,1,rima_type_norm),            &
     &                      RIMWIDTHA(rima_type_norm),                   &
     &                      model_domain, row_length, rows, n_rows,      &
     &                      model_levels, wet_levels,                    &
     &                      Depart_scheme, Depart_order,                 &
     &                      high_order_scheme(Wind_SL),                  &
     &                      monotone_scheme(Wind_SL),                    &
     &                      L_trivial_trigs,                             &
     &                      L_high(Wind_SL), L_mono(Wind_SL),            &
     &                      L_conserv(Wind_SL),                          &
     &                      L_Ritchie_high,                              &
     &                      L_Ritchie_mono,                              &
     &                      Ritchie_high_order_scheme,                   &
     &                      Ritchie_monotone_scheme,                     &
     &                      first_constant_r_rho_level,                  &
     &                      check_bottom_levels,                         &
     &                      interp_vertical_search_tol,                  &
     &                      r_at_u, r_at_v,                              &
     &                      mype, nproc, nproc_x, nproc_y,               &
     &                      offx, offy, halo_i, halo_j,                  &
     &                      global_row_length, global_rows,              &
     &                      datastart, at_extremity, g_i_pe, g_j_pe,     &
     &                      l_2dcomm, size_2dcomm, group_2dcomm,         &
     &                      gc_proc_row_group, gc_proc_col_group,        &
     &                      gc_all_proc_group,                           &
     &                      L_2d_sl_geometry, L_sl_halo_reprod,          &
     &                      L_free_slip, L_regular,                      &
     &                      L_qwaterload, L_interp_depart,               &
     &                      L_lbc_old, L_new_tdisc, CycleNo,             &
     &                      R_u, R_v, R_w, ErrorStatus )
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AA SL_Full_Wind',6)

! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS S-L Advect (AA)',6)
! ---------------------------------------------------------------------
!    diagnostic printing of l2norms
      if( L_print_L2norms ) then
        IF( L_print_pe .or. mype ==0 ) then
          write(6,*)' ***   L2 norms after SL_Full_wind  ***'
        ENDIF ! L_print_pe .or. mype ==0
        CALL prfldincnorm(.FALSE.)
      endif !  L_print_L2norms
! ---------------------------------------------------------------

        IF ( .Not.SF(0,12) .and. CycleNo == NumCycles ) Then
          DEALLOCATE (depart_r_w)
          IF (.not. L_moist_nonhydro_conserve .and. .not. L_tracer ) THEN
            DEALLOCATE (depart_phi)
            DEALLOCATE (depart_lambda)
          END IF
        END IF

      End If         !  ErrorStatus  ==  0

      IF(L_mix_ratio)then
!  convert mix_v, mix_cl,mix_cf to q, qcl,qcf

! DEPENDS ON: mix_to_q
        call mix_to_q   (row_length, rows, wet_levels,                  &
     &                   offx, offy,                                    &
     &                   mix_v_star, mix_cl_star, mix_cf_star,          &
     &                   mix_cf2_star, mix_rain_star, mix_graup_star,   &
     &                   L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,         &
     &                   q_star, qcl_star, qcf_star,                    &
     &                   qcf2_star, qrain_star, qgraup_star )

      END IF         !     L_mix_ratio

      If ( L_tracer .and. CycleNo == NumCycles ) Then
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS S-L Advect (AA)',5)

! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AA SL_Tracer',5)
! DEPENDS ON: sl_tracer1 
        Call SL_tracer1(super_array_size, eta_theta_levels,             &
     &                 r_rho_levels, r_theta_levels,                    &
     &                 PSTAR, P, P_THETA_LEVELS,                        &
     &                 RHO, L_tracer1_non_hydro,                        &
     &                 row_length, rows, n_rows, model_levels,          &
     &                 delta_lambda, delta_phi,                         &
     &                 glambda_p, phi_p, gdlambda_u, dphi_v,            &
     &                 grecip_dlamp, recip_dphip,                       &
     &                 lambda_p_rm, lambda_p_rp, phi_p_rm, phi_p_rp,    &
     &                 recip_lambda_p_m, recip_lambda_p_0,              &
     &                 recip_lambda_p_p, recip_lambda_p_p2,             &
     &                 recip_phi_p_m, recip_phi_p_0,                    &
     &                 recip_phi_p_p, recip_phi_p_p2,                   &
     &                 base_lambda, base_phi,                           &
     &                 recip_dlam, recip_dphi, max_look,                &
     &                 look_lam, look_phi, halo_lam, halo_phi,          &
     &                 FV_cos_theta_latitude,                           &
     &                 mype, nproc, nproc_x, nproc_y,                   &
     &                 halo_i, halo_j, datastart,                       &
     &                 g_i_pe, g_j_pe, l_2dcomm, size_2dcomm,           &
     &                 group_2dcomm, at_extremity,                      &
     &                 global_row_length, global_rows,                  &
     &                 gc_proc_row_group, gc_proc_col_group,            &
     &                 gc_all_proc_group, offx, offy,                   &
     &                 L_regular, L_sl_halo_reprod,                     &
     &                 high_order_scheme(moist_SL),                     &
     &                 monotone_scheme(moist_SL),                       &
     &                 model_domain, L_high(moist_SL),                  &
     &                 L_mono(moist_SL),                                &
     &                 L_conserve_tracers,                              &
     &                 check_bottom_levels,                             &
     &                 interp_vertical_search_tol,                      &
     &                 first_constant_r_rho_level,                      &
     &                 depart_lambda, depart_phi, depart_r_theta,       &
!  d1(jxxx) holds time level n value plus physics1 increment
     &                 CO2, L_CO2_interactive,                           &
     &                 MURK, L_Murk_advect,                              &
     &                 DUST_DIV1,DUST_DIV2,                              &
     &                 DUST_DIV3,DUST_DIV4,                              &
     &                 DUST_DIV5,DUST_DIV6, L_DUST,                      &
     &                 SOOT_NEW, SOOT_AGD,                               &
     &                 SOOT_CLD, L_soot,                                 &
     &                 BMASS_NEW, BMASS_AGD,                             &
     &                 BMASS_CLD, L_biomass,                             &
     &                 OCFF_NEW, OCFF_AGD, OCFF_CLD, L_OCFF,             &
     &                 SO2, SO4_AITKEN,                                  &
     &                 SO4_ACCU,                                         &
     &                 SO4_DISS, NH3, DMS,                               &
     &                 L_sulpc_so2, L_sulpc_nh3, l_sulpc_dms,            &
     &                 nitr_acc, nitr_diss, L_nitrate,                   &
     &                 TRACER, tr_levels, tr_vars,                       &
     &                 TR_LBC_VARS,TRACER_LBC,                           &
     &                 A_MAX_TRVARS,A_TR_ACTIVE_LBC_INDEX,               &
     &                 tracer_ukca, tr_ukca,                             &
     &                 L_USE_CARIOLLE, OZONE_TRACER,                     &
     &                 TR_LBC_UKCA,TRACER_UKCA_LBC,                      &
     &                 A_MAX_UKCAVARS,UKCA_TR_ACTIVE_LBC_INDEX,          &
     &                 RIMWIDTHA(rima_type_norm),RIMWEIGHTSA,            &
     &                 LENRIMA(fld_type_p,halo_type_extended,            &
     &                         rima_type_norm),                          &
     &                 LBC_SIZEA(1,fld_type_p,halo_type_extended,        &
     &                           rima_type_norm),                        &
     &                 LBC_STARTA(1,fld_type_p,halo_type_extended,       &
     &                            rima_type_norm),                       &
     &                 ErrorStatus)
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AA SL_Tracer',6)
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS S-L Advect (AA)',6)

! ----------------------------------------------------------------------
!    diagnostic printing of l2norms
      if( L_print_L2norms ) then
        IF( L_print_pe .or. mype ==0 ) then
          write(6,*)' ***   L2 norms after SL_tracer1  ***'
        ENDIF ! L_print_pe .or. mype ==0
        CALL prfldincnorm(.FALSE.)
      endif !  L_print_L2norms
! ---------------------------------------------------------------

      end if  ! L_tracer

! ----------------------------------------------------------------------
! Section 2.21 Calculation of coefficients in turbulence scheme
! ----------------------------------------------------------------------

! DEPENDS ON: Atm_Step_phys_init
        CALL Atm_Step_phys_init( &
#include "arg_atm_fields.h"
#include "argsts.h"
          r_v, r_u, theta_star, q_star, qcl_star, qcf_star,  &
          cf_star, cfl_star, cff_star, rho_km, cH, exner_lbc_real_tend, &
          w_lbc_real_tend, errorstatus, 'turb_cof')

      IF (L_subfilter_horiz .OR. L_subfilter_vert) THEN

! Calculate  lambda^2*S in TURB_Smagorinsky
        If (L_regular) then  ! regular grid
! DEPENDS ON: turb_smagorinsky
          CALL turb_Smagorinsky(                                        &
                                u, v, w, z0, timestep,                  &
                                row_length, rows, n_rows,               &
                                model_levels, bl_levels,                &
                                r_theta_levels, r_rho_levels,           &
                                cos_theta_latitude, cos_v_latitude,     &
                                delta_lambda, delta_phi )
        Else ! variable resolution
! DEPENDS ON: turb_smagorinsky_varres
          CALL turb_Smagorinsky_varres(                                 &
                                        u, v, w, z0, timestep,          &
                                        row_length, rows, n_rows,       &
                                        model_levels, bl_levels,        &
                                        r_theta_levels, r_rho_levels,   &
                                        cos_theta_latitude,             &
                                        cos_v_latitude,                 &
                                        glambda_p(lambda_start), phi_p, &
                                        glambda_u(lambda_start), phi_v, &
                                        gdlambda_p(lambda_start),       &
                                        dphi_p,                         &
                                        gdlambda_u(lambda_start),       &
                                        dphi_v,                         &
                                        grecip_dlamp(lambda_start),     &
                                        recip_dphip,                    &
                                        grecip_dlamu(lambda_start),     &
                                        recip_dphiv ) 
        End If

      END IF  !  L_subfilter_horiz OR L_subfilter_vert

! ---------------------------------------------------------------
! Section 2.3 Diagnostics at end of advection
! ----------------------------------------------------------------------
! Apply diagnostics at final cycle only
      If ( CycleNo == NumCycles ) Then

! section 12: 'dynamics advection' based quantities
      IF(      SF(0,12)                                                  &
                                ! Diagnostics required for this section
     &   .AND. ErrorStatus == 0) THEN

! Allocate diagnostic space for STASH
        ALLOCATE (STASHwork12(STASH_maxlen(12,A_im)))


        CALL Diagnostics_adv(                                            &
     &            row_length, rows, n_rows, model_levels, wet_levels,    &
! primary wind fields:
     &            U, V,                                                  &
     &            THETA, Q, QCL, QCF, qrain, qgraup, qcf2,               &
     &            CF_BULK, CF_LIQUID, CF_FROZEN,                         &
! wind field increments after advection:
     &            R_u, R_v, R_w,                                         &
! wind field increments before advection (on stashflag):
     &            u_incr_diagnostic, v_incr_diagnostic,                  &
     &            T_incr_diagnostic, q_incr_diagnostic,                  &
     &            qcl_incr_diagnostic, qcf_incr_diagnostic,              &
                  qrain_incr_diagnostic, qgraup_incr_diagnostic,         &
                  qcf2_incr_diagnostic,                                  &
     &            cf_incr_diagnostic, cfl_incr_diagnostic,               &
     &            cff_incr_diagnostic,                                   &
     &            theta_star, q_star, qcl_star, qcf_star,                &
                  qrain_star, qgraup_star, qcf2_star,                    &
     &            cf_star, cfl_star, cff_star,                           &
     &            EXNER_THETA_LEVELS,                                    &
! Departure points for w
     &            depart_lambda, depart_phi, depart_r_w,                 &
     &            r_theta_levels,                                        &
#include "argsts.h"
     &            STASHwork12)

        DEALLOCATE (depart_r_w)
        If (.not. L_moist_nonhydro_conserve .and.                        &
     &      .not. L_tracer )then
          DEALLOCATE (depart_phi)
          DEALLOCATE (depart_lambda)
        endif

! DEPENDS ON: Atm_Step_diag
        CALL Atm_Step_diag( &
#include "arg_atm_fields.h"
#include "argsts.h"
             r_v, r_u, r_w, q_star, qcl_star, qcf_star,  theta_star, &
             cf_star, cfl_star, cff_star, 3)
        
! DEPENDS ON: stash
        CALL STASH(a_sm,a_im,12,STASHwork12,                             &
#include "argd1.h"
#include "argdumga.h"
#include "argsts.h"
     &    ErrorStatus,Cmessage)

        DEALLOCATE (STASHwork12)

      ENDIF !   SF(0,12)
! ----------------------------------------------------------------------

      End If ! CycleNo == NumCycles


! ----------------------------------------------------------------------
! Section 3.0  Call Atmospheric Physics2
! ----------------------------------------------------------------------

      If (.NOT. L_run_with_physics2) Then
        L_physics_store=l_physics
        L_physics=.false.
      end if

      If (ErrorStatus  ==  0 ) Then

        If (L_idealised_data) Then

          If ( CycleNo == 1 ) Then

            ALLOCATE (q_inc_subs (qdims2%i_start:qdims2%i_end,           &
                                  qdims2%j_start:qdims2%j_end,           &
                                  qdims2%k_start:qdims2%k_end))
            ALLOCATE (th_inc_subs(tdims%i_start:tdims%i_end,             &
                                  tdims%j_start:tdims%j_end,             &
                                  tdims%k_start:tdims%k_end ))
            ALLOCATE (q_inc_ls   (qdims2%i_start:qdims2%i_end,           &
                                  qdims2%j_start:qdims2%j_end,           &
                                  qdims2%k_start:qdims2%k_end))
            ALLOCATE (th_inc_ls  (tdims%i_start:tdims%i_end,             &
                                  tdims%j_start:tdims%j_end,             &
                                  tdims%k_start:tdims%k_end))
            ALLOCATE (u_inc_dmp  (udims%i_start:udims%i_end,             &
                                  udims%j_start:udims%j_end,             &
                                  udims%k_start:udims%k_end))
            ALLOCATE (q_inc_dmp  (qdims2%i_start:qdims2%i_end,           &
                                  qdims2%j_start:qdims2%j_end,           &
                                  qdims2%k_start:qdims2%k_end))
            ALLOCATE (th_inc_dmp (tdims%i_start:tdims%i_end,             &
                                  tdims%j_start:tdims%j_end,             &
                                  tdims%k_start:tdims%k_end))
            ALLOCATE (v_inc_dmp  (vdims%i_start:vdims%i_end,             &
                                  vdims%j_start:vdims%j_end,             &
                                  vdims%k_start:vdims%k_end))

          End If
! include idealised forcing for theta.

! DEPENDS ON: idl_force
          CALL IDL_Force(                                               &
     &                   row_length, rows, model_levels, timestep       &
     &,                  delta_lambda, delta_phi, model_domain          &
     &,                  lambda_half_width, phi_half_width              &
     &,                  p_max, p_top, p_bottom, p_arbitrary            &
     &,                  lambda_heat_centre, phi_heat_centre            &
     &,                  max_heat_per_day, newtonian_timescale          &
! Dynamical core settings
     &,                  SuHe_newtonian_timescale_ka                     &
     &,                  SuHe_newtonian_timescale_ks                     &
     &,                  SuHe_pole_equ_deltaT, SuHe_static_stab          &
     &,                  SuHe_level_weight, SuHe_sigma_cutoff            &
     &,                  L_SH_Williamson, SuHE_relax                     &
     &,                  L_damp, L_geo_for                               &
     &,                  L_bomex                                         &
     &,                  DMPTIM, HDMP, ZDMP                              &
     &,                  u_geo, v_geo                                    &
     &,                  offx, offy, datastart, at_extremity             &
     &,                  EXNER_THETA_LEVELS                              &
     &,                  P, theta_star                                   &
     &,                  P_THETA_LEVELS, PSTAR                           &
     &,                  THETA                                           &
     &,                  cos_theta_latitude,sin_theta_latitude           &
     &,                  cool_rate, theta_surface                        &
     &,                  timestep_number                                 &
     &,                  model_levels_max, max_num_force_times           &
     &,                  Q, q_star                                       &
     &,                  U, V, R_u, R_v                                  &
     &,                  u_ref, v_ref, theta_ref, n_rows                 &
     &,                  q_ref                                           &
     &,                  eta_theta_levels, eta_rho_levels                &
     &,                  height_domain                                   &
     &,                  global_row_length, global_rows                  &
     &,                  gc_all_proc_group, nproc                        &
     &,                  tforce_option, qforce_option, uvforce_option    &
     &,                  num_tforce_times, num_qforce_times              &
     &,                  num_uvforce_times                               &
     &,                  tforce_time_interval, qforce_time_interval      &
     &,                  uvforce_time_interval                           &
     &,                  tforce_data_modlev, qforce_data_modlev          &
     &,                  uforce_data_modlev, vforce_data_modlev          &
     &,                  r_rho_levels, r_theta_levels                    &
     &,                  halo_i, halo_j                                  &
     &,                  q_inc_subs, th_inc_subs                         &
     &,                  q_inc_ls, th_inc_ls                             &
     &,                  u_inc_dmp, q_inc_dmp, th_inc_dmp                &
     &,                  v_inc_dmp                                       &
     &,                  f3_at_u, f3_at_v                                &
     &,                  L_physics, problem_number, L_force)


          If ( CycleNo == NumCycles ) Then
            DEALLOCATE (q_inc_subs)
            DEALLOCATE (th_inc_subs)
            DEALLOCATE (q_inc_ls)
            DEALLOCATE (th_inc_ls)
            DEALLOCATE (u_inc_dmp)
            DEALLOCATE (q_inc_dmp)
            DEALLOCATE (th_inc_dmp)
            DEALLOCATE (v_inc_dmp)
          End If

        End If      !  L_idealised_data

        If (L_tracer) then

! protect from multiple mem allocations
! save input fields to obtain Atmos_Physics2 increments

! tracers only at final cycle
          If ( CycleNo == NumCycles ) Then
! DEPENDS ON: tr_set_phys 
            call TR_Set_Phys(super_array_size, super_tracer_phys2,      &
                         L_CO2_interactive, CO2, L_Murk_advect, murk,   &
                         L_Soot, soot_new, soot_agd,soot_cld,           &
                         L_SULPC_SO2, SO2,SO4_aitken,so4_accu,so4_diss, &
                         L_sulpc_nh3, nh3,L_sulpc_dms, dms,             &
                         L_dust, DUST_DIV1,DUST_DIV2,DUST_DIV3,         &
                                 DUST_DIV4,DUST_DIV5,DUST_DIV6,         &
                         L_biomass, bmass_new, bmass_agd, bmass_cld,    &
                         L_ocff, ocff_new, ocff_agd, ocff_cld,          &
                         L_nitrate, nitr_acc, nitr_diss,                &
                         L_USE_CARIOLLE, OZONE_TRACER,                  &
                         tracer_phys1, tracer,                          &
                         ukca_tracer_phys1, tracer_ukca,                &
                         row_length, rows,                              &
                         model_levels, tr_levels, tr_vars, tr_ukca,     &
                         offx, offy, model_domain,                      &
                         .false., tdims )
          End If ! CycleNo == NumCycles

        end if  ! L_tracer

! Save star fields to obtain increments after call to Atmos_Physics2

! This call is allocating mix_*_phys2 arrays based on l_mix_ratio switch for
! dynamics. It assumes *_star are in specific humidities and converts these to
! mixing ratios and stores in mix_*_phys2
! If not l_mix_ratio allocating *_phys2 arrays and copying *_star to *_phys2 

! DEPENDS ON: Atm_Step_alloc
        CALL Atm_Step_alloc( &
#include "arg_atm_fields.h"
               cf_star, cfl_star, cff_star, q_star, qcl_star, qcf_star, &
               frac_control, r_u, r_v, r_w, errorstatus, 'phy2star')

        IF (L_Physics) THEN
! DEPENDS ON: timer
      IF (Ltimer) CALL TIMER('AS Atmos_Phys2 (AP2)',5)

! On cycleno 1 this call is allocating mix_*_phys2 arrays if .NOT.L_mix_ratio
! Converts *_star fields to mixing ratio held in mix_*_phys2 fields

! DEPENDS ON: Atm_Step_alloc
          CALL Atm_Step_alloc( &
#include "arg_atm_fields.h"
             cf_star, cfl_star, cff_star, q_star, qcl_star, qcf_star, &
             frac_control, r_u, r_v, r_w, errorstatus, 'q_to_mix' ) 

! NB: the star variables and R_u and R_v have not been set in the
!     halo region yet.

! DEPENDS ON: Atm_Step_diag
          CALL Atm_Step_diag( &
#include "arg_atm_fields.h"
#include "argsts.h"
             r_v, r_u, r_w, q_star, qcl_star, qcf_star,  theta_star, &
             cf_star, cfl_star, cff_star, 4)

          IF (l_mr_physics2) THEN
            DO k = qdims%k_start, qdims%k_end
              DO j = qdims%j_start, qdims%j_end
                DO i = qdims%i_start, qdims%i_end
                  q(i,j,k)   = mix_v(i,j,k)
                  qcl(i,j,k) = mix_cl(i,j,k)
                  qcf(i,j,k) = mix_cf(i,j,k)
                  q_star(i,j,k)   = mix_v_phys2(i,j,k)
                  qcl_star(i,j,k) = mix_cl_phys2(i,j,k)
                  qcf_star(i,j,k) = mix_cf_phys2(i,j,k)
                END DO
              END DO
            END DO
          END IF

! NB if you are changing the argument list to atmos_physics2, please
! do an equivalent change in routine scm_main to keep the single column
! model consistent. Note there are two calls to atmos_physics2 in scm_main.

! DEPENDS ON: atmos_physics2
          CALL Atmos_Physics2(                                           &
! Parallel variables
        global_row_length, global_rows, nproc, nproc_x, nproc_y          &
      , g_rows, g_row_length, NumCycles, CycleNo                         &
! field dimensions etc.
      ,     row_length, rows, n_rows, land_field, model_levels, nice     &
      ,     nice_use                                                     &
      ,     wet_levels, bl_levels, st_levels, sm_levels, cloud_levels    &
      ,     land_ice_points, soil_points, n_cca_lev, ntiles, tr_levels   &
      ,     first_constant_r_rho_level, DIM_CS1, DIM_CS2                 &
! IN Substepping information and switches
      ,     L_regular, l_mr_physics2                                     &
      ,     L_dry, L_lbc_old, LCAL360, Ltimer                            &
      ,     L_CLD_AREA                                                   &
! Model Parameters
      ,     RHcrit, CO2_MMR, tr_vars, tr_ukca                            &
! Vertical coordinate levels.
      ,     unscaled_dry_rho                                             &
      ,     delta_lambda, delta_phi                                      &
      ,     gdlambda_p(lambda_start), dphi_p, wt_lambda_p, wt_lambda_u   & 
      ,     wt_phi_p, wt_phi_v, lat_rot_NP, long_rot_NP, f3_at_u         &
! Time stepping information
      , I_year, I_day_number, I_hour, I_minute, I_second,                &
! River routing
        AOCPL_ROW_LENGTH,AOCPL_P_ROWS,XPA,XUA,XVA,YPA,YUA,YVA,           &
        G_P_FIELD,G_R_FIELD,A_INTHD(16),lasize(1,fld_type_r,             &
        halo_type_no_halo),lasize(2,fld_type_r,halo_type_no_halo),       &
        glsize(1,fld_type_r),glsize(2,fld_type_r),RIVER_VEL,             &
        RIVER_MCOEF,I_RIVER_VN,RIV_DIRECTION,RIV_SEQUENCE,RIV_STORAGE,   &
!  Add inland basin outflow to arguments
        RIV_INLANDATM,                                                   &
!  Add lake evaporation: 
        ACC_LAKE_EVAP,                                                   &
! Grid-to-grid river routing
        RIV_IAREA,RIV_SLOPE, RIV_FLOWOBS1,RIV_INEXT, RIV_JNEXT,RIV_LAND, &
        RIV_SUBSTORE, RIV_SURFSTORE,RIV_FLOWIN,RIV_BFLOWIN,              &

! diagnostic info
#include "argsts.h"
            STASHwork3,STASHwork5,STASHwork8,STASHwork9,STASHwork19,     &
            STASHwork26,                                                 &
! SCM Diagnostics (dummy values in full UM)
        nSCMDpkgs, L_SCMDiags,                                           &
! Data Fields.
       THETA, Q, QCL, QCF, qrain, qgraup, qcf2,                          &
       RHO, U, V, W, W_ADV, P ,PSTAR,                                    &
       EXNER_RHO_LEVELS,EXNER_THETA_LEVELS, LAND, P_THETA_LEVELS         &

! ancillary fields and fields needed to be kept from timestep to
! timestep
      ,land_index, land_ice_index, soil_index, CANOPY_WATER              &
      ,SNODEP, THERM_COND, THERM_CAP, VOL_SMC_CRIT                       &
      ,VOL_SMC_WILT, VOL_SMC_SAT, STHF, STHU                             &
      ,OROG_SIL,OROG_HO2,OROG_SD,ICE_THICKNESS,ICE_FRACTION              &
      ,U_SEA,V_SEA,U_0_P,V_0_P,CCA,CCB                                   &
      ,CCT, CCLWP, CCW_RAD, LCBASE, DEEP_SOIL_TEMP,p_ti,TI,ICE_K_CAT     &
      ,TSTAR,Z0, p_ice_fract, p_ice_thick                                &
      ,SAT_SOIL_COND,SAT_SOILW_SUCTION,CLAPP_HORN                        &
      ,SMCL, T1_SD, Q1_SD, ZH, ddmfx, CF_AREA, CF_BULK, CF_LIQUID        &
      ,CF_FROZEN, ls_rain, ls_snow, micro_tends                          &
      ,photosynth_act_rad, rad_hr, SOIL_CLAY                             &
      ,SOIL_SILT,SOIL_SAND, DUST_MREL1,DUST_MREL2                        &
      ,DUST_MREL3,DUST_MREL4,DUST_MREL5,DUST_MREL6                       &
      ,SO2_HILEM, SO2_EM, NH3_EM, DMS_EM,SOOT_HILEM, SOOT_EM, OCFF_HILEM &
      ,OCFF_EM, CO2_EMITS, CO2FLUX ,deep_flag, past_precip, past_conv_ht &

! IN/OUT
      , theta_star,q_star,qcl_star,qcf_star, qrain_star, qgraup_star     &
      , qcf2_star, cf_star,cfl_star,cff_star                             &
      , R_u, R_v, R_w, net_flux, net_mflux, murk,tracer, tracer_ukca     &
      , DUST_DIV1, DUST_DIV2, DUST_DIV3 ,DUST_DIV4, DUST_DIV5, DUST_DIV6 &
      , so2, dms, so4_aitken, so4_accu, so4_diss, nh3, soot_new          &
      , soot_agd, soot_cld, bmass_new, bmass_agd, bmass_cld, ocff_new    & 
      , ocff_agd, ocff_cld, nitr_acc, nitr_diss, co2                     &
! IN/OUT River routing
      , TOT_SURFROFF, TOT_SUBROFF                                        &
! OUT Fields
      , rho_km, cH, NTML, CUMULUS, NBDSC, NTDSC                          &
      , rhcpt, rhc_row_length, rhc_rows                                  &

! Additional variables for MOSES II
      , FRAC_TYP, DISTURB_VEG, CANHT_PFT, LAI_PFT                        &
      , CAN_WATER_TILE, CATCH_TILE, CATCH_SNOW                           &
      , SNOW_GRND, SNODEP_TILE, Z0_TILE, Z0H_TILE, TSTAR_TILE            &
      , INFIL_TILE, RGRAIN_TILE, CS, GS                                  &
      , co2_dim_row, co2_dim_len, A_INTHD(23)                            &
      , STEPim(atmos_im), G_LF_PFT_ACC, G_PHLF_PFT_ACC, NPP_PFT_ACC      &
      , RSP_W_PFT_ACC, RSA                                               &
      , land_pts_trif, npft_trif, dolr, LW_down, SW_tile                 &
      , FRAC_LAND,TSTAR_LAND,TSTAR_SEA,p_tstar_sice,TSTAR_SICE           &
      , SOIL_ALB,cos_zenith_angle                                        &
! INOUT variables for TKE based turbulence schemes 
      , e_trb, tsq_trb, qsq_trb, cov_trb, zhpar_shcu                     &
! Additional variables required for large-scale hydrology:
      , FEXP,GAMMA_INT,TI_MEAN,TI_SIG, FSFC_SAT,F_WETLAND                &
      , WATER_TABLE,STHZW, A_FSAT,C_FSAT,A_FWET,C_FWET                   &
! IN/OUT JULES 2 prognostics
      , SNOWDEPTH, RHO_SNOW_GRND, NSNOW                                  &
      , DS, SICE, SLIQ, TSNOWLAYER, RHO_SNOW, RGRAINL                    &
! IN/OUT FLake lake scheme prognostics
      , lake_depth, lake_fetch, lake_t_mean, lake_t_mxl                  &
      , lake_t_ice, lake_h_mxl, lake_h_ice,  lake_shape                  &
      , lake_g_dt                                                        &
! Cariolle ozone 
      , OZONE_TRACER                                                     &

! Additional screen-level variables
      , TScrnDcl_SSI, TScrnDcl_TILE, tStbTrans                           &

! Variables required for COSP (out)
      ,cosp_crain_3d, cosp_csnow_3d                                      &

! SCM and idealised UM surface forcing parameters
      ,L_flux_bc,flux_e,flux_h,L_spec_z0,z0m_scm,z0h_scm                 &

! error information
      ,                      ErrorStatus )

          IF (l_mr_physics2) THEN

          ! Copy output from atmos_physics2 into mix_* and mix_*_phys2
 
            DO k = qdims%k_start, qdims%k_end
              DO j = qdims%j_start, qdims%j_end
                DO i = qdims%i_start, qdims%i_end
                  mix_v(i,j,k)  = q(i,j,k)
                  mix_cl(i,j,k) = qcl(i,j,k)
                  mix_cf(i,j,k) = qcf(i,j,k)
                  mix_v_phys2(i,j,k)  = q_star(i,j,k) 
                  mix_cl_phys2(i,j,k) = qcl_star(i,j,k)
                  mix_cf_phys2(i,j,k) = qcf_star(i,j,k)
                END DO
              END DO
            END DO
           
            ! convert Mixing ratios back to specific humidities so that
            ! q,qcf etc and *_star arrays hold specific humidities
! DEPENDS ON: mix_to_q
            CALL mix_to_q(row_length, rows, wet_levels,                 &
                        offx, offy,                                     &
                        mix_v, mix_cl, mix_cf,                          &
                        mix_cf2, mix_rain, mix_graup,                   &
                        L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,          &
                        q, qcl, qcf, qcf2, qrain, qgraup )

! DEPENDS ON: mix_to_q
            CALL mix_to_q(row_length, rows, wet_levels,                 &
                        offx, offy,                                     &
                        mix_v_phys2, mix_cl_phys2, mix_cf_phys2,        &
                        mix_cf2_phys2, mix_rain_phys2, mix_graup_phys2, &
                        L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,          &
                        q_star, qcl_star, qcf_star,                     &
                        qcf2_star, qrain_star, qgraup_star )

          END IF  ! l_mr_physics2
          If(.NOT. (L_mix_ratio .and. L_moist_nonhydro_conserve)         &
     &            .and. CycleNo == NumCycles)then
            Deallocate(mix_graup_phys2)
            Deallocate(mix_rain_phys2)
            Deallocate(mix_cf2_phys2)
            Deallocate(mix_cf_phys2)
            Deallocate(mix_cl_phys2)
            Deallocate(mix_v_phys2)
          End If
! DEPENDS ON: timer
      IF (Ltimer) CALL TIMER('AS Atmos_Phys2 (AP2)',6)

! ---------------------------------------------------------------
!    diagnostic printing of l2norms
      if( L_print_L2norms ) then
        IF( L_print_pe .or. mype ==0 ) then
          write(6,*)' ***   L2 norms after Atmos_Physics2 ***'
        ENDIF ! L_print_pe .or. mype ==0
        CALL prfldincnorm(.FALSE.)
      endif !  L_print_L2norms
! ---------------------------------------------------------------

! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS STASH',5)
! DEPENDS ON: Atm_Step_stash
          CALL Atm_Step_stash( &
#include "argd1.h"
#include "argduma.h"
#include "arg_atm_fields.h"
#include "argsts.h"
               errorstatus, 2)
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS STASH',6)

!L --------- UM Section 35---- Stochastic Physics SKEB2 -------------
!
! Call SKEB2 after the physics have been completed
!
 IF (PrintStatus >=  PrStatus_Diag .and. first_atmstep_call) THEN
   WRITE(6,*) 'ATM_STEP: L_SKEB2', l_skeb2
 ENDIF
 IF (l_skeb2) THEN 
! DEPENDS ON: timer
      IF (Ltimer) CALL TIMER('AS Stochastic_Phys',5)

   IF (sf(0,35)) THEN
     ! ALLOCATE diagnostic space for STASH
     ALLOCATE (stashwork35(stash_maxlen(35,a_im)))
   ELSE
     ! Code to ALLOCATE unity arrays when not used
     ALLOCATE(stashwork35(1))
     stashwork35(1) = 0.0
   END IF

   CALL stph_skeb2(                                                     &
! in
           row_length, rows, n_rows, model_levels,                      &
           delta_phi, delta_lambda,                                     &
           rho, u, v,                                                   &
! in/out
           r_u, r_v,                                                    &
! STASH array sf
#include "argsts.h"
           stashwork35, first_atmstep_call)

! Send diagnostics to STASH
   IF (sf(0,35) .and. errorstatus == 0) THEN

     ! DEPENDS ON: stash
     CALL stash( a_sm, a_im, 35, stashwork35,                           &
#include "argd1.h"
#include "argdumga.h"
#include "argsts.h"
          errorstatus, cmessage)

   ENDIF !(sf(0,35))  

   DEALLOCATE (stashwork35)

! DEPENDS ON: timer
      IF (Ltimer) CALL TIMER('AS Stochastic_Phys',6)
 ENDIF  !(l_skeb2)

! -------- AC ASSIMILATION SECTION -------------
! DEPENDS ON: timer
          IF (LTimer) CALL TIMER('AS Assimilation',5)

! DEPENDS ON: Atm_Step_ac_assim
          CALL Atm_Step_ac_assim( &
#include "argd1.h"
#include "argduma.h"
#include "arg_atm_fields.h"
#include "argsts.h"
            obs_flag_len, obs_len, obs_flag, obs, rho_km, cH,  &
            q_star, qcl_star, qcf_star, theta_star,            &
            errorstatus)

! DEPENDS ON: timer
          IF (LTimer) CALL TIMER('AS Assimilation',6)

        ELSE    ! L_physics .false.

! -------- FRICTION SECTION -------------------

! DEPENDS ON: Atm_Step_phys_init
         CALL Atm_Step_phys_init( &
#include "arg_atm_fields.h"
#include "argsts.h"
           r_v, r_u, theta_star, q_star, qcl_star, qcf_star,  &
           cf_star, cfl_star, cff_star, rho_km, cH, exner_lbc_real_tend, &
           w_lbc_real_tend, errorstatus, 'friction')

          If (.NOT. L_run_with_physics2 ) Then
            L_physics=L_physics_store
          End If

        END IF !   ! L_physics

! remove any instability
        If (L_adjust_wet .or.L_dry .or.                                 &
     &      ( .not. L_physics .and.                                     &
     &        (.not. L_idealised_data .or. problem_number  ==  1        &
     &        .or. problem_number  ==  2) ) ) Then

! DEPENDS ON: dry_static_adj
          Call dry_static_adj( theta_star, q_star,                      &
     &                       rows, row_length, model_levels,            &
     &                       wet_levels, offx, offy,                    &
     &                       Instability_diagnostics,.false.)

        End If   ! L_adjust_wet .or. L_dry ....

      End If   ! ErrorStatus = 0

! DEPENDS ON: timer
      IF (Ltimer) CALL TIMER('AS Diffusion',5)

      If (ErrorStatus  ==  0 ) Then

! -------- DIFFUSION SECTION ------------------ 

        If ( L_diff_ctl ) then

! DEPENDS ON: Atm_Step_diag
          CALL Atm_Step_diag( &
#include "arg_atm_fields.h"
#include "argsts.h"
               r_v, r_u, r_w, q_star, qcl_star, qcf_star,  theta_star, &
               cf_star, cfl_star, cff_star, 5)

! DEPENDS ON: ni_diff_ctl
          CALL ni_diff_ctl(                                             &
                           L_diffusion, L_cdiffusion,                   &
     &                     L_vertical_diffusion, L_divdamp,             &
     &                     L_ramp, ramp_lat_radians,                    &
     &                     L_Backwards, Ltimer,                         &
     &                     timestep, pos_timestep, neg_timestep,        &
     &                     THETA, W, Q,                                 &
     &                     QCL, QCF,                                    &
     &                     QCF2, QRAIN, QGRAUP,                         &
     &                     mix_v, mix_cl, mix_cf,                       &
     &                     mix_cf2, mix_rain, mix_graup,                &
     &                     L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,       &
     &                     U, V, RHO,                                   &
     &                     EXNER_RHO_LEVELS, W_ADV,                     &
     &                     r_theta_levels, r_rho_levels, r_at_u, r_at_v,&
     &                     eta_theta_levels, eta_rho_levels,            &
     &                     sec_theta_latitude,                          &
     &                     cos_theta_latitude, sec_v_latitude,          &
     &                     FV_sec_theta_latitude, cos_v_latitude,       &
     &                     sin_theta_latitude, sin_v_latitude,          &
     &                     offx, offy, halo_i, halo_j,                  &
     &                     at_extremity, gc_proc_row_group,             &
     &                     mype, nproc, nproc_x, nproc_y, neighbour,    &
     &                     delta_lambda, delta_phi, L_regular,          &
     &                     glambda_p(lambda_start), phi_p,              &
     &                     glambda_u(lambda_start), phi_v,              &
     &                     grecip_dlamp(lambda_start), recip_dphip,     &
     &                     grecip_dlamu(lambda_start), recip_dphiv,     &
     &                     rows, row_length, n_rows,                    &
     &                     model_levels, bl_levels, model_domain,      &
     &                     global_row_length, global_rows,              &
     &                     diffusion_coefficient_thermo,                &
     &                     diffusion_coefficient_w,                     &
     &                     diffusion_coefficient_q,                     &
     &                     diffusion_coefficient_wind,                  &
     &                     diffusion_order_thermo,                      &
     &                     diffusion_order_w,                           &
     &                     diffusion_order_q,                           &
     &                     diffusion_order_wind,                        &
     &                     horizontal_level, tar_horizontal,            &
     &                     level_start_wind, level_stop_wind,           &
     &                     level_start_q, level_stop_q,                 &
     &                     level_start_theta, level_stop_theta,         &
     &                     L_tardiff_q, w_conv_limit, tardiffq_factor,  &
     &                     tardiffq_test, tardiffq_start, tardiffq_end, &
     &                     sf(201,13), w_local_mask,                    &
     &                     L_adjust_theta,                              &
     &                     adjust_theta_start, adjust_theta_end,        &
     &                     L_vdiff_uv, vdiffuv_test,                    &
     &                     vdiffuv_factor, vdiffuv_start, vdiffuv_end,  &
     &                     vert_diffusion_coeff_wind,                   &
     &                     vert_diffusion_coeff_q,                      &
     &                     vert_diffusion_coeff_theta,                  &
     &                     div_damp_coefficient,                        &
     &                     theta_star, R_w, q_star,                     &
     &                     qcl_star, qcf_star,                          &
     &                     qcf2_star, qrain_star, qgraup_star,          &
     &                     R_u, R_v, L_mix_ratio)

! ----------------------------------------------------------------------
! Section 13.1 Diagnostics at from diffusion and divergence damping
! ----------------------------------------------------------------------
! Apply diagnostics only at last cycle
        If ( CycleNo == NumCycles ) Then

! section 13:
          IF( SF(0,13) .AND. ErrorStatus == 0) THEN

! Allocate diagnostic space for STASH
            IF(.NOT. L_Filter) ALLOCATE (STASHwork13(STASH_maxlen(13,A_im)))

! DEPENDS ON: diagnostics_dif
            CALL Diagnostics_dif(                                        &
     &         row_length, rows, n_rows, model_levels, bl_levels,        &
! primary  fields:
     &         THETA, Q,                                                 &
! wind field increments after  dif :
     &         R_u, R_v,                                                 &
! wind field increments before diffusion (on stashflag):
     &         u_incr_diagnostic, v_incr_diagnostic,                     &
     & T_incr_diagnostic,q_incr_diagnostic,                              &
     &         w_local_mask,                                             &
     &         theta_star, q_star,                                       &
     &         EXNER_THETA_LEVELS,                                       &
#include "argsts.h"
     &         STASHwork13)

! DEPENDS ON: Atm_Step_diag
            CALL Atm_Step_diag( &
#include "arg_atm_fields.h"
#include "argsts.h"
               r_v, r_u, r_w, q_star, qcl_star, qcf_star, theta_star, &
               cf_star, cfl_star, cff_star, 6)

! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS STASH',5)
! DEPENDS ON: Atm_Step_stash
            CALL Atm_Step_stash( &
#include "argd1.h"
#include "argduma.h"
#include "arg_atm_fields.h"
#include "argsts.h"
               errorstatus, 3)
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS STASH',6)

          ENDIF !   SF(0,13)

          End If ! CycleNo == NumCycles

        Endif      ! L_diff_ctl

      End If       ! ErrorStatus  ==  0

       IF ( cycleno == numcycles ) THEN

! DEPENDS ON: Atm_Step_alloc
        CALL Atm_Step_alloc(                                           &
#include "arg_atm_fields.h"
             cf_star, cfl_star, cff_star, q_star, qcl_star, qcf_star,   &
             frac_control, r_u, r_v, r_w, errorstatus, 'dealsmag' ) 

       END IF ! cycleno == numcycles

! DEPENDS ON: timer
      IF (Ltimer) CALL TIMER('AS Diffusion',6)

!---------- END DIFFUSION SECTION -------------------------------

      If ( L_tracer .and. CycleNo == NumCycles ) Then

! Obtain increments from Atmos_Physics2
! DEPENDS ON: tr_reset 
        call TR_Reset (super_array_size, super_tracer_phys2,     &
             L_CO2_interactive, CO2,  L_Murk_advect, murk,       &
             L_Soot, soot_new, soot_agd, soot_cld,               &
             L_SULPC_SO2, SO2, SO4_aitken, so4_accu, so4_diss,   &
             L_sulpc_nh3, nh3, L_sulpc_dms, dms, L_dust,         &
             DUST_DIV1, DUST_DIV2, DUST_DIV3, DUST_DIV4,         &
             DUST_DIV5, DUST_DIV6,                               &
             L_biomass, bmass_new, bmass_agd, bmass_cld,         &
             L_ocff, ocff_new, ocff_agd, ocff_cld,               &
             L_nitrate, nitr_acc, nitr_diss,                     &
             L_USE_CARIOLLE, OZONE_TRACER, tracer_phys2, tracer, &
             ukca_tracer_phys2, tracer_ukca, row_length, rows,   &
             model_levels, tr_levels, tr_vars, tr_ukca, offx, offy )

      end If ! L_tracer .and. CycleNo == NumCycles

! Obtain increments from Atmos_Physics2 and diffusion
! DEPENDS ON: Atm_Step_alloc
        CALL Atm_Step_alloc( &
#include "arg_atm_fields.h"
             cf_star, cfl_star, cff_star, q_star, qcl_star, qcf_star, &
             frac_control, r_u, r_v, r_w, errorstatus, 'phy2diff' ) 

! ---------------------------------------------------------------
!    diagnostic printing of l2norms
      if( L_print_L2norms ) then
        IF( L_print_pe .or. mype ==0 ) then
          write(6,*)' ***   L2 norms after diffusion  ***'
        ENDIF ! L_print_pe .or. mype ==0
        CALL prfldincnorm(.FALSE.)
      endif !  L_print_L2norms

      IF ( L_trap_theta ) THEN
! DEPENDS ON: trap_theta
        CALL trap_theta(theta, theta_star, max_thinc,                   &
     &                  rows, row_length, model_levels,                 &
     &                  offx, offy, halo_i, halo_j, trap_option)

!    diagnostic printing of l2norms
        IF( L_print_L2norms ) THEN
          IF( L_print_pe .or. mype ==0 ) THEN
            WRITE(6,*)' ***   L2 norms after trap_theta  ***'
          ENDIF ! L_print_pe .or. mype ==0
          CALL prfldincnorm(.FALSE.)
        END IF !  L_print_L2norms
      END IF ! L_trap_theta

! ----------------------------------------------------------------------
! Section 4.  Form and solve Helmholtz equation and update variables.
! ----------------------------------------------------------------------

      IF (ErrorStatus  ==  0) THEN
! DEPENDS ON: timer
      If (Ltimer) Call timer ('AS PG_Update',5)

! DEPENDS ON: Atm_Step_diag
        CALL Atm_Step_diag( &
#include "arg_atm_fields.h"
#include "argsts.h"
             r_v, r_u, r_w, q_star, qcl_star, qcf_star,  theta_star, &
             cf_star, cfl_star, cff_star, 7)

! DEPENDS ON: Atm_Step_swapbnds
        CALL Atm_Step_swapbnds &
            (theta_star, q_star, qcl_star, qcf_star, r_u, r_v, &
#include "arg_atm_fields.h"
             u_lbc_real_tend, v_lbc_real_tend, w_lbc_real_tend, 1)

        If (L_mix_ratio) Then

! DEPENDS ON: q_to_mix_halo
        call q_to_mix_halo (row_length, rows, wet_levels,               &
     &                 offx,offy     ,                                  &
     &                 q_star, qcl_star, qcf_star,                      &
     &                 qcf2_star,qrain_star, qgraup_star,               &
     &                 L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,           &
     &                 mix_v_star, mix_cl_star, mix_cf_star,            &
     &                 mix_cf2_star, mix_rain_star, mix_graup_star    )

        End If       !  L_mix_ratio

! Calculate updated values of pressure gradient terms.

! DEPENDS ON: ni_pg_update
        Call NI_pg_update(   Q, QCL, QCF,                               &
     &                 QCF2, QRAIN, QGRAUP,                             &
     &                 q_star, qcl_star, qcf_star,                      &
     &                 qcf2_star, qrain_star, qgraup_star,              &
     &                 mix_v, mix_cl, mix_cf,                           &
     &                 mix_cf2, mix_rain, mix_graup,                    &
     &                 mix_v_star, mix_cl_star, mix_cf_star,            &
     &                 mix_cf2_star, mix_rain_star, mix_graup_star,     &
     &                 L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,           &
     &                 theta_star, THETA, EXNER_RHO_LEVELS,             &
     &                 r_theta_levels, r_rho_levels,                    &
     &                 sec_theta_latitude,                              &
     &                 delta_lambda, delta_phi, timestep,               &
     &                 grecip_dlamp(lambda_start), recip_dphip,         &
     &                 wt_lambda_p, wt_phi_p,                           &
     &                 alpha_3, alpha_4,                                &
     &                 row_length, rows, n_rows, model_levels,          &
     &                 wet_levels, model_domain,                        &
     &                 first_constant_r_rho_level,                      &
     &                 offx, offy, halo_i, halo_j, at_extremity,        &
     &                 CycleNo, L_new_tdisc, L_qwaterload,              &
     &                 R_u, R_v, R_w, L_mix_ratio, L_regular )
! DEPENDS ON: timer
      If (Ltimer) Call timer ('AS PG_Update',6)

! ---------------------------------------------------------------
!    diagnostic printing of l2norms
      if( L_print_L2norms ) then
        IF( L_print_pe .or. mype ==0 ) then
          write(6,*)' ***   L2 norms after pg_update  ***'
        ENDIF ! L_print_pe .or. mype ==0
        CALL prfldincnorm(.FALSE.)
      endif !  L_print_L2norms
! ---------------------------------------------------------------

        If (model_domain  ==  mt_global ) Then
          If ( L_polar_filter_incs .or. L_filter_incs) Then
! DEPENDS ON: timer
      If (Ltimer) Call timer ('AS Filter',5)

! DEPENDS ON: ni_filter_incs_ctl
            Call NI_filter_incs_Ctl( THETA, theta_star, R_u, R_v, R_w,  &
     &                      row_length, rows, n_rows, model_levels,     &
     &                      r_theta_levels, r_rho_levels,               &
     &                      r_at_u, r_at_v,                             &
     &                      delta_lambda, delta_phi,                    &
     &                      cos_theta_longitude, sin_theta_longitude,   &
     &                      sin_theta_latitude, sin_v_latitude,         &
     &                      cos_theta_latitude, sec_theta_latitude,     &
     &                      cos_v_latitude, sec_v_latitude,             &
     &                      polar_filter_north_lat_limit,               &
     &                      polar_filter_south_lat_limit,               &
     &                      polar_filter_coefficient,                   &
     &                      polar_filter_n_sweeps,                      &
     &                      polar_filter_step_per_sweep,                &
     &                      polar_filter_lat_limit,                     &
     &                      max_121_rows, u_sweeps, v_sweeps,           &
     &                      global_u_filter, global_v_filter,           &
     &                      u_begin, u_end, v_begin, v_end,             &
     &                      diff_coeff_phi, diff_coeff_u, diff_coeff_v, &
     &                      diff_coeff_thermo, diff_coeff_wind,         &
     &                      diff_order_thermo, diff_order_wind,         &
     &                      horizontal_level, mype,                     &
     &                      offx, offy, halo_i, halo_j,                 &
     &                      nproc, nproc_x, nproc_y, datastart,         &
     &                      neighbour, at_extremity, model_domain,      &
     &                      gc_proc_row_group, gc_proc_col_group,       &
     &                      L_polar_filter_incs, L_filter_incs,         &
     &                      L_pfcomb, L_pftheta, L_pfuv, L_pfw,         &
     &                      L_pofil_new, L_diff_incs,                   &
     &                      exner_theta_levels,                         &
#include "argsts.h"
     &                      STASHwork13)
! DEPENDS ON: timer
      If (Ltimer) Call timer ('AS Filter',6)

            IF( SF(0,13) ) THEN  ! Diagnostics required for this section

! DEPENDS ON: stash
              CALL STASH(a_sm,a_im,13,STASHwork13,                       &
#include "argd1.h"
#include "argdumga.h"
#include "argsts.h"
     &        ErrorStatus,Cmessage)


            ENDIF !   SF(0,13)
            DEALLOCATE (STASHwork13)

          End If   ! L_polar_filter_incs .or. L_filter_incs

! calculate R_u at the poles.

! DEPENDS ON: polar_vector_wind_n
          Call Polar_vector_wind_n(  R_v, sin_theta_longitude,          &
     &                       cos_theta_longitude, row_length,           &
     &                       n_rows, model_levels, mag_vector_np,       &
     &                       dir_vector_np, mag_vector_sp,              &
     &                       dir_vector_sp,                             &
     &                       offx, offy, global_row_length,             &
     &                       gc_proc_row_group, at_extremity)

          If (at_extremity(PSouth) ) Then
            Do k = 1,model_levels
              Do i = 1, row_length
                R_u(i,1,k) = - mag_vector_sp(k) *                        &
     &                         sin ( lambda_a(i) - dir_vector_sp(k) )
              End Do
            End Do
          End If    ! at_extremity(PSouth)
          If (at_extremity(PNorth) ) Then
            Do k = 1,model_levels
              Do i = 1, row_length
                R_u(i,rows,k) = mag_vector_np(k) *                       &
     &                         sin ( lambda_a(i) - dir_vector_np(k) )
              End Do
            End Do
          End If    ! at_extremity(PNorth)

        End If    !model_domain

! DEPENDS ON: Atm_Step_swapbnds
        CALL Atm_Step_swapbnds &
            (theta_star, q_star, qcl_star, qcf_star, r_u, r_v, &
#include "arg_atm_fields.h"
             u_lbc_real_tend, v_lbc_real_tend, w_lbc_real_tend, 2)

      End If        !   ErrorStatus  ==  0

! ----------------------------------------------------------------------
! Section 4.1 Form and solve Helmholtz equation, returning all
!             corrections that are required.
! ----------------------------------------------------------------------
      If (ErrorStatus  ==  0) then

! in the idealised versions the boundary layer coefficients Km are
! replaced by the frictional timescale.

        If (model_domain ==  mt_lam) Then
! DEPENDS ON: timer
      If (Ltimer) Call timer('AS LAM_LBCS',5)

          IF (L_fixed_lbcs) Then
            L_update_lbcs = .false.
          Else
            L_update_lbcs = .true.
          END IF !  L_fixed_lbcs

          L_do_halos=.FALSE.
          L_do_boundaries=.TRUE.

          If (RIM_STEPSA  ==  0) Then
            increment_factor = 0.0
            L_balance = .false.
          Else If ( MOD(Timestep_Number-1,RIM_STEPSA) == 0) Then
            L_balance = .true.
            increment_factor = 1.0 / RIM_STEPSA
          Else
            L_balance = .false.
            increment_factor = 1.0 /                                     &
     &                 (RIM_STEPSA - MOD(Timestep_Number-1,RIM_STEPSA))
          End If

          lbc_size=LENRIMA(fld_type_p,halo_type_extended, rima_type_norm)

          If (L_lbc_new .and. L_update_lbcs) Then
! Obtain Exner tendency to pass to the solver to apply lbc

! Apply additional balance to EXNER_LBC_TEND, RHO_LBC_TEND 
! and W_LBC_TEND only on timestep after up_bound has been called.

          If (L_LBC_balance .and. L_balance) Then

! DEPENDS ON: balance_lbc_values
              CALL BALANCE_LBC_VALUES(                                   &
     &        EXNER_LBC_TEND, RHO_LBC_TEND, THETA_LBC_TEND,              &
     &        Q_LBC_TEND, W_LBC_TEND, W_ADV_LBC_TEND,                    &
     &        U_LBC_TEND, V_LBC_TEND,                                    &
     &        R_RHO_LEVELS, R_THETA_LEVELS,                              &
     &        ROW_LENGTH, ROWS, model_levels, wet_levels, HALO_I,HALO_J, &
     &        LENRIMA(fld_type_p,halo_type_extended,rima_type_norm),     &
     &        LENRIMA(fld_type_u,halo_type_extended,rima_type_norm),     &
     &        LENRIMA(fld_type_v,halo_type_extended,rima_type_norm),     &
     &        LBC_STARTA(1,fld_type_p,halo_type_extended,rima_type_norm),&
     &        LBC_STARTA(1,fld_type_u,halo_type_extended,rima_type_norm),&
     &        LBC_STARTA(1,fld_type_v,halo_type_extended,rima_type_norm),&
     &        RIMWIDTHA, N_RIMS_TO_DO, RIMWEIGHTSA, AT_EXTREMITY,       &
     &        DELTA_PHI, DELTA_LAMBDA,                                  &
     &        BASE_PHI, BASE_LAMBDA,                                    &
     &        DATASTART,                                                & 
     &        LAT_ROT_NP, GLOBAL_ROW_LENGTH, GLOBAL_ROWS, L_int_uvw_lbc )

          End If  !  L_LBC_balance and. L_balance

          DO k = 1, MODEL_LEVELS
            DO i=1,lbc_size
              EXNER_LBC_REAL_TEND(i,k) = increment_factor *              &
     &                         ( EXNER_LBC_TEND(i,k) - EXNER_LBC(i,k) )
            END DO
          END DO

          End If !  L_lbc_new .and. L_update_lbcs

          If (.not. L_transparent) Then
! Original lbc algorithm requires winds at boundaries
! Use these for new lbcs as well  (but loses transparency)

! Set the outer boundaries of R_u, R_v and R_w to the
! difference between the boundary at time level n+1 and time level n

          DO k=wdims_s%k_start,wdims_s%k_end
            DO i=1,lbc_size
              W_LBC_REAL_TEND(i,k) = increment_factor *                  &
     &             (W_LBC_TEND(i,k) - W_LBC(i,k))
            END DO
          END DO

          lbc_size=LENRIMA(fld_type_u,halo_type_extended,                &
     &             rima_type_norm)
          DO k=1,MODEL_LEVELS
            DO i=1,lbc_size
              U_LBC_REAL_TEND(i,k) = increment_factor *                  &
     &             (U_LBC_TEND(i,k) - U_LBC(i,k))
            END DO
          END DO
          lbc_size=LENRIMA(fld_type_v,halo_type_extended,                &
     &             rima_type_norm)

          DO k=1,MODEL_LEVELS
            DO i=1,lbc_size
              V_LBC_REAL_TEND(i,k) = increment_factor *                  &
     &             (V_LBC_TEND(i,k) - V_LBC(i,k))
            END DO
          END DO
! U
! DEPENDS ON: set_lateral_boundaries
          CALL SET_LATERAL_BOUNDARIES(   ROW_LENGTH,ROWS,Offx,Offy,     &
     &    MODEL_LEVELS,fld_type_u,R_u,                                  &
     &    LENRIMA(fld_type_u,halo_type_extended,rima_type_norm),        &
     &    LBC_SIZEA(1,fld_type_u,halo_type_extended,rima_type_norm),    &
     &    LBC_STARTA(1,fld_type_u,halo_type_extended,rima_type_norm),   &
     &    halo_i, halo_j,                                               &
     &    U_LBC_REAL_TEND,                                              &
     &    RIMWIDTHA(rima_type_norm),n_rims_to_do,                       &
     &    RIMWEIGHTSA,AT_EXTREMITY,                                     &
     &    L_do_boundaries,L_do_halos)

! V
! DEPENDS ON: set_lateral_boundaries
          CALL SET_LATERAL_BOUNDARIES(   ROW_LENGTH,N_ROWS,Offx,Offy,   &
     &    MODEL_LEVELS,fld_type_v,R_v,                                  &
     &    LENRIMA(fld_type_v,halo_type_extended,rima_type_norm),        &
     &    LBC_SIZEA(1,fld_type_v,halo_type_extended,rima_type_norm),    &
     &    LBC_STARTA(1,fld_type_v,halo_type_extended,rima_type_norm),   &
     &    halo_i, halo_j,                                               &
     &    V_LBC_REAL_TEND,                                              &
     &    RIMWIDTHA(rima_type_norm),n_rims_to_do,                       &
     &    RIMWEIGHTSA,AT_EXTREMITY,                                     &
     &    L_do_boundaries,L_do_halos)

! W
! Note: w_lbc_real_tend is passed from wdims%k_start (i.e. level 1) only,
!       since r_w starts at level 1. 
! DEPENDS ON: set_lateral_boundaries
          CALL SET_LATERAL_BOUNDARIES(                                   &
     &    ROW_LENGTH,ROWS,0,0,                                           &
     &    MODEL_LEVELS,fld_type_p,R_w,                                   &
     &    LENRIMA(fld_type_p,halo_type_extended,rima_type_norm),         &
     &    LBC_SIZEA(1,fld_type_p,halo_type_extended,rima_type_norm),     &
     &    LBC_STARTA(1,fld_type_p,halo_type_extended,rima_type_norm),    &
     &    halo_i, halo_j,                                                &
     &    W_LBC_REAL_TEND(:,wdims%k_start:),                             &
     &    RIMWIDTHA(rima_type_norm),n_rims_to_do,                        &
     &    RIMWEIGHTSA,AT_EXTREMITY,                                      &
     &    L_do_boundaries,L_do_halos)

          End If !  .not. L_transparent
! DEPENDS ON: timer
      If (Ltimer) Call timer('AS LAM_LBCS',6)

        Else If (model_domain  ==  mt_cyclic_lam) Then

! DEPENDS ON: Atm_Step_alloc
       CALL Atm_Step_alloc( &
#include "arg_atm_fields.h"
            cf_star, cfl_star, cff_star, q_star, qcl_star, qcf_star, &
            frac_control, r_u, r_v, r_w, errorstatus,'bound_uv')

        Endif  ! model_domain

! Note:
! R_u, R_v, R_w on output contain u_prime, v_prime, w_prime

        ALLOCATE ( dtheta_dr_term(tdims%i_start:tdims%i_end,             &
                                  tdims%j_start:tdims%j_end,             &
                                  tdims%k_start:tdims%k_end) )


        If ( CycleNo == 1 ) Then
          GCR_zero_guess_it = GCR_zero_init_guess
        Else
          GCR_zero_guess_it = ( .not. L_GCR_cycle_opt )                  &
     &                          .and. GCR_zero_init_guess
          If ( .NOT. L_GCR_cycle_opt ) Then
              exner_prime(:,:,:) = 0.0
          End If
        End If

        If ( CycleNo == 1 ) Then
          If ( GCR_use_tol_abs ) Then
            GCR_run_tol_abs = GCR_tol_abs
            GCR_run_tol_res = 0.0
          Else
            GCR_run_tol_res = GCR_tol_res
            GCR_run_tol_abs = 0.0
          End If
        Else
          If ( GCR_use_tol_abs ) Then
            GCR_run_tol_abs = GCR_tol_abs2
            GCR_run_tol_res = 0.0
          Else
            GCR_run_tol_res = GCR_tol_res2
            GCR_run_tol_abs = 0.0
          End If
        End If

! ---------------------------------------------------------------
!    diagnostic printing of l2norms
      if( L_print_L2norms ) then
        IF( L_print_pe .or. mype ==0 ) then
          write(6,*)' ***   L2 norms before solver  ***'
        ENDIF ! L_print_pe .or. mype ==0
        CALL prfldincnorm(.FALSE.)
      endif !  L_print_L2norms
! ---------------------------------------------------------------
! DEPENDS ON: timer
      If (Ltimer) Call timer ('AS Solver',5)

! DEPENDS ON: ni_pe_helmholtz
        Call NI_PE_Helmholtz(   U, V, W, r_theta_levels,                &
     &                      r_rho_levels, P, RHO,                       &
     &                      rho_np1, THETA,                             &
     &                      theta_star,theta_np1,                       &
     &                      Q, QCL, QCF,                                &
     &                      QCF2, QRAIN, QGRAUP,                        &
     &                      mix_v, mix_cl, mix_cf,                      &
     &                      mix_cf2, mix_rain, mix_graup,               &
     &                      mix_v_star, mix_cl_star, mix_cf_star,       &
     &                      mix_cf2_star, mix_rain_star, mix_graup_star,&
     &                      mix_v_np1, mix_cl_np1, mix_cf_np1,          &
     &                      mix_cf2_np1, mix_rain_np1, mix_graup_np1,   &
     &                      q_star, q_np1, qcl_star, qcf_star,          &
     &                      qcf2_star, qrain_star, qgraup_star,         &
     &                      qcl_np1, qcf_np1,                           &
     &                      qcf2_np1 , qrain_np1, qgraup_np1,           &
     &                      rho_Km, cH, G_term_tol,                     &
     &                      EXNER_RHO_LEVELS,                           &
     &                      EXNER_THETA_LEVELS,                         &
     &                      frictional_timescale,                       &
     &                      cos_theta_latitude, sec_theta_latitude,     &
     &                      cos_v_latitude, sec_v_latitude,             &
     &                      FV_cos_theta_latitude,                      &
     &                      FV_sec_theta_latitude,                      &
     &                      f3_at_u, f3_at_v,                           &
     &                      timestep, timestep_number,                  &
     &                      row_length, rows, n_rows,                   &
     &                      model_levels, wet_levels, bl_levels,        &
! ---------------------------------------------------------------
     &                      delta_lambda, delta_phi,                     &
     &                      glambda_p(lambda_start), phi_p,              &
     &                      glambda_u(lambda_start), phi_v,              &
     &                      gdlambda_p(lambda_start), dphi_p,            &
     &                      gdlambda_u(lambda_start), dphi_v,            &
     &                      grecip_dlamp(lambda_start), recip_dphip,     &
     &                      grecip_dlamu(lambda_start), recip_dphiv,     &
     &                      wt_lambda_p, wt_phi_p,                       &
     &                      wt_lambda_u, wt_phi_v,                       &
     &                      GCR_max_iterations, GCR_diagnostics,         &
     &                      GCR_its_switch(CycleNo), GCR_its_avg_step,   &
     &                      GCR_max_its(CycleNo), GCR_min_its(CycleNo),  &
     &                      GCR_sum_its(CycleNo), GCR_max_time(CycleNo), &
     &                      GCR_min_time(CycleNo),                       &
     &  GCR_run_tol_res, GCR_run_tol_abs, GCR_use_tol_abs,               &
     &                      GCR_zero_guess_it,                           &
     &                      GCR_use_residual_Tol,                        &
     &                      GCR_adi_add_full_soln, L_gcr_fast_x,         &
     &                      GCR_precon_option, GCR_ADI_Pseudo_timestep,  &
     &                      GCR_n_ADI_pseudo_timesteps,                  &
     &                      eta_theta_levels, eta_rho_levels,            &
     &                      alpha1, alpha2, alpha3, alpha4,              &
     &                      alpha_Cd,                                    &
     &                      model_domain, L_physics,                     &
     &                      GCR_Restart_value,                           &
     &                      first_constant_r_rho_level,                  &
     &                      first_constant_r_rho_level_m1,               &
     &                      R_u, R_v, R_w, exner_prime, dtheta_dr_term,  &
     &                      EXNER_LBC_REAL_TEND,                         &
     &                      LENRIMA(1,1,rima_type_norm),                 &
     &                      LBC_SIZEA(1,1,1,rima_type_norm),             &
     &                      LBC_STARTA(1,1,1,rima_type_norm),            &
     &                      RIMWIDTHA(rima_type_norm), RIMWEIGHTSA,      &
!!! parallel variables
     &                      mype, nproc, nproc_x, nproc_y,              &
     &                      halo_i, halo_j, datastart,                  &
     &                      L_regular, at_extremity,                    &
     &                      n_rims_to_do, offx, offy,                   &
     &                      gc_proc_row_group, gc_proc_col_group,       &
     &                      global_row_length, global_rows,             &
     &                      g_rows, g_row_length, CycleNo,              &
     &                      L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,      &
     &                      L_mix_ratio, L_fint_theta, L_new_tdisc,     &
     &                      L_qwaterload, L_lbc_new, L_fixed_lbcs,      &
     &                      L_transparent, ldump    )

! If increments are swapped now then this avoids needing swap
! in update rho
        i_field = 0

        i_field = i_field + 1
        fields_to_swap(i_field) % field      => r_u(:,:,:)
        fields_to_swap(i_field) % field_type =  fld_type_u
        fields_to_swap(i_field) % levels     =  model_levels
        fields_to_swap(i_field) % rows       =  rows
        fields_to_swap(i_field) % vector     =  .TRUE.

        i_field = i_field + 1
        fields_to_swap(i_field) % field      => r_v(:,:,:)
        fields_to_swap(i_field) % field_type =  fld_type_v
        fields_to_swap(i_field) % levels     =  model_levels
        fields_to_swap(i_field) % rows       =  n_rows
        fields_to_swap(i_field) % vector     =  .TRUE.

! DEPENDS ON: swap_bounds_mv
        CALL swap_bounds_mv( fields_to_swap, i_field,                    &
                             row_length, offx, offy )
! DEPENDS ON: timer
      If (Ltimer) Call timer ('AS Solver',6)

!----------------------------------------------------------------------
! Section 4.1 Diagnostics from Solver (may need to move to after
!  rho update if wanted to output rho increments in future).
!----------------------------------------------------------------------
! Apply diagnostics only at lat cycle.
        If ( CycleNo == NumCycles ) Then

        IF (SF(0,10)                                                     &
                         ! diagnostics required for this section
     &      .and. ErrorStatus == 0) THEN

! Allocate diagnostic space for STASH
          ALLOCATE (STASHwork10(STASH_maxlen(10,A_im)))

! DEPENDS ON: diagnostics_solver
          CALL Diagnostics_solver(                                       &
     &       row_length,rows,n_rows,model_levels,                        &
! wind field increments after  solver :
     &       R_u,R_v,R_w,                                                &
! wind field increments before solver (on stashflag):
     &       u_incr_diagnostic,v_incr_diagnostic,w_incr_diagnostic,      &
#include "argsts.h"
     &       STASHwork10)

! Tidy allocatable arrays
          IF( sf(185,10) ) THEN
            DEALLOCATE ( u_incr_diagnostic )
          ENDIF  ! on STASHflag
          IF( sf(186,10) ) THEN
            DEALLOCATE ( v_incr_diagnostic )
          ENDIF  ! on STASHflag
          IF( sf(187,10) ) THEN
            DEALLOCATE ( w_incr_diagnostic )
          ENDIF  ! on STASHflag

! DEPENDS ON: stash
          CALL STASH(a_sm,a_im,10,STASHwork10,                           &
#include "argd1.h"
#include "argdumga.h"
#include "argsts.h"
     &    ErrorStatus,Cmessage)

          DEALLOCATE (STASHwork10)

        ENDIF !   SF(0,10)

        End If ! CycleNo == NumCycles

      End If  !      ErrorStatus  ==  0

! ---------------------------------------------------------------
!    diagnostic printing of l2norms
      if( L_print_L2norms ) then
        IF( L_print_pe .or. mype ==0 ) then
          write(6,*)' ***   L2 norms after solver  ***'
        ENDIF ! L_print_pe .or. mype ==0
        CALL prfldincnorm(.FALSE.)
      endif !  L_print_L2norms
! ---------------------------------------------------------------

! ----------------------------------------------------------------------
! Section 5.0 Calculate rho at new time level, using flux form of
!             equation.
! ----------------------------------------------------------------------
! Call timer for updates code
! DEPENDS ON: timer
      If (Ltimer) CALL TIMER('AS Updates',5)

      If (ErrorStatus  ==  0) then

        If ( CycleNo == 1 ) Then
          If ( sf(188,30) ) Then
            ALLOCATE ( inc_rho(tdims_s%i_start:tdims_s%i_end,            &
                               tdims_s%j_start:tdims_s%j_end,            &
                               tdims_s%k_start:tdims_s%k_end) )
          Else
            ALLOCATE ( inc_rho( 1,1,1) )
          End If
        End If

! stash diag 30188 only needed at last cycle.
        L_do_inc_rho = (CycleNo == NumCycles) .and. sf(188,30)

! Set rho_n at
        If ( CycleNo == NumCycles .and. ( L_tracer .or.                  &
     &       L_moist_nonhydro_conserve .or. L_do_inc_rho ) ) Then

! store rho_n before updating by flux_rho
! DEPENDS ON: copy_field
      CALL COPY_FIELD(RHO,rho_n                                          &
     &,               row_length, row_length, rows, rows                 &
     &,               model_levels, model_levels, 1, model_levels        &
     &,               Offx, Offy, Offx, Offy                             &
     &,               fld_type_p, .true., .false., .false.)

! DEPENDS ON: fill_external_halos
          CALL FILL_EXTERNAL_HALOS(rho_n,row_length, rows,               &
     &                     model_levels,offx,offy)

        End If ! ! CycleNo == NumCycles .and. ...


        If (  CycleNo == NumCycles .OR. L_new_tdisc ) Then

! DEPENDS ON: ni_update_rho
          Call NI_Update_Rho(  rho, rho_n, rho_np1, inc_rho,            &
     &                 u, v, w, R_u, R_v, R_w,                          &
     &                 q, qcl,qcf,                                      &
     &                 qcf2, qrain, qgraup,                             &
     &                 q_star, qcl_star, qcf_star,                      &
     &                 qcf2_star, qrain_star, qgraup_star,              &
     &                 q_np1, qcl_np1, qcf_np1,                         &
     &                 qcf2_np1, qrain_np1, qgraup_np1,                 &
     &                 mix_v, mix_cl, mix_cf,                           &
     &                 mix_cf2, mix_rain, mix_graup,                    &
     &                 mix_v_star, mix_cl_star, mix_cf_star,            &
     &                 mix_cf2_star, mix_rain_star, mix_graup_star,     &
     &                 mix_v_np1, mix_cl_np1, mix_cf_np1,               &
     &                 mix_cf2_np1, mix_rain_np1, mix_graup_np1,        &
     &                 L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,           &
     &                 timestep, CycleNo, NumCycles,                    &
     &                 rows, n_rows, row_length,                        &
     &                 model_levels, wet_levels, model_domain,          &
     &                 first_constant_r_rho_level,                      &
     &                 alpha_1, alpha_2, n_rims_to_do,                  &
     &                 nproc, gc_proc_row_group,                        &
     &                 L_regular, at_extremity, global_row_length,      &
     &                 offx, offy, halo_i, halo_j,                      &
     &                 cos_v_latitude, delta_lambda, delta_phi,         &
     &                 gdlambda_p(lambda_start), dphi_p,                &
     &                 grecip_dlamu(lambda_start), recip_dphiv,         &
     &                 wt_lambda_p, wt_lambda_u, wt_phi_p, wt_phi_v,    &
     &                 r_theta_levels, r_rho_levels, eta_theta_levels,  &
     &                 eta_rho_levels, FV_sec_theta_latitude,           &
     &                 wet_to_dry_n, wet_to_dry_np1,                    &
     &                 L_do_inc_rho, L_new_tdisc,                       &
     &                 L_mix_ratio, L_dry )

        End If !  CycleNo == NumCycles .OR. L_new_tdisc

        If ( CycleNo == NumCycles ) Then

          if(L_mix_ratio .and. .not. L_moist_nonhydro_conserve)then
            deallocate(mix_v)
            deallocate(mix_cl)
            deallocate(mix_cf)
            deallocate(mix_cf2)
            deallocate(mix_rain)
            deallocate(mix_graup)
          endif

          If (NumCycles > 1 .and. L_mix_ratio                            &
     &                      .and. .not. L_moist_nonhydro_conserve) Then
            deallocate( mix_v_star_phys1)
            deallocate( mix_cl_star_phys1)
            deallocate( mix_cf_star_phys1)
            If ( L_mcr_qcf2 ) deallocate( mix_cf2_star_phys1)
            If ( L_mcr_qrain ) deallocate( mix_rain_star_phys1)
            If ( L_mcr_qgraup ) deallocate( mix_graup_star_phys1)
          End If

! DEPENDS ON: timer
      If (Ltimer) CALL TIMER('AS Updates',6)

        If (L_tracer) then
! DEPENDS ON: timer
          If (Ltimer) CALL timer('AS S-L Advect (AA)',5)

! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AA SL_Tracer',5)
! DEPENDS ON: sl_tracer2 
          Call SL_tracer2(       super_array_size,                      &
     &                  super_tracer_phys1,super_tracer_phys2,          &
     &                 eta_theta_levels,                                &
     &                 r_rho_levels,  r_theta_levels,                   &
     &                 rho_n, RHO,                                      &
     &                 row_length, rows, model_levels, bl_levels,       &
     &                 delta_lambda, delta_phi,                         &
     &                 glambda_p, phi_p, grecip_dlamp, recip_dphip,     & 
     &                 lambda_p_rm, lambda_p_rp, phi_p_rm, phi_p_rp,    &
     &                 recip_lambda_p_m, recip_lambda_p_0,              &
     &                 recip_lambda_p_p, recip_lambda_p_p2,             &
     &                 recip_phi_p_m, recip_phi_p_0,                    &
     &                 recip_phi_p_p, recip_phi_p_p2,                   &
     &                 base_lambda, base_phi,                           &
     &                 recip_dlam, recip_dphi, max_look,                &
     &                 look_lam, look_phi, halo_lam, halo_phi,          &
     &                 FV_cos_theta_latitude,                           &
     &                 wet_to_dry_n, wet_to_dry_np1,                    &
     &                 mype, nproc, nproc_x, nproc_y,                   &
     &                 halo_i, halo_j, datastart,                       &
     &                 g_i_pe, g_j_pe, l_2dcomm, size_2dcomm,           &
     &                 group_2dcomm, at_extremity,                      &
     &                 global_row_length,                               &
     &                 global_rows,                                     &
     &                 gc_all_proc_group,                               &
     &                 gc_proc_row_group,                               &
     &                 gc_proc_col_group, offx, offy,                   &
     &                 L_regular, L_sl_halo_reprod,                     &
     &                 high_order_scheme(moist_SL),                     &
     &                 monotone_scheme(moist_SL),                       &
     &                 model_domain, L_high(moist_SL),                  &
     &                 L_mono(moist_SL),                                &
     &                 L_conserve_tracers,                              &
     &                 check_bottom_levels,                             &
     &                 interp_vertical_search_tol,                      &
     &                 first_constant_r_rho_level,                      &
     &                 depart_lambda, depart_phi, depart_r_theta,       &
     &                 co2, L_CO2_interactive,                          &
     &                 Murk, L_Murk_advect,                             &
     &                 soot_new, soot_agd,                              &
     &                 soot_cld, L_soot,                                &
     &                 bmass_new, bmass_agd,                            &
     &                 bmass_cld, L_biomass,                            &
     &                 ocff_new, ocff_agd, ocff_cld, L_ocff,            &
     &                 DUST_DIV1,DUST_DIV2,                             &
     &                 DUST_DIV3,DUST_DIV4,                             &
     &                 DUST_DIV5,DUST_DIV6, L_DUST,                     &
     &                 so2, so4_aitken,                                 &
     &                 so4_accu,                                        &
     &                 so4_diss, nh3, dms,                              &
     &                 L_sulpc_so2, L_sulpc_nh3, l_sulpc_dms,           &
     &                 nitr_acc, nitr_diss, L_nitrate,                  &
     &                 tracer, tr_levels, tr_vars,                      &
     &                 tracer_phys1,tracer_phys2,                       &
     &                 TR_LBC_VARS,TRACER_LBC,                          &
     &                 A_MAX_TRVARS,A_TR_ACTIVE_LBC_INDEX,              &
     &                 tracer_ukca, tr_ukca,                            &
     &                 ukca_tracer_phys1, ukca_tracer_phys2,            &
     &                 TR_LBC_UKCA,TRACER_UKCA_LBC,                     &
     &                 A_MAX_UKCAVARS,UKCA_TR_ACTIVE_LBC_INDEX,         &
     &                 RIMWIDTHA(rima_type_norm),RIMWEIGHTSA,           &
     &                 LENRIMA(fld_type_p,halo_type_extended,           &
     &                 rima_type_norm),                                 &
     &                 LBC_SIZEA(1,fld_type_p,halo_type_extended,       &
     &                 rima_type_norm),                                 &
     &                 LBC_STARTA(1,fld_type_p,halo_type_extended,      &
     &                 rima_type_norm),                                 &
     &                 i_start, i_stop, j_start, j_stop,                &
     &                 L_USE_CARIOLLE, OZONE_TRACER, l_qpos_diag_pr,    &
     &                 qpos_diag_limit, q_pos_tracer_method, ErrorStatus)
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AA SL_Tracer',6)
! DEPENDS ON: timer
          If (Ltimer) CALL timer('AS S-L Advect (AA)',6)

!      Cariolle scheme is called to calculate the tracer ozone. All the tracers
!      are calculated at the end of the timestep. The ozone tracer 
!      calculated here will be used in the radiation scheme on the next timestep.

!      Insert if statement here to check that tracer that goes into this call 
!      ozone. Ozone may not be the only tracer.
            IF (PrintStatus  >=  PrStatus_Normal .AND.                   &
                first_atmstep_call .AND. mype == 0) THEN

               WRITE(6,*) 'Atm_Step: L_USE_CARIOLLE = ',L_USE_CARIOLLE
            END IF

            If (L_USE_CARIOLLE) then
               
               If (PrintStatus  >=  PrStatus_Normal .AND.                  &
                first_atmstep_call .AND. mype == 0) then

                    WRITE(6,*) 'Atm_Step: Calling Cariolle_o3_psc'
               End If

! DEPENDS ON: cariolle_o3_psc
               CALL cariolle_o3_psc (OZONE_TRACER,                       &
                      O3_PROD_LOSS,   O3_P_L_VMR,                        &
                      O3_VMR,         O3_P_L_TEMP,                       &
                      O3_TEMP,        O3_P_L_COLO3,                      &
                      O3_COLO3,                                          &
                      THETA,                                             &
                      P_THETA_LEVELS,                                    &
                      offx,offy,theta_off_size,                          &
                      rows,row_length,                                   &
                      timestep,                                          &
                      EXNER_THETA_LEVELS,                                &
                      model_levels)      
!    
! Halos updated
! DEPENDS ON: swap_bounds
               call Swap_Bounds(OZONE_TRACER,                            &
                        row_length, rows, model_levels,                  &
                        offx, offy, fld_type_p, .false.)
            else
               If (PrintStatus  >=  PrStatus_Normal .AND.                &
                   first_atmstep_call) then

                   WRITE(6,*) 'Atm_Step: Cariolle scheme not called'
               End If
            End if

          If (.not. L_moist_nonhydro_conserve)then
            DEALLOCATE (depart_r_theta)
            DEALLOCATE (depart_lambda)
            DEALLOCATE (depart_phi)
          endif
        end if  ! L_tracer

! ----------------------------------------------------------------------
! Section 5.2 Recalculate sl_thermo to allow rho at n+1
!             to be used in the conservation step
!            Estimated values are returned in the _phys1 variables.
! ----------------------------------------------------------------------
!  At this point, q_star, qcl_star and qcf_star in the boundary layer
!  have been changed by the implicit bl calculation but are not used
!  from now on.
        If (L_moist_nonhydro_conserve) then
! DEPENDS ON: timer
          If (Ltimer) CALL timer('AS S-L Advect (AA)',5)

! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AA SL_Moist',5)
! DEPENDS ON: ni_sl_moist
          Call NI_SL_moist( moisture_array_size,                        &
     &                 q, qcl, qcf,                                     &
     &                 cf_bulk,cf_frozen,                               &
     &                 cf_liquid,                                       &
     &                 qcf2, qrain, qgraup,                             &
     &                 q_phys1, qcl_phys1, qcf_phys1,                   &
     &                 cf_phys1, cff_phys1, cfl_phys1,                  &
     &                 qcf2_phys1, qrain_phys1, qgraup_phys1,           &
     &                 mix_v, mix_cl, mix_cf,                           &
     &                 mix_cf2, mix_rain, mix_graup,                    &
     &                 mix_v_phys1, mix_cl_phys1, mix_cf_phys1,         &
     &                 mix_cf2_phys1, mix_rain_phys1, mix_graup_phys1,  &
     &                 L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,           &
     &                 L_pc2, L_mix_ratio,                              &
     &                 eta_theta_levels,                                &
     &                 r_rho_levels,  r_theta_levels,                   &
     &                 exner_theta_levels,                              &
     &                 rho_n, rho,                                      &
     &                 depart_lambda, depart_phi, depart_r_theta,       &
     &                 row_length, rows, n_rows,                        &
     &                 model_levels, wet_levels,                        &
     &                 delta_lambda, delta_phi,                         &
     &                 base_lambda, base_phi,                           &
     &                 glambda_p, phi_p, grecip_dlamp, recip_dphip,     &
     &                 lambda_p_rm, lambda_p_rp, phi_p_rm, phi_p_rp,    &
     &                 recip_lambda_p_m, recip_lambda_p_0,              &
     &                 recip_lambda_p_p, recip_lambda_p_p2,             &
     &                 recip_phi_p_m, recip_phi_p_0,                    &
     &                 recip_phi_p_p, recip_phi_p_p2,                   &
     &                 recip_dlam, recip_dphi, max_look,                &
     &                 look_lam, look_phi, halo_lam, halo_phi,          &
     &                 FV_cos_theta_latitude,                           &
     &                 wet_to_dry_n, wet_to_dry_np1,                    &
     &                 L_regular, n_rims_to_do,                         &
     &                 mype, nproc, nproc_x, nproc_y,                   &
     &                 halo_i, halo_j, datastart,                       &
     &                 g_i_pe, g_j_pe, l_2dcomm, size_2dcomm,           &
     &                 group_2dcomm, at_extremity,                      &
     &                 global_row_length, global_rows,                  &
     &                 gc_proc_row_group, gc_proc_col_group,            &
     &                 gc_all_proc_group, offx, offy,                   &
     &                 L_sl_halo_reprod,                                &
     &                 high_order_scheme(moist_SL),                     &
     &                 monotone_scheme(moist_SL),                       &
     &                 model_domain, L_high(moist_SL),                  &
     &                 L_mono(moist_SL), L_conserv(moist_SL),           &
     &                 check_bottom_levels, interp_vertical_search_tol, &
     &                 first_constant_r_rho_level,                      &
     &                 ErrorStatus )
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AA SL_Moist',6)

          DEALLOCATE (depart_lambda)
          DEALLOCATE (depart_phi)
          DEALLOCATE (depart_r_theta)
          if(L_mix_ratio)then
            deallocate(mix_v)
            deallocate(mix_cl)
            deallocate(mix_cf)
            deallocate(mix_cf2)
            deallocate(mix_rain)
            deallocate(mix_graup)
          endif

          If ( NumCycles > 1 .and. L_mix_ratio ) Then
            deallocate( mix_v_star_phys1)
            deallocate( mix_cl_star_phys1)
            deallocate( mix_cf_star_phys1)
            If ( L_mcr_qcf2 ) deallocate( mix_cf2_star_phys1)
            If ( L_mcr_qrain ) deallocate( mix_rain_star_phys1)
            If ( L_mcr_qgraup ) deallocate( mix_graup_star_phys1)
          End If
! DEPENDS ON: timer
          If (Ltimer) CALL timer('AS S-L Advect (AA)',6)
        end if          ! L_moist_nonhydro_conserve

      End If ! NoCycles == NumCycles

      END IF       ! ErrorStatus  ==  0

! ----------------------------------------------------------------------
! Section 6.0 Calculate u, v, w, theta, q, qcl, qcf at new time level.
!             Extrapolate u, v, w, to time level n+1.5 for use in
!             advection step on next timestep.
! ----------------------------------------------------------------------
      If ( ErrorStatus  ==  0) Then

! DEPENDS ON: timer
          If (Ltimer) CALL TIMER('AS Updates',5)

! DEPENDS ON: update_fields
        call update_fields( NumCycles, CycleNo, L_new_tdisc,            &
     &                   L_mix_ratio, extrp_weight,                     &
     &                   EXNER_RHO_LEVELS,                              &
     &                   EXNER_THETA_LEVELS,                            &
     &                   U, u_np1, V, v_np1,                            &
     &                   W, w_np1,                                      &
     &                   U_ADV, V_ADV, W_ADV,                           &
     &                   THETA,Q,QCL,QCF,                               &
     &                   QCF2, QRAIN, QGRAUP,                           &
     &                   CF_BULK,CF_LIQUID,                             &
     &                   CF_FROZEN,                                     &
     &                   exner_prime, R_u, R_v, R_w,                    &
     &                   theta_star, theta_np1, dtheta_dr_term,         &
     &                   q_star, q_np1, qcl_star, qcl_np1, qcf_star,    &
     &                   qcf_np1, qcf2_star, qcf2_np1, qrain_star,      &
     &                   qrain_np1, qgraup_star, qgraup_np1,            &
     &                   cf_star, cfl_star, cff_star,                   &
     &                   row_length, rows, n_rows, model_levels,        &
     &                   offx, offy, halo_i, halo_j,                    &
     &                   sin_theta_longitude, cos_theta_longitude,      &
     &                   mag_vector_np, dir_vector_np,                  &
     &                   mag_vector_sp, dir_vector_sp,                  &
     &                   global_row_length, gc_proc_row_group,          &
     &                   at_extremity, datastart,model_domain,          &
     &                   alpha_2 , timestep, wet_levels,                &
     &                   i_start, i_stop, j_start, j_stop,              &
     &                   j_begin, j_end,                                &
     &                   delta_lambda, glambda_p(lambda_start),         &
     &                   glambda_u(lambda_start),                       &
     &                   inc_t, inc_u, inc_v, inc_w,                    &
     &                   sf(181,30), L_do_inc_vels, L_pc2,              &
     &                   L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,         &
     &                   L_regular )
! DEPENDS ON: timer
          If (Ltimer) CALL TIMER('AS Updates',6)

        If ( NumCycles > 1 .and. CycleNo < NumCycles ) Then

! For consistency in the formulation, the _adv and _np1 vars need
! to be updated in the lateral boundary region.

! DEPENDS ON: Atm_Step_swapbnds
          CALL Atm_Step_swapbnds &
              (theta_star, q_star, qcl_star, qcf_star, r_u, r_v, &
#include "arg_atm_fields.h"
               u_lbc_real_tend, v_lbc_real_tend, w_lbc_real_tend, 3)

! Now need to fix Uadv and temp n+1 dynamical vars at LB region.
          If ( model_domain == mt_lam                                    &
     &         .or.  model_domain == mt_cyclic_lam ) Then
! DEPENDS ON: TIMER
            IF (ltimer) CALL timer('AS LAM_LBCS',5)

! DEPENDS ON: Atm_Step_swapbnds
            CALL Atm_Step_swapbnds &
              (theta_star, q_star, qcl_star, qcf_star, r_u, r_v, &
#include "arg_atm_fields.h"
               u_lbc_real_tend, v_lbc_real_tend, w_lbc_real_tend, 4)


            If ( .NOT. L_mix_ratio ) Then
! DEPENDS ON: UPDATE_LBC_ITERSL
              CALL UPDATE_LBC_ITERSL(                                    &
     &           ROW_LENGTH,ROWS,N_ROWS,MODEL_LEVELS,WET_LEVELS,         &
     &           OFFX,OFFY,HALO_I,HALO_J,AT_EXTREMITY, L_new_tdisc,      &
     &           L_mcr_qcf2_lbc, L_mcr_qrain_lbc,                        &
     &           L_mcr_qgraup_lbc, L_pc2_lbc,                            &
     &           RIMWIDTHA(rima_type_norm),RIMWEIGHTSA,                  &
     &           LENRIMA(1,1,rima_type_norm),                            &
     &           LBC_SIZEA(1,1,1,rima_type_norm),                        &
     &           LBC_STARTA(1,1,1,rima_type_norm),                       &
     &           THETA_LBC,Q_LBC,QCL_LBC,                                &
     &           QCF_LBC,QCF2_LBC,QRAIN_LBC,                             &
     &           QGRAUP_LBC, CF_BULK_LBC,CF_LIQUID_LBC,                  &
     &           CF_FROZEN_LBC, RHO_LBC_REAL_TEND, EXNER_LBC,            &
     &           U_LBC_REAL_TEND,V_LBC_REAL_TEND,W_LBC_REAL_TEND,        &
     &           U_ADV_LBC,V_ADV_LBC,W_ADV_LBC,                          &
     &           theta_np1, q_np1, qcl_np1, qcf_np1,                     &
     &           qcf2_np1, qrain_np1, qgraup_np1,                        &
     &           CF_BULK,CF_LIQUID,CF_FROZEN,                            &
     &           rho_np1,EXNER_RHO_LEVELS,                               &
     &           u_np1,v_np1,w_np1,                                      &
     &           U_ADV,V_ADV,W_ADV)          
            Else
! mix_v_star etc hold q_star etc temporarily
! DEPENDS ON: UPDATE_LBC_ITERSL
              CALL UPDATE_LBC_ITERSL(                                    &
     &           ROW_LENGTH,ROWS,N_ROWS,MODEL_LEVELS,WET_LEVELS,         &
     &           OFFX,OFFY,HALO_I,HALO_J,AT_EXTREMITY, L_new_tdisc,      &
     &           L_mcr_qcf2_lbc, L_mcr_qrain_lbc,                        &
     &           L_mcr_qgraup_lbc, L_pc2_lbc,                            &
     &           RIMWIDTHA(rima_type_norm),RIMWEIGHTSA,                  &
     &           LENRIMA(1,1,rima_type_norm),                            &
     &           LBC_SIZEA(1,1,1,rima_type_norm),                        &
     &           LBC_STARTA(1,1,1,rima_type_norm),                       &
     &           THETA_LBC,Q_LBC,QCL_LBC,                                &
     &           QCF_LBC,QCF2_LBC,QRAIN_LBC,                             &
     &           QGRAUP_LBC, CF_BULK_LBC,CF_LIQUID_LBC,                  &
     &           CF_FROZEN_LBC, RHO_LBC_REAL_TEND, EXNER_LBC,            &
     &           U_LBC_REAL_TEND,V_LBC_REAL_TEND,W_LBC_REAL_TEND,        &
     &           U_ADV_LBC,V_ADV_LBC,W_ADV_LBC,                          &
     &           theta_np1, mix_v_star, mix_cl_star, mix_cf_star,        &
     &           mix_cf2_star, mix_rain_star, mix_graup_star,            &
     &           CF_BULK,CF_LIQUID,CF_FROZEN,                            &
     &           rho_np1,EXNER_RHO_LEVELS,                               &
     &           u_np1,v_np1,w_np1,                                      &
     &           U_ADV,V_ADV,W_ADV)
            End If ! .NOT. L_mix_ratio


            If ( CycleNo == NumCycles-1 ) Deallocate(RHO_LBC_REAL_TEND)
! DEPENDS ON: TIMER
            IF (ltimer) CALL timer('AS LAM_LBCS',6)

          End If ! If model_domain == mt_lam ...

! Now convert qX(=mix_v_star,...) to mX and store at mix_v_np1 etc
          If ( L_mix_ratio .and. L_new_tdisc ) Then
            call q_to_mix_halo (row_length, rows, wet_levels,           &
     &                 offx, offy, mix_v_star, mix_cl_star, mix_cf_star,&
     &                 mix_cf2_star, mix_rain_star, mix_graup_star,     &
     &                 L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup,           &
     &                 mix_v_np1, mix_cl_np1, mix_cf_np1,               &
     &                 mix_cf2_np1, mix_rain_np1, mix_graup_np1 )
          End If

        End If ! NumCycles > 1 .and. CycleNo < NumCycles

!  LBC updating now done at end of time step     
! ---------------------------------------------------------------------
!   Section 6.1  Update lbcs for LAMs
! ---------------------------------------------------------------------

      If ( model_domain == mt_lam ) Then
! DEPENDS ON: TIMER
            IF (ltimer) CALL timer('AS LAM_LBCS',5)
      
        IF (L_Fixed_lbcs) Then
          L_update_lbcs = .false.
          L_apply_lbcs = .true.
        Else IF ( RIM_STEPSA == 0 ) Then
          L_update_lbcs = .false.
        Else IF ( L_lbc_new ) Then
          L_update_lbcs = .true.
        Else
          L_update_lbcs = .false.
        END IF !  L_Fixed_lbcs

        If ( L_update_lbcs ) Then
        
! DEPENDS ON: BOUNDVAL
              Call BOUNDVAL(LENRIMA(1,1,rima_type_norm),                 &
     &                      L_mcr_qcf2_lbc, L_mcr_qrain_lbc,             &
     &                      L_mcr_qgraup_lbc, L_pc2_lbc,                 &
     &                      L_murk_lbc, L_int_uvw_lbc,                   &
     &                      L_dust_div1_lbc,L_dust_div2_lbc,             &
     &                      L_dust_div3_lbc,L_dust_div4_lbc,             &
     &                      L_dust_div5_lbc,L_dust_div6_lbc,             &
     &                      L_so2_lbc,L_dms_lbc,L_so4_aitken_lbc,        &
     &                      L_so4_accu_lbc,L_so4_diss_lbc,               &
     &                      L_nh3_lbc,L_soot_new_lbc,L_soot_agd_lbc,     &
     &                      L_soot_cld_lbc,L_bmass_new_lbc,              &
     &                      L_bmass_agd_lbc,L_bmass_cld_lbc,             &
     &                      L_ocff_new_lbc,                              &
     &                      L_ocff_agd_lbc,L_ocff_cld_lbc,               &
     &                      L_nitr_acc_lbc, L_nitr_diss_lbc,             &
     &                      U_LBC, U_LBC_TEND,                           &
     &                      V_LBC, V_LBC_TEND,                           &
     &                      W_LBC, W_LBC_TEND,                           &
     &                      RHO_LBC, RHO_LBC_TEND,                       &
     &                      THETA_LBC, THETA_LBC_TEND,                   &
     &                      Q_LBC, Q_LBC_TEND,                           &
     &                      QCL_LBC, QCL_LBC_TEND,                       &
     &                      QCF_LBC, QCF_LBC_TEND,                       &
     &                      QCF2_LBC, QCF2_LBC_TEND,                     &
     &                      QRAIN_LBC, QRAIN_LBC_TEND,                   &
     &                      QGRAUP_LBC, QGRAUP_LBC_TEND,                 &
     &                      CF_BULK_LBC, CF_BULK_LBC_TEND,               &
     &                      CF_LIQUID_LBC, CF_LIQUID_LBC_TEND,           &
     &                      CF_FROZEN_LBC, CF_FROZEN_LBC_TEND,           &
     &                      EXNER_LBC, EXNER_LBC_TEND,                   &
     &                      U_ADV_LBC, U_ADV_LBC_TEND,                   &
     &                      V_ADV_LBC, V_ADV_LBC_TEND,                   &
     &                      W_ADV_LBC, W_ADV_LBC_TEND,                   &
     &                      MURK_LBC, MURK_LBC_TEND,                     &
     &                      DUST_DIV1_LBC, DUST_DIV1_LBC_TEND,           &
     &                      DUST_DIV2_LBC, DUST_DIV2_LBC_TEND,           &
     &                      DUST_DIV3_LBC, DUST_DIV3_LBC_TEND,           &
     &                      DUST_DIV4_LBC, DUST_DIV4_LBC_TEND,           &
     &                      DUST_DIV5_LBC, DUST_DIV5_LBC_TEND,           &
     &                      DUST_DIV6_LBC, DUST_DIV6_LBC_TEND,           &
     &                      SO2_LBC, SO2_LBC_TEND,                       &
     &                      DMS_LBC, DMS_LBC_TEND,                       &
     &                      SO4_AITKEN_LBC, SO4_AITKEN_LBC_TEND,         &
     &                      SO4_ACCU_LBC, SO4_ACCU_LBC_TEND,             &
     &                      SO4_DISS_LBC, SO4_DISS_LBC_TEND,             &
     &                      NH3_LBC, NH3_LBC_TEND,                       &
     &                      SOOT_NEW_LBC, SOOT_NEW_LBC_TEND,             &
     &                      SOOT_AGD_LBC, SOOT_AGD_LBC_TEND,             &
     &                      SOOT_CLD_LBC, SOOT_CLD_LBC_TEND,             &
     &                      BMASS_NEW_LBC, BMASS_NEW_LBC_TEND,           &
     &                      BMASS_AGD_LBC, BMASS_AGD_LBC_TEND,           &
     &                      BMASS_CLD_LBC, BMASS_CLD_LBC_TEND,           &
     &                      OCFF_NEW_LBC, OCFF_NEW_LBC_TEND,             &
     &                      OCFF_AGD_LBC, OCFF_AGD_LBC_TEND,             &
     &                      OCFF_CLD_LBC, OCFF_CLD_LBC_TEND,             &
     &                      NITR_ACC_LBC, NITR_ACC_LBC_TEND,             &
     &                      NITR_DISS_LBC, NITR_DISS_LBC_TEND,           &
     &                      TRACER_LBC, TRACER_LBC_TEND,                 &
     &                      TRACER_UKCA_LBC, TRACER_UKCA_LBC_TEND,       &
     &                      1, 0, ErrorStatus, CMESSAGE)

         End If ! L_update_lbcs

        IF ( L_lbc_new ) Then

        !--------------------------------------------------------------
        !           Update primary fields with LAM LBC data
        !--------------------------------------------------------------

! DEPENDS ON: update_lam_lbcs
        CALL UPDATE_LAM_LBCS(                                           &
     &    r_rho_levels, r_theta_levels,                                 &
     &    ROW_LENGTH,ROWS,N_ROWS,MODEL_LEVELS,WET_LEVELS,               &
     &    TR_VARS,TR_LBC_VARS,TR_LEVELS,                                &
     &    A_MAX_TRVARS,A_tr_active_lbc_index,                           &
     &    TR_UKCA,TR_LBC_UKCA,                                          &
     &    A_MAX_UKCAVARS,UKCA_tr_active_lbc_index,                      &     
     &    OFFX,OFFY,HALO_I,HALO_J,AT_EXTREMITY,                         &
     &    L_mcr_qcf2, L_mcr_qrain, L_mcr_qgraup, L_pc2,                 &
     &    L_mcr_qcf2_lbc, L_mcr_qrain_lbc, L_mcr_qgraup_lbc, L_pc2_lbc, &
     &    L_murk, L_murk_lbc,                                           &
     &    L_LBC_balance, L_int_uvw_lbc,                                 &
     &    L_dust_div1, L_dust_div1_lbc, L_dust_div2, L_dust_div2_lbc,   &
     &    L_dust_div3, L_dust_div3_lbc, L_dust_div4, L_dust_div4_lbc,   &
     &    L_dust_div5, L_dust_div5_lbc, L_dust_div6, L_dust_div6_lbc,   &
     &    L_so2,L_so2_lbc,L_dms,L_dms_lbc,L_so4_aitken,L_so4_aitken_lbc,&
     &    L_so4_accu, L_so4_accu_lbc, L_so4_diss, L_so4_diss_lbc,       &
     &    L_nh3, L_nh3_lbc,                                             &
     &    L_soot_new, L_soot_new_lbc, L_soot_agd, L_soot_agd_lbc,       &
     &    L_soot_cld, L_soot_cld_lbc, L_bmass_new, L_bmass_new_lbc,     &
     &    L_bmass_agd, L_bmass_agd_lbc, L_bmass_cld, L_bmass_cld_lbc,   &
     &    L_ocff_new, L_ocff_new_lbc,                                   &
     &    L_ocff_agd, L_ocff_agd_lbc, L_ocff_cld, L_ocff_cld_lbc,       &
     &    L_nitr_acc, L_nitr_acc_lbc, L_nitr_diss, L_nitr_diss_lbc,     &
     &    RIMWIDTHA(rima_type_norm),RIMWEIGHTSA,                        &
     &    LENRIMA(1,1,rima_type_norm),                                  &
     &    LBC_SIZEA(1,1,1,rima_type_norm),                              &
     &    LBC_STARTA(1,1,1,rima_type_norm),                             &
     &    THETA_LBC, Q_LBC, QCL_LBC,                                    &
     &    QCF_LBC, QCF2_LBC, QRAIN_LBC,                                 &
     &    QGRAUP_LBC, CF_BULK_LBC, CF_LIQUID_LBC,                       &
     &    CF_FROZEN_LBC, RHO_LBC,EXNER_LBC,                             &
     &    U_LBC, V_LBC, W_LBC,                                          &
     &    U_ADV_LBC, V_ADV_LBC, W_ADV_LBC,                              &
     &    MURK_LBC,                                                     &
     &    DUST_DIV1_LBC, DUST_DIV2_LBC, DUST_DIV3_LBC,                  &
     &    DUST_DIV4_LBC, DUST_DIV5_LBC, DUST_DIV6_LBC,                  &
     &    SO2_LBC, DMS_LBC, SO4_AITKEN_LBC,                             &
     &    SO4_ACCU_LBC,SO4_DISS_LBC,NH3_LBC,                            &
     &    SOOT_NEW_LBC, SOOT_AGD_LBC, SOOT_CLD_LBC,                     &
     &    BMASS_NEW_LBC, BMASS_AGD_LBC, BMASS_CLD_LBC,                  &
     &    OCFF_NEW_LBC, OCFF_AGD_LBC, OCFF_CLD_LBC,                     &
     &    NITR_ACC_LBC, NITR_DISS_LBC,                                  &
     &    TRACER_LBC, TRACER_UKCA_LBC,                                  &
     &    QDIMS_L, THETA, Q,QCL, QCF,                                   &
     &    QCF2, QRAIN, QGRAUP,                                          &
     &    CF_BULK, CF_LIQUID, CF_FROZEN,                                &
     &    RHO, EXNER_RHO_LEVELS,                                        &
     &    U, V, W, U_ADV, V_ADV, W_ADV, MURK,                           &
     &    DUST_DIV1, DUST_DIV2, DUST_DIV3,                              &
     &    DUST_DIV4, DUST_DIV5, DUST_DIV6,                              &
     &    SO2, DMS, SO4_AITKEN,SO4_ACCU,                                &
     &    SO4_DISS, NH3,                                                &
     &    SOOT_NEW, SOOT_AGD, SOOT_CLD,                                 &
     &    BMASS_NEW, BMASS_AGD, BMASS_CLD,                              &
     &    OCFF_NEW, OCFF_AGD, OCFF_CLD,                                 &
     &    NITR_ACC, NITR_DISS,                                          &     
     &    DELTA_PHI, DELTA_LAMBDA,                                      &
     &    BASE_PHI, BASE_LAMBDA,                                        &
     &    DATASTART, lat_rot_NP,                                        &
     &    GLOBAL_ROW_LENGTH, GLOBAL_ROWS,                               &
     &    TRACER, TRACER_UKCA  )

        END IF ! L_lbc_new
! DEPENDS ON: TIMER
            IF (ltimer) CALL timer('AS LAM_LBCS',6)

      ENDIF     !   model_domain  ==  mt_lam

! Clear workspace
        DEALLOCATE (dtheta_dr_term)

      End If !  ErrorStatus == 0

      Enddo ! end iterations for trajectory calc

      If ( ErrorStatus == 0 ) Then

! DEPENDS ON: Atm_Step_alloc
       CALL Atm_Step_alloc( &
#include "arg_atm_fields.h"
            cf_star, cfl_star, cff_star, q_star, qcl_star, qcf_star, &
            frac_control, r_u, r_v, r_w, errorstatus,'lbc_updt')

      End If        !  ErrorStatus  ==  0

! ----------------------------------------------------------------------
! section 6.2  Check for q below qlimit and reset
! ----------------------------------------------------------------------

      If(L_qpos)then

! DEPENDS ON: q_pos_ctl
        call Q_Pos_Ctl(  Q, row_length, rows, wet_levels, 1, bl_levels, &
                         global_row_length, global_rows,                &
                         mype, nproc, halo_i, halo_j,                   &
                         model_domain,                                  &
                         halo_type_extended, q_pos_method, qlimit,      &
                         l_qpos_diag_pr, qpos_diag_limit,               &
                         'Q call from atm_step')
!
        If (L_pc2) then
! For the PC2 cloud scheme we also need to limit the liquid

! DEPENDS ON: q_pos_ctl
          call Q_Pos_Ctl(QCL, row_length, rows, wet_levels, 1,          &
                         bl_levels, global_row_length, global_rows,     &
                         mype, nproc, halo_i, halo_j,                   &
                         model_domain,                                  &
                         halo_type_extended, q_pos_method, 0.0,         &
                         l_qpos_diag_pr, qpos_diag_limit,               &
                        'QCL call from atm_step')
!
       End If       ! L_pc2
!
      End If       ! L_qpos

! ----------------------------------------------------------------------
! Section 7.0 Mean all polar variables on a level to remove deviation
!             of values due to rounding error.
!             Only called every polar_reset_timesteps
! ----------------------------------------------------------------------
      If (ErrorStatus  ==  0 .and. model_domain  ==  mt_global           &
     &    .and. L_polar_reset) Then

! need if test on correct timestep
        If (Timestep_Number  ==  1 .or. (                                &
     &      (Timestep_number-1)/polar_reset_timesteps*                   &
     &      polar_reset_timesteps  ==  Timestep_number-1 ) )             &
     &    Then

! DEPENDS ON: polar_reset_mean
          Call Polar_Reset_Mean(                                         &
     &                      EXNER_RHO_LEVELS,RHO,                        &
     &                      THETA,W,                                     &
     &                      Q, QCL, QCF,                                 &
     &                      CF_BULK, CF_LIQUID,                          &
     &                      CF_FROZEN,                                   &
     &                      row_length, rows, model_levels,              &
     &                      wet_levels, global_row_length,               &
     &                      offx, offy, halo_i, halo_j,                  &
     &                      nproc, nproc_y, gc_proc_row_group,           &
     &                      at_extremity)
        End If
      End If

! ------------------------------------------------------------------
! Section 17  Aerosol Modelling - includes Sulphur Cycle, soot, biomass
! and fossil-fuels organic carbon (OCFF) 
! ------------------------------------------------------------------
!
! DEPENDS ON: timer
      IF (Ltimer) CALL TIMER('AS Aerosol Modelling',5)
!
      IF (L_SULPC_SO2.OR.L_SOOT.OR.L_BIOMASS.OR.                         &
          L_OCFF.OR.L_NITRATE.OR.L_DUST) THEN
!
! Allocate diagnostic space for STASH
        ALLOCATE (STASHwork17(STASH_maxlen(17,A_im)))
!
! Don't call Swap_bounds for fields used in Aero_Ctl
!
        ALLOCATE(O3_MMR  (tdims%i_start:tdims%i_end,                     &
                          tdims%j_start:tdims%j_end,                     &
                          tdims%k_start:tdims%k_end))
        ALLOCATE(HNO3_MMR(tdims%i_start:tdims%i_end,                     &
                          tdims%j_start:tdims%j_end,                     &
                          tdims%k_start:tdims%k_end))
        ALLOCATE(H2O2_MMR(tdims%i_start:tdims%i_end,                     &
                          tdims%j_start:tdims%j_end,                     &
                          tdims%k_start:tdims%k_end))
        ALLOCATE(OH_conc (tdims%i_start:tdims%i_end,                     &
                          tdims%j_start:tdims%j_end,                     &
                          tdims%k_start:tdims%k_end))
        ALLOCATE(HO2_conc(tdims%i_start:tdims%i_end,                     &
                          tdims%j_start:tdims%j_end,                     &
                          tdims%k_start:tdims%k_end)) 
!

        CALL GET_SULPC_OXIDANTS(                                         &
     & L_SULPC_ONLINE_OXIDANTS, L_UKCA, L_UKCA_TROP, L_UKCA_TROPISOP,    &
     & L_UKCA_STRATTROP, l_ukca_raq, first_atmstep_call,                 &
#include "arg_atm_fields.h"
#include "argd1.h"
     & O3_MMR, HNO3_MMR, H2O2_MMR, OH_conc, HO2_conc)
!
        CALL AERO_CTL(                                                   &
! Parallel variables
     &  halo_i, halo_j, offx, offy, global_row_length, global_rows       &
     &, gc_proc_row_group, gc_proc_col_group                             &
     &, at_extremity, nproc, nproc_x, nproc_y                            &
     &, neighbour, g_rows, g_row_length, mype                            &
! model dimensions
     &, row_length, rows, n_rows, land_field                             &
     &, model_levels, wet_levels, bl_levels, n_cca_lev                   &
     &, theta_field_size                                                 &
     &, salt_dim1, salt_dim2, salt_dim3                                  &
     &, aero_dim1, aero_dim2, aero_dim3                                  &
! Model switches
     &, model_domain, LCAL360, L_SEC_VAR, L_EqT, Ltimer                  &
! Model parameters
     &, Ntot_land, Ntot_sea                                              &

! Co-ordinate information
     &, delta_lambda, delta_phi                                          &
     &, lat_rot_NP, long_rot_NP                                          &
! Time stepping information
     &, timestep                                                         &
     &, I_year, I_day_number, I_hour, I_minute                           &
     &, I_second, timestep_number                                        &
     &, PREVIOUS_TIME                                                    &
     &, CALL_CHEM_FREQ                                                   &
! Trig arrays
     &, sin_theta_longitude, cos_theta_longitude                         &
     &, FV_cos_theta_latitude                                            &
! Grid-dependent arrays
     &,     f3_at_u, true_longitude, true_latitude                       &
!
! Data fields IN
     &, U, V, TSTAR, TSTAR_SEA                                           &
     &, THETA, Q, QCL, QCF                                               &
     &, RHO, LAND, FRAC_LAND, P_THETA_LEVELS                             &
     &, EXNER_RHO_LEVELS, EXNER_THETA_LEVELS                             &
     &, ICE_FRACTION, SNODEP                                             &
     &, CF_BULK                                                          &
     &, OH_conc, H2O2_MMR, HO2_conc, O3_MMR, HNO3_MMR                    &
     &, SO2_EM, SO2_HILEM, SO2_NATEM                                     &
     &, DMS_EM, DMS_CONC, NH3_EM                                         &
     &, DMS_OFLUX                                                        &
     &, SOOT_EM, SOOT_HILEM, BMASS_EM, BMASS_HILEM, OCFF_EM, OCFF_HILEM  &
     &, land_index                                                       &
! Logicals IN
     &, L_SULPC_SO2, L_SULPC_DMS, L_SULPC_OZONE                          &
     &, L_SULPC_SO2_O3_NONBUFFERED, L_SULPC_NH3                          &
     &, L_SULPC_ONLINE_OXIDANTS                                          &
     &, L_SULPC_2_WAY_COUPLING                                           &
     &, L_use_sulphate_sulpc, L_use_seasalt_sulpc, L_SOOT                &
     &, l_use_soot_sulpc, l_biomass, l_use_bmass_sulpc                   &
     &, l_ocff, l_use_ocff_sulpc                                         &
     &, l_nitrate, l_use_nitrate_sulpc                                   &
     &, L_SO2_SURFEM, L_SO2_HILEM, L_SO2_NATEM, L_DMS_EM                 &
     &, L_DMS_em_inter, L_DMS_Liss_Merlivat                              &
     &, L_DMS_Wanninkhof, L_DMS_Nightingale                              &
     &, L_DMS_Ointer                                                     &
     &, L_NH3_EM, L_CTILE                                                &
     &, L_SOOT_SUREM, L_SOOT_HILEM, L_BMASS_SUREM, L_BMASS_HILEM         &
     &, L_OCFF_SUREM, L_OCFF_HILEM, L_USE_BIOGENIC                       &
     &, L_USE_SEASALT_DIRECT, L_USE_SEASALT_INDIRECT                     &
     &, L_USE_SEASALT_AUTOCONV, L_USE_SEASALT_PM, L_DUST                 &
!
! Data fields IN/OUT
     &, SO2, DMS                                                         &
     &, SO4_AITKEN, SO4_ACCU, SO4_DISS                                   &
     &, H2O2, NH3                                                        &
     &, SOOT_NEW, SOOT_AGD, SOOT_CLD                                     &
     &, BMASS_NEW, BMASS_AGD, BMASS_CLD                                  &
     &, OCFF_NEW, OCFF_AGD, OCFF_CLD                                     &
     &, biogenic                                                         &
     &, NITR_ACC, NITR_DISS                                              &
!
! Data fields IN
     &, DUST_DIV1, DUST_DIV2, DUST_DIV3, DUST_DIV4, DUST_DIV5, DUST_DIV6 &
!
! Data fields OUT
! Diagnostic info
     &,                                                                  &
#include "argsts.h"
     &  STASHwork17                                                      &
! Error info
     &, ErrorStatus )


      IF(L_SULPC_ONLINE_OXIDANTS .AND. L_SULPC_2_WAY_COUPLING) THEN 
! 
        CALL WRITE_SULPC_OXIDANTS(                                       &
     &         OH_conc, H2O2_MMR, HO2_conc, O3_MMR, HNO3_MMR,            &
#include "arg_atm_fields.h" 
#include "argd1.h" 
     &  first_atmstep_call  ) 
! 
      ENDIF 
!
! Don't call Swap_bounds for updated fields
!
! Diagnostics STASHed for Aerosol section 17
!
! DEPENDS ON: stash
        CALL STASH(a_sm,a_im,17,STASHwork17,                             &
#include "argd1.h"
#include "argdumga.h"
#include "argsts.h"
     &    ErrorStatus,Cmessage)
!
        DEALLOCATE (STASHwork17)
!
        IF(ALLOCATED(O3_MMR)) DEALLOCATE(O3_MMR)
        IF(ALLOCATED(HNO3_MMR)) DEALLOCATE(HNO3_MMR)
        IF(ALLOCATED(H2O2_MMR)) DEALLOCATE(H2O2_MMR)
        IF(ALLOCATED(OH_conc)) DEALLOCATE(OH_conc)
        IF(ALLOCATED(HO2_conc)) DEALLOCATE(HO2_conc)

      END IF         ! END L_SULPC_SO2.OR.L_SOOT TEST
!
! DEPENDS ON: timer
      IF (Ltimer) CALL TIMER('AS Aerosol Modelling',6)
!
! ----------------------------------------------------------------------
! Section 8.0 Update pressure to value at new-time level.
!             Calculate surface pressure, and exner and pressure at
!             theta levels.
!             If old lbcs then need to update exner on boundaries first.
! ----------------------------------------------------------------------
      If (ErrorStatus  ==  0 .and. model_domain  ==  mt_lam) Then

      If ( L_lbc_old ) Then

! Calculate EXNER_LBCs at next time level

        CALL Atm_Step_phys_init( &
#include "arg_atm_fields.h"
#include "argsts.h"
            r_v, r_u, theta_star, q_star, qcl_star, qcf_star,  &
            cf_star, cfl_star, cff_star, rho_km, cH, exner_lbc_real_tend, &
            w_lbc_real_tend, errorstatus, 'bc_exner')

! DEPENDS ON: set_lateral_boundaries
        CALL SET_LATERAL_BOUNDARIES(ROW_LENGTH,ROWS,Offx,Offy,          &
     &    MODEL_LEVELS+1,fld_type_p,EXNER_RHO_LEVELS,                   &
     &    LENRIMA(fld_type_p,halo_type_extended,rima_type_norm),        &
     &    LBC_SIZEA(1,fld_type_p,halo_type_extended,rima_type_norm),    &
     &    LBC_STARTA(1,fld_type_p,halo_type_extended,rima_type_norm),   &
     &    halo_i, halo_j,                                               &
     &    EXNER_LBC_REAL_TEND,                                          &
     &    RIMWIDTHA(rima_type_norm),n_rims_to_do,                       &
     &    RIMWEIGHTSA,AT_EXTREMITY,                                     &
     &    L_do_boundaries,L_do_halos)

       End If ! L_lbc_old

      Endif      ! ErrorStatus  ==  0 .and. model_domain  ==  mt_lam

! DEPENDS ON: timer
      If (Ltimer) CALL TIMER('AS Updates',5)

      If (ErrorStatus  ==  0) Then

! Check for negative pressure if requested to
        If (Instability_diagnostics  >   0) Then
          Do k = 1, model_levels+1
            Do j = 1, rows
              Do i = 1, row_length
                ij = i+offx + (j+offy-1) * (row_length+2*offx)
                If (exner_rho_levels(((k-1)*theta_off_size)+ij) <  0.) Then
                  ErrorStatus = 123
                End If
              End Do
            End Do
          End Do
! ErrorStatus 123 message now generated outside of the loops.
          if ( ErrorStatus == 123 ) THEN

            Call Ereport("ATM_STEP", ErrorStatus,                        &
     &           "Negative pressure value about to be created" )
          endif
        End If       !    Instability_diagnostics  >   0

      End If      ! ErrorStatus  ==  0

      If ( ErrorStatus  ==  0) Then
! DEPENDS ON: consistent_pressure
        call     Consistent_Pressure ( exner_rho_levels,                &
     &           offx,offy,halo_i,halo_J,                               &
     &           row_length,rows,model_levels,                          &
     &           r_theta_levels, r_rho_levels, rho,                     &
     &           p, pstar, p_theta_levels,exner_theta_levels)
!
        IF (L_physics) then
!
! Are we using the PC2 cloud scheme?
!
        IF (L_pc2) THEN

! ----------------------------------------------------------------------
! PC2: Calculate condensation due to changes in temperature resulting
!    from adiabatic changes in pressure (mainly from vertical advection)
!    Also call checking routine from within this subroutine.
! ----------------------------------------------------------------------

          CALL Atm_Step_alloc( &
#include "arg_atm_fields.h"
              cf_star, cfl_star, cff_star, q_star, qcl_star, qcf_star, &
              frac_control, r_u, r_v, r_w, errorstatus, 'allocPC2')

! DEPENDS ON: pc2_pressure_forcing
          Call pc2_pressure_forcing (                                   &
     &              p, pstar, p_theta_levels,                           &
     &              rhc_row_length, rhc_rows,                           &
     &              timestep, rhcpt, THETA, CF_BULK,                    &
     &              CF_LIQUID, CF_FROZEN,                               &
     &              Q, QCL,QCF,                                         &
     &              exner_star, EXNER_THETA_LEVELS,                     &
     &              CCB, CUMULUS, rhts, tlts, qtts, ptts,CF_AREA(1,1,1),&
     &              t_inc_pres, q_inc_pres, qcl_inc_pres, qcf_inc_pres, &
     &              cf_inc_pres, cfl_inc_pres,                          &
     &              cff_inc_pres, t_dini, q_dini, qcl_dini, qcf_dini,   &
     &              cf_dini, cfl_dini, cff_dini, l_mr_pc2, l_acf_cusack,&
     &              l_cld_area)

          DEALLOCATE(rhts)
          DEALLOCATE(tlts)
          DEALLOCATE(qtts)
          DEALLOCATE(ptts)
        END IF  ! L_pc2

! ----------------------------------------------------------------------
! Add ability to get increments from qt_bal_cld call and output in section 15
! ----------------------------------------------------------------------

        CALL Atm_Step_diag( &
#include "arg_atm_fields.h"
#include "argsts.h"
            r_v, r_u, r_w, q_star, qcl_star, qcf_star,  theta_star, &
            cf_star, cfl_star, cff_star, 8)

        IF ( (.not. L_pc2) .or. l_pc2_reset ) THEN
! ----------------------------------------------------------------------
! Call cloud scheme to make cloud consistent with moisture fields
! ----------------------------------------------------------------------

! DEPENDS ON: qt_bal_cld
          call qt_bal_cld( PSTAR,P_THETA_LEVELS,P,                      &
     &        THETA,EXNER_THETA_LEVELS,                                 &
     &        Q,QCL,QCF,QCF2,                                           &
     &        rhcpt, rhc_row_length, rhc_rows, bl_levels,               &
     &        delta_lambda, delta_phi,                                  &
     &        FV_cos_theta_latitude,                                    &
     &        L_cld_area, L_ACF_Cusack, L_ACF_Brooks,                   &
     &        L_mcr_qcf2, l_mr_qtbalcld,                                &
     &        ntml, cumulus, CF_AREA, CF_BULK, CF_LIQUID, CF_FROZEN,    &
     &        mype)

          ! calculate changes to T , q etc
          CALL Atm_Step_diag( &
#include "arg_atm_fields.h"
#include "argsts.h"
            r_v, r_u, r_w, q_star, qcl_star, qcf_star,  theta_star, &
            cf_star, cfl_star, cff_star, 9)

          END IF  ! L_pc2 and L_pc2_reset

          IF (L_run_with_physics2) DEALLOCATE ( RHCPT )

        END IF   ! L_physics
! DEPENDS ON: timer
      If (Ltimer) CALL TIMER('AS Updates',6)

      END IF      !  ErrorStatus  ==  0

! ----------------------------------------------------------------------
! Section 9.0a Calculate Mass and energy of atmosphere if required
!              using end of timestep values
!             Only done every energy correction period.
! ----------------------------------------------------------------------
! DEPENDS ON: timer
          IF(ltimer) CALL TIMER('AS Energy mass   ',5)
      IF (L_EMCORR) THEN

! Set energy correction for use in section 30
        energy_corr_now=A_REALHD(rh_energy_corr)

        IF (LENERGY) THEN
! zero fields to be calculated

          tot_energy_final = 0.0
          tot_dry_mass_final = 0.0
          tot_moist_final = 0.0

          CALL eng_mass_diag (                                           &
! Parallel variables
     &                      wet_levels                                   &
     &,                     delta_lambda,delta_phi                       &
     &,                     THETA , U, V                                 &
     &,                     W, RHO , Q                                   &
     &,                     QCL, QCF                                     &
     &,                     wet_to_dry_n                                 &
     &,                     EXNER_THETA_LEVELS                           &
!     &,                     d1(jexner_rho_levels(1))
!     &,                     d1(jp(1)), d1(jp_theta_levels(1))
!     &,                     d1(jpstar)
! sum of moist fluxes
     &,                     NET_MFLUX                                    &
     &,                     A_REALHD(rh_tot_mass_init)                   &
     &,                     A_REALHD(rh_tot_m_init)                      &
! logical to indicate mass and moist correction required
     &,                     Lmass_corr,Lqt_corr,Lemq_print               &
! energy correction timestep info
     &,                     a_energysteps,timestep                       &
! IN/OUT  results from calculations
     &,                     tot_energy_final,tot_dry_mass_final          &
     &,                     tot_moist_final)

! DEPENDS ON: cal_eng_mass_corr
          Call cal_eng_mass_corr (                                       &
     &                      delta_lambda, delta_phi                      &
     &,                     a_energysteps                                &
     &,                     NET_FLUX                                     &
     &,                     A_REALHD(rh_tot_mass_init)                   &
     &,                     A_REALHD(rh_tot_energy_init)                 &
     &,                     A_REALHD(rh_energy_corr)                     &
     &,                     tot_energy_final )

! Swap initial energy and final energy.

          A_REALHD(rh_tot_energy_init) = tot_energy_final

! Swap initial moisture and final moisture.

          A_REALHD(rh_tot_m_init) = tot_moist_final


        ENDIF   ! LENERGY

      ELSE
! Set energy correction for use in section 30
        energy_corr_now=0.0
      ENDIF     ! L_EMCORR
! DEPENDS ON: timer
          IF(ltimer) CALL TIMER('AS Energy mass   ',6)

! DEPENDS ON: timer 
        IF (ltimer) CALL TIMER('AS IAU',5) 
!---------------------------------------------------------------------- 
! Incremental Analysis Update (IAU). 
!---------------------------------------------------------------------- 
 
      IF (L_IAU .AND. STEPim(a_im) >= IAU_FirstCallTS .AND. & 
                      STEPim(a_im) <= IAU_LastCallTS) THEN 
 
! DEPENDS ON: IAU
        CALL IAU (                                      & 
#include "arglndm.h"
                   l_mr_iau,                            & ! in 
                   u,      v,          w,               & ! inout 
                   u_adv,  v_adv,      w_adv,           & ! inout 
                   theta,  rho,        murk,            & ! inout 
                   q,      qCL,        qCF,             & ! inout 
                   TStar,  TStar_tile, Deep_soil_temp,  & ! inout 
                   smcl,   tstar_anom,                  & ! inout 
                   Pstar,  p,                           & ! inout 
                   p_theta_levels,                      & ! inout 
                   exner_rho_levels,                    & ! inout 
                   exner_theta_levels,                  & ! inout 
                   snodep,                              & ! inout 
                   cf_area,                             & ! inout 
                   cf_bulk,                             & ! inout 
                   cf_liquid,                           & ! inout 
                   cf_frozen,                           & ! inout 
                   dust_div1, dust_div2, dust_div3,     & ! inout
                   dust_div4, dust_div5, dust_div6,     & ! inout
                   ozone_tracer )                         ! inout 

 
      END IF 
 
! DEPENDS ON: timer 
        IF (LTIMER) CALL TIMER('AS IAU',6)       

!
! Are we using the PC2 cloud scheme to determine area cloud fraction?
!
        If (L_pc2 .and. .not. L_pc2_reset) then
          If (.not. L_cld_area) then
!
! ----------------------------------------------------------------------
! PC2: Set area cloud fraction to the bulk cloud fraction. Use the
!    D1 arrays directly
! ----------------------------------------------------------------------
!
          Do k = 1, wet_levels
            Do j = j_start, j_stop
              Do i = i_start, i_stop
                CF_AREA(i,j,k) = CF_BULK(i,j,k)
              End do
            End do
          End do

          Else If (L_cld_area) then

            If (L_ACF_Brooks) then
              Allocate ( cf_bulk_nohalo(row_length,rows,wet_levels) )
              Allocate ( cf_liquid_nohalo(row_length,rows,wet_levels) )
              Allocate ( cf_frozen_nohalo(row_length,rows,wet_levels) )

! Place bulk, liquid and frozen cloud fractions in halo-free arrays
! Use indexing over the full row and row_length (including any LAM
! boundary rim) since the call to ls_acf_brooks uses this indexing.
              Do k = 1, wet_levels
                Do j = 1, rows
                  Do i = 1, row_length
                    cf_bulk_nohalo(i,j,k)   = cf_bulk(i,j,k)
                    cf_liquid_nohalo(i,j,k) = cf_liquid(i,j,k)
                    cf_frozen_nohalo(i,j,k) = cf_frozen(i,j,k)
                  End Do
                End Do
              End Do

! DEPENDS ON: ls_acf_brooks
              Call LS_ACF_Brooks (                                      &
                 delta_lambda, delta_phi,                               &
                 FV_cos_theta_latitude,                                 &
                 cf_bulk_nohalo, cf_liquid_nohalo,                      &
                 cf_frozen_nohalo, cumulus, CF_AREA )

              Deallocate ( cf_bulk_nohalo )
              Deallocate ( cf_liquid_nohalo )
              Deallocate ( cf_frozen_nohalo )

            End If ! L_ACF_Brooks

          End If ! L_cld_area
!
        End if  ! L_pc2 .and. .not. L_pc2_reset

! Nudging with analysis data
        IF ( L_nudging ) THEN 
 
          IF ( model_domain /= mt_global ) THEN 
            ErrorStatus = 39 
            cmessage = 'Nudging not yet tested for Limited Area Model' 
 
            CALL ereport('ATM_STEP',ErrorStatus,cmessage) 
          END IF 

! Allocate stashwork array
          CALL Atm_Step_diag(                                        &
#include "arg_atm_fields.h"
#include "argsts.h"
             r_v, r_u, r_w, q_star, qcl_star, qcf_star,  theta_star, &
            cf_star, cfl_star, cff_star, 39)

! DEPENDS ON: nudging_main1         
          CALL nudging_main1 (                                       & 
#include "argd1.h" 
 
! time stepping information. 
           timestep, i_year, i_month, i_day, i_hour, i_minute        & 
           , i_second, timestep_number                               & 
 
! in data fields. 
           , theta, u, v, p, exner_theta_levels, p_theta_levels,     & 
! out updated STASH 
#include "argsts.h" 
           STASHwork39) 

! Swap bounds for modified fields
!Potential temperature
! DEPENDS ON: swap_bounds
      CALL Swap_Bounds(theta, row_length, rows, model_levels,        &
            offx, offy, fld_type_p, .FALSE.)

! Zonal wind
! DEPENDS ON: swap_bounds
      CALL swap_bounds( u, row_length, rows, model_levels,           & 
            offx, offy, fld_type_u, .TRUE.) 

! Meridional wind
! DEPENDS ON: swap_bounds
      CALL swap_bounds( v, row_length, n_rows, model_levels,         & 
            offx, offy, fld_type_v, .TRUE.) 

! Copy diagnostics into main D1 aaray 
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS STASH',5)
! DEPENDS ON: Atm_Step_stash
          CALL Atm_Step_stash(                                       &
#include "argd1.h"
#include "argduma.h"
#include "arg_atm_fields.h"
#include "argsts.h"
            errorstatus,39)
! DEPENDS ON: timer
      IF (Ltimer) CALL timer('AS STASH',6)
          
        END IF                ! L_nudging 
! ----------------------------------------------------------------------
! Section 9.0 Diagnostics at end of timestep
! ----------------------------------------------------------------------
! Calculation of total increments
! NOTE - this must be after all processes which alter model prognostics
! ----------------------------------------------------------------------

! DEPENDS ON: timer
      IF(ltimer) CALL TIMER('AS End TStep Diags',5)

        CALL Atm_Step_diag( &
#include "arg_atm_fields.h"
#include "argsts.h"
            r_v, r_u, r_w, q_star, qcl_star, qcf_star,  theta_star, &
            cf_star, cfl_star, cff_star, 10)


! section 15: 'dynamics' based quantities
      IF(      SF(0,15) .AND. ErrorStatus == 0) THEN

! DEPENDS ON: st_diag1
        CALL St_diag1(STASH_maxlen(15,A_im),                             &
#include "argd1.h"
#include "argduma.h"
#include "argsts.h"
#include "arg_atm_fields.h"
#include "arglndm.h"
       t_incr_diagnostic,q_incr_diagnostic,qcl_incr_diagnostic,          &
     & ErrorStatus,CMessage)

      ENDIF ! SF(0,15)

! Cleanup arrays holding increments

      IF (L_physics .and. ErrorStatus == 0) THEN
        DEALLOCATE(t_incr_diagnostic)
        DEALLOCATE(q_incr_diagnostic)
        DEALLOCATE(qcl_incr_diagnostic)
      ENDIF

! section 16: 'physics' based quantities
      IF(      SF(0,16) .AND. ErrorStatus == 0) THEN

! DEPENDS ON: st_diag2
        CALL St_diag2(STASH_maxlen(16,A_im),                             &
#include "argd1.h"
#include "argduma.h"
#include "argsts.h"
#include "arg_atm_fields.h"
#include "arglndm.h"
     & t_inc_pres, q_inc_pres, qcl_inc_pres, qcf_inc_pres,              & 
     & cf_inc_pres, cfl_inc_pres, cff_inc_pres,                         & 
     & t_dini, q_dini, qcl_dini, qcf_dini,                              & 
     & cf_dini, cfl_dini, cff_dini,                                     & 
     & ErrorStatus,CMessage)

      ENDIF !   SF(0,16)

      IF (L_pc2 .and. ErrorStatus == 0 .and. l_physics) then

        CALL Atm_Step_alloc( &
#include "arg_atm_fields.h"
             cf_star, cfl_star, cff_star, q_star, qcl_star, qcf_star, &
             frac_control, r_u, r_v, r_w, errorstatus, 'dllocPC2')

      End If  ! L_pc2 and ErrorStatus == 0

! section 30: climate diagnostics
      IF(      SF(0,30) .AND. ErrorStatus == 0) THEN
! size of diagnostic space
        ALLOCATE (STASHwork30(STASH_maxlen(30,A_im)))
! DEPENDS ON: st_diag3
        CALL St_diag3(STASHwork30,STASH_maxlen(30,A_im),                 &
#include "argd1.h"
#include "argduma.h"
#include "argsts.h"
#include "arg_atm_fields.h"
     &    energy_corr_now,                                               &
     &    inc_u, inc_v, inc_w, inc_t,                                    &
     &    inc_q, inc_qcl, inc_qcf,                                       &
     &    inc_rho,sin_v_latitude,                                        &
          inc_qrain, inc_qgraup, inc_qcf2,                               &
          wet_to_dry_n,                                                  &
     &    ErrorStatus,CMessage)

        DEALLOCATE (STASHwork30) ! Clear space
        DEALLOCATE (inc_q)
        DEALLOCATE (inc_qcl)
        DEALLOCATE (inc_qcf)
        DEALLOCATE (inc_qrain)
        DEALLOCATE (inc_qgraup)
        DEALLOCATE (inc_qcf2)

      ENDIF ! SF(0,30)

      If( ErrorStatus == 0) then
        DEALLOCATE (inc_rho)
        DEALLOCATE (inc_t)
        DEALLOCATE (inc_u)
        DEALLOCATE (inc_v)
        DEALLOCATE (inc_w)
      Endif

! Check error condition
      IF(ErrorStatus >  0) THEN
 
         CALL Ereport(RoutineName,ErrorStatus,Cmessage)
      ENDIF

! DEPENDS ON: timer
      IF(ltimer) CALL TIMER('AS End TStep Diags',6)

! DEPENDS ON: timer
      IF(ltimer) CALL TIMER('AS STASH',5)
! DEPENDS ON: Atm_Step_stash
      CALL Atm_Step_stash( &
#include "argd1.h"
#include "argduma.h"
#include "arg_atm_fields.h"
#include "argsts.h"
               errorstatus, 4)
! DEPENDS ON: timer
      IF(ltimer) CALL TIMER('AS STASH',6)
 
! ---------------------------------------------------------------
! Section 9.1  Optional diagnostic printing of w, divergence
!               lapse rate and bottom level theta
! ---------------------------------------------------------------
       if( L_diag_print .and. L_diag_print_ops ) then
! DEPENDS ON: print_ops_diag
         Call Print_ops_diag( W, THETA, row_length, rows,               &
     &                  model_levels, model_domain,                     &
     &                  global_row_length, global_rows,                 &
     &                  offx, offy, halo_i, halo_j,                     &
     &                  mype, nproc, gc_proc_row_group,                 &
     &                  timestep_number, print_step, diag_interval,     &
     &                  rpemax, rpemin, ipesum,                         &
     &                  L_print_pe, L_print_wmax, L_print_theta1,       &
     &                  max_w_run(1:), min_theta1_run,                  &
     &                  time_w_max, time_theta1_min )
! DEPENDS ON: um_fort_flush
         if ( L_flush6 ) CALL um_fort_flush(6,info)
       elseif( L_diag_print ) then
! DEPENDS ON: print_diag
         Call Print_diag( U, V, THETA, RHO, W, Q, QCL, QCF,             &
     &                  rows, n_rows, row_length,                       &
     &                  model_levels, wet_levels, model_domain,         &
     &                  global_row_length, global_rows,                 &
     &                  r_theta_levels, r_rho_levels,                   &
     &                  r_at_u, r_at_v ,                                &
     &                  FV_sec_theta_latitude, FV_cos_theta_latitude,   &
     &                  cos_v_latitude,                                 &
     &                  cos_theta_longitude, sin_theta_longitude,       &
     &                  offx, offy, halo_i, halo_j,                     &
     &                  mype, nproc, at_extremity, datastart,           &
     &                  gc_proc_row_group, delta_lambda, delta_phi,     &
     &                  timestep_number, print_step, diag_interval,     &
     &                  rpemax, rpemin, ipesum, rpesum, w_print_limit,  &
     &                  L_print_pe, L_print_w,                          &
     &                  L_print_wmax, L_print_max_wind,                 &
     &                  L_print_div, L_print_lapse, L_print_theta1,     &
     &                  L_print_shear, L_diag_wind, L_diag_noise,       &
     &                  max_w_run(1:), max_wind_run, min_theta1_run,    &
     &                  dtheta1_run, max_div_run, min_div_run,          &
     &                  min_lapse_run, max_shear_run, time_max_shear,   &
     &                  time_div_max, time_div_min, time_lapse_min,     &
     &                  time_w_max, time_max_wind, time_theta1_min,     &
     &                  max_KE_run, min_KE_run, max_noise_run,          &
     &                  time_KE_max, time_KE_min, time_noise_max )
! DEPENDS ON: um_fort_flush
         if ( L_flush6 ) CALL um_fort_flush(6,info)
       endif     !  L_diag_print

! Logical first_atmstep_call is true on the first call to ATM_STEP
! and is set to false at the end of ATM_STEP (uses the SAVE command)

        first_atmstep_call = .false.

! DEPENDS ON: timer
      If (Ltimer) Call timer('Atm_Step (AS)',6)
      IF (lhook) CALL dr_hook('ATM_STEP',zhook_out,zhook_handle)
      RETURN

      CONTAINS
        SUBROUTINE prfldincnorm (l_full)
      USE nlstcall_mod, ONLY : ldump, &
                               ltimer
          USE nlstcall_mod, ONLY : ldump, &
                               ltimer

      USE chsunits_mod, ONLY : nunits

          IMPLICIT NONE
        ! Internal routine so no DR HOOK or local variables required.
        LOGICAL :: l_full ! T if Prognostic, F if increments.
        
        IF( norm_lev_start == norm_lev_end ) THEN
          DO k =1, model_levels
! DEPENDS ON: array_l2_print
          CALL array_l2_print(                                            &
                             exner_rho_levels, rho,                       &
                             u, v, w,                                     &
                             u_adv, v_adv, w_adv,                         &
                             theta, q, qcl, qcf,                          &
                             R_u, R_v, R_w, theta_star,                   &
                             q_star, qcl_star, qcf_star,                  &
                             row_length, rows, n_rows, n_rims_to_do,      &
                             model_levels, wet_levels,                    &
                             k, k,                                        &
                             offx, offy, halo_i, halo_j, mype,            &
                             .false., L_do_rims, l_full, L_print_pe )
          END DO !  k =1, model_levels
        ELSE ! norms from norm_lev_start to norm_lev_end
! DEPENDS ON: array_l2_print
          CALL array_l2_print(                                            &
                             exner_rho_levels, rho,                       &
                             u, v, w,                                     &
                             u_adv, v_adv, w_adv,                         &
                             theta, q, qcl, qcf,                          &
                             R_u, R_v, R_w, theta_star,                   &
                             q_star, qcl_star, qcf_star,                  &
                             row_length, rows, n_rows, n_rims_to_do,      &
                             model_levels, wet_levels,                    &
                             norm_lev_start, norm_lev_end,                &
                             offx, offy, halo_i, halo_j, mype,            &
                             .false., L_do_rims, l_full, L_print_pe )
        ENDIF !  norm_lev_start == norm_lev_end
! DEPENDS ON: um_fort_flush
        IF ( L_flush6 ) CALL um_fort_flush(6,info)
        RETURN
        END SUBROUTINE prfldincnorm
      END SUBROUTINE Atm_Step
