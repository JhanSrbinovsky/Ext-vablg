! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!
! Subroutine Interface:
MODULE eg_info_file_mod
IMPLICIT NONE
CONTAINS
SUBROUTINE eg_info_file (mype,nproc_x,nproc_y,numcycles,innits,              &
                  global_row_length,global_rows,model_levels,                &
                  ih,height_domain,l_shallow,l_rotating,l_rotate_grid,       &
                  gcr_tol_abs,gcr_precon_option,                             &
                  l_inc_solver, l_RK_dps,                                    &
                  l_accel_convergence,l_eliminate_rho,                       &
                  l_init_fnm1,                                               &
                  alpha_u,alpha_v,alpha_w,alpha_theta,                       &
                  alpha_rho,alpha_p,l_rotate_winds,l_baroclinic,l_solid_body,&
                  l_baro_inst,l_baro_perturbed,                              &
                  l_deep_baro_inst, T0_E, T0_P, b_const, k_const,            &
                  L_const_grav,l_expl_horz_drag,                             &
                  l_impl_horz_drag,l_eg_dry_static_adj,l_fix_mass,           &
                  L_conserv_smooth_lap,                                      &
                  eg_vert_damp_coeff,eg_vert_damp_profile,eta_s,grid_np_lon, &
                  grid_np_lat,aa_jet_u0,l_cartesian,surface_type,            &
                  grid_number,tprofile_number,trefer_number,t_surface_in,    &
                  h_o,lambda_fraction,phi_fraction,half_width_x,half_width_y)

USE parkind1,     ONLY: jpim, jprb       !DrHook
USE yomhook,      ONLY: lhook, dr_hook   !DrHook
USE timestep_mod, ONLY: timestep
USE ereport_mod,  ONLY: ereport
USE eg_parameters_mod, ONLY : l_rho_av_zz
USE horiz_grid_mod, ONLY: Nxi1L, Nxi1V, Nxi2L, Nxi2V,                        &
                          delta_xi1_H, delta_xi1_L, delta_xi2_H, delta_xi2_L
USE domain_params
USE PrintStatus_mod
USE proc_info_mod, ONLY: model_domain

USE chsunits_mod, ONLY : nunits

IMPLICIT NONE
!
! Description: Writes summary of salient ENDGame variables in concise
!              format
!  
!
! Method:
!  
!
! Code Owner: See Unified Model Code Owners HTML page
! This file belongs in section: Control
!
! Code description:
!   Language: Fortran 90.
!   This code is written to UM programming standards version 8.3.

! Subroutine arguments

INTEGER mype,nproc_x,nproc_y,numcycles,innits,global_row_length,      &
        global_rows,model_levels,                                     &
        gcr_precon_option,                                            &
        surface_type,tprofile_number,trefer_number,                   &
        aa_jet_m, aa_jet_n, grid_number,eg_vert_damp_profile,         &
        b_const, k_const


REAL    ih, height_domain, gcr_tol_abs,                               &
        alpha_u,alpha_v,alpha_w,alpha_theta,alpha_rho,                &
        alpha_p,grid_np_lon,grid_np_lat,aa_jet_u0,aa_jet_a,           &
        t_surface_in,h_o,lambda_fraction,phi_fraction,half_width_x,   &
        half_width_y,eg_vert_damp_coeff, eta_s, T0_E, T0_P 

LOGICAL l_shallow,l_rotating,l_rotate_grid,l_accel_convergence,       &
        l_eliminate_rho,l_init_fnm1,                                  &
        l_solid_body,l_baro_inst,                                     &
        l_baro_perturbed,l_baroclinic,l_cartesian,l_rotate_winds,     &
        l_const_grav,l_expl_horz_drag,l_impl_horz_drag,               &
        l_eg_dry_static_adj,l_fix_mass, l_deep_baro_inst,             &
        l_inc_solver, l_RK_dps,                                       &
        L_conserv_smooth_lap 

#include "clfhist.h"

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle


! 1.0 Start of subroutine code: perform the calculation.

IF (lhook) CALL dr_hook('EG_INFO_FILE',zhook_in,zhook_handle)
 
        IF( mype == 0 ) THEN

          IF( PrintStatus >= PrStatus_Normal ) THEN
            IF ( l_eliminate_rho ) THEN
              WRITE(6,fmt='(A)') ''
              WRITE(6,fmt='(A)') '*** USING DIAGNOSTIC DENSITY ***'
              WRITE(6,fmt='(A)') 'Mode not compatible with SLICE'
              WRITE(6,"(A,F12.6)") 'alpha_p = ',alpha_p
              WRITE(6,fmt='(A)') ''
            ELSE
              WRITE(6,fmt='(A)') ''
              WRITE(6,fmt='(A)') '*** USING PROGNOSTIC DENSITY ***'
              WRITE(6,fmt='(A)') ''
            END IF

            IF ( l_accel_convergence ) THEN

              CALL ereport('eg_info_file',1,                        &
                   'l_accel_convergence=T not available anymore')

            END IF
          END IF


         OPEN(UNIT=eg_unit,FILE='eg_job.info')
         WRITE(eg_unit,*) '**************************************************'
         WRITE(eg_unit,*) ' Summary of ENDGame parameters'
         WRITE(eg_unit,*)
         WRITE(eg_unit,*) ' Code version (local working copy may differ!)'
         WRITE(eg_unit,*) ' $Revision: 26957 $'
         WRITE(eg_unit,*) ' Note: Rev number based on last eg_atm_step change,'
         WRITE(eg_unit,*) '       not on last branch chage!'
         WRITE(eg_unit,*)
         WRITE(eg_unit,*)
         SELECT CASE(model_domain)
            CASE(mt_global)
               WRITE(eg_unit,*) 'Model Domain...................Global'
             CASE(mt_lam)
               WRITE(eg_unit,*) 'Model Domain...................LAM'   
             CASE(mt_cyclic_lam)
               WRITE(eg_unit,*) 'Model Domain...................cyclic E-W LAM'
             CASE(mt_bi_cyclic_lam)
               WRITE(eg_unit,*) 'Model Domain...................bicyclic LAM'
            CASE DEFAULT
               CALL ereport('eg_info_file',model_domain, 'Invalid model domain')
         END SELECT 
         WRITE(eg_unit,"(A,I3)") ' No. of E-W PEs................',nproc_x
         WRITE(eg_unit,"(A,I3)") ' No. of N-S PEs................',nproc_y
         WRITE(eg_unit,"(A,I5)") ' row_length....................',            &
                                   global_row_length
         WRITE(eg_unit,"(A,I5)") ' rows..........................',global_rows
         WRITE(eg_unit,"(A,I3)") ' model_levels..................',model_levels
         WRITE(eg_unit,"(A,F25.10)") ' height_domain.................',        &
                                   height_domain
         WRITE(eg_unit,"(A,F15.10)") ' timestep......................',timestep
         WRITE(eg_unit,*)
         WRITE(eg_unit,"(A,I3)") ' numcycles.....................',numcycles
         WRITE(eg_unit,"(A,I3)") ' innits........................',innits
         WRITE(eg_unit,*)
         WRITE(eg_unit,*) 'L_shallow.....................',l_shallow
         WRITE(eg_unit,"(A,F15.10)") ' Ih............................',ih
         WRITE(eg_unit,"(A,L1)") ' L_rotating....................',l_rotating
         WRITE(eg_unit,"(A,L1)") ' L_rotate_grid.................',l_rotate_grid
         WRITE(eg_unit,*)
         IF ( l_inc_solver ) WRITE(eg_unit,*) ' Using Incremental Solver '         
         WRITE(eg_unit,"(A,E20.10)") ' solver tol....................',        &
                                      gcr_tol_abs

         SELECT CASE(gcr_precon_option)
            CASE(0)
               WRITE(eg_unit,*) 'pre_type......................Diagonal'
            CASE(1)
               WRITE(eg_unit,*) 'pre_type......................ILU'
               WRITE(eg_unit,*) 'WARNING: This may not work '
               CALL ereport('eg_info_file',gcr_precon_option,                  &
                            'Preconditioner nolonger supported')
            CASE(2)
               WRITE(eg_unit,*) 'pre_type......................Jacobi'
            CASE(3)
               WRITE(eg_unit,*) 'pre_type......................ILU-Neumann'
               WRITE(eg_unit,*) 'WARNING: This may not work '
               CALL ereport('eg_info_file',gcr_precon_option,                  &
                            'Preconditioner nolonger supported')
            CASE(4)
               WRITE(eg_unit,*) 'pre_type......................SOR+tridiag'
            CASE DEFAULT
               CALL ereport('eg_info_file',gcr_precon_option,                  &
                            'Invalid preconditioner')
         END SELECT

         WRITE(eg_unit,*)
         WRITE(eg_unit,"(A,L1)") ' L_accel_convergence...........',            &
                                 l_accel_convergence
         WRITE(eg_unit,"(A,L1)") ' L_eliminate_rho...............',            &
                                 l_eliminate_rho
         WRITE(eg_unit,"(A,L1)") ' L_init_Fnm1...................',l_init_fnm1
         WRITE(eg_unit,*)

         WRITE(eg_unit,*)
         WRITE(eg_unit,"(A,F15.10)") ' ALPHA_U.......................',alpha_u
         WRITE(eg_unit,"(A,F15.10)") ' ALPHA_V.......................',alpha_v
         WRITE(eg_unit,"(A,F15.10)") ' ALPHA_W.......................',alpha_w
         WRITE(eg_unit,"(A,F15.10)") ' ALPHA_THETA...................',        &
                                    alpha_theta
         IF ( l_eliminate_rho ) THEN
           WRITE(eg_unit,"(A,F15.10)") ' ALPHA_P.......................',alpha_p
         ELSE
           WRITE(eg_unit,"(A,F15.10)") ' ALPHA_RHO.....................',      &
                                      alpha_rho
         END IF
         WRITE(eg_unit,*)
         WRITE(eg_unit,"(A,L1)") ' L_rotate_winds................',            &
                                l_rotate_winds
         WRITE(eg_unit,"(A,L1)") ' L_baroclinic..................',l_baroclinic
         WRITE(eg_unit,"(A,L1)") ' L_solid_body..................',l_solid_body
         WRITE(eg_unit,"(A,L1)") ' L_baro_inst...................',l_baro_inst
         WRITE(eg_unit,"(A,L1)") ' L_deep_baro_inst..............',            &
                                l_deep_baro_inst
         IF ( l_deep_baro_inst ) THEN
             WRITE(eg_unit,"(A,F15.10)") '   T0_Pole.....................',T0_P
             WRITE(eg_unit,"(A,F15.10)") '   T0_Equator..................',T0_E   
             WRITE(eg_unit,"(A,I2)")   '   k..........................',k_const
             WRITE(eg_unit,"(A,I2)")   '   b..........................',b_const 
         END IF
         WRITE(eg_unit,"(A,L1)") ' L_baro_perturbed..............',            &
                                l_baro_perturbed
         WRITE(eg_unit,*)
         WRITE(eg_unit,"(A,F15.10)") ' grid_NP_lon...................',        &
                                       grid_np_lon
         WRITE(eg_unit,"(A,F15.10)") ' grid_NP_lat...................',        &
                                       grid_np_lat
         WRITE(eg_unit,"(A,F15.10)") ' AA_jet_u0.....................',aa_jet_u0
         WRITE(eg_unit,"(A,L1)") ' L_Cartesian...................',l_cartesian
         SELECT CASE(surface_type)
           CASE(0)
               WRITE(eg_unit,*) 'surface_type..............surface_flat'
!            CASE(1)
!              WRITE(eg_unit,*) 'surface_type..............surface_ellipse'
!            CASE(2)
!              WRITE(eg_unit,*) 'surface_type..............surface_ridge'
!            CASE(3)
!              WRITE(eg_unit,*) 'surface_type..............surface_plateau'
!            CASE(4)
!              WRITE(eg_unit,*) 'surface_type..............surface_massif'
!            CASE(5)
!              WRITE(eg_unit,*) 'surface_type..............surface_mask'
           CASE(6)
               WRITE(eg_unit,*) 'surface_type..............surface_gauss'
!            CASE(7)
!              WRITE(eg_unit,*) 'surface_type..............surface_ridge_series'
           CASE(8)
               WRITE(eg_unit,*) 'surface_type..............surface_schar_ridge'
           CASE(9)
               WRITE(eg_unit,*) 'surface_type..............surface_baroclinic'
           CASE(10)
               WRITE(eg_unit,*) 'surface_type..............surface_dump'
           CASE DEFAULT
             CALL ereport('eg_info_file',1, 'Invalid surface type')
         END SELECT
         IF(surface_type /= 0 .AND. surface_type /= 10) THEN
           WRITE(eg_unit,"(A,F15.10)") '   h_o.....................',h_o
           WRITE(eg_unit,"(A,F15.10)") '   lambda_fraction.........',       &
                                           lambda_fraction
           WRITE(eg_unit,"(A,F15.10)") '   phi_fraction............',       &
                                           phi_fraction
           WRITE(eg_unit,"(A,F25.10)") '   half_width_x............',       &
                                           half_width_x
           WRITE(eg_unit,"(A,F25.10)") '   half_width_y............',       &
                                           half_width_y
         ENDIF
         WRITE(eg_unit,"(A,I3)") ' grid_number...............',grid_number
         WRITE(eg_unit,"(A,I3)") ' tprofile_number...........',tprofile_number
         WRITE(eg_unit,"(A,I3)") ' trefer_number.............',trefer_number
         WRITE(eg_unit,"(A,F15.10)") ' t_surface_in..............',          &
                                       t_surface_in

         WRITE(eg_unit,*)
         WRITE(eg_unit,"(A,L1)") ' L_rk_dps................. ', l_RK_dps

         WRITE(eg_unit,*)
         IF (l_const_grav) THEN
           WRITE(eg_unit,"(A)") 'CONSTANT GRAVITY SELECTED (l_const_grav)!  ' 
           WRITE(eg_unit,"(A)") 'Whilst this option is required for starting' 
           WRITE(eg_unit,"(A)") 'from ND dumps it is entirely at the users risk'
           WRITE(eg_unit,"(A)") 'It is not supported and known to lead to'
           WRITE(eg_unit,"(A)") 'a spurious 3D divergence tendency'
         ELSE
          WRITE(eg_unit,"(A)") 'gravity is height dependent (note: this is'
          WRITE(eg_unit,"(A)") 'incompatible with running from ND start dumps!)'
         END IF

         WRITE(eg_unit,*)
         WRITE(eg_unit,"(A,L1)") ' l_expl_horz_drag ............. ',           &
                                   l_expl_horz_drag
         WRITE(eg_unit,"(A,L1)") ' l_impl_horz_drag ............. ',           &
                                   l_impl_horz_drag
         WRITE(eg_unit,"(A,L1)") ' l_eg_dry_static_adj .......... ',           &
                                   l_eg_dry_static_adj
         WRITE(eg_unit,"(A,L1)") ' l_fix_mass ................... ',           &
                                   l_fix_mass
         WRITE(eg_unit,"(A,I1)") ' eg_vert_damp_profile ......... ',           &
                                   eg_vert_damp_profile
         WRITE(eg_unit,"(A,F15.10)") ' eta_s ........... ',                    &
                                       eta_s
         WRITE(eg_unit,"(A,E15.10)") ' eg_vert_damp_coeff ........... ',       &
                                       eg_vert_damp_coeff
         WRITE(eg_unit,"(A,I3)") '******************************************'
        CLOSE(eg_unit)
       END IF

IF (lhook) CALL dr_hook('EG_INFO_FILE',zhook_out,zhook_handle)

END SUBROUTINE eg_info_file
END MODULE eg_info_file_mod
