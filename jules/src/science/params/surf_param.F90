! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************


! Description:
!     Parameter values for surface routines.

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.


MODULE surf_param

  USE c_epslon, ONLY : repsilon
  USE c_lheat, ONLY : lc,lf
  USE c_r_cp, ONLY : cp
  USE c_g, ONLY : g
  
  USE c_kappai, ONLY : kappai, kappai_snow, kappa_seasurf

  IMPLICIT NONE
  SAVE

!-----------------------------------------------------------------------
!  Latent heat of sublimation.
  REAL, PARAMETER ::                                              &
   ls=lc+lf     ! Latent heat of sublimation (J per kg).

!-----------------------------------------------------------------------
!  Parameters for heat capacity of vegetation.
  REAL ::                                                         &
   hleaf = 5.7e4                                                  &
                     ! Specific heat capacity of leaves
!                          (J / K / kg Carbon).
  ,hwood = 1.1e4       ! Specific heat capacity of wood
!                          (J / K / kg Carbon).

!-----------------------------------------------------------------------
!  Parameters for leaf conductance and photosynthesis.
  REAL ::                                                         &
   beta1 = 0.83                                                   &
                   ! Coupling coefficients for co-limitation.
  ,beta2 = 0.93                                                   &
                   ! Coupling coefficients for co-limitation.
!  Factors in expressions for limitation of photosynthesis by transport
!  of products, for C3 and C4 respectively
  ,fwe_c3 = 0.5                                                   &
  ,fwe_c4 = 20000.0                                               &
  ,q10_leaf = 2.0  ! Q10 factor for plant respiration.

  REAL, PARAMETER ::                                              &
   ratio=1.6       ! Ratio of leaf resistance for CO2 to leaf
!                    resistance for H2O.

  REAL, PARAMETER :: ratio_o3 = 1.67
                   ! Ratio of leaf resistance for O3 to leaf
!                    resistance for H2O.

!-----------------------------------------------------------------------
! Parameters used for stomatal conductance.
  INTEGER, PARAMETER ::                                           &
   iter = 3                   ! Number of iterations to
!                               determine the canopy climate.

  REAL, PARAMETER ::                                              &
   o2 = 0.23                  ! Atmospheric concentration of
!                               oxygen (kg O2/kg air).

!-----------------------------------------------------------------------
! Parameters for soil respiration.
  REAL ::                                                         &
   kaps = 0.5e-8                                                  &
                    ! Specific soil respiration rate at 25 degC
!                     and optimum soil moisture (/s).
  ,kaps_roth(4)                                                   &
                    ! Specific soil respiration rate for RothC
  ,q10_soil = 2.0   ! Q10 factor for soil respiration.

  DATA kaps_roth / 3.22e-7, 9.65e-9, 2.12e-8, 6.43e-10 /

!-----------------------------------------------------------------------
!  Parameters for surface transfer coefficients.
  INTEGER, PARAMETER ::                                           &
   n_its=5      ! Number of iterations for Monin-Obukhov length
!                 and stability functions.

  REAL, PARAMETER ::                                              &
   beta=0.08                                                      &
                ! Tunable parameter in the surface layer scaling
!                 velocity formula (multiplying the turbulent
!                 convective scaling velocity).
  ,third=1./3.  ! One third.

      REAL, PARAMETER :: BETA_cndd = 0.04
!                   ! Tunable parameter in the surface layer scaling
!                   ! velocity formula multiplying the turbulent
!                   ! convective scaling velocity (halved from the
!                   ! value in the original scheme to compensate for
!                   ! the convective contribution).
      REAL, PARAMETER :: cnst_cndd_0 = 1.0
!                   ! Constant in formula for convective gustiness
      REAL, PARAMETER :: cnst_cndd_1 = 6.004e+02 / g
!                   ! Constant in formula for convective gustiness
      REAL, PARAMETER :: cnst_cndd_2 = -4.375e+03 / g**2
!                   ! Constant in formula for convective gustiness
      REAL, PARAMETER :: min_wind = 1.0e-3
!                   ! minimum windspeed for iteration convergence
      REAL, PARAMETER :: min_ustar = min_wind / 50.0
!                   ! corresponding friction velocity
      REAL, PARAMETER :: Ri_m = 10.0
!                   ! maximum Richardson number
!-----------------------------------------------------------------------
! Constants used in the Beljaars and Holtslag stable stability functions
  REAL, PARAMETER ::                                              &
   a = 1.0                                                        &
  ,b = 2.0/3.0                                                    &
  ,c = 5.0                                                        &
  ,d = 0.35                                                       &
  ,c_over_d = c/d

!-----------------------------------------------------------------------
! Parameters for calculation of aerodynamic resistance.
  REAL, PARAMETER ::                                              &
   ah = 10.0                                                      &
                   !  Stability parameter.
  ,cz = 4.0        !  Stability parameter.

!-----------------------------------------------------------------------
! Parameters used for calculation of saturated specific humidity.
  REAL, PARAMETER ::                                              &
   one_minus_epsilon = 1.0 - REPSILON                             &
                 ! ONE MINUS THE RATIO OF THE
!                  MOLECULAR WEIGHTS OF WATER AND DRY AIR
  ,t_low = 183.15                                                 &
                 ! LOWEST TEMPERATURE FOR WHICH LOOK-UP TABLE OF
!                  SATURATION WATER VAPOUR PRESSURE IS VALID (K)
  ,t_high = 338.15                                                &
                 ! HIGHEST TEMPERATURE FOR WHICH LOOK-UP TABLE OF
!                  SATURATION WATER VAPOUR PRESSURE IS VALID (K)
  ,delta_t = 0.1
                 ! TEMPERATURE INCREMENT OF THE LOOK-UP
!                  TABLE OF SATURATION VAPOUR PRESSURES

  INTEGER, PARAMETER ::                                           &
   n = ((t_high - t_low + (delta_t*0.5))/delta_t) + 1.0
!                  SIZE OF LOOK-UP TABLE OF SATURATION WATER VAPOUR
!                  PRESSURES. With T_LOW = 183.15, T_HIGH = 338.15 and
!                  DELTA_T = 0.1, this gives n=1551.

!-----------------------------------------------------------------------
! Parameters used in diagnosis of screen T or q.
  REAL, PARAMETER ::                                              &
   grcp = g / cp   !  ratio of acceleration due to gravity to
!                     specific heat of dry air at constant
!                     pressure ( K/m)

!-----------------------------------------------------------------------
! Parameters used in interpolation of wind, T and q.
  REAL, PARAMETER ::                                              &
   z_obs_tq = 1.5                                                 &
                     ! Height of screen observations of temperature
!                      and humidity.
  ,z_obs_wind = 10.0 ! Height of surface wind observations.

  LOGICAL, PARAMETER ::                                           &
   eff_int = .FALSE. !  switch for orographic roughness in
!                       calculation of 10m wind speed.
!        .TRUE. orographic roughness included
!        .FALSE. orographic roughness NOT included

  INTEGER, PARAMETER :: ip_scrndecpl1 = 1
!                       Diagnose the screen temperature using
!                       surface similarity theory, but allow
!                       decoupling in very stable conditions
!                       based on the quasi-equilibrium radiative
!                       solution.

  INTEGER, PARAMETER :: ip_scrndecpl2 = 2 
!                       Diagnose the screen temperature using  
!                       including transient effects and radiative 
!                       cooling 

!-----------------------------------------------------------------------
! Parameters used to calculate blending height.
  REAL, PARAMETER ::                                              &
   h_blend_max=1000.0                                             &
                         ! Maximum blending height (m).
  ,h_blend_min=0.0       ! Minimum blending height (m).
!-----------------------------------------------------------------------
!  Roughness lengths for sea ice.
  REAL ::                                                         &
   z0miz  = 1.0e-1                                                &
                   ! roughness length for heat, moisture and
                   ! momentum over the Marginal Ice Zone (m).
  ,z0sice = 3.0e-3
                   ! roughness length for heat, moisture and
                   ! momentum over sea-ice (m).
  REAL ::                                                         & 
   z0h_z0m_miz = 1.0                                              & 
                   ! Ratio of thermal to momentum roughness 
                   ! lengths for marginal ice 
  ,z0h_z0m_sice = 1.0 
                   ! Ratio of thermal to momentum roughness 
                   ! lengths for sea ice 
!-----------------------------------------------------------------------
! Emissivities of sea and sea-ice 
!----------------------------------------------------------------------- 
  REAL ::                                                         & 
   emis_sea  = 1.0                                                & 
!                  ! Emissivity of open sea 
  ,emis_sice = 1.0    
!                  ! Emissivity of sea-ice 
   
!
!----------------------------------------------------------------------- 

!-----------------------------------------------------------------------
! Other variables used in surface calculations
!-----------------------------------------------------------------------
  REAL, ALLOCATABLE ::                                            &
   diff_frac(:)          !  The fraction of radiation that is diffuse

  REAL, PARAMETER ::                                              &
    kn = 0.78            ! Nitrogen Allocation coefficient:
                         ! Tellus 59B, 553-565 (2007), page 555.


!-----------------------------------------------------------------------
! Set up a namelist to allow the scalars to be set via io
!-----------------------------------------------------------------------
  NAMELIST /jules_surf_param/ hleaf,hwood,beta1,beta2,fwe_c3,     &
                              fwe_c4,q10_leaf,kaps,kaps_roth,     &
                              q10_soil,z0miz,z0sice,              &
                              z0h_z0m_miz, z0h_z0m_sice,          &
                              emis_sea, emis_sice,                &
                              kappai, kappai_snow, kappa_seasurf

END MODULE surf_param
