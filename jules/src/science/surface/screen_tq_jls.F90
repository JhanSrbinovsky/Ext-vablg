! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!   SUBROUTINE SCREEN_TQ----------------------------------------------

!  Purpose: Diagnose temperature and/or specific humidity at screen
!           height (1.5 metres), as requested via the STASH flags.

!---------------------------------------------------------------------
SUBROUTINE screen_tq (                                            &
 land_pts,ntiles,                                                 &
 land_index,tile_index,tile_pts,flandg,                           &
 sq1p5,st1p5,chr1p5m,chr1p5m_sice,pstar,qw_1,resft,               &
 tile_frac,tl_1,tstar_ssi,tstar_tile,                             &
 z0hssi,z0h_tile,z0mssi,z0m_tile,z1,                              &
 timestep,tstar_ssi_old,tstar_tile_old,                           &
 l_co2_interactive, co2_mmr, co2_3d,                              &
 f3_at_p, ustargbm, rho1,                                         &
 tscrndcl_ssi,tscrndcl_tile,tstbtrans,                            &
 q1p5m,q1p5m_tile,t1p5m,t1p5m_tile,                               &
 lq_mix_bl                                                        &
 )

USE atm_fields_bounds_mod
USE theta_field_sizes, ONLY : t_i_length,t_j_length

USE c_ht_m
USE surf_param, ONLY : grcp, ip_scrndecpl2, g, cp
USE switches, ONLY : iscrntdiag
USE c_mdi, ONLY    : rmdi
USE c_pi, ONLY     : pi
USE csigma, ONLY  : sbcon

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER                                                           &
 land_pts                                                         &
                      ! IN Number of land points to be processed.
,ntiles                                                           &
                      ! IN Number of tiles per land point.
,land_index(land_pts)                                             &
                      ! IN Index of land points.
,tile_index(land_pts,ntiles)                                      &
!                           ! IN Index of tile points.
,tile_pts(ntiles)     ! IN Number of tile points.


LOGICAL                                                           &
 sq1p5                                                            &
                      ! IN STASH flag for 1.5-metre sp humidity.
,st1p5                ! IN STASH flag for 1.5-metre temperature.

LOGICAL                                                           &
 lq_mix_bl              ! TRUE if mixing ratios used in
!                             ! boundary layer code

REAL                                                              &
 flandg(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)      &
!                           ! IN Fraction of gridbox which is land.
,chr1p5m(land_pts,ntiles)                                         &
!                           ! IN Ratio of coefficients for  calculation
!                           !    of 1.5 m T.
,chr1p5m_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)&
!                           ! IN Ratio of coefficients for  calculation
!                           !    of 1.5 m T.
,pstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                       ! IN Surface pressure (Pa).
,qw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)        &
                       ! IN Total water content of lowest
!                                 atmospheric layer (kg per kg air).
,resft(land_pts,ntiles)                                           &
!                           ! IN Surface resistance factor.
,tile_frac(land_pts,ntiles)                                       &
!                           ! IN Tile fractions.
,tl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)        &
                      ! IN Liquid/frozen water temperature for
!                                lowest atmospheric layer (K).
,tstar_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)   &
!                           ! IN Sea/sea-ice mean sfc temperature (K).
,tstar_tile(land_pts,ntiles)                                      &
!                           ! IN Tile surface temperatures (K).
,z0hssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)      &
                      ! IN Roughness length for heat and
!                           !    moisture (m).
,z0h_tile(land_pts,ntiles)                                        &
!                           ! IN Tile roughness lengths for heat and
!                           !    moisture (m).
,z0mssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)      &
                      ! IN Roughness length for momentum (m).
,z0m_tile(land_pts,ntiles)                                        &
!                           ! IN Tile roughness lengths for momentum (m)
,z1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                            ! IN Height of lowest atmospheric level (m).

REAL, INTENT(IN)    :: timestep
                      ! Timestep of atmospheric model
REAL, INTENT(IN)    :: tstar_ssi_old(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                      ! Surface temperature at beginning of
                      ! timestep
REAL, INTENT(IN)    :: tstar_tile_old(land_pts,ntiles)
                      !    Surface temperature at beginning of
                      !    timestep

! IN Additional variables for screen-level diagnostics
LOGICAL, INTENT(IN) :: l_co2_interactive
                      ! Flag for interactive 3-D CO2
REAL, INTENT(IN)    :: co2_mmr
                      ! Initial or fixed mass mixing ratio
                      ! of CO2
REAL, INTENT(IN)    :: co2_3d                                         &
                      (tdims_s%i_start:tdims_s%i_end,tdims_s%j_start:tdims_s%j_end)
                      ! 3-D field of CO2
REAL, INTENT(IN)    :: rho1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                      ! Density on lowest level
REAL, INTENT(IN)    :: f3_at_p(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                      ! Coriolis parameter
REAL, INTENT(IN)    :: ustargbm(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                      ! GBM surface friction velocity


REAL                                                              &
 q1p5m(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                       ! OUT Specific humidity at screen height
!                           !      of 1.5 metres (kg water per kg air).
,q1p5m_tile(land_pts,ntiles)                                      &
!                           ! OUT Q1P5M over land tiles.
,t1p5m(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)       &
                       ! OUT Temperature at screen height of
!                           !     1.5 metres (K).
,t1p5m_tile(land_pts,ntiles)
!                           ! OUT T1P5M over land tiles.

REAL, INTENT(INOUT) :: TScrnDcl_SSI(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                            !    Decoupled screen-level temperature
                            !    over sea or sea-ice
REAL, INTENT(INOUT) :: TScrnDcl_TILE(land_pts,ntiles)
                            !    Decoupled screen-level temperature
                            !    over land tiles
REAL, INTENT(INOUT) :: tStbTrans(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                            !    Time since the transition


!  External routines called :-
EXTERNAL qsat_mix


REAL                                                              &
 cer1p5m                                                          &
                      ! Ratio of coefficients reqd for
!                           ! calculation of 1.5 m Q.
,pstar_land(land_pts)                                             &
                      ! Surface pressure for land points.
,qs(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)          &
                      ! Surface saturated sp humidity.
,qs_tile(land_pts)    ! Surface saturated sp humidity.

!     Variables for the decoupled diagnostic

  REAL :: ks
                            ! Temporary in calculation of radiative
                            ! screen temperature
  REAL :: exks
                            ! Temporary in calculation of radiative
                            ! screen temperature
  REAL :: waterpath     ! Water path from the surface to the
                            ! screen-level (kgm-2)
  REAL :: CO2Path       ! Path of CO2 from the surface to the
                            ! screen-level (kgm-2)
  REAL :: TRadCool      ! Temporary variable holding the screen
                            ! temperature due to radiative cooling
                            ! of the previous decoupled value

  REAL, PARAMETER :: cc_c(0:5) = (/ 1.25363, 1.56663, 0.763231,         &
                         0.115082, 0.00789067, 0.000204645 /)
                            ! Fit used to calculate radiative cooling
                            ! due to CO2
  REAL, PARAMETER :: cc_h(0:5) = (/ -2.32590, -0.832639, 0.0498829,     &
                         0.0164081, 0.00148810, 5.06995e-05 /)
                            ! Fit used to calculate radiative cooling
                            ! due to water vapour
  REAL, Parameter :: cc_h_large = 0.06376
                            ! Coefficient of fit to large water paths
                            ! of water vapour
  REAL, Parameter :: t0_rfit = 288.15
                            ! Reference temperature for fit to
                            ! radiative cooling
  REAL, Parameter :: dt_rfit = 15.0
                            ! Temperature scale for fit to
                            ! radiative cooling
  REAL, Parameter :: pl0_h2o_rfit = 0.657638
                            ! Zeroth coefficient in fit to the derivative
                            ! of the Planckian for water vapour
  REAL, Parameter :: pl1_h2o_rfit = 0.126593
                            ! First order coefficient in fit to the derivative
                            ! of the Planckian for water vapour
  REAL, Parameter :: pl0_co2_rfit = 0.349957
                            ! Zeroth coefficient in fit to the derivative
                            ! of the Planckian for carbon dioxide
  REAL, Parameter :: pl1_co2_rfit = 0.135816
                            ! First order coefficient in fit to the derivative
                            ! of the Planckian for carbon dioxide
  REAL, Parameter :: t_scale_h2o_rfit = 0.015
                            ! Temperature-dependent variation of
                            ! the derivative of the emissivity of water vapour
  REAL, Parameter :: t_scale_co2_rfit = 0.02
                            ! Temperature-dependent variation of
                            ! the derivative of the emissivity of CO2
  LOGICAL, ALLOCATABLE :: LDclDiag(:, :)
                            ! Flag for full calculation of the decoupled
                            ! temperature including radiative cooling
                            ! and interpolation
  REAL, ALLOCATABLE :: TStarGBM(:, :)
                            ! Grid-box mean surface temperature
  REAL :: t1p5m_ssi (tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                            ! Temperature at 1.5 m over sea and sea-ice
  REAL :: q1p5m_ssi (tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                            ! Screen-level humidity


  REAL :: QScrn
  REAL :: CO2Scrn
  REAL :: lp, tx
  REAL :: tRotational_Inv
  REAL :: ThDot, LTrans2
  REAL :: WeightDcl


 INTEGER                                                          &
 i,j                                                              &
             ! Loop counter (horizontal field index).
,k                                                                &
             ! Loop counter (tile point index).
,l                                                                &
             ! Loop counter (land point field index).
,n           ! Loop counter (tile index).

INTEGER :: row_len
INTEGER :: nrows

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle


  IF (lhook) CALL dr_hook('SCREEN_TQ',zhook_in,zhook_handle)


!-----------------------------------------------------------------------
! Diagnose local and GBM humidities at 1.5 m if requested via SQ1P5
!-----------------------------------------------------------------------

row_len = tdims%i_end - tdims%i_start + 1
nrows   = tdims%j_end - tdims%j_start + 1

IF (sq1p5 .OR. (IScrnTDiag == IP_ScrnDecpl2) ) THEN

!jhan: make these switchable
! DEPENDS ON: qsat_mix
  CALL qsat_mix(qs,tstar_ssi,pstar,t_i_length*t_j_length,lq_mix_bl)
  DO j=tdims%j_start,tdims%j_end
    DO i=tdims%i_start,tdims%i_end
      ! CABLE: already calculated in CABLE
      !q1p5m(i,j) = 0.
      q1p5m_ssi(i,j) = 0.
      IF (flandg(i,j) <  1.0 ) THEN
        cer1p5m = chr1p5m_sice(i,j) - 1.
        q1p5m_ssi(i,j) =                                            &
          (qw_1(i,j) + cer1p5m*( qw_1(i,j) - qs(i,j) ))
        !CABLE: already calculated in CABLE
        !q1p5m(i,j) = (1.-flandg(i,j))*q1p5m_ssi(i,j)
      END IF  
    END DO
  END DO

  DO l=1,land_pts
    j=(land_index(l)-1)/t_i_length + 1
    i = land_index(l) - (j-1)*t_i_length
    pstar_land(l) = pstar(i,j)
  END DO

  DO n=1,ntiles
    DO l=1,land_pts
      q1p5m_tile(l,n) = 0.
    END DO
! DEPENDS ON: qsat_mix
    CALL qsat_mix(qs_tile,tstar_tile(:,n),pstar_land,land_pts     &
    ,lq_mix_bl)
    DO k=1,tile_pts(n)
      l = tile_index(k,n)
      j=(land_index(l)-1)/t_i_length + 1
      i = land_index(l) - (j-1)*t_i_length
      cer1p5m = resft(l,n)*(chr1p5m(l,n) - 1.)
      q1p5m_tile(l,n) = qw_1(i,j) +                               &
                        cer1p5m*( qw_1(i,j) - qs_tile(l) )
      q1p5m(i,j) = q1p5m(i,j)                                     &
        + flandg(i,j)*tile_frac(l,n)*q1p5m_tile(l,n)
    END DO
  END DO

END IF

!-----------------------------------------------------------------------
! Diagnose local and GBM temperatures at 1.5 m if requested via ST1P5
! The transitional diagnostic requires that this be done on all
! timesteps as the decoupled screen temperature is prognostic.
!-----------------------------------------------------------------------
  IF (st1p5 .OR. (IScrnTDiag == IP_ScrnDecpl2) ) THEN

!       Calculate the screen-level temperature using the standard
!       interpolation. If using the decoupled diagnosis with
!       transitional effects (IScrnTDiag=IP_ScrnDecpl2), this will
!       subsequently be adjusted.

    DO j = tdims%j_start, tdims%j_end
      DO i = tdims%i_start, tdims%i_end
        !CABLE: already calculated in CABLE
        !t1p5m_ssi(i,j) = 0.
        t1p5m(i,j) = 0.

        IF (flandg(i,j) <  1.0 ) THEN
!             Calculate the screen-level temperature.
          t1p5m_ssi(i,j) = tstar_ssi(i,j) - grcp*z1p5m +                &
            chr1p5m_sice(i,j) * (tl_1(i,j) - tstar_ssi(i,j) +           &
            grcp*(z1(i,j)+z0mssi(i,j)-z0hssi(i,j)))
          !CABLE: already calculated in CABLE
          !t1p5m(i,j) = (1.-flandg(i,j)) * t1p5m_ssi(i,j)
        END IF

      END DO
    END DO


    DO n = 1, ntiles

      t1p5m_tile(1:land_pts,n)=0.0

      DO k = 1, tile_pts(n)
        l = tile_index(k,n)
        j=(land_index(l)-1)/row_len + 1
        i = land_index(l) - (j-1)*row_len
        t1p5m_tile(l,n) = tstar_tile(l,n) - grcp*z1p5m +                &
          chr1p5m(l,n)*( tl_1(i,j) - tstar_tile(l,n) +                  &
          grcp*(z1(i,j)+z0m_tile(l,n)-z0h_tile(l,n)) )
        t1p5m(i,j) = t1p5m(i,j)                                         &
          + flandg(i,j)*tile_frac(l,n)*t1p5m_tile(l,n)
      END DO

    END DO

    IF (IScrnTDiag == IP_ScrnDecpl2) THEN

!         Allocate and set arrays specific to this diagnostic
      ALLOCATE(LDclDiag(row_len,nrows))
      ALLOCATE(TStarGBM(row_len,nrows))

!         Determine the grid-box mean surface temperatures to check for
!         changing near-surface stability.

      WHERE (flandg < 1.0)
        TStarGBM = (1.0 - flandg) * tstar_ssi
      ELSEWHERE
        TStarGBM = 0.0
      END WHERE
      DO n = 1, ntiles
        DO k = 1, tile_pts(n)
          l = tile_index(k,n)
          j=(land_index(l)-1)/row_len + 1
          i = land_index(l) - (j-1)*row_len
          TStarGBM(i,j) = TStarGBM(i,j) + flandg(i,j) *                 &
            tile_frac(l, n) * tstar_tile(l,n)
        END DO
      END DO

      DO j = tdims%j_start, tdims%j_end
        DO i = tdims%i_start, tdims%i_end

!             The following test for missing data is designed to allow
!             for last-bit differences.
          IF (ABS(tStbTrans(i,j)-rmdi) < 0.01 * ABS(rmdi)) THEN
!               The point must have been unstable on the last
!               time-step. If it remains unstable no action is required
!               and the flag is set to .FALSE.. If it has just become
!               stable the decoupled diagnosis must be initialized from
!               the standard diagnostic. The test for stability used
!               here is deliberately simple and neglects moisture.
            LDclDiag(i,j) =.FALSE.
            IF (TStarGBM(i,j) < tl_1(i,j)+grcp*z1(i,j))                 &
              tStbTrans(i,j) = 0.0
          ELSE
!               The point was stable last time: check for a transition
!               to unstable stratification.
            IF (TStarGBM(i,j) >= tl_1(i,j)+grcp*z1(i,j)) THEN
              tStbTrans(i,j) = rmdi
              LDclDiag(i,j) = .FALSE.
            ELSE
              LDclDiag(i,j) = .TRUE.
              tStbTrans(i,j) = tStbTrans(i,j) + timestep
            END IF
          END IF

        END DO
      END DO

!         The 1.5 m temperature will be recalculated where decoupled
!         diagnosis is applied: to start the process, it is zeroed at
!         points marked with LDclDiag.
      WHERE (LDclDiag)
        t1p5m = 0.0
      END WHERE

!         Initialize the decoupled temperatures: separate blocks
!         are used here because indexing over tiles requires
!         special treatment. The foregoing code requires that
!         initialization must take place where the time since the
!         transition is positive and the logical for the full
!         decoupled diagnosis is false.
      WHERE( (tStbTrans >= 0.0) .AND. (flandg < 1.0) .AND.              &
             (.NOT.LDclDiag) )
        TScrnDcl_SSI = t1p5m_ssi
      END WHERE
      DO n = 1, ntiles
        DO k = 1, tile_pts(n)
          l = tile_index(k,n)
          j=(land_index(l)-1)/row_len + 1
          i = land_index(l) - (j-1)*row_len
          IF ( (tStbTrans(i,j) >= 0) .AND. (.NOT.LDclDiag(i,j)) )       &
            TScrnDcl_TILE(l,n)=t1p5m_tile(l,n)
        END DO
      END DO

!         Calculate the radiative temperatures at valid points.

!         First sea and sea-ice:

      DO j = tdims%j_start, tdims%j_end
        DO i = tdims%i_start, tdims%i_end

          IF ( (LDclDiag(i,j)) .AND. (flandg(i,j) < 1.0) ) THEN

!               Calculate the integrated paths of water vapour and
!               CO2 between the surface and the screen level.
!               We assume that the mixing ratio varies linearly in
!               calculating the water path. The cooling time will not
!               be greatly sensitive to this.

            WaterPath = rho1(i,j) * z1p5m *                             &
              (qs(i,j) + 0.5*(qw_1(i,j)-qs(i,j)) *                      &
              (z1p5m/z1(i,j)))
            QScrn = q1p5m_ssi(i,j)
            IF (l_co2_interactive) THEN
              CO2Path = rho1(i,j) * co2_3d(i,j) * z1p5m
              CO2Scrn = co2_3d(i,j)
            ELSE
              CO2Path = rho1(i,j) * co2_mmr * z1p5m
              CO2Scrn = co2_mmr
            END IF

!               Contribution of cooling to the surface due to water
!               vapour.
            IF (WaterPath < 0.04) THEN
              lp  = LOG(MAX(1.0e-05, WaterPath))
              lp  = EXP( cc_h(0) + lp * ( cc_h(1) + lp *                &
                    ( cc_h(2) + lp * ( cc_h(3) + lp * ( cc_h(4) +       &
                    lp * cc_h(5) ) ) ) ) )
            ELSE
              lp = cc_h_large / WaterPath
            END IF
            tx  = (tstar_ssi(i,j) - t0_rfit) / dt_rfit
            ks  = QScrn * lp * (1.0 + t_scale_h2o_rfit*tx) *            &
                  (pl0_h2o_rfit + pl1_h2o_rfit*tx)

!               Contribution of cooling to the surface due to carbon
!               dioxide.
            lp  = LOG(CO2Path)
            lp  = cc_c(0) + lp * ( cc_c(1) + lp * ( cc_c(2) + lp *      &
                  ( cc_c(3) + lp * ( cc_c(4) + lp * cc_c(5) ) ) ) )
            tx  = (tstar_ssi(i,j) - t0_rfit) / dt_rfit
            ks  = ks + CO2Scrn * EXP(lp) * (1.0 - t_scale_co2_rfit*tx) *&
                  (pl0_co2_rfit - pl1_co2_rfit*tx)

!               Limit on ks in case range of it is exceeded.
            ks  = MAX(4.0 * sbcon * tstar_ssi(i,j)**3 * ks /cp,         &
                    EPSILON(ks))
!               ks now holds K such that dT/dt = -K(T - T_surf)

!               Calculate the impact of radiative cooling through the
!               current timestep on the decoupled screen temperature.
            exks = EXP( - ks * timestep )
            TRadCool = tstar_ssi(i,j) +                                 &
              (TScrnDcl_SSI(i,j) - tstar_ssi_old(i,j)) * exks -         &
              (tstar_ssi(i,j) - tstar_ssi_old(i,j)) * (1.0 - exks) /    &
              (ks * timestep )


!               Calculate the adjustment of the radiatively cooled
!               decoupled temperature towards that implied by standard
!               similarity theory.

            tRotational_Inv = 0.4 * ABS(f3_at_p(i,j))

            ThDot = (tstar_ssi(i,j) - tstar_ssi_old(i,j)) / timestep
            LTrans2 = ( uStarGBM(i,j)**3 /                              &
              ( (g/tstar_ssi(i,j)) * (1.0e-08 + ABS(ThDot) ) ) ) *      &
              ( LOG(1.0 + z1p5m/z0mssi(i,j)) /                          &
                LOG(1.0 + z1p5m/0.05) ) **2

            WeightDcl = EXP( - timestep * tStbTrans(i,j) *              &
              tRotational_Inv**2 ) * 1.0 /                              &
              (1.0 + MIN(0.2*uStarGBM(i,j)*timestep/z1p5m,              &
                         LTrans2/z1p5m**2)*(timestep/300.0))

!               Interpolate between the value obtained from surface
!               similarity theory and the radiative temperature,
!               provided that the radiative temperature is higher
!               than the M-O value.
            t1p5m_ssi(i,j) = t1p5m_ssi(i,j) + WeightDcl *               &
              MAX(0.0, (TRadCool - t1p5m_ssi(i,j)))
            t1p5m(i,j) = (1.0 - flandg(i,j)) * t1p5m_ssi(i,j)
!               Set the decoupled temperature to the newly diagnosed
!               screen temperature.
            TScrnDcl_SSI(i,j) = t1p5m_ssi(i,j)

          END IF
        END DO
      END DO

!         PSTAR_LAND will previously have been calculated if the
!         humidity diagnostic has been requested.
      IF (.NOT.sq1p5) THEN
        DO l = 1, land_pts
          j=(land_index(l)-1)/row_len + 1
          i = land_index(l) - (j-1)*row_len
          pstar_land(l) = pstar(i,j)
        END DO
      END IF


!         Repeat for land points.
      DO n = 1, ntiles

! DEPENDS ON: qsat_mix
        CALL qsat_mix(qs_tile,tstar_tile(1,n),pstar_land,land_pts,      &
           lq_mix_bl)

!           Advance the radiative temperature on tiles.
        DO k = 1, tile_pts(n)

          l = tile_index(k,n)
          j=(land_index(l)-1)/row_len + 1
          i = land_index(l) - (j-1)*row_len

          IF (LDclDiag(i,j)) THEN

!               Calculate the integrated paths of water vapour and CO2
!               between the surface and the screen level. We assume that
!               the mixing ratio varies linearly in calculating the
!               water path. The cooling time will not be greatly
!               sensitive to this.

            WaterPath = rho1(i,j) * z1p5m *                             &
              (qs_tile(l) + 0.5*(qw_1(i,j)-qs_tile(l)) *                &
              (z1p5m/z1(i,j)))
            QScrn = q1p5m_tile(l,n)
            IF (l_co2_interactive) THEN
              CO2Path = rho1(i,j) * co2_3d(i,j) * z1p5m
              CO2Scrn = co2_3d(i,j)
            ELSE
              CO2Path = rho1(i,j) * co2_mmr * z1p5m
              CO2Scrn = co2_mmr
            END IF

!               Contribution of cooling to the surface due to water
!               vapour.
            IF (WaterPath < 0.04) THEN
              lp  = LOG(MAX(1.0e-05, WaterPath))
              lp  = EXP( cc_h(0) + lp * ( cc_h(1) + lp *                &
                    ( cc_h(2) + lp * ( cc_h(3) + lp * ( cc_h(4) +       &
                    lp * cc_h(5) ) ) ) ) )
            ELSE
              lp = cc_h_large / WaterPath
            END IF
            tx    = (tstar_tile(l,n) - t0_rfit) / dt_rfit
            ks    = QScrn * lp * (1.0 + t_scale_h2o_rfit*tx) *          &
                    (pl0_h2o_rfit + pl1_h2o_rfit*tx)

!               Contribution of cooling to the surface due to carbon
!               dioxide.
            lp    = LOG(CO2Path)
            lp    = cc_c(0) + lp * ( cc_c(1) + lp * ( cc_c(2) +         &
                    lp * ( cc_c(3) + lp * ( cc_c(4) + lp * cc_c(5)      &
                    ) ) ) )
            tx    = (tstar_tile(l,n) - t0_rfit) / dt_rfit
            ks    = ks + CO2Scrn * EXP(lp) *                            &
                    (1.0 - t_scale_co2_rfit*tx) *                       &
                    (pl0_co2_rfit - pl1_co2_rfit*tx)

            ks    = MAX(4.0 * sbcon * tstar_tile(l,n)**3 * ks /cp,      &
                      EPSILON(ks))
!               ks now holds K such that dT/dt = -K(T - T_surf)

!               Integrate forward in time across the timestep.
            exks = EXP( - ks * timestep )

!               Advance the decoupled temperature radiatively.
            TRadCool = tstar_tile(l,n) +                                &
              (TScrnDcl_TILE(l,n) - tstar_tile_old(l,n)) * exks -       &
              (tstar_tile(l,n) - tstar_tile_old(l,n)) *                 &
              (1.0 - exks) / (ks * timestep )

!               Calculate the weighting between the M-O and decoupled
!               values.

            tRotational_Inv = 0.4 * ABS(f3_at_p(i,j))

!               Only the GBM value of uStar is used here:
!               the use of different values of uStar on different
!               tiles would involve significant extra complexity, while
!               being unlikely to provide much extra benefit.

            ThDot = (tstar_tile(l,n) - tstar_tile_old(l,n)) /           &
                       timestep
            LTrans2 = ( uStarGBM(i,j)**3 /                              &
              ( (g/tstar_tile(l,n)) * (1.0e-08 + ABS(ThDot) ) ) ) *     &
              ( LOG(1.0 + z1p5m/z0m_tile(l,n)) /                        &
                LOG(1.0 + z1p5m/0.05) ) **2

            WeightDcl = EXP( - timestep * tStbTrans(i,j) *              &
              tRotational_Inv**2) * 1.0 /                               &
              (1.0 + MIN(0.2*uStarGBM(i,j)*timestep/z1p5m,              &
                         LTrans2/z1p5m**2)*(timestep/300.0))

!               Interpolate between the value obtained from surface
!               similarity theory and the radiative temperature, provided
!               that the radiative temperature is above the M-O value.
            t1p5m_tile(l,n) = t1p5m_tile(l,n) + WeightDcl *             &
              MAX(0.0, (TRadCool - t1p5m_tile(l,n)))
            t1p5m(i,j) = t1p5m(i,j) + flandg(i,j) *                     &
              tile_frac(l,n) * t1p5m_tile(l,n)
!               Reset the decoupled temperature to the newly
!               diagnosed one.
            TScrnDcl_TILE(l,n) = t1p5m_tile(l,n)

          END IF

        END DO

      END DO

!         Release space no longer required.
      DEALLOCATE(LDclDiag)
      DEALLOCATE(TStarGBM)

    END IF

  END IF

IF (lhook) CALL dr_hook('SCREEN_TQ',zhook_out,zhook_handle)
RETURN
END SUBROUTINE screen_tq
