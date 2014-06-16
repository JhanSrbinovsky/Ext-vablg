! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

!    Subroutine FTSA -------------------------------------------------
!
!    It calculates (true) surface albedos for sea and sea ice
!    Release 2.8 of the UM allows for separate surface
!    albedos for direct and diffuse light over sea, calculating the
!    former from the formula of Briegleb and Ramanathan (1982, J. Appl.
!    Met., 21, 1160-1171) and passes the albedos out in different form
!                                              William Ingram 25/9/92
!    Suitable for single column model use.
!
!    Author: William Ingram
!
!    Offline documentation is in UMDP 23, sections "True surface albedo
!    specification" and "Modifications to the radiation scheme to
!    accommodate the leads model"
!
!    There are two options for the sea ice albedo scheme: 
!    1) the default albedo parameterisation of the Los Alamos sea ice 
!    model CICE vn4.1 (Hunke and Lipscomb, 2010) which was used in CCSM3.
!    This calculates the albedo for 4 bands, visible/near-IR 
!    diffuse/direct.
!    2) the original UM scheme which simply uses 1 band.
!
SUBROUTINE ftsa (                                                       &
  flandg, aice, aice_cat, tstar, tstar_sice_cat, cosz, s, s_sea_cat,    &
  di_cat,                                                               &
  alpham,alphac,alphab,dtice,l_ssice_albedo, l_mod_barker_albedo,       &
  l_sice_meltponds,l_sice_scattering,l_sice_hadgem1a,l_cice_alb,        &
  dt_bare,dalb_bare_wet,pen_rad_frac,beta,                              &
  l1, l2, nice_use, sa_sice, saos)

USE c_0_dg_c
USE fluxes, ONLY: alb_sice
USE ancil_info, ONLY:                                                   &
  sice_pts, ssi_index, sice_pts_ncat, sice_index_ncat, sice_frac_ncat
USE c_kappai, ONLY : kappai,kappai_snow,rhosnow
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                  &
     l1,                                                                &
!      Full field dimension
     l2,                                                                &
!      Number of points to treat
     nice_use
!      Number of sea ice categories used in radiation scheme

LOGICAL, INTENT(IN) ::                                                  &
     l_ssice_albedo,                                                    &
!      Switch on the effect of snow on sea-ice albedo
     l_mod_barker_albedo,                                               &
!      Use modified Barker albedo (open sea).
     l_sice_meltponds,                                                  &
!      Switch on seaice albedo meltponds
     l_sice_scattering,                                                 &
!      Switch on seaice albedo internal scatter
     l_sice_hadgem1a,                                                   &
!      Switch for HadGEM1 bug correction NOT USED IN JULES
!      Remains to maintain a consistent
!      subroutine interface for the UM
     l_cice_alb
!      Switch to use CICE albedo scheme

REAL, INTENT(IN) ::                                                     &
     flandg(l1),                                                        &
                                    ! Land fraction
     aice(l1),                                                          &
                                    ! Sea-ice fraction
     aice_cat(l1,nice_use),                                             &
                                    ! Category sea-ice fraction
     tstar(l1),                                                         &
                                    ! Surface temperature
     tstar_sice_cat(l1,nice_use),                                       &
                                    ! Category seaice surface temperature
     cosz(l1),                                                          &
                                    ! cos(solar zenith angle)
     s(l1),                                                             &
                                    ! Snow amount (mass/area)
     s_sea_cat(l1,nice_use),                                            &
                     ! Category snow amount on sea ice (mass/area of ice)

     di_cat(l1, nice_use)
                     ! Effective thickness of sea ice categories

REAL, INTENT(IN) ::                                               &
!                       
!     Constants used to determine the albedo of sea-ice:
! Albedo of sea-ice at melting point (TM) if .not.l_ssice_albedo, or
! Albedo of snow on sea-ice at melting point (TM) if l_ssice_albedo
!
     alpham                                                       &
                       ! "M" for "melting"
! Albedo of sea-ice at and below TM-DTICE if .not.l_ssice_albedo, or
! Albedo of snow on sea-ice at and below TM-DTICE if l_ssice_albedo
!
    ,alphac                                                       &
                       ! "C" for "cold"
! Albedo of snow-free sea-ice if l_ssice_albedo
    ,alphab                                                       &
                       ! "B" for "bare"
!
! Temperature range in which albedo of sea-ice, if .not.l_ssice_albedo,
! or of snow on sea-ice, if l_ssice_albedo, varies between its limits
    ,dtice                                                        &
! Temperature range below TM over which meltponds form if
! l_sice_meltponds and l_ssice_albedo
    ,dt_bare                                                      &
! Increment to albedo for each degree temperature rises above
! TM-DT_BARE. Only used if l_sice_meltponds and l_ssice_albedo
    ,dalb_bare_wet                                                &
! Fraction of SW radiation that penetrates seaice and scatters
! back causing an addition to the albedo. Only active if l_ssice_albedo
! and l_sice_scattering
    ,pen_rad_frac                                                 &
! attenutation parameter for SW in seaice which controls the
! additional albedo due to internal scattering
   ,beta

REAL, INTENT(OUT) ::                                                    &
     sa_sice(l1, 4),                                                    &
!      Surface Albedo for sea ice
!             (*,1) - direct beam visible
!             (*,2) - diffuse visible
!             (*,3) - direct beam near-ir
!             (*,4) - diffuse near-ir
     saos(l1, 2)
!      Surface albedo for Open Sea (direct and diffuse components)
!      (with zeros for safety where no value applies)

INTEGER band, n, j, l, ll           ! Loops over points
REAL dsa                            ! Deep-snow albedo (alphasubD)
REAL bx                                                           &
                                    ! 1 - COSZ
! Temperature at which (snow on) sea-ice reaches its "cold" value
    ,tcice                                                        &
! Slope and intercept of temperature dependence of the albedo of
! (snow on) sea-ice
    ,ice1,ice2

! Local parameters

REAL, PARAMETER :: adifc = 0.06
!      Surface albedo of ice-free sea for the diffuse beam

REAL, PARAMETER :: maskd = 0.2
!      Masking depth (S in 3.6.1)

! Parameters for the CICE scheme:
REAL, PARAMETER :: albicev = 0.78
!      Visible ice albedo for h > ahmax and T < -1C
REAL, PARAMETER :: albicei = 0.36
!      Near-ir ice albedo for h > ahmax and T < -1C
REAL, PARAMETER :: albsnowv = 0.98
!      Visible snow albedo for cold snow (T < -1C)
REAL, PARAMETER :: albsnowi = 0.70
!      Near-ir snow albedo for cold snow (T < -1C)
REAL, PARAMETER :: ahmax = 0.3
!      Albedo is constant above this thickness (metres)
REAL, PARAMETER :: dalb_mlt  = -0.075
!      Ice albedo change per 1 degree change in temp for ice (for -1C to 0C)
REAL :: dalb_mlts(4)
DATA dalb_mlts  /-0.1,-0.1,-0.15,-0.15/
!      Snow albedo change per 1 degree change in temp (for -1C to 0C)
!       (values for each radiation band)
REAL, PARAMETER :: snowpatch = 0.02
!      Length scale for parameterizing non uniform snow coverage (m)


REAL :: fh, albo, albice(4), albsnow(4), fT, albs(4), area_snow
REAL :: fhtan 

REAL :: snow_albedo
!      Snow albedo

REAL :: hice(l1,nice_use)
!      Ice thickness

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook('FTSA',zhook_in,zhook_handle)

fhtan = ATAN(ahmax*4.0)

! Open Sea albedo
saos(:,:) = 0.0

DO j=1, l2
  IF (flandg(j) <  1.0) THEN
    IF (l_mod_barker_albedo) THEN
       bx=1.0-cosz(j)
       saos(j, 1)= 0.0315 + 0.0421*bx**2                          &
             + 0.128*bx**3 - 0.04*bx**4                           &
             + (3.12/(5.68) + (0.074*bx/(1.0)))*bx**5
    ELSE
       saos(j, 1)=0.026/(cosz(j)**1.7+0.065)                      &
          +0.15*(cosz(j)-0.1)*(cosz(j)-0.5)*(cosz(j)-1.0)
    END IF
    saos(j,2) = adifc
  END IF
END DO

! Sea ice albedo

sa_sice (:,:)   = 0.0
alb_sice(:,:,:) = 0.0

IF (l_cice_alb) THEN

! Parametrisation from Los Alamos sea ice model (CICE vn4.1), 
!         their 'default' option
  albice(1)=albicev  ! Direct=diffuse
  albice(2)=albicev
  albice(3)=albicei
  albice(4)=albicei
  albsnow(1)=albsnowv
  albsnow(2)=albsnowv
  albsnow(3)=albsnowi
  albsnow(4)=albsnowi

  DO band = 1, 4
    DO n = 1, nice_use
      DO l = 1, sice_pts_ncat(n)
        ll=sice_index_ncat(l,n)
        j=ssi_index(ll)

        ! Convert effective ice thickness to true ice thickness         
        hice(j,n) = di_cat(j,n)                                         &
             - (kappai/kappai_snow) * (s_sea_cat(j,n)/rhosnow)

        ! Bare ice, thickness dependence
        fh = MIN(atan(hice(j,n)*4.0)/fhtan, 1.0)
        albo = albice(band)*fh + adifc*(1.0-fh)

        ! Bare ice, temperature dependence
        fT = MIN(tm-tstar_sice_cat(j,n)-1.0, 0.0)
        alb_sice(ll,n,band) = MAX(albo - dalb_mlt*fT, adifc)

        ! Snow, temperature dependence
        IF (s_sea_cat(j,n) > 0.) THEN
          albs(band) = albsnow(band)
          albs(band) = albs(band) - dalb_mlts(band)*fT
          ! Note rhosnow in next line is required to convert s_sea_cat 
          !        from kg/m2 to m
          area_snow = s_sea_cat(j,n)                                    &
               / (s_sea_cat(j,n) + snowpatch*rhosnow)
        ELSE
          area_snow = 0.0
        ENDIF    

        ! Combine snow and ice albedo
        alb_sice(ll,n,band) =                                           &
                  alb_sice(ll,n,band) * (1.0 - area_snow)  +            &
                  albs(band)*area_snow

        ! Mean sea ice albedo
        sa_sice(j, band) = sa_sice(j, band) +                           &
          alb_sice(ll,n,band)*sice_frac_ncat(ll,n) / aice(j)
      END DO
    END DO
  END DO

ELSE

! Use old UM schemes
! Derive 3 constants from the basic quantities (supplied in the
! UM namelist RUN_Radiation) for sea-ice albedo calculations:
  tcice = tm - dtice
  ice1 = (alpham-alphac) / dtice
  ice2 = alpham - tm*ice1

  DO n = 1, nice_use
    DO l = 1, sice_pts_ncat(n)
      ll=sice_index_ncat(l,n)
      j=ssi_index(ll)

      IF (l_ssice_albedo) THEN   ! Which albedo scheme?
        ! Effect of snow on sea-ice albedo

        IF (s_sea_cat(j,n) >  0.0) THEN   ! snow on sea ice
                                          ! Cox et al., Clim Dyn,1999
          IF (tstar_sice_cat(j,n) >  tcice) THEN
            snow_albedo=ice2+ice1*tstar_sice_cat(j,n)
          ELSE
            snow_albedo=alphac
          END IF

          alb_sice(ll,n,1)=alphab                                         &
          +(snow_albedo-alphab)*(1.0-EXP(-maskd*s_sea_cat(j,n)))

        ELSE           ! no snow so bare ice only

          IF(l_sice_meltponds) THEN

            ! bare ice, temperature dep. (Ebert &Curry,1993)
            IF (tstar_sice_cat(j,n) > (tm - dt_bare))THEN
              alb_sice(ll,n,1) = alphab+(tstar_sice_cat(j,n)-tm+dt_bare)  &
                                 *dalb_bare_wet
            ELSE      ! surface is dry
              alb_sice(ll,n,1)=alphab
            END IF     ! end melt ponds

          ELSE         ! just use bare ice albedo

            alb_sice(ll,n,1)=alphab

          END IF       ! l_sice_meltponds

          IF(l_sice_scattering) THEN
            ! Semtner modification dry albedo for internal
            ! scattering (Semnter, 1976)
            alb_sice(ll,n,1)=alb_sice(ll,n,1)+beta*(1.0-alb_sice(ll,n,1)) &
                                     *pen_rad_frac
          END IF       ! l_sice_scattering

        END IF         !  any snow on ice

      ELSE             ! default to operational NWP scheme 3.5.1:

        IF ( tstar_sice_cat(j,n)  <   tcice ) THEN
          alb_sice(ll,n,1) = alphac
        ELSE
          alb_sice(ll,n,1) = ice1 * tstar_sice_cat(j,n) + ice2
        END IF

      END IF          ! Which albedo scheme?

      IF (alb_sice(ll,n,1) <  0.0) THEN
        alb_sice(ll,n,1)=alpham
      END IF

!     For this scheme all band albedos are the same:
      DO band = 2, 4
        alb_sice(ll,n,band) = alb_sice(ll,n,1)
      END DO

    END DO  ! l
  END DO    ! n

! Calculate sea ice mean albedo
  DO band = 1, 4
    DO n = 1, nice_use
      DO l = 1, sice_pts_ncat(n)
        ll=sice_index_ncat(l,n)
        j=ssi_index(ll)
        sa_sice(j,band) = sa_sice(j,band) +                             &
               alb_sice(ll,n,band)*sice_frac_ncat(ll,n) / aice(j)
      END DO
    END DO
  END DO

END IF


IF (lhook) CALL dr_hook('FTSA',zhook_out,zhook_handle)
RETURN
END SUBROUTINE ftsa
