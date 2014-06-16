#if defined(OASIS3) 
! *****************************COPYRIGHT****************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt which you should
! have received as part of this distribution.
! *****************************COPYRIGHT****************************************
      Subroutine oasis_inita2o(                                        &
#include "argd1.h"
#include "argsts.h"
#include "argduma.h"
#include "argptra.h"
       icode, cmessage)
!
! Description: Initialise pointers to STASH diagnostic arrays
!              which are used when coupling with external ocean 
!              and ice models.
!              This is expected to be applicable to all OASIS
!              versions including OASIS3-MCT.
!
! Code Owner: See Unified Model Code Owners HTML page
! This file belongs in section: Coupling
!
!--------------------------------------------------------------------

      USE OASIS_process_translist_mod, ONLY: OASIS_process_translist

      ! Variable definitions for use with OASIS3 
      USE OASIS_atm_data_mod

      USE yomhook, ONLY: lhook, dr_hook
      USE parkind1, ONLY: jprb, jpim
      USE atm_fields_bounds_mod
      USE mask_compression, ONLY: expand_from_mask
      USE UM_ParVars
      USE domain_params
      USE switches,  ONLY:l_sice_multilayers, l_ctile
      USE Submodel_Mod

      IMPLICIT NONE

#include "typsize.h"
#include "typd1.h"
#include "typsts.h"
#include "typduma.h"
#include "typptra.h"
#include "atm_lsm.h"
!
      INTEGER       icode       ! OUT - Error return code
      CHARACTER(LEN=*) cmessage    ! OUT - Error return message
!*----------------------------------------------------------------------
!
#include "c_mdi.h"
#include "stparam.h"
!
!  Local variables
!
      INTEGER :: process_code   ! Processing code
      INTEGER :: freq_code      ! Frequency code
      INTEGER :: start,end,period ! Start, end and period step
      INTEGER :: gridpt_code,weight_code ! Gridpt and weighting codes
      INTEGER :: bottom_level,top_level ! Bottom and top input level
      INTEGER :: grid_n,grid_s,grid_w,grid_e ! Grid corner definitions
      INTEGER :: stashmacro_tag ! STASHmacro tag number
      INTEGER :: field_code     ! Temporary STASH field code holder

      INTEGER :: nlandpt        ! Dummy argument for expand_from_mask
                                ! output field.
      INTEGER :: i, j, n, tc    ! Local loop indices

      INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
      INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
      REAL(KIND=jprb)               :: zhook_handle

!----------------------------------------------------------------------
!     Set grid definition information (undefined as search is on
!     STASHmacro tag number)
!
      IF (lhook) CALL dr_hook('OASIS_INITA2O',zhook_in,zhook_handle)
      process_code=imdi
      freq_code   =imdi
      start       =imdi
      end         =imdi
      period      =imdi
      gridpt_code =imdi
      weight_code =imdi
      bottom_level=imdi
      top_level   =imdi
      grid_n      =imdi
      grid_s      =imdi
      grid_e      =imdi
      grid_w      =imdi

      ! Initialise flags for outgoing coupling fields
      l_heatflux = .FALSE.
      l_bluerad = .FALSE.
      l_solar = .FALSE.
      l_runoff = .FALSE.
      l_w10 = .FALSE.
      l_train = .FALSE.
      l_tsnow = .FALSE.
      l_evap2d = .FALSE.
      l_taux = .FALSE.
      l_tauy = .FALSE.
      l_sublim(:) = .FALSE.
      l_topmeltn(:) = .FALSE.
      l_fcondtopn(:) = .FALSE.

      ! Initialise flags for incoming coupling fields
      l_ocn_sst = .FALSE.
      l_ocn_u = .FALSE.
      l_ocn_v = .FALSE.
      l_ocn_freeze = .FALSE.
      l_ocn_icetn(:) = .FALSE.
      l_ocn_icekn(:) = .FALSE.
      l_ocn_freezen(:) = .FALSE.
      l_ocn_snowthickn(:) = .FALSE.
      l_ocn_hicen(:) = .FALSE.

      ! Set up dimensions for use with incoming and outgoing 
      ! transient arrays.
      oasis_imt=lasize(1,fld_type_p,halo_type_no_halo)
      oasis_jmt=lasize(2,fld_type_p,halo_type_no_halo)
      oasis_jmt_u=lasize(2,fld_type_u,halo_type_no_halo)
      oasis_jmt_v=lasize(2,fld_type_v,halo_type_no_halo)

      ! ALLOCATE space for incoming arrays and associated
      ! fields required in processing them. At the moment we do ALL
      ! potential fields, even though some of them may not be 
      ! needed, because we want to set up pointers to them when 
      ! we define each transient (via a derived type.) 
      ALLOCATE(ocn_sst(oasis_imt,oasis_jmt))
      ALLOCATE(ocn_sst_orig(oasis_imt,oasis_jmt))
      ALLOCATE(tstar_local(oasis_imt,oasis_jmt))
      ALLOCATE(tstar_ssi(oasis_imt,oasis_jmt))
      ALLOCATE(tstar_sice_local(oasis_imt,oasis_jmt))
      ALLOCATE(tstar_sice_cat_local(oasis_imt,oasis_jmt,nice_use))
      ALLOCATE(tstar_land_local(oasis_imt,oasis_jmt))
      ALLOCATE(ocn_freeze(oasis_imt,oasis_jmt))
      ALLOCATE(ocn_freezen(oasis_imt,oasis_jmt,nice))
      ALLOCATE(ocn_hice(oasis_imt,oasis_jmt))
      ALLOCATE(ocn_hicen(oasis_imt,oasis_jmt,nice))
      ALLOCATE(ocn_snowthick(oasis_imt,oasis_jmt))
      ALLOCATE(ocn_snowthickn(oasis_imt,oasis_jmt,nice))
      ALLOCATE(ocn_icetn(oasis_imt,oasis_jmt,nice))
      ALLOCATE(ocn_icet_gb(oasis_imt,oasis_jmt))
      ALLOCATE(ocn_icekn(oasis_imt,oasis_jmt,nice))
      ALLOCATE(ocn_u(oasis_imt,oasis_jmt_u))
      ALLOCATE(ocn_v(oasis_imt,oasis_jmt_v))   

      ! ALLOCATE space for outgoing arrays. At the moment we do ALL
      ! potential fields, even though some of them may not be 
      ! needed, because we want to set up pointers to them when 
      ! we define each transient (via a derived type.)  
      ALLOCATE(taux(oasis_imt,oasis_jmt_u))
      ALLOCATE(tauy(oasis_imt,oasis_jmt_v))
      ALLOCATE(solar2d(oasis_imt,oasis_jmt))
      ALLOCATE(blue2d(oasis_imt,oasis_jmt))
      ALLOCATE(evap2d(oasis_imt,oasis_jmt))
      ALLOCATE(sublim(oasis_imt,oasis_jmt,nice_use))
      ALLOCATE(longwave2d(oasis_imt,oasis_jmt))
      ALLOCATE(sensible2d(oasis_imt,oasis_jmt))

      ALLOCATE(rainls(oasis_imt,oasis_jmt))
      ALLOCATE(snowls(oasis_imt,oasis_jmt))
      ALLOCATE(rainconv(oasis_imt,oasis_jmt))
      ALLOCATE(snowconv(oasis_imt,oasis_jmt))
      ALLOCATE(calving(oasis_imt,oasis_jmt))
      ALLOCATE(riverout(oasis_imt,oasis_jmt))
      ALLOCATE(w10(oasis_imt,oasis_jmt))

      ALLOCATE(heatflux(oasis_imt,oasis_jmt))
      ALLOCATE(totalrain(oasis_imt,oasis_jmt))
      ALLOCATE(totalsnow(oasis_imt,oasis_jmt))

      ALLOCATE(topmeltn(oasis_imt,oasis_jmt,nice))
      ALLOCATE(fcondtopn(oasis_imt,oasis_jmt,nice))

      ! Set up fractional land points
      ALLOCATE(fland_loc(oasis_imt,oasis_jmt))

      CALL expand_from_mask(fland_loc,D1(JFRAC_LAND),                   &
       atmos_landmask_local,                                            &
       lasize(1,fld_type_p,halo_type_no_halo)*                          &
       lasize(2,fld_type_p,halo_type_no_halo), nlandpt)

      ! Ensure land fraction is zero on any missing data points. 
      DO j=1,oasis_jmt
         DO i=1,oasis_imt
            IF (fland_loc(I,J).eq.RMDI) fland_loc(I,J)=0.0
         END DO
      END DO

      ! Process our transient list to identify those fields which need
      ! any special treatment
      CALL OASIS_process_translist()

      ! Set up pointers for the transients we're using in this run 
! DEPENDS ON: OASIS_point_translist
      CALL OASIS_point_translist(nice,nice_use)

!----------------------------------------------------------------------
! Get address for each field from its STASH section/item code
! and STASHmacro tag if a diagnostic, or from its primary pointer
! if prognostic or ancillary field
! Atmosphere -> Ocean (tag=10)
!
      stashmacro_tag=10

      IF (l_taux) THEN

        ! Get the surface X-component of windstress (U grid)
        ! The actual field we need depends on whether
        ! coastal tiling is active or not.
        IF (L_CTILE) THEN
          field_code = 392
        ELSE
          field_code = 219
        END IF


! DEPENDS ON: findptr
        CALL findptr(atmos_im,3,field_code,                              &
        process_code,freq_code,start,end,period,                         &
        gridpt_code,weight_code,                                         &
        bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,              &
        stashmacro_tag,imdi,ja_taux,                                     &
#include "argsts.h"
        icode,cmessage)


        IF (ja_taux==0) THEN
          icode=3000 + field_code
          cmessage="oasis_inita2o: Coupling field not enabled - taux"
        END IF

      END IF ! taux requested


      IF (l_tauy) THEN

        ! Get the surface Y-component of windstress (V grid)
        IF (L_CTILE) THEN
          field_code = 394
        ELSE
          field_code = 220
        END IF
      
! DEPENDS ON: findptr
        CALL findptr(                                                    &   
        atmos_im,3,field_code,                                           &
        process_code,freq_code,start,end,period,                         &
        gridpt_code,weight_code,                                         &
        bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,              &
        stashmacro_tag,imdi,ja_tauy,                                     &
#include "argsts.h"
        icode,cmessage)

        IF (ja_tauy==0) THEN
          icode=3000 + field_code
          cmessage="oasis_inita2o: Coupling field not enabled - tauy"
        END IF
      END IF ! tauy requested



      IF (l_heatflux) THEN
        ! Set up pointers to various components specifically invloved in
        ! the heatflux field. 

! Net integrated downward solar on atmos grid
! DEPENDS ON: findptr
        CALL findptr(                                                      &
        atmos_im,1,203,                                                  &
        process_code,freq_code,start,end,period,                         &
        gridpt_code,weight_code,                                         &
        bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,              &
        stashmacro_tag,imdi,ja_solar,                                    &
#include "argsts.h"
        icode,cmessage)

        IF (ja_solar==0) THEN
          icode=1203
          cmessage="oasis_inita2o: Cpling field not enabled - Net solar"
        END IF

        IF (icode==0) THEN
! Net downward longwave on atmos grid
! DEPENDS ON: findptr
          CALL findptr(                                                    &
          atmos_im,2,203,                                                &
          process_code,freq_code,start,end,period,                       & 
          gridpt_code,weight_code,                                       &    
          bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,            &
          stashmacro_tag,imdi,ja_longwave,                               &
#include "argsts.h"
          icode,cmessage)

          IF (ja_longwave==0) THEN
            icode=2203
            cmessage="oasis_inita2o: Coupling field not enabled - Longwave"
          END IF
        END IF

        IF (icode==0) THEN
! Sensible heat on atmos grid, area mean over open sea
! DEPENDS ON: findptr
          CALL findptr(                                                   &
          atmos_im,3,228,                                               &
          process_code,freq_code,start,end,period,                      &
          gridpt_code,weight_code,                                      &
          bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,           &
          stashmacro_tag,imdi,ja_sensible,                              &
#include "argsts.h"
          icode,cmessage)

          IF (ja_sensible==0) THEN
            icode=3228
            cmessage=                                                     &
           "oasis_inita2o: Coupling field not enabled - Sensible heat"
          END IF
        END IF

      END IF   ! l_heatflux=true

      IF (l_bluerad) THEN

        ! Actual blue-band field used depends on coastal tiling switch.
        IF (L_CTILE) THEN
           field_code = 260
        ELSE
           field_code = 204
        END IF

        IF (icode==0) THEN
! Net downward blueband solar on atmos grid
! DEPENDS ON: findptr
          CALL findptr(                                                  &
            atmos_im,1,field_code,                                       & 
            process_code,freq_code,start,end,period,                     &
            gridpt_code,weight_code,                                     &
            bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,          &
            stashmacro_tag,imdi,ja_blue,                                 &
#include "argsts.h"
            icode,cmessage)


          IF (ja_blue==0) THEN
            icode=1000 + field_code
            cmessage=                                                     &
           "oasis_inita2o: Coupling field not enabled - Blue solar atm"
          END IF
        END IF

       END IF   ! l_bluerad=true


      IF (l_evap2d) THEN
      
        IF (icode==0) THEN
! Surface evaporation over sea weighted by fractional leads
! DEPENDS ON: findptr
          CALL findptr(                                                    &
          atmos_im,3,232,                                                &
          process_code,freq_code,start,end,period,                       & 
          gridpt_code,weight_code,                                       &
          bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,            &
          stashmacro_tag,imdi,ja_evap,                                   & 
#include "argsts.h"
          icode,cmessage)

          IF (ja_evap==0) THEN
            icode=3232
            cmessage=                                                      &
            "oasis_inita2o: Coupling field not enabled - Evap over sea"
          END IF
        END IF
      END IF 

             
      IF (l_tsnow) THEN
                                                    
        IF (icode==0) THEN
! Large-scale snowfall rate on atmos grid
! DEPENDS ON: findptr
        CALL findptr(atmos_im, 4,204,                                     &
                   process_code,freq_code,start,end,period,             &
                   gridpt_code,weight_code,                             &
                   bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,  &
                   stashmacro_tag,imdi,ja_lssnow,                       &
#include "argsts.h"
                             icode,cmessage)

          IF (ja_lssnow == 0) THEN
            icode=4204
            cmessage="oasis_inita2o: Coupling field not enabled - LS Snow"
          END IF
        END IF

        IF (icode==0) THEN
! Convective snowfall rate on atmos grid
! DEPENDS ON: findptr
          CALL findptr(atmos_im, 5,206,                                 &
                   process_code,freq_code,start,end,period,             &
                   gridpt_code,weight_code,                             &
                   bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,  &
                   stashmacro_tag,imdi,ja_cvsnow,                       &
#include "argsts.h"
                             icode,cmessage)

          IF (ja_cvsnow == 0) THEN
            icode=5206
            cmessage="oasis_inita2o: Coupling field not enabled - Conv Snow"
          END IF
        END IF
      END IF  ! Total snow


      IF (l_train) THEN

        IF (icode==0) THEN
! Large-scale rainfall rate on atmos grid
! DEPENDS ON: findptr
          CALL findptr(atmos_im, 4,203,                                 &
                   process_code,freq_code,start,end,period,             &
                   gridpt_code,weight_code,                             &
                   bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,  &
                   stashmacro_tag,imdi,ja_lsrain,                       &
#include "argsts.h"
                             icode,cmessage)
          IF (ja_lsrain == 0) THEN
            icode=4203
            cmessage="oasis_inita2o: Coupling field not enabled - LS Rain"
          END IF
        END IF


        IF (icode==0) THEN
! Convective rainfall rate on atmos grid
! DEPENDS ON: findptr
          CALL findptr(atmos_im, 5,205,                                 &
                   process_code,freq_code,start,end,period,             &
                   gridpt_code,weight_code,                             &
                   bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,  &
                   stashmacro_tag,imdi,ja_cvrain,                       &
#include "argsts.h"
                             icode,cmessage)

          IF (ja_cvrain == 0) THEN
            icode=5205
            cmessage="oasis_inita2o: Coupling field not enabled - Conv Rain"
          END IF
        END IF
      END IF   ! Total rain

      IF (l_runoff) THEN

! River routing (if required for Ocean
        IF (icode==0) THEN
! RIVER OUTFLOW on atmos grid
! DEPENDS ON: findptr
          CALL findptr(atmos_im, 26,004,                                &
                   process_code,freq_code,start,end,period,             &
                   gridpt_code,weight_code,                             &
                   bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,  &
                   stashmacro_tag,imdi,ja_riverout,                     &
#include "argsts.h"
                                 icode,cmessage)

          IF (ja_riverout == 0) THEN
            icode=26004
            cmessage=                                                   &
            "oasis_inita2o: Coupling field not enabled - River Outflow ATMOS"
          END IF
        END IF
      END IF

      
      IF (l_w10) THEN

! 10m wind
        IF (icode==0) THEN
! 10m wind on atmos grid
! DEPENDS ON: findptr
          CALL findptr(atmos_im, 3,230,                                 &
                   process_code,freq_code,start,end,period,             &
                   gridpt_code,weight_code,                             &
                   bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,  &
                   stashmacro_tag,imdi,ja_w10,                          &
#include "argsts.h"
                                 icode,cmessage)

          IF (ja_w10 == 0) THEN
            icode=03230
            cmessage=                                                   &
          "oasis_inita2o: Coupling field not enabled - 10m wind ATMOS"
          END IF
        END IF
      END IF



      IF (l_sublim(1)) THEN
! Sublimation rate
! Note that field 3353 is a rate(kg/m2/s) and 3231 is a total (kg/ts). 
! Sublimation usually needs to be passed to ocean models (eg. NEMO)  
! as rate. So if coastal tiling is not used, the sublimation total 
! field is converted to a rate in oasis_updatecpl.
! (See UM7.0 or earlier swap_a2o for historical perspective).

        IF (icode==0) THEN
! Sublimation
          IF (L_CTILE) THEN
            IF (NICE_USE .EQ. 1) THEN
              field_code = 353 ! Single category field
            ELSE
              field_code = 509 ! Multi-category field
            END IF
          ELSE
            field_code = 231
          END IF
        END IF

! DEPENDS ON: findptr
        CALL findptr(atmos_im, 3,field_code,                           &
                   process_code,freq_code,start,end,period,             &
                   gridpt_code,weight_code,                             &
                   bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,  &
                   stashmacro_tag,imdi,ja_sublim,                       &
#include "argsts.h"
                             icode,cmessage)

        IF (ja_sublim == 0) THEN
           icode=3000 + field_code
           cmessage="oasis_inita2o: Cpling field not enabled - Sublim"
        END IF
      END IF

      IF (l_fcondtopn(1)) THEN
        IF (icode==0) THEN
! Multi category FCONDTOP
! NOTE: We're assuming that only multi category fields are being used!

          IF (l_sice_multilayers) THEN
          ! Multilayers coupling: fcondtop = the sea ice surface downwards
          ! heat flux (surf_ht_flux_sice in UM)

! DEPENDS ON: findptr
            CALL findptr(atmos_im, 3,510,                                  &
                     process_code,freq_code,start,end,period,            &
                     gridpt_code,weight_code,                            &
                     bottom_level,top_level,grid_n,grid_s,grid_w,grid_e, &
                     stashmacro_tag,imdi,ja_fcondtopn,                   &
#include "argsts.h"
                              icode,cmessage)

          ELSE
          ! 'Zero layer' coupling: fcondtop = the sea ice conductive flux 
          !  through the ice (sea_ice_htf in UM, or botmelt in old
          !  UM ocean language)

! DEPENDS ON: findptr
            CALL findptr(atmos_im, 3,256,                                  &
                     process_code,freq_code,start,end,period,            &
                     gridpt_code,weight_code,                            &
                     bottom_level,top_level,grid_n,grid_s,grid_w,grid_e, &
                     stashmacro_tag,imdi,ja_fcondtopn,                   &
#include "argsts.h"
                              icode,cmessage)
          END IF
         
          IF (ja_fcondtopn == 0) THEN
            icode=3256
            cmessage="oasis_inita2o: Cpling field not enabled - FCONDTOPN"
          END IF
        END IF
      END IF

      
      IF (l_topmeltn(1)) THEN
        IF (icode==0) THEN
! Multi category TOPMELT
! NOTE: We're assuming that only multi category fields are being used!
! DEPENDS ON: findptr
          CALL findptr(atmos_im, 3,257,                                 &
                   process_code,freq_code,start,end,period,             &
                   gridpt_code,weight_code,                             &
                   bottom_level,top_level,grid_n,grid_s,grid_w,grid_e,  &
                   stashmacro_tag,imdi,ja_topmeltn,                     &
#include "argsts.h"
                              icode,cmessage)

          IF (ja_topmeltn == 0) THEN
            icode=3257
            cmessage="oasis_inita2o: Cpling field not enabled - TOPMELTN"
          END IF
        END IF
      END IF




      IF (lhook) CALL dr_hook('OASIS_INITA2O',zhook_out,zhook_handle)
      RETURN

      End Subroutine oasis_inita2o
#endif
