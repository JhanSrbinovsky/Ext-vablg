!==============================================================================
! This source code is part of the 
! Australian Community Atmosphere Biosphere Land Exchange (CABLE) model.
! This work is licensed under the CABLE Academic User Licence Agreement 
! (the "Licence").
! You may not use this file except in compliance with the Licence.
! A copy of the Licence and registration form can be obtained from 
! http://www.accessimulator.org.au/cable
! You need to register and read the Licence agreement before use.
! Please contact cable_help@nf.nci.org.au for any questions on 
! registration and the Licence.
!
! Unless required by applicable law or agreed to in writing, 
! software distributed under the Licence is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the Licence for the specific language governing permissions and 
! limitations under the Licence.
! ==============================================================================
!
! Purpose: Updates CABLE variables (as altered by first pass through boundary 
!          layer and convection scheme), calls cbm, passes CABLE variables back 
!          to UM. 'Implicit' is the second call to cbm in each UM timestep.
!
! Called from: UM/JULES sf_impl
!
! Contact: Jhan.Srbinovsky@csiro.au
!
! History: Developed for CABLE v1.8
!
! ==============================================================================

subroutine cable_implicit_driver( LS_RAIN, CON_RAIN, LS_SNOW, CONV_SNOW,       &
                                  DTL_1,DQW_1, TSOIL, TSOIL_TILE, SMCL,        &
                                  SMCL_TILE, timestep, SMVCST,STHF, STHF_TILE, &
                                  STHU,STHU_TILE, snow_tile, SNOW_RHO1L,       &
                                  ISNOW_FLG3L, SNOW_DEPTH3L, SNOW_MASS3L,      &
                                  SNOW_RHO3L, SNOW_TMP3L, SNOW_COND,           &
                                  FTL_1, FTL_TILE, FQW_1, FQW_TILE,    &
                                  TSTAR_TILE, &
                                  SURF_HT_FLUX_LAND, ECAN_TILE, ESOIL_TILE,    &
                                  EI_TILE, RADNET_TILE, TOT_ALB, SNAGE_TILE,   &
                                  CANOPY_TILE, GS, T1P5M_TILE, Q1P5M_TILE,     &
                                  CANOPY_GB, FLAND, MELT_TILE, DIM_CS1,        &
                                  DIM_CS2, NPP, NPP_FT, GPP, GPP_FT, RESP_S,   &
                                  RESP_S_TOT, RESP_S_TILE, RESP_P, RESP_P_FT,  &
                                  G_LEAF, & 
                                  LYING_SNOW, SURF_ROFF, SUB_SURF_ROFF,  &
                                  TOT_TFALL )

   USE cable_def_types_mod, ONLY : mp
   USE cable_data_module,   ONLY : PHYS
   USE cable_data_mod,   ONLY : cable 
   USE cable_um_tech_mod,   ONLY : um1, conv_rain_prevstep, conv_snow_prevstep, &
                                  air, bgc, canopy, met, bal, rad, rough, soil,&
                                  ssnow, sum_flux, veg
   USE cable_common_module, ONLY : cable_runtime, cable_user, ktau_gl
   USE cable_um_init_subrs_mod, ONLY : um2cable_rr
   USE cable_cbm_module,    ONLY : cbm

   IMPLICIT NONE
        
   REAL, DIMENSION(um1%ROW_LENGTH,um1%ROWS) ::                                 &
      LS_RAIN,  & ! IN Large scale rain
      LS_SNOW,  & ! IN Large scale snow
      CON_RAIN, & ! IN Convective rain
      CONV_SNOW,& ! IN Convective snow
      DTL_1,    & ! IN Level 1 increment to T field 
      DQW_1       ! IN Level 1 increment to q field 

   integer :: timestep

   INTEGER ::                                                                  &
      DIM_CS1, DIM_CS2 

   REAL, DIMENSION(um1%land_pts) ::                                            &
      GS,      &  ! OUT "Stomatal" conductance to
      SMVCST,  &  ! IN Volumetric saturation point
      FLAND       ! IN Land fraction on land tiles
   
   REAL, DIMENSION(um1%ROW_LENGTH,um1%ROWS) ::                                 &
      !--- Net downward heat flux at surface over land.
      !--- fraction of gridbox (W/m2).
      SURF_HT_FLUX_LAND,                                                       &   
      !--- Moisture flux between layers. (kg/m^2/sec).
      !--- FQW(,1) is total water flux from surface, 'E'.
      FQW_1,                                                                   &  
      !--- FTL(,K) =net turbulent sensible heat flux into layer K
      !--- from below; so FTL(,1) = surface sensible heat, H.(W/m2)
      FTL_1         

   REAL, DIMENSION(um1%land_pts,um1%ntiles) ::                              &
      !___Surface FTL, FQL for land tiles
      FTL_TILE, FQW_TILE,                                                   &
      
      !___(tiled) latent heat flux, melting, stomatatal conductance
     LE_TILE, MELT_TILE, GS_TILE,                                           &
     
     !___ INOUT Surface net radiation on tiles (W/m2)
     RADNET_TILE, &
     TOT_ALB,     & ! total albedo
     EI_TILE,     & ! OUT EI for land tiles.
     ECAN_TILE,   & ! OUT ECAN for snow-free land tiles
     ESOIL_TILE     ! evapotranspiration from soil moisture store (kg/m2/s) 

   !___ soil prognostics: moisture, frozen, unfrozen content, soil temp.
   !___ runoff ??
   REAL, dimension(um1%land_pts,um1%sm_levels) ::                           &
      SMCL,       & ! 
      STHF,       & !
      STHU,       & !
      TSOIL         !

   !___(tiled) soil prognostics: as above 
   REAL, dimension(um1%land_pts,um1%ntiles,um1%sm_levels) ::                &
      SMCL_TILE, & !
      STHU_TILE, & !
      TSOIL_TILE,& !
      STHF_TILE    !

   !___flag for 3 layer snow pack
   INTEGER :: ISNOW_FLG3L(um1%LAND_PTS,um1%NTILES)
   
   !___(tiled, 3 layer) Snow depth (m), mass, density, temp., conductivity
   REAL, dimension(um1%land_pts,um1%ntiles,3) :: &
      SNOW_DEPTH3L,  & ! 
      SNOW_MASS3L,   & !
      SNOW_RHO3L,    & !
      SNOW_TMP3L,    & !
      SNOW_COND        !

   REAL, DIMENSION(um1%land_pts,um1%ntiles) ::                              &
      FRS_TILE,   & ! Local
      NEE_TILE,   & ! Local
      NPP_TILE,   & ! Local
      GPP_TILE,   & ! Local
      GLEAF_TILE, & ! Local, kdcorbin, 10/10
      FRP_TILE,   &
      NPP_FT,     &
      NPP_FT_old, &
      GPP_FT,     &
      GPP_FT_old       

   REAL, DIMENSION(um1%land_pts) ::                                         &
      SNOW_GRD,    & !
      CANOPY_GB,   & !
      RESP_P,      & !
      NPP,         & !
      GPP            !
      
   REAL, DIMENSION( um1%land_pts,um1%ntiles ) ::                               &
      SNOW_TILE,     &
      SNOW_RHO1L,    &  ! Mean snow density
      SNAGE_TILE,    &
      CANOPY_TILE,   &
      T1P5M_TILE,    &
      Q1P5M_TILE,    &
      TSTAR_TILE,    &
      RESP_S_TILE,   & 
      RESP_P_FT,     &
      RESP_P_FT_old, &
      G_LEAF

   REAL ::                                                                     &
      RESP_S(um1%LAND_PTS,DIM_CS1),     &
      RESP_S_old(um1%LAND_PTS,DIM_CS1), &
      RESP_S_TOT(DIM_CS2)    
     
   REAL, DIMENSION(mp) ::                                                      & 
      dtlc, & 
      dqwc
   
   REAL, INTENT(OUT), DIMENSION(um1%LAND_PTS) ::                               &
      LYING_SNOW,    & ! OUT Gridbox snowmass (kg/m2)        
      SUB_SURF_ROFF, & !
      SURF_ROFF,     & !
      TOT_TFALL        !

   REAL, POINTER :: TFRZ

   !___ 1st call in RUN (!=ktau_gl -see below) 
   LOGICAL, SAVE :: first_cable_call = .TRUE.

   integer :: itest =2
   integer :: iunit=77772
   character(len=44) :: testfname = "/home/599/jxs599/cable_implicit.txt"
    
      TFRZ => PHYS%TFRZ

      ! FLAGS def. specific call to CABLE from UM
      cable_runtime%um_explicit = .FALSE.
      cable_runtime%um_implicit = .TRUE.
   
      dtlc = 0. ; dqwc = 0.

      !--- All these subrs do is pack a CABLE var with a UM var.
      !-------------------------------------------------------------------
      !--- UM met forcing vars needed by CABLE which have UM dimensions
      !---(rowlength,rows)[_rr], which is no good to CABLE. These have to be 
      !--- re-packed in a single vector of active tiles. Hence we use 
      !--- conditional "mask" l_tile_pts(land_pts,ntiles) which is .true.
      !--- if the land point is/has an active tile
      !--- generic format:
      !--- um2cable_rr( UM var, default value for snow tile, CABLE var, mask )
      !--- where mask tells um2cable_rr whether or not to use default value 
      !--- for snow tile 
      !-------------------------------------------------------------------
      CALL um2cable_rr( (LS_RAIN+CON_RAIN)*um1%TIMESTEP, met%precip)
      CALL um2cable_rr( (LS_SNOW+CONV_SNOW)*um1%TIMESTEP, met%precip_sn)
print *, "jhan:_impl:sh(dtl_1)", shape(dtl_1)
print *, "jhan:_impl:sh(dqw_1)", shape(dqw_1)
print *, "jhan:_impl:sh(dtlc)", shape(dtlc)
print *, "jhan:_impl:sh(dqwc)", shape(dqwc)
      CALL um2cable_rr( dtl_1, dtlc)
      CALL um2cable_rr( dqw_1, dqwc)
      
      !--- conv_rain(snow)_prevstep are added to precip. in explicit call
      CALL um2cable_rr( (CON_RAIN)*um1%TIMESTEP, conv_rain_prevstep)
      CALL um2cable_rr( (CONV_snow)*um1%TIMESTEP, conv_snow_prevstep)
      
      met%precip   =  met%precip + met%precip_sn
      met%tk = met%tk + dtlc
      met%qv = met%qv + dqwc
      met%tvair = met%tk
      met%tvrad = met%tk
 
      canopy%cansto = canopy%oldcansto

      CALL cbm(real(TIMESTEP), air, bgc, canopy, met, bal,  &
           rad, rough, soil, ssnow, sum_flux, veg)
  
        
      CALL implicit_unpack( TSOIL, TSOIL_TILE, SMCL, SMCL_TILE,                &
                            SMVCST, STHF, STHF_TILE, STHU, STHU_TILE,          &
                            snow_tile, SNOW_RHO1L ,ISNOW_FLG3L, SNOW_DEPTH3L,  &
                            SNOW_MASS3L, SNOW_RHO3L, SNOW_TMP3L, SNOW_COND,    &
                            FTL_1, FTL_TILE, FQW_1,  FQW_TILE, TSTAR_TILE,     &
                            SURF_HT_FLUX_LAND, ECAN_TILE,        &
                            ESOIL_TILE, EI_TILE, RADNET_TILE, TOT_ALB,         &
                            SNAGE_TILE, CANOPY_TILE, GS, T1P5M_TILE,           &
                            Q1P5M_TILE, CANOPY_GB, FLAND, MELT_TILE, DIM_CS1,  &
                            DIM_CS2, NPP, NPP_FT, GPP, GPP_FT, RESP_S,         &
                            RESP_S_TOT, RESP_S_TILE, RESP_P, RESP_P_FT, G_LEAF )
       
! DEPENDS ON: cable_hyd_driver
      call cable_hyd_driver( SNOW_TILE, LYING_SNOW, SURF_ROFF, SUB_SURF_ROFF,  &
                             TOT_TFALL )


      cable_runtime%um_implicit = .FALSE.
  
END SUBROUTINE cable_implicit_driver


!========================================================================= 
!========================================================================= 
!========================================================================= 
        
SUBROUTINE implicit_unpack( TSOIL, TSOIL_TILE, SMCL, SMCL_TILE,                &
                            SMVCST, STHF, STHF_TILE, STHU, STHU_TILE,          &
                            snow_tile, SNOW_RHO1L ,ISNOW_FLG3L, SNOW_DEPTH3L,  &
                            SNOW_MASS3L, SNOW_RHO3L, SNOW_TMP3L, SNOW_COND,    &
                            FTL_1, FTL_TILE, FQW_1,  FQW_TILE, TSTAR_TILE,     &
                            SURF_HT_FLUX_LAND, ECAN_TILE,        &
                            ESOIL_TILE, EI_TILE, RADNET_TILE, TOT_ALB,         &
                            SNAGE_TILE, CANOPY_TILE, GS, T1P5M_TILE,           &
                            Q1P5M_TILE, CANOPY_GB, FLAND, MELT_TILE, DIM_CS1,  &
                            DIM_CS2, NPP, NPP_FT, GPP, GPP_FT, RESP_S,         &
                            RESP_S_TOT, RESP_S_TILE, RESP_P, RESP_P_FT, G_LEAF )
 
   USE cable_def_types_mod, ONLY : mp
   USE cable_data_module,   ONLY : PHYS
   USE cable_um_tech_mod,   ONLY : um1 ,canopy, rad, soil, ssnow, air
   USE cable_common_module, ONLY : cable_runtime, cable_user, fudge_out,       &
                                   L_fudge
   IMPLICIT NONE
 
   !jhan:these need to be cleaned out to what is actualllly passed
   INTEGER :: DIM_CS1 ,DIM_CS2 

   REAL, DIMENSION(um1%land_pts) ::                                            &
      GS,         &  ! OUT "Stomatal" conductance to
      SMVCST,     &  ! IN Volumetric saturation point
      FLAND          ! IN Land fraction on land tiles
   
   real, dimension(um1%ROW_LENGTH,um1%ROWS) ::                                 &
      !--- Net downward heat flux at surface over land.
      !--- fraction of gridbox (W/m2).
      SURF_HT_FLUX_LAND,           &
      !--- Moisture flux between layers. (kg/m^2/sec).
      !--- FQW(,1) is total water flux from surface, 'E'.
      FQW_1,       &  
      !--- FTL(,K) =net turbulent sensible heat flux into layer K
      !--- from below; so FTL(,1) = surface sensible heat, H.(W/m2)
      FTL_1         

   REAL, DIMENSION(um1%land_pts,um1%ntiles) ::                                 &
      !___Surface FTL, FQL for land tiles
      FTL_TILE, FQW_TILE,                 &  
      !___(tiled) latent heat flux, melting, stomatatal conductance
      LE_TILE, MELT_TILE, GS_TILE,     &  
      RADNET_TILE, & ! INOUT Surface net radiation on tiles (W/m2)
      TOT_ALB,     & ! total albedo
      EI_TILE,     & ! OUT EI for land tiles.
      ECAN_TILE,   & ! OUT ECAN for snow-free land tiles
      ESOIL_TILE     ! evapotranspiration from soil moisture store (kg/m2/s) 

   !___ soil prognostics: moisture, frozen, unfrozen content, soil temp.
   !___ runoff ??
   REAL, DIMENSION(um1%land_pts,um1%sm_levels) ::                              &
      SMCL,       & !
      STHF,       &
      STHU,       &
      TSOIL       

   !___(tiled) soil prognostics: as above 
   REAL, DIMENSION(um1%land_pts,um1%ntiles,um1%sm_levels) ::                   &
      SMCL_TILE,  & 
      STHU_TILE,  &
      TSOIL_TILE, &
      STHF_TILE  

   !___flag for 3 layer snow pack
   INTEGER :: ISNOW_FLG3L(um1%LAND_PTS,um1%NTILES)
   
   !___(tiled, 3 layer) Snow depth (m), mass, density, temp., conductivity
   REAL, DIMENSION(um1%land_pts,um1%ntiles,3) ::                               &
      SNOW_DEPTH3L,  &
      SNOW_MASS3L,   &
      SNOW_RHO3L,    &
      SNOW_TMP3L,    &
      SNOW_COND 

   REAL, dimension(um1%land_pts,um1%ntiles) ::                                 &
      FRS_TILE,   & ! Local
      NEE_TILE,   & ! Local
      NPP_TILE,   & ! Local
      GPP_TILE,   & ! Local
      SURF_HTF_T_CAB, &
      GLEAF_TILE, & ! Local, kdcorbin, 10/10
      FRP_TILE,   &
      NPP_FT,     &
      NPP_FT_old, &
      GPP_FT,     &
      GPP_FT_old       

   REAL, dimension(um1%land_pts) ::                                            &
      SNOW_GRD,   &  
      CANOPY_GB,  &
      RESP_P,     & 
      NPP,        & 
      GPP
   
   REAL, DIMENSION(um1%land_pts,um1%ntiles) ::                                 &
      SNOW_TILE,     & !
      SNOW_RHO1L,    & ! Mean snow density
      SNAGE_TILE,    & !
      CANOPY_TILE,   & !
      T1P5M_TILE,    &
      Q1P5M_TILE,    &
      TSTAR_TILE,    &
      RESP_S_TILE,   & 
      RESP_P_FT,     &
      RESP_P_FT_old, &
      G_LEAF

   REAL ::                                                                     &
      RESP_S(um1%LAND_PTS,DIM_CS1),    & !
      RESP_S_old(um1%LAND_PTS,DIM_CS1),& !
      RESP_S_TOT(DIM_CS2)                !
  
   REAL, DIMENSION(mp) ::                                                                     &
      fe_dlh,    & !
      fes_dlh,   & !
      fev_dlh      !

   !--- Local vars
   INTEGER :: i,j,l,k,n

   REAL, DIMENSION(um1%land_pts,um1%ntiles) ::                                 &
         !--- Local buffer surface FTL, FQL @ prev dt
         FTL_TILE_old, FQW_TILE_old

   INTEGER:: i_miss = 0
   REAL :: miss = 0.0
   
   REAL, POINTER :: TFRZ
   
   
      TFRZ => PHYS%TFRZ
  
      !--- set UM vars to zero
      SMCL_TILE = 0.; STHF_TILE = 0.; STHU_TILE = 0.

      DO j = 1,um1%SM_LEVELS
         TSOIL_TILE(:,:,j)= UNPACK(ssnow%tgg(:,j), um1%L_TILE_PTS, miss)
         SMCL_TILE(:,:,j)= UNPACK(REAL(ssnow%wb(:,j)), um1%L_TILE_PTS, miss)
         SMCL_TILE(:,:,j)=SMCL_TILE(:,:,j)*soil%zse(j)*um1%RHO_WATER
         STHF_TILE(:,:,j)= UNPACK(REAL(ssnow%wbice(:,j)), um1%L_TILE_PTS, miss)
         SMCL(:,j) = SUM(um1%TILE_FRAC * SMCL_TILE(:,:,j),2)
         TSOIL(:,j) = SUM(um1%TILE_FRAC * TSOIL_TILE(:,:,j),2)
         
         DO N=1,um1%NTILES
            DO K=1,um1%TILE_PTS(N)
               I = um1%TILE_INDEX(K,N)
               IF ( SMVCST(I) > 0. ) THEN ! Exclude permanent ice - mrd
                  STHF_TILE(I,N,J)= STHF_TILE(I,N,J)/SMVCST(I)
                  STHU_TILE(I,N,J)= MAX( 0., SMCL_TILE(I,N,J) -                &
                                    STHF_TILE(I,N,J) * SMVCST(I) * soil%zse(J) &
                                    * um1%RHO_WATER ) / ( soil%zse(J) *        &
                                    um1%RHO_WATER * SMVCST(I) )
               ENDIF
            ENDDO
         ENDDO

         STHF(:,J) = SUM(um1%TILE_FRAC * STHF_TILE(:,:,J),2)
         STHU(:,J) = SUM(um1%TILE_FRAC * STHU_TILE(:,:,J),2)
      ENDDO

      !--- unpack snow vars 
      SNOW_RHO1L  = UNPACK(ssnow%ssdnn, um1%L_TILE_PTS, miss)
      ISNOW_FLG3L = UNPACK(ssnow%isflag, um1%L_TILE_PTS, i_miss)
      MELT_TILE   = UNPACK(ssnow%smelt, um1%L_TILE_PTS, miss)
      SNOW_TILE= UNPACK(ssnow%snowd, um1%L_TILE_PTS, miss)
      SNOW_GRD=  SUM(um1%TILE_FRAC * SNOW_TILE,2)  ! gridbox snow mass & snow below canopy 

      !--- unpack layered snow vars 
      do k = 1,3
        SNOW_TMP3L(:,:,k) = UNPACK(ssnow%tggsn(:,k), um1%L_TILE_PTS, miss)
        SNOW_MASS3L(:,:,k)= UNPACK(ssnow%smass(:,k), um1%L_TILE_PTS, miss)
        SNOW_RHO3L(:,:,k) = UNPACK(ssnow%ssdn(:,k), um1%L_TILE_PTS, miss)
        SNOW_COND(:,:,k)  = UNPACK(ssnow%sconds(:,k),um1%L_TILE_PTS,miss)
        SNOW_DEPTH3L(:,:,k)  = UNPACK(ssnow%sdepth(:,k),um1%L_TILE_PTS,miss)
      enddo

      !---???
      GS_TILE = UNPACK(canopy%gswx_T,um1%L_TILE_PTS,miss)
      GS =  SUM(um1%TILE_FRAC * GS_TILE,2)

      !---preserve fluxes from the previous time step for the coastal grids
      FTL_TILE_old = FTL_TILE
      FQW_TILE_old = FQW_TILE
      !___return fluxes
      FTL_TILE = UNPACK(canopy%fh,  um1%l_tile_pts, miss)
      fe_dlh = canopy%fe/(air%rlam*ssnow%cls)
      where ( fe_dlh .ge. 0.0 ) fe_dlh = MAX ( 1.e-6, fe_dlh )
      where ( fe_dlh .lt. 0.0 ) fe_dlh = MIN ( -1.e-6, fe_dlh )
      fes_dlh = canopy%fes/(air%rlam*ssnow%cls)
      fev_dlh = canopy%fev/air%rlam

      !---update fluxes 
      FQW_TILE = UNPACK(fe_dlh, um1%l_tile_pts, miss)


      !___return temp and roughness
      TSTAR_TILE = UNPACK(rad%trad, um1%l_tile_pts, miss)

      !___return miscelaneous 
      RADNET_TILE = unpack( canopy%rnet , um1%l_tile_pts, miss)

     TOT_ALB=UNPACK(rad%albedo_T,um1%L_TILE_PTS, miss) 
     ESOIL_TILE = UNPACK(fes_dlh, um1%L_TILE_PTS, miss)
     ECAN_TILE = UNPACK(fev_dlh,  um1%L_TILE_PTS, miss)
     EI_TILE = 0.
     SNAGE_TILE = UNPACK(ssnow%snage, um1%L_TILE_PTS, miss) 

     !unpack screen level (1.5m) variables
     !Convert back to K 
     t1p5m_tile     = UNPACK(canopy%tscrn+tfrz, um1%L_TILE_PTS, miss)
     q1p5m_tile     = UNPACK(canopy%qscrn, um1%L_TILE_PTS, miss)
     CANOPY_TILE    = UNPACK(canopy%cansto, um1%L_TILE_PTS, miss)
     CANOPY_GB      = SUM(um1%TILE_FRAC * CANOPY_TILE,2)

     ! Lestevens - Passing CO2 from CABLE to bl_trmix_dd.F90
     FRS_TILE       = UNPACK(canopy%frs, um1%L_TILE_PTS, miss)
     NEE_TILE       = UNPACK(canopy%fnee, um1%L_TILE_PTS, miss)
     NPP_TILE       = UNPACK(canopy%fnpp, um1%L_TILE_PTS, miss)
     GLEAF_TILE     = UNPACK(canopy%frday,um1%L_TILE_PTS, miss)

      IF( cable_user%leaf_respiration == 'on' .OR.                             &
           cable_user%leaf_respiration == 'ON') THEN
         GPP_TILE = UNPACK(canopy%fnpp+canopy%frp, um1%L_TILE_PTS, miss)
      ELSE 
         GPP_TILE = UNPACK(canopy%fnpp+canopy%frp+canopy%frday,  &
                            um1%L_TILE_PTS, miss)
      ENDIF

     FRP_TILE       = UNPACK(canopy%frp, um1%L_TILE_PTS, miss)
     NPP_FT_old     = NPP_FT
     GPP_FT_old     = GPP_FT
     RESP_P_FT_old  = RESP_P_FT
     RESP_S_old     = RESP_S

     !initialse full land grids and retain coastal grid fluxes
      DO N=1,um1%NTILES
         DO K=1,um1%TILE_PTS(N)
           L = um1%TILE_INDEX(K,N)
           J=(um1%LAND_INDEX(L)-1)/um1%ROW_LENGTH + 1
           I = um1%LAND_INDEX(L) - (J-1)*um1%ROW_LENGTH
           IF( FLAND(L) == 1.0) THEN 
             FTL_1(I,J) =  0.0
             FQW_1(I,J) =  0.0
           ELSE
             !retain sea/ice contribution and remove land contribution
             FTL_1(I,J) = FTL_1(I,J) - FLAND(L) * um1%TILE_FRAC(L,N) *         &
                          FTL_TILE_old(L,N)
             FQW_1(I,J) = FQW_1(I,J) - FLAND(L) * um1%TILE_FRAC(L,N) *         &
                          FQW_TILE_old(L,N)
           ENDIF
           SURF_HT_FLUX_LAND(I,J) = 0.
         ENDDO
     ENDDO

      DO N=1,um1%NTILES
         DO K=1,um1%TILE_PTS(N)
           L = um1%TILE_INDEX(K,N)
           J=(um1%LAND_INDEX(L)-1)/um1%ROW_LENGTH + 1
           I = um1%LAND_INDEX(L) - (J-1)*um1%ROW_LENGTH
           FTL_1(I,J) = FTL_1(I,J)+FLAND(L)*um1%TILE_FRAC(L,N)*FTL_TILE(L,N)
           FQW_1(I,J) = FQW_1(I,J)+FLAND(L)*um1%TILE_FRAC(L,N)*FQW_TILE(L,N)
           SURF_HT_FLUX_LAND(I,J) = SURF_HT_FLUX_LAND(I,J) +                   &
                                    FLAND(L)*um1%TILE_FRAC(L,N) *              &
                                    SURF_HTF_T_CAB(L,N)
         ENDDO
      ENDDO

      DO N=1,um1%NTILES
         DO K=1,um1%TILE_PTS(N)
            L = um1%TILE_INDEX(K,N)
            IF( FLAND(L) == 1.0) THEN
               NPP(L)=0.; NPP_FT(L,N)=0.; GPP(L)=0.; GPP_FT(L,N)=0.
               RESP_P(L)=0.; RESP_P_FT(L,N)=0.; RESP_S(L,:)=0.; G_LEAF(L,N)=0.   
            ELSE
               ! For coastal points: currently no contribution
               NPP(L)=NPP(L)-FLAND(L)*um1%TILE_FRAC(L,N)*NPP_FT_old(L,N)
               GPP(L)=GPP(L)-FLAND(L)*um1%TILE_FRAC(L,N)*GPP_FT_old(L,N)
               RESP_P(L)=RESP_P(L)-FLAND(L)*um1%TILE_FRAC(L,N)*RESP_P_FT_old(L,N)
               !--- loop for soil respiration
               DO I=1,DIM_CS1
                  RESP_S(L,I)=RESP_S(L,I)-FLAND(L)*RESP_S_old(L,I)
               ENDDO
               RESP_S_TOT(L)=sum(RESP_S(L,:))
            ENDIF
         ENDDO
      ENDDO

     RESP_S_TILE=FRS_TILE*1.e-3

      DO N=1,um1%NTILES 
         DO K=1,um1%TILE_PTS(N)
            L = um1%TILE_INDEX(K,N)
            !add leaf respiration to output
            G_LEAF(L,N)=GLEAF_TILE(L,N)*1.e-3
            NPP_FT(L,N)=NPP_TILE(L,N)*1.e-3
            NPP(L)=NPP(L)+FLAND(L)*um1%TILE_FRAC(L,N)*NPP_FT(L,N)
            GPP_FT(L,N)=GPP_TILE(L,N)*1.e-3
            GPP(L)=GPP(L)+FLAND(L)*um1%TILE_FRAC(L,N)*GPP_FT(L,N)

            !loop for soil resp. - all UM levels = single CABLE output 
            DO I=1,DIM_CS1
               RESP_S(L,I) = RESP_S(L,I) + &
                             FLAND(L)*um1%TILE_FRAC(L,N)*FRS_TILE(L,N)*1.e-3
            ENDDO

            RESP_S_TOT(L)=sum(RESP_S(L,:))
            RESP_P_FT(L,N)=FRP_TILE(L,N)*1.e-3
            RESP_P(L)=RESP_P(L)+FLAND(L)*um1%TILE_FRAC(L,N)*RESP_P_FT(L,N)
         ENDDO
      ENDDO

      if(L_fudge) then
         call fudge_out( 1,1,1, tsoil_tile, 'tsoil_tile')
         call fudge_out( 1,1,1, smcl_tile, 'smcl_tile')
         call fudge_out( 1,1, tsoil, 'tsoil')
         call fudge_out( 1,1,1, sthf_tile, 'sthf_tile')
         call fudge_out( 1,1,1, sthu_tile, 'sthu_tile')
         call fudge_out( 1,1, sthf, 'sthf')
         call fudge_out( 1,1, sthu, 'sthu')
         call fudge_out( 1,1,1, tsoil_tile, 'tsoil_tile')
         call fudge_out( 1,1,1, smcl_tile, 'smcl_tile')
         call fudge_out( 1,1, tsoil, 'tsoil')
         call fudge_out( 1,1,1, sthf_tile, 'sthf_tile')
         call fudge_out( 1,1,1, sthu_tile, 'sthu_tile')
         call fudge_out( 1,1, sthf, 'sthf')
         call fudge_out( 1,1, sthu, 'sthu')
         call fudge_out( 1,1, snow_rho1l, 'snow_rho1l')
         call fudge_out( 1,1, ISNOW_FLG3L, 'ISNOW_FLG3L')
         call fudge_out( 1,1, MELT_TILE, 'MELT_TILE')
         call fudge_out( 1,1, snow_TILE, 'snow_TILE')
         call fudge_out( 1, snow_grd, 'snow_grd')
         call fudge_out( 1,1,1, snow_Tmp3L, 'snow_Tmp3L')
         call fudge_out( 1,1,1, snow_mass3L, 'snow_mass3L')
         call fudge_out( 1,1,1, snow_rho3L, 'snow_rho3L')
         call fudge_out( 1,1,1, snow_depth3L, 'snow_depth3L')
         call fudge_out( 1,1,1, snow_cond, 'snow_cond')
         call fudge_out( 1,1, GS_TILE, 'GS_TILE')
         call fudge_out( 1, GS, 'GS')
         call fudge_out( 1,1, FTL_TILE, 'FTL_TILE')
         call fudge_out( 1,1, Fqw_TILE, 'Fqw_TILE')
         call fudge_out( 1,1, tstar_TILE, 'tstar_TILE')
         call fudge_out( 1,1, radnet_TILE, 'radnet_TILE')
         call fudge_out( 1,1, TOT_ALB, 'TOT_ALB')
         call fudge_out( 1,1, ESOIL_TILE, 'ESOIL_TILE')
         call fudge_out( 1,1, ECAN_TILE, 'ECAN_TILE')
         call fudge_out( 1,1, snage_TILE, 'snage_TILE')
         call fudge_out( 1,1, t1p5m_TILE, 't1p5m_TILE')
         call fudge_out( 1,1, q1p5m_TILE, 'q1p5m_TILE')
         call fudge_out( 1,1, canopy_TILE, 'canopy_TILE')
         call fudge_out( 1, canopy_GB, 'canopy_GB')
         call fudge_out( 1,1,frs_TILE, 'frs_TILE')
         call fudge_out( 1,1, NEE_TILE, 'NEE_TILE')
         call fudge_out( 1,1, NPP_TILE, 'NPP_TILE')
         call fudge_out( 1,1, Gleaf_TILE, 'Gleaf_TILE')
         call fudge_out( 1,1, GPP_TILE, 'GPP_TILE')
         call fudge_out( 1,1,frp_TILE, 'frp_TILE')
         call fudge_out( 1,1,fTL_1, 'fTL_1')
         call fudge_out( 1,1,fqw_1, 'fqw_1')
         call fudge_out( 1,1,SURF_HT_FLUX_LAND, 'SURF_HT_FLUX_LAND')
         call fudge_out( 1,1,RESP_S_TILE, 'RESP_S_TILE')
         call fudge_out( 1,1,g_leaf, 'g_leaf')
         call fudge_out( 1,1, NPP_ft, 'NPP_ft')
         call fudge_out( 1,1, GPP_ft, 'GPP_ft')
         call fudge_out( 1,1,RESP_S, 'RESP_S')
         call fudge_out( 1,RESP_S_tot, 'RESP_S_tot')
         call fudge_out( 1,1,RESP_P_FT, 'RESP_p_FT')
         call fudge_out( 1,RESP_p, 'RESP_p')
      endif

END SUBROUTINE Implicit_unpack


