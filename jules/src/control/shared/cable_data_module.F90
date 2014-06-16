! this module is to live in the JULES src code to be compiled with the UM
! data/fields/vars in this module are required when calling CABLE from UM
! this module serves as a mechanism of transporting all those vars from the
! top_level in the UM to teh calling points. It also serves as a library
! defining what vars are required for CABLE and what it can give back.
! It is further intended that this module serve asa template for the future 
! data atructure to be adopted by CABLE

!jhan***************************************************************************
   !this needs to be cleaned up, types renamed appropriately etc.
   !impl_params (eg)are vars pointed to by cable%typed BUT lazily are passed just
   !before the call anyway. it has already been shown that the method works SO 
   !for the sake of getting the model working - put them here and clan up later 
   !by redistributing to higher level calls
!jhan***************************************************************************

! vn8.2 version is copied over the top of this CABLE-JULES(8.6-9.0)
module cable_data_mod 
   !use define_dimensions, only : nrb 
   implicit none
  
interface cond_print
   module procedure cond_print1D, cond_print2D
End interface cond_print
   
   ! public variables. ALL vars above "contains" are deliberately public
    
   ! public subroutines 
   ! vn8.6
   public :: cable_control

   ! remove hard wired values
   integer, parameter :: ms= 6, & ! soil levels must be same as UM
                         msn = 3   

   !jhan: in JULES standalone these are declared below.               
   !jhan: therefore only neccesary here for UM AND note these have different name here
   !jhan: convert to use JULES names 
   !! introduced tiled prognostics that have to go through UM's I/O 
   ! vn8.6 omments these: JULES => soiltemp_CABLE etc def. in JULES i/o
   real, dimension(:), pointer, save ::           &
      TSOIL_TILE,    & !
      SMCL_TILE,     & !
      STHF_TILE,     & !
      SNOW_DEPTH3L,  & !
      SNOW_MASS3L,   & !
      SNOW_TMP3L,    & !
      SNOW_RHO3L,    & !
      SNOW_RHO1L,    & !
      SNOW_AGE,      & !
      SNOW_FLG3L       !

   ! vn8.6 intro these: JULES => soiltemp_CABLE etc def. in JULES i/o
   ! introduced tiled prognostics that have to go through JULES's I/O 
   real, dimension(:,:,:), pointer, save ::           &
      SOIL_Temp_CABLE, & !
      SMCL_CABLE,     & !
      STHF_CABLE        !

   real, dimension(:,:,:), pointer, save ::           &
      SNOW_DEPTH_CABLE,  & !
      SNOW_MASS_CABLE,   & !
      SNOW_TEMP_CABLE,    & !
      SNOW_RHO_CABLE       !

   real, dimension(:,:), pointer, save ::           &
      SNOW_RHO1L_CABLE,    & !
      SNOW_AGE_CABLE         !

   real, dimension(:,:), pointer, save ::           &
      SNOW_FLG3L_CABLE       !

!------------------------------------------------------------------------------

   type model_params

      INTEGER, POINTER ::                                                      &
         endstep,            & !       
         row_length, rows,  & !
         sm_levels,          & !
         ! vn8.6 land_pts <= (was) land_points
         land_pts,          & !
         ntiles,          & !
         npft,          & !
         timestep_number!,    & !
         ! vn8.6 comments as not available
         !mype

      !integer, POINTER ::                                                      &
      REAL, POINTER ::                                                      &
         timestep_width
      
      ! JULES uses as array (was REAL in 8.2)
      REAL, DIMENSION(:), POINTER ::                                           &
         dzsoil
      
      REAL, DIMENSION(:,:), POINTER ::                                         &
         latitude, &
         longitude!, &

   end type model_params
  
!------------------------------------------------------------------------------
   ! these are prognostic initializations that are generally not calculated
   ! dynamically per timestep
   type prognostic_params
   
      REAL, DIMENSION(:,:), POINTER ::                                         &
         tile_frac!, &
   
   end type prognostic_params
   
!------------------------------------------------------------------------------

   ! cable prognostic_vars
   type cable_vars
      ! JULES uses as:
      !real, DIMENSION(:,:), POINTER :: &
      integer, DIMENSION(:), POINTER :: &
         snow_flg3l
      
      ! JULES uses as:
      !REAL, DIMENSION(:,:), POINTER :: &
      REAL, DIMENSION(:), POINTER :: &
         snow_rho1l,    & !
         snow_age

      ! JULES uses as:
      !REAL, DIMENSION(:,:,:), POINTER :: &
      REAL, DIMENSION(:), POINTER :: &
         tsoil_tile, &
         smcl_tile, &
         sthf_tile, &
         snow_depth3l, &
         snow_mass3l, &
         snow_tmp3l, &
         snow_rho3l

      REAL, DIMENSION(:,:,:), POINTER :: &
         sthu_tile !jhan: c nwe kill this

      ! JULES uses as:
      REAL, DIMENSION(:,:,:), POINTER :: &
      !REAL, DIMENSION(:), POINTER :: &
         snow_cond ! no init from file

   end type cable_vars
 
!------------------------------------------------------------------------------

   ! forcing vars 
   type forcing_vars
      
      real, dimension(:,:,:), pointer :: & 
      ShortWave!, & ! => surf_down_sw

      !LongWave, &
      !AirTemper, &
      !SurfPressure, &
      !Humidity, &
      !WindSpeed, &
      !Precip 
   end type forcing_vars

!------------------------------------------------------------------------------
   ! TYPEd vars pased onto cable after UM version being pointed to 
   type UM_params

      LOGICAL, POINTER ::                                                      &
         L_cable

      INTEGER, POINTER ::                                                      &
         dim_cs1, dim_cs2     !

      INTEGER, DIMENSION(:), POINTER ::                                        &
         land_index,       &
         tile_pts

      INTEGER, DIMENSION(:,:), POINTER ::                                      &
         tile_index
       ! JULES uses as 2D-array (was 1-D in 8.2)
!         bexp, & !
!         hcon, & !
!         satcon, & !
!         sathh, & !
!         smvcst, & !
!         smvcwt, & !
!         smvccl!, & !
!         ! JULES uses these as 1-D [below] (was REAL in 8.2)
!         !albsoil, &
!         !CANOPY_GB

!      REAL, DIMENSION(:,:), POINTER :: &
      REAL, DIMENSION(:), POINTER :: &
         bexp, & !
         hcon, & !
         satcon, & !
         sathh, & !
         smvcst, & !
         smvcwt, & !
         smvccl, & !
         albsoil, &
         CANOPY_GB, &
         GS
      
      REAL, POINTER ::                                              &
         co2_mmr

      REAL, DIMENSION(:,:), POINTER :: &
         sthu, &
         smcl, &
         sthf, &
         tot_alb

      ! JULES uses as 2D- (was 3-D)
      !REAL, DIMENSION(:,:,:), POINTER :: &
      !   land_alb
      
      REAL, DIMENSION(:,:), POINTER :: &
         snow_tile, &
         vshr_land, &
         sin_theta_latitude, &
         pstar, &
         canht_ft, & !
         lai_ft,   & !
         land_alb,   & !
         canopy

      !REAL, DIMENSION(:), POINTER :: &
      !  cos_zenith_angle

      REAL, DIMENSION(:,:), POINTER :: &
        lw_down, &
        cos_zenith_angle, &
        ls_rain, &
        ls_snow 

      ! JULES uses as 2D- (was 3-D)
      REAL, DIMENSION(:,:,:), POINTER :: &
         tl_1, &
         qw_1

       real, dimension(:,:), pointer :: & 
         SW_DOWN, &  
         Z1_TQ, &
         Z1_UV, &
         U_S, &
         conv_rain, & 
         conv_snow
        
      real, dimension(:,:), pointer :: & 
         FTL_TILE, &
         fqw_tile, &
         tstar_tile, &
         U_S_STD_TILE, &
         CD_TILE, &
         CH_TILE, &
         RADNET_TILE, &
         FRACA, &
         rESFS, &
         RESFT, &
         Z0H_TILE, &
         Z0M_TILE, &
         RECIP_L_MO_TILE, &
         EPOT_TILE  
   
      real, dimension(:), POINTER :: & 
         FLAND(:)

    real, dimension(:,:), pointer :: & 
      !(LAND_PTS,NTILES)                                       &
      NPP_FT, &
      GPP_FT,&
      RESP_S_TILE,     &
      RESP_P_FT, &
      G_LEAF
      
   real, dimension(:), pointer :: & 
      !land_pts
      NPP,&
      GPP,&
      RESP_P
      
   real, dimension(:), pointer :: & 
      !(DIM_CS2)                                           &
      RESP_S_TOT
     
   real, dimension(:,:), pointer :: & 
     !(LAND_PTS,DIM_CS1)                                      &
     RESP_S

      Real, dimension(:,:,:), pointer ::                       &
         alb_tile,      & !
         land_albedo

   end type UM_params
  
!------------------------------------------------------------------------------
   
   type impl_params 
   
   real, dimension(:,:), pointer :: & 
      !(ROW_LENGTH,ROWS),                                         &  
      dtl_1, &
      dqw_1, &
      FTL_1,&
      FQW_1,  &
      SURF_HT_FLUX_LAND
      
   real, dimension(:,:), pointer :: & 
      !(LAND_PTS,SM_LEVELS)                                       &
      T_SOIL
      
   real, dimension(:,:), pointer :: & 
      !(LAND_PTS,NTILES)                                       &
      ECAN_TILE,&
      ESOIL_TILE,&
      EI_TILE,&
      T1P5M_TILE, &
      Q1P5M_TILE, &
      MELT_TILE

   end type impl_params

!------------------------------------------------------------------------------

   type hyd_type
      real, dimension(:), pointer :: &                                                               &
         sub_surf_roff, &
         surf_roff, &
         tot_tfall, &
         LYING_SNOW
   end type hyd_type


!------------------------------------------------------------------------------

   type tmp_pvars
   
      LOGICAL, DIMENSION(:,:), POINTER ::                                      &
         L_tile_pts
   
      ! vn8.6 intros
      Real ::                                                                  &
         Epsilon,                                                              &
         c_virtual,                                                            &
         D_T,                                                                  &
         DS_RATIO,                                                             & 
         LH 
       
      real, POINTER :: & 
         rho_water

   end type tmp_pvars

!------------------------------------------------------------------------------

   type model 
      type (UM_params) :: um 
      type (tmp_pvars) :: tmp 
      type (model_params) :: mp
      type (prognostic_params) :: ppar
      type (cable_vars) :: cable 
      type (forcing_vars) :: forcing 
      type (impl_params) :: im 
      type (hyd_type) :: hyd

      !integer, allocatable :: gridcell(:) 
      !real, allocatable :: lat(:), lon(:)
   end type model 

!------------------------------------------------------------------------------
      
   !instantiate types
   !type (model_constants),save, target :: const
   type (model),save :: cable 
   
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   

contains


!===============================================================================

! vn8.6 abandons doingthisas well as mype BUT cable_diag needs! reinstate in UM8.5
subroutine set_endstep_umodel(fendstep)
   integer, target :: fendstep

      cable% mp% endstep            => fendstep

end subroutine set_endstep_umodel

!===============================================================================

SUBROUTINE cable_control( UM_eq_TRUE, L_cable, a_step, timestep_len, row_length,     &
               rows, land_pts, ntiles, sm_levels, dim_cs1, dim_cs2,              &
               latitude, longitude,                                              &
               land_index, bexp, hcon, satcon, sathh, smvcst, smvcwt, smvccl,       &
               albsoil, lw_down, cosz, ls_rain, ls_snow, pstar, CO2_MMR,         &
               sthu, smcl, sthf, GS, canopy_gb , land_albedo )

   LOGICAL, target ::                                                          &
      UM_eq_TRUE,    & !
      L_CABLE          !
   
   INTEGER, target ::                                              &
      a_step,  & !
      row_length, rows, &
      land_pts,      &
      ntiles,        &
      sm_levels,        &
      dim_cs1, dim_cs2

   INTEGER, DIMENSION(:), target::                                              &
      land_index
   
   ! vn8.6 fudged at present as NA in JULES
   !INTEGER, target :: endstep, mype

   ! integer, target : elsewhere
   ! UM vn 8.6 atm_step carries as REAL
   REAL, target::                                              &
      timestep_len

   ! passed from M 8.6 as cos_theta....
   REAL, DIMENSION(:,:), TARGET :: &
      latitude, longitude

   ! right dims in JULES standalone? 
   ! REAL, DIMENSION(:,:), TARGET :: &
   ! right dims in UM8.6 
   REAL, DIMENSION(:), TARGET :: &
      albsoil,    & ! passed from UM as soil_alb
      bexp,       & ! passed from UM as clapp_horn
      hcon,       & ! passed from UM as therm_cond
      satcon,     & ! passed from UM as SAT_SOIL_COND
      sathh,      & ! passed from UM as SAT_SOILW_SUCTION
      smvcst,     & ! passed from UM as VOL_SMC_criT
      smvcwt,     & ! passed from UM as VOL_SMC_WILT
      smvccl,     & ! passed from UM as  VOL_SMC_saT
      canopy_gb     ! passed from UM as canopy_water 

    REAL, DIMENSION(:,:), TARGET:: &
      lw_down,    &
      cosz,       & ! vn 8.2 had cos_zenith_angle here
      ls_rain,    &
      ls_snow,    &   
      pstar,      &
      sthu,       &
      smcl,       &
      sthf,       &
      land_albedo 
 
   REAL, target::                                              &
      co2_mmr
   
   REAL, DIMENSION(:), TARGET :: &
      GS 
   ! previously dims have been
   !REAL, DIMENSION(:,:,:), target ::                                         &
   !   land_albedo 
 
   !REAL, DIMENSION(:), TARGET :: &
   !  cosz,    &
   !  canopy_gb, &
   !  GS 
                            

!   REAL, DIMENSION(:,:), TARGET, ALLOCATABLE :: &
!      flatitude, flongitude

   !---------------------------------------------------------------------------
   !local vars
   
   LOGICAL :: first_atmstep_call
   
   integer :: i,j, k,n
   
   !---------------------------------------------------------------------------

   !call print_control_args()

   !vn8.6 intros
   if( a_step == 1) first_atmstep_call = .TRUE. 
   ! vn8.6 some differences herewhat is allocated etc
   if( first_atmstep_call ) then 
      !if( UM_eq_TRUE) then
      !   allocate( flatitude(row_length,rows) )
      !   allocate( flongitude(row_length,rows) )
      !endif
      allocate( cable% um% sin_theta_latitude(row_length,rows) )
      allocate( cable% cable% SNOW_COND(land_pts,NTILES,3))
      allocate( cable% cable% SNOW_flg3L(land_pts*NTILES))
      allocate( cable% cable% STHU_TILE(land_pts,NTILES,sm_levels))
      allocate( cable% tmp% L_TILE_PTS(land_pts,NTILES))
      !this can be deleted once rm from cable_explicit_driver (call/recieve )
      allocate( cable% um% SW_DOWN(row_length,rows)           )
      allocate( cable% forcing% ShortWave(row_length,rows,4)    )
      allocate( cable% um% TILE_INDEX(land_pts,NTiles) ) 
      allocate( cable% um% TILE_PTS(NTiles) )  
      allocate( cable% um% TOT_ALB(land_pts,ntiles)        )
   endif
   !vn 8.6 we didnt use this typecasting through calls
   !call cable_parse_isnow( land_pts, ntiles, snow_flg3l,                       &
   !        tsoil_tile, smcl_tile, sthf_tile,                                   &
   !        snow_depth3l, snow_mass3l, snow_tmp3l,                              & 
   !        snow_rho3l, snow_rho1l, snow_age ) 
  
      cable% um% L_cable            => L_cable
      ! vn8.6 some revisions and renaming
      !cable% mp% mype               => mype
      !cable% mp% endstep            => endstep     
      cable% mp% timestep_number    => a_step
      cable% mp% timestep_width     => timestep_len
      cable% mp% row_length         => row_length 
      cable% mp% rows               => rows        
      cable% mp% land_pts           => land_pts
      cable% mp% ntiles             => ntiles
      cable% mp% sm_levels          => sm_levels
      
      cable% um% dim_cs1           => dim_cs1
      cable% um% dim_cs2           => dim_cs2
      ! JULES uses theseCABLE prognostics
      !cable% cable% tsoil_tile       => soil_temp_CABLE
      !cable% cable% smcl_tile        => smcl_CABLE
      !cable% cable% sthf_tile        => sthf_CABLE
      !cable% cable% snow_depth3l     => snow_depth_CABLE
      !cable% cable% snow_mass3l      => snow_mass_CABLE
      !cable% cable% snow_tmp3l       => snow_temp_CABLE
      !cable% cable% snow_rho3l       => snow_rho_CABLE
      !cable% cable% snow_flg3l       => snow_flg3l_CABLE
      !cable% cable% snow_rho1l       => snow_rho1l_CABLE
      !cable% cable% snow_age         => snow_age_CABLE

      cable% cable% tsoil_tile       =>  tsoil_tile   
      cable% cable% smcl_tile        =>  smcl_tile    
      cable% cable% sthf_tile        =>  sthf_tile    
      cable% cable% snow_depth3l     =>  snow_depth3l 
      cable% cable% snow_mass3l      =>  snow_mass3l  
      cable% cable% snow_tmp3l       =>  snow_tmp3l   
      cable% cable% snow_rho3l       =>  snow_rho3l   
      cable% cable% snow_rho1l       =>  snow_rho1l   
      cable% cable% snow_age         =>  snow_age     
      cable% cable% snow_flg3l       =   int(snow_flg3l)   

      !jhan: re-implement sin_theta_lat by computing here      
      !if( UM_eq_TRUE) then
      !   flatitude  = asin( latitude )
      !   flongitude = acos( longitude )
      !endif
      cable% mp% latitude           => latitude
      cable% mp% longitude          => longitude


      cable% um% sin_theta_latitude = sin( cable% mp% latitude )

      cable% um% land_index         => land_index 

      cable% um% sthu             => sthu        
      
      cable% um% sthf             => sthf        
      cable% um% smcl             => smcl        
      
      cable% um% land_alb         => land_albedo   
          

      !vn8.6 uses some different names morereflective of names in UM
      cable% um% bexp     => bexp 
      cable% um% hcon     => hcon
      cable% um% satcon   => satcon
      cable% um% sathh    => sathh
      cable% um% smvcst   => smvcst  
      cable% um% smvcwt   => smvcwt  
      cable% um% smvccl   => smvccl  
      cable% um% albsoil  => albsoil 
      
      cable% um% pstar => pstar 

      cable% um% lw_down   => lw_down
      cable% um% cos_zenith_angle   => cosz
      cable% um% ls_rain     => ls_rain 
      cable% um% ls_snow      => ls_snow
      cable% um% co2_mmr      => co2_mmr

      cable% um% gs => gs
      cable% um% canopy_gb => canopy_gb

      cable% cable% snow_cond = -huge(1.)
      cable% cable% sthu_tile = -huge(1.)
      cable% tmp% l_tile_pts = .false.
      !vn8.6 intros
      cable% tmp% Epsilon = 0.62198 
      cable% tmp% c_virtual =  1. / cable% tmp% Epsilon - 1. 

contains
   
   subroutine print_control_args()
   print *, 'UM_eq_TRUE ',UM_eq_TRUE 
   print *, 'L_cable ', L_cable
   print *, 'a_step ', a_step
   print *, 'timestep_len ', timestep_len 
   print *, 'row_length ', row_length
   print *, 'rows  ', rows 
   print *, 'land_pts  ', land_pts 
   print *, 'ntiles ',ntiles 
   print *, 'sm_levels ',sm_levels 
   print *, 'dim_cs1 ',  dim_cs1 
   print *, 'dim_cs2 ', dim_cs2
   print *, 'latitude ', latitude(1:2,1:2)
   print *, 'longitude ',longitude(1:2,1:2)
   print *, 'land_index ',land_index(1:2)
   print *, 'bexp ',bexp(1:2)
   print *, 'hcon ',hcon(1:2)
   print *, 'satcon ', satcon(1:2)
   print *, 'sathh ',sathh(1:2)
   print *, 'smvcst ',smvcst(1:2)
   print *, 'smvcwt ',smvcwt(1:2)
   print *, 'smvccl ',smvccl(1:2)
   print *, 'albsoil ',albsoil(1:2)
   print *, 'lw_down ',lw_down(1:2,1:2)
   print *,"cosz ", cosz(1:2,1:2)
   print *,"ls_rain ", ls_rain(1:2,1:2)
   print *,"ls_snow ", ls_snow(1:2,1:2)
   print *,"pstar ", pstar(1:2,1:2)
   print *,"CO2_MMR ", CO2_MMR
   print *,"sthu ", sthu(1:2,1:2)
   print *,"smcl ", smcl(1:2,1:2)
   print *,"sthf ", sthf(1:2,1:2)
   print *,"GS ", GS(1:2)
   print *,"canopy_gb ", canopy_gb(1:2)
   print *,"land_albedo ", land_albedo (1:2,1:2)
End subroutine print_control_args


END SUBROUTINE cable_control
 
!===============================================================================
!vn8.2 WAS
!SUBROUTINE cable_atmos_physics2(npft,tile_frac,snow_tile,vshr_land,   &
!                  canopy,canht_ft, lai_ft, conv_rain,conv_snow,NPP,&
!                  NPP_FT, GPP,GPP_FT, RESP_S, RESP_S_TOT,&
!                  RESP_S_TILE, RESP_P, RESP_P_FT,  G_LEAF, &
!                  Radnet_TILE, Lying_snow, surf_roff, &
!                  sub_surf_roff, tot_tfall )  
SUBROUTINE cable_control2( npft, tile_frac, snow_tile, vshr_land, canopy,      &
              canht_ft, lai_ft, conv_rain, conv_snow, NPP,NPP_FT,              &
              GPP, GPP_FT, RESP_S, rESP_S_TOT, RESP_S_TILE, RESP_P,            &
              RESP_P_FT, G_LEAF, Radnet_TILE, Lying_snow, surf_roff,           &
              sub_surf_roff, tot_tfall )

   INTEGER, target ::                                              &
      npft 

   REAL, DIMENSION(:,:), TARGET:: &
      tile_frac, &
      snow_tile, &
      vshr_land
 
   REAL, DIMENSION(:,:), TARGET ::                                         &
     canopy, &
     canht_ft, &
     lai_ft 
 
   REAL, DIMENSION(:,:), TARGET ::                                         &
     !(row_length, rows)                                     &
     conv_rain, &
     conv_snow
   
   real, dimension(:), target :: & 
      GPP, & ! Gross primary productivity (kg C/m2/s).
      NPP, & ! Net primary productivity
      RESP_P ! Plant respiration (kg C/m2/s).
   
   real, dimension(:,:), target :: & 
      GPP_FT, & !     on PFTs (kg C/m2/s).
      NPP_FT, & ! Net primary productivity (kg C/m2/s).
      G_LEAF, & ! Leaf turnover rate (/360days).
      RESP_P_FT, & !  Plant respiration on PFTs (kg C/m2/s).
      RESP_S_TILE  ! Soil respiration on tiles (kg C/m2/s).
              
   real, dimension(:,:), target :: & 
      RESP_S ! Soil respiration (kg C/m2/s).
   
   real, dimension(:), target :: & 
      RESP_S_TOT ! OUT total RESP_S over pools

   real, dimension(:,:), target :: & 
      RADNET_TILE
 
   real, dimension(:), target :: &                                                               &
      sub_surf_roff, &
      surf_roff, &
      tot_tfall, &
      LYING_SNOW
   
      !CALL print_control_args()

      cable% mp% npft            => npft
      cable% ppar% tile_frac     => tile_frac
      cable% um% snow_tile       => snow_tile
      cable% um% canopy => canopy
      cable% um% canht_ft  => canht_ft
      cable% um% lai_ft  => lai_ft
      cable% um% vshr_land       => vshr_land 
      cable% um% conv_rain       => conv_rain
      cable% um% conv_snow       => conv_snow
      cable% um% NPP => NPP
      cable% um% NPP_FT => NPP_FT
      cable% um% GPP => GPP
      cable% um% GPP_FT => GPP_FT
      cable% um% RESP_S => RESP_S
      cable% um% RESP_S_TOT => RESP_S_TOT
      cable% um% RESP_S_TILE => RESP_S_TILE
      cable% um% RESP_P => RESP_P
      cable% um% RESP_P_FT => RESP_P_FT
      cable% um% G_LEAF => G_LEAF
      cable% um% RADNET_TILE => RADNET_TILE
      cable% hyd% sub_surf_roff  => sub_surf_roff
      cable% hyd% surf_roff      => surf_roff
      cable% hyd% tot_tfall      => tot_tfall
      cable% hyd% LYING_SNOW     => LYING_SNOW


contains 

subroutine print_control_args()

   !print *,''
   !print *,'jhan:shape:npft ',         shape(npft)
   !print *,'jhan:shape:tile_frac ',    shape(tile_frac) 
   !print *,'jhan:shape:snow_tile ',    shape(snow_tile)
   !print *,'jhan:shape:vshr_land ',    shape(vshr_land)
   !print *,'jhan:shape:canopy ',       shape(canopy)
   !print *,'jhan:shape:canht_ft ',     shape(canht_ft)
   !print *,'jhan:shape:lai_ft ',       shape(lai_ft)
   !print *,'jhan:shape:conv_rain ',    shape(conv_rain)
   !print *,'jhan:shape:conv_snow ',    shape(conv_snow)
   !print *,'jhan:shape:NPP ',          shape(NPP)
   !print *,'jhan:shape:NPP_FT ',       shape(NPP_FT)
   !print *,'jhan:shape:GPP ',          shape(GPP)
   !print *,'jhan:shape:GPP_FT ',       shape(GPP_FT)
   !print *,'jhan:shape:RESP_S ',       shape(RESP_S)
   !print *,'jhan:shape:rESP_S_TOT ',   shape(rESP_S_TOT)
   !print *,'jhan:shape:RESP_S_TILE ',  shape(RESP_S_TILE)
   !print *,'jhan:shape:RESP_P ',       shape(RESP_P)
   !print *,'jhan:shape:RESP_P_FT ',    shape(RESP_P_FT)
   !print *,'jhan:shape:G_LEAF ',       shape(G_LEAF)
   !print *,'jhan:shape:Radnet_TILE ',  shape(Radnet_TILE)
   !print *,'jhan:shape:Lying_snow ',   shape(Lying_snow)
   !print *,'jhan:shape:surf_roff ',    shape(surf_roff)
   !print *,'jhan:shape:sub_surf_roff ',shape(sub_surf_roff)
   !print *,'jhan:shape:tot_tfall ',    shape(tot_tfall)
   
   !print *,'jhan:npft ',         npft
   !print *,'jhan:tile_frac ',    tile_frac(1,1)
   !print *,'jhan:snow_tile ',    snow_tile(1,1)
   !print *,'jhan:vshr_land ',    vshr_land(1,1)
   !print *,'jhan:canopy ',       canopy(1,1)
   !print *,'jhan:canht_ft ',     canht_ft(1,1)
   !print *,'jhan:lai_ft ',       lai_ft(1,1)
   !print *,'jhan:conv_rain ',    conv_rain(1,1)
   !print *,'jhan:conv_snow ',    conv_snow(1,1)
   !print *,'jhan:NPP ',          NPP(1)
   !print *,'jhan:NPP_FT ',       NPP_FT(1,1)
   !print *,'jhan:GPP ',          GPP(1)
   !print *,'jhan:GPP_FT ',       GPP_FT(1,1)
   !print *,'jhan:RESP_S ',       RESP_S(1,1)
   !print *,'jhan:rESP_S_TOT ',   rESP_S_TOT(1)
   !print *,'jhan:RESP_S_TILE ',  RESP_S_TILE(1,1)
   !print *,'jhan:RESP_P ',       RESP_P(1)
   !print *,'jhan:RESP_P_FT ',    RESP_P_FT(1,1)
   !print *,'jhan:G_LEAF ',       G_LEAF(1,1)
   !print *,'jhan:Radnet_TILE ',  Radnet_TILE(1,1)
   !print *,'jhan:Lying_snow ',   Lying_snow(1)
   !print *,'jhan:surf_roff ',    surf_roff(1)
   !print *,'jhan:sub_surf_roff ',sub_surf_roff(1)
   !print *,'jhan:tot_tfall ',    tot_tfall(1)
  
   !print *,''
   !print *,'jhan:control2_var:tile_frac '
   !CALL cond_print(     tile_frac   )
   !print *,'jhan:control2_var:snow_tile '
   !CALL cond_print(     snow_tile   )
   !print *,'jhan:control2_var:vshr_land '
   !CALL cond_print(     vshr_land   )
   !print *,'jhan:control2_var:canopy '
   !CALL cond_print(     canopy      )
   !print *,'jhan:control2_var:canht_ft '
   !CALL cond_print(     canht_ft    )
   !print *,'jhan:control2_var:lai_ft '
   !CALL cond_print(     lai_ft      )
   !print *,'jhan:control2_var:conv_rain '
   !CALL cond_print(     conv_rain   )
   !print *,'jhan:control2_var:conv_snow '
   !CALL cond_print(     conv_snow   )
   !CALL cond_print(     NPP         )
   !CALL cond_print(     NPP_FT      )
   !CALL cond_print(     GPP         )
   !CALL cond_print(     GPP_FT      )
   !CALL cond_print(     RESP_S      )  
   !CALL cond_print(     rESP_S_TOT  )
   !CALL cond_print(     RESP_S_TILE )
   !CALL cond_print(     RESP_P      )
   !CALL cond_print(     RESP_P_FT   )
   !CALL cond_print(     G_LEAF      )
   !CALL cond_print(     Radnet_TILE )
   !CALL cond_print(     Lying_snow  )
   !CALL cond_print(     surf_roff   )
   !CALL cond_print(     tot_tfall   )
   !CALL cond_print(     sub_surf_roff)
   
End subroutine print_control_args

  

END SUBROUTINE cable_control2

   subroutine cond_print1D( var )
      real, dimension(:) :: var
      integer :: len1, i
         
      len1 = SIZE( var,1 )
      do i=1,len1
         if( var(i) < -1.e9 .OR. var(i) > 1.e9) then
            print *, "jhan:cond1d: i val", i, var(i)
         endif                
      enddo        

   End subroutine cond_print1D
   
   subroutine cond_print2D( var )
      real, dimension(:,:) :: var
      integer :: len1, len2, i, j
         
      len1 = SIZE( var,1 )
      len2 = SIZE( var,2 )
      do i=1,len1
      do j=1,len2
         if( var(i,j) < -1.e9 .OR. var(i,j) > 1.e9) then
            print *, "jhan:cond2d: i val", i, j, var(i,j)
         endif                
      enddo        
      enddo        

   End subroutine cond_print2D
 

!===============================================================================


!vn8.2 was SUBROUTINE cable_bdy_layr( TL, qw )  
SUBROUTINE cable_control3( TL, qw )  
   !JULES3.4.1 
   !REAL, DIMENSION(:,:), TARGET:: &
   REAL, DIMENSION(:,:,:), TARGET:: &
      tl, &
      qw

   cable% um% tl_1 => tl 
   cable% um% qw_1 => qw 

END SUBROUTINE cable_control3


!===============================================================================

!JULES standalone we fudge this SW
!SUBROUTINE cable_control5( alb_tile, land_albedo,         &
!                  TILE_PTS, TILE_INDEX )        
SUBROUTINE cable_control5( alb_tile, land_albedo,         &
                  TILE_PTS, TILE_INDEX, surf_down_sw )

   INTEGER, DIMENSION(:) ::                                        &
      tile_pts

   INTEGER, DIMENSION(:,:) ::                                      &
      tile_index

   Real, dimension(:,:,:), target ::                       &
      alb_tile

   Real, dimension(:,:,:) :: surf_down_sw

   Real, dimension(cable% mp% rows, cable% mp% row_length,4), target ::                 &
      land_albedo


   cable% um% alb_tile => alb_tile
   cable% um% land_albedo => land_albedo
   cable% um% TILE_PTS = TILE_PTS
   cable% um% TILE_INDEX = TILE_INDEX
   !cable% forcing% ShortWave    = surf_down_sw

END SUBROUTINE cable_control5 


!===============================================================================

! vn8.2 had cable% forcing% ShortWave    = surf_down_sw
!jhan: this is a very temp HACK - for offline SW is split in cable radiation
!module by spitter. online it recieves th SW calculated by the UM rad scheme 
!SUBROUTINE cable_control4( sw_down )
!   
!   Real, dimension(:,:) :: sw_down
!   !Real, dimension( cable% mp% row_length, cable% mp% rows, 4) :: surf_down_sw
!
!     !jhan: offline receives total SW and splits (CABLE uses subr spitter)
!     cable% forcing% ShortWave(:,:,1)    = sw_down(:,:) 
!     cable% forcing% ShortWave(:,:,2)    = 0. 
!     cable% forcing% ShortWave(:,:,3)    = 0. 
!     cable% forcing% ShortWave(:,:,4)    = 0. 
!     !cable% forcing% ShortWave(:,:,1)    = sw_down(:,:) / 4.
!     !cable% forcing% ShortWave(:,:,2)    = sw_down(:,:) / 4.
!     !cable% forcing% ShortWave(:,:,3)    = sw_down(:,:) / 4.
!     !cable% forcing% ShortWave(:,:,4)    = sw_down(:,:) / 4.
!     
!
!END SUBROUTINE cable_control4

!vn 8.6 standalone used above hack as surf_down_sw N/A
SUBROUTINE cable_glue_rad_init( surf_down_sw )
   Real, dimension(:,:,:), target :: surf_down_sw
     cable% forcing% ShortWave => surf_down_sw 
END SUBROUTINE cable_glue_rad_init


!===============================================================================
! vn8.2 WAS SUBROUTINE cable_sf_exch( + rho_water in args as well
SUBROUTINE cable_control6( z1_tq, z1_uv, Fland, dzsoil, FTL_TILE, &
             FQW_TILE, TSTAR_TILE, U_S, U_S_STD_TILE, CD_TILE, CH_TILE, FRACA, &
             rESFS, RESFT, Z0H_TILE, Z0M_TILE, RECIP_L_MO_TILE, EPOT_TILE )

   real, dimension(:,:), target :: & 
      Z1_TQ, &
      Z1_UV, &
      U_S 
     
   real, dimension(:,:), target :: & 
      !FTL_TILE(land_pts,NTILES)                                        &
      FTL_TILE, &
      fqw_tile, &
      tstar_tile, &
      U_S_STD_TILE, &
      CD_TILE, &
      CH_TILE, &
      FRACA, &
      rESFS, &
      RESFT, &
      Z0H_TILE, &
      Z0M_TILE, &
      RECIP_L_MO_TILE, &
      EPOT_TILE  

   real, target :: & 
      rho_water

   real, dimension(:), target :: & 
      !FLAND(land_pts)
      FLAND

   real, dimension(ms), target :: dzsoil
 
      cable% um% Z1_TQ => Z1_TQ
      cable% um% Z1_UV => Z1_UV
      cable% um% U_S => U_S 
      
     
      cable% um% FTL_TILE => FTL_TILE
      cable% um% fqw_tile => fqw_tile
      cable% um% tstar_tile => tstar_tile
      cable% um% U_S_STD_TILE => U_S_STD_TILE
      cable% um% CD_TILE => CD_TILE
      cable% um% CH_TILE => CH_TILE
      cable% um% FRACA => FRACA
      cable% um% rESFS => rESFS
      cable% um% RESFT => RESFT
      cable% um% Z0H_TILE => Z0H_TILE
      cable% um% Z0M_TILE => Z0M_TILE
      cable% um% RECIP_L_MO_TILE => RECIP_L_MO_TILE
      cable% um% EPOT_TILE => EPOT_TILE  
      
      cable% tmp% rho_water => rho_water
      
      cable% um% FLAND => FLAND
     
      cable% mp% dzsoil => dzsoil
       

END SUBROUTINE cable_control6


!===============================================================================


!vn 8.2 was SUBROUTINE cable_sf_implicit(                      &
SUBROUTINE cable_control7(                      &
                     dtl_1, &
                     dqw_1, &
                     T_SOIL, &
                      FTL_1,&
                      FQW_1,  &
                     SURF_HT_FLUX_LAND, &
                     ECAN_TILE,&
                     ESOIL_TILE,&
                     EI_TILE,&
                     T1P5M_TILE, &
                     Q1P5M_TILE, &
                     MELT_TILE &
                  )
   
   real, dimension(:,:), target :: & 
      !(ROW_LENGTH,ROWS),                                         &  
      dtl_1, &
      dqw_1, &
      FTL_1,&
      FQW_1,  &
      SURF_HT_FLUX_LAND
      
   real, dimension(:,:), target :: & 
      !(LAND_PTS,SM_LEVELS)                                       &
      T_SOIL
      
   real, dimension(:,:), target :: & 
      !(LAND_PTS,NTILES)                                       &
      ECAN_TILE,&
      ESOIL_TILE,&
      EI_TILE,&
      T1P5M_TILE, &
      Q1P5M_TILE, &
      MELT_TILE
      
      cable% im% dtl_1 => dtl_1
      cable% im% dqw_1 => dqw_1
      cable% im% T_SOIL => T_SOIL
      cable% im% FTL_1 => FTL_1
      cable% im% FQW_1 => FQW_1
      cable% im% SURF_HT_FLUX_LAND => SURF_HT_FLUX_LAND
      cable% im% ECAN_TILE => ECAN_TILE
      cable% im% ESOIL_TILE => ESOIL_TILE
      cable% im% EI_TILE => EI_TILE
      cable% im% T1P5M_TILE => T1P5M_TILE
      cable% im% Q1P5M_TILE => Q1P5M_TILE
      cable% im% MELT_TILE => MELT_TILE
 

END SUBROUTINE cable_control7

!===============================================================================
! vn8.6 abandons this in JULES stadnalone application
!   subroutine cable_point_isnow(isnow_flg3l, &
!                          ftsoil_tile, fsmcl_tile, fsthf_tile,     & !
!                          fsnow_depth3l, fsnow_mass3l, fsnow_tmp3l,    & !
!                          fsnow_rho3l, fsnow_rho1l, fsnow_age )
!    
!      integer, DIMENSION(:,:), target :: isnow_flg3L
!      
!      REAL, DIMENSION(:,:), target :: &
!         fsnow_rho1l,    & !
!         fsnow_age
!
!      REAL, DIMENSION(:,:,:), target :: &
!         ftsoil_tile, &
!         fsmcl_tile, &
!         fsthf_tile, &
!         fsnow_depth3l, &
!         fsnow_mass3l, &
!         fsnow_tmp3l, &
!         fsnow_rho3l
!
!      cable% cable% snow_flg3l      => isnow_flg3l
!
!      cable% cable% tsoil_tile       => ftsoil_tile
!      cable% cable% smcl_tile        => fsmcl_tile
!      cable% cable% sthf_tile        => fsthf_tile
!      cable% cable% snow_depth3l     => fsnow_depth3l
!      cable% cable% snow_mass3l      => fsnow_mass3l
!      cable% cable% snow_tmp3l       => fsnow_tmp3l
!      cable% cable% snow_rho3l       => fsnow_rho3l
!      cable% cable% snow_rho1l       => fsnow_rho1l
!      cable% cable% snow_age         => fsnow_age
!
!   end subroutine cable_point_isnow
!
!!===============================================================================
! vn8.6 abandons this in JULES stadnalone application reinstate in UM8.5
! curiously does not work with locally declared j pointers
subroutine cable_set_atm_pointers( SI, NITEMS,NSECTS,N_INTERNAL_MODEL, &
                                   Sect_No,im_index, & 
                                   jTSOIL_TILE, jSMCL_TILE, jSTHF_TILE,            &
                                   jSNOW_DEPTH3L, jSNOW_MASS3L, jSNOW_TMP3L,       &
                                   jSNOW_RHO3L, jSNOW_RHO1L, jSNOW_AGE, jSNOW_FLG3L )

   implicit none 
   ! Address of item from generating plug compatible routine (often
   ! workspace) !UM: include/declaration/typsts.h
   INTEGER :: NITEMS,NSECTS,N_INTERNAL_MODEL
   INTEGER :: SI(  NITEMS,0:NSECTS,N_INTERNAL_MODEL)
   integer :: Sect_No, im_index

   INTEGER :: JTSOIL_TILE(ms)  ! Tiled soil temperature
   INTEGER :: JSMCL_TILE(ms)   ! Tiled soil moisture content in layers
   INTEGER :: JSTHF_TILE(ms)   ! Tiled frozen soil moisture fraction
   INTEGER :: JSNOW_DEPTH3L(msn)        ! Tiled snow depth
   INTEGER :: JSNOW_MASS3L(msn)         ! Tiled snow mass
   INTEGER :: JSNOW_TMP3L(msn)          ! Tiled snow temperature
   INTEGER :: JSNOW_RHO3L(msn)          ! Tiled snow density
   INTEGER :: JSNOW_RHO1L             ! Tiled mean snow density
   INTEGER :: JSNOW_AGE               ! Tiled snow age
   INTEGER :: JSNOW_FLG3L             ! Flag for use of 3 level snow scheme

   !--- allow for 6 layers
   JTSOIL_TILE(1) = SI(301,Sect_No,im_index)
   JTSOIL_TILE(2) = SI(302,Sect_No,im_index)
   JTSOIL_TILE(3) = SI(303,Sect_No,im_index)
   JTSOIL_TILE(4) = SI(304,Sect_No,im_index)
   JTSOIL_TILE(5) = SI(305,Sect_No,im_index)
   JTSOIL_TILE(6) = SI(306,Sect_No,im_index)

   JSMCL_TILE(1) = SI(307,Sect_No,im_index)
   JSMCL_TILE(2) = SI(308,Sect_No,im_index)
   JSMCL_TILE(3) = SI(309,Sect_No,im_index)
   JSMCL_TILE(4) = SI(310,Sect_No,im_index)
   JSMCL_TILE(5) = SI(311,Sect_No,im_index)
   JSMCL_TILE(6) = SI(312,Sect_No,im_index)
   
   JSTHF_TILE(1) = SI(313,Sect_No,im_index)
   JSTHF_TILE(2) = SI(314,Sect_No,im_index)
   JSTHF_TILE(3) = SI(315,Sect_No,im_index)
   JSTHF_TILE(4) = SI(316,Sect_No,im_index)
   JSTHF_TILE(5) = SI(317,Sect_No,im_index)
   JSTHF_TILE(6) = SI(318,Sect_No,im_index)

   JSNOW_TMP3L(1) = SI(323,Sect_No,im_index)
   JSNOW_TMP3L(2) = SI(324,Sect_No,im_index)
   JSNOW_TMP3L(3) = SI(325,Sect_No,im_index)
   JSNOW_RHO3L(1) = SI(326,Sect_No,im_index)
   JSNOW_RHO3L(2) = SI(327,Sect_No,im_index)
   JSNOW_RHO3L(3) = SI(328,Sect_No,im_index)
   JSNOW_RHO1L = SI(329,Sect_No,im_index)
   JSNOW_AGE = SI(330,Sect_No,im_index)
   JSNOW_FLG3l = SI(331,Sect_No,im_index)

   JSNOW_DEPTH3L(1) = SI(332,Sect_No,im_index)
   JSNOW_DEPTH3L(2) = SI(333,Sect_No,im_index)
   JSNOW_DEPTH3L(3) = SI(334,Sect_No,im_index)
   JSNOW_MASS3L(1) = SI(335,Sect_No,im_index)
   JSNOW_MASS3L(2) = SI(336,Sect_No,im_index)
   JSNOW_MASS3L(3) = SI(337,Sect_No,im_index)

end subroutine cable_set_atm_pointers

!===============================================================================



! vn8.6 abandons this in JULES  standalone reinstate in UM8.5
subroutine cable_set_atm_fields( D1, LEN_TOT, land_pts,no_halo,sm_levels,ntiles, &
                                 jTSOIL_TILE, jSMCL_TILE, jSTHF_TILE,            &
                                 jSNOW_DEPTH3L, jSNOW_MASS3L, jSNOW_TMP3L,       &
                                 jSNOW_RHO3L, jSNOW_RHO1L, jSNOW_AGE, jSNOW_FLG3L )

   use field_length_mod , only : field_length
   implicit none 

   REAL,    TARGET, INTENT(IN) :: D1(LEN_TOT)
   integer, intent(in) :: LEN_TOT, land_pts,no_halo,sm_levels,ntiles
      INTEGER :: jTSOIL_TILE(ms)  ! Tiled soil temperature
      INTEGER :: jSMCL_TILE(ms)   ! Tiled soil moisture content in layers
      INTEGER :: jSTHF_TILE(ms)   ! Tiled frozen soil moisture fraction
      INTEGER :: jSNOW_DEPTH3L(msn)        ! Tiled snow depth
      INTEGER :: jSNOW_MASS3L(msn)         ! Tiled snow mass
      INTEGER :: jSNOW_TMP3L(msn)          ! Tiled snow temperature
      INTEGER :: jSNOW_RHO3L(msn)          ! Tiled snow density
      INTEGER :: jSNOW_RHO1L             ! Tiled mean snow density
      INTEGER :: jSNOW_AGE               ! Tiled snow age
      INTEGER :: jSNOW_FLG3L             ! Flag for use of 3 level snow scheme

     TSOIL_TILE  => D1(jTSOIL_TILE(1):jTSOIL_TILE(1)+  &
                         field_length(land_pts,no_halo,sm_levels*ntiles))
     SMCL_TILE   => D1(jSMCL_TILE(1):jSMCL_TILE(1)+  &
                          field_length(land_pts,no_halo,sm_levels*ntiles))
     STHF_TILE   => D1(jSTHF_TILE(1):jSTHF_TILE(1)+  &
                   field_length(land_pts,no_halo,sm_levels*ntiles))
      ! MRD - should be a parameter for number of snow levels here rather than 3
     SNOW_DEPTH3L=> D1(jSNOW_DEPTH3L(1):jSNOW_DEPTH3L(1)+  &
                          field_length(land_pts,no_halo,3*ntiles))
     SNOW_MASS3L => D1(jSNOW_MASS3L(1):jSNOW_MASS3L(1)+  &
                          field_length(land_pts,no_halo,3*ntiles))
     SNOW_TMP3L  => D1(jSNOW_TMP3L(1):jSNOW_TMP3L(1)+  &
                          field_length(land_pts,no_halo,3*ntiles))
     SNOW_RHO3L  => D1(jSNOW_RHO3L(1):jSNOW_RHO3L(1)+  &
                          field_length(land_pts,no_halo,3*ntiles))
     SNOW_RHO1L  => D1(jSNOW_RHO1L:jSNOW_RHO1L+  &
                          field_length(land_pts,no_halo,ntiles))
     SNOW_AGE    => D1(jSNOW_AGE:jSNOW_AGE+  &
                          field_length(land_pts,no_halo,ntiles))
     SNOW_FLG3L  => D1(jSNOW_FLG3L:jSNOW_FLG3L+  &
                          field_length(land_pts,no_halo,ntiles))

end subroutine cable_set_atm_fields


!===============================================================================


end module cable_data_mod 


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!subroutine cable_parse_isnow(land_pts, ntiles, isnow_flg3l, &
!                          tsoil_tile, smcl_tile, sthf_tile,     & !
!                          snow_depth3l, snow_mass3l, snow_tmp3l,    & !
!                          snow_rho3l, snow_rho1l, snow_age ) 
!  
!   use cable_data_mod, ONLY: cable_point_isnow, msn, ms
!
!   integer, DIMENSION(land_pts, ntiles), target :: isnow_flg3L
!   real :: TSOIL_TILE(land_pts,ntiles,ms)
!   real :: SMCL_TILE(land_pts,ntiles,ms)
!   real :: STHF_TILE(land_pts,ntiles,ms)
!   real :: SNOW_DEPTH3L(land_pts,ntiles,msn)
!   real :: SNOW_MASS3L(land_pts,ntiles,msn)
!   real :: SNOW_TMP3L(land_pts,ntiles,msn)
!   real :: SNOW_RHO3L(land_pts,ntiles,msn)
!   real :: SNOW_RHO1L(land_pts,ntiles)
!   real :: SNOW_AGE(land_pts,ntiles)
!   real :: SNOW_FLG3L(land_pts,ntiles)
!
!   call cable_point_isnow(isnow_flg3l, &
!                          tsoil_tile, smcl_tile, sthf_tile,     & !
!                          snow_depth3l, snow_mass3l, snow_tmp3l,    & !
!                          snow_rho3l, snow_rho1l, snow_age ) 
!  
!end subroutine cable_parse_isnow































