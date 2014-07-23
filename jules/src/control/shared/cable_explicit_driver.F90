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
! Purpose: Passes UM variables to CABLE, calls cbm, passes CABLE variables 
!          back to UM. 'Explicit' is the first of two routines that call cbm at 
!          different parts of the UM timestep.
!
! Called from: UM/JULES sf_exch
!
! Contact: Jhan.Srbinovsky@csiro.au
!
! History: Developed for CABLE v1.8
!
!
! ==============================================================================

SUBROUTINE cable_explicit_driver( row_length, rows, land_pts, ntiles,npft,     &
                                  sm_levels, timestep, latitude, longitude,    &
                                  land_index, tile_frac,  tile_pts, tile_index,&
                                  bexp, hcon, satcon, sathh, smvcst,           &
                                  smvcwt,  smvccl, albsoil, snow_tile,         &
                                  snow_rho1l, snage_tile, snow_flg3l,         &
                                  snow_rho3l, snow_cond, snow_depth3l,         &
                                  snow_tmp3l, snow_mass3l, sw_down, lw_down,   &
                                  cos_zenith_angle, surf_down_sw, ls_rain,     &
                                  ls_snow, tl_1, qw_1, vshr_land, pstar, z1_tq,&
                                  z1_uv, L_tile_pts, canopy_tile,   &
                                  Fland, CO2_MMR, sthu_tile, smcl_tile,        &
                                  sthf_tile, sthu, tsoil_tile, canht_ft,       &
                                  lai_ft, sin_theta_latitude, dzsoil,          &
                                  FTL_TILE,  &
                                  FQW_TILE, TSTAR_TILE,   &
                                  U_S, U_S_STD_TILE,&
                                  CD_TILE, CH_TILE,   &
                                  RADNET_TILE, FRACA, rESFS, RESFT, Z0H_TILE,  &
                                  Z0M_TILE, RECIP_L_MO_TILE, EPOT_TILE,        &
                                  endstep, timestep_number, mype & 
)    
   
   !--- reads runtime and user switches and reports
   USE cable_um_tech_mod, ONLY : cable_um_runtime_vars, air, bgc, canopy,      &
                                 met, bal, rad, rough, soil, ssnow, sum_flux, veg 
   
   !--- vars common to CABLE declared 
   USE cable_common_module, ONLY : cable_runtime, cable_user, ktau_gl,         &
                                   knode_gl, kwidth_gl, kend_gl,               &
                                   report_version_no
   
   !--- subr to (manage)interface UM data to CABLE
   USE cable_um_init_mod, ONLY : interface_UM_data
   
   !--- subr to call CABLE model
   USE cable_cbm_module, ONLY : cbm

   USE cable_def_types_mod, ONLY : mp, ms

   USE cable_expl_unpack_mod
   !--- include subr called to write data for testing purposes 
   USE cable_diag_module

   IMPLICIT NONE
  
   !-------------------------------------------------------------------------- 
   !--- INPUT ARGS FROM sf_exch() --------------------------------------------
   !-------------------------------------------------------------------------- 
   
   !___IN: UM dimensions, array indexes, flags
   INTEGER ::                                                      & 
      row_length, rows, & ! UM grid resolution
      land_pts,         & ! # of land points being processed
      ntiles,           & ! # of tiles 
      npft,             & ! # of plant functional types
      sm_levels           ! # of soil layers 

   ! index of land points being processed
   INTEGER, DIMENSION(land_pts) :: land_index 

   ! # of land points on each tile
   INTEGER,  DIMENSION(ntiles) :: tile_pts 
   
   real,  DIMENSION(land_pts, ntiles) ::                         & 
      snow_flg3l      ! 3 layer snow flag

   INTEGER,  DIMENSION(land_pts, ntiles) ::                         & 
      tile_index ,& ! index of tile points being processed
      isnow_flg3l   ! 3 layer snow flag

   !___UM parameters: water density, soil layer thicknesses 
   REAL, parameter :: rho_water = 1000.
   REAL,  DIMENSION(sm_levels) :: dzsoil

   !___UM soil/snow/radiation/met vars
   REAL,  DIMENSION(land_pts) :: & 
      bexp,    & ! => parameter b in Campbell equation 
      hcon,    & ! Soil thermal conductivity (W/m/K).
      satcon,  & ! hydraulic conductivity @ saturation [mm/s]
      sathh,   &
      smvcst,  &
      smvcwt,  &
      smvccl,  &
      albsoil, &
      fland,   & 
      cos_zenith_angle ! jules
   
   REAL,  DIMENSION(row_length,rows) :: &
      sw_down!,          & 
      !cos_zenith_angle
   
   REAL,  DIMENSION(row_length,rows) ::                             &
      latitude,   &
      longitude,  &
      lw_down,    &
      ls_rain,    &
      ls_snow,    &
      tl_1,       &
      qw_1,       &  
      vshr_land,  &
      pstar,      &
      z1_tq,      &
      z1_uv

   REAL,  DIMENSION(land_pts, ntiles) ::                         &
      snow_tile

   REAL, DIMENSION(land_pts, ntiles) ::                            &
      tile_frac,  &    
      snow_rho1l, &
      snage_tile
   
   REAL, DIMENSION(row_length, rows, 4) ::                         &
      surf_down_sw 
   
   REAL, DIMENSION(land_pts, npft) ::                              &
      canht_ft, lai_ft 
   
   REAL, DIMENSION(land_pts, ntiles) ::                             &
      canopy_tile
   
   REAL,  DIMENSION(land_pts, ntiles,3) ::                       &
      snow_cond
   
   REAL, DIMENSION(land_pts, ntiles,3) ::                          &
      snow_rho3l,    &
      snow_depth3l,  &
      snow_mass3l,   &
      snow_tmp3l
   
   REAL, DIMENSION(land_pts, sm_levels) ::                         &
      sthu 
   
   REAL, DIMENSION(land_pts, ntiles, sm_levels) :: & 
      sthu_tile, &
      sthf_tile, &
      smcl_tile, &
      tsoil_tile
   
   REAL :: co2_mmr

   !___true IF vegetation (tile) fraction is greater than 0
   LOGICAL, DIMENSION(land_pts, ntiles) :: L_tile_pts
  
   REAL :: sin_theta_latitude(row_length,rows) 
     
   !___return fluxes

   REAL, DIMENSION(land_pts,ntiles) :: &
      FTL_TILE,   &  ! Surface FTL for land tiles     
      FQW_TILE       ! Surface FQW for land tiles     

   !___return temp and roughness
   REAL, DIMENSION(land_pts,ntiles) :: &
      TSTAR_TILE,       & 
      Z0H_TILE,         &
      Z0M_TILE

   !___return friction velocities/drags/ etc
   REAL, DIMENSION(land_pts,ntiles) :: &
      CD_TILE,    &     ! Drag coefficient
      CH_TILE,    &     ! Transfer coefficient for heat & moisture
      U_S_STD_TILE      ! Surface friction velocity

   REAL, DIMENSION(row_length,rows)  :: &
      U_S               ! Surface friction velocity (m/s)

   ! end step of experiment, this step, step width, processor num
   INTEGER :: endstep, timestep_number, mype
   !JULES already passing as integer
   !REAL ::  timestep     
   INTEGER :: timestep
   
   INTEGER:: itimestep
    
   !___return miscelaneous 
   REAL,  DIMENSION(land_pts,ntiles) :: &
      RADNET_TILE,   & ! Surface net radiation
      RESFS,         & ! Combined soil, stomatal & aerodynamic resistance
                       ! factor for fraction (1-FRACA) of snow-free land tiles
      RESFT,         & ! Total resistance factor.
                       ! FRACA+(1-FRACA)*RESFS for snow-free l_tile_pts,        
                       ! 1 for snow.    
      FRACA,         & ! Fraction of surface moisture
      RECIP_L_MO_TILE,& ! Reciprocal of the Monin-Obukhov length for tiles (m^-1)
      EPOT_TILE
     
   !-------------------------------------------------------------------------- 
   !--- end INPUT ARGS FROM sf_exch() ----------------------------------------
   !-------------------------------------------------------------------------- 
   
   !___ declare local vars 
   
   !___ location of namelist file defining runtime vars
   CHARACTER(LEN=200), PARAMETER ::                                            & 
      runtime_vars_file = 'cable.nml'


   !___ 1st call in RUN (!=ktau_gl -see below) 
   LOGICAL, SAVE :: first_cable_call = .TRUE.
 
   INTEGER, SAVE ::  iDiag0=0,iDiag1=0, iDiag2=0
    
   INTEGER :: i,j

   !isnow_flg3l = floor( snow_flg3l )
   isnow_flg3l = int( snow_flg3l )

print *, "jhan:cable_explicit1 "

   !--- initialize cable_runtime% switches 
   IF(first_cable_call) THEN
      cable_runtime%um = .TRUE.
      write(6,*) ""
      write(6,*) "CABLE_log"
      CALL report_version_no(6) ! wriite revision number to stdout(6)
   ENDIF
      
   !--- basic info from global model passed to cable_common_module 
   !--- vars so don't need to be passed around, just USE _module
   ktau_gl = timestep_number     !timestep of EXPERIMENT not necesarily 
                                 !the same as timestep of particular RUN
   knode_gl = mype               !which processor am i on?
   itimestep = INT(timestep)    !realize for 'call cbm' pass
   kwidth_gl = itimestep          !width of timestep (secs)
   kend_gl = endstep             !timestep of EXPERIMENT not necesarily 

   !--- internal FLAGS def. specific call of CABLE from UM
   !--- from cable_common_module
   cable_runtime%um_explicit = .TRUE.

   !--- user FLAGS, variables etc def. in cable.nml is read on 
   !--- first time step of each run. these variables are read at 
   !--- runtime and for the most part do not require a model rebuild.
   IF(first_cable_call) THEN
      CALL cable_um_runtime_vars(runtime_vars_file) 
      first_cable_call = .FALSE.
   ENDIF      
!integer :: jhistart, jhiend, jhjstart, jhjend, jhkstart, jhkend 
!integer :: jhi, jhj, jhk
!logical, parameter :: write125=.false.
!logical, save :: first_call = .true.
!
!jhistart = 1-offx
!jhiend = row_length+offx
!jhjstart = 1-offy
!jhjend = rows+offy
!jhkstart = 0
!jhkend = model_levels
!
!
!      IF (lhook) CALL dr_hook('EG_HELM_RHS_STAR',zhook_in,zhook_handle)
!
!if(write125) then
!   open(unit=125,file='s_th.1',status="unknown", &
!        action="write", form="formatted",position='append' )
!      do jhi=jhistart,jhiend; do jhj=jhjstart,jhjend;do jhk=jhkstart,jhkend
!         WRITE(125,*) ,s_thetav(jhi,jhj,jhk)
!      enddo; enddo; enddo
!      WRITE(125,*) "" 
!   close(125)
! endif 
 
!   do i=1,land_pts   
!   do j=1, ntiles
!   if(tile_index(i,j)>1000 .OR. tile_index(i,j) < 0 ) &
!      print *, "jhan:explicit tile_index", tile_index(i,j)
!   enddo   
!   enddo   
   !---------------------------------------------------------------------!
   !--- initialize CABLE using UM forcings etc. these args are passed ---!
   !--- down from UM.                                                 ---! 
   !---------------------------------------------------------------------!
   CALL interface_UM_data( row_length, rows, land_pts, ntiles, npft,           & 
                           sm_levels, itimestep, latitude, longitude,          &
                           land_index, tile_frac, tile_pts, tile_index,        &
                           bexp, hcon, satcon, sathh, smvcst, smvcwt,          &
                           smvccl, albsoil, snow_tile, snow_rho1l,             &
                           snage_tile, isnow_flg3l, snow_rho3l, snow_cond,     &
                           snow_depth3l, snow_tmp3l, snow_mass3l, sw_down,     &
                           lw_down, cos_zenith_angle, surf_down_sw, ls_rain,   &
                           ls_snow, tl_1, qw_1, vshr_land, pstar, z1_tq,       &
                           z1_uv, rho_water, L_tile_pts, canopy_tile, Fland,   &
                           CO2_MMR, sthu_tile, smcl_tile, sthf_tile,           &
                           sthu, tsoil_tile, canht_ft, lai_ft,                 &
                           sin_theta_latitude, dzsoil )                         

   canopy%oldcansto=canopy%cansto

   !---------------------------------------------------------------------!
   !--- real(timestep) width, CABLE types passed to CABLE "engine" as ---!  
   !--- req'd by Mk3L  --------------------------------------------------!
   !---------------------------------------------------------------------!
   CALL cbm( real(timestep), air, bgc, canopy, met, bal,                             &
             rad, rough, soil, ssnow, sum_flux, veg )


   !---------------------------------------------------------------------!
   !--- pass land-surface quantities calc'd by CABLE in explicit call ---!
   !--- back to UM.                                                   ---!
   !---------------------------------------------------------------------!
   call cable_expl_unpack( FTL_TILE, FQW_TILE,          &
                           TSTAR_TILE, &
                           U_S, U_S_STD_TILE, &
                           CD_TILE, CH_TILE, FLAND, RADNET_TILE,       &
                           FRACA, rESFS, RESFT, Z0H_TILE, Z0M_TILE,            &
                           RECIP_L_MO_TILE, EPOT_TILE, l_tile_pts,             &
                           ssnow%snowd, ssnow%cls, air%rlam, air%rho,          &
                           canopy%fe, canopy%fh, canopy%us, canopy%cdtq,       &
                           canopy%fwet, canopy%wetfac_cs, canopy%rnet,         &
                           canopy%zetar, canopy%epot, met%ua, rad%trad,        &
                           rad%transd, rough%z0m, rough%zref_tq )


   ! dump bitwise reproducible testing data
   IF( cable_user%RUN_DIAG_LEVEL == 'zero')                                    &
      call cable_diag( iDiag0, "FLUXES", mp, kend_gl, ktau_gl, knode_gl,            &
                          "FLUXES", canopy%fe + canopy%fh )
                

   cable_runtime%um_explicit = .FALSE.

END SUBROUTINE cable_explicit_driver

