! *****************************COPYRIGHT****************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt which you should
! have received as part of this distribution.
! *****************************COPYRIGHT****************************************

MODULE oasis_atm_data_mod
  ! Description:
  ! Useful data and arrays for use with OASIS coupling.
  ! This is expected to be applicable to all OASIS3
  ! versions including OASIS3-MCT.
  !
  ! Code Owner: See Unified Model Code Owners HTML page
  ! This file belongs in section: Coupling
  !
  !=====================================================================

  USE um_types
  USE field_types, ONLY : fld_type_u, fld_type_v, fld_type_p

  IMPLICIT NONE

  ! Things needed for general compilation of all models
  INTEGER (KIND=integer64) :: oasis_couple_ts ! Coupling timestep length

#if defined(OASIS3)

  ! Things only needed when OASIS3 or OASIS3-MCT is active.

  ! Address pointers for atmosphere-to-ocean (or any other
  ! potentially coupled component) fields which may be exchanged through
  ! any internal or external coupling mechanism.

  INTEGER:: ja_taux       ! Surface x-windstress on atmos grid
  INTEGER:: ja_tauy       ! Surface y-windstress on atmos grid
  INTEGER:: ja_w10        ! 10m wind on atmos grid
  INTEGER:: ja_solar      ! Net downward SW at surf on atmos grid
  INTEGER:: ja_blue       ! Net blueband SW at surf on atmos grid
  INTEGER:: ja_evap       ! Net evaporation over sea on atmos grid
  INTEGER:: ja_longwave   ! Net downward LW at surf on atmos grid
  INTEGER:: ja_sensible   ! Sensible heat flux over sea on atmos grid
  INTEGER:: ja_lssnow     ! Large-scale snowfall rate on atmos grid
  INTEGER:: ja_cvsnow     ! Convective snowfall rate on atmos grid
  INTEGER:: ja_lsrain     ! Large-scale rainfall rate on atmos grid
  INTEGER:: ja_cvrain     ! Convective rainfall rate on atmos grid
  INTEGER:: ja_riverout   ! Total river outflow on atmos grid
  INTEGER:: ja_sublim     ! Sublimation on atmos grid
  INTEGER:: ja_fcondtopn  ! Diff heat thro ice (ncat)
  INTEGER:: ja_topmeltn   ! Seaice top melting flux (ncat)

  ! OASIS-defined or related variables
  INTEGER (KIND=integer32) :: oasis_comp_id

  INTEGER :: oasis_cntlpe

  INTEGER (KIND=integer64) :: comm_in  ! OASIS defined communicator

  INTEGER (KIND=integer32) :: icpl  ! Indicates to OASIS whether this PE
                                    ! is involved in coupling or not

  INTEGER (KIND=integer32), PARAMETER :: icpl_atm = 1  ! Indicates an 
                                    ! atmos coupling process

  ! Standard OASIS partition types likely to be useful 
  INTEGER (KIND=integer32), PARAMETER :: PartitionSerial = 0
  INTEGER (KIND=integer32), PARAMETER :: PartitionBox = 2

  ! The UM atmos doesn't have land sea masks available
  ! to us as a matter of course. So we have to go
  ! through the agony of calculating these ourselves
  ! on each grid point type. Here, we define the arrays
  ! which hold the masks.

  ! Mask arrays, local.
  ! We have two versions of each mask because OASIS and the UM 
  ! atmosphere use opposite conventions for defining land (the 
  ! UM has a land point set to "TRUE", OASIS has a land point 
  ! set to "FALSE"! ).
  LOGICAL, ALLOCATABLE :: oasis_tmask(:,:,:)
  LOGICAL, ALLOCATABLE :: oasis_umask(:,:,:)
  LOGICAL, ALLOCATABLE :: oasis_vmask(:,:,:)
 
  LOGICAL, ALLOCATABLE :: um_tmask(:,:,:)
  LOGICAL, ALLOCATABLE :: um_umask(:,:,:)
  LOGICAL, ALLOCATABLE :: um_vmask(:,:,:)


  ! Mask arrays, global.
  LOGICAL, ALLOCATABLE :: gmasku(:,:)
  LOGICAL, ALLOCATABLE :: gmaskv(:,:)

  ! The current prism timestep.
  REAL :: prism_timestep
  INTEGER (KIND=integer64) :: prism_nsec  ! number of model seconds

  INTEGER (KIND=integer64) :: max_transients

  ! Transient field description components 
  TYPE TRANSIENT
   INTEGER (KIND=integer32) :: indx       ! Unique identifier
   INTEGER (KIND=integer32) :: field_id   ! Unique field identifier
   INTEGER (KIND=integer32) :: oasis_id   ! OASIS defined ID number
   INTEGER (KIND=integer32) :: oasis_info ! Return code from OASIS ops
   CHARACTER (LEN=1) :: grid              ! Grid cell type
   CHARACTER (LEN=8) :: name              ! Transient name
   LOGICAL (KIND=logical64):: polar_mean  ! Polar row meaning required
   LOGICAL (KIND=logical64):: ice_min     ! Ice polar row processing requd.
   REAL (KIND=REAL64), POINTER :: field(:,:)  ! Pointer to the actual field. 
  END TYPE TRANSIENT

  CHARACTER (LEN=3), PARAMETER :: my_component="ATM"

  TYPE (TRANSIENT), ALLOCATABLE :: transient_in(:)
  TYPE (TRANSIENT), ALLOCATABLE :: transient_out(:)

  CHARACTER (LEN=4) :: c_out
  CHARACTER (LEN=4) :: c_in
  CHARACTER (LEN=8) :: n_out
  CHARACTER (LEN=8) :: n_in
  INTEGER  ::          i_number
  INTEGER  ::          f_id

  ! Unit number for transient field namelist
  INTEGER, PARAMETER :: lb = 5

  NAMELIST/TRANSCOUNT/ max_transients

  NAMELIST/TRANSFLD/       &
           c_out,             &
           c_in,              &
           n_out,             &
           n_in,              & 
           i_number,          &                
           f_id                

  ! Logical switches indicate whether various coupling 
  ! fields are active or not. 

  ! Define flags for outgoing coupling fields
  LOGICAL :: l_heatflux 
  LOGICAL :: l_bluerad 
  LOGICAL :: l_solar
  LOGICAL :: l_runoff 
  LOGICAL :: l_w10 
  LOGICAL :: l_train 
  LOGICAL :: l_tsnow 
  LOGICAL :: l_evap2d 
  LOGICAL :: l_taux 
  LOGICAL :: l_tauy 
  LOGICAL :: l_sublim(5)
  LOGICAL :: l_topmeltn(5)
  LOGICAL :: l_fcondtopn(5)

  ! Define flags for incoming coupling fields
  LOGICAL :: l_ocn_sst 
  LOGICAL :: l_ocn_u 
  LOGICAL :: l_ocn_v 
  LOGICAL :: l_ocn_freeze 
  LOGICAL :: l_ocn_icetn(5)
  LOGICAL :: l_ocn_icekn(5)
  LOGICAL :: l_ocn_freezen(5)
  LOGICAL :: l_ocn_snowthickn(5)
  LOGICAL :: l_ocn_hicen(5)


  ! Arrays containing the "vind" numbers of the 
  ! transients we want to use. These are used to 
  ! define our transients using a simple loop.

  ! Indices used to point to various coupling fields.
  INTEGER :: vind_sublim(5)
  INTEGER :: vind_ocn_freezen(5)
  INTEGER :: vind_ocn_snowthickn(5)
  INTEGER :: vind_ocn_hicen(5)
  INTEGER :: vind_topmeltn(5)
  INTEGER :: vind_fcondtopn(5)
  INTEGER :: vind_ocn_icetn(5)
  INTEGER :: vind_ocn_icekn(5)

  ! Define indexes for outgoing coupling fields

  INTEGER , PARAMETER :: vind_heatflux = 1
  INTEGER , PARAMETER :: vind_bluerad = 2
  INTEGER , PARAMETER :: vind_solar = 54
  INTEGER , PARAMETER :: vind_runoff = 3
  INTEGER , PARAMETER :: vind_w10 = 4
  INTEGER , PARAMETER :: vind_train = 5
  INTEGER , PARAMETER :: vind_tsnow = 6
  INTEGER , PARAMETER :: vind_evap2d = 7

  DATA vind_topmeltn       / 8, 9,10,11,12/
  DATA vind_fcondtopn      /13,14,15,16,17/
  DATA vind_sublim         /18,19,20,21,22/

  INTEGER , PARAMETER :: vind_taux = 23
  INTEGER , PARAMETER :: vind_tauy = 24

  ! Define indexes for incoming coupling fields

  INTEGER , PARAMETER :: vind_ocn_sst = 25

  ! DATA statements are used to initialise the following because 
  ! potentially we might have gaps under which circumstances the 
  ! use of PARAMETER is no good.
  DATA vind_ocn_freezen    /26,27,28,29,30/
  DATA vind_ocn_snowthickn /31,32,33,34,35/
  DATA vind_ocn_hicen      /36,37,38,39,40/
  DATA vind_ocn_icetn      /41,42,43,44,45/
  DATA vind_ocn_icekn      /46,47,48,49,50/ 

  INTEGER , PARAMETER :: vind_ocn_u = 51
  INTEGER , PARAMETER :: vind_ocn_v = 52
  INTEGER , PARAMETER :: vind_ocn_freeze = 53

  INTEGER , PARAMETER :: vind_max=150

  ! Dimensions for coupling transient arrays.
  ! These will simply hold copies of lasize, but make 
  ! code less long winded.
  INTEGER :: oasis_jmt
  INTEGER :: oasis_jmt_u
  INTEGER :: oasis_jmt_v
  INTEGER :: oasis_imt

  ! Outgoing coupling arrays or components thereof
  ! Anything defined without a "target" is simply 
  ! a potential constituent value, not a direct coupling field. 

  REAL, ALLOCATABLE, target :: taux(:,:)
  REAL, ALLOCATABLE, target :: tauy(:,:)

  REAL, ALLOCATABLE, target :: solar2d(:,:)
  REAL, ALLOCATABLE, target :: blue2d(:,:)
  REAL, ALLOCATABLE, target :: evap2d(:,:)
  REAL, ALLOCATABLE :: longwave2d(:,:)
  REAL, ALLOCATABLE :: sensible2d(:,:)
  REAL, ALLOCATABLE, target :: sublim(:,:,:)
  REAL, ALLOCATABLE, target :: heatflux(:,:)

  REAL, ALLOCATABLE :: rainls(:,:)
  REAL, ALLOCATABLE :: snowls(:,:)
  REAL, ALLOCATABLE :: rainconv(:,:)
  REAL, ALLOCATABLE :: snowconv(:,:)
  REAL, ALLOCATABLE, target :: totalrain(:,:)
  REAL, ALLOCATABLE, target :: totalsnow(:,:)
  REAL, ALLOCATABLE, target :: riverout(:,:)
  REAL, ALLOCATABLE, target :: calving(:,:)
  REAL, ALLOCATABLE, target :: w10(:,:)

  REAL, ALLOCATABLE, target :: topmeltn(:,:,:)
  REAL, ALLOCATABLE, target :: fcondtopn(:,:,:)

  ! Incoming coupling transients or components thereof
  ! Anything defined without a "target" is simply 
  ! a potential constituent value, not a direct coupling field. 
  REAL, ALLOCATABLE, target :: ocn_sst(:,:) 
  REAL, ALLOCATABLE :: ocn_sst_orig(:,:)
  REAL, ALLOCATABLE, target :: ocn_freeze(:,:)
  REAL, ALLOCATABLE, target :: ocn_freezen(:,:,:)
  REAL, ALLOCATABLE :: ocn_hice(:,:)
  REAL, ALLOCATABLE, target :: ocn_hicen(:,:,:)
  REAL, ALLOCATABLE :: ocn_snowthick(:,:)
  REAL, ALLOCATABLE, target :: ocn_snowthickn(:,:,:)
  REAL, ALLOCATABLE, target :: ocn_u(:,:)
  REAL, ALLOCATABLE, target :: ocn_v(:,:)
  REAL, ALLOCATABLE, target :: ocn_icetn(:,:,:)
  REAL, ALLOCATABLE :: ocn_icet_gb (:,:)
  REAL, ALLOCATABLE, target :: ocn_icekn(:,:,:) 
  REAL, ALLOCATABLE :: tstar_local(:,:)
  REAL, ALLOCATABLE :: tstar_ssi(:,:)
  REAL, ALLOCATABLE :: tstar_sice_local(:,:)
  REAL, ALLOCATABLE :: tstar_sice_cat_local(:,:,:)
  REAL, ALLOCATABLE :: tstar_land_local(:,:)

  ! Local land-fraction      
  REAL, ALLOCATABLE :: fland_loc(:,:)

#endif

END MODULE oasis_atm_data_mod

