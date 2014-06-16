! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine UPDATE_VEG
!
!  Purpose: The routine is entered when any of the ancillary
!           fields have to be updated. It then checks to see if
!           leaf area index and/or canopy height have been updated.
!           If this is the case, then the subroutine SPARM is called
!           to ensure that all other vegetation parameters are
!           consistent.
!
!           Code Owner: See Unified Model Code Owners HTML page
!           This file belongs in section: ancillaries

      Subroutine UPDATE_VEG (                                           &
#include "argd1.h"
     &                   submodel                                       &
     &                       )

      USE yomhook, ONLY: lhook, dr_hook
      USE parkind1, ONLY: jprb, jpim
      USE atm_fields_bounds_mod

      USE switches, ONLY: l_aggregate, can_model

      USE UM_ParParams
      USE domain_params
      USE nstypes
      IMPLICIT NONE

#include "typsize.h"
#include "typd1.h"
#include "typptra.h"
#include "cancila.h"

      Integer            :: submodel
!                        !  dummy variable to close subroutine
!                        !  argument list

      Integer, Parameter :: Anc_Ref_No_Leaf_Index = 84
      Integer, Parameter :: Anc_Ref_No_Canopy_Ht  = 85


!     Local variables
      Integer            :: TILE_PTS(NTYPE)
!                        !  Number of land points which
!                        !  include the nth surface type
      Integer            :: TILE_INDEX(LAND_FIELD,NTYPE)
!                        !  Indices of land points which
!                        !  include the nth surface type

      INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
      INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
      REAL(KIND=jprb)               :: zhook_handle

! Update vegetation parameters if required

      IF (lhook) CALL dr_hook('UPDATE_VEG',zhook_in,zhook_handle)
      If (       Update (Anc_Ref_No_Leaf_Index)                         &
     &      .or. Update (Anc_Ref_No_Canopy_Ht)                          &
     &    ) Then


!-----------------------------------------------------------------------
! Call TILEPTS to initialise TILE_PTS and TILE_INDEX
!-----------------------------------------------------------------------

! DEPENDS ON: tilepts
        CALL TILEPTS(LAND_FIELD,D1(JFRAC_TYP),TILE_PTS,TILE_INDEX)

!-----------------------------------------------------------------------
! Initialise tiled and gridbox mean vegetation parameters
!-----------------------------------------------------------------------

! DEPENDS ON: sparm
        CALL SPARM (LAND_FIELD,NTILES,CAN_MODEL,L_AGGREGATE,            &
     &              TILE_PTS,TILE_INDEX,                                &
     &              D1(JFRAC_TYP),D1(JCANHT_PFT),                       &
     &              D1(JLAI_PFT),D1(JSAT_SOIL_COND),D1(JCATCH_SNOW),    &
                    D1(JCATCH_TILE),D1(JINFIL_TILE),D1(JZ0_TILE),       &
                    D1(JZ0H_TILE))

      End If

      IF (lhook) CALL dr_hook('UPDATE_VEG',zhook_out,zhook_handle)
      RETURN
      END SUBROUTINE UPDATE_VEG
