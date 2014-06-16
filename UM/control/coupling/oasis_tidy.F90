#if defined(OASIS3) 
! *****************************COPYRIGHT****************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt which you should
! have received as part of this distribution.
! *****************************COPYRIGHT****************************************
SUBROUTINE oasis_tidy(                                        &
#include "argd1.h"
#include "argsts.h"
#include "argduma.h"
#include "argptra.h"
    icode,cmessage)
  !
  ! Description: Tidy up at the end of an OASIS-based coupled run.
  !              Expected to be applicable to all OASIS versions
  !              including OASIS3-MCT.
  !
  ! Code Owner: See Unified Model Code Owners HTML page
  ! This file belongs in section: Coupling
  !
  !--------------------------------------------------------------------

  ! Variable definitions for OASIS3 
  USE oasis_atm_data_mod

  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim
  USE atm_fields_bounds_mod

  USE UM_ParVars
  USE domain_params
  USE Submodel_Mod

  IMPLICIT NONE

#include "typsize.h"
#include "typd1.h"
#include "typsts.h"
#include "typduma.h"
#include "typptra.h"

  ! Dummy arguments - only used with stub version of this routine
  ! and hence theoretically never used.

  INTEGER       icode       ! OUT - Error return code
  CHARACTER(LEN=*) cmessage    ! OUT - Error return message

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle



  ! Deallocate arrays used in the processing of
  ! outgoing coupling data
  IF (lhook) CALL dr_hook('OASIS_TIDY',zhook_in,zhook_handle)
  DEALLOCATE(taux)
  DEALLOCATE(tauy)
  DEALLOCATE(solar2d)
  DEALLOCATE(blue2d)
  DEALLOCATE(evap2d)
  DEALLOCATE(longwave2d)
  DEALLOCATE(sensible2d)
  DEALLOCATE(heatflux)
  DEALLOCATE(sublim)
  DEALLOCATE(rainls)
  DEALLOCATE(snowls)
  DEALLOCATE(rainconv)
  DEALLOCATE(snowconv)
  DEALLOCATE(totalrain)
  DEALLOCATE(totalsnow)
  DEALLOCATE(riverout)
  DEALLOCATE(calving)
  DEALLOCATE(w10)
  DEALLOCATE(topmeltn)
  DEALLOCATE(fcondtopn)

  ! Deallocate arrays used in the processing of
  ! incoming coupling data
  DEALLOCATE(ocn_hice)
  DEALLOCATE(ocn_hicen)
  DEALLOCATE(ocn_freeze)
  DEALLOCATE(ocn_freezen)
  DEALLOCATE(ocn_snowthick)
  DEALLOCATE(ocn_snowthickn)
  DEALLOCATE(ocn_sst)
  DEALLOCATE(ocn_sst_orig)
  DEALLOCATE(ocn_u)
  DEALLOCATE(ocn_v)
  DEALLOCATE(ocn_icetn)
  DEALLOCATE(ocn_icet_gb)
  DEALLOCATE(ocn_icekn)
  DEALLOCATE(tstar_local)
  DEALLOCATE(tstar_ssi)
  DEALLOCATE(tstar_sice_local)
  DEALLOCATE(tstar_sice_cat_local)
  DEALLOCATE(tstar_land_local)
  DEALLOCATE(fland_loc)
  IF (lhook) CALL dr_hook('OASIS_TIDY',zhook_out,zhook_handle)
  RETURN


END SUBROUTINE oasis_tidy
#endif
