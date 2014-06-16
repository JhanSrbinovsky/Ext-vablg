! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module defines absolute maximum values for array dimensions
! that are used in IO
  
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.


MODULE max_dimensions

  IMPLICIT NONE

  INTEGER, PARAMETER ::                                               &
    ntype_max       = 20,                                             &
    npft_max        = 20,                                             &
    nnvg_max        = 10,                                             &
    ntiles_max      = 20,                                             &
    sm_levels_max   = 10,                                             &
    snow_layers_max = 10

END MODULE max_dimensions
