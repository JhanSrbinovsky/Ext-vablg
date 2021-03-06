! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

      MODULE eg_helm_var_rhs_mod
      IMPLICIT NONE
      CONTAINS
      SUBROUTINE eg_helm_var_rhs(RHS,Rn,Ih, eta_rho_levels,           &
             exner_prime,                                             &
             R_u_a, R_v_a, R_w_a, R_theta_a, R_rho_a, R_p_a,          &
             R_etadot,row_length, rows, n_rows, model_levels,         &
             offx, offy)

      USE eg_vert_damp_mod, ONLY : mu_w
      USE yomhook,  ONLY: lhook, dr_hook
      USE parkind1, ONLY: jprb, jpim
      USE eg_helmholtz_mod
      USE horiz_grid_mod
      USE ref_pro_mod
      USE atm_fields_bounds_mod
      USE proc_info_mod, ONLY: model_domain, at_extremity
      USE UM_ParParams
      USE Field_Types
      USE helmholtz_const_matrix_mod
      USE coriolis_mod

      IMPLICIT NONE

!
! Description: Code to calculate the Fixed RHS terms
!              in the Helmholtz problem
!
! Method: ENDGame formulation version 3.02
!
! Code Owner: See Unified Model Code Owners HTML page
! This file belongs in section: Dynamics Solver
!
! Code description:
!   Language: Fortran 90.
!   This code is written to UM programming standards version 8.3.

!

! Array dimensions

      INTEGER,  INTENT(IN)    :: offx, offy
      INTEGER,  INTENT(IN)    :: row_length, rows, n_rows, model_levels

! Hydrostatic switch

      REAL,      INTENT(IN)    :: Ih

      REAL, INTENT(IN) ::   eta_rho_levels(model_levels)

      REAL, INTENT(IN) :: exner_prime(1-offx:row_length+offx,                  &
                                      1-offy:rows+offy,model_levels)

      REAL, INTENT(OUT)   ::                                                   &
          RHS(1-offx:row_length+offx,1-offy:rows+offy,model_levels)

      REAL, INTENT(IN)    ::                                                   &
           Rn(1-offx:row_length+offx,1-offy:rows+offy,model_levels)


      REAL, INTENT(IN) ::                                                      &
       R_u_a(-offx:row_length+offx-1,1-offy:rows+offy,model_levels),           &
       R_v_a(1-offx:row_length+offx,-offy:n_rows+offy-1,model_levels),         &
       R_w_a(row_length,rows,0:model_levels),                                  &
       R_theta_a(1-offx:row_length+offx,1-offy:rows+offy,0:model_levels),      &
       R_rho_a(1-offx:row_length+offx,1-offy:rows+offy,model_levels),          &
       R_p_a(1-offx:row_length+offx,1-offy:rows+offy,model_levels),            &
       R_etadot(row_length,rows,0:model_levels)


! Local temporary variables

      REAL    :: Rk, Rkm1(1-offx:row_length+offx,1-offy:rows+offy)
      REAL    :: rdxi1, rdxi2, rdxi3
      INTEGER :: i, j, k

      INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
      INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
      REAL(KIND=jprb)               :: zhook_handle

integer :: jhistart, jhiend, jhjstart, jhjend, jhkstart, jhkend 
integer :: jhi, jhj, jhk
logical, parameter :: write125=.false.
logical, parameter :: write126=.false.

      IF (lhook) CALL dr_hook('EG_HELM_VAR_RHS',zhook_in,zhook_handle)


! Calculate total RHS = RHS^n + RHS^star, eqn(9.44) of EG3.01
if(write126) then
   open(unit=126,file='helmvar126.rn1',status="unknown", &
        action="write", form="formatted",position='append' )
   open(unit=127,file='helmvar127.rho1',status="unknown", &
        action="write", form="formatted",position='append' )
   open(unit=128,file='helmvar128',status="unknown", &
        action="write", form="formatted",position='append' )
   open(unit=129,file='helmvar129',status="unknown", &
        action="write", form="formatted",position='append' )
   open(unit=130,file='helmvar130',status="unknown", &
        action="write", form="formatted",position='append' )
   open(unit=131,file='helmvar131',status="unknown", &
        action="write", form="formatted",position='append' )
   open(unit=132,file='helmvar132',status="unknown", &
        action="write", form="formatted",position='append' )
   open(unit=133,file='helmvar133',status="unknown", &
        action="write", form="formatted",position='append' )
   open(unit=134,file='helmvar134',status="unknown", &
        action="write", form="formatted",position='append' )
   open(unit=135,file='helmvar135',status="unknown", &
        action="write", form="formatted",position='append' )
   open(unit=136,file='helmvar136',status="unknown", &
        action="write", form="formatted",position='append' )

      DO k = 1, model_levels
         DO j = pdims%j_start, pdims%j_end
            rdxi2 = 1.0/(xi2_v(j) - xi2_v(j-1))
            DO i = pdims%i_start, pdims%i_end

                rdxi1      = 1.0/(xi1_u(i) - xi1_u(i-1))

         WRITE(126,*) rn(i,j,k)
         WRITE(127,*) rho_ref_pro(i,j,k)
         write(128,*) HM_p(i,j,k)
         write(129,*) R_p_a(i,j,k) 
         write(130,*) intw_w2rho(k,1)
write(131,*) R_theta_a(i,j,k)
         write(132,*) thetav_ref_pro(i,j,k)
         write(133,*) intw_w2rho(k,2)
write(134,*) R_theta_a(i,j,k-1) 
         write(135,*) thetav_ref_pro(i,j,k-1)
         write(136,*) R_rho_a(i,j,k)

                RHS(i,j,k) = Rn(i,j,k) - (rho_ref_pro(i,j,k)/HM_p(i,j,k)) *  &
                             ( R_p_a(i,j,k) +                                &
                             ( intw_w2rho(k,1)*R_theta_a(i,j,k) /            &
                                              thetav_ref_pro(i,j,k) +        &
                               intw_w2rho(k,2)*R_theta_a(i,j,k-1) /          &
                                              thetav_ref_pro(i,j,k-1) ))     &
                             - R_rho_a(i,j,k)

                RHS(i,j,k) = RHS(i,j,k) + HM_vol(i,j,k)*(                    &
                                     ( HM_rhox(i,j,k)*R_u_a(i,j,k)           &
                                     - HM_rhox(i-1,j,k)*R_u_a(i-1,j,k)       &
                                     )*rdxi1                                 &
                                    +( HM_rhoy(i,j,k)*R_v_a(i,j,k)           &
                                     - HM_rhoy(i,j-1,k)*R_v_a(i,j-1,k)       &
                                     )*rdxi2 )

            END DO
         END DO
      END DO
      WRITE(126,*) "xxx" 
      WRITE(127,*) "xxx" 
   close(126)
   close(127)
   close(128)
   close(129)
   close(130)
   close(131)
   close(132)
   close(133)
   close(134)
   close(135)
   close(136)
 endif  

jhistart = pdims%i_start
jhiend = pdims%i_end
jhjstart = pdims%j_start
jhjend = pdims%j_end
jhkstart = 1
jhkend = model_levels

if(write125) then
   open(unit=125,file='helmvar125.rhs1',status="unknown", &
        action="write", form="formatted",position='append' )
      do jhi=jhistart,jhiend; do jhj=jhjstart,jhjend;do jhk=jhkstart,jhkend
         WRITE(125,*) ,rhs(jhi,jhj,jhk)
      enddo; enddo; enddo
      WRITE(125,*) "xxx" 
   close(125)
 endif  
! These are the terms arising from D_1 in (9.46) of EG2.02

      Rkm1(:,:) = 0.0
      DO k = 1, model_levels-1
         rdxi3 = 1.0/(eta_rho_levels(k+1)-eta_rho_levels(k))
         DO j = pdims%j_start, pdims%j_end
            DO i = pdims%i_start, pdims%i_end

! Use same definintion of D_1(R) as in LHS : 
!            D_1(X) = E_k.R_k + F_k.R_(k-1)

               Rk = Hlm_Ck(i,j,k) *  ( R_w_a(i,j,k) +                          &
                       (Ih+mu_w(i,j,k))*R_etadot(i,j,k) -                      &
                      HM_w(i,j,k)*( exner_ref_pro(i,j,k+1)                     &
                                   -exner_ref_pro(i,j,k  )) *                  &
                      R_theta_a(i,j,k)*rdxi3/thetav_ref_pro(i,j,k) )

               RHS(i,j,k) = RHS(i,j,k) + Hlm_Ek(i,j,k)*Rk                      &
                                       + Hlm_Fk(i,j,k)*Rkm1(i,j)

               Rkm1(i,j)  = Rk

            END DO
         END DO
      END DO

if(write125) then
   open(unit=125,file='helmvar125.rhs2',status="unknown", &
        action="write", form="formatted",position='append' )
      do jhi=jhistart,jhiend; do jhj=jhjstart,jhjend;do jhk=jhkstart,jhkend
         WRITE(125,*) ,rhs(jhi,jhj,jhk)
      enddo; enddo; enddo
      WRITE(125,*) "xxx" 
   close(125)
 endif  
! Fix up top

      k = model_levels
      DO j = pdims%j_start, pdims%j_end
         DO i = pdims%i_start, pdims%i_end

            RHS(i,j,k) = RHS(i,j,k) + Hlm_Fk(i,j,k)*Rkm1(i,j)

         END DO
      END DO

if(write125) then
   open(unit=125,file='helmvar125.rhs3',status="unknown", &
        action="write", form="formatted",position='append' )
      do jhi=jhistart,jhiend; do jhj=jhjstart,jhjend;do jhk=jhkstart,jhkend
         WRITE(125,*) ,rhs(jhi,jhj,jhk)
      enddo; enddo; enddo
      WRITE(125,*) "xxx" 
   close(125)
 endif  
! Now rescale by the diagonal of linear operator: Hlm_Lp contains the
! reciprocal of the diagonal, see eg_set_helm_lhs.

      DO k = 1, model_levels
         DO j = pdims%j_start, pdims%j_end
            DO i = pdims%i_start, pdims%i_end
               RHS(i,j,k) = RHS(i,j,k)*Hlm_Lp(i,j,k)
            END DO
         END DO
      END DO

if(write125) then
   open(unit=125,file='helmvar125.rhs4',status="unknown", &
        action="write", form="formatted",position='append' )
      do jhi=jhistart,jhiend; do jhj=jhjstart,jhjend;do jhk=jhkstart,jhkend
         WRITE(125,*) ,rhs(jhi,jhj,jhk)
      enddo; enddo; enddo
      WRITE(125,*) "xxx" 
   close(125)
 endif  
! Exner fixed on boundary (exner prime =0)

      IF( model_domain == mt_lam ) THEN
         i = pdims%i_end - 1         ! one less p-point in LAM's
         IF( at_extremity(PWest) ) THEN
            RHS(1,:,:) = exner_prime(1,:,:)
         END IF
         IF( at_extremity(PEast) ) THEN
            RHS(i,:,:)   = exner_prime(i,:,:)
            RHS(i+1,:,:) = exner_prime(i+1,:,:)
         END IF

         j = pdims%j_end
         IF( at_extremity(PSouth) ) THEN
            RHS(:,1,:) = exner_prime(:,1,:)
         END IF
         IF( at_extremity(PNorth) ) THEN
            RHS(:,j,:) = exner_prime(:,j,:)
         END IF
      END IF

if(write125) then
   open(unit=125,file='helmvar125.rhs5',status="unknown", &
        action="write", form="formatted",position='append' )
      do jhi=jhistart,jhiend; do jhj=jhjstart,jhjend;do jhk=jhkstart,jhkend
         WRITE(125,*) ,rhs(jhi,jhj,jhk)
      enddo; enddo; enddo
      WRITE(125,*) "xxx" 
   close(125)
 endif  
      IF (lhook) CALL dr_hook('EG_HELM_VAR_RHS',zhook_out,zhook_handle)

      END SUBROUTINE eg_helm_var_rhs
      END MODULE eg_helm_var_rhs_mod
