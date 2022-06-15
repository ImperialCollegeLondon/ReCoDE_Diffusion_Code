Module Solver_Mod

    use Constants_Mod
    use CRS_Mod
    Implicit None

    private

    type, public :: t_Solver
        private

    contains
        procedure, public :: solve
    end type
    !!Solves the system of equations to generate a result which can be fed into the output module
    !!Solver used when code is compiled without PETSc
contains


Subroutine solve(this, mat_CRS, Vecb, N_Size, phi)
    class(t_Solver)    :: this
    type(t_CRS) :: mat_CRS
    Integer :: CG_Iterations, ii
    Integer :: N_Size
    Real(kind=dp) :: alpha, alpha_den, r_s_old, r_s_new
    Real(kind=dp), dimension(:) :: Vecb
    Real(kind=dp), dimension(N_Size) :: Output_Operate, Input_Operate
    Real(kind=dp), dimension(N_Size) :: r_old, p, phi, phi_Previous
    Logical :: CG_Conv
    Real(kind=dp) :: rsold, rsnew, coeff
    Real(kind=dp), dimension(N_Size) :: r, b, x, Ap, Ax
    Integer :: i, n

    !!Conjugate Gradient Solver converted from wikipeadia matlab code
    x = 1._dp
    b = Vecb
    call mat_CRS%operate(x,Ax)
    r = b - Ax 
    p = r 
    rsold = dot_product(r,r) 
    Do CG_Iterations = 1, 10
      call mat_CRS%operate(p,Ap)
      alpha = rsold/dot_product(p,Ap)
      x = x + (alpha*p)
      r = r - (alpha*Ap)
      rsnew = dot_product(r,r)
      If (SQRT(rsnew) < 1E-4) Then 
        exit 
      EndIf
      p = r + (rsnew/rsold)*p
      rsold = rsnew
    EndDo
    phi = x
    write(*,*) "Wiki CG Alg Result after Its:", CG_Iterations
    write(*,*) phi(1), "->", phi(N_Size)


    
    !!Previously used CG Implementation
    phi = 1._dp
    call mat_CRS%operate(phi, Output_Operate)
    r_old = Vecb - Output_Operate
    p = r_old
    r_s_old = dot_product(r_old,r_old)

    CG_Conv = .False.

    Do CG_Iterations = 1, 10
      If (r_s_old < 1E-4) Then
        CG_Conv = .True.
        exit
      EndIf
      call mat_CRS%operate(p, Output_Operate)
      alpha_den = dot_product(p, Output_Operate)
      alpha = r_s_old/alpha_den
      phi_Previous = phi
      phi = phi + (alpha*p)
      r_old = r_old - (alpha*Output_Operate)
      r_s_new = dot_product(r_old, r_old)
      p = r_old + ((r_s_new/r_s_old)*p)
      r_s_old = r_s_new
    EndDo
    write(*,*) "Precious CG Alg Result after Its:", CG_Iterations
    write(*,*) phi(1), "->", phi(N_Size)

    !!Thomas Algorithm
    Do ii= 2, N_SIZE
      coeff = mat_CRS%get(ii,ii-1)/mat_CRS%get(ii-1,ii-1)
      alpha = mat_CRS%get(ii,ii) - coeff*mat_CRS%get(ii-1,ii)
      call mat_CRS%insert(ii,ii,alpha)
      b(ii)= b(ii) - coeff*b(ii-1)
    EndDo

    !step 2: back substitution
    x(N_SIZE) = b(N_SIZE)/mat_CRS%get(N_SIZE,N_SIZE)
    Do ii = N_SIZE-1, 1, -1
      x(ii) = (b(ii)- mat_CRS%get(ii,ii+1)*x(ii+1))/mat_CRS%get(ii,ii)
    EndDo

    phi = x
    write(*,*) "Thom Alg Result"
    write(*,*) phi(1), "->", phi(N_Size)



    Write(*,*) "PETSc Result"
    Write(*,*) 1.4009_dp, "->", 1.5991_dp

    ! If (.Not. CG_Conv) Then
    !   Write(*,*) "---CG Convergence Failed---"
    !   Write(*,*) "Maximum number of iterations reached"
    !   Write(*,*) "Terminating"
    !   Write(*,*) "-------------------------------"
    !   Error Stop "Solver failed to converge"
    ! EndIf
    

    !!Debug version
    ! phi = 1._dp
    ! call mat_CRS%operate(phi, Output_Operate)
    ! write(*,*) "out:", Output_Operate
    ! write(*,*) "vecb:", vecb
    ! r_old = Vecb - Output_Operate
    ! write(*,*) "r_old:", r_old
    ! p = r_old
    ! r_s_old = dot_product(r_old,r_old)
    ! ! write(*,*) "p1", p
    ! write(*,*) "r_s_old", r_s_old

    ! CG_Conv = .False.

    ! Do CG_Iterations = 1, 30
    !   If (r_s_old < 1E-1) Then
    !     CG_Conv = .True.
    !     exit
    !   EndIf
    !   ! write(*,*) "Residual:", r_s_old

    !   call mat_CRS%operate(p, Output_Operate)
    !   alpha_den = dot_product(p, Output_Operate)
    !   alpha = r_s_old/alpha_den
    !   phi_Previous = phi
    !   phi = phi + (alpha*p)
    !   write(*,*) "---"
    !   write(*,*) "phi it:", CG_Iterations
    !   write(*,*) phi_Previous
    !   write(*,*) "                   \/"
    !   write(*,*) phi
    !   Write(*,*) "Delta"
    !   write(*,*) phi-phi_Previous
    !   r_old = r_old - (alpha*Output_Operate)
    !   r_s_new = dot_product(r_old, r_old)
    !   p = r_old + ((r_s_new/r_s_old)*p)
    !   write(*,*) "residual:", r_s_old, "->", r_s_new
    !   Write(*,*) "p:", p
    !   r_s_old = r_s_new
      
    ! EndDo

    ! If (.Not. CG_Conv) Then
    !   Write(*,*) "---CG Convergence Failed---"
    !   Write(*,*) "Maximum number of iterations reached"
    !   Write(*,*) "Terminating"
    !   Write(*,*) "-------------------------------"
    !   ! Error Stop "Solver failed to converge"
    ! EndIf


    !!Old version
    ! phi = 1.0_dp
    ! Input_Operate(:) = phi(:)
    ! call mat_CRS%operate(Input_Operate, Output_Operate)
    ! r_old(:) = Vecb(:) - Output_Operate(:)
    ! p(:) = r_old(:)
    ! CG_Conv = .FALSE.
    ! r_s_old = dot_product(r_old,r_old)
    ! If (r_s_old .LT. 1E-5) Then
    !   CG_Conv = .TRUE.
    ! EndIf
    ! CG_Iterations = 0
    ! Do While ((CG_Conv .EQV. .FALSE.) .AND. (CG_Iterations .LT. 1000))
    !   CG_Iterations = CG_Iterations + 1
    !   Input_Operate(:) = p(:)
    !   call mat_CRS%operate(Input_Operate, Output_Operate)
    !   alpha_den = 0.0_dp
    !   Do ii = 1, N_SIZE
    !     alpha_den = alpha_den + (p(ii)*Output_Operate(ii))
    !   EndDo
    !   alpha = r_s_old/alpha_den
    !   phi_Previous(:) = phi(:)
    !   phi(:) = phi(:) + (alpha*p(:))
    !   If (dot_product(r_old,r_old) .LT. 1E-6) Then
    !     CG_Conv = .TRUE.
    !     Exit
    !   EndIf
    !   r_old(:) = r_old(:) - (alpha*Output_Operate(:))
    !   r_s_new = 0.0_dp
    !   Do ii = 1, N_SIZE
    !     r_s_new = r_s_new + (r_old(ii)**2)
    !   EndDo
    !   p(:) = r_old(:) + ((r_s_new/r_s_old)*p(:))
    !   r_s_old = r_s_new
    ! EndDo
    ! Write(*,*) "CG Iterations Performed:", CG_Iterations
    ! Write(*,*) "Phi:"
    ! Write(*,*) phi

! # ifdef DEBUG 
!   Write(*,*) "---CG Convergence Succeeded---"
!   Write(*,'(g0)',advance='no') "Succeeded after iterations:  "
!   Write(*,'(g0)',advance='no') CG_Iterations
!   Write(*,'(g0)',advance='no') "  with residual:"
!   Write(*,'(E14.6)') dot_product(r_old,r_old)
!   Write(*,*) "-------------------------------"
! # endif

!!Testing known solution to problem
! Write(*,*) "----Test----"
! Write(*,*) phi
! write(*,*) "->"
! call mat_CRS%operate(phi, Output_Operate)
! Write(*,*) Output_Operate-Vecb
! Write(*,*) "----Ans----"
! Input_Operate(1) = 1.4009
! Input_Operate(2) = 1.42495
! Input_Operate(3) = 1.5
! Input_Operate(4) = 1.57505
! Input_Operate(5) = 1.5991
! Write(*,*) Input_Operate
! write(*,*) "->"
! call mat_CRS%operate(Input_Operate, Output_Operate)
! Write(*,*) Output_Operate-Vecb


  End subroutine solve


End Module
