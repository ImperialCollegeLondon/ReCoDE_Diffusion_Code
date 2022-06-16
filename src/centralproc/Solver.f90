Module Solver_Mod

    use Constants_Mod
    use CRS_Mod
    Implicit None

    private
    public :: ThomAlg_Solve
    public :: CG_Solve
    public :: BCG_Solve

    type, public :: t_Solver
        
    end type
    !!Solves the system of equations to generate a result which can be fed into the output module
    !!Solver used when code is compiled without PETSc
contains


Subroutine ThomAlg_Solve(mat_CRS, Vecb, N_Size, phi)
    type(t_CRS) :: mat_CRS
    Integer :: ii, N_Size
    Real(kind=dp) :: alpha, beta
    Real(kind=dp), dimension(:) :: Vecb
    Real(kind=dp), dimension(N_Size) :: phi
    
    !!Thomas Algorithm
    Do ii = 2, N_Size
      beta = mat_CRS%get(ii,ii-1)/mat_CRS%get(ii-1,ii-1)
      alpha = mat_CRS%get(ii,ii) - beta*mat_CRS%get(ii-1,ii)
      call mat_CRS%insert(ii,ii,alpha)
      Vecb(ii)= Vecb(ii) - beta*Vecb(ii-1)
    EndDo
    phi(N_Size) = Vecb(N_Size)/mat_CRS%get(N_Size,N_Size)
    Do ii = N_Size-1, 1, -1
      phi(ii) = (Vecb(ii)- mat_CRS%get(ii,ii+1)*phi(ii+1))/mat_CRS%get(ii,ii)
    EndDo

  End subroutine ThomAlg_Solve


  Subroutine CG_Solve(mat_CRS, Vecb, N_Size, phi)
    type(t_CRS) :: mat_CRS
    Integer :: N_Size, CG_Iterations
    Real(kind=dp) :: alpha, rsold, rsnew
    Real(kind=dp), dimension(N_Size) :: phi, p, r, Ap, Ax
    Real(kind=dp), dimension(:) :: Vecb
    Logical :: CG_Conv = .False.


    !!Conjugate Gradient Algorithm
    phi = 1._dp
    call mat_CRS%operate(phi,Ax)
    r = Vecb - Ax 
    p = r 
    rsold = dot_product(r,r) 
    Do CG_Iterations = 1, N_Size
      call mat_CRS%operate(p,Ap)
      alpha = rsold/dot_product(p,Ap)
      phi = phi + (alpha*p)
      r = r - (alpha*Ap)
      rsnew = dot_product(r,r)
      If (SQRT(rsnew) < 1E-4) Then 
        CG_Conv = .True.
        exit 
      EndIf
      p = r + (rsnew/rsold)*p
      rsold = rsnew
    EndDo

    !!Exit if convergence failed
    If (.Not. CG_Conv) Then
      Write(*,*) "---CG Convergence Failed---"
      Write(*,*) "Maximum number of iterations reached"
      Write(*,*) "Terminating"
      Write(*,*) "-------------------------------"
      Error Stop "Solver failed to converge"
    EndIf
  
  !!Debug writes residual and iterations to terminal
# ifdef DEBUG 
  Write(*,*) "---CG Convergence Succeeded---"
  Write(*,'(g0)',advance='no') "Succeeded after iterations:  "
  Write(*,'(g0)',advance='no') CG_Iterations
  Write(*,'(g0)',advance='no') "  with residual:"
  Write(*,'(E14.6)') dot_product(r,r)
  Write(*,*) "-------------------------------"
# endif


  End subroutine CG_Solve


  Subroutine BCG_Solve(mat_CRS, Vecb, N_Size, phi)
    type(t_CRS) :: mat_CRS, M
    Integer :: N_Size
    Integer :: ii, BCG_Iterations
    Real(kind=dp) :: alpha, beta, omega, rho1, rho2
    Real(kind=dp), dimension(N_Size) :: phi, p, ph, s, sh, t, v, r, rh, Ax, b
    Real(kind=dp), dimension(:) :: Vecb
    Logical :: BCG_Conv=.False.

    !!Inverse Jacobi Preconditioner Matrix M
    call M%construct(N_Size)
    Do ii = 1, N_Size 
      call M%insert(ii,ii,(1._dp/mat_CRS%get(ii,ii)))
    EndDo

    !!Biconjugate Gradient Algorithm
    phi = 1._dp
    b = Vecb
    call mat_CRS%operate(phi,Ax)
    r = b - Ax 
    rh = r 
    
    Do BCG_Iterations = 1, 10
      rho1 = dot_product(rh,r)
      If (SQRT(rho1) < 1E-4) Then 
        BCG_Conv = .True.
        exit 
      EndIf 
      If (BCG_Iterations /= 1) Then 
        beta = (rho1/rho2)*(alpha/omega)
        p = r + beta*(p-omega*v)
      Else
        p = r 
      EndIf 
      call M%operate(p,ph)
      call mat_CRS%operate(ph,v)
      alpha = rho1/dot_product(rh,v)
      s = r-alpha*v
      call M%operate(s,sh)
      call mat_CRS%operate(sh,t)
      omega = dot_product(t,s)/dot_product(t,t)
      phi = phi + alpha * ph + omega * sh 
      r = s - omega * t 
      rho2 = rho1
    EndDo

    !!Exit if convergence failed
    If (.Not. BCG_Conv) Then
      Write(*,*) "---BCG Convergence Failed---"
      Write(*,*) "Maximum number of iterations reached"
      Write(*,*) "Terminating"
      Write(*,*) "-------------------------------"
      Error Stop "Solver failed to converge"
    EndIf
  
  !!Debug writes residual and iterations to terminal
# ifdef DEBUG 
  Write(*,*) "---BCG Convergence Succeeded---"
  Write(*,'(g0)',advance='no') "Succeeded after iterations:  "
  Write(*,'(g0)',advance='no') BCG_Iterations
  Write(*,'(g0)',advance='no') "  with residual:"
  Write(*,'(E14.6)') rho1
  Write(*,*) "-------------------------------"
# endif



    ! ! Bi-conjugate gradient squared stabilized.
    ! ! Van der Vorst's Bi-CGSTAB, 1992 SIAM J. Sci. Stat. Comput.
    ! !
    ! ! Solves x, for A*x = y,  where A*x is computed by mv_prod()
    ! ! On entry, X contains Y, on exit X is the found solution.
    ! ! If X_start is not present, X_start=0 is used.
    ! !
    ! Implicit None
    ! type(t_CRS) :: mat_CRS
    ! integer     :: mxiter, N_Size
    ! real(kind=dp)     :: err
    ! real(kind=dp), dimension(N_Size)       :: X, y, Vecb, phi
    ! integer                        :: iter
    ! real(kind=dp), dimension(N_Size) :: X_start
    ! real(kind=dp), allocatable, dimension(:)  :: P, R, R0, T, V
    ! real(kind=dp)  :: alpha, beta, omega, rho0, rho1, rnorm, ynorm
    ! integer        :: i, n

    ! !On entry x = y
    ! y = Vecb 
    ! x = y
    ! X_start = 1._dp
    ! err = 1E-5_dp
    ! mxiter = 20

    ! n = N_Size
    ! allocate( P(n), R(n), R0(n), T(n), V(n) )
    ! ynorm = dot_product(X,X)
    ! call mat_CRS%operate(X_start,R)
    ! R = X - R
    ! R0 = R
    ! P = 0
    ! V = 0
    ! rho0 = 1
    ! alpha = 1
    ! omega = 1
    ! iter = 0
    ! do i=1,mxiter
    !     iter = i
    !     rho1 = dot_product(R0,R)
    !     beta = (rho1/rho0)* (alpha/omega)
    !     P = R + beta*(P-omega*V)
    !     call mat_CRS%operate(P,V)
    !     alpha = rho1/dot_product(R0,V)
    !     R = R - alpha*V
    !     call mat_CRS%operate(R,T)
    !     omega = dot_product(T,R) / dot_product(T,T)
    !     X = X + alpha*P + omega*R
    !     R = R - omega*T
    !     rnorm = dot_product(R,R)
    !     write(unit=*, fmt=*) "|r|/|y| =", sqrt(rnorm/ynorm)
    !     if (sqrt(rnorm/ynorm) < err) then
    !       Write(*,*) "Exit loop"
    !       Write(*,*) "Its:", iter
    !       exit                          ! exit this loop
    !     end if
    !     rho0 = rho1
    ! end do
    ! deallocate( P, R, R0, T, V )
    
    ! Write(*,*) "Final Flux:"
    ! phi = X 
    ! Write(*,*) phi

  End subroutine BCG_Solve


End Module
