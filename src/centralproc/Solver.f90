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
    
    Do BCG_Iterations = 1, N_Size
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

  End subroutine BCG_Solve


End Module
