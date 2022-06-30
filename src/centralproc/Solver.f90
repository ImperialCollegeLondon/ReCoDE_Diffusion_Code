Module Solver_Mod

    use Constants_Mod
    use CRS_Mod
    Implicit None

    private
    public :: BCG_Solve

    type, public :: t_Solver
        
    end type
    !!Solves the system of equations to generate a result which can be fed into the output module
    !!Solver used when code is compiled without PETSc
contains


  Subroutine BCG_Solve(mat_CRS, Vecb, N_Size, phi)
    Class(t_matrix_base), pointer :: mat_CRS, M
    Integer :: N_Size
    Integer :: ii, BCG_Iterations
    Real(kind=dp) :: alpha, beta, omega, rho1, rho2
    Real(kind=dp), dimension(N_Size) :: phi, p, ph, s, sh, t, v, r, rh, Ax, b
    Real(kind=dp), dimension(:) :: Vecb
    Logical :: BCG_Conv=.False.

    !!Inverse Jacobi Preconditioner Matrix M
    allocate(t_crs :: M)
    call M%construct(N_Size,N_Size)
    Do ii = 1, N_Size 
      call M%set(ii,ii,(1._dp/mat_CRS%get(ii,ii)))
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

    call M%Destroy()

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
