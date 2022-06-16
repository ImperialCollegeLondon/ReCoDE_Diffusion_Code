Module Solver_Mod

    use Constants_Mod
    use CRS_Mod
    Implicit None

    private
    public :: ThomAlg_Solve
    public :: CG_Solve

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
    Integer :: N_Size
    Real(kind=dp) :: alpha
    Real(kind=dp), dimension(:) :: Vecb
    Real(kind=dp), dimension(N_Size) :: phi
    Integer :: CG_Iterations
    Real(kind=dp) :: rsold, rsnew
    Real(kind=dp), dimension(N_Size) :: p, r, Ap, Ax
    Logical :: CG_Conv


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


End Module
