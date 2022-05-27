Module Solver_Mod

    use Constants_Mod
    use CRS_Mod
    Implicit none

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
    Implicit None
    class(t_Solver)    :: this
    type(t_CRS) :: mat_CRS
    Integer :: CG_Iterations, ii
    Integer :: N_Size
    Real(kind=dp) :: alpha, alpha_den, r_s_old, r_s_new
    Real(kind=dp), dimension(:) :: Vecb
    Real(kind=dp), dimension(N_Size) :: Output_Operate
    Real(kind=dp), dimension(N_Size) :: r_old, p, phi, phi_Previous
    Real(kind=dp), dimension(N_Size) :: Input_Operate
    Logical :: CG_Conv

    phi = 1._dp
    call mat_CRS%operate(phi, Output_Operate)

    r_old(:) = Vecb(:) - Output_Operate(:)
    p(:) = r_old(:)
    CG_Conv = .FALSE.
    r_s_old = dot_product(r_old,r_old)
    If (r_s_old .LT. 1E-8) Then
      CG_Conv = .TRUE.
    EndIf
    CG_Iterations = 0
    Do While ((CG_Conv .EQV. .FALSE.) .AND. (CG_Iterations .LT. 10))
      CG_Iterations = CG_Iterations + 1
      Input_Operate(:) = p(:)
      call mat_CRS%operate(Input_Operate, Output_Operate)
      alpha_den = 0.0_dp
      Do ii = 1, N_Size
        alpha_den = alpha_den + (p(ii)*Output_Operate(ii))
      EndDo
      alpha = r_s_old/alpha_den
      phi_Previous(:) = phi(:)
      phi(:) = phi(:) + (alpha*p(:))
      r_old(:) = r_old(:) - (alpha*Output_Operate(:))
      r_s_new = 0.0_dp
      Do ii = 1, N_Size
        r_s_new = r_s_new + (r_old(ii)**2)
      EndDo

      If (dot_product(r_old,r_old) .LT. 1E-8) Then
        CG_Conv = .TRUE.
      EndIf
      
      p(:) = r_old(:) + ((r_s_new/r_s_old)*p(:))
      r_s_old = r_s_new
    EndDo

# ifndef NDEBUG 
  Write(*,*) "---CG Convergence Succeeded---"
  Write(*,'(g0)',advance='no') "Succeeded after iterations:  "
  Write(*,'(g0)',advance='no') CG_Iterations
  Write(*,'(g0)',advance='no') "  with residual:"
  Write(*,'(E14.6)') dot_product(r_old,r_old)
  Write(*,*) "-------------------------------"
# endif

  End subroutine solve


End Module
