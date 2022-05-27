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


Subroutine solve(this, mat_CRS, Vecx, N_Size, phi_array)
    Implicit None
    class(t_Solver)    :: this
    type(t_CRS) :: mat_CRS
    Integer :: CG_Iterations, ii
    Integer :: N_Size
    Real(kind=dp) :: alpha, alpha_den, r_s_old, r_s_new
    Real(kind=dp), dimension(:) :: Vecx
    Real(kind=dp), dimension(N_Size) :: Output_Operate_array
    Real(kind=dp), dimension(N_Size) :: r_old_array, p_array, phi_array, phi_Previous_array
    Real(kind=dp), dimension(N_Size) :: Input_Operate_array
    Logical :: CG_Conv

    Write(*,*) Size(Vecx), N_Size
    Input_Operate_array(:) = 1._dp
    call mat_CRS%operate(Input_Operate_array, Output_Operate_array)
    r_old_array(:) = Vecx(:) - Output_Operate_array(:)
    p_array(:) = r_old_array(:)
    CG_Conv = .FALSE.
    r_s_old = dot_product(r_old_array,r_old_array)
    If (r_s_old .LT. 1E-5) Then
      CG_Conv = .TRUE.
    EndIf
    CG_Iterations = 0
    Do While ((CG_Conv .EQV. .FALSE.) .AND. (CG_Iterations .LT. 1000))
      CG_Iterations = CG_Iterations + 1
      Input_Operate_array(:) = p_array(:)
      call mat_CRS%operate(Input_Operate_array, Output_Operate_array)
      alpha_den = 0.0_dp
      Do ii = 1, N_Size
        alpha_den = alpha_den + (p_array(ii)*Output_Operate_array(ii))
      EndDo
      alpha = r_s_old/alpha_den
      phi_Previous_array(:) = phi_array(:)
      phi_array(:) = phi_array(:) + (alpha*p_array(:))
      If (dot_product(r_old_array,r_old_array) .LT. 1E-6) Then
        CG_Conv = .TRUE.
        Exit
      EndIf
      r_old_array(:) = r_old_array(:) - (alpha*Output_Operate_array(:))
      r_s_new = 0.0_dp
      Do ii = 1, N_Size
        r_s_new = r_s_new + (r_old_array(ii)**2)
      EndDo
      p_array(:) = r_old_array(:) + ((r_s_new/r_s_old)*p_array(:))
      r_s_old = r_s_new
    EndDo

  Write(*,*) "---CG Convergence Succeeded---"
  Write(*,'(g0)',advance='no') "Succeeded after iterations:  "
  Write(*,'(g0)',advance='no') CG_Iterations
  Write(*,'(g0)',advance='no') "  with residual:"
  Write(*,'(E14.6)') dot_product(r_old_array,r_old_array)
  Write(*,*) "-------------------------------"

  End subroutine solve


End Module
