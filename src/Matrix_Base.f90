module Matrix_Base

  use Constants_Mod

  implicit none

  type, abstract :: t_matrix_base
    integer ::  n_row, n_column !The number of rows and columns in the matrix
    contains
      private
      procedure, public                                   ::  print => print_matrix
      procedure(construct_matrix_base), public, deferred  ::  construct
      procedure(destroy_matrix_base), public, deferred    ::  destroy
      procedure(get_matrix_base), public, deferred        ::  get
      procedure(set_matrix_base), public, deferred        ::  set
      procedure(operate_matrix_base), public, deferred    ::  operate
  end type t_matrix_base

  !===============================================
  !===============================================
  
  abstract interface
    subroutine construct_matrix_base(this, n_row, n_column)
      import t_matrix_base
      class(t_matrix_base), intent(inout) ::  this
      integer, intent(in)         ::  n_row, n_column
    end subroutine construct_matrix_base
  end interface

  !===============================================
  !===============================================
  
  abstract interface
    subroutine destroy_matrix_base(this)
      import t_matrix_base
      class(t_matrix_base), intent(inout) ::  this
    end subroutine destroy_matrix_base
  end interface

  !===============================================
  !===============================================
  
  abstract interface
    function get_matrix_base(this, row, column)result(evaluation)
      use Constants_Mod
      import t_matrix_base
      class(t_matrix_base), intent(in)  ::  this
      integer, intent(in)               ::  row, column
      real(kind=dp)                     ::  evaluation
    end function get_matrix_base
  end interface

  !===============================================
  !===============================================

  abstract interface
    subroutine set_matrix_base(this, row, column, value)
      use Constants_Mod
      import t_matrix_base
      class(t_matrix_base), intent(inout) ::  this  
      integer, intent(in)         ::  row, column
      real(kind=dp), intent(in)   ::  value
    end subroutine set_matrix_base
  end interface

  !===============================================
  !===============================================

  abstract interface
    subroutine operate_matrix_base(this, vector_in, vector_out)
      use Constants_Mod
      import t_matrix_base
      class(t_matrix_base), intent(in)            ::  this !The matrix to multiply the vector by
      real(kind=dp), dimension(:), intent(in)     ::  vector_in  !The vector to be multiplied
      real(kind=dp), dimension(:), intent(inout)  ::  vector_out  !The vector which results from the multiplication
    end subroutine operate_matrix_base
  end interface

  contains

    subroutine print_matrix(this)
      class(t_matrix_base), intent(in)                    ::  this
      real(kind=dp), dimension(:), allocatable    ::  row_values
      integer                                     ::  ii, jj

      allocate(row_values(this%n_column))

      do ii=1, this%n_row
        do jj=1, this%n_column
          row_values(jj)=this%get(ii,jj)
        end do
        print*, real(row_values)
      end do

    end subroutine print_matrix

end module Matrix_Base