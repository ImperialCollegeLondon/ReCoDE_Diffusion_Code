module CRS_Mod

  use Matrix_Base

  implicit none

  type, extends(t_matrix_base)  ::  t_crs
    private
    integer, dimension(:), allocatable                        ::  col_index !The column of each non-zero entry into the matrix
    integer, dimension(:), allocatable                        ::  row_start !The index of the first entry in each row in the values array
    real(kind=dp), dimension(:), allocatable                  ::  values !The values of each non-zero entry in the matrix
    
    contains
      procedure, public ::  construct => construct_crs
      procedure, public ::  destroy => destroy_crs
      procedure, public ::  get => get_crs
      procedure, public ::  set => set_crs
      procedure, public ::  operate => operate_crs
      procedure ::  check_explicit => check_explicit_crs
      procedure ::  remove_zeroes => remove_zeroes_crs
      procedure ::  find_row_by_values_index => find_row_by_values_index_crs
      procedure ::  change_array_sizes => change_array_sizes_crs
      procedure ::  find_last_index_of_row => find_last_index_of_row_crs

  end type t_crs

  contains

    subroutine construct_crs(this, n_row, n_column)
      class(t_crs), intent(inout) ::  this
      integer, intent(in)         ::  n_row, n_column

      call this%destroy()

      this%n_row=n_row
      this%n_column=n_column

      allocate(this%row_start(n_row), this%values(0), this%col_index(0))
      
      this%row_start=1     

    end subroutine construct_crs

    !===============================================
    !===============================================

    subroutine destroy_crs(this)
      class(t_crs), intent(inout)          ::  this

      this%n_row=0
      this%n_column=0

      if (allocated(this%col_index)) deallocate(this%col_index)
      if (allocated(this%row_start)) deallocate(this%row_start)
      if (allocated(this%values)) deallocate(this%values)

    end subroutine destroy_crs

    !===============================================
    !===============================================

    function get_crs(this, row, column)result(evaluation) 
      class(t_crs), intent(in)     ::  this
      integer, intent(in)               ::  row, column
      real(kind=dp)                     ::  evaluation
      integer                           ::  ii
      logical                           ::  explicit

      !Check to see if the value is explicitly stored
      explicit=this%check_explicit(row, column)

      !The case where the value rquested is not explicitly stored and, so, is zero.
      if (.not. explicit)then
        evaluation=0.0_dp
        return
      end if

      !The case where the value is in a row containing non-zeroes
      do ii=this%row_start(row), this%find_last_index_of_row(row)
        if (column==this%col_index(ii))then   !The case where the value is explicitly stored
          evaluation=this%values(ii)
          return
        else if (column<this%col_index(ii))then   !The case where the value is not stored and so is implcitly zero
          evaluation=0.0_dp
          return
        end if        
      end do
  
      !The case where the value was not found in the row and so is implicitly zero
      evaluation=0.0_dp

    end function get_crs

    !===============================================
    !===============================================

    subroutine set_crs(this, row, column, value)
        class(t_crs), intent(inout) ::  this  
        integer, intent(in)         ::  row, column
        real(kind=dp), intent(in)   ::  value
        integer                     ::  ii, row_last_index, old_array_size

        !First, find the index of the last entry in the row of the value to be added
        row_last_index=this%find_last_index_of_row(row)
        !The case where the location has an explicit value already
        if (this%check_explicit(row, column))then
            do ii=this%row_start(row), row_last_index !Loop over the entire row
                if (column==this%col_index(ii))then !Replace the value with the new value once one of the matching column has been found
                    this%values(ii)=value
                end if
            end do
        else !The case where the location does not have an explicit value
            !Increase the size of arrays to accommodate the new value
            old_array_size = size(this%values)
            call this%change_array_sizes(old_array_size + 1) 

            do ii=this%row_start(row), row_last_index !Loop over all entire row
                if (column<this%col_index(ii))then!Found an entry which is to the right of the one to insert
                    !Shift existing values cto the right in the arrays
                    this%row_start(row+1:)=this%row_start(row+1:)+1
                    this%col_index(ii+1:)=this%col_index(ii:old_array_size)
                    this%values(ii+1:)=this%values(ii:old_array_size)
                    !Set the new values
                    this%col_index(ii)=column
                    this%values(ii)=value
                    return
                end if
            end do
            !Did not find a current entry to the right of the one to be inserted so insert it after the last one of the current row
            this%row_start(row+1:)=this%row_start(row+1:)+1
            this%col_index(row_last_index+2:old_array_size+1)=this%col_index(row_last_index+1:old_array_size)
            this%values(row_last_index+2:old_array_size+1)=this%values(row_last_index+1:old_array_size)
            this%col_index(row_last_index+1)=column
            this%values(row_last_index+1)=value
        end if

        !Finally, remove any zeroes in the matrix
        call this%remove_zeroes()

    end subroutine set_crs

    !===============================================
    !===============================================

      subroutine operate_crs(this, vector_in, vector_out)
        class(t_crs), intent(in)                      ::  this !The matrix to multiply the vector by
        real(kind=dp), dimension(:), intent(in)       ::  vector_in  !The vector to be multiplied
        real(kind=dp), dimension(:), intent(inout)    ::  vector_out  !The vector which results from the multiplication
        integer                                       ::  ii  !Generic counting variable
        integer                                       ::  current_row
  
        !First, check if the vector to be multiplied has the same number of rows as the matrix
        if (size(vector_in).ne.this%n_column) then
          write(*, '(2(A, I0),A)') "operate has been given a vector of size ", size(vector_in), " to multiply which is of a different size to the matrix which has ", this%n_column, " columns. Terminating."
          Error Stop "Incorrectly sized vector for matrix multiplication"
        end if
  
        !Set the output to zero start with, then add up all contributions
        vector_out=0.0_dp
        current_row=1
        do ii=1, size(this%values)
          if (current_row<this%n_row) then
            do while (ii>=this%row_start(current_row+1))
              current_row=current_row+1
              if (current_row==this%n_row) exit
            end do
          end if
  
          vector_out(current_row)=vector_out(current_row)+this%values(ii)*vector_in(this%col_index(ii))
        end do
  
      end subroutine operate_crs

    !===============================================
    !===============================================

    function check_explicit_crs(this, row, column)result(explicit)
      class(t_crs), intent(in)     ::  this
      integer, intent(in)               ::  row, column
      integer                           ::  ii, row_last_index
      logical                           ::  explicit

      !The case that the matrix does not have a positive number of entries
      if (this%n_row<1 .or. this%n_column<1)then
        write(*, '(A, 2(I0, A))') "Error: check_explicit_crs was asked for a value when the matrix only has ", this%n_row, " row(s) and ", this%n_column, " column(s). Terminating."
        Error Stop "Unitialised matrix"
      end if

      !The case that the value asked for falls outside the matrix
      if (row<1 .or. row>this%n_row .or. column<1 .or. column>this%n_column)then
        write(*, '(A, 4(I0, A))') "Error: check_explicit_crs was asked for the value at location (", row, ", ", column, ") of the matrix when the matrix's row numbers extend from 1 to ", this%n_row, " and the matrix's columns extend from 1 to ", this%n_column, ". Terminating."
        Error Stop "Matrix access out of bounds"
      end if

      !The case that the row of the requested value is before the first non-zero row or after the last non-zero row
      if (this%row_start(row)==0 .or. this%row_start(row).gt. size(this%values))then 
        explicit=.false.
        return
     end if

      !If this is the last row then, as we already know it is not after the last non-zero row, it must contain non-zeroes so do not evaluate the contained if statement
      if (row.ne.this%n_row)then
        !The case that the row of the requested value is not before the first non-zero row or after the last non-zero row but is in a row of zeroes
        if (this%row_start(row)==this%row_start(row+1))then
          explicit=.false.
          return
        end if
      end if

      !Find the index of the last entry in the row of the value to be added
      row_last_index=this%find_last_index_of_row(row)

      do ii=this%row_start(row),  row_last_index
        if (column==this%col_index(ii))then
          explicit=.true.
          return
        end if
      end do

      explicit=.false.

    end function check_explicit_crs

    !===============================================
    !===============================================

    subroutine remove_zeroes_crs(this)
      class(t_crs), intent(inout)  ::  this
      integer                           ::  ii, n_zeroes

      !Move any values in the values array which are after a zero one to the left. Also track the number of zeroes
      n_zeroes=0      
      do ii=1, size(this%values)
        if (abs(this%values(ii-n_zeroes))<1.0e-200_dp)then
!          print*, "ZERO-REMOVAL"
!          print*, ii-n_zeroes, this%find_row_by_values_index(ii-n_zeroes)
          this%row_start(this%find_row_by_values_index(ii-n_zeroes)+1:)=this%row_start(this%find_row_by_values_index(ii-n_zeroes)+1:)-1
          this%col_index(ii-n_zeroes:size(this%values)-1)=this%col_index(ii-n_zeroes+1:size(this%values))
          this%values(ii-n_zeroes:size(this%values)-1)=this%values(ii-n_zeroes+1:size(this%values))
          n_zeroes=n_zeroes+1
        end if
      end do

      !Now shorten the arrays to remove unnecessary storage
      call this%change_array_sizes(size(this%values)-n_zeroes)

    end subroutine remove_zeroes_crs

    !===============================================
    !===============================================

    function find_row_by_values_index_crs(this, values_index)result(row)
      class(t_crs), intent(in)     ::  this
      integer, intent(in)               ::  values_index
      integer                           ::  row, ii

      do ii=1, this%n_row-1
        if (values_index.ge.this%row_start(ii) .and. values_index<this%row_start(ii+1)) then
          row=ii
          return
        end if
      end do

      row=this%n_row
    
    end function find_row_by_values_index_crs

    !===============================================
    !===============================================

    subroutine change_array_sizes_crs(this, new_size)
        class(t_crs), intent(inout)          ::  this
        integer, intent(in)                       ::  new_size
        integer, dimension(:), allocatable        ::  col_index_temp
        real(kind=dp), dimension(:), allocatable  ::  values_temp
        integer                                   ::  n_kept

        n_kept=min(size(this%values), new_size)
        
        !Warning: changing the size of arrays to a size below the original size will result in information being lost from the end of values and col_index

        allocate(col_index_temp(n_kept), values_temp(n_kept))

        col_index_temp(1:n_kept)=this%col_index(1:n_kept)
        values_temp=this%values(1:n_kept)
        
        deallocate(this%col_index, this%values)

        allocate(this%col_index(new_size), this%values(new_size))
    
        this%col_index=0
        this%values=0.0_dp

        this%col_index(1:n_kept)=col_index_temp(1:n_kept)
        this%values(1:n_kept)=values_temp(1:n_kept)

        deallocate(col_index_temp, values_temp)

      end subroutine change_array_sizes_crs

    !===============================================
    !===============================================

    function find_last_index_of_row_crs(this, row)result(row_last_index)
      class(t_crs), intent(in)  ::  this
      integer, intent(in)       ::  row
      integer                   ::  row_last_index, ii

      !Initialise with a value which will cause an error if no value is returned for some reason
      row_last_index = -1

      if (row==this%n_row) then!This is the last row
        do ii=size(this%values), 1, -1
          if (this%col_index(ii)>0)then
            row_last_index=ii
            return
          end if
        end do
      else if (this%row_start(row).ne.this%row_start(row+1)) then !This is not the last row and is not all zeroes
        row_last_index=this%row_start(row+1)-1
      else if (this%row_start(row)==this%row_start(row+1)) then !This is not the last row, but is empty
        row_last_index=this%row_start(row)-1
      else !This row is not the last row or empty
          row_last_index=this%row_start(row+1)
      end if
    end function find_last_index_of_row_crs

end module CRS_Mod
