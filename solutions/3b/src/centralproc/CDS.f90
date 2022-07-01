module CDS_Mod

use Matrix_Base

implicit none

type, extends(t_matrix_base)  ::  t_cds  !A type designed to store a matrix in Compressed Diagonal storage fromat using the Intel MKL diagonal storage format
  private
  integer                            ::  ndiag !An integer containing the number of diagonals with non-zero elements
  integer, dimension(:), allocatable ::  distance  !An array which contains the locations of the diagonals with non-zero elements
  real(kind=dp), dimension(:,:), allocatable  ::  values  !An array which contains the values found in each diagonal. The first dimension increases with increasing diagonal number, the second with increasing row number
contains
  procedure, public ::  construct => cds_construct
  procedure, public ::  destroy => cds_destroy
  procedure, public ::  get => cds_get
  procedure, public ::  set => cds_set
  procedure, public ::  operate => operate_cds
  procedure ::  find_diag_ref => cds_find_diag_ref
  procedure ::  remove_zero => cds_remove_zero

end type t_cds

contains

!----------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------

subroutine cds_construct(this, n_row, n_column) !A subroutine which takes nthe size of the matrix as an argument to create an instance of the type t_cds
  class(t_cds), intent(inout) :: this !The instance of t_cds to be created
  integer, intent(in)              :: n_row, n_column !the size of the matrix

  call this%destroy()

  if (n_column /= n_row)then
    Error Stop "In cds_construct, the number of rows and number of columns were not equal. Terminating."
  end if

  this%n_row=n_row
  this%ndiag=0

end subroutine

!----------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------

subroutine cds_destroy(this) !A subroutine which takes an instance of the t_cds type and deallocates all arrays and returns the equivalent of a matrix of size 0.
  class(t_cds), intent(inout) :: this !The instance of t_cds to be destroyed

  if (allocated(this%distance)) deallocate(this%distance)
  if (allocated(this%values)) deallocate(this%values)

end subroutine

!----------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------

real(kind=dp) function cds_get(this, row, column) !A function which returns the value of a specific location in the matrix
  class(t_cds), intent(in) ::  this !The matric whose values are to be returned
  integer, intent(in)           ::  row  !The row of the value to be returned
  integer, intent(in)           ::  column  !The column of the value to be returned
  integer                       ::  diagonal !The diagonal of the value to be returned
  integer                       ::  location  !The location of the value to be returned within the diagonal
  integer                       ::  diagref  !The first array index of the value to be returned in the values array of the t_cds type
  logical                       ::  padded  !Will be set to false if the value is not padded

  !Check the called for value is within the matrix. If it's not then end the program
  if (row.gt.this%n_row .or. column.gt.this%n_row .or. row.lt.1 .or. column.lt.1) then
    print*, "The value asked to be retrieved by cds_get is outside the size of the matrix. Terminating program."
    STOP
  end if

  !Check the matrix is non-zero. If it is, return zero
  if (this%ndiag==0) then
    cds_get=0.0_dp
    return
  end if

  !Find the diagonal and the location in that diagonal of the value to be returned
  diagonal=column-row
  location=column+row-1

  call this%find_diag_ref(diagonal, diagref, padded)

  !If the value is not in a stored diagonal the return the value 0
  if (padded) then
    cds_get=0.0_dp
  !Otherwise return the value stored in the values array of the containing t_cds type
  else
    cds_get=this%values(diagref,row)
  end if

end function

!----------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------

subroutine cds_set(this, row, column, value)
  class(t_cds), intent(inout)            ::  this !The matrix to be modified
  integer, intent(in)                         ::  row !The row of the entry to be modified
  integer, intent(in)                         ::  column !The column of the entry to be modified
  real(kind=dp), intent(in)                   ::  value !The value it is to be modifed to
  integer                   ::  diagonal !The diagonal to be modified
  integer                   ::  diagref !The index of diagonal in this%diagonals
  logical                   ::  padded !Whether the current value is padded or not
  integer, dimension(:), allocatable                    ::  distancetemp  !Temporary version of distance array
  real(kind=dp), dimension(:,:), allocatable            ::  valuestemp  !Temporary version of the values array
  integer                                     ::  jj !A loop variable

  !Find the diagonal of the matrix to be modified
  diagonal=column-row

  !Find out whether the diagnoals exist and, if so, what their references are
  call this%find_diag_ref(diagonal, diagref, padded)

  if (padded)then
    !The case where the diagonal is not explicitly stored in the matrix
    !If the values array of the cds is allocated then store the value array of the cds into the temporary array
    if (allocated(this%values)) then
      allocate(valuestemp(this%ndiag, this%n_row))
      valuestemp=this%values
      !Reallocate the value array and put the values back in
      deallocate(this%values)
    end if

    !Change values so it is the correct value in previously defined entries and zero in new diagonals
    allocate(this%values(this%ndiag+1,this%n_row))
    if(allocated(valuestemp))this%values(1:this%ndiag,:)=valuestemp
    this%values(this%ndiag+1:this%ndiag+1,:)=0.0_dp

    !Now do the same for distance
    if (allocated(this%distance)) then
      allocate(distancetemp(this%ndiag))
      distancetemp=this%distance
      !Reallocate the value array and put the values back in
      deallocate(this%distance)
    end if

    !Change distance so it is the correct value in previously defined entries and zero for new diagonals which are at the end
    allocate(this%distance(this%ndiag+1))
    if(allocated(distancetemp))this%distance(1:this%ndiag)=distancetemp
    this%distance(this%ndiag+1:this%ndiag+1)=0

    !Deallocate the temporary arrays
    if (allocated(distancetemp)) deallocate(distancetemp)
    if (allocated(valuestemp)) deallocate(valuestemp)

    !Move the values and distances to make space for the new diagonal
    !First, find where the diagonal needs to be
    do jj=1, this%ndiag+1
      if (this%distance(jj).gt.diagonal .or. jj==this%ndiag+1) then
        !Now we have found where it should be inserted, insert the diagonal in distance and move the values array appropriately. Set the new diagonal to a value of zero for now.
        if (jj.ne.this%ndiag+1) then
          this%distance(jj+1:this%ndiag+1)=this%distance(jj:this%ndiag+1-1)
          this%values(jj+1:this%ndiag+1,:)=this%values(jj:this%ndiag+1-1,:)
        end if
        this%distance(jj)=diagonal
        this%values(jj,:)=0.0_dp
        diagref = jj
        exit
      end if
    end do

    !Increase ndiag to represent the new diagonal
    this%ndiag = this%ndiag + 1
  end if

  !Find the relevant diagonal for the new value and insert it
  this%values(diagref,row)=value

  !Remove any zero diagonals which have been created
  call this%remove_zero()

end subroutine

!----------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------

subroutine operate_cds(this, vector_in, vector_out)
  class(t_cds), intent(in)                    ::  this !The matrix to multiply the vector by
  real(kind=dp), dimension(:), intent(in)     ::  vector_in  !The vector to be multiplied
  real(kind=dp), dimension(:), intent(inout)  ::  vector_out  !The vector which results from the multiplication
  integer                                     ::  column !Temporarily stores the column of a variable
  integer                                     ::  ii, jj  !Generic counting variables

  !First, check if the vector to be multiplied has the same number of rows as the matrix
  if (size(vector_in).ne.this%n_row) then
    print*, "cds_multiply_vector has been given a vector of size ", size(vector_in), " to multiply which is of a different size to the matrix which is of size ", this%n_row, ". Terminating."
    stop
  end if

  !Set the output to zero start with, then add up all contributions
  vector_out=0.0_dp
  do ii=1, this%n_row
    do jj=1, this%ndiag
      !Caluculate column of element to see if the element is within the matrix
      column=this%distance(jj)+ii
      if (column.gt.0 .and. column .le. this%n_row)then
        !Perform the multiplication
        vector_out(ii)=vector_out(ii)+this%values(jj,ii)*vector_in(column)
      end if
    end do
  end do

end subroutine operate_cds

!----------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------

subroutine cds_find_diag_ref(this, diagonal, diagref, padded)!A subroutine which returns whether the diagonal is padded and, if not, what the reference of the diagonal is.

  class(t_cds), intent(in) ::  this !The matrix whose values are to be returned
  integer                       ::  diagonal !The location of the diagonal whose reference is to be found
  integer, intent(out)          ::  diagref  !The first array index of the value to be returned in the values array of the t_cds type
  integer                       ::  ii  !A generic counting variable
  logical, intent(out)          ::  padded  !Will be set to false if the value is not padded

  !Initially assume the variable is in a padded cell
  padded=.true.
  !Find out which diagonal stored in t_cds type the value to bre returned is and turn padded to false. If
  do ii=1, this%ndiag
    if (diagonal==this%distance(ii)) then
      padded=.false.
      diagref=ii
      exit
    else if (diagonal.lt.this%distance(ii)) then
      exit
    end if
  end do

  !If the diagnoal is padded then return the dummy value -666 as the diagonal value
  if(padded) diagref=-666

end subroutine

!----------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------

subroutine cds_remove_zero(this) !This subroutine removes any diagonals which contain only zeros
  class(t_cds), intent(inout)            ::  this !The matrix to be modified
  integer                                     ::  ndiagtemp !Temporary value of nubmer of non-zero diagonals
  logical                                     ::  allzero !False if a diagonal has a non-zero entry
  integer                                     ::  lower, upper  !Lower and upper locations of
  integer, dimension(:), allocatable          ::  distancetemp  !Temporary storage for distance variable
  real(kind=dp), dimension(:,:), allocatable  ::  valuestemp  !Temporary storage for values variable
  integer, dimension(:), allocatable          ::  diagdelref !The references of diagonals to be deleted
  integer                                     ::  ndel  !The number of diagonals to delete
  integer                                     ::  ii,jj !Generic counting variables

  allocate(diagdelref(this%ndiag))

  !Check if there are zero diagonals. If so, return.
  if (this%ndiag==0) return

  !Initially set ndiagtemp to ndiag and reduce it as zero diagonals are found
  ndiagtemp=this%ndiag
  diagdelref=0
  ndel=0

  !Set allzero to true and then loop through each diagonal, setting it to flase if a non-zero entry is found
  do ii=1, this%ndiag
    allzero=.true.
    if (this%distance(ii).ge.0) then
      lower=1
      upper=this%n_row-this%distance(ii)
    else
      lower=1-this%distance(ii)
      upper=this%n_row
    end if
      do jj=lower, upper
        if (this%values(ii,jj).ne.0.0_dp) then
          allzero=.false.
          exit
        end if
      end do
    !If the diagonal only contains zeros then make a note of it and reduce ndiagtemp
    if (allzero) then
      ndel=ndel+1
      diagdelref(ndel)=ii
      ndiagtemp=ndiagtemp-1
    end if
  end do

  !If a diagonal is to be deleted then shift the distance and values arrays into its position instead, if necessary
  do ii=1, ndel-1
    this%distance(diagdelref(ii)-ii+1:diagdelref(ii+1)-ii-1)=this%distance(diagdelref(ii)+1:diagdelref(ii+1)-1)
    this%values(diagdelref(ii)-ii+1:diagdelref(ii+1)-ii-1,:)=this%values(diagdelref(ii)+1:diagdelref(ii+1)-1,:)
  end do
  !Finally, do it for the final diagonal to be deleted, if it is not the final diagonal of the input matrix
  if (ndel.gt.0) then
    if(diagdelref(ndel).ne.this%ndiag) then
      this%distance(diagdelref(ndel)-ndel+1:this%ndiag-ndel)=this%distance(diagdelref(ndel)+1:this%ndiag)
      this%values(diagdelref(ndel)-ndel+1:this%ndiag-ndel,:)=this%values(diagdelref(ndel)+1:this%ndiag,:)
    end if
  end if

!  !Finally, if necessary, place the values and distance variables into temporary arrays before real(kind=dp)locating the main array and insering variables
  if (this%ndiag.ne.ndiagtemp)then
    this%ndiag=ndiagtemp
    allocate(distancetemp(ndiagtemp), valuestemp(ndiagtemp,this%n_row))
    distancetemp=this%distance(1:ndiagtemp)
    valuestemp=this%values(1:ndiagtemp,:)
    deallocate(this%distance, this%values)
    allocate(this%distance(this%ndiag), this%values(this%ndiag,this%n_row))
    this%distance=distancetemp
    this%values=valuestemp
  end if

end subroutine

end module CDS_Mod