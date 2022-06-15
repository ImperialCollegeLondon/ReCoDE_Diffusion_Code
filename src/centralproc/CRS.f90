Module CRS_Mod

    use Constants_Mod
    !!Compressed row storage module which handles the optimal data storage of sparse matrices for the problem
    Implicit None

    private

    type, public :: t_CRS
        private
        integer :: length
        Real(kind=dp), allocatable, dimension(:,:) :: value_col_array
        Integer, allocatable, dimension(:) :: row_ptr_array

    contains
        procedure, public :: construct => construct_CRS
        procedure, public :: input => input_CRS
        procedure, public :: insert => insert_CRS
        procedure, public :: get => get_CRS
        procedure, public :: get_N_rows
        procedure, public :: get_N_elements
        procedure, public :: operate => operate_CRS
        procedure, public :: destroy => destroy_CRS
        procedure, public :: print_CRS
    end type

contains


Subroutine construct_CRS(this, N_SIZE)
    class(t_CRS), intent(inout) :: this
    Integer :: N_SIZE, jj 
    !!Construct a blank CRS matrix of known size

    Allocate(this%value_col_array(N_SIZE,2))
    Allocate(this%row_ptr_array(N_SIZE))

    !!Generates a diagonal matrix with a value of 1. in each filled position
    Do jj = 1, N_SIZE
        this%value_col_array(jj,1) = 1.0_dp
        this%value_col_array(jj,2) = Real(jj,dp)
    EndDo

    Do jj = 1, N_SIZE
        this%row_ptr_array(jj) = jj
    EndDo

End subroutine construct_CRS



Subroutine input_CRS(this, row_ptr, val, col_ind)
    Class(t_CRS), intent(inout) :: this
    Integer, dimension(:) :: row_ptr
    Real(kind=dp), dimension(:) :: val, col_ind
    !!Insert a fully described CRS set for storage. Generally used for quick debugging/testing

    Deallocate(this%value_col_array,this%row_ptr_array)
    this%length = Size(row_ptr)
    Allocate(this%value_col_array(Size(val),2),this%row_ptr_array(Size(row_ptr)))

    this%row_ptr_array(:) = row_ptr(:)
    this%value_col_array(:,1) = val(:)
    this%value_col_array(:,2) = col_ind(:)

End Subroutine input_CRS



Subroutine insert_CRS(this, Matrix_j, Matrix_i, Matrix_ij_value)
    Class(t_CRS), intent(inout) :: this
    Integer, intent(in) :: Matrix_i, Matrix_j
    Real(kind=dp), intent(in) :: Matrix_ij_value
    Real(kind=dp)  :: Input_Value
    Integer  :: Row_Start_N, Row_End_N, i, j, New_ValCol_Length, Input_Row, Input_Column
    Real(kind=dp), allocatable, dimension(:,:) :: New_Row_ValCol_Array, New_ValCol_array
    Logical  :: Sparsity_Change_Logical

    Input_Row = Matrix_j
    Input_Column = Matrix_i
    Input_Value = Matrix_ij_value

    !!Recreate the matrix such that individual values can be tested
    Row_Start_N = this%row_ptr_array(Input_Row)
    If (Input_Row .EQ. Size(this%row_ptr_array)) Then
        Row_End_N = Size(this%value_col_array,1)
    Else
        Row_End_N = this%row_ptr_array(Input_Row+1)-1
    EndIf

    !!Check if value being replaced is zero or non-zero
    Sparsity_Change_Logical = .TRUE.
    Do j = Row_Start_N, Row_End_N
        If ((this%value_col_array(j,2) .EQ. Input_Column) .AND. (Input_Value .NE. 0))Then
            Sparsity_Change_Logical = .FALSE.
            this%value_col_array(j,1) = Input_Value
        EndIf
    EndDo

    If (Sparsity_Change_Logical .EQV. .TRUE.) Then
        If (ABS(Input_Value) .GT. 1E-9) Then
            ! Write(*,*) "Non-Zero Inserted"
            New_ValCol_Length = Size(this%value_col_array,1) + 1
            Allocate(New_ValCol_array(New_ValCol_Length,2))
            Allocate(New_Row_ValCol_Array(Row_End_N-Row_Start_N+2,2))
            j = Row_Start_N
            i = 1
            Do While ((this%value_col_array(j,2) .LT. Input_Column) .AND. (i .LT. (Row_End_N-Row_Start_N+2)))
            New_Row_ValCol_Array(i,:) = this%value_col_array(j,:)
            j = j + 1
            i = i + 1
            EndDo

            New_Row_ValCol_Array(i,1) = Input_Value
            New_Row_ValCol_Array(i,2) = Input_Column
            i = i + 1

            Do While (j .LT. Row_End_N+1)
                New_Row_ValCol_Array(i,:) = this%value_col_array(j,:)
                j = j + 1
                i = i + 1
            EndDo

            !!Copy over old values from before modified row
            Do j = 1, (Row_Start_N-1)
                New_ValCol_array(j,:) = this%value_col_array(j,:)
            EndDo

            !!Copy over values from newly modified row
            i = 0
            Do j = Row_Start_N, (Row_End_N+1)
                i = i + 1
                New_ValCol_array(j,:) = New_Row_ValCol_Array(i,:)
            EndDo

            !!Copy over old values from after old modified row
            Do j = (Row_End_N+2), New_ValCol_Length
                New_ValCol_array(j,:) = this%value_col_array(j-1,:)
            EndDo

            !!Deallocate old ValCol array and allocate the new one with increased size
            Deallocate(this%value_col_array)
            Allocate(this%value_col_array(New_ValCol_Length,2))

            !!Copy over new values into the CRS array
            Do j = 1, New_ValCol_Length
                this%value_col_array(j,:) = New_ValCol_array(j,:)
            EndDo

            !!Modify the row_ptr array to account for the additional value (unless at final row)
            If (Input_Row .NE. Size(this%row_ptr_array,1))Then
                Do j = (Input_Row+1), Size(this%row_ptr_array,1)
                    this%row_ptr_array(j) = this%row_ptr_array(j) + 1
                EndDo
            EndIf

        Else
            !!Value is zero therefore remove a value

            New_ValCol_Length = Size(this%value_col_array,1) - 1
            Allocate(New_ValCol_array(New_ValCol_Length,2))
            Allocate(New_Row_ValCol_Array(Row_End_N-Row_Start_N,2))

            j = Row_Start_N
            i = 1
            Do While (this%value_col_array(j,2) .LT. Input_Column)
                New_Row_ValCol_Array(i,:) = this%value_col_array(j,:)
                j = j + 1
                i = i + 1
            EndDo

            i = i - 1

            Do While (j .LT. Row_End_N)
                j = j + 1
                i = i + 1
                New_Row_ValCol_Array(i,:) = this%value_col_array(j,:)
            EndDo

            !!Copy over old values from before modified row
            Do j = 1, (Row_Start_N-1)
                New_ValCol_array(j,:) = this%value_col_array(j,:)
            EndDo

            !!Copy over values from newly modified row
            i = 0
            Do j = Row_Start_N, Row_End_N-1
                i = i + 1
                New_ValCol_array(j,:) = New_Row_ValCol_Array(i,:)
            EndDo

            !!Copy over old values from after old modified row
            Do j = (Row_End_N), New_ValCol_Length
                New_ValCol_array(j,:) = this%value_col_array(j+1,:)
            EndDo

            !!Deallocate old ValCol array and allocate the new one with increased size
            Deallocate(this%value_col_array)
            Allocate(this%value_col_array(New_ValCol_Length,2))

            !!Copy over new values into the CRS array
            Do j = 1, New_ValCol_Length
                this%value_col_array(j,:) = New_ValCol_array(j,:)
            EndDo

            !!Modify the row_ptr array to account for the additional value (unless at final row)
            If (Input_Row .NE. Size(this%row_ptr_array,1))Then
                Do j = (Input_Row+1), Size(this%row_ptr_array,1)
                    this%row_ptr_array(j) = this%row_ptr_array(j) - 1
                EndDo
            EndIf

        EndIf
    EndIf

End Subroutine insert_CRS


Function get_CRS(this, Matrix_j, Matrix_i) Result(Matrix_ij_value)
    Class(t_CRS), intent(inout) :: this
    Integer :: Matrix_i, Matrix_j, Row_Start_Element, Row_End_Element, j
    Real(kind=dp) :: Matrix_ij_value
    Logical :: Value_Get_Logical

    !!Returns the value of the matrix at a specified row and column

    !!Check if the row is the final one in the matrix
    If (Matrix_j .EQ. Size(this%row_ptr_array)) Then
        Row_End_Element = Size(this%value_col_array,1)
    Else
        Row_End_Element = this%row_ptr_array(Matrix_j+1) - 1
    EndIf

    Row_Start_Element = this%row_ptr_array(Matrix_j)

    !!Loop over the whole row to find the column
    Value_Get_Logical = .FALSE.
    Do j = Row_Start_Element, Row_End_Element
        If (this%value_col_array(j,2) .EQ. Matrix_i) Then
            Value_Get_Logical = .TRUE.
            Matrix_ij_value = this%value_col_array(j,1)
        EndIf
    EndDo

    If (Value_Get_Logical .EQV. .FALSE.) Then
        Matrix_ij_value = 0.0_dp
    EndIf

End Function get_CRS



Function get_N_rows(this) Result(res)
    Class(t_CRS), intent(inout) :: this
    Integer :: res
    !!Get the number of rows in the problem

    res = Size(this%row_ptr_array)

End Function get_N_rows


Function get_N_elements(this) Result(res)
    Class(t_CRS), intent(inout) :: this
    Integer :: res
    !!Get the number of elements in the problem

    res = Size(this%value_col_array,1)

End Function get_N_elements


Subroutine operate_CRS(this, Input_Operate_array, Output_Operate_array)
    Class(t_CRS), intent(inout) :: this
    Real(kind=dp), dimension(Size(this%row_ptr_array)), intent(in) :: Input_Operate_array
    Real(kind=dp), dimension(Size(this%row_ptr_array)), intent(out) :: Output_Operate_array
    Integer :: i, j, N_Row_Elements, Row_Element, Column_Element
    !!Multiply the CRS matrix by another input matrix
    
    ! !!Loops over each row
    Do i = 1, Size(this%row_ptr_array)
        !!Calculates the number of elements in each row
        If (i .EQ. Size(this%row_ptr_array)) Then
            N_Row_Elements = Size(this%value_col_array,1)-this%row_ptr_array(i)+1
        Else
            N_Row_Elements = this%row_ptr_array(i+1) - this%row_ptr_array(i)
        EndIf

        Output_Operate_array(i) = 0.0_dp
        Do j = 1, N_Row_Elements
            !!Numbers the specific element for legibility
            Row_Element = this%row_ptr_array(i) + (j-1)
            !!Multiply by the relevant matrix value to get the new values
            Column_Element = NINT(this%value_col_array(Row_Element,2))
            Output_Operate_array(i) = Output_Operate_array(i) + (this%value_col_array(Row_Element,1)*Input_Operate_array(NINT(this%value_col_array(Row_Element,2))))
        EndDo
    EndDo

End Subroutine operate_CRS



Subroutine destroy_CRS(this)
    class(t_CRS), intent(inout) :: this
    !!Destroy the data stored within the CRS module 

    Deallocate(this%value_col_array)
    Deallocate(this%row_ptr_array)

End subroutine destroy_CRS


Subroutine print_CRS(this)
    class(t_CRS) :: this
    Write(*,*) "-------------"
    Write(*,*) "Val:", this%value_col_array(:,1)
    Write(*,*) "Col:", this%value_col_array(:,2)
    Write(*,*) "Row:", this%row_ptr_array
    Write(*,*) "-------------"
End subroutine print_CRS


End Module
