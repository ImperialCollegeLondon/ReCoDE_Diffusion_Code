Module Output_Mod

    use Constants_Mod
    use Problem_Mod
    use Materials_Mod
    !!Handles the data generated by solver routines, generating data readable by paraview etc
    !!Any other postprocessing of data would also go here

    type, public :: t_output

    contains
        procedure, public :: GenerateVTU
    end type
contains

Subroutine GenerateVTU(this,Problem,Flux)
    Implicit None
    class(t_output) :: this
    type(t_problem) :: Problem 
    Integer :: ii, jj, N_Nodes, N_Regions, NodeID
    Integer, allocatable, dimension(:) :: RegionNodes
    Integer, parameter :: textfile = 201, vtufile = 202 
    Real(kind=dp) :: Position
    Real(kind=dp), allocatable, dimension(:) :: Flux, Boundary_Pos
    !!Generate the VTU file using the resultand flux sorted in the PETSc vector x

    !!Extract relevant problem information
    N_Regions = Problem%GetN_Regions()
    Allocate(RegionNodes(N_Regions),Boundary_Pos(N_Regions))
    Boundary_Pos = Problem%GetBoundary_Pos()
    RegionNodes = Problem%GetNodes()
    N_Nodes = Problem%GetN_Nodes()

    !!Generate textfile
    Open(textfile,File='OutputFile.txt',Status='Replace')
    Position = 0._dp
    !!First node
    NodeID = 1
    Write(textfile,'(2E14.6)') Position, Flux(NodeID)
    Do ii = 1, N_Regions
        Do jj = 1, RegionNodes(ii)-1
            NodeID = NodeID + 1
            Position = Position + (Boundary_Pos(ii+1)-Boundary_Pos(ii))/Real(RegionNodes(ii)-1,dp)
            Write(textfile,'(2E14.6)') Position, Flux(NodeID)
        EndDo 
    EndDo
    Close(textfile)


    !!Generate VTU file
    Open(vtufile,File='OutputFile.vtu',Status='Replace')

    !!Describe the format
    Write(vtufile,'(g0)') "<?xml version=""1.0""?>"
    Write(vtufile,'(g0)') "<VTKFile type=""UnstructuredGrid"" version=""0.1"" byte_order=""LittleEndian"">"
    Write(vtufile,'(g0)',advance='no') "  "
    Write(vtufile,'(g0)') "<UnstructuredGrid>"

    !!Describe the amount of data
    Write(vtufile,'(g0)',advance='no') "    "
    Write(vtufile,'(g0)',advance='no') '<Piece NumberOfPoints="'
    Write(vtufile,'(g0)',advance='no') N_Nodes*2
    Write(vtufile,'(g0)',advance='no') '" NumberOfCells="'
    Write(vtufile,'(g0)',advance='no') N_Nodes-1
    Write(vtufile,'(g0)') '">'

    !!Fill in the point data
    Write(vtufile,'(g0)',advance='no') "      "
    Write(vtufile,'(g0)') '<PointData Scalars="flux">'

    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') '<DataArray Name="flux" format="ascii" type="Float64" >'
    Write(vtufile,'(g0)',advance='no') "          "
    !!Flux values
    Do ii = 1, N_Nodes
        Write(vtufile,'(E14.6)',advance='no') Flux(ii)
        Write(vtufile,'(g0)',advance='no') " "
    EndDo
    !!Copy of flux to be smeared in y
    Do ii = 1, N_Nodes
        Write(vtufile,'(E14.6)',advance='no') Flux(ii)
        Write(vtufile,'(g0)',advance='no') " "
    EndDo
    Write(vtufile,'(g0)') ""
    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') "</DataArray>"

    Write(vtufile,'(g0)',advance='no') "      "
    Write(vtufile,'(g0)') "</PointData>"


    !!Cell data section 
    Write(vtufile,'(g0)',advance='no') "      "
    Write(vtufile,'(g0)') '<CellData Scalars="Region, Cell">'

    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') '<DataArray Name="Region" format="ascii" type="Int32" >'
    Write(vtufile,'(g0)',advance='no') "          "

    !!Loop over the problem to find the region number
    Do ii = 1, N_Regions
        Do jj = 1, RegionNodes(ii)-1
            Write(vtufile,'(g0)',advance='no') ii
            Write(vtufile,'(g0)',advance='no') " "
        EndDo    
    EndDo
    Write(vtufile,'(g0)') ""
    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') "</DataArray>"

    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') '<DataArray Name="Cell" format="ascii" type="Int32" >'
    Write(vtufile,'(g0)',advance='no') "          "

    Do ii = 1, N_Nodes-1
        Write(vtufile,'(g0)',advance='no') ii
        Write(vtufile,'(g0)',advance='no') " "
    EndDo
    Write(vtufile,'(g0)') ""
    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') "</DataArray>"
    

    Write(vtufile,'(g0)',advance='no') "      "
    Write(vtufile,'(g0)') "</CellData>"


    !!Fill in the point data
    Write(vtufile,'(g0)',advance='no') "      "
    Write(vtufile,'(g0)') "<Points>"
    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') "<DataArray Name=""Coordinates"" NumberOfComponents=""3"" format=""ascii"" type=""Float64"" >"
    Write(vtufile,'(g0)',advance='no') "          "
    !!1 dimensional flux results
    Position = 0._dp
    Do ii = 1, N_Regions
        Do jj = 1, RegionNodes(ii)-1
            Write(vtufile,'(E14.6)',advance='no') Position
            Write(vtufile,'(g0)',advance='no') " 0.000000E+00 0.000000E+00 "
            Position = Position + (Boundary_Pos(ii+1)-Boundary_Pos(ii))/Real(RegionNodes(ii)-1,dp)
        EndDo 
    EndDo
    !!Final Node
    Write(vtufile,'(E14.6)',advance='no') Boundary_Pos(N_Regions+1)
    Write(vtufile,'(g0)',advance='no') " 0.000000E+00 0.000000E+00 "

    !!Copy of results smeared in y for vtu visualisation
    Position = 0._dp
    Do ii = 1, N_Regions
        Do jj = 1, RegionNodes(ii)-1
            Write(vtufile,'(E14.6)',advance='no') Position
            Write(vtufile,'(g0)',advance='no') " 0.100000E+01 0.000000E+00 "
            Position = Position + (Boundary_Pos(ii+1)-Boundary_Pos(ii))/Real(RegionNodes(ii)-1,dp)
        EndDo 
    EndDo
    !!Final Node
    Write(vtufile,'(E14.6)',advance='no') Boundary_Pos(N_Regions+1)
    Write(vtufile,'(g0)',advance='no') " 0.100000E+01 0.000000E+00 "
    Write(vtufile,'(g0)') ""
    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') "</DataArray>"
    Write(vtufile,'(g0)',advance='no') "      "
    Write(vtufile,'(g0)') "</Points>"


    !!Fill in the cell data
    Write(vtufile,'(g0)',advance='no') "      "
    Write(vtufile,'(g0)') "<Cells>"
    !!Connectivity
    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') '<DataArray type="Int32" Name="connectivity" format="ascii">'
    Write(vtufile,'(g0)',advance='no') "          "
    Do ii = 1, N_Nodes-1
        Write(vtufile,'(g0)',advance='no') ii-1
        Write(vtufile,'(g0)',advance='no') " "
        Write(vtufile,'(g0)',advance='no') ii
        Write(vtufile,'(g0)',advance='no') " "
        Write(vtufile,'(g0)',advance='no') ii+N_Nodes
        Write(vtufile,'(g0)',advance='no') " "
        Write(vtufile,'(g0)',advance='no') ii-1+N_Nodes
        Write(vtufile,'(g0)',advance='no') " "
    EndDo
    Write(vtufile,'(g0)') ""
    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') " </DataArray>"
    !!Offsets
    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') '<DataArray type="Int32" Name="offsets" format="ascii">'
    Write(vtufile,'(g0)',advance='no') "          "
    Do ii = 1, N_Nodes-1
        Write(vtufile,'(g0)',advance='no') ii*4
        Write(vtufile,'(g0)',advance='no') " "
    EndDo
    Write(vtufile,'(g0)') ""
    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') " </DataArray>"
    !!Types
    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)',advance='no') "  "
    Write(vtufile,'(g0)') '<DataArray type="Int32" Name="types" format="ascii">'
    Write(vtufile,'(g0)',advance='no') "          "
    Do ii = 1, N_Nodes-1
        Write(vtufile,'(g0)',advance='no') 5
        Write(vtufile,'(g0)',advance='no') " "
    EndDo
    Write(vtufile,'(g0)') ""
    Write(vtufile,'(g0)',advance='no') "        "
    Write(vtufile,'(g0)') " </DataArray>"

    Write(vtufile,'(g0)',advance='no') "      "
    Write(vtufile,'(g0)') "</Cells>"
    Write(vtufile,'(g0)',advance='no') "    "
    Write(vtufile,'(g0)') '</Piece>'
    Write(vtufile,'(g0)',advance='no') "  "
    Write(vtufile,'(g0)') "</UnstructuredGrid>"
    Write(vtufile,'(g0)') "</VTKFile>"

    Close(vtufile)
    Deallocate(Flux)
End Subroutine GenerateVTU

End Module
