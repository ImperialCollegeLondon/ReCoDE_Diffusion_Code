# 2 - Object-Oriented Programming

## Introduction

Object-Oriented Programming (OOP) is a method of coding by which a piece of software is designed around the different required data types and classes which will be used in the programme. In its simplest sense, this translates to a code which is made of multiple different modules which make use of one another, rather than one large block of code. This style of programming is particularly advantageous when working with large coding projects, such as those used in research or data science. OOP codes are also much easier to read once their structure has been understood. As such, this section of the readme will give an explanation of how the Diffusion Code project is structured.

At its simplest level, the code reads a specified problem from an input file, converts that to a system of equations which can then be solved, and outputs the resulting data to a set of files which can be read by an external program such a GNUPlot or ParaView.

$$ \text{Input File } \rightarrow \text{Generate Equations } \rightarrow \text{Solve } \rightarrow \text{Output File } $$

This explanation can now be further expanded in terms of complexity, where the structure will be given in terms files and modules. For the sake of readability, this is given as a full flow chart below. The `Problem` module reads through the input file, storing relevant data or passing it to the `Materials` module. Data from these modules is the used by the `MatGen` module to generate the system of equations. If PETSc is used, this data is then passed to the `PETScMat` and `PETScVec` modules, which are wrappers for the data library. These are then passed into the `PETScKSP` module which solves the problem. If PETSc isn't used, this data is passed into the `CRS` module, which stores the data efficiently, such that it can be fed into the `Solver` module. The solved data is then passed into the `Output` module, which generates an output both in .txt and .vtu format.

![DiffCode drawio](https://user-images.githubusercontent.com/83182489/173537965-ac15206a-dc13-4659-89f0-5b7141eb3091.png)

This can also be represented through some blocks of pseudocode

First read through the input file

```fortran
Open( Input File)
Read Problem Data
Read Material Data
Close( Input File)

Set Problem Data
Set Material Data
```

Then generate the equations and solve for the flux

```fortran
Get Problem Data
Get Material Data

do i = 1, Problem Size
  Calculate Matrix Value
  Calculate Vector Value
end do

if (PETSc Used) then
  do i = 1, Problem Size
    Fill PETSc Matrix
    Fill PETSc Vector
  end do
  Flux = PETScSolver( PETSc Matrix, PETSc Vector)
else
  do i = 1, Problem Size
    Fill CRS Matrix
    Fill Vector
  end do
  Flux = Solver( CRS Matrix, Vector)
end if
```

Finally generate the output files

```fortran
open( Output File)
do i = 1, Problem Size
  write( Output File) Position, Flux
  write( Output File) Region Number
  write( Output File) Node Number
end do
close( Output File)
```

## OOP in Fortran

As discussed previously in the [Introduction](#introduction) section, this code utilises Object Oriented Programming. An example of such an abject orented structure can be seen in the code snippet below. This shows some of the `Materials` module, which handles the storage of data pertaining the material properties of the problem.

```fortran
module Materials_Mod

    use Constants_Mod
    !!Stores standard material data and material data explicitly set via an input file

    implicit none

  type, public :: t_material
        private
        real(kind=dp) :: Sig_a, S
        character(len=20) :: Name
    contains
    !!Procedures which handle the storing, calculation and retrieval of material data
        procedure, public :: SetName => SetMaterialName
        procedure, public :: GetName => GetMaterialName
        procedure, public :: SetProps => SetMaterialProperties
        procedure, public :: GetSig_a
        procedure, public :: GetS
  end type
```

We first define the name of the module such that it can be used by other modules in our code.

```fortran
module Materials_Mod
```

We then tell our module to make use of the `Constants` module.

```fortran
use Constants_Mod
```

This method of declaring the name of this specific module and the modules which this code can be seen throughout our various pieces of code. We would like to store some data within this module such that it can be called forth at a later date. As such, we set up a public type which contains our chosen private data.

```fortran
type, public :: t_material
        private
        real(kind=dp) :: Sig_a, S
        character(len=20) :: Name
```

We can then also choose what public procedures this module should utilise, which other pieces of code can then utilise. The most common routines you will often utilise in a code are 'gets' and 'sets' which pull stored data and set stored data respectively. In fortran you can also utilise pointers to point to certain pieces of memory. A nice example of this can be seen in the code block below where `SetName` points to `SetMaterialName` allowing us to use quick subroutine names when repeatedly calling a routine, but verbose names within the actual module.

```fortran
      procedure, public :: SetName => SetMaterialName
      procedure, public :: GetName => GetMaterialName
      procedure, public :: SetProps => SetMaterialProperties
      procedure, public :: GetSig_a
      procedure, public :: GetS
end type
```
