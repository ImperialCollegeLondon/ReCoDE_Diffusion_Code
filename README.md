# ReCoDE Diffusion Code

# Table of Contents
- [Abstract](#abstract)
- [Structure of the Code](#structure-of-the-code)
- [User Guide](#user-guide)
- [Features of the Code](#features-of-the-code)
- [Exercises](#exercises)
- [PETSc Installation](#petsc-installation)
- [Theory](#theory)

# Abstract
This code is part of the Research Computing and Data Science Examples (ReCoDE) project. The code itself is a 1-dimensional neutron diffusion solver written in Fortran in an object oriented format. The example will focus on features of the code that can be used as a teaching aid to give readers some experience with the concepts such that they can implement them in the exercises or directly in their own codes. An understanding of neutron diffusion and reactor physics is not required for this example, but a discussion of the theory can be found in the bottom section of this readme.

This project aims to provide examples of:
- [Compiled Codes]()
- [Object Oriented Programming](##object-oriented-programming) (OOP)
- [Makefiles]()
- [Compiler Directives]()
- [Reading input data from file](##Reading-input-data-from-file)
- [Generating output files]()
- [Paraview]()
- [Solving mathematical problems]()
- [Discretisation of a spatial dimension]()
- [Optimised data storage and solvers]()
- [Incorporating external libraries (PETSc)]()


# Structure of the Code
Object Oriented Programming is a method of coding by which a piece of software is designed around the different required data types and classes which will be used in the programme. In it's simplest sense, this translates to a code which is made of multiple different modules which make use of one another, rather than one large block of code. This style of programming is particularly advantageous when working with large coding projects, such as those used in research or data science. Object oriented codes are also much easier to read once their structure has been understood. As such, this section of the readme will give an explanation of how the Diffusion Code project is structured.

At it's simplest level, the code reads a specified problem from an input file, converts that to a system of equations which can then be solved, and outputs the resulting data to a set of files which can be read by an external program such a GNUPlot or Paraview.
$$ \text{Input File } \rightarrow \text{Generate Equations } \rightarrow \text{Solve } \rightarrow \text{Output File } $$

This explanation can now be further expanded in terms of complexity, where the structure will be given in terms files and modules. For the sake of readability, this is given as a full flow chart below. The **Problem** module reads through the input file, storing relevant data or passing it to the **Materials** module. Data from these modules is the used by the **MatGen** module to generate the system of equations. If PETSc is used, this data is then passed to the **PETScMat** and **PETScVec** modules, which are wrappers for the data library. These are then passed into the **PETScKSP** module which solves the problem. If PETSc isn't used, this data is passed into the **CRS** module, which stores the data effieicently, such that it can be fed into the **Solver** module. The solved data is then passed into the **Output** module, which generates an output both in .txt and .vtu format.

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

Do 1, Problem Size
  Calculate Matrix Value
  Calculate Vector Value
EndDo

If (PETSc Used) Then
  Do 1, Problem Size
    Fill PETSc Matrix
    Fill PETSc Vector
  EndDo
  Flux = PETScSolver( PETSc Matrix, PETSc Vector)
Else
  Do 1, Problem Size
    Fill CRS Matrix
    Fill Vector
  EndDo
  Flux = Solver( CRS Matrix, Vector)
EndIf
```

Finally generate the output files
```fortran
Open( Output File)
Do 1, Problem Size
  Write( Output File) Position, Flux
  Write( Output File) Region Number
  Write( Output File) Node Number
EndDo
Close( Output File)
```

# User Guide
  ## Compiling the Code
  As fortran is a compiled language, the code must first be compiled before it can be executed. To do so, navigate to the **src** directory within the code and enter the commands:
  ```bash
  make clean
  make
  ```
  The **make clean** command first leans the directory of any module files or executables created by any past makes. This should generally be done before you compile the code with a new make option. The **make** command will execute the makefile contained within the same directory, converting the fortran code to an optimised form that can be read by your computer. This then generates an executable named **diffusion**. Once this has been done, the code can be executed from the parent directory using the command:
  ```bash
  ./diffusion
  ```
  This command tells the executable to run and will generate relevant output files containing the solution to the problem. 


  ## Changing Input Options
  The code is designed such that the user can easily change the problem which the code is attempting to solve. The code uses the input file **Input.txt** to read details about the problem, such as the positions of boundaries or materials in the problem. An example of such an input can be seen below:
  ```
  ------------------------------------------
  Solver: - Thomas, BCG or CG (only active for non-PETSc usage)
  BCG
  ------------------------------------------
  Regions: - Integer number of regions in the problem
  2
  ------------------------------------------
  Boundaries: - Real number positions of the boundaries between the regions (one per line)
  0.0
  0.5
  1.0
  ------------------------------------------
  Nodes: - Integer number of nodes in each region (one per line)
  5
  5
  ------------------------------------------
  Materials: - Fuel, Water or Steel (one per line)
  Fuel
  Fuel
  ------------------------------------------
  Boundary_Conditions: - Zero or Reflective (two parameters - one per line)
  Zero
  Zero
  ------------------------------------------
  ```
   For this example problem, we are stating that we have a geometry ranging from *x = 0.0* to *1.0*, half fuel and half steel with a central boundary at *x = 0.5*. As seen from the above input, the code needs four different parameters to be described to it.
  - **Solver** - This tells our code what solver we will be using. The specific types of solvers utilised are discussed later in the readme, but this can be left as BCG if you are not interested in specific solver routines. Note that CG should not be used unless both boundary conditions are set to 'Zero'. 
  - **Regions** - An integer number of regions that exists within the problem. We have 1 region from *0.0* to *0.5* and another from *0.5* to *1.0*, hence we give the code the integer number **2**.
  - **Boundaries** - The positions of the boundaries within the problem. Our first boundary is at the start of our geometry so we enter the number **0.0**. We then have an internal boundary halfway through the problem seperating the regions so we enter the number **0.5**. Finally we have the exterior boundary of our geometry, so we enter the number **1.0**. The code will always read one more value here than the number of regions in the problem.
  - **Nodes** - This desribes how refined we want the geometry in each region. For the example we want a quick solve with just enough nodes to see the flux profile. As we need to describe this for each region we enter the value **10** twice. The code will always read the same number of values here as the number of regions in the problem.
  - **Materials** - This described the materials that are present within the system. The first half of our geometry is fuel, with the latter half being Steel, so we enter **Fuel** and **Steel**. The code will always read the same number of values here as the number of regions in the problem.
  - **Boundary Conditions** - This tells the code what boundaries exist at the edges of our problem. Two boudnary conditions have been implemented in out code, that of 'Zero' and 'Reflective'. The former simply ensures that the flux will tend to zero at the boundary, while the latter ensures that the derivative of the flux will tend to zero at the boundary.
  ## Reading Output Files
  The code generates two output files, **Output.txt** and **Output.vtu**. The former is a simple text file containing the position and flux of the solution to the problem. These are simply given in two columns such that they can be read easily by someting like a GNUPlot or Python script. An example of such a flux profile can be seen below:
  ```
  0.000000E+00  0.135813E+01
  0.166667E+00  0.137306E+01
  0.333333E+00  0.141907E+01
  0.500000E+00  0.150000E+01
  0.666667E+00  0.158093E+01
  0.833333E+00  0.162694E+01
  0.100000E+01  0.164187E+01
  ```
  This can then be plotted using tools such as GNUPlot to produce the profile seen below:

  ![FluxProfile](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/images/FluxProfile.png)

  GNUPlot was chosen here as the application is very simple and the program itself is very easy to install. On Linux, you only need the commands:
  ```bash
  sudo apt update
  sudo apt install gnuplot
  ```
  For installation on Windows, you will need to download the software from http://www.gnuplot.info/download.html. The GNUPlot script used to generate the plot is named **fluxplot** and can be found in the home directory of the project. This can be run with the command:
  ```bash
  gnuplot -p fluxplot
  ```
  The  **Output.vtu** file stores additional data such as the region and cell numbers in an XML format. This can be directly read by software such as Paraview, allowing for more detailed visualisations than that of the previous flux profile. For visualisation purposes, the data has been smeared in a second dimension, which should give users an idea of how multi-dimensional cell data can be viewed. An example output from paraview can be seen in the image below:

  ![FluxParaview](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/images/FluxParaview.png)
  
  Instructions on how to install paraview can be found at: https://www.paraview.org/Wiki/ParaView:Build_And_Install

  A user guid for paraview can be found at: https://docs.paraview.org/en/latest/

# Features of the Code

## Object Oriented Programming

As discussed previously in the [Structure of the Code](#structure-of-the-code) section, this code utilises Object Oriented Programming. An example of such an abject orented structure can be seen in the code snippet below. This shows some of the **Materials** module, which handles the storage of data pertaining the material properties of the problem.

```Fortran
Module Materials_Mod

    use Constants_Mod
    !!Stores standard material data and material data explicitly set via an input file

    Implicit None

  type, public :: t_material
        private
        Real(kind=dp) :: Sig_a, S
        Character(len=20) :: Name
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

```Fortran
Module Materials_Mod
```
We then tell our module to make use of the **Constants** module.

```Fortran
use Constants_Mod
```
This method of declaring the name of this specific module and the modules which this code can be seen throughout our various pieces of code. We would like to store some data within this module such that it can be called forth at a later date. As such, we set up a public type which contains our chosen private data.

```Fortran
type, public :: t_material
        private
        Real(kind=dp) :: Sig_a, S
        Character(len=20) :: Name
```
We can then also choose what public procedures this module should utilise, which other pieces of code can then utilise. The most common routines you will often utilise in a code are 'gets' and 'sets' which pull stored data and set stored data respectively. In fortran you can also utilise pointers to point to certain pieces of memory. A nice example of this can be seen in the code block below where **SetName** points to **SetMaterialName** allowing us to use quick subroutine names when repeatedly calling a routine, but verbose names within the actual module.

```Fortran
      procedure, public :: SetName => SetMaterialName
      procedure, public :: GetName => GetMaterialName
      procedure, public :: SetProps => SetMaterialProperties
      procedure, public :: GetSig_a
      procedure, public :: GetS
end type
```







## Makefiles

Makefiles are particularly useful in a code that utilises OOP as it allows for easy compilation of the required files and handles the order of dependencies. The code snippet below comes from the makefile used in the project and shows how an ordered list of objects is used when compiling the code. This is written such that the compiler will work through the files in a way that each dependency is compiled before the modules that utilise it, ending with the main.

```Makefile
OBJS= stdlib/Constants.o \
 inoutproc/Materials.o \
 inoutproc/Problem.o \
 inoutproc/Output.o \
 centralproc/Matrix_Base.o \
 centralproc/CRS.o \
 centralproc/Solver.o \
 centralproc/MatGen.o \
 main.o \
```

The makefile can also facilitate compilation in a number of different ways depending on the options provided. The snippet below compiles the code when the command ```make debug``` is used on the command line, such that it uses all common flags set by **$(FC_FLAGS_COMMON)**. In addition, the flag ```-DDBEUG``` creates the compiler flag 'DEBUG'.
```Makefile
debug:   FC_FLAGS = $(FC_FLAGS_COMMON) -DDEBUG
```
This flag can then be checked with C pre-processor language to allow for differnet parts of the code to be compiled dependent on the flags incorporated. In the below example, a more detailed output is given by the **Solve** module when the code is compiled with ```DEBUG```.
```fortran
# ifdef DEBUG 
  Write(*,*) "---CG Convergence Succeeded---"
  Write(*,'(g0)',advance='no') "Succeeded after iterations:  "
  Write(*,'(g0)',advance='no') BCG_Iterations
  Write(*,'(g0)',advance='no') "  with residual:"
  Write(*,'(E14.6)') rho1
  Write(*,*) "-------------------------------"
# endif
```

## Reading input data from file

Scientific codes will often make use of an input file which contains the problem specification. These codes must therefore be able to open an input file and read through it, extracting the relevant data such that it can be used to solve the problem. This can be seen in the code snippet below taken from the **Problem** module, where an input file has been opened and the desired solver type read in.

```Fortran
!!Open input file containing problem specification
Open(InputFile, File='input.in', Status='Old')

!!Read in the solver type
String_Read = ''
Do While (String_Read .NE. 'Solver:')
  Read(InputFile,*) String_Read
EndDo
Read(InputFile,*) String_Read
If (String_Read == 'Thomas') Then 
  this%SolverID = 1
ElseIf (String_Read == 'BCG') Then 
  this%SolverID = 2
ElseIf (String_Read == 'CG') Then 
  this%SolverID = 3
Else 
  Write(*,*) "ERROR: Unrecognised Solver Type"
EndIf
```


When reading an input file, fortran needs an Integer ID, Filename and Status. The ID allows the file to be referred to easily in future, and is good practice to set through a parameter for readability. The Filename simply matches the name of the file, and the status describes the state of the file, in this case it is an 'Old' file which already exists in the directory.
```Fortran
Integer, parameter :: InputFile = 101
Open(InputFile, File='input.in', Status='Old')
```

We then wish to read through our file until we reach the name of the descired solver. To do so, we loop through our file until we reach the 'Solver:' string. We now know that the next line will contain the name of our solver, and hence this can be read in.
```Fortran
String_Read = ''
Do While (String_Read .NE. 'Solver:')
  Read(InputFile,*) String_Read
EndDo
```

We finally need to check what type of solver has been specified and store this information. Here we have an If statement which will loop over the known solver types.

```Fortran
Read(InputFile,*) String_Read
If (String_Read == 'Thomas') Then 
  this%SolverID = 1
ElseIf (String_Read == 'BCG') Then 
  this%SolverID = 2
ElseIf (String_Read == 'CG') Then 
  this%SolverID = 3
Else 
  Write(*,*) "ERROR: Unrecognised Solver Type"
EndIf
```

Finally we should close this file, achieved through the close command and the associated ID.

```Fortran
Close(InputFile)
```

- Storing data within a module

![StorageImg](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/images/StorageImg.png)

## Output Module

-  Writing data to an output file

![OutputImg](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/images/OutputImg.png)

-  Using paraview and vtk structure

![VTKImg](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/images/VTKImg.png)


## MatGen Module

- Discuss how a mathematical problem can be converted into something solveable

![MatGenImg](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/images/MatGenImg.png)

- Discretisation in space using data stored within other modules

![DiscretiseImg](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/images/DiscretiseImg.png)

## Compressed Row Storage Module

- How data storage can be optimised for efficiency

![CRSImg](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/images/CRSImg.png)

## Conjugate Gradient Module

- How you can write your own modules to solve systems of equations

![SolveImg](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/images/SolveImg.png)

- Brief aside on preconditioners

![PreCondImg](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/images/PreCondImg.png)

## One of the PETSc Modules

- Writing a wrapper for a library

![WrapperImg](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/images/WrapperImg.png)

- Compiling the code with PETSc

![PETScDirsImg](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/images/PETScDirsImg.png)

- Why we use external libraries



# Exercises
This section contains a number of suggested exercises that will give the user a deeper understanding of the topics covered in the above descriptions and utilised throughout this code. Solutions to the exercises can be found in the 'solutions' directory, where a version of the main code exists, modified such that it solves the problem. The exercises will increase progressively in complexity, with jumps in challenge roughly seperated by the numeric ID of the exercise.

## Exercise 1a

**Task:** Add a print statement to the **SetName** Subroutine in the Materials Module

**Aims:**

-  Gain an initial understanding of how a code can utilise an OOP structure

For this exercise, add a print (or write) statement to the **SetName** Subroutine contained within the Materials Module, which prints the name of the material being set to the terminal.

## Exercise 1b

**Task:** Write a new subroutine which prints all material data stored in the module

**Aims:** 

- Develop an understanding of Object Orientation
- Learn how to write a new subroutine and implement it in a class structure
- Learn how to utilise a type to call the generated routine
- Opportunity to gain experience with Fortran output formatting

For this exercise, you must make your own subroutine within the Materials Module called **PrintMaterial**. This subroutine, when called from the Main, should print all material data contained within the materials class to the terminal. This is also a good opportunity for users to gain some experience with Fortran formatting if they wish, as this can make the terminal output much easier to read.

## Exercise 1c

**Task:** Make a new material which can be used in the input deck

**Aims:** 

- Experience with fortran logical arguments, namely **If Statements**
- Initial practice adjusting some exisitng logic within a subroutine

For this exercise, a new material will be added to the list of those that the code can handle. The material 'Iron' will be added with an absoprtion cross section of *4.0* and source of *0.1*.




## Exercise 2a

**Task:** Get input deck to instead explicitly read in material data rather than associating with a name

**Aims:** 

- Experience making larger adjustments to the logic of the code
- Understanding of how to adjust various sections of a code to handle a modification
- Experience handling input files

Currently, the code reads in a material name and then assigns material properties to the region based on a set of data stored in an If statement. For this exercise, the code will instead read in the absorption and source terms directly, such that 'Fuel' would instead become '1.0 6.0'. To achieve this, the user will need to adjust the **Problem** module, as well as adjust how they have defined their problem in the **input.in** file.



## Exercise 2b

**Task:** Add another set of cell data to the .vtu file where the material properties of each cell can be viewed e.g. absorption 

**Aims:** 

- Gain experience writing data to an output file
- Learn how to utilise vtu files to add data sets to paraview

The current vtu file output containts the Flux, Cell Number and Region Number data sets. In this exercise, the user will add the Source term data set to the vtu output, such that the source value of each cell can be viewed in Paraview. An additional Cell Data set will need to be added which uses the known Region Numbers to pull data from the **Materials** Module.


## Exercise 3a

**Task:** Smear the problem into another dimension to produce a 3D paraview output

**Aims:** 

- Gain further experience writing data to an output file
- Learn how to utilise vtu files to generate more complex outputs in paraview

The main version of the code already smears a 1-dimensional flux profile into a second dimension for the sake of a visualisation example. This could be furthered by also smearing the results in a third dimension. To do so, the user will need to make an additional set of nodes that have been translated in the z axis and ensure that these are then associated with the correct cells. Users should utilise the paraview vtk format guide to do so, noting that they will now need to define the cells as a three dimensional shape. The vtk file format is described here: https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf



## Exercise 3b

**Task:** Create a Compressed Diagonal Storage Module and switch it out for the existing CRS Module

**Aims:** 

- Gain an understanding of how to write a completely new module and incorporate it into the OOP structure
- Gain experience with memory efficient programming
- Gain experinece with abstract classes

Currently the non-PETSc version of the code utilises a CRS system for handling the matrices involved in the problem. For this task, the user should create their own module which performs the same task utilising Compressed Diagonal Storage (CDS). The CDS module should have all the features of the CRS, allowing it to be used with the polymorphic matrix constructor and solver that already exists within the code. The user should ensure that they have added their CDS module to the makefile, and allocated it as 't_cds' where this has been done for 't_crs'. As a stretch goal, the user could also compare the solve times with their CDS module against that of the original CRS. An explanation of the CDS methodology can be found at: http://netlib.org/linalg/html_templates/node94.html



## Exercise 3c

**Task:** Make a wrapper for the blas/lapack library and use instead of PETSc/CG

**Aims:** 

- Gain experience installing external libraries
- Understand how one can incorporate an external library into the code

This final exercise involves incorporating an additional external library into the code. BLAS and LAPACK are often used in professional codes as the utilise a number of highly optimised mathematical routines. To solve the problem, the user should utilise DGETRF and DGETRI to perform LU decomposition and inversion of the matrix, then multiply it by the source vector using DGEMV. The code can be installed easily on a linux OS with the commands:

```bash
sudo apt-get install libblas-dev liblapack-dev
```

An explanation of how to use the code and each routine can be found at:
http://www.netlib.org/lapack/explore-html/

# Installing PETSc 
  The Portable, Extensible Toolkit for Scientific Computation (PETSc) is an optional external library which can be utilised in this problem to solve the system of equations generated by the code. This section will give an explanation of how to download and compile PETSc. Note that the configuring and testing of PETSc may take some time. Installation instructions can also be foind at https://petsc.org/release/install/

  - Download a copy of PETSc from:

  https://petsc.org/release/download/

  - Place the downloaded PETSc folder into the directory of your choice and navigate inside it through a terminal. Configure the code by running:
  ```bash
  ./configure --download-mpich --download-fblaslapack=1
  ```
  - Check that the installation was successful by running:
  ```bash
  make all check
  ```

# Compiling with PETSc

  With PETSc installed on your system, the last stage is to set up the environment variables that the code will use to find your PETSc directory. The text below shows how this can be done on a Linux OS, by entering the following commands into your bashrc file:

  ```bash
export PETSC_DIR="/home/jack/petsc-3.16.0"

export PETSC_BUILDS_DIR="/home/jack/petsc-3.16.0/builds"
```

  Once you have installed PETSc (see [installation instructions](##Installing-PETSc)) and set up the required environment variables, the code can also be compiled to use it instead of the custom storage and solvers. To do so, navigate to the **src** directory within the code and enter the commands:
  ```bash
  make clean
  make petsc
  ```
  This will execute the makefile contained within the same directory with the petsc options, converting the fortran code to an optimised form that utilises the PETSc library. This then generates an executable named **diffusion_petsc**. Once this has been done, the code can be executed from the parent directory using the command:
  ```bash
  ./diffusion_petsc
  ```


# Theory

In the design of any nuclear reactor, it is critical to understand the distribution of neutrons throughout the system. As neutrons move through various parts of the reactor such as the fuel or the moderator, their behaviour will greatly change based on the medium in which they find themselves. This neutron behaviour can be most simply approximated as a form of diffusion occurring within the reactor, producing a solvable equation which can be used to accurately describe this behaviour. The analysis and solving of this equation is known as neutron diffusion theory, and is a critical area of nuclear physics and engineering, used widely in the industry to calculate neutron flux profiles  and multiplication factors within a reactor.

The first step in generating this equation is to describe the spatial neutron balance within a volume $dV$, where $dV = dx dy dz$, centred at $r$, where $r = (x,y,z)$. Assuming steady state:

$$ \text{neutrons lost in } dV/s = \text{neutrons produced in } dV/s$$

Splitting these into the separate sources and sinks:

$$ \text{neutrons leaking from } dV/s + \text{neutrons absorbed in } dV/s = \text{neutrons emitted in } dV/s + \text{fission neutrons produced in } dV/s$$

Where: 

$$ \text{neutrons leaking from } dV/s =  \left(  \frac{\partial}{\partial x} J_x (x,y,z) + \frac{\partial}{\partial y} J_y (x,y,z)  + \frac{\partial}{\partial z} J_z (x,y,z)   \right) dV$$

$$\text{neutrons absorbed in } dV/s = \Sigma_a (x,y,z) \phi(x,y,z) dV$$

$$ \text{neutrons emitted in } dV/s = S (x,y,z) dV$$

$$\text{fission neutrons produced in } dV/s = \nu \Sigma_F (x,y,z) \phi(x,y,z) dV$$

Combining these and eliminating $dV$ gives:

$$ \frac{\partial}{\partial x} J_x (r) + \frac{\partial}{\partial y} J_y (r) + \frac{\partial}{\partial z} J_x (z) + \Sigma_{a} (r) \phi(r) = S(r) + \nu \Sigma_f (r) \phi (r) $$

Given the vector definitions: 

$$ J(r) = \hat{i} J_x (r) + \hat{j} J_y (r) + \hat{k} J_z (r) \text{  and   } \nabla = \hat{i} \frac{\partial}{\partial x} + \hat{j} \frac{\partial}{\partial y} + \hat{k} \frac{\partial}{\partial z} $$

We can write the balance equation as:

$$ \nabla \cdot J(r) + \Sigma_{a} (r) \phi(r) = S(r) + \nu \Sigma_f (r) \phi (r) $$

Given that we are using diffusion to describe the behaviour of neutrons within a reactor, we can therefore make use of Fick's Law, which states that any solute (the neutrons) will diffuse from an area of high concentration (high neutron flux) to an area of low concentration. This law gives an equation that will relate the current in a system to the concentration gradient multiplied by a diffusion coefficient. In the simplest 1-D reactor case the current of neutrons will therefore be given by the negative of a diffusion coefficient multiplied by the gradient of the flux. A general form of this can be seen below, where $J$ is the neutron current (or diffusion flux), $D$ is the diffusion coefficient and $\phi$ is the neutron flux. 

$$ J = - D(r)\nabla\phi(r) $$

This can be substituted into the full form of the balance equation to form the neutron diffusion equation: 

$$ - \nabla \cdot D(r)\nabla\phi(r) + \Sigma_{a} (r) \phi(r) = S(r) + \nu \Sigma_f (r) \phi (r) $$

It is therefore also important to have an accurate method of calculation for the diffusion coefficient. The diffusion coefficient is equal to 1/3 multiplied by the inverse of the transport cross section. For simple neutron diffusion cases involving isotropic scatter, the transport cross section can be set to be equal to the total cross section, which is equal to the sum of the absorption and scattering (including self-scatter) cross sections. This can be seen below where $\Sigma_{tr}$ is the transport cross section, $\Sigma_{t}$ is the total cross section, $\Sigma_{a}$ is the absorption cross section and $\Sigma_{s}$ is the scattering cross section. \par

$$ D =  \frac{1}{3\Sigma_{tr}} \simeq \frac{1}{3\Sigma_{t}} = \frac{1}{3(\Sigma_{a}+\Sigma_{s})} $$

This relation exists as the transport cross section is defined as $\Sigma_{tr} = \Sigma_{t} - \bar{\mu}\Sigma_{s}$, where $\bar{\mu}$ is the average cosine of the scattering angle. This has a value of $0$ in the laboratory system for isotropic scatter, so the transport and total cross sections can be approximated to one another in this case. 

This equation effectively relates the rate of change of neutrons within a system to a number of material properties and the flux, and hence can be solved with knowledge of the materials involved and the use of mathematical solvers. An example of the neutron diffusion equation can be seen below, for a 1-D slab reactor at steady state, where $\lambda$ is the eigenvalue of the system, $\nu$ is the average neutrons produced per fission and $\Sigma_f$ is the fission cross section. In this example a fission source of neutrons is being used instead of a simple volumetric source. \par

$$ -\frac{d}{d x} D(x) \frac{d \phi(x)}{d x}+\Sigma_{a}(x) \phi(x)=\frac{1}{\lambda} \nu \Sigma_{f}(x) \phi(x) $$

Neutron diffusion codes will principally solve this equation to calculate the neutron flux over a specified geometry. Problems involving a fission neutron source can be solved for the eigenvalue $\lambda$ of the system, utilising a fission source normalisation.
