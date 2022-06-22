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
- Compiled Codes
- Object Oriented Programming (OOP)
- Makefiles
- Compiler Directives
- Reading input data from file
- Generating output files
- Paraview
- Solving mathematical problems
- Discretisation of a spatial dimension
- Optimised data storage and solvers
- Incorporating external libraries (PETSc)


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
Close( Output File
```

# User Guide
  ## Compiling the Code
  As fortran is a compiled language, the code must first be compiled before it can be executed. To do so, navigate to the **src** directory within the code and enter the commands:
  ```bash
  make clean
  make
  ```
  This will execute the makefile contained within the same directory, converting the fortran code to an optimised form that can be read by your computer. This then generates an executable named **diffusion**. Once this has been done, the code can be executed from the parent directory using the command:
  ```bash
  ./diffusion
  ```
  This command tells the executable to run and will generate relevant output files containing the solution to the problem. 


  ## Changing Input Options
  The code is designed such that the user can easily change the problem which the code is attempting to solve. The code uses the input file **Input.txt** to read details about the problem, such as the positions of boundaries or materials in the problem. An example of such an input can be seen below:
  ```
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
  10
  10
  ------------------------------------------
  Materials: - Fuel, Water or Steel (one per line)
  Fuel
  Steel
  ------------------------------------------
  ```
   For this example problem, we are stating that we have a geometry ranging from *x = 0.0* to *1.0*, half fuel and half steel with a central boundary at *x = 0.5*. As seen from the above input, the code needs four different parameters to be described to it.
  - **Regions** - An integer number of regions that exists within the problem. We have 1 region from *0.0* to *0.5* and another from *0.5* to *1.0*, hence we give the code the integer number **2**.
  - **Boundaries** - The positions of the boundaries within the problem. Our first boundary is at the start of our geometry so we enter the number **0.0**. We then have an internal boundary halfway through the problem seperating the regions so we enter the number **0.5**. Finally we have the exterior boundary of our geometry, so we enter the number **1.0**. The code will always read one more value here than the number of regions in the problem.
  - **Nodes** - This desribes how refined we want the geometry in each region. For the example we want a quick solve with just enough nodes to see the flux profile. As we need to describe this for each region we enter the value **10** twice. The code will always read the same number of values here as the number of regions in the problem.
  - **Materials** - This described the materials that are present within the system. The first half of our geometry if fuel, with the latter half being Steel, so we enter **Fuel** and **Steel**. The code will always read the same number of values here as the number of regions in the problem.
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

  ![FluxProfile](https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code/blob/main/FluxProfile.png)

  GNUPlot was chosen here as the application is very simple and the program itself is very easy to install. On Linux, you only need the commands:
  ```bash
  sudo apt update
  sudo apt install gnuplot
  ```
  For installation on Windows, you will need to download the software from http://www.gnuplot.info/download.html. The GNUPlot script used to generate the plot is named **fluxplot** and can be found in the home directory of the project. This can be run with the command:
  ```bash
  gnuplot -p fluxplot
  ```
  
  ## Installing PETSc 
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

  ## Compiling with PETSc

  If you have installed PETSc (see [installation instructions](##Installing-PETSc)), the code can also be compiled to use it instead of the custom storage and solvers. To do so, navigate to the **src** directory within the code and enter the commands:
  ```bash
  make clean
  make petsc
  ```
  This will execute the makefile contained within the same directory with the petsc options, converting the fortran code to an optimised form that utilises the PETSc library. This then generates an executable named **diffusion_petsc**. Once this has been done, the code can be executed from the parent directory using the command:
  ```bash
  ./diffusion_petsc
  ```

# Features of the Code
  - Makefile
    - Discuss Makefile and compilation options
    - Compiler directives
  - Input Module
    - Reading data from input files
    - Storing data within a module
  - Output Module
    -  Writing data to an output file
    -  Using paraview and vtk structure
  - Materials Module
    - Clear example of OOP (simple gets and sets)
  - MatGen Module
    - Discuss how a mathematical problem can be converted into something solveable
    - Discretisation in space using data stored within other modules
  - Compressed Row Storage Module
    - How data storage can be optimised for efficiency
  - Conjugate Gradient Module
    - How you can write your own modules to solve systems of equations
    - Brief aside on preconditioners
  - One of the PETSc Modules
    - Writing a wrapper for a library
    - Compiling the code with PETSc
    - Why we use external libraries

# Exercises
  - Easier
    - Add a print statement to one of the materials subroutines
    - Write a new subroutine which prints all material data stored in the module
    - Make a new material which can be used in the input deck
  - More Challenging
    - Get input deck to instead explicitly read in material data rather than associating with a name
    - Add another set of cell data to the .vtu file where the material properties of each cell can be viewed e.g. absorption 
  - Challenging
    - Smear the problem into another dimension to produce a 3D paraview output
    - Create a Compressed Diagonal Storage Module and switch it out for the existing CRS Module
    - Make a wrapper for the blas/lapack library and use instead of PETSc/CG


# PETSc Installation

Current required steps for use with PETSc:
- Install PETSc
- Set up environment variables such that PETSC_DIR and PETSC_BUILDS_DIR point to your PETSc and PETSc builds directories respectively, e.g.:

```bash
export PETSC_DIR="/home/jack/petsc-3.16.0"

export PETSC_BUILDS_DIR="/home/jack/petsc-3.16.0/builds"
```

- compile with 'make petsc' for optimised version of code or 'make petsc debug' for unoptimised run with more descriptive outputs


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

