# ReCoDE_Transport_Code
Repository for the ReCoDE project which aims to solve the Neutron Transport Equation

Current required steps for use:
- Install PETSc
- Set up environment variables such that PETSC_DIR and PETSC_BUILDS_DIR point to your PETSc and PETSc builds directories respectively, e.g.:

export PETSC_DIR="/home/jack/petsc-3.16.0"

export PETSC_BUILDS_DIR="/home/jack/petsc-3.16.0/builds"

- compile with 'make' for optimised version of code or 'make debug' for unoptimised run with more descriptive outputs

# Proposed Structure of Readme

- Abstract

- Structure of the Code
  - Description of Logic
  - Flowchart to clearly show OOP structure
  - Pseudocode version of code

- User Guide
  - Compiling the Code
  - Changing Input Options
  - Reading Output Files
  - Installing PETSc 

- Features of the Code
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
    - Clear example of OOP (simple gets and sets
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

- Exercises
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





