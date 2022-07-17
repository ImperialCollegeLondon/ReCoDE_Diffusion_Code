# Exercises

This section contains a number of suggested exercises that will give the user a deeper understanding of the topics covered in the above descriptions and utilised throughout this code. The exercises will increase progressively in complexity, with jumps in challenge roughly seperated by the numeric ID of the exercise.

Solutions to the exercises can be found in the 'solutions' directory, where a version of the main code exists, modified such that it solves the problem. Where possible, changes made to the main code have been appropriately commented with a double dollar symbol such that users can more readily find these modifications. For example, changes made for exercise 1a can be found by searching for '$$ Exercise 1a' within the appropriate solution directory.

## Exercise 1a

**Task:** Add a print statement to the **SetName** Subroutine in the Materials Module

**Aims:**

- Gain an initial understanding of how a code can utilise an OOP structure

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

For this exercise, a new material will be added to the list of those that the code can handle. The material 'Iron' will be added with an absoprtion cross section of _4.0_ and source of _0.1_.

## Exercise 2a

**Task:** Get input deck to instead explicitly read in material data rather than associating with a name

**Aims:**

- Experience making larger adjustments to the logic of the code
- Understanding of how to adjust various sections of a code to handle a modification
- Experience handling input files

Currently, the code reads in a material name and then assigns material properties to the region based on a set of data stored in an If statement. For this exercise, the code will instead read in the absorption and source terms directly, such that 'Fuel' would instead become '1.0 6.0'. To achieve this, the user will need to adjust the **Problem** module, as well as adjust how they have defined their problem in the **input.in** file.

## Exercise 2b

**Task:** Add another two sets of cell data to the .vtu file where the absorption and source material properties of each cell can be viewed

**Aims:**

- Gain experience writing data to an output file
- Learn how to utilise vtu files to add data sets to ParaView

The current vtu file output containts the Flux, Cell Number and Region Number data sets. In this exercise, the user will add the Source term data set to the vtu output, such that the source value of each cell can be viewed in ParaView. An additional Cell Data set will need to be added which uses the known Region Numbers to pull data from the **Materials** Module.

## Exercise 3a

**Task:** Smear the problem into another dimension to produce a 3D ParaView output

**Aims:**

- Gain further experience writing data to an output file
- Learn how to utilise vtu files to generate more complex outputs in ParaView

The main version of the code already smears a 1-dimensional flux profile into a second dimension for the sake of a visualisation example. This could be furthered by also smearing the results in a third dimension. To do so, the user will need to make an additional set of nodes that have been translated in the z axis and ensure that these are then associated with the correct cells. Users should utilise the ParaView vtk format guide to do so, noting that they will now need to define the cells as a three dimensional shape. The vtk file format is described here: https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf

## Exercise 3b

**Task:** Create a Compressed Diagonal Storage Module and switch it out for the existing CRS Module

**Aims:**

- Gain an understanding of how to write a completely new module and incorporate it into the OOP structure
- Gain experience with memory efficient programming
- Gain experinece with abstract classes

Currently the non-PETSc version of the code utilises a CRS system for handling the matrices involved in the problem. For this task, the user should create their own module which performs the same task utilising Compressed Diagonal Storage (CDS). The CDS module should have all the features of the CRS, allowing it to be used with the polymorphic matrix constructor and solver that already exists within the code. The user should ensure that they have added their CDS module to the makefile, and allocated it as 't_cds' where this has been done for 't_crs'. As a stretch goal, the user could also compare the solve times with their CDS module against that of the original CRS. An explanation of the CDS methodology can be found at: http://netlib.org/linalg/html_templates/node94.html

## Possible Extension Exercise

**Task:** Make a wrapper for the blas/lapack library and use instead of PETSc/CG

**Aims:**

- Gain experience installing external libraries
- Understand how one can incorporate an external library into the code

This extension exercise involves incorporating an additional external library into the code. BLAS and LAPACK are often used in professional codes as the utilise a number of highly optimised mathematical routines. To solve the problem, the user should utilise DGETRF and DGETRI to perform LU decomposition and inversion of the matrix, then multiply it by the source vector using DGEMV. The code can be installed easily on a linux OS with the commands:

```bash
sudo apt-get install libblas-dev liblapack-dev
```

An explanation of how to use the code and each routine can be found at:
http://www.netlib.org/lapack/explore-html/