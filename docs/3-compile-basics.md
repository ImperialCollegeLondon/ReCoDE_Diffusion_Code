# 3 - Compilation Basics

## Compiled Codes and Makefiles

Programming languages can be differentiated into 'Interpreted' and 'Compiled' languages. Languages such as Python are interpreted languages, where the code is read directly by the computer and the actions described are performed. In contrast, languages such as Fortran must be compiled by a program before the code can be used, where a program known as a compiler reads through the code, converging it to machine language. While time must be spent compiling the code, this generally results in very noticeable performance increases and is the reason why most high performance codes are compiled. Readers familiar with Python may be familiar with the `numpy` library that is often used for numerical calculations, a library which makes use of compiled codes to give Python codes some dramatic reductions in numerical computation time.

Makefiles are particularly useful in a compiled code that utilises OOP as it allows for easy compilation of the required files and handles the order of dependencies. The code snippet below comes from the makefile used in the project and shows how an ordered list of objects is used when compiling the code. This is written such that the compiler will work through the files in a way that each dependency is compiled before the modules that utilise it, ending with the main.

```makefile
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

## Compiler Directives

The makefile can also facilitate compilation in a number of different ways depending on the options provided. The snippet below compiles the code when the command `make debug` is used on the command line, such that it uses all common flags set by **$(FC_FLAGS_COMMON)**. In addition, the flag `-DDBEUG` creates the compiler flag 'DEBUG'.

```makefile
debug:   FC_FLAGS = $(FC_FLAGS_COMMON) -DDEBUG
```

This flag can then be checked with C pre-processor language to allow for different parts of the code to be compiled dependent on the flags incorporated. In the below example, a more detailed output is given by the **Solve** module when the code is compiled with `DEBUG`.

```fortran
#ifdef DEBUG
  write(*,*) "---CG Convergence Succeeded---"
  write(*,'(g0)',advance='no') "Succeeded after iterations:  "
  write(*,'(g0)',advance='no') BCG_Iterations
  write(*,'(g0)',advance='no') "  with residual:"
  write(*,'(E14.6)') rho1
  write(*,*) "-------------------------------"
#endif
```

To test this we can enter the following commands to view the modified output with additional information:

```bash
make clean
make debug
```

Then in our main directory we can run the newly compiled 'debug' version of the code

```bash
./diffusion
```
