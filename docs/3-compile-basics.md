# 3 - Compilation Basics

Programming languages can be differentiated into _Interpreted_ and _Compiled_ languages. Languages such as Python are interpreted languages, where the code is read directly by the computer and the actions described are performed. In contrast, languages such as Fortran must be compiled by a program before the code can be used, where a program known as a compiler reads through the code, converting it to machine language. While time must be spent compiling the code, this generally results in very noticeable performance increases and is the reason why most high performance codes are compiled. Readers familiar with Python may be familiar with the `numpy` library that is often used for numerical calculations, a library which makes use of compiled codes to give Python codes some dramatic reductions in numerical computation time.

## Compiled Codes and CMake

This project uses `CMake` for building and compiling.
`CMake` is a tool commonly used for C/C++ and Fortran projects
in order to allow for easy compilation of the project in multiple operating systems (Linux, OSX, Windows, etc.),
using various compilers (GCC, Clang, Intel, etc.) with potentially different levels of optimisations (Debug, Release, etc.), all at the same time.

The way `CMake` works is by reading a `CMakeLists.txt` file, which is a text file that contains the commands to be executed by `CMake`. `CMake` then scans for the project's dependencies and builds the
necessary files e.g. `Makefile`s to compile the project. You then use the newly generated files to compile the project.

A normal project being built with `CMake` will look something like this:

```bash
mkdir build
cd build
cmake ..   # This is relative to top level directory, containing CMakeLists.txt
make all   # Assuming we are using GNU Makefiles else use cmake --build .
```

The greater discussion on build tools and more advanced forms of compilation
is deferred for the later sections, see [7 - Build Tools](7-build-tools.md).

Build tools such as `CMake` are particularly useful in a compiled code that utilises OOP as it allows for easy compilation of the required files since it automatically deduces the order of dependencies, thus removing this burden from the programmer.
The code snippet below comes from the `CMakeLists.txt` used in the project.

```cmake
set(SOURCES
    src/Constants.F90
    src/Materials.F90
    src/Problem.F90
    src/Output.F90
    src/Matrix_Base.F90
    src/CRS.F90
    src/Solver.F90
    src/MatGen.F90)
```

Try and change the compilation order of the files in the `SOURCES` in `CMakeLists.txt`
to see how the `CMake` can handle it gracefully. Run the following to reconfigure the project:

```bash
cd build
cmake ..
make all
```

The `diffusion` binary can be found under `build/bin/diffusion` and can be run with the following command:

```bash
./build/bin/diffusion
```

> NOTE: make sure you have an `input.in` file in the directory where you are running `diffusion` from.

## Debug and Release builds

One of the most appealing features of `CMake` is the ability to build the project in different modes
side-by-side. This allows for easy switching between different modes of compilation, e.g. Debug and Release.

This project is by default built in Release mode, which enables various optimisations to make the code run faster.
Let us try and build the project in Debug mode. From the root directory of the project run the following commands:

```bash
mkdir debug
cd debug
cmake .. -DCMAKE_BUILD_TYPE=Debug
make all
```

That is all there is to it! The `diffusion` binary in the `Debug` build can be found under `debug/bin/diffusion` and can be run with like the normal `diffusion` binary.

> NOTE: make sure you have an `input.in` file in the directory where you are running `diffusion` from.

## Compiler Directives

There will be instances where one might wish to pass certain options to the compiler, e.g. to disable a warning, or to enable
some experimental optimisations or toggle a preprocessor macro. This can be done via the `CMakeLists.txt` file but also
through the command-line interface of `CMake`. Here the latter is demonstrated.

Say that in our `Debug` build we wish to disable the verbose debug output found in `Solve.F90`

```fortran
#ifdef DEBUG  ! If DEBUG macro is defined, then compile the following code
  write(*,*) "---CG Convergence Succeeded---"
  write(*,'(g0)',advance='no') "Succeeded after iterations:  "
  write(*,'(g0)',advance='no') BCG_Iterations
  write(*,'(g0)',advance='no') "  with residual:"
  write(*,'(E14.6)') rho1
  write(*,*) "-------------------------------"
#endif
```

As seen above, the `#ifdef` directive is used to enable or disable the code in the file.
When we build the project in Debug mode, we also define the `DEBUG` preprocessor macro in our `CMakeLists.txt`.
To prevent that snippet of code from compiling, normally we would pass the `-UDEBUG` option to the compiler.
In `CMake` that can be achieved via

```bash
cd debug
cmake .. -DCMAKE_BUILD_TYPE=Debug -DCMAKE_Fortran_FLAGS=-UDEBUG
make all
```

To test that it worked run the following and observe that the output is missing the
`---CG Convergence Succeeded---`... lines

```bash
./debug/bin/diffusion
```

> NOTE: make sure you have an `input.in` file in the directory where you are running `diffusion` from.
