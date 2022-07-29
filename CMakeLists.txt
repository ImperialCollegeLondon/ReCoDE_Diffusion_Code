cmake_minimum_required(VERSION 3.13)

set(CMAKE_INSTALL_PREFIX "${CMAKE_CURRENT_SOURCE_DIR}/install"
    CACHE STRING "Select where to install the library.")

project(diffusion)
enable_language(Fortran)

# Use MSVS folders to organize projects on windows
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(LIB_NAME lib${CMAKE_PROJECT_NAME}) # Name of the library
set(BIN_NAME ${CMAKE_PROJECT_NAME})
set(PROJECT_DESCRIPTION "1D neutron diffusion solver using Finite Difference")
set(PROJECT_URL "https://github.com/ImperialCollegeLondon/ReCoDE_Diffusion_Code")
set(CMAKE_CONFIGURATION_TYPES "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Select which configuration to build.")

option(BUILD_SOLUTIONS "Build the exemplar's solutions" OFF)
option(USE_PETSC "Use PETSc for the Linear Algebra" OFF)

if(NOT CMAKE_BUILD_TYPE OR CMAKE_BUILD_TYPE STREQUAL "")
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
endif()

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    # Add the DEBUG compiler flag
    add_compile_definitions(DEBUG)
endif()

# ------------------------------------------------------------------------------
# Build with PETSc
# ------------------------------------------------------------------------------
if(USE_PETSC)
    # Check for PETSc in-place installation, else PETSc will be out-of-source
    # Add the possible locations to find_package() search path
    if(DEFINED ENV{PETSC_ARCH})
        set(CMAKE_PREFIX_PATH $ENV{PETSC_DIR}/$ENV{PETSC_ARCH})
    else()
        set(CMAKE_PREFIX_PATH $ENV{PETSC_DIR})
    endif()

    find_package(PkgConfig)
    pkg_search_module(PETSC REQUIRED IMPORTED_TARGET PETSc)
endif()

if(PETSC_FOUND)
    message(STATUS "Using PETSc version ${PETSC_VERSION}")
    set(SOURCES
        src/Constants.F90
        src/PETSc/PETSc_Init.F90
        src/PETSc/PETSc_Vec.F90
        src/PETSc/PETSc_Mat.F90
        src/PETSc/PETSc_Ksp.F90
        src/Materials.F90
        src/Problem.F90
        src/Output.F90
        src/Matrix_Base.F90
        src/PETScSolver.F90
        src/MatGen.F90)

    # Create the preprocessor definitions for PETSC
    add_compile_definitions(PETSC)

    message(STATUS "PETSC_INCLUDE_DIRS: ${PETSC_INCLUDE_DIRS}")
    message(STATUS "PETSC_LIBRARY_DIRS: ${PETSC_LIBRARY_DIRS}")
    message(STATUS "PETSC_LIBRARIES: ${PETSC_LIBRARIES}")

    # Handle differently the inclusion of paths for inplace and out-of-source
    if(DEFINED ENV{PETSC_ARCH})
        include_directories(${PETSC_INCLUDE_DIRS})
        link_directories(${PETSC_LIBRARY_DIRS})
        link_libraries(${PETSC_LIBRARIES})
    else()
        include_directories(${PETSC_INCLUDEDIR})
        link_directories(${PETSC_LIBDIR})
        link_libraries(${PETSC_LIB})
    endif()

# No PETSc
else()
    set(SOURCES
        src/Constants.F90
        src/Materials.F90
        src/Problem.F90
        src/Output.F90
        src/Matrix_Base.F90
        src/CRS.F90
        src/Solver.F90
        src/MatGen.F90)
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
    message(STATUS "GNU (gfortran) compiler detected")
    add_compile_options(-Wall -ffree-line-length-none)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
    message(STATUS "Intel (ifort/ifx) compiler detected")
    add_compile_options(-warn all)
endif()

set(INSTALL_MOD_DIR "include")
set(INSTALL_LIB_DIR "lib")
set(INSTALL_BIN_DIR "bin")
set(MODULE_DIR ${CMAKE_BINARY_DIR}/include) # CMAKE_BINARY_DIR is the build dir

# Create targets. The names should be amongst targets
add_library(${LIB_NAME} SHARED ${SOURCES})
add_library(${LIB_NAME}-static STATIC ${SOURCES})
add_executable(${BIN_NAME} ${SOURCES} app/main.F90)

# Set additional properties for static library target
set_target_properties(${LIB_NAME}-static
    PROPERTIES
    OUTPUT_NAME ${PROJECT_NAME}
    if(NOT MSVC_IDE)
    PREFIX lib
    endif()
    ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
    Fortran_MODULE_DIRECTORY ${MODULE_DIR}) # Place .mod files in the module dir

# Set additional properties for shared library target
set_target_properties(${LIB_NAME}
    PROPERTIES
    OUTPUT_NAME ${PROJECT_NAME}
    if(NOT MSVC_IDE)
    PREFIX lib
    endif()
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/src) # Dump .mod files to src dir to not pollute the build dir

# Set additional properties for executable target
set_target_properties(${BIN_NAME}
    PROPERTIES
    OUTPUT_NAME ${PROJECT_NAME}
    if(NOT MSVC_IDE)
    PREFIX bin
    endif()
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/src) # Dump .mod files to src dir to not pollute the build dir

# install the library: specify the install location
install(TARGETS ${LIB_NAME} ${LIB_NAME}-static
    LIBRARY DESTINATION ${INSTALL_LIB_DIR}
    ARCHIVE DESTINATION ${INSTALL_LIB_DIR})

# install the executable: specify the install location
install(TARGETS ${BIN_NAME}
    RUNTIME DESTINATION ${INSTALL_BIN_DIR}
    ARCHIVE DESTINATION ${INSTALL_BIN_DIR})

# install the compiled modules: specify the install location
install(DIRECTORY "${MODULE_DIR}/" DESTINATION ${INSTALL_MOD_DIR})

# Windows settings:
if(MSVC_IDE)
    INCLUDE_DIRECTORIES("src")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp")
endif()

# ------------------------------------------------------------------------------
# Enable the solutions in the build
# ------------------------------------------------------------------------------
if(BUILD_SOLUTIONS)
    set(SOL_DIRS 1a 1c 2a 3a 3b)

    foreach(SOL ${SOL_DIRS})
        file(GLOB SRC_SOL_FILES solutions/${SOL}/*.F90)
        add_executable(${SOL} ${SRC_SOL_FILES} app/main.F90)
        add_dependencies(${SOL} ${LIB_NAME}-static)
        target_link_libraries(${SOL} PRIVATE ${LIB_NAME}-static)
        set_target_properties(${SOL}
            PROPERTIES
            OUTPUT_NAME ${SOL}
            if(NOT MSVC_IDE)
            PREFIX bin
            endif()
            RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/sol

            # First include .mod files from the solutions directory and then
            # include the precompiled MODULEs from the `include` directory
            # thus prioritizing the solution MODULEs over the precompiled ones.
            INCLUDE_DIRECTORIES "${CMAKE_BINARY_DIR}/solutions/${SOL};${CMAKE_BINARY_DIR}/include"

            # Output directory for the .mod files for the solution MODULEs
            # We don't want them to overwrite the main library's .mod files
            # found in `include/`
            Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/solutions/${SOL})
    endforeach(SOL)

    # Solutions that change the PUBLIC interface of a MODULE e.g.
    # add a PUBLIC function, change the arguments in a PUBLIC function etc.
    # require recompilation of the altered MODULE  +
    # recompilation of all MODULEs where the altered MODULE is being USEd.
    # Here we recompile all MODULEs found in `src` to keep things simple.
    # If the codebase was large, to minimise compilation time, we would
    # have to find a way to create a list of all the MODULEs being USEd
    # from each solution MODULE and recompile only those.
    set(SOL 1b) # Test name
    unset(SRC_SOL_FILES) # Reset the list of files to be compiled
    set(MAIN_SRCS ${SOURCES}) # Copy the file names under `src/`
    file(GLOB SRC_SOL_FILES solutions/${SOL}/*.F90) # Get all Fortran files for this solution
    list(REMOVE_ITEM MAIN_SRCS src/Materials.F90) # Remove duplicate modules from MAIN_SRCS
    add_executable(${SOL} ${SRC_SOL_FILES} ${MAIN_SRCS})
    set_target_properties(${SOL}
        PROPERTIES
        OUTPUT_NAME ${SOL}
        if(NOT MSVC_IDE)
        PREFIX bin
        endif()
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/sol
        Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/solutions/${SOL})

    set(SOL 2b)
    unset(SRC_SOL_FILES)
    set(MAIN_SRCS ${SOURCES})
    file(GLOB SRC_SOL_FILES solutions/${SOL}/*.F90)
    list(REMOVE_ITEM MAIN_SRCS src/Output.F90)
    add_executable(${SOL} ${SRC_SOL_FILES} ${MAIN_SRCS})
    set_target_properties(${SOL}
        PROPERTIES
        OUTPUT_NAME ${SOL}
        if(NOT MSVC_IDE)
        PREFIX bin
        endif()
        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/sol
        Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/solutions/${SOL})

    # Copy the solution binaries to the install directory under `sol/`
    list(APPEND SOL_DIRS 1b 2b)
    install(TARGETS ${SOL_DIRS}
        RUNTIME DESTINATION ${INSTALL_BIN_DIR}/sol
        ARCHIVE DESTINATION ${INSTALL_BIN_DIR}/sol)
endif()