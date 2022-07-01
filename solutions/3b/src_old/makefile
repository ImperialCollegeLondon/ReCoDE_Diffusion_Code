#Set the compilers and linker
FC = gfortran
LD = gfortran


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# PETSc
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PETSC_ARCH = $(PETSC_DIR)/linux_opt/
PETSC_INC_PATH = -I$(PETSC_DIR)/include/  -I$(PETSC_ARCH)/include/
PETSC_LINK_PATH =  -L$(PETSC_DIR)/lib/ -L$(PETSC_ARCH)/lib/
PETSC_LOAD_PATH = -Wl,-rpath=$(PETSC_ARCH)/lib/
PETSC_LIBS = -lpetsc



#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# PATHS
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
petsc: INCLUDE_PATH = $(PETSC_INC_PATH)
petsc_debug: INCLUDE_PATH = $(PETSC_INC_PATH)
petsc: LINK_PATH =  $(PETSC_LINK_PATH)
petsc_debug: LINK_PATH =  $(PETSC_LINK_PATH)
petsc: LOAD_PATH =  $(PETSC_LOAD_PATH)
petsc_debug: LOAD_PATH =  $(PETSC_LOAD_PATH)
petsc: LIBS =$(PETSC_LIBS)
petsc_debug: LIBS =$(PETSC_LIBS)


# Compiler options
FC_FLAGS_COMMON = -ffree-line-length-800 -Wall -std=f2018
FC_FLAGS_DEBUG = $(FC_FLAGS_COMMON) -O0 -g -pg -fcheck=bounds -DDEBUG
FC_FLAGS = $(FC_FLAGS_COMMON) -O3 
petsc:   FC_FLAGS = $(FC_FLAGS_COMMON) -DPETSC
debug:   FC_FLAGS = $(FC_FLAGS_DEBUG)
petsc_debug:   FC_FLAGS = $(FC_FLAGS_DEBUG) -DPETSC

#Linker flags
LD_FLAGS = -O2
LD_FLAGS_DEBUG = -g -Og
debug:   LD_FLAGS = $(LD_FLAGS_DEBUG)
petsc_debug:   LD_FLAGS = $(LD_FLAGS_DEBUG)


#Set the objects
#Note the order below is determined by dependencies between object files
OBJS= stdlib/Constants.o \
 stdlib/StdLib.o \
 inoutproc/Materials.o \
 inoutproc/Problem.o \
 inoutproc/Output.o \
 centralproc/Matrix_Base.o \
 centralproc/CRS.o \
 centralproc/CDS.o \
 centralproc/Solver.o \
 centralproc/MatGen.o \
 main.o \

OBJS_PETSC= stdlib/Constants.o \
 stdlib/StdLib.o \
 PETSc/PETSc_Init.o \
 PETSc/PETSc_Vec.o \
 PETSc/PETSc_Mat.o \
 PETSc/PETSc_Ksp.o \
 inoutproc/Materials.o \
 inoutproc/Problem.o \
 inoutproc/Output.o \
 centralproc/PETScSolver.o \
 centralproc/MatGen.o \
 main.o \




#Set up the MODS so it contains the same as OBJS but with the .o replaced by .mod
MODS= $(OBJS:.o=.mod)
MODS_PETSC= $(OBJS:.o=.mod)

#Set the executable name
EXEC=../diffusion
EXEC_PETSC=../diffusion_petsc

#Default make command requires the executable to be up to date
all : $(EXEC)
petsc:   $(EXEC_PETSC)
debug:   $(EXEC)
petsc_debug:   $(EXEC_PETSC)

#Object files required to be updated if corresponding .F90 files have changed
%.o : %.f90
	$(FC) $(FC_FLAGS) -cpp -c -o $@ $< $(INCLUDE_PATH) $(LINK_PATH) $(LIBS)

#For the executable to be up to date the object files must be up to date. Then link the objects
$(EXEC): $(OBJS)
	$(LD) $(LD_FLAGS) $(LOAD_PATH) -o $@ $^ $(LIBS) $(LINK_PATH)

$(EXEC_PETSC): $(OBJS_PETSC)
	$(LD) $(LD_FLAGS) $(LOAD_PATH) -o $@ $^ $(LIBS) $(LINK_PATH)

#Clean the directory and any directories searched
.PHONY : clean
clean :
	rm -f $(OBJS) $(OBJS_PETSC) $(EXEC) $(EXEC_PETSC) $(MODS) $(MODS_PETSC) *.o *.mod
