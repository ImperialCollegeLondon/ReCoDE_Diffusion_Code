#Set the compilers and linker
FC = gfortran
CC = gcc
LD = gfortran


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# PETSc
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PETSC_ARCH = $(PETSC_DIR)/linux_opt/
PETSC_INC_PATH = -I$(PETSC_DIR)/include/  -I$(PETSC_ARCH)/include/ #-I$(PETSC_DIR)/include/petsc/finlcude/ -I$(PETSC_DIR)/arch-linux-c-debug/
PETSC_LINK_PATH =  -L$(PETSC_DIR)/lib/ -L$(PETSC_ARCH)/lib/
PETSC_LOAD_PATH = -Wl,-rpath=$(PETSC_ARCH)/lib/
PETSC_LIBS = -lpetsc -lblas -llapack



#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# PATHS
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
INCLUDE_PATH = $(PETSC_INC_PATH)
LINK_PATH =  $(PETSC_LINK_PATH)
LOAD_PATH =  $(PETSC_LOAD_PATH)
LIBS =$(PETSC_LIBS)


# Compiler options
FC_FLAGS_WARNING    =  -ffree-line-length-800 -O0 -g -pg -Wall -fcheck=bounds
FC_FLAGS_DEBUG    =  -ffree-line-length-800 -O0 -g -pg -fcheck=bounds
FC_FLAGS  =      -ffree-line-length-800 -O3 -DNDEBUG
debug:   FC_FLAGS = $(FC_FLAGS_DEBUG)
warning: FC_FLAGS = $(FC_FLAGS_WARNING)

#Linker flags
LD_FLAGS = -O2
LD_FLAGS_DEBUG = -g -Og
LD_FLAGS_WARNING = -g -Og
debug:   LD_FLAGS = $(LD_FLAGS_DEBUG)
warning:   LD_FLAGS = $(LD_FLAGS_WARNING)


#Set the objects
OBJS= stdlib/Constants.o \
 stdlib/StdLib.o \
 PETSc/PETScInit.o \
 PETSc/PETScVec.o \
 PETSc/PETScMat.o \
 PETSc/PETScKSP.o \
 inoutproc/Input.o \
 inoutproc/Materials.o \
 inoutproc/Output.o \
 centralproc/Solver.o \
 centralproc/PETScSolver.o \
 centralproc/DiamondDiff.o \
 main.o \




#Set up the MODS so it contains the same as OBJS but with the .o replaced by .mod
MODS= $(OBJS:.o=.mod)

#Set the executable name
EXEC=../output

#Set up a variable to represent the makefile
DEFAULT=makefile


#Set directories to look for source files, etc
VPATH = $(SCRIPTS_PATH)

#Default make command requires the executable to be up to date
all : $(EXEC)
debug:   $(EXEC)
warning:   $(EXEC)
home:    $(EXEC)

#Objects required to be updated if the makefile has changed
%.o : $(DEFAULT)

#Object files required to be updated if corresponding .F95 files have changed
%.o : %.f95
	$(FC)  $(FC_FLAGS) -cpp -c -o $@ $< $(INCLUDE_PATH) $(LINK_PATH) $(LIBS)

#Object files required to be updated if corresponding .F90 files have changed
%.o : %.f90
	$(FC) $(FC_FLAGS) -cpp -c -o $@ $< $(INCLUDE_PATH) $(LINK_PATH) $(LIBS)

#Object files required to be updated if corresponding .f files have changed
%.o : %.f
	$(FC) $(FFLAGS) -cpp -c -o $@ $< $(INCLUDE_PATH) $(LINK_PATH) $(LIBS)

#Object files required to be updated if corresponding .c files have changed
%.o : %.c
	$(CC) $(CFLAGS) -cpp -c -o $@ $< $(INCLUDE_PATH) $(LINK_PATH) $(LIBS)

#For the executable to be up to date the object files must be up to date. Then link the objects
$(EXEC): $(OBJS)
	$(LD) $(LD_FLAGS) $(LOAD_PATH) -o $@ $^ $(LIBS) $(LINK_PATH)

#Clean the directory and any directories searched
.PHONY : clean
clean :
	rm -f $(OBJS) $(EXEC) $(MODS) *.o *.mod
	rm -f $(VPATH)/$(OBJS) $(VPATH)/$(MODS)
