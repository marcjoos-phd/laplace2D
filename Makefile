# Preprocessing arguments & debug
OACC  = 0
OMP   = 0
CUDA  = 1
DEBUG = 0
DEBUG_CUDA = 0
FASTREDUCE = 1
WITHCUFPRAGMA = 0

# Check arguments compatibility
ifeq ($(FASTREDUCE),1)
ifeq ($(WITHCUFPRAGMA),1)
$(error You cannot use both CUF pragma and FASTREDUCE)
endif
endif

# General flags
F90       = pgfortran
FCFLAGS   = -O3 -Mpreprocess
PPFLAGS   = -DOACC=$(OACC) -DOMP=$(OMP) -DCUDA=$(CUDA)
OMPFLAGS  = -mp
AUTOFLAGS = -Mconcur
GPUFLAGS  = -Mcuda -ta:nvidia,cc35
OACCFLAGS = -acc
H5INC     = -I/opt/Modules/hdf5/seq/1.8.12/pgi14.1/include
H5LIB     = -L/opt/Modules/hdf5/seq/1.8.12/pgi14.1/lib -lhdf5_fortran -lhdf5

# Activate debug-mode
ifeq ($(DEBUG),1)
FCFLAGS  += -traceback -g -Mbounds
GPUFLAGS  = -Mcuda=emu
endif

# Gather flags depending on the parallelization strategy
FLAGS     = $(FCFLAGS)
PPFLAGS   =
EXEC      = laplace
ifeq ($(OACC),1)
FLAGS    += $(OACCFLAGS) $(GPUFLAGS)
PPFLAGS  += -DOACC
EXEC      = laplace_oacc
endif
ifeq ($(OMP),1)
FLAGS    += $(OMPFLAGS)
PPFLAGS  += -DOMP
EXEC      = laplace_omp
endif
ifeq ($(CUDA),1)
FLAGS    += $(GPUFLAGS)
PPFLAGS  += -DCUDA
EXEC      = laplace_cuda
endif
ifeq ($(DEBUG_CUDA),1)
PPFLAGS  += -DDEBUG_CUDA
endif
ifeq ($(FASTREDUCE),1)
PPFLAGS  += -DFASTREDUCE
endif
ifeq ($(WITHCUFPRAGMA),1)
PPFLAGS  += -DWITHCUFPRAGMA
endif
FLAGS    += $(PPFLAGS)

# Objects to compile
OBJ = commons.o kernels.o io.o laplace.o

# Compile the objects
%.o:%.f90
	$(F90) $(H5INC) $(FLAGS) -c $^ -o $@

# Compile the source
laplace: $(OBJ)
	$(F90) $(FLAGS) -o $(EXEC) $(OBJ) $(H5LIB)

# Clean method
clean:
	@rm *.o *.mod
