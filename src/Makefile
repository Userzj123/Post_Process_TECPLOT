#=======================================================================
# User definitions
#

NAME = 01_Insfield.e

F90FILES = module.f90 \
	main.f90 \
	setup.f90 \
	openmpcut.f90 \
	write_grid.f90 \
	read_rstrt.f90 \
        mesh.f90 \
	indices.f90 \
	writeins3d.f90 \
	writeins2d.f90 \
	writeins2d_2.f90 \

#F77FILES = readavg.f \

##LIBS = /home/software/tec360/lib/tecio64.a -lstdc++
LIBS = /home/ext-zyou6474/LESGO/Post_processing_TECPLOT/libtecio.a -lstdc++

CFILES = 

#=======================================================================
# Standard definitions
#
OFILES = $(F90FILES:.f90=.o) $(F77FILES:.f=.o) $(CFILES:.c=.o)

LOPTS = -L/opt/intel/mkl/lib/intel64 \
 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread

# INTEL compilers
F90 = ifort -r8 -mcmodel=large -shared-intel
F77 = ifort -r8 -mcmodel=large -shared-intel
CC  = icc
LD  = ifort -r8 -mcmodel=large -shared-intel -o post.exe

VPATH = obj
.SUFFIXES:
.SUFFIXES: .out .o .f90 .f .c .inc .h

#=======================================================================
# Targets and dependencies
#
default:: opt
all:: post.exe
debug:
	@make opt "FLAGS = -g $(FLAGS)"
prof:
	@make opt "LIBS = $(LIBS) -p"
opt:
	@make all "FLAGS = -O2 $(FLAGS)"
clean:
	rm -rf obj $(NAME) *.mod
post.exe: obj $(OFILES)
	@echo "$(LD) $(OFILES) $(LIBFILES) $(LOPTS) $(LIBS)"
	@cd obj; $(LD) $(OFILES) $(LIBFILES) $(LOPTS) $(LIBS); mv post.exe ../$(NAME);
obj:
	mkdir obj

#=======================================================================
# Compilation rules
#
.f90.o:
	$(F90) $(FLAGS) -c $*.f90 -o obj/$*.o
.f.o:
	$(F77) $(FLAGS) -c $*.f -o obj/$*.o
.c.o:
	$(CC) $(FLAGS) -c $*.c -o obj/$*.o

