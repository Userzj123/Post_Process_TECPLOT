#!/bin/bash -l
rm ./post.exe
make clean
mkdir obj
ifort -r8 -mcmodel=large -shared-intel  -c module.f90 -o obj/module.o
ifort -r8 -mcmodel=large -shared-intel  -c main.f90 -o obj/main.o
ifort -r8 -mcmodel=large -shared-intel  -c setup.f90 -o obj/setup.o
ifort -r8 -mcmodel=large -shared-intel  -c openmpcut.f90 -o obj/openmpcut.o
ifort -r8 -mcmodel=large -shared-intel  -c write_grid.f90 -o obj/write_grid.o
ifort -r8 -mcmodel=large -shared-intel  -c read_rstrt.f90 -o obj/read_rstrt.o
ifort -r8 -mcmodel=large -shared-intel  -c mesh.f90 -o obj/mesh.o
ifort -r8 -mcmodel=large -shared-intel  -c indices.f90 -o obj/indices.o
ifort -r8 -mcmodel=large -shared-intel  -c writeins3d.f90 -o obj/writeins3d.o
ifort -r8 -mcmodel=large -shared-intel  -c writeins2d.f90 -o obj/writeins2d.o
ifort -r8 -mcmodel=large -shared-intel  -c writeins2d_2.f90 -o obj/writeins2d_2.o
ifort -r8 -mcmodel=large -shared-intel -o post.exe obj/module.o obj/main.o obj/setup.o obj/openmpcut.o obj/write_grid.o obj/read_rstrt.o obj/mesh.o obj/indices.o obj/writeins3d.o obj/writeins2d.o obj/writeins2d_2.o    -L/opt/intel/mkl/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread libtecio.a -lstdc++

