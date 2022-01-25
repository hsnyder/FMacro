#!/usr/bin/env sh
gfortran11 -g fmacro.f90 -o fmacro && ./fmacro test/Typefile test/inputfile.f90
