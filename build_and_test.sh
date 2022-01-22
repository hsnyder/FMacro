#!/usr/bin/env sh
gfortran11 -g ftntemple.f90 -o ftntemple && ./ftntemple test/Typefile test/inputfile.f90
