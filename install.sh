#!/usr/bin/env sh

mkdir build
cd build
# Configure your project i.e. generate Makefiles, Ninja files, etc.
cmake ..
# Compile your project, if using Makefiles, that is equivelant to: make
cmake --build .
# Install your project, if using Makefiles, that is equivelant to: make install
cmake --install .
cd ..
