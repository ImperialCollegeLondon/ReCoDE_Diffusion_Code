# ReCoDE_Transport_Code
Repository for the ReCoDE project which aims to solve the Neutron Transport Equation

Current required steps for use:
- Install PETSc
- Set up environment variables such that PETSC_DIR and PETSC_BUILDS_DIR point to your PETSc and PETSc builds directories respectively, e.g.:

export PETSC_DIR="/home/jack/petsc-3.16.0"

export PETSC_BUILDS_DIR="/home/jack/petsc-3.16.0/builds"

- compile with 'make' for optimised version of code or 'make debug' for unoptimised run with more descriptive outputs
