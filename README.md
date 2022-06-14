# ReCoDE_Transport_Code
Repository for the ReCoDE project which aims to solve the Neutron Transport Equation

Current required steps for use:
- Install PETSc
- Set up environment variables such that PETSC_DIR and PETSC_BUILDS_DIR point to your PETSc and PETSc builds directories respectively, e.g.:

export PETSC_DIR="/home/jack/petsc-3.16.0"

export PETSC_BUILDS_DIR="/home/jack/petsc-3.16.0/builds"

- compile with 'make' for optimised version of code or 'make debug' for unoptimised run with more descriptive outputs

# Proposed Structure of Readme

- Abstract



- Structure of the Code
  - Description of Logic
  - Flowchart to clearly show OOP structure
  - Pseudocode version of code
![DiffCode drawio](https://user-images.githubusercontent.com/83182489/173537965-ac15206a-dc13-4659-89f0-5b7141eb3091.png)

- User Guide
  - Compiling the Code
  - Changing Input Options
  - Reading Output Files
  - Installing PETSc 

- Features of the Code
  - Makefile
    - Discuss Makefile and compilation options
    - Compiler directives
  - Input Module
    - Reading data from input files
    - Storing data within a module
  - Output Module
    -  Writing data to an output file
    -  Using paraview and vtk structure
  - Materials Module
    - Clear example of OOP (simple gets and sets)
  - MatGen Module
    - Discuss how a mathematical problem can be converted into something solveable
    - Discretisation in space using data stored within other modules
  - Compressed Row Storage Module
    - How data storage can be optimised for efficiency
  - Conjugate Gradient Module
    - How you can write your own modules to solve systems of equations
    - Brief aside on preconditioners
  - One of the PETSc Modules
    - Writing a wrapper for a library
    - Compiling the code with PETSc
    - Why we use external libraries

- Exercises
  - Easier
    - Add a print statement to one of the materials subroutines
    - Write a new subroutine which prints all material data stored in the module
    - Make a new material which can be used in the input deck
  - More Challenging
    - Get input deck to instead explicitly read in material data rather than associating with a name
    - Add another set of cell data to the .vtu file where the material properties of each cell can be viewed e.g. absorption 
  - Challenging
    - Smear the problem into another dimension to produce a 3D paraview output
    - Create a Compressed Diagonal Storage Module and switch it out for the existing CRS Module
    - Make a wrapper for the blas/lapack library and use instead of PETSc/CG

# Theory

In the design of any nuclear reactor, it is critical to understand the distribution of neutrons throughout the system. As neutrons move through various parts of the reactor such as the fuel or the moderator, their behaviour will greatly change based on the medium in which they find themselves. This neutron behaviour can be most simply approximated as a form of diffusion occurring within the reactor, producing a solvable equation which can be used to accurately describe this behaviour. The analysis and solving of this equation is known as neutron diffusion theory, and is a critical area of nuclear physics and engineering, used widely in the industry to calculate neutron flux profiles  and multiplication factors within a reactor.

The first step in generating this equation is to describe the spatial neutron balance within a volume $dV$, where $dV = dx dy dz$, centred at $r$, where $r = (x,y,z)$. Assuming steady state:

$$ \text{neutrons lost in } dV/s = \text{neutrons produced in } dV/s$$

Splitting these into the separate sources and sinks:

$$ \text{neutrons leaking from } dV/s + \text{neutrons absorbed in } dV/s $$
$$ = \text{neutrons emitted in } dV/s + \text{fission neutrons produced in } dV/s$$

Where: 

$$ \text{neutrons leaking from } dV/s =  \left(  \frac{\partial}{\partial x} J_x (x,y,z) + \frac{\partial}{\partial y} J_y (x,y,z)  + \frac{\partial}{\partial z} J_z (x,y,z)   \right) dV$$

$$\text{neutrons absorbed in } dV/s = \Sigma_a (x,y,z) \phi(x,y,z) dV$$

$$ \text{neutrons emitted in } dV/s = S (x,y,z) dV$$

$$\text{fission neutrons produced in } dV/s = \nu \Sigma_F (x,y,z) \phi(x,y,z) dV$$

Combining these and eliminating $dV$ gives:

$$ \frac{\partial}{\partial x} J_x (r) + \frac{\partial}{\partial y} J_y (r) + \frac{\partial}{\partial z} J_x (z) + \Sigma_{a} (r) \phi(r) = S(r) + \nu \Sigma_f (r) \phi (r) $$

Given the vector definitions: 

$$ J(r) = \hat{i} J_x (r) + \hat{j} J_y (r) + \hat{k} J_z (r) \text{  and   } \nabla = \hat{i} \frac{\partial}{\partial x} + \hat{j} \frac{\partial}{\partial y} + \hat{k} \frac{\partial}{\partial z} $$

We can write the balance equation as:

$$ \nabla \cdot J(r) + \Sigma_{a} (r) \phi(r) = S(r) + \nu \Sigma_f (r) \phi (r) $$

Given that we are using diffusion to describe the behaviour of neutrons within a reactor, we can therefore make use of Fick's Law, which states that any solute (the neutrons) will diffuse from an area of high concentration (high neutron flux) to an area of low concentration. This law gives an equation that will relate the current in a system to the concentration gradient multiplied by a diffusion coefficient. In the simplest 1-D reactor case the current of neutrons will therefore be given by the negative of a diffusion coefficient multiplied by the gradient of the flux. A general form of this can be seen below, where $J$ is the neutron current (or diffusion flux), $D$ is the diffusion coefficient and $\phi$ is the neutron flux. 

$$ J = - D(r)\nabla\phi(r) $$

This can be substituted into the full form of the balance equation to form the neutron diffusion equation: 

$$ - \nabla \cdot D(r)\nabla\phi(r) + \Sigma_{a} (r) \phi(r) = S(r) + \nu \Sigma_f (r) \phi (r) $$

It is therefore also important to have an accurate method of calculation for the diffusion coefficient. The diffusion coefficient is equal to 1/3 multiplied by the inverse of the transport cross section. For simple neutron diffusion cases involving isotropic scatter, the transport cross section can be set to be equal to the total cross section, which is equal to the sum of the absorption and scattering (including self-scatter) cross sections. This can be seen below where $\Sigma_{tr}$ is the transport cross section, $\Sigma_{t}$ is the total cross section, $\Sigma_{a}$ is the absorption cross section and $\Sigma_{s}$ is the scattering cross section. \par

$$ D =  \frac{1}{3\Sigma_{tr}} \simeq \frac{1}{3\Sigma_{t}} = \frac{1}{3(\Sigma_{a}+\Sigma_{s})} $$

This relation exists as the transport cross section is defined as $\Sigma_{tr} = \Sigma_{t} - \bar{\mu}\Sigma_{s}$, where $\bar{\mu}$ is the average cosine of the scattering angle. This has a value of $0$ in the laboratory system for isotropic scatter, so the transport and total cross sections can be approximated to one another in this case. 

This equation effectively relates the rate of change of neutrons within a system to a number of material properties and the flux, and hence can be solved with knowledge of the materials involved and the use of mathematical solvers. An example of the neutron diffusion equation can be seen below, for a 1-D slab reactor at steady state, where $\lambda$ is the eigenvalue of the system, $\nu$ is the average neutrons produced per fission and $\Sigma_f$ is the fission cross section. In this example a fission source of neutrons is being used instead of a simple volumetric source. \par

$$ -\frac{d}{d x} D(x) \frac{d \phi(x)}{d x}+\Sigma_{a}(x) \phi(x)=\frac{1}{\lambda} \nu \Sigma_{f}(x) \phi(x) $$

Neutron diffusion codes will principally solve this equation to calculate the neutron flux over a specified geometry. Problems involving a fission neutron source can be solved for the eigenvalue $\lambda$ of the system, utilising a fission source normalisation.




