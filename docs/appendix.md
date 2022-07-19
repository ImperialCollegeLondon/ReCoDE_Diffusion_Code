# Appendix A - Theory

## Neutron Diffusion Theory

In the design of any nuclear reactor, it is critical to understand the distribution of neutrons throughout the system. As neutrons move through various parts of the reactor such as the fuel or the moderator, their behaviour will greatly change based on the medium in which they find themselves. This neutron behaviour can be most simply approximated as a form of diffusion occurring within the reactor, producing a solvable equation which can be used to accurately describe this behaviour. The analysis and solving of this equation is known as neutron diffusion theory, and is a critical area of nuclear physics and engineering, used widely in the industry to calculate neutron flux profiles and multiplication factors within a reactor.

The first step in generating this equation is to describe the spatial neutron balance within a volume $dV$, where $dV = dx dy dz$, centred at $r$, where $r = (x,y,z)$. Assuming steady state:

$$ \text{neutrons lost in } dV/s = \text{neutrons produced in } dV/s$$

Splitting these into the separate sources and sinks:

$$
\begin{aligned}
& \text{neutrons leaking from } dV/s  \text{neutrons absorbed in } dV/s = \\
& \text{neutrons emitted in } dV/s  + \text{fission neutrons produced in } dV/s
\end{aligned}
$$

Where:

$$ \text{neutrons leaking from } dV/s = \left( \frac{\partial}{\partial x} J_x (x,y,z) + \frac{\partial}{\partial y} J_y (x,y,z) + \frac{\partial}{\partial z} J_z (x,y,z) \right) dV$$

$$\text{neutrons absorbed in } dV/s = \Sigma_a (x,y,z) \phi(x,y,z) dV$$

$$ \text{neutrons emitted in } dV/s = S (x,y,z) dV$$

$$\text{fission neutrons produced in } dV/s = \nu \Sigma_F (x,y,z) \phi(x,y,z) dV$$

Combining these and eliminating $dV$ gives:

$$ \frac{\partial}{\partial x} J*x (r) + \frac{\partial}{\partial y} J_y (r) + \frac{\partial}{\partial z} J_x (z) + \Sigma*{a} (r) \phi(r) = S(r) + \nu \Sigma_f (r) \phi (r) $$

Given the vector definitions:

$$ J(r) = \hat{i} J_x (r) + \hat{j} J_y (r) + \hat{k} J_z (r) \text{ and } \nabla = \hat{i} \frac{\partial}{\partial x} + \hat{j} \frac{\partial}{\partial y} + \hat{k} \frac{\partial}{\partial z} $$

We can write the balance equation as:

$$ \nabla \cdot J(r) + \Sigma\_{a} (r) \phi(r) = S(r) + \nu \Sigma_f (r) \phi (r) $$

Given that we are using diffusion to describe the behaviour of neutrons within a reactor, we can therefore make use of Fick's Law, which states that any solute (the neutrons) will diffuse from an area of high concentration (high neutron flux) to an area of low concentration. This law gives an equation that will relate the current in a system to the concentration gradient multiplied by a diffusion coefficient. In the simplest 1-D reactor case the current of neutrons will therefore be given by the negative of a diffusion coefficient multiplied by the gradient of the flux. A general form of this can be seen below, where $J$ is the neutron current (or diffusion flux), $D$ is the diffusion coefficient and $\phi$ is the neutron flux.

$$ J = - D(r)\nabla\phi(r) $$

This can be substituted into the full form of the balance equation to form the neutron diffusion equation:

$$ - \nabla \cdot D(r)\nabla\phi(r) + \Sigma_{a} (r) \phi(r) = S(r) + \nu \Sigma_f (r) \phi (r) $$

It is therefore also important to have an accurate method of calculation for the diffusion coefficient. The diffusion coefficient is equal to 1/3 multiplied by the inverse of the transport cross section. For simple neutron diffusion cases involving isotropic scatter, the transport cross section can be set to be equal to the total cross section, which is equal to the sum of the absorption and scattering (including self-scatter) cross sections. This can be seen below where $\Sigma_{tr}$ is the transport cross section, $\Sigma_{t}$ is the total cross section, $\Sigma_{a}$ is the absorption cross section and $\Sigma_{s}$ is the scattering cross section.

$$ D = \frac{1}{3\Sigma*{tr}} \simeq \frac{1}{3\Sigma*{t}} = \frac{1}{3(\Sigma*{a}+\Sigma*{s})} $$

This relation exists as the transport cross section is defined as $\Sigma_{tr} = \Sigma_{t} - \bar{\mu}\Sigma_{s}$ , where $\bar{\mu}$ is the average cosine of the scattering angle. This has a value of $0$ in the laboratory system for isotropic scatter, so the transport and total cross sections can be approximated to one another in this case.

This equation effectively relates the rate of change of neutrons within a system to a number of material properties and the flux, and hence can be solved with knowledge of the materials involved and the use of mathematical solvers. An example of the neutron diffusion equation can be seen below, for a 1-D slab reactor at steady state, where $\lambda$ is the eigenvalue of the system, $\nu$ is the average neutrons produced per fission and $\Sigma_f$ is the fission cross section. In this example a fission source of neutrons is being used instead of a simple volumetric source.

$$ -\frac{d}{d x} D(x) \frac{d \phi(x)}{d x}+\Sigma*{a}(x) \phi(x)=\frac{1}{\lambda} \nu \Sigma*{f}(x) \phi(x) $$

Neutron diffusion codes will principally solve this equation to calculate the neutron flux over a specified geometry. Problems involving a fission neutron source can be solved for the eigenvalue $\lambda$ of the system, utilising a fission source normalisation.
