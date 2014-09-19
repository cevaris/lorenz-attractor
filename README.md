# Lorenz Attractor


A Haskell OpenGL/GLUT implementation of a Lorenz System


## Installation
	# Build/Compile project
	make
	## Execute application
	./LorenzAttractor

  
### Key Bindings

| Command         | Key Binding   |
| --------------- |:-------------:|
| Close App       | ESC           |
| Rotate Right    | Right Arrow   |
| Rotate Left     | Left Arrow    |
| Rotate Down     | Down Arrow    |
| Rotate Up       | Up Arrow      |
| Zoom In         | z             |
| Zoom Out        | Shift + z     |
| Increase Sigma  | s             |
| Decrease Sigma  | Shift + s     |
| Increase Beta   | b             |
| Decrease Beta   | Shift + b     |
| Increase Rho    | r             |
| Decrease Rho    | Shift + r     |
| Increase DT     | d             |
| Decrease DT     | Shift + d     |
| Increase Steps  | p             |
| Decrease Steps  | Shift + p     |

#### Steps
Number of points or iterations of the Lorenz System

#### DT
The distance of each step in the Lorenz System discrete equation.


### Sources

- Haskell OpenGL
  - http://www.cprogramming.com/tutorial/opengl_projections.html
  - https://github.com/haskell-opengl/GLUT 
  - https://github.com/haskell-opengl/GLUT/blob/master/examples/Misc/Gears.hs
  - https://github.com/ghorn/not-gloss-lorentz/blob/master/src/Main.hs
- Makefile
  - https://github.com/jrahm/HaskellGL4/blob/master/Makefile