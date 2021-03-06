# Tensor: A System for Procedural Generation of Street Maps
Created by Will Clarkson, Marcella Hastings, and Nate Tenczar.

## Usage
Try out `tensor` by running:

```tensor <constraints-file> <width> <height> <placement-method>```

There are some example constraints files in `json_files`. Valid placement
methods are currently `random` and `furthest`.

## Resources
### Interactive Procedural Street Modeling ([pdf][wonka])
The main paper that we're working from. Their code is [here][wonka-code].

[wonka]: http://www.sci.utah.edu/~chengu/street_sig08/street_sig08.pdf
[wonka-code]: http://www.sci.utah.edu/~chengu/street/streetmodeling.zip

### Farthest Point Seeding for Efficient Placement of Streamlines ([pdf][streamlines])
A paper on calculating and placing well-spaced streamlines which should be useful.

[streamlines]: ftp://ftp-sop.inria.fr/geometrica/alliez/streamlines.pdf

### The Topology of Symmetric, Second-Order Tensor Fields ([pdf][tensor-topo])
Some information on representing and visualizing tensor fields that could be
potentially useful.

[tensor-topo]: http://www.inf.ethz.ch/personal/peikert/SciVis/Literature/DelmarcelleHesselink94.pdf

### Tensor Visualization ([pdf][tensor-viz])
Some pretty straightforward slides on understanding tensors, hyperstreamlines, etc.

[tensor-viz]: http://www.inf.ed.ac.uk/teaching/courses/vis/lecture_notes/lecture14.pdf

### A Brief Introduction to Tensors and their properties ([html][tensor-props])
These are some general notes with useful formulas about operations on tensors
and vectors. Includes some information about calculating eigenvectors that
shoudld be useful.

[tensor-props]: http://www.brown.edu/Departments/Engineering/Courses/En221/Notes/Tensors/Tensors.htm

### Finding Eigenvectors ([pdf][eigenvectors])
The 4th page of these notes goes through the formula for the calculation of
the eigenvectors of a 2x2 tensor.

[eigenvectors]: https://bearspace.baylor.edu/Vince_Cronin/www/PBO_ed/FindingEigenvectors.pdf
