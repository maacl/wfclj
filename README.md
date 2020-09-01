# wfclj

A pure Clojure implementation of the Wave Function Collapse [algorithm](https://github.com/mxgmn/WaveFunctionCollapse) invented by Maxim Gumin.

![wfc](https://github.com/maacl/wfclj/wfc.png)

This implementation is inspired by the Python version referenced in this [article](https://robertheaton.com/2018/12/17/wavefunction-collapse-algorithm/). 

## Usage

```
(print-grid
 (make-grid-and-run 20 20 input-matrix-1))
 ```
 
This will derive the weights and compatibilities from the tiles in ```input-matrix-1```, construct a 20 x 20 grid based on these, and run the algorithm on it. 

The output to the terminal should be colored. Please note that the CIDER REPL will print some "�[42ml" chars due to chunking, please see this [issue](https://github.com/clojure-emacs/cider/issues/2891).

There is another Clojure version of the WFC alogrithm [here](https://github.com/allison-casey/wavefunctioncollapse-clj)  but that "just" calls into a Java library.

## License

Apache 2.0
Copyright © 2020 Martin Clausen
