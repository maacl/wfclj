# wfclj

A Clojure implementation of the Wave Function Collapse algorithm invented by Maxim Gumin.

https://github.com/mxgmn/WaveFunctionCollapse

This implementation is inspired by the Python version referenced in this article: https://robertheaton.com/2018/12/17/wavefunction-collapse-algorithm/

## Usage

(print-grid
 (make-grid-and-run 20 20 input-matrix-1))
 
 This will derive the weights and compatibilities from the tiles in input-matrix-1, construct a 20 x 20 grid based on these, and run the algorithm on it. 

## License

Apache 2.0
Copyright © 2020 Martin Clausen
