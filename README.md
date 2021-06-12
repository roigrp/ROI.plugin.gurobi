# Installation
Before `ROI.plugin.gurobi` can be installed `gurobi` and its corresponding 
**R** package need to be installed. Both can be obtained from
[https://www.gurobi.com](https://www.gurobi.com) 
and more information about the installation can be found in the `Quick Start Guides` at
[https://www.gurobi.com/documentation](https://www.gurobi.com/documentation).    


`ROI.plugin.gurobi` was tested on `Debian GNU/Linux 10 (buster)` with the
`gurobi` version `9`.   


After `gurobi` and `gurobi` (**R** package) are installed simply use
```r
remotes:::install_github("roigrp/ROI.plugin.gurobi")
```
to install `ROI.plugin.gurobi`.

## Control Parameters
An overview on all the available Gurobi parameters can be found at
[`https://www.gurobi.com/documentation/9.1/refman/parameters.html#sec:Parameters`](https://www.gurobi.com/documentation/9.1/refman/parameters.html#sec:Parameters).

