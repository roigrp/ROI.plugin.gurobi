# Installation
Before `ROI.plugin.gurobi` can be installed `gurobi` and its corresponding 
**R** package need to be installed. Both can be obtained from
[https://www.gurobi.com](https://www.gurobi.com) 
and more information about the installation can be found in the `Quick Start Guides` at
[https://www.gurobi.com/documentation](https://www.gurobi.com/documentation).    


`ROI.plugin.gurobi` was tested on `Debian GNU/Linux 8 (jessie)` with the
`gurobi` versions `6.5` and `7`.    


After `gurobi` and `gurobi` (**R** package) are installed simply use
```r
install.packages("ROI.plugin.gurobi", repos="http://R-Forge.R-project.org")
## or (recommended version)
remotes:::install_svn("svn://svn.r-forge.r-project.org/svnroot/roi/pkg/ROI.plugin.gurobi")
## or
remotes:::install_github("fl0sch/ROI.plugin.gurobi")
```
to install `ROI.plugin.gurobi`.


