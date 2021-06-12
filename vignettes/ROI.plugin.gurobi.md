# ROI.plugin.gurobi Examples


```r
Sys.setenv("ROI_LOAD_PLUGINS" = FALSE)
suppressPackageStartupMessages(library("ROI"))
library("ROI.plugin.gurobi")
```

## Linear programming (LP)

\[
\begin{array}{rrrrr}
\text{minimize}
& 7 x_1 & + & 8 x_2 \\
\text{subject to}
& 3 x_1 & + & 4 x_2 &  =   9 \\
& 2 x_1 & + & 1 x_2 & \geq 3
\end{array}
\]

$$ -100 \leq x_1, x_2, \leq 100$$


```r
lp  <- OP(objective = L_objective(c(7, 8), names=c("x", "y")),
          constraints = L_constraint(L = rbind(c(3, 4), c(2, 1)), 
                                     dir = c("==", ">="), rhs = c(9, 3)),
          bounds = V_bound(li = 1:2, ui = 1:2, 
                           lb = c(-100, -100), ub = c(100, 100)))
ROI_applicable_solvers(lp)
```

```
## [1] "gurobi"
```

```r
(sol <- ROI_solve(lp, solver = "gurobi"))
```

```
## Optimal solution found.
## The objective value is: 1.860000e+01
```

```r
solution(sol)
```

```
##   x   y 
## 0.6 1.8
```

## Mixed integer linear programming (MILP)
$$\begin{array}{rrrrrrr}
\text{maximize}
& 7 x_1 & + & 3 x_2 & + & 1 x_3 & \\
\text{subject to}
& 6 x_1 & + & 4 x_2 & + & 5 x_3 & \leq 60 \\
& 8 x_1 & + &   x_2 & + & 2 x_3 & \leq 80 \\
& 9 x_1 & + & 1 x_2 & + & 7 x_3 & \leq 70 
\end{array}
$$
$$x_1, x_3 \in \mathbb{Z}_{\geq 0}$$
$$x_2 \geq 0$$


```r
A <- rbind(c(6, 4, 5), c(8, 0, 2), c(9, 1, 7))
milp <- OP(objective = L_objective(c(7, 1, 3), c("x", "y", "z")),
           constraints = L_constraint(L = rbind(c(6, 4, 5), c(8, 0, 2), c(9, 1, 7)),
                                      dir = c("<=", "<=", "<="),
                                      rhs = c(60, 80, 70)),
           types = c("I", "C", "I"), 
           maximum = TRUE)
(sol <- ROI_solve(milp))
```

```
## Optimal solution found.
## The objective value is: 5.350000e+01
```

```r
solution(sol)
```

```
##   x   y   z 
## 7.0 4.5 0.0
```


## Quadratic programming
### Quadratic objective with linear constraints (QP)
$$
\text{minimize} \ \ x_1 + 2x_2 + 3x_3 +  \frac{1}{2} (x_1^2 + x_2^2 + x_3^2)
$$
$$
\begin{array}{rrrrrrrr}
\text{subject to} & x_1 & + &  x_2 &   &      & \geq & 1 \\
                  &     &   &  x_2 & + &  x_3 & \geq & 2 \\
                  & x_1 &   &      & + &  x_3 & \geq & 3
\end{array}
$$


```r
qp <- OP(Q_objective(diag(3), c(1, 2, 3), c("x", "y", "z")),
         L_constraint(L = rbind(c(1, 1, 0), c(0, 1, 1), c(1, 0, 1)), 
                      dir = c(">=", ">=", ">="), rhs = c(1, 2, 3)))
(sol <- ROI_solve(qp, solver = "gurobi"))
```

```
## Optimal solution found.
## The objective value is: 9.333333e+00
```

```r
solution(sol)
```

```
##         x         y         z 
## 1.3333333 0.3333333 1.6666667
```

### Quadratic objective with quadratic constraints (QCQP)
$$
\text{maximize} \ \ 90 x_1 + 110 x_2 + 160 x_3 - \frac{1}{2} (x_1^2 + x_2^2 + x_3^2)
$$
$$
\begin{array}{rrrrr}
\text{subject to} & x_1^2 + x_2^2 + 4 x_3     & \leq & 4 & \\
                  & x_2^2 + x_3^2 + x_1 + x_3 & \leq & 3 & \\
                  & x_1^2 + x_3^2 + 2 x_1 x_3 & \leq & 2 & \\
                  & x_1, x_2, x_3 \geq 0      &      &   &
\end{array}
$$


```r
qcqp <- OP(Q_objective(-diag(3), c(90, 110, 160), c("x", "y", "z")),
           Q_constraint(Q = list(rbind(c(2, 0, 0), c(0, 2, 0), c(0, 0, 0)),
                                 rbind(c(0, 0, 0), c(0, 2, 0), c(0, 0, 2)),
                                 rbind(c(2, 0, 2), c(0, 0, 0), c(2, 0, 2))),
                        L = rbind(c(0, 0, 4), c(1, 0, 1), c(0, 0, 0)),
                        dir = rep("<=", 3), rhs = c(4, 3, 2)),
           maximum = TRUE)
```

```r
(sol <- ROI_solve(qcqp, solver = "gurobi"))
```

```
## Optimal solution found.
## The objective value is: 2.836219e+02
```

```r
solution(sol)
```

```
##         x         y         z 
## 1.0606575 1.2086302 0.3535547
```

### Non-Convex Quadratic objective with quadratic constraints (QCQP)
If we change the sign of the quadratic terms the problem change from a convex
to a non-convex problem. To be able to solve the new problem we have
to set the parameter `NonConvex` equal to `2`.
$$
\text{maximize} \ \ 90 x_1 + 110 x_2 + 160 x_3 + \frac{1}{2} (x_1^2 + x_2^2 + x_3^2)
$$
$$
\begin{array}{rrrrr}
\text{subject to} & x_1^2 + x_2^2 + 4 x_3     & \leq & 4 & \\
                  & x_2^2 + x_3^2 + x_1 + x_3 & \leq & 3 & \\
                  & x_1^2 + x_3^2 + 2 x_1 x_3 & \leq & 2 & \\
                  & x_1, x_2, x_3 \geq 0      &      &   &
\end{array}
$$


```r
qcqp <- OP(Q_objective(diag(3), c(90, 110, 160), c("x", "y", "z")),
           Q_constraint(Q = list(rbind(c(2, 0, 0), c(0, 2, 0), c(0, 0, 0)),
                                 rbind(c(0, 0, 0), c(0, 2, 0), c(0, 0, 2)),
                                 rbind(c(2, 0, 2), c(0, 0, 0), c(2, 0, 2))),
                        L = rbind(c(0, 0, 4), c(1, 0, 1), c(0, 0, 0)),
                        dir = rep("<=", 3), rhs = c(4, 3, 2)),
           maximum = TRUE)
(sol <- ROI_solve(qcqp, solver = "gurobi", NonConvex = 2L))
```

```
## Optimal solution found.
## The objective value is: 2.863326e+02
```

```r
solution(sol)
```

```
##         x         y         z 
## 1.0606602 1.2086300 0.3535534
```

#### Binding constraints
The check below shows that all three constraints are binding.

```r
s <- solution(sol)
sapply(as.F_constraint(constraints(qcqp))$F, function(fun) fun(s))
```

```
## [1] 4 3 2
```

