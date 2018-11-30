\name{Example-1}
\title{Linear Problem 1}

\description{
\deqn{maximize \ \ 2 x_1 + 4 x_2 + 3 x_3}
\deqn{subject \ to:}
\deqn{3 x_1 + 4 x_2 + 2 x_3 \leq 60}
\deqn{2 x_1 +   x_2 + 2 x_3 \leq 40}
\deqn{  x_1 + 3 x_2 + 2 x_3 \leq 80}
\deqn{x_1, x_2, x_3 \geq 0}
}

\examples{

library(ROI)
mat <- matrix(c(3, 4, 2,
                2, 1, 2,
                1, 3, 2), nrow=3, byrow=TRUE)
x <- OP(objective = c(2, 4, 3),
        constraints = L_constraint(L = mat,
                                   dir = c("<=", "<=", "<="),
                                   rhs = c(60, 40, 80)),
        maximum = TRUE)


opt <- ROI_solve(x, solver = "gurobi")
opt
## Optimal solution found.
## The objective value is: 7.666667e+01
solution(opt)
## [1]  0.000000  6.666667 16.666667

}

