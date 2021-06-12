## ====================
## ROI plugin: Gurobi
## Author: Florian Schwendinger
## Date  : 11/17/2016
## Author: Kuo LIu
## Date  : 12/16/2015
## ====================

gurobi_status_code <- list(
    LOADED = 1L,
    OPTIMAL = 2L,
    INFEASIBLE = 3L,
    INF_OR_UNBD = 4L,
    UNBOUNDED = 5L,
    CUTOFF = 6L,
    ITERATION_LIMIT = 7L,
    NODE_LIMIT = 8L,
    TIME_LIMIT = 9L,
    SOLUTION_LIMIT = 10L,
    INTERRUPTED = 11L,
    NUMERIC = 12L,
    SUBOPTIMAL = 13L,
    INPROGRESS = 14L,
    USER_OBJ_LIMIT = 15L
)

## get_lb
## ======
## get lower bound constraints
get_lb <- function(x) {
    if ( is.null(bounds(x)) )
        return( NULL )
    if( !length(bounds(x)$lower$val) )
        return( NULL )
    lb <- numeric( length(x$objective) )
    lb[ bounds(x)$lower$ind ] <- bounds(x)$lower$val
    return(lb)
}

## get_ub
## ======
## get upper bound constraints
get_ub <- function(x) {
    if ( is.null(bounds(x)) )
        return( NULL )
    if( !length(bounds(x)$upper$val) ) 
        return( NULL )
    ub <- rep.int(Inf, length(x$objective))
    ub[ bounds(x)$upper$ind ] <- bounds(x)$upper$val
    return(ub)
}

## NOTE: Gurobi requires sense to be element of 
##       c("<", "<=", ">", ">=", "=")
map_dir <- function(x) {
    x[x == "=="] <- "="
    x
}

is.LP <- function(x) {
    ( ( is.NO_constraint(constraints(x)) | is.L_constraint(constraints(x)) )
      & inherits( objective(x), "L_objective" ) )
}

solve_OP <- function(x, control = list()) {
    multiple_solutions <- FALSE
    if ("nsol_max" %in% names(control)) {
        stopifnot(is.numeric(control$nsol_max), lengths(is.numeric(control$nsol_max)) == 1L)
        nsol_max <- control$nsol_max
        control$nsol_max <- NULL
        if (isTRUE(nsol_max > 1L)) {
            multiple_solutions <- TRUE
            control[["PoolSearchMode"]] <- 2L
            if (is.finite(nsol_max)) {
                control[["PoolSolutions"]] <- as.integer(nsol_max)
            }
        }
    }

    if ( is.LP(x) ) {
        out <- .solve_LP( x, control )
    } else {
        out <- .solve_QP( x, control )
    }

    if (multiple_solutions) {
        delta <- abs(out$objval - sapply(out$pool, "[[", "objval"))
        pool_gap <- if (is.null(control$PoolGap)) 1e-5 else control$PoolGap
        is_optimal <- (delta / abs(out$objval)) < pool_gap
        pool <- out$pool[is_optimal]
        sol <- ROI_plugin_canonicalize_solution(solution = out$x,
                                                optimum  = out$objval,
                                                status   = gurobi_status_code[[out$status]],
                                                solver   = "gurobi",
                                                message  = out)
        solutions <- vector("list", length(pool))
        for (i in seq_along(solutions)) {
            sol[["solution"]] <- pool[[i]][["xn"]]
            sol[["optimum"]] <- pool[[i]][["objval"]]
            solutions[[i]] <- sol
        }
        class(solutions) <- c("lpsolve_solution_set", "OP_solution_set")
        return(solutions)
    } else {
        ROI_plugin_canonicalize_solution(solution = out$x,
                                         optimum  = out$objval,
                                         status   = gurobi_status_code[[out$status]],
                                         solver   = "gurobi",
                                         message  = out )
    }
}

canonicalize_control <- function(x) {
    x$OutputFlag <- if ( is.null(x$OutputFlag) ) 0L else as.integer(x$OutputFlag)
    x
}

.solve_LP <- function(x, control) {
    model <- list()
    ## objective
    model$obj <- as.vector(terms(objective(x))[["L"]])

    ## constraints
    model$A <- constraints(x)$L
    model$sense <- map_dir(constraints(x)$dir)
    model$rhs <- constraints(x)$rhs
    
    ## types
    model$vtype <- ROI::types(x)

    ## bounds
    model$lb <- get_lb(x)
    model$ub <- get_ub(x)

    ## maximum
    model$modelsense <- if(x$maximum) "max" else "min"

    gurobi(model, canonicalize_control(control))
}

.emtpy_Q <- function(x) {
    if ( is.null(x) )
        return( TRUE )
    if ( is.simple_triplet_matrix(x) ) {
        if ( length(x$i) == 0 )
            return(TRUE)
    }
    FALSE
}

.solve_QP <- function(x, control = list()) {
    model <- list()
    ## objective
    model$Q <- terms(objective(x))[['Q']] / 2
    model$obj <- as.vector(terms(objective(x))[['L']])

    ## linear constraints
    stopifnot( all(constraints(x)$dir %in% c("<=", ">=", "==")) )
    if ( is.NO_constraint(constraints(x)) ) {
        ## dummy constraint since one is needed!
        model$A <- matrix(0, 1, length(objective(x)))
        model$sense <- "<="
        model$rhs <- 99
    } else if ( is.L_constraint(constraints(x)) ) {
        model$A <- constraints(x)$L
        model$rhs <- constraints(x)$rhs
        model$sense <- map_dir(constraints(x)$dir)
    } else { ## quadratic constraints        
        lc <- sapply(constraints(x)$Q, .emtpy_Q)
        if ( any(lc) ) {
            model$A <- constraints(x)$L[lc,]
            model$rhs <- constraints(x)$rhs[lc]
            model$sense <- map_dir(constraints(x)$dir[lc])
        } else {
            ## dummy constraint since one is needed!
            model$A <- matrix(0, 1, length(objective(x)))
            model$sense <- "<="
            model$rhs <- 99
        }
        con.dir <- constraints(x)$dir[!lc]
        con.Q <- constraints(x)$Q[!lc]
        con.L <- constraints(x)$L[!lc, ]
        con.rhs <- constraints(x)$rhs[!lc]
        nc <- 2 * sum(con.dir == "==") + sum(con.dir != "==")
        qc <- vector("list", nc)
        counter <- 1L
        for (i in seq_along(qc)) {
            ## NOTE: for "==" we have to add 2 constraints therefore no if else
            if ( con.dir[i] %in% c("<=", "==") ) {
                qc[[counter]] <- list(Qc = as.simple_triplet_matrix(con.Q[[i]])/2, 
                                      q = as.vector(con.L[i,]), rhs=con.rhs[i])
                counter <- counter + 1L
            }
            if ( con.dir[i] %in% c(">=", "==") ) {
                qc[[counter]] <- list(Qc = as.simple_triplet_matrix(-con.Q[[i]])/2, 
                                      q = -as.vector(con.L[i,]), rhs=-con.rhs[i])
                counter <- counter + 1L
            }
        }
        model$quadcon <- qc
    }

    ## types
    model$vtype <- ROI::types(x)

    ## bounds
    model$lb <- get_lb(x)
    model$ub <- get_ub(x)

    ## maximum
    model$modelsense <- if(x$maximum) "max" else "min"
    
    gurobi(model, canonicalize_control(control))
}

