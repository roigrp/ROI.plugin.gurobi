.add_status_codes <- function() {
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    ROI_plugin_add_status_code_to_db(solver,
                              1L,
                              "LOADED",
                              "Model is loaded, but no solution information is available."
                            )
    ROI_plugin_add_status_code_to_db(solver,
                              2L,
                              "OPTIMAL",
                              "Model was solved to optimality (subject to tolerances), and an optimal solution is available.",
                              0L # this is for roi_code. 0 means we have got the solution. 1L is the default value.
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              3L,
                              "INFEASIBLE",
                              "Model was proven to be infeasible."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              4L,
                              "INF_OR_UNBD",
                              "Model was proven to be either infeasible or unbounded."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              5L,
                              "UNBOUNDED",
                              "Model was proven to be unbounded."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              6L,
                              "CUTOFF",
                              "Optimal objective for model was proven to be worse than the value specified in the Cutoff parameter. No solution information is available."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              7L,
                              "ITERATION_LIMIT",
                              "Optimization terminated because the total number of simplex iterations performed exceeded the value specified in the IterationLimit parameter, or because the total number of barrier iterations exceeded the value specified in the BarIterLimit parameter."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              8L,
                              "NODE_LIMIT",
                              "Optimization terminated because the total number of branch-andcut nodes explored exceeded the value specified in the NodeLimitparameter."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              9L,
                              "TIME_LIMIT",
                              "Optimization terminated because the time expended exceeded the value specified in the TimeLimit parameter."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              10L,
                              "SOLUTION_LIMIT",
                              "Optimization terminated because the number of solutions found reached the value specified in the SolutionLimit parameter."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              11L,
                              "INTERRUPTED",
                              "Optimization was terminated by the user."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              12L,
                              "NUMERIC",
                              "Optimization was terminated due to unrecoverable numerical difficulties."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              13L,
                              "SUBOPTIMAL",
                              "Unable to satisfy optimality tolerances; a sub-optimal solution is available."
                              )
    ROI_plugin_add_status_code_to_db(solver,
                              14L,
                              "INPROGRESS",
                              "An asynchronous optimization call was made, but the associated optimization run is not yet complete."
                              )

    invisible(TRUE)
}
