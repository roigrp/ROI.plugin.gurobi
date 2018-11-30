
.add_controls <- function(solver) {
    ROI_plugin_register_solver_control( solver, "BarIterLimit", "X" )    ## Barrier iteration limit
    ROI_plugin_register_solver_control( solver, "Cutoff", "X" )    ## Objective cutoff
    ROI_plugin_register_solver_control( solver, "IterationLimit", "X" )    ## Simplex iteration limit
    ROI_plugin_register_solver_control( solver, "NodeLimit", "X" )    ## MIP node limit
    ROI_plugin_register_solver_control( solver, "SolutionLimit", "X" )    ## MIP feasible solution limit
    ROI_plugin_register_solver_control( solver, "TimeLimit", "X" )    ## Time limit
    ROI_plugin_register_solver_control( solver, "BestObjStop", "X" )    ## Best objective value to stop
    ROI_plugin_register_solver_control( solver, "BestBdStop", "X" )    ## Best objective bound to stop
    ROI_plugin_register_solver_control( solver, "BarConvTol", "X" )    ## Barrier convergence tolerance
    ROI_plugin_register_solver_control( solver, "BarQCPConvTol", "X" )    ## Barrier QCP convergence tolerance
    ROI_plugin_register_solver_control( solver, "FeasibilityTol", "X" )    ## Primal feasibility tolerance
    ROI_plugin_register_solver_control( solver, "IntFeasTol", "X" )    ## Integer feasibility tolerance
    ROI_plugin_register_solver_control( solver, "MarkowitzTol", "X" )    ## Threshold pivoting tolerance
    ROI_plugin_register_solver_control( solver, "MIPGap", "X" )    ## Relative MIP optimality gap
    ROI_plugin_register_solver_control( solver, "MIPGapAbs", "X" )    ## Absolute MIP optimality gap
    ROI_plugin_register_solver_control( solver, "OptimalityTol", "X" )    ## Dual feasibility tolerance
    ROI_plugin_register_solver_control( solver, "PSDTol", "X" )    ## Positive semi-definite tolerance
    ROI_plugin_register_solver_control( solver, "InfUnbdInfo", "X" )    ## Generate additional info for infeasible/unbounded models
    ROI_plugin_register_solver_control( solver, "NormAdjust", "X" )    ## Simplex pricing norm
    ROI_plugin_register_solver_control( solver, "ObjScale", "X" )    ## Objective scaling
    ROI_plugin_register_solver_control( solver, "PerturbValue", "X" )    ## Simplex perturbation magnitude
    ROI_plugin_register_solver_control( solver, "Quad", "X" )    ## Quad precision computation in simplex
    ROI_plugin_register_solver_control( solver, "ScaleFlag", "X" )    ## Model scaling
    ROI_plugin_register_solver_control( solver, "Sifting", "X" )    ## Sifting within dual simplex
    ROI_plugin_register_solver_control( solver, "SiftMethod", "X" )    ## LP method used to solve sifting sub-problems
    ROI_plugin_register_solver_control( solver, "SimplexPricing", "X" )    ## Simplex variable pricing strategy
    ROI_plugin_register_solver_control( solver, "BarCorrectors", "X" )    ## Central correction limit
    ROI_plugin_register_solver_control( solver, "BarHomogeneous", "X" )    ## Barrier homogeneous algorithm
    ROI_plugin_register_solver_control( solver, "BarOrder", "X" )    ## Barrier ordering algorithm
    ROI_plugin_register_solver_control( solver, "Crossover", "X" )    ## Barrier crossover strategy
    ROI_plugin_register_solver_control( solver, "CrossoverBasis", "X" )    ## Crossover initial basis construction strategy
    ROI_plugin_register_solver_control( solver, "QCPDual", "X" )    ## Compute dual variables for QCP models
    ROI_plugin_register_solver_control( solver, "BranchDir", "X" )    ## Branch direction preference
    ROI_plugin_register_solver_control( solver, "DegenMoves", "X" )    ## Degenerate simplex moves
    ROI_plugin_register_solver_control( solver, "ConcurrentJobs", "X" )    ## Enables distributed concurrent solver
    ROI_plugin_register_solver_control( solver, "ConcurrentMIP", "X" )    ## Enables concurrent MIP solver
    ROI_plugin_register_solver_control( solver, "ConcurrentSettings", "X" )    ## Comma-separated list of .prm files - used to create concurrent environments
    ROI_plugin_register_solver_control( solver, "Disconnected", "X" )    ## Disconnected component strategy
    ROI_plugin_register_solver_control( solver, "DistributedMIPJobs", "X" )    ## Enables the distributed MIP solver
    ROI_plugin_register_solver_control( solver, "Heuristics", "X" )    ## Turn MIP heuristics up or down
    ROI_plugin_register_solver_control( solver, "ImproveStartGap", "X" )    ## Trigger solution improvement
    ROI_plugin_register_solver_control( solver, "ImproveStartNodes", "X" )    ## Trigger solution improvement
    ROI_plugin_register_solver_control( solver, "ImproveStartTime", "X" )    ## Trigger solution improvement
    ROI_plugin_register_solver_control( solver, "MinRelNodes", "X" )    ## Minimum relaxation heuristic control
    ROI_plugin_register_solver_control( solver, "MIPFocus", "X" )    ## Set the focus of the MIP solver
    ROI_plugin_register_solver_control( solver, "MIQCPMethod", "X" )    ## Method used to solve MIQCP models
    ROI_plugin_register_solver_control( solver, "NodefileDir", "X" )    ## Directory for MIP node files
    ROI_plugin_register_solver_control( solver, "NodefileStart", "X" )    ## Memory threshold for writing MIP tree nodes to disk
    ROI_plugin_register_solver_control( solver, "NodeMethod", "X" )    ## Method used to solve MIP node relaxations
    ROI_plugin_register_solver_control( solver, "PumpPasses", "X" )    ## Feasibility pump heuristic control
    ROI_plugin_register_solver_control( solver, "RINS", "X" )    ## RINS heuristic
    ROI_plugin_register_solver_control( solver, "SolutionNumber", "X" )    ## Sub-optimal MIP solution retrieval
    ROI_plugin_register_solver_control( solver, "SubMIPNodes", "X" )    ## Nodes explored by sub-MIP heuristics
    ROI_plugin_register_solver_control( solver, "Symmetry", "X" )    ## MIP symmetry detection
    ROI_plugin_register_solver_control( solver, "VarBranch", "X" )    ## Branch variable selection strategy
    ROI_plugin_register_solver_control( solver, "ZeroObjNodes", "X" )    ## Zero objective heuristic control
    ROI_plugin_register_solver_control( solver, "TuneCriterion", "X" )    ## Specify tuning criterion
    ROI_plugin_register_solver_control( solver, "TuneJobs", "X" )    ## Enables distributed tuning
    ROI_plugin_register_solver_control( solver, "TuneOutput", "X" )    ## Tuning output level
    ROI_plugin_register_solver_control( solver, "TuneResults", "X" )    ## Number of improved parameter sets returned
    ROI_plugin_register_solver_control( solver, "TuneTimeLimit", "X" )    ## Time limit for tuning
    ROI_plugin_register_solver_control( solver, "TuneTrials", "X" )    ## Perform multiple runs on each parameter set to limit the effect of random noise
    ROI_plugin_register_solver_control( solver, "PoolSearchMode", "X" )    ## Choose the approach used to find additional solutions
    ROI_plugin_register_solver_control( solver, "PoolGap", "X" )    ## Gap for solutions in pool
    ROI_plugin_register_solver_control( solver, "PoolSolutions", "X" )    ## Number of solutions to keep in pool
    ROI_plugin_register_solver_control( solver, "Cuts", "X" )    ## Global cut generation control
    ROI_plugin_register_solver_control( solver, "CliqueCuts", "X" )    ## Clique cut generation
    ROI_plugin_register_solver_control( solver, "CoverCuts", "X" )    ## Cover cut generation
    ROI_plugin_register_solver_control( solver, "FlowCoverCuts", "X" )    ## Flow cover cut generation
    ROI_plugin_register_solver_control( solver, "FlowPathCuts", "X" )    ## Flow path cut generation
    ROI_plugin_register_solver_control( solver, "GUBCoverCuts", "X" )    ## GUB cover cut generation
    ROI_plugin_register_solver_control( solver, "ImpliedCuts", "X" )    ## Implied bound cut generation
    ROI_plugin_register_solver_control( solver, "MIPSepCuts", "X" )    ## MIP separation cut generation
    ROI_plugin_register_solver_control( solver, "MIRCuts", "X" )    ## MIR cut generation
    ROI_plugin_register_solver_control( solver, "StrongCGCuts", "X" )    ## Strong-CG cut generation
    ROI_plugin_register_solver_control( solver, "ModKCuts", "X" )    ## Mod-k cut generation
    ROI_plugin_register_solver_control( solver, "NetworkCuts", "X" )    ## Network cut generation
    ROI_plugin_register_solver_control( solver, "ProjImpliedCuts", "X" )    ## Projected implied bound cut generation
    ROI_plugin_register_solver_control( solver, "SubMIPCuts", "X" )    ## Sub-MIP cut generation
    ROI_plugin_register_solver_control( solver, "ZeroHalfCuts", "X" )    ## Zero-half cut generation
    ROI_plugin_register_solver_control( solver, "InfProofCuts", "X" )    ## Infeasibility proof cut generation
    ROI_plugin_register_solver_control( solver, "CutAggPasses", "X" )    ## Constraint aggregation passes performed during cut generation
    ROI_plugin_register_solver_control( solver, "CutPasses", "X" )    ## Root cutting plane pass limit
    ROI_plugin_register_solver_control( solver, "GomoryPasses", "X" )    ## Root Gomory cut pass limit
    ROI_plugin_register_solver_control( solver, "WorkerPassword", "X" )    ## Password for distributed workers
    ROI_plugin_register_solver_control( solver, "WorkerPool", "X" )    ## List of available distributed workers
    ROI_plugin_register_solver_control( solver, "WorkerPort", "X" )    ## Non-default port number for distributed workers
    ROI_plugin_register_solver_control( solver, "AggFill", "X" )    ## Allowed fill during presolve aggregation
    ROI_plugin_register_solver_control( solver, "Aggregate", "X" )    ## Presolve aggregation control
    ROI_plugin_register_solver_control( solver, "DisplayInterval", "X" )    ## Frequency at which log lines are printed
    ROI_plugin_register_solver_control( solver, "DualReductions", "X" )    ## Disables dual reductions in presolve
    ROI_plugin_register_solver_control( solver, "FeasRelaxBigM", "X" )    ## Big-M value for feasibility relaxations
    ROI_plugin_register_solver_control( solver, "IISMethod", "X" )    ## IIS method
    ROI_plugin_register_solver_control( solver, "InputFile", "X" )    ## File to be read before optimization commences
    ROI_plugin_register_solver_control( solver, "LazyConstraints", "X" )    ## Programs that add lazy constraints must set this parameter
    ROI_plugin_register_solver_control( solver, "LogFile", "X" )    ## Log file name
    ROI_plugin_register_solver_control( solver, "LogToConsole", "X" )    ## Console logging
    ROI_plugin_register_solver_control( solver, "Method", "X" )    ## Algorithm used to solve continuous models
    ROI_plugin_register_solver_control( solver, "MultiObjMethod", "X" )    ## Warm-start method to solve for subsequent objectives
    ROI_plugin_register_solver_control( solver, "MultiObjPre", "X" )    ## Initial presolve on multi-objective models
    ROI_plugin_register_solver_control( solver, "NumericFocus", "X" )    ## Set the numerical focus
    ROI_plugin_register_solver_control( solver, "ObjNumber", "X" )    ## Set index of multi-objectives
    ROI_plugin_register_solver_control( solver, "OutputFlag", "verbose" )    ## Solver output control
    ROI_plugin_register_solver_control( solver, "PreCrush", "X" )    ## Allows presolve to translate constraints on the original model to equivalent constraints on the presolved model
    ROI_plugin_register_solver_control( solver, "PreDepRow", "X" )    ## Presolve dependent row reduction
    ROI_plugin_register_solver_control( solver, "PreDual", "X" )    ## Presolve dualization
    ROI_plugin_register_solver_control( solver, "PreMIQCPForm", "X" )    ## Format of presolved MIQCP model
    ROI_plugin_register_solver_control( solver, "PrePasses", "X" )    ## Presolve pass limit
    ROI_plugin_register_solver_control( solver, "PreQLinearize", "X" )    ## Presolve Q matrix linearization
    ROI_plugin_register_solver_control( solver, "Presolve", "X" )    ## Presolve level
    ROI_plugin_register_solver_control( solver, "PreSOS1BigM", "X" )    ## Controls SOS1 converstion to binary form
    ROI_plugin_register_solver_control( solver, "PreSOS2BigM", "X" )    ## Controls SOS2 converstion to binary form
    ROI_plugin_register_solver_control( solver, "PreSparsify", "X" )    ## Presolve sparsify reduction
    ROI_plugin_register_solver_control( solver, "Record", "X" )    ## Enable API call recording
    ROI_plugin_register_solver_control( solver, "ResultFile", "X" )    ## Result file written upon completion of optimization
    ROI_plugin_register_solver_control( solver, "Seed", "X" )    ## Modify the random number seed
    ROI_plugin_register_solver_control( solver, "Threads", "X" )    ## Number of parallel threads to use
    ROI_plugin_register_solver_control( solver, "UpdateMode", "X" )    ## Change the behavior of lazy updates
    invisible( TRUE )
}
