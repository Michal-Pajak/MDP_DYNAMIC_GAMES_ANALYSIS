process_analysis <- function(no_processes,type,convergence,continuous)
{
	load_packages()

	source("MODEL_PREDATOR_PREY.R")
	# Change names for Nash equilibrium analysisi
	markov_decision_problem_analysis(no_processes,type,"ANALYSIS_SO","SO",convergence,continuous)
}


LIST_V <- list()
LIST_P <- list()
LIST_NS <- list()
LIST2_A <- list()

SOLUTION_TYPE <- "x"
IT <- 0

prepare_global_data_structures <- function()
{
	# Resting the data structures
	LIST_V <<- list()
	LIST_P <<- list()
	LIST_NS <<- list()
	LIST2_A <<- list()

	# Getting dimensions for data structures
	SD <- GET_STATES_DIMENSIONS()

	# Dummy data structures used to construct final structures
	phases <- GET_NO_PHASES()	
	T_A <- array(-1,dim=c(SD,2*phases))
	T_L <- list()

	# LIST_V (value list) stores accumulated value for each player
	# LIST_P (payoff list) stores myopic payoff for each player
	# External list iterates over players' vector
	# Internal arrays contain data for specific player for each state of the system for each iteration
	# Access to array of P: LIST[[P]]
	# Access to value of P for state vector S in timestep T LIST[[P]][v<-c(S,T)] require using do.call()
	for (ip in 1:GET_PLAYERS_NO())
	{
		temp_list <- list(T_A)
		LIST_V <<- append(LIST_V,temp_list)
		LIST_P <<- append(LIST_P,temp_list)
	}

	# LIST_H (harvest list) stores information about harvest for each element of state vector
	# LIST_NS (next state list) stores information about next state for each element of state vector
	# External list iterates over states' vector
	# Internal arrays contain data for specific state vector for each state of the system
	for (is in 1:length(SD))
	{
		temp_list <- list(T_A)
		LIST_NS <<- append(LIST_NS,temp_list)
	}

	# LIST2_A (action list) stores actions of each player for each state vector
	# External list iterates over states' vector
	# Internal lists iterate over players' vector
	# Internal arrays contain data for specific player for specific state vector for each state of the system for each iteration	
	for (is in 1:GET_STRATEGY_NO())
	{
		T_L <- list()
		for (ip in 1:GET_PLAYERS_NO())
		{
			temp_list <- list(T_A)
			T_L <- append(T_L,temp_list)
		}
		temp_list2 <- list(T_L)
		LIST2_A <<- append(LIST2_A,temp_list2)
	}
}


prepare_list_for_resultClass <- function()
{
	# Data structures for resultClass
	rc_LIST_V <- list()
	rc_LIST_P <- list()
	rc_LIST_NS <- list()
	rc_LIST2_A <- list()

	# Getting dimensions for data structures
	SD <- GET_STATES_DIMENSIONS()
	
	# Set the first element of the vector as 1 (in FOREACH calculations are done for each 'line' separately)
	SD[1] <- 1

	# Dummy data structures used to construct final structures
	# If there was only one dimension of the states T_A contains just value instead of an array)
	T_A <- array(-1,dim=c(SD))	
	T_L <- list()

	# LIST_V (value list) stores accumulated value for each player
	# LIST_P (payoff list) stores myopic payoff for each player
	# External list iterates over players' vector
	# Internal arrays contain data for specific player for each state of the system for each iteration
	# Access to array of P: LIST[[P]]
	# Access to value of P for state vector S in timestep T LIST[[P]][v<-c(S,T)] require using do.call()
	for (ip in 1:GET_PLAYERS_NO())
	{
		temp_list <- list(T_A)
		rc_LIST_V <- append(rc_LIST_V,temp_list)
		rc_LIST_P <- append(rc_LIST_P,temp_list)
	}

	# LIST_H (harvest list) stores information about harvest for each element of state vector
	# LIST_NS (next state list) stores information about next state for each element of state vector
	# External list iterates over states' vector
	# Internal arrays contain data for specific state vector for each state of the system
	for (is in 1:length(SD))
	{
		temp_list <- list(T_A)
		rc_LIST_NS <- append(rc_LIST_NS,temp_list)
	}

	# LIST2_A (action list) stores actions of each player for each state vector
	# External list iterates over states' vector
	# Internal lists iterate over players' vector
	# Internal arrays contain data for specific player for specific state vector for each state of the system for each iteration
	for (is in 1:GET_STRATEGY_NO())
	{
		T_L <- list()
		for (ip in 1:GET_PLAYERS_NO())
		{
			temp_list <- list(T_A)
			T_L <- append(T_L,temp_list)
		}
		temp_list2 <- list(T_L)
		rc_LIST2_A <- append(rc_LIST2_A,temp_list2)
	}

	# Preparing and returning finall list for resultClass

	resultClass_list <- list(rc_LIST_V,rc_LIST_P,rc_LIST_NS,rc_LIST2_A)
	return(resultClass_list)
}

markov_decision_problem_analysis <- function(no_processes,solution_type,file_directory,file_name="",convergence=TRUE, continuous=FALSE)
{
	# Setting solution type
	SOLUTION_TYPE <<- solution_type

	# Preparing data structures
	prepare_global_data_structures()

	# Getting state dimensions
	SD <- GET_STATES_DIMENSIONS()

	IT_F<-1										# Final iteration, used as a marker for the data if the process converges before final iteration
	DIFF_MATRIX <- array(-1,dim=c(SD,GET_MAX_ITERATIONS_PROCESS()))   # Matrix storing differences between value functions in subsequent iterations
	
	# Setting up convergence flags and variables
	value_convergence <- FALSE

	# Defining class to store results of parallel computations
	resultClass <- function()
	{
		base_list <- prepare_list_for_resultClass()
		me<-list(
			rc_LIST_V = base_list[[1]],
			rc_LIST_P = base_list[[2]],
			rc_LIST_NS = base_list[[3]],
			rc_LIST2_A = base_list[[4]]
		)
		class(me) <- append(class(me),"resultClass")
		return(me)
	}

	# Listing all functions in global environment to be used by worker nodes in parallel computations
	all_functions <- ls(globalenv(), pattern = "^[A-Za-z0-9_]+$")

	cat(paste0("\nMDP analysis started.\n\n"))

	# Loop over possible iterations of the algorithm
	for (it in 1:(GET_MAX_ITERATIONS_PROCESS()))
	{
		cat(paste0("Iteration: ",it," "))

		IT <<- it

		# Setting up parallel processes count
		cl <- makeCluster(no_processes)
		registerDoParallel(cl)

		result_par <- foreach (icl=1:(SD[1]), .export = c(all_functions), .options.smp = list(preschedule = FALSE))	%dorng% #%do% 
		{
			# Setting up the result class
			result_class <- resultClass()
			# Recursive analysis of the system
			final_result_class <- recursive_iteration_states_solution_search(icl,result_class)
			return(final_result_class)
		}

		# Stop the cluster after parallel computations are done
		stopCluster(cl)

		for (irp in 1:length(result_par))
		{
			recursive_iteration_states_save_parallel_data(irp,result_par)
		}

		# Convergence makes sense only if there are at least two iterations to compare
		if (IT > 1 && convergence==TRUE)
		{
			# Get the number of phases
			n_ph <- GET_NO_PHASES()

			if((IT %% n_ph)==0)
			{
				state_value <- vector()
				# Convergence results list contains convergence flag (if values converged), number converged, number analyzed and lowest convergence value
				convergence_results <- list("TRUE",0,0,0)
				for (icl in 1:(SD[1]))
				{
					# Running recursive calculations on all the states
					state_value <- icl
					convergence_results <- recursive_iteration_states_convergence_check(state_value,SD,convergence_results)
				}

				# Information about convergence results
				cat(paste0("Converged: ",convergence_results[[2]],"/",convergence_results[[3]]), " lowest convergence: ",convergence_results[[4]],"\n")

				if (convergence_results[[1]]==TRUE){break}
			}
			else{cat("\n")}


		}else{cat("\n")}

		if(continuous == TRUE)
		{
			output <- get_output_from_iteration(2)
			save_output_into_csv(output,file_directory,file_name,continuous)
		}
	}

	cat(paste0("\nCalcualtions completed.\n"))

	if(continuous==FALSE)
	{
		no_ph <- GET_NO_PHASES()

		for(iph in 1:no_ph)
		{
			output <- get_output_from_iteration(iph+no_ph)
			save_output_into_csv(output,file_directory,continuous,phase=iph)
		}

		cat(paste0("\nData saved.\n"))
	}
}

recursive_iteration_states_save_parallel_data <- function(state_value,rC)
{
	# Code recursively generates all allowed combinations of states

	# Getting state dimensions for verification if the state value is completed
	SD <- GET_STATES_DIMENSIONS()

	# Checking if this is the end of recursive iteration of states - state value is completed
	if (length(state_value)==length(SD))
	{
		# Getting information about number of players and numer of strategies
		p <- GET_PLAYERS_NO()
		s <- GET_STRATEGY_NO()

		rc_state <- state_value
		rc_state[1] <- 1

		# Reevaluate the iteration based on the phases
		n_ph <- GET_NO_PHASES()
		r_it <- ((IT-1) %% n_ph)+1

		# Saving value value and payoff
		for (ip in 1:GET_PLAYERS_NO())
		{
			temp_array <- LIST_V[[ip]]
			temp_value2 <- do.call('[', c(list(rC[[state_value[1]]]$rc_LIST_V[[ip]]), c(rc_state)))
			temp_value1 <- do.call('[', c(list(temp_array), c(state_value,n_ph+r_it)))
			temp_array <- do.call('[<-', c(list(temp_array), c(state_value,r_it), temp_value1))
			temp_array <- do.call('[<-', c(list(temp_array), c(state_value,n_ph+r_it), temp_value2))
			LIST_V[[ip]] <<- temp_array


			temp_array <- LIST_P[[ip]]
			temp_value2 <- do.call('[', c(list(rC[[state_value[1]]]$rc_LIST_P[[ip]]), c(rc_state)))
			temp_value1 <- do.call('[', c(list(temp_array), c(state_value,n_ph+r_it)))
			temp_array <- do.call('[<-', c(list(temp_array), c(state_value,r_it), temp_value1))
			temp_array <- do.call('[<-', c(list(temp_array), c(state_value,n_ph+r_it), temp_value2))
			LIST_P[[ip]] <<- temp_array
 		}

		# Saving values for harvest and next states
		for (is in 1:length(SD))
		{	
			temp_array <- LIST_NS[[is]]
			temp_value2 <- do.call('[', c(list(rC[[state_value[1]]]$rc_LIST_NS[[is]]), c(rc_state)))
			temp_value1 <- do.call('[', c(list(temp_array), c(state_value,n_ph+r_it)))
			temp_array <- do.call('[<-', c(list(temp_array), c(state_value,r_it), temp_value1))
			temp_array <- do.call('[<-', c(list(temp_array), c(state_value,n_ph+r_it), temp_value2))
			LIST_NS[[is]] <<- temp_array
		}

		# Saving 2d lists
		for (is in 1:GET_STRATEGY_NO())
		{
			for (ip in 1:GET_PLAYERS_NO())
			{
				temp_array <- LIST2_A[[is]][[ip]]
				temp_value2 <- do.call('[', c(list(rC[[state_value[1]]]$rc_LIST2_A[[is]][[ip]]), c(rc_state)))
				temp_value1 <- do.call('[', c(list(temp_array), c(state_value,n_ph+r_it)))
				temp_array <- do.call('[<-', c(list(temp_array), c(state_value,r_it), temp_value1))
				temp_array <- do.call('[<-', c(list(temp_array), c(state_value,n_ph+r_it), temp_value2))
				LIST2_A[[is]][[ip]] <<- temp_array
			}
		}
	}
	else
	{
		# Iterate over all possible states for current depth of the iteration
		for (is in 1:SD[length(state_value)+1])
		{
			temp_state_value <- c(state_value,is)
			recursive_iteration_states_save_parallel_data(temp_state_value,rC)
		}
	}
}

recursive_iteration_states_solution_search <- function(state_value,rC)
{
	# Function recursively generates all allowed combinations of states
	# When the full state value is generated the program either runs code for optimal harvest or Nash equilibrium search

	# Getting state dimensions for verification whether the state value was fully generated
	SD <- GET_STATES_DIMENSIONS()

	# Preparing temporary data
	# Note: First element of state is switched to 1
	t_result_class <- rC
	rc_state <- state_value
	rc_state[1] <- 1

	# If the length of the state value is equal to the length of the states dimensions vector the whole state value was generated
	if(length(state_value)==length(SD))
	{
		# Getting information about number of players and numer of strategies
		p <- GET_PLAYERS_NO()
		s <- GET_STRATEGY_NO()

		# Creating strategy array in form [STRATEGY,PLAYER] filled with NaNs
		strategy_array <- array(NaN,dim=c(s,p))

		# Initial depth for strategy iteration is first strategy of the first player
		depth <- c(1,1)
		
		# Prepare empty data structure for saving the best solution
		temp_solution <- list()
		temp_solution[[1]] <- list()				# List of payoffs
		temp_solution[[1]][[1]] <- c(-Inf)
		temp_solution[[2]] <- list()				# List of strategies
		temp_solution[[2]][[1]] <- c(NaN)		
		temp_solution[[3]] <- list()				# Information about symmetricity of the solution
		temp_solution[[3]][[1]] <- FALSE			
		temp_solution[[4]] <- 0						# Placeholder for best response graph
		solution <- list()

		max_a <- GET_LOCAL_MAX_STRATEGIES_INDICES(state_value)

		# Analysis of the optimal harvesoptimumt
		if (SOLUTION_TYPE == "optimum")
		{
			# Running the recursive procedure to iterate over all stratetgies for given state
			solution <- recursive_iteration_strategies(strategy_array,depth,state_value,temp_solution,max_a)
		}
		
		# Analysis of the Nash equilibrium
		if (SOLUTION_TYPE == "nash")
		{
			# Prepare data structure to save best responses
			best_response_graph <- generate_best_response_graph(state_value)

			symmetric_depth <- c(1,p)
			solution <- recursive_iteration_symmetric_strategies(strategy_array,symmetric_depth,state_value,temp_solution,max_a,best_response_graph)

			if(solution[[1]][[1]][1]==-99)
			{
				solution <- recursive_iteration_strategies(strategy_array,depth,state_value,temp_solution,max_a,best_response_graph)
				if(GET_CALCULATE_MIXED()==TRUE){solution <- verify_mixed_nash_equilibria(solution,state_value)}
			}
		}

		# Using returned solution values to generate all the solution data
		for (ip in 1:p)
		{
			t_array <- t_result_class$rc_LIST_V[[ip]]
			t_array <- do.call('[<-', c(list(t_array), c(rc_state), solution[[1]][[1]][ip]))
			t_result_class$rc_LIST_V[[ip]] <- t_array

			myopic_payoff <- GET_MYOPIC_PAYOFF_INDICES(state_value,solution[[2]][[1]])

			t_array <- t_result_class$rc_LIST_P[[ip]]
			t_array <- do.call('[<-', c(list(t_array), c(rc_state), myopic_payoff[ip]))
			t_result_class$rc_LIST_P[[ip]] <- t_array
		}

		for (is in 1:length(SD))
		{
			# harvest_values <- GET_HARVEST_VALUES(state_value,solution[[2]][[1]])

			# temp_array <- t_result_class$rc_LIST_H[[is]]
			# temp_array <- do.call('[<-', c(list(temp_array), c(rc_state), harvest_values[is]))
			# t_result_class$rc_LIST_H[[is]] <- temp_array

			next_state_value <- GET_NEXT_STATE_VALUES(state_value,solution[[2]][[1]])

			temp_array <- t_result_class$rc_LIST_NS[[is]]
			temp_array <- do.call('[<-', c(list(temp_array), c(rc_state), next_state_value[is]))
			t_result_class$rc_LIST_NS[[is]] <- temp_array
		}

		for (is in 1:s)
		{
			for (ip in 1:p)
			{
				temp_array <- t_result_class$rc_LIST2_A[[is]][[ip]]
				temp_a <- GET_STRATEGY_VALUES_FROM_INDICES(solution[[2]][[1]])
				temp_array <- do.call('[<-', c(list(temp_array), c(rc_state), temp_a[is,ip]))
				t_result_class$rc_LIST2_A[[is]][[ip]] <- temp_array
			}
		}
	}
	else
	{
		# Iterate over all possible states for current depth of the iteration running the recursive function
		for (is in 1:SD[length(state_value)+1])
		{
			temp_state_value <- c(state_value,is)
			n_result_class <- recursive_iteration_states_solution_search(temp_state_value,t_result_class)
			t_result_class <- n_result_class
		}
	}

	return(t_result_class)
}

recursive_iteration_states_convergence_check <- function(state_value,SD,convergence_results)
{
	# Setting up in-function version of the convergence result list to be updated
	t_convergence_results <- convergence_results

	# Checking if this is the end of recursive iteration of states
	if (length(state_value)==length(SD))
	{
		# Get number of phases
		no_ph <- GET_NO_PHASES()

		for (iph in 1:no_ph)
		{
			# Check convergence for each player in the game separately
			for(ip in 1:GET_PLAYERS_NO())
			{
				v1 <- do.call('[', c(list(LIST_V[[ip]]), c(state_value,iph+no_ph)))
				v2 <- do.call('[', c(list(LIST_V[[ip]]), c(state_value,iph)))
				v_conv <- abs(v1-v2)

				epsilon <- GET_EPSILON_VALUE_CONVERGENCE()

				if (v_conv > epsilon){t_convergence_results[[1]] <- FALSE}
				else{t_convergence_results[[2]] <- t_convergence_results[[2]]+1}

				t_convergence_results[[3]] <- t_convergence_results[[3]] + 1

				if(v_conv > t_convergence_results[[4]]){t_convergence_results[[4]] <- v_conv}
			}
		}
	}
	else
	{
		# Iterate over all possible states for current depth of the iteration
		for (is in 1:SD[length(state_value)+1])
		{
			temp_state_value <- c(state_value,is)
			new_convergence_results <- recursive_iteration_states_convergence_check(temp_state_value,SD,t_convergence_results)
			t_convergence_results <- new_convergence_results
		}
	}

	# Returning convergence results
	return(t_convergence_results)
}

recursive_iteration_symmetric_strategies <- function(strategy_array,depth,state,temp_solution,max_local_a,br_graph=NULL)
{
	# If the strategy array is filled in it means that there are no NaN values and full strategy profile was generated
	if (any(is.nan(strategy_array))==FALSE)
	{
		# Calculating value and payoff for this strategy profile
		next_state <- GET_NEXT_STATE_VALUES(state,strategy_array)
		values <- rep(0,GET_PLAYERS_NO())

		if (IT > 1)
		{
			# Get number of phases and calculate the offset
			no_ph <- GET_NO_PHASES()
			offset <- ((IT-1) %% no_ph) + no_ph

			# If model allows interpolation use it, if it is discrete use grid value
			if(GET_CALCULATE_INTERPOLATION()==TRUE){values <- interpolated_value(next_state,offset)}
			else{values <- grid_value(next_state,offset)}
			values <- GET_DISCOUNTED_VALUES(values)
		}

		values <- values+GET_MYOPIC_PAYOFF_INDICES(state,strategy_array)

		new_solution <- list()

		# Verification if the generated strategy set is a Nash equilibrium
		new_solution <- verification_best_response(state,strategy_array,br_graph)

		# Update solution and best response graph
		updated_solution <- update_solutions(temp_solution,new_solution)
		updated_solution[[4]] <- new_solution[[4]]

		return(updated_solution)
	}
	else
	{
		# Get information about possible actions for current state
		max_a <- max_local_a

		# Depth vector contains information about the current depth of the iteration in form [STRATEGY,PLAYER]
		s <- depth[1]
		p <- depth[2]

		# Preparing data structure for storing best solution
		best_solution <- temp_solution
		n_br_graph <- br_graph

		# For given player and given strategy vector iterate over all possible actions
		for (ia in 1:max_a[s,p])
		{
			# Update strategy value
			n_strategy_array <- strategy_array
			for(ip in 1:p){n_strategy_array[s,ip]<-ia}

			n_depth <- depth
			n_depth[1] <- 1


			# Get the solution and update the graph
			new_solution <- recursive_iteration_symmetric_strategies(n_strategy_array,n_depth,state,temp_solution,max_a,n_br_graph)
			n_br_graph <- new_solution[[4]]

			# Update keeps the best (highest value, symmetric) solution at the beginning of the list and updates the graph
			best_solution <- update_solutions(best_solution,new_solution)
			best_solution[[4]] <- new_solution[[4]]
		}	
	}

	# Return the best solution found on this level of analysis
	return(best_solution)
}

recursive_iteration_strategies <- function(strategy_array,depth,state,temp_solution,max_local_a,br_graph=NULL)
{
	# If the strategy array is filled in it means that there are no NaN values and full strategy profile was generated
	if (any(is.nan(strategy_array))==FALSE)
	{
		# Calculating value and payoff for this strategy profile
		next_state <- GET_NEXT_STATE_VALUES(state,strategy_array)

		values <- rep(0,GET_PLAYERS_NO())
		if (IT > 1)
		{
			# Get number of phases and calculate the offset
			no_ph <- GET_NO_PHASES()
			offset <- ((IT-1) %% no_ph) + no_ph

			# If model allows interpolation use it, if it is discrete use grid value
			if(GET_CALCULATE_INTERPOLATION()==TRUE){values <- interpolated_value(next_state,offset)}
			else{values <- grid_value(next_state,2)}
			values <- GET_DISCOUNTED_VALUES(values)
		}

		values <- values+GET_MYOPIC_PAYOFF_INDICES(state,strategy_array)

		new_solution <- list()

		if (SOLUTION_TYPE == "nash")
		{	
			# Verification if the generated strategy set is a Nash equilibrium
			new_solution <- verification_best_response(state,strategy_array,br_graph)

			# Update solution and best response graph
			updated_solution <- update_solutions(temp_solution,new_solution)
			updated_solution[[4]] <- new_solution[[4]]

			return(updated_solution)
		}

		if(SOLUTION_TYPE == "optimum")
		{
			# Checking if current solution is better than the one stored in temp_solution
			best_solution <- TRUE
			for (iv in 1:(length(temp_solution[[1]])))
			{
				if(values[iv] > temp_solution[[1]][iv])
				{
					# Stored solution is better analysis can be terminated
					best_solution <- FALSE
					break
				}

				# Ensuring break
				if(best_solution == FALSE){break}
			}

			# If current solution is better than the temporary one update it and return it
			if(best_solution == FALSE)
			{
				new_solution[[1]] <- list(values)
				new_solution[[2]] <- list(strategy_array)
				new_solution[[3]] <- list(TRUE)
				new_solution[[4]] <- 0
			}
			else{new_solution <- temp_solution}
		}

		# Returning current solution
		return(new_solution)
	}
	else
	{
		# Get information about possible actions for current state
		max_a <- max_local_a

		# Depth vector contains information about the current depth of the iteration in form [STRATEGY,PLAYER]
		s <- depth[1]
		p <- depth[2]

		# Preparing data structure for storing best solution
		best_solution <- temp_solution
		n_br_graph <- br_graph

		# For given player and given strategy vector iterate over all possible actions
		for (ia in 1:max_a[s,p])
		{
			# Update strategy value
			n_strategy_array <- strategy_array
			n_strategy_array[s,p] <- ia

			# If this is the final strategy vector for given player restart the vector counter and increase the player counter
			# Store the solution received from the DFS as the best solution and pass it for the another instance of recurence
			if (s == dim(max_a)[1])
			{
				n_depth <- depth
				n_depth[1] <- 1
				n_depth[2] <- p+1

				# Get the solution and update the graph
				new_solution <- recursive_iteration_strategies(n_strategy_array,n_depth,state,temp_solution,max_a,n_br_graph)
				n_br_graph <- new_solution[[4]]

				if(SOLUTION_TYPE=="nash")
				{
					# Update keeps the best (highest value, symmetric) solution at the beginning of the list and updates the graph
					best_solution <- update_solutions(best_solution,new_solution)
					best_solution[[4]] <- new_solution[[4]]
				}

				if(SOLUTION_TYPE == "optimum")
				{
					if(is.nan(best_solution[[2]][[1]][1])==FALSE)
					{
						if(sum(new_solution[[1]][[1]]>sum(best_solution[[1]][[1]]))){best_solution <- new_solution}
					}
					else
					{
						# First solution is always saved to serve as baseline solution					
						best_solution <- new_solution
					}				
				}
			}
			else
			{
				# Move to the next strategy
				n_depth <- depth
				n_depth[1] <- s+1
				new_solution <- recursive_iteration_strategies(n_strategy_array,n_depth,state,temp_solution,max_a,n_br_graph)
				n_br_graph <- new_solution[[4]]

				if(SOLUTION_TYPE=="nash")
				{
					# Update keeps the best (highest value, symmetric) solution at the beginning of the list and update the graph
					best_solution <- update_solutions(best_solution,new_solution)
					best_solution[[4]] <- new_solution[[4]]
				}

				if(SOLUTION_TYPE == "optimum")
				{
					if(is.nan(best_solution[[2]][[1]][1])==FALSE)
					{
						if(sum(new_solution[[1]][[1]]>sum(best_solution[[1]][[1]]))){best_solution <- new_solution}
					}
					else
					{
						# First solution is always saved to serve as baseline solution					
						best_solution <- new_solution
					}					
				}
			}
		}	
	}

	# Return the best solution found on this level of analysis
	return(best_solution)
}

recursive_best_response_search <- function(state,strategy_array,depth,best_response_solution)
{

	# If the strategy array is filled in it means that there are no NaN values and full strategy profile was generated
	if (any(is.nan(strategy_array))==FALSE)
	{
		# Calculating value and payoff for this strategy profile
		next_state <- GET_NEXT_STATE_VALUES(state,strategy_array)
		t_values <- rep(0,GET_PLAYERS_NO())
		if (IT > 1)
		{
			# Get number of phases and calculate the offset
			no_ph <- GET_NO_PHASES()
			offset <- ((IT-1) %% no_ph) + no_ph

			if(GET_CALCULATE_INTERPOLATION()==TRUE){t_values <- interpolated_value(next_state,offset)}
			else{t_values <- grid_value(next_state,2)}
			t_values <- GET_DISCOUNTED_VALUES(t_values)
		}
		t_values <- t_values+GET_MYOPIC_PAYOFF_INDICES(state,strategy_array)

		# If temporary solution gives same payoff than the current solution add it to the graph another best response
		if(isTRUE(all.equal(t_values[depth[2]],best_response_solution[[1]]))==TRUE)
		{	
			# Update data in best response graph
			response <- list()
			response[[1]] <- strategy_array[,depth[2]]
			n_br_graph <- set_best_response_data(best_response_solution[[3]],depth[2],strategy_array,response,TRUE)

			best_response_solution[[2]][[length(best_response_solution[[2]])+1]] <- strategy_array[,depth[2]]
			best_response_solution[[3]] <- n_br_graph
		}
		else
		{
			# If temporary solution gives better payoff than the current solution overwrite the extisting response with the new one	
			if(t_values[depth[2]] > best_response_solution[[1]])
			{
				# Update data in best response graph
				response <- list()
				response[[1]] <- strategy_array[,depth[2]]

				n_br_graph <- set_best_response_data(best_response_solution[[3]],depth[2],strategy_array,response,FALSE)

				# Values are different and the new value is higher than the saved best response update best response structure (strategies,values,ne_check,append_check)
				best_response_solution[[1]] <- t_values[depth[2]]
				best_response_solution[[2]][[1]] <- strategy_array[,depth[2]]
				best_response_solution[[3]] <- n_br_graph
				best_response_solution[[4]] <- FALSE
	
				# Returning the best solution
				return(best_response_solution)	
			}
		}
	}
	else
	{
		# Get information about possible actions for current state
		max_a <- GET_LOCAL_MAX_STRATEGIES_INDICES(state)

		# Depth vector contains information about the current depth of the iteration in form [STRATEGY,PLAYER]
		s <- depth[1]
		p <- depth[2]

		# For given player and given strategy vector iterate over all possible actions
		for (ia in 1:max_a[s,p])
		{
			# Update strategy value
			n_strategy_array <- strategy_array
			n_strategy_array[s,p] <- ia

			n_depth <- depth
			n_depth[1] <- s+1

			n_br_solution <- recursive_best_response_search(state,n_strategy_array,n_depth,best_response_solution)
			best_response_solution <- n_br_solution
		}
	}

	return(best_response_solution)
}

verification_best_response <- function(state,strategy_array,br_graph)
{
	# Get maximum local strategies array
	max_local_a <- GET_LOCAL_MAX_STRATEGIES_INDICES(state)
	no_strategies <- dim(max_local_a)[1]
	no_players <- dim(max_local_a)[2]

	# Calculating value and payoff for baseline strategy array	
	b_next_state <- GET_NEXT_STATE_VALUES(state,strategy_array)
	b_values <- rep(0,no_players)

	if (IT > 1)
	{
		# Get number of phases and calculate the offset
		no_ph <- GET_NO_PHASES()
		offset <- ((IT-1) %% no_ph) + no_ph

		if(GET_CALCULATE_INTERPOLATION()==TRUE){b_values <- interpolated_value(b_next_state,offset)}
		else{b_values <- grid_value(b_next_state,2)}
		b_values <- GET_DISCOUNTED_VALUES(b_values)
	}

	b_values <- b_values+GET_MYOPIC_PAYOFF_INDICES(state,strategy_array)

	# Flag for verification of Nash equilibria
	ne_check <- TRUE

	# Creating copy of the best response graph to be updated
	n_br_graph <- br_graph

	# For all players check best responses for the given strategy vector
	for (ip in 1:no_players)
	{
		# Check if the best response was already calculated
		t_br <- retrive_best_response(br_graph,ip,strategy_array)

		if(is.nan(t_br[[1]][[1]][[1]])==FALSE)
		{
			# If solution was already calculated check if corresponds to the current solution
			same_check <- TRUE

			for (i_br in 1:length(t_br))
			{
				# Do the check separately to each element of the response list
				same_check <- TRUE
				for(i_br2 in 1:length(t_br[[i_br]]))
				{
					if(isTRUE(all.equal(strategy_array[i_br2,ip],t_br[[i_br]][i_br2]))!=TRUE)
					{
						same_check <- FALSE
						break
					}

					# Ensuring exit from the loop
					if(same_check==FALSE){break}
				}
				if(same_check==TRUE){break}
			}

			if(same_check==FALSE){ne_check <- FALSE}
		}
		else
		{
			# Calculate best response for given player to predefined strategies of the other players

			# Set up best response vector
			best_response_strategies <- vector()

			# Remove strategies from the strategy array for given player and add best
			t_strategy_array <- strategy_array

			for (is in 1:(dim(strategy_array)[1]))
			{
				best_response_strategies <- append(best_response_strategies,t_strategy_array[is,ip])
				t_strategy_array[is,ip] <- NaN
			}

			# Set up initial depth (first element of strategy vector and ip element of player verctor)
			depth <- c(1,ip)

			# Set up data structure for best response solution (strategy vector, value, information if original solution is the best response)
			# Note: best_response_solution works only in the area of this function, solution structure, that stores NE is separate entity
			best_response_solution <- list()
			best_response_solution[[1]] <- b_values[ip]
			best_response_solution[[2]]  <- list()
			best_response_solution[[2]][[1]]<- strategy_array[,ip]
			best_response_solution[[3]] <- n_br_graph
			best_response_solution[[4]] <- TRUE

			n_br_solution <- recursive_best_response_search(state,t_strategy_array,depth,best_response_solution)

			# Updating the graph
			n_br_graph <- n_br_solution[[3]]

			if(n_br_solution[[4]]==FALSE){ne_check <- FALSE}
		}
	}

	# Returning solution containing information about values (if it is Nash equlibrium) and updated version of the best reponse graph
	solution <- list() 

	if (ne_check == TRUE)
	{
		solution[[1]] <- list()
		solution[[1]][[1]] <- b_values
		solution[[2]] <- list()
		solution[[2]][[1]] <- strategy_array
		solution[[3]] <- list()
		solution[[3]][[1]] <- FALSE
		solution[[4]] <- n_br_graph	
	}
	else
	{
		solution[[1]] <- list()
		solution[[1]][[1]] <- rep(-99,GET_PLAYERS_NO())
		solution[[2]] <- list()
		solution[[2]][[1]] <- strategy_array
		solution[[3]] <- list()
		solution[[3]][[1]] <- FALSE
		solution[[4]] <- n_br_graph	
	}

	return(solution)
}

update_solutions <- function(baseline_solution,new_solution)
{
	# Function merges solutions assuring that each Nash equilibrium is presented once and highest value is preserved at the beginning of the solution structure

	baseline_empty <- TRUE
	new_empty <- TRUE

	# Checking if solutions are empty (containing the -99 code for not a Nash equilibrium)
	if (baseline_solution[[1]][[1]][1] > -99){baseline_empty <- FALSE}
	if (new_solution[[1]][[1]][1] > -99){new_empty <- FALSE}

	# If both solutions are empty return the new one as merged
	if (baseline_empty==TRUE & new_empty==TRUE){return(new_solution)}
	else
	{
		# If new one baseline is empty and new one contains data return new solution with information about symmetricity
		if (baseline_empty==TRUE & new_empty==FALSE)
		{
			# Check if the strategies are symmetric
			new_solution[[3]][1] <- check_symmetric_strategies(new_solution[[2]][[1]])

			return(new_solution)
		}
		else
		{
			# If new solution does not contain Nash equilibrium use baseline solution
			if(baseline_empty==FALSE & new_empty==TRUE){return(baseline_solution)}
			else
			{
				# If both solutions contain Nash equilibria merge the information about NE ensuring that symmetric solution with highest sum of payoffs is the first

				# Verify symmetricity for new solution
				new_solution[[3]][1] <- check_symmetric_strategies(new_solution[[2]][[1]])

				# Checking if the baseline solution is symmetric
				if(baseline_solution[[3]][1] == FALSE)
				{
					# Checking if the new solution is symmetric
					if(new_solution[[3]][1] == TRUE)
					{
						# If new solution is symmetric and baseline is not, set new solution at the beginning of the solution structure
						baseline_solution[[1]] <- append(new_solution[[1]],baseline_solution[[1]])
						baseline_solution[[2]] <- append(new_solution[[2]],baseline_solution[[2]])
						baseline_solution[[3]] <- append(new_solution[[3]],baseline_solution[[3]])

						# Return data structure
						return(baseline_solution)
					}
					else
					{
						# If both solutions are asymmetric compare sum of payoffs
						baseline_sum <- sum(baseline_solution[[1]][[1]])
						new_sum <- sum(new_solution[[1]][[1]])

						if(new_sum > baseline_sum)
						{
							# If new solution yields higher payoff, set new solution at the beginning of the solution structure
							baseline_solution[[1]] <- append(new_solution[[1]],baseline_solution[[1]])
							baseline_solution[[2]] <- append(new_solution[[2]],baseline_solution[[2]])
							baseline_solution[[3]] <- append(new_solution[[3]],baseline_solution[[3]])

							# Return data structure
							return(baseline_solution)
						}
						else
						{
							# If sum of payoffs is higher for or equal for baseline solution, add new solution at the end of the data structure
							baseline_solution[[1]] <- append(baseline_solution[[1]],new_solution[[1]])
							baseline_solution[[2]] <- append(baseline_solution[[2]],new_solution[[2]])
							baseline_solution[[3]] <- append(baseline_solution[[3]],new_solution[[3]])

							# Return data structure
							return(baseline_solution)
						}
					}
				}
				else
				{
					# Checking if the new solution is symmetric
					if(new_solution[[3]][1] == FALSE)
					{
						# If baseline solution is symmetric and new is not, set new solution at the end of the solution structure
						baseline_solution[[1]] <- append(baseline_solution[[1]],new_solution[[1]])
						baseline_solution[[2]] <- append(baseline_solution[[2]],new_solution[[2]])
						baseline_solution[[3]] <- append(baseline_solution[[3]],new_solution[[3]])

						# Return data structure
						return(baseline_solution)
					}
					else
					{


						# If both solutions are symmetric compare sum of payoffs
						baseline_sum <- sum(baseline_solution[[1]][[1]])
						new_sum <- sum(new_solution[[1]][[1]])

						if(new_sum > baseline_sum)
						{
							# If new solution yields higher payoff, set new solution at the beginning of the solution structure
							baseline_solution[[1]] <- append(new_solution[[1]],baseline_solution[[1]])
							baseline_solution[[2]] <- append(new_solution[[2]],baseline_solution[[2]])
							baseline_solution[[3]] <- append(new_solution[[3]],baseline_solution[[3]])

							# Return data structure
							return(baseline_solution)
						}
						else
						{
							# If sum of payoffs is higher for or equal for baseline solution, add new solution at the end of the data structure
							baseline_solution[[1]] <- append(baseline_solution[[1]],new_solution[[1]])
							baseline_solution[[2]] <- append(baseline_solution[[2]],new_solution[[2]])
							baseline_solution[[3]] <- append(baseline_solution[[3]],new_solution[[3]])

							# Return data structure
							return(baseline_solution)
						}
					}
				}
			}
		}
	}
}

verify_mixed_nash_equilibria <- function(solution,state)
{
	##TODO: DEAL WITH THE SITUATION WHEN THERE ARE NO SOLUTIONS AT ALL

	# Save data for baseline solution and save information if baseline solution was overwritten
	baseline_solution <- list()
	baseline_solution[[1]] <- solution[[1]][[1]]
	baseline_solution[[2]] <- solution[[2]][[1]]
	baseline_solution[[3]] <- solution[[3]][[1]]
	baseline_overwritten <- FALSE

	# Prepare list of strategy set that are not symmetric
	strategy_set_list <- list()
	for (it in 1:length(solution[[2]]))
	{
		if(solution[[3]][it]==FALSE){strategy_set_list <- append(strategy_set_list,solution[[2]][it])}
	}

	strategy_set_length <- length(strategy_set_list)

	# If there are no asymmetric strategies return the original solution list
	if(strategy_set_length==0){return(solution)}

	# Prepare vector with information whether solutions were analyzed
	strategy_test <- rep(FALSE,strategy_set_length)

	# Save number of players, used in controlling the depth of the recursive search
	p_limit <- GET_PLAYERS_NO()

	# Iterate over strategy set list
	for (it in 1:strategy_set_length)
	{
		# Potential loop have to start with the strategy set that was not tested
		if(strategy_test[it] == FALSE)
		{
			# Calculate number of strategies that can be part of a loop
			l_limit <- strategy_set_length - sum(strategy_test)

			# Calculate data strating from the current solution[it] as initial and current one, with player depth of 1
			loop_strategies <- list()
			looped_strategies <- loop_recursive_search(it,it,strategy_set_list,strategy_test,1,p_limit,l_limit,loop_strategies)

			# Update strategy_test vector
			strategy_test <- looped_strategies[[2]]			

			# If algorithm found a loop calculate mixed Nash equilibria
			mixed_solution <- calculate_mixed_nash_equilibria(looped_strategies,state)

			# Compare mixed solution with baseline

			# Check if at least one of the solutions is symmetric
			if(mixed_solution[[3]]==TRUE || baseline_solution[[3]]==TRUE)
			{
				# Check if both solutions are symmetric
				if(mixed_solution[[3]]==TRUE && baseline_solution[[3]]==TRUE)
				{
					# Calculate sum of values
					m_sum <- sum(mixed_solution[[1]])
					b_sum <- sum(baseline_solution[[1]])

					# Checking if the values are not almost equal
					if(all.equal(m_sum,b_sum)==FALSE)
					{
						# If both solutions are symmetric update solution only if mixed one improves group payoff
						if(m_sum > b_sum)
						{
							baseline_solution <- mixed_solution
							baseline_overwritten <- TRUE
						}
					}
				}
				else
				{
					# Update solution only if mixed one is symmetric one
					if(mixed_solution[[3]]==TRUE)
					{
						baseline_solution <- mixed_solution
						baseline_overwritten <- TRUE
					}
				}
			}
			else
			{
				# Calculate sum of values
				m_sum <- sum(mixed_solution[[1]])
				b_sum <- sum(baseline_solution[[1]])

				# Checking if the values are not almost equal
				if(all.equal(m_sum,b_sum)==FALSE)
				{
					# If both solutions are symmetric update solution only if mixed one improves group payoff
					if(m_sum > b_sum)
					{
						baseline_solution <- mixed_solution
						baseline_overwritten <- TRUE
					}
				}
			}
		}

		# Save the information that the strategy was tested
		strategy_test[it] <- TRUE
	}

	# If baseline solution was overwritten return the solution with new element, otherwise return original solution list
	if(baseline_overwritten == TRUE)
	{
		solution[[1]] <- append(list(baseline_solution[[1]]),solution[[1]])
		solution[[2]] <- append(list(baseline_solution[[2]]),solution[[2]])
		solution[[3]] <- append(list(baseline_solution[[3]]),solution[[3]])
		return(solution)
	}else{return(solution)}
}

calculate_mixed_nash_equilibria <- function(loop_strategies,state)
{
	# In current version it is only possible to calculate Nash equlibria for a loop in the form (a,b) (b,a)

	## TODO: REVIEW IT AFTER SWITCHING THE MODEL TO 2 STRATGIES (ADD VERIFICATION OF STRATEGY SET LENGHT)

	if(length(loop_strategies[[3]]) > 2){return(-99)}
	else
	{
		# Check number of columns fo the strategy array
		dimensions <- dim(loop_strategies[[3]][[1]])
		strategies <- dimensions[1]
		players <- dimensions[2]
		valid_strategy <- 0

		# Check which strategy vector contains valid strategies
		for(it in 1:strategies)
		{
			if(sum(loop_strategies[[3]][[1]][it,]) > players)
			{
				valid_strategy <- it
				break
			}	
		}
	}
	
	# Calculating solution in the form of (a,b), (b,a)
	a <- loop_strategies[[3]][[1]][valid_strategy,1]
	b <- loop_strategies[[3]][[1]][valid_strategy,2]

	a_t <- min(a,b)
	b_t <- max(a,b)

	a <- a_t
	b <- b_t

	# Preparing strategy sets
	dimensions <- dim(loop_strategies[[3]][[1]])

	s_aa <- array(1,dim=dimensions)
	s_ab <- s_aa
	s_ba <- s_aa
	s_bb <- s_aa

	s_aa[valid_strategy,1] <- a
	s_aa[valid_strategy,2] <- a

	s_ab[valid_strategy,1] <- a
	s_ab[valid_strategy,2] <- b

	s_ba[valid_strategy,1] <- b
	s_ba[valid_strategy,2] <- a

	s_bb[valid_strategy,1] <- b
	s_bb[valid_strategy,2] <- b

	# Calculating myopic values
	v_aa <- GET_MYOPIC_PAYOFF_INDICES(state,s_aa)
	v_ab <- GET_MYOPIC_PAYOFF_INDICES(state,s_ab)
	v_ba <- GET_MYOPIC_PAYOFF_INDICES(state,s_ba)
	v_bb <- GET_MYOPIC_PAYOFF_INDICES(state,s_bb)

	# If it is not the first iteration calculate value using next state data and interpolation procedure
	if(IT > 1)
	{
		# Get number of phases and calculate the offset
		no_ph <- GET_NO_PHASES()
		offset <- ((IT-1) %% no_ph) + no_ph

		ns_aa <- GET_NEXT_STATE_VALUES(state,s_aa)
		ns_ab <- GET_NEXT_STATE_VALUES(state,s_ab)
		ns_ba <- GET_NEXT_STATE_VALUES(state,s_ba)
		ns_bb <- GET_NEXT_STATE_VALUES(state,s_bb)

		v_aa_in <- interpolated_value(ns_aa,offset)
		v_aa_in <- GET_DISCOUNTED_VALUES(v_aa_in)
		v_aa <- v_aa + v_aa_in

		v_ab_in <- interpolated_value(ns_ab,offset)
		v_ab_in <- GET_DISCOUNTED_VALUES(v_ab_in)
		v_ab <- v_ab + v_ab_in

		v_ba_in <- interpolated_value(ns_ba,offset)
		v_ba_in <- GET_DISCOUNTED_VALUES(v_ba_in)
		v_ba <- v_ba + v_ba_in

		v_bb_in <- interpolated_value(ns_bb,offset)
		v_bb_in <- GET_DISCOUNTED_VALUES(v_bb_in)
		v_bb <- v_bb + v_bb_in
	}

	# Calculating probabilities for mixed Nash equilibrium
	p <- (v_bb[1]-v_ab[1])/(v_aa[1]-v_ab[1]-v_ba[1]+v_bb[1])
	q <- (v_bb[2]-v_ba[2])/(v_aa[2]-v_ba[2]-v_ab[2]+v_bb[2])

	# Calculate action indices
	a <- a-1
	b <- b-1

	a1 <- q*a+(1-q)*b
	a2 <- p*a+(1-p)*b

	a1 <- a1+1
	a2 <- a2+1

	# Generate strategy array with mixed strategies
	m <- array(1,dim=c(2,2))
	m[valid_strategy,1] <- a1
	m[valid_strategy,2] <- a2

	# Verify if mixed solution is symmetric
	symmetric <- FALSE
	if(all.equal(a1,a2)==TRUE){symmetric <- TRUE}

	# Recalculate values
	v <- GET_MYOPIC_PAYOFF_INDICES(state,m)

	# If it is not the first iteration calculate value using next state data and interpolation procedure
	if(IT > 1)
	{
		# Get number of phases and calculate the offset
		no_ph <- GET_NO_PHASES()
		offset <- ((IT-1) %% no_ph) + no_ph

		ns_m <- GET_NEXT_STATE_VALUES(state,m)
		v_m_in <- interpolated_value(ns_m,offset)
		v_m_in <- GET_DISCOUNTED_VALUES(v_m_in)
		v <- v + v_m_in
	}

	# Preparing solution structure
	mixed_solution <- list()
	mixed_solution[[1]] <- v
	mixed_solution[[2]] <- m
	mixed_solution[[3]] <- symmetric

	return(mixed_solution)
}

loop_recursive_search <- function(initial_strategy_id,current_strategy_id,strategy_set_list,strategy_test,depth_p,p_limit,l_limit,loop_strategies)
{
	# Save strategy in the variable
	current_strategy <- strategy_set_list[[current_strategy_id]]

	# Update l_limit
	l_limit <- length(strategy_set_list) - sum(strategy_test)

	# If it is the first instance of recursion (and current strategy is initial strategy) do not update the verification as the unverified strategy is need to close the loop
	if(initial_strategy_id != current_strategy_id){strategy_set_list[current_strategy_id] <- TRUE}

	# Iterate over all not tested strategies that are not current strategy
	for (it in 1:length(strategy_set_list))
	{
		if(strategy_test[it]==FALSE)
		{
			# Check if the strategy from the list can be next in the loop for the current strategy
			if(all(strategy_set_list[[it]][,1] == current_strategy[,2]))
			{
				# Verify if the next solution closes the loop (is the same as initial strategy)
				if(initial_strategy_id==it)
				{
					# Check if the loop is valid (procedure iterated over all players)
					if(depth_p == p_limit)
					{
						# Set next strategy as tested
						strategy_test[it] <- TRUE
						
						# Return updated information about solutions loop and updated information about tested stratgies				
						looped_solutions <- list()
						looped_solutions[[1]] <- TRUE
						looped_solutions[[2]] <- strategy_test
						loop_strategies <- append(loop_strategies,list(current_strategy))
						looped_solutions[[3]] <- loop_strategies

						return(looped_solutions)	
					} # Else ignore the solution at this point of the procedure
				}
				else
				{
					# Verify if it is possible to continue loop
					if(l_limit > 1)
					{
						# Add strategy to the loop, update the information whether the solution is part of the loop
						loop_strategies <- append(loop_strategies,list(current_strategy))
						strategy_test[it] <- TRUE

						# Update control variables
						n_depth_p <- depth_p+1
						if(depth_p > p_limit){depth_p <- 1}
						l_limit <- length(strategy_set_list) - sum(strategy_test)

						# Run recursive procedure with new set of data
						looped_solutions <- loop_recursive_search(initial_strategy_id,it,strategy_set_list,strategy_test,n_depth_p,p_limit,l_limit,loop_strategies)

						if(looped_solutions[[1]]==TRUE){return(looped_solutions)}
						else{strategy_test <- looped_solutions[[2]]}

					} # Else this solution cannot create valid closed loop
				}
			}
		}
	}

	# None of the solutions on the list can continue the loop, the loop does not exist
	# Return updated information non-looping solutions and updated information about tested stratgies
	looped_solutions <- list()
	looped_solutions[[1]] <- FALSE
	looped_solutions[[2]] <- strategy_test

	return(looped_solutions)
}

interpolated_value <- function(state_value,iteration)
{
	# NOTE: Bilinear interpolation works in 2 dimensions
	if (length(state_value)>2)
	{
		return(-99)
	}


	DF_G <- GET_DISCRETIZATION_FACTOR_STATES()
	SD <- GET_STATES_DIMENSIONS()

	interpolation_results <- vector()

	for(ip in 1:GET_PLAYERS_NO())
	{
		# Setting up grid and variables for interpolation procedure
		gx_l <- floor(state_value[1]/DF_G[1]+1)
		gx_l <- min(gx_l,SD[1]-1)
		gx_l <- as.integer(round(gx_l))
		gx_h <- gx_l+1
		gx_h <- as.integer(round(gx_h))
		gy_l <- floor(state_value[2]/DF_G[2]+1)
		gy_l <- min(gy_l,SD[2]-1)
		gy_l <- as.integer(round(gy_l))
		gy_h <- gy_l+1
		gy_h <- as.integer(round(gy_h))
		sx_l <- (gx_l-1)*DF_G[1]
		sy_l <- (gy_l-1)*DF_G[2]
		ex <- (state_value[1]-sx_l)/DF_G[1]
		ey <- (state_value[2]-sy_l)/DF_G[2]

		v1_states <- c(gx_l,gy_l)
		v1 <- do.call('[', c(list(LIST_V[[ip]]), c(v1_states,iteration)))

		v2_states <- c(gx_h,gy_l)
		v2 <- do.call('[', c(list(LIST_V[[ip]]), c(v2_states,iteration)))

		v3_states <- c(gx_l,gy_h)
		v3 <- do.call('[', c(list(LIST_V[[ip]]), c(v3_states,iteration)))

		v4_states <- c(gx_h,gy_h)
		v4 <- do.call('[', c(list(LIST_V[[ip]]), c(v4_states,iteration)))

		interpolation_results <- c(interpolation_results,(v1+ex*(v2-v1)+ey*(v3-v1)+ex*ey*(v4-v2-v3+v1)))
	}

	return(interpolation_results)
}

grid_value <- function(state_value,iteration)
{
	DF_G <- GET_DISCRETIZATION_FACTOR_STATES()
	SD <- GET_STATES_DIMENSIONS()
	state_id <- GET_STATE_INDICES_FROM_VALUE(state_value)

	grid_results <- vector()

	for(ip in 1:GET_PLAYERS_NO())
	{
		v <- do.call('[', c(list(LIST_V[[ip]]), c(state_id,iteration)))

		grid_results <- c(grid_results,v)
	}

	return(grid_results)
}

generate_best_response_graph <- function(state)
{
	# Code generates best response matrix for the given state
	# The data structure is better represented as tree of nexted lists
	
	# Generating empty response that will be substituted with response vector in the analysis
	initial_depth <- c(1,1)
	player_best_responses <- list()
	
	# Generate best response matrix for all players
	for (ip in 1:GET_PLAYERS_NO())
	{
		player_best_responses[[ip]] <- recursive_generation_best_response_graph(ip,state,initial_depth) 
	} 

	return(player_best_responses)
}

recursive_generation_best_response_graph <- function(player,state,depth)
{
	# Setting up control data
	s <- depth[1]
	p <- depth[2]

	# Prepare data structure to store the generated graph
	action_list <- list()

	# Removing active player from the responses (we only have actions of other players)
	local_max_a <- GET_LOCAL_MAX_STRATEGIES_INDICES(state)
	local_max_a <- local_max_a[,-player,drop=FALSE]

	# Checking if it is the final strategy on the vector
	if(s == GET_STRATEGY_NO())
	{
		# Checking if it is the final player on the vector
		if(p == GET_PLAYERS_NO()-1)
		{
			# Generate empty payoffs for the terminal node of the graph
			for(ia in 1:local_max_a[s,p])
			{
				action_list[[ia]] <- list()
				action_list[[ia]][[1]]  <- NaN
			}

			return(action_list)
		}
		else
		{
			# If it is not the last player on the vector switch to next player and restart the strategies
			n_depth <- depth
			n_depth[1] <- 1
			n_depth[2] <- n_depth[2]+1

			for(ia in 1:local_max_a[s,p])
			{
				action_list[[ia]] <- recursive_generation_best_response_graph(player,state,n_depth)
			}

			return(action_list)
		}
	}
	else
	{
		# If this is not the final strategy on the vector switch to next stategy
		n_depth <- depth
		n_depth[1] <- n_depth[1]+1

		for(ia in 1:local_max_a[s,p])
		{
			action_list[[ia]] <- recursive_generation_best_response_graph(player,state,n_depth)
		}

		return(action_list)	
	}
}

set_best_response_data <- function(best_response_graph,player,actions,response,add_flag)
{
	# Function updates data in best response graph for given player, new data is always added to the list
	# Note: Function does not verify the validity of actions array
	
	# Set up initial depth 
	initial_depth <- c(1,1)

	# Updating actions for the searh
	n_actions <- actions[,-player, drop=FALSE]

	best_response_graph[[player]] <- recursive_set_best_response(n_actions,initial_depth,response,best_response_graph[[player]],add_flag)
	return(best_response_graph)
}

recursive_set_best_response <- function(actions,depth,response,graph,add_flag)
{
	# Prepare control data
	s <- depth[1]
	p <- depth[2]

	if(s == GET_STRATEGY_NO())
	{
		if(p == GET_PLAYERS_NO()-1)
		{
			# For final element set the best response vector 
			action <- actions[depth[1],depth[2]]

			if (add_flag == TRUE)
			{
				# Checking if current response is NaN, if so overwrite it
				if(is.nan(graph[[action]][[1]][1])==TRUE)
				{
					graph[[action]] <- list()
					for (il in 1:length(response))
					{
						graph[[action]][[il]] <- response[[il]]
					}
				}
				else
				{
					# Adding the vector to the existing best response (more than one best response)
					for (il in 1:length(response))
					{
						graph[[action]][[length(graph[[action]])+1]] <- response[[il]]
					}
				}
			}
			else
			{
				# Overwriting extisting best responses with better one
				graph[[action]] <- list()
				for (il in 1:length(response))
				{
					graph[[action]][[il]] <- response[[il]]
				}
			}
				
			return(graph)
		}
		else
		{
			# If this is the last element of the stategy vector switch to the next player
			n_depth <- depth
			n_depth[1] <- 1
			n_depth[2] <- n_depth[2]+1

			# Getting the action value to separate subgraph
			action <- actions[depth[1],depth[2]]
			n_graph <- graph[[action]]
			n_graph <- recursive_set_best_response(actions,n_depth,response,n_graph,add_flag)
			graph[[action]] <- n_graph

			return(graph)
		}
	}
	else
	{
		# If this is not last element of the strategy vector switch to the next element of the strategy vector
		n_depth <- depth
		n_depth[1] <- depth[1]+1

		# Getting the action value to separate subgraph
		action <- actions[depth[1],depth[2]]
		n_graph <- graph[[action]]
		n_graph <- recursive_set_best_response(actions,n_depth,response,n_graph,add_flag)
		graph[[action]] <- n_graph

		return(graph)
	}
}

retrive_best_response <- function(best_response_graph,player,actions)
{
	# Function retrives best response of given player to set of actions of other players
	# Note: Function does not verify the validity of actions array

	# Set up initial depth
	initial_depth <- c(1,1)

	# Updating actions for the searh
	n_actions <- actions[,-player, drop=FALSE]

	return(recursive_retrive_best_response(n_actions,initial_depth,best_response_graph[[player]]))
}


recursive_retrive_best_response <- function(actions,depth,graph)
{
	# Prepare control data
	s <- depth[1]
	p <- depth[2]

	if(s == GET_STRATEGY_NO())
	{
		if(p == GET_PLAYERS_NO()-1)
		{
			# For final element return the best response vector
			action <- actions[depth[1],depth[2]]
			return(graph[[action]])
		}
		else
		{
			# If this is the last element of the stategy vector switch to the next player
			n_depth <- depth
			n_depth[1] <- 1
			n_depth[2] <- n_depth[2]+1

			# Getting the action value to separate subgraph
			action <- actions[depth[1],depth[2]]
			n_graph <- graph[[action]]
			return(recursive_retrive_best_response(actions,n_depth,n_graph))
		}
	}
	else
	{
		# If this is not last element of the strategy vector switch to the next element of the strategy vector
		n_depth <- depth
		n_depth[1] <- depth[1]+1

		# Getting the action value to separate subgraph
		action <- actions[depth[1],depth[2]]
		n_graph <- graph[[action]]
		
		return(recursive_retrive_best_response(actions,n_depth,n_graph))
	}

}

check_symmetric_strategies <- function(strategy_array)
{
	# Checking if all players make the same decision

	no_strategies <- dim(strategy_array)[1]

	for (is in 1:no_strategies)
	{
		test <- isTRUE(all.equal(max(strategy_array[is,]),min(strategy_array[is,])))

		if(test==FALSE){return(test)}
	}

	return(TRUE)
}

get_output_from_iteration <- function(iteration_value)
{
	# Separating data from value and payoff lists
	O_LIST_V <- LIST_V
	O_LIST_P <- LIST_P

	for (ip in 1:GET_PLAYERS_NO())
	{
		dimensions <- dim(LIST_V[[ip]])
		index <- c(rep(list(TRUE), length(dimensions)-1), list(iteration_value))
		O_LIST_V[[ip]] <- do.call("[",c(list(LIST_V[[ip]]),index)) 
		O_LIST_P[[ip]] <- do.call("[",c(list(LIST_P[[ip]]),index)) 
	}

	O_LIST_NS <- LIST_NS

	SD <- GET_STATES_DIMENSIONS()

	for (is in 1:length(SD))
	{
		dimensions <- dim(LIST_NS[[is]])
		index <- c(rep(list(TRUE), length(dimensions)-1), list(iteration_value))
		O_LIST_NS[[is]] <- do.call("[",c(list(LIST_NS[[is]]),index)) 
	}

	O_LIST2_A <- LIST2_A
	
	for (is in 1:GET_STRATEGY_NO())
	{
		for (ip in 1:GET_PLAYERS_NO())
		{
			dimensions <- dim(LIST2_A[[is]][[ip]])
			index <- c(rep(list(TRUE), length(dimensions)-1), list(iteration_value))
			O_LIST2_A[[is]][[ip]] <- do.call("[",c(list(O_LIST2_A[[is]][[ip]]),index)) 
		}
	}	

	output <- list(O_LIST_V,O_LIST_P,O_LIST_NS,O_LIST2_A)
	return(output)
}

save_output_into_csv <- function(output,dir_name,file_name,continuous = FALSE,phase = 1)
{
	file_directory <- paste0("Output/",dir_name)

	dir.create(file_directory)

	if(continuous == FALSE)
	{
		# Write payoff and value data of all players (using same length)
		for (ip in 1:length(output[[1]]))
		{
			write.table(output[[1]][[ip]],file=paste0(file_directory,"/v",ip,"_ph_",phase,"_policy.csv"), col.names=FALSE, row.names=FALSE)
			write.table(output[[2]][[ip]],file=paste0(file_directory,"/p",ip,"_ph_",phase,"_policy.csv"), col.names=FALSE, row.names=FALSE)
		}

		# Write harvest and next state data of all players (using same length)
		for (is in 1:length(output[[3]]))
		{
			write.table(output[[3]][[is]],file=paste0(file_directory,"/ns",is,"_ph_",phase,"_policy.csv"), col.names=FALSE, row.names=FALSE)
		}

		for (is in 1:length(output[[4]]))
		{
			for (ip in 1:length(output[[4]][[is]]))
			{
				write.table(output[[4]][[is]][[ip]],file=paste0(file_directory,"/a",ip,"_ph_",phase,"_x",is,"_policy.csv"), col.names=FALSE, row.names=FALSE)
			}
		}
	}
	else
	{
		# Write payoff and value data of all players (using same length)
		for (ip in 1:length(output[[1]]))
		{
			write.table(output[[1]][[ip]],file=paste0(file_directory,"/v",ip,"_policy_T",IT,"_",file_name,".csv"), col.names=FALSE, row.names=FALSE)
			write.table(output[[2]][[ip]],file=paste0(file_directory,"/p",ip,"_policy_T",IT,"_",file_name,".csv"), col.names=FALSE, row.names=FALSE)
		}

		# Write harvest and next state data of all players (using same length)
		for (is in 1:length(output[[3]]))
		{
			write.table(output[[3]][[is]],file=paste0(file_directory,"/ns",is,"_policy_T",IT,"_",file_name,".csv"), col.names=FALSE, row.names=FALSE)
		}

		for (is in 1:length(output[[4]]))
		{
			for (ip in 1:length(output[[4]][[is]]))
			{
				write.table(output[[4]][[is]][[ip]],file=paste0(file_directory,"/a",ip,"_x",is,"_policy_T",IT,"_",file_name,".csv"), col.names=FALSE, row.names=FALSE)
			}
		}
	}
}

## MANAGEMENT FUNCTIONS ##

clear_model <- function()
{
	# Function removes from workspace all elements connected with the model
	
	# Removing parameters lists
	rm(STATES_PARAMETERS,PROCESS_PARAMETERS,ACTION_PARAMETERS)

	# Removing interfaces
	rm(GET_IS_SYMMETRIC,GET_MAX_ITERATIONS_PROCESS,GET_EPSILON_VALUE_CONVERGENCE,GET_DISCRETIZATION_FACTOR_STATES,GET_DISCOUNTED_VALUES,GET_MYOPIC_PAYOFF_INDICES)
	rm(GET_HARVEST_VALUES,GET_NEXT_STATE_VALUES,GET_STATES_DIMENSIONS,GET_STATE_VALUE_FROM_INDICES,GET_STRATEGY_VALUES_FROM_INDICES,GET_PLAYERS_NO,GET_STRATEGY_NO)
	rm(GET_LOCAL_MAX_STRATEGIES_INDICES,GET_LOCAL_MAX_SYMMETRIC_STRATEGIES_INDICES,GET_LOCAL_MIN_STRATEGIES)
}

load_packages <- function()
{
	# Function installs and loads all packages necessary to run analysis scripts

	# Packages for parallel computing
	install.packages("doParallel")
	library(doParallel)
	install.packages("doRNG")
	library(doRNG)
}


## TEST FUNCTIONS ##

generate_2D_payoff_matrix <- function(state,strategy_test,myopic,name="")
{
	# Functions assumes that there are 2 symmetric players each with one strategy vestor defined as strategy_test
	max_local_a <- GET_LOCAL_MAX_STRATEGIES_INDICES(state)

	p1_payoff_matrix <- array(NaN,dim=c(max_local_a[strategy_test,1],max_local_a[strategy_test,1]))
	p2_payoff_matrix <- array(NaN,dim=c(max_local_a[strategy_test,1],max_local_a[strategy_test,1]))

	strategy_array <- max_local_a

	for (is_p1 in 1:max_local_a[strategy_test,1])
	{
		for (is_p2 in 1:max_local_a[strategy_test,2])
		{
			strategy_array[strategy_test,1] <- is_p1
			strategy_array[strategy_test,2] <- is_p2

			p1 <- GET_MYOPIC_PAYOFF_INDICES(state,strategy_array)[1]
			p2 <- GET_MYOPIC_PAYOFF_INDICES(state,strategy_array)[2]

			if(myopic==FALSE)
			{
				next_state <- GET_NEXT_STATE_VALUES(state,strategy_array)
				values <- rep(0,GET_PLAYERS_NO())
				if (IT > 1)
				{
					# Get number of phases and calculate the offset
					no_ph <- GET_NO_PHASES()
					offset <- ((IT-1) %% no_ph) + no_ph

					if(GET_CALCULATE_INTERPOLATION()==TRUE){values <- interpolated_value(next_state,offset)}
					else{values <- grid_value(next_state,2)}
					values <- GET_DISCOUNTED_VALUES(values)
				}
				p1 <- p1+values[1]
				p2 <- p2+values[2]
			}

			p1_payoff_matrix[is_p1,is_p2] <- p1
			p2_payoff_matrix[is_p1,is_p2] <- p2
		}
	}

	filename1 <- paste0("P1",name,".csv")
	filename2 <- paste0("P2",name,".csv")

	write.csv(p1_payoff_matrix,filename1,col.names=FALSE,row.names=FALSE)
	write.csv(p2_payoff_matrix,filename2,col.names=FALSE,row.names=FALSE)
}

prepare_empty_solution <- function()
{
		# Prepare empty data structure for saving the best solution
		temp_solution <- list()
		temp_solution[[1]] <- list()				# List of payoffs
		temp_solution[[1]][[1]] <- c(-Inf)
		temp_solution[[2]] <- list()				# List of strategies
		temp_solution[[2]][[1]] <- c(NaN)		
		temp_solution[[3]] <- list()				# Information about symmetricity of the solution
		temp_solution[[3]][[1]] <- FALSE			
		temp_solution[[4]] <- 0						# Placeholder for best response graph

		return(temp_solution)
}