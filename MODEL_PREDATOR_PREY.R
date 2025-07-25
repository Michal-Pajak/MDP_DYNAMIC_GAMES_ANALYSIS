## PARAMETERS

STATES_PARAMETERS <- list(
		K = c(1,0.95),		    # Maximum size of populations - KX,KY=(NY*KX+CY)
		RX = 0.05,     	# Intrinsic growth rate of prey population (r)
		MX = 0.02,      	# Minimum viable population / Allee threshold (m)
		QX = 0.0336,    	# Maximum predation rate per capita (q)
		AX = 0.9,       	# Population value at ,which the predator function is one half of the saturated level (a)
		SY = 0.05,      	# Intrinsic growth rate of predator population (s)
		NY = 0.9,       	# Parameter for prey dependent carrying capacity (n)
		CY = 0.05      	# Environmental carrying capacity for the predator (non-prey food source) (c)
)

PROCESS_PARAMETERS <- list(
		E = 1/10^5,		# Difference between value functions in subsequent iterations that terminates the process (epsilon)
		R = 0.9,	    # Discount factor (rho)
		T = 5000,		# Maximum number of iterations till the end of the process
    	DF_G = c(0.01,0.01),	# Discretization factor for populations grid (prey,predator)
    	DF_A = c(0.01),	# Discretization factor for harvesting effort (prey,predator)
		PH = 2		# Number of phases in one iteration
)

ACTION_PARAMETERS <- list(
		MZ = 1,	   		# Maximum group harvesting effort that provides the non-negative payoff
		SYMMETRIC = TRUE, # information whether the game is symmetric strategy- and payoff-wise
		MAX_A = array(c(STATES_PARAMETERS$K[2]*0.5,STATES_PARAMETERS$K[2]*0.5),dim=c(1,2))
)

GET_NO_PHASES <- function()
{
	return(PROCESS_PARAMETERS$PH)
}

GET_CALCULATE_INTERPOLATION <- function()
{
	return(1)
}

GET_CALCULATE_MIXED <- function()
{
	return(1)
}

GET_IS_SYMMETRIC <- function()
{
	return(ACTION_PARAMETERS$SYMMETRIC)
}

GET_MAX_ITERATIONS_PROCESS <- function()
{
	return(PROCESS_PARAMETERS$T)
}

GET_EPSILON_VALUE_CONVERGENCE <- function()
{
	return(PROCESS_PARAMETERS$E)
}

GET_DISCRETIZATION_FACTOR_STATES <- function()
{
	return(PROCESS_PARAMETERS$DF_G)
}

GET_DISCOUNTED_VALUES <- function(values)
{
	if((IT %% PROCESS_PARAMETERS$PH) == 0)
	{
		return(values * PROCESS_PARAMETERS$R)
	}
	else
	{
		return(values)
	}
}

GET_MYOPIC_PAYOFF_INDICES <- function(state,strategies)
{
	if((IT %% PROCESS_PARAMETERS$PH) == 1)
	{
		# Translating indices into state values
		state <- GET_STATE_VALUE_FROM_INDICES(state)

		strategies <- GET_STRATEGY_VALUES_FROM_INDICES(strategies)
		myopic_payoffs <- rep(0,GET_PLAYERS_NO())

		# Calculating group efforts for each resource
		ge <- vector()
		ge <- rowSums(strategies)

		# Calculate payoff for one round for each player
		for (ip in 1:dim(strategies)[2])
		{
			# Calculate payoff from the strategy 
			for (is in 1:dim(strategies)[1])
			{
				if (ge[is] > state[is])
				{
					if (strategies[is,ip] > 0){myopic_payoffs[ip] <- myopic_payoffs[ip] + (strategies[is,ip]*state[is])/(ge[is])*(ACTION_PARAMETERS$MZ*state[is]-ge[is])*state[is]}
				}else{myopic_payoffs[ip] <- myopic_payoffs[ip] + strategies[is,ip]*(ACTION_PARAMETERS$MZ*state[is]-ge[is])*state[is]}
			}
		}
	}
	else
	{
		myopic_payoffs <- rep(0,GET_PLAYERS_NO())
	}

	return(myopic_payoffs)
}

GET_HARVEST_VALUES <- function(state,strategies)
{
	# Translating indices into state values
	state <- GET_STATE_VALUE_FROM_INDICES(state)
	strategies <- GET_STRATEGY_VALUES_FROM_INDICES(strategies)

	# Calculating harvest
	harvest <- vector()
	harvest = pmin(state,rowSums(strategies))

	return(harvest)
}

GET_NEXT_STATE_VALUES <- function(state,strategies)
{
	if((IT %% PROCESS_PARAMETERS$PH) == 1)
	{
		# Getting harvest values
		harvest <- GET_HARVEST_VALUES(state,strategies)

		# Translating indices into state values
		state <- GET_STATE_VALUE_FROM_INDICES(state)

		# Calculating next state
		sx_h <- state[1]-harvest[1]
		sy_h <- state[2]
		sx_n <- sx_h+STATES_PARAMETERS$RX*sx_h*(1-sx_h/STATES_PARAMETERS$K[1])*(sx_h-STATES_PARAMETERS$MX)-(STATES_PARAMETERS$QX*sx_h*sy_h)/(sx_h+STATES_PARAMETERS$AX)
		sx_n <- max(0,sx_n)	
		sy_n <- sy_h+STATES_PARAMETERS$SY*sy_h*(1-sy_h/(STATES_PARAMETERS$NY*sx_h+STATES_PARAMETERS$CY))
		sy_n <- max(0,sy_n)
	}
	else
	{
		# Getting harvest values
		harvest <- GET_HARVEST_VALUES(state,strategies)

		# Translating indices into state values
		state <- GET_STATE_VALUE_FROM_INDICES(state)

		# Calculating next state
		sx_h <- state[1]+harvest[1]*10
		sy_h <- state[2]
		sx_n <- max(0,sx_h)	
		sy_n <- max(0,sy_h)
	}

	return(c(sx_n,sy_n))
}

GET_STATES_DIMENSIONS <- function()
{
	SD <- vector()
	for (isd in 1:(length(STATES_PARAMETERS$K)))
	{
		SD <- c(SD,STATES_PARAMETERS$K[isd]/PROCESS_PARAMETERS$DF_G[isd]+1)
	}

	return(SD)
}

GET_STATE_VALUE_FROM_INDICES <- function(state_indices)
{
	# Translate state indexes into state values
	state <- (state_indices-1)*PROCESS_PARAMETERS$DF_G

	return(state)
}

GET_STRATEGY_VALUES_FROM_INDICES <- function(strategy_indices)
{
	# Translate strategy indexes into strategy values
	strategies <- (strategy_indices-1)*PROCESS_PARAMETERS$DF_A

	return(strategies)
}

GET_PLAYERS_NO <- function()
{
	if(SOLUTION_TYPE=="nash"){return(dim(ACTION_PARAMETERS$MAX_A)[2])}
	if(SOLUTION_TYPE=="optimum"){return(1)}
}

GET_STRATEGY_NO <- function()
{
	return(dim(ACTION_PARAMETERS$MAX_A)[1])
}

GET_LOCAL_MAX_STRATEGIES_INDICES <- function(state)
{
	MAX_LOCAL_A <- 0

	if((IT %% PROCESS_PARAMETERS$PH) == 1)
	{
		if(SOLUTION_TYPE=="nash")
		{
			T_MAX_LOCAL_A <- ACTION_PARAMETERS$MAX_A
			MAX_LOCAL_A <- T_MAX_LOCAL_A

			for(ip in 1:dim(MAX_LOCAL_A)[2])
			{
				# Harvesting prey

				# Translate state iterator into state value
				state_value <- (state[1]-1)*PROCESS_PARAMETERS$DF_G[1]
				action_value <- T_MAX_LOCAL_A[1,ip]
				action_value <- min(state_value,action_value)
				MAX_LOCAL_A[1,ip] <- floor(action_value/PROCESS_PARAMETERS$DF_A+1)
			}
		}

		if(SOLUTION_TYPE=="optimum")
		{
			state_value <- state_value <- (state[1]-1)*PROCESS_PARAMETERS$DF_G[1]
			max_action_value <- ACTION_PARAMETERS$MAX_A[1,1]
			max_action <- min(state_value,max_action_value)
			max_action <- floor(max_action/PROCESS_PARAMETERS$DF_A[1]+1)

			MAX_LOCAL_A <- array(max_action,dim=c(1,1))
		}
	}
	else
	{
		if(SOLUTION_TYPE=="nash")
		{
			T_MAX_LOCAL_A <- ACTION_PARAMETERS$MAX_A
			MAX_LOCAL_A <- T_MAX_LOCAL_A
			for(ip in 1:dim(MAX_LOCAL_A)[2])
			{
				MAX_LOCAL_A[1,ip] <- 3
			}
		}

		if(SOLUTION_TYPE=="optimum")
		{
			MAX_LOCAL_A <- array(6,dim=c(1,1))
		}
	}


	return(MAX_LOCAL_A)
}

# GET_LOCAL_MAX_SYMMETRIC_STRATEGIES_INDICES <- function(state)
# {
# 	# Getting maximum possible action in given state
# 	MAX_LOCAL_A <- GET_LOCAL_MAX_STRATEGIES_INDICES(state)

# 	# Iterating over strategy vector elements
# 	for(is in 1:length(STATES_PARAMETERS$K))
# 	{
# 		# Restarting initial value
# 		local_max_symmetric_strategy <- Inf

# 		# Iterating over players to get the lowest value for strategy
# 		for (ip in 1:dim(ACTION_PARAMETERS$MAX_A)[2])
# 		{
# 			local_max_symmetric_strategy <- min(MAX_LOCAL_A[is,ip],local_max_symmetric_strategy)
# 		}

# 		# Overwrite with the lowest value 
# 		for (ip in 1:dim(ACTION_PARAMETERS$MAX_A)[2])
# 		{
# 			MAX_LOCAL_A[is,ip] <- local_max_symmetric_strategy
# 		}
# 	}

# 	return(MAX_LOCAL_A)
# }

GET_LOCAL_MIN_STRATEGIES <- function(state)
{
	MIN_LOCAL_A <- array(1,dim=c(length(STATES_PARAMETERS$K),GET_PLAYERS_NO()))

	return(MIN_LOCAL_A)
}

