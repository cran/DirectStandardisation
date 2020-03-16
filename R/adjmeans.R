adjmeans <- 
function(dataset, outcome_vars, categorical_vars, outcome_var_labels = NULL, categorical_var_labels = NULL, adjustment_vars = c("age", "sex"), adjustment_var_labels = NULL, format_table = FALSE, transpose_table = FALSE, ndigits = 2, title = "") {

    # assign labels if null
    if (is.null(categorical_var_labels)) {
        categorical_var_labels <- list()
        for (cc in categorical_vars) {
            categorical_var_labels[[cc]] <- list(cc, levels(as.factor(dataset[,cc])))
            }
        }
    if (is.null(outcome_var_labels)) {
        outcome_var_labels <- list()
        for (oo in outcome_vars) {
            outcome_var_labels[[oo]] <- list(oo, "")
            }
        }
    names(categorical_var_labels) <- categorical_vars
    names(outcome_var_labels) <- outcome_vars
    if (is.null(adjustment_var_labels)) {
        adjustment_var_labels <- adjustment_vars
    	  }

    # list of tables and of dataframes with tables
    tables_df <- list()

    adjustment_vars_temp <- adjustment_vars    
    # loop over outcome variables and categorical variables
    for (oo in outcome_vars) {
        tables_df[[oo]] <- list()
	      for (cc in categorical_vars) {
	           # do not standardise variable by itself
       	     if (cc %in% adjustment_vars) {
            	adjustment_vars_temp <- adjustment_vars[-which(adjustment_vars == cc)]
        	    }
       	     if (oo %in% adjustment_vars) {
            	adjustment_vars_temp <- adjustment_vars[-which(adjustment_vars == oo)]
        	    }
      	     adjustment_variables <- list()
      	     for (a in 1:length(adjustment_vars_temp)) {
      		         adjustment_variables[[a]] <- dataset[,adjustment_vars_temp[a]]
              	   }

             weights_df <- list()
             var_standardised_mean <- standardised_mean <- numeric()

		         # loop over levels of categorical variable
             for (lc in levels(as.factor(dataset[,cc]))) {
		                 # calculate weights
		                 weights <- table(adjustment_variables)
		                 # means of outcome level by adjustment variable within that level of categorical variable
                     table_temp <- tapply(X = dataset[which(dataset[,cc] == lc), oo], 
                              					  INDEX = lapply(adjustment_variables, function(x) x[which(dataset[,cc] == lc)]), 
                              					  FUN = mean, na.rm = TRUE)
                     variance_temp <- tapply(X = dataset[which(dataset[,cc] == lc), oo], 
                                          INDEX = lapply(adjustment_variables, function(x) x[which(dataset[,cc] == lc)]), 
                                          FUN = var, na.rm = TRUE)
                     n_temp <- tapply(X = dataset[which(dataset[,cc] == lc), oo], 
                              					  INDEX = lapply(adjustment_variables, function(x) x[which(dataset[,cc] == lc)]), 
                              					  FUN = function(x) length(x[which(!is.na(x))]))
                     weights_temp <- data.frame(expand.grid(dimnames(weights)), expand.grid(weights))
                     names(weights_temp) <- c(adjustment_vars_temp, "weight")
                     means_table_temp <- data.frame(expand.grid(dimnames(table_temp)), expand.grid(table_temp), expand.grid(variance_temp))
                     names(means_table_temp) <- c(adjustment_vars_temp, "mean", "variance")
                     n_table_temp <- data.frame(expand.grid(dimnames(n_temp)), expand.grid(n_temp))
                     names(n_table_temp) <- c(adjustment_vars_temp, "number")
                     weights_df[[lc]] <- merge(weights_temp, means_table_temp)
                     weights_df[[lc]] <- merge(weights_df[[lc]], n_table_temp)                     
		                 weights_df[[lc]]$variance_mean <- weights_df[[lc]]$variance / weights_df[[lc]]$number
		
                     standardised_mean[lc] <- sum(weights_df[[lc]]$mean * weights_df[[lc]]$weight, na.rm = TRUE) / sum(weights_df[[lc]]$weight, na.rm = TRUE)
                     var_standardised_mean[lc] <- sum(weights_df[[lc]]$variance_mean * (weights_df[[lc]]$weight)^2, na.rm = TRUE) / (sum(weights_df[[lc]]$weight, na.rm = TRUE)^2)
              	     } # end lc loop

            tables_df[[oo]][[cc]] <- data.frame(variable = categorical_var_labels[[cc]][[1]], levels = categorical_var_labels[[cc]][[2]], N = table(dataset[,cc]), mean = standardised_mean, variance = var_standardised_mean, se = sqrt(var_standardised_mean))
            tables_df[[oo]][[cc]] <- tables_df[[oo]][[cc]][1:length(categorical_var_labels[[cc]][[2]]),]
      	    } # end cc loop
      	} # end oo loop

    table_dataframe <- do.call(cbind, lapply(tables_df, function(x) do.call(rbind, x)))
    
    # table formatting
    if (format_table == TRUE) {
	      formatted <- table_dataframe[,-grep("N.Var1", names(table_dataframe))]
	      formatted <- formatted[,-grep(".variance", names(formatted), fixed = TRUE)]
#	      formatted <- formatted[,-grep(".se", names(formatted))]
	      # remove duplicated columns with variable, levels and number of individuals
	      if (length(grep(".variable", names(formatted), fixed = TRUE)[which(grep(".variable", names(formatted), fixed = TRUE) > 4)]) != 0) {
	        formatted <- formatted[,-grep(".variable", names(formatted), fixed = TRUE)[which(grep(".variable", names(formatted), fixed = TRUE) > 4)]]
	        formatted <- formatted[,-grep(".levels", names(formatted), fixed = TRUE)[which(grep(".levels", names(formatted), fixed = TRUE) > 4)]]
	        formatted <- formatted[,-grep(".N.Freq", names(formatted), fixed = TRUE)[which(grep(".N.Freq", names(formatted), fixed = TRUE) > 4)]]
	        }

	      formatted[,1:3] <- sapply(formatted[,1:3], as.character)
	      formatted[,4:(dim(formatted)[2])] <- sapply(formatted[,4:(dim(formatted)[2])], function(x) as.numeric(as.character(x)))
	      formatted[,4:(dim(formatted)[2])] <- format(formatted[,4:(dim(formatted)[2])], digits = ndigits, nsmall = ndigits)

	      names(formatted)[grep(".variable", names(formatted))] <- "Variable"
	      formatted$Variable <- as.character(formatted$Variable)
	      formatted[duplicated(formatted$Variable), ]$Variable <- " "
	      
	      names(formatted)[grep(".levels", names(formatted))] <- "Levels"
	      names(formatted)[grep(".N.Freq", names(formatted))] <- "N"

	      # mean (se)
	      formatted[grep(".mean", names(formatted), fixed = TRUE)] <- mapply(FUN = function(x, y) paste0(x, " (", y, ")"), formatted[grep(".mean", names(formatted), fixed = TRUE)], formatted[grep(".mean", names(formatted), fixed = TRUE) + 1])
	      formatted <- formatted[,-grep(".se", names(formatted), fixed = TRUE)]
	      
	      for (oo in outcome_vars) {
  	      names(formatted)[grep(oo, names(formatted), fixed = TRUE)] <- paste0(outcome_var_labels[[oo]][[1]], outcome_var_labels[[oo]][[2]])
	        }
	      
        # transpose table
        if (transpose_table == TRUE) {
          formatted <- data.frame(t(formatted), stringsAsFactors = FALSE)
          }

  	    if (!("Variable" %in% names(formatted))) {
  	      formatted$Variable <- row.names(formatted)
  	      formatted <- formatted[,c("Variable", names(formatted)[1:(dim(formatted)[2] - 1)])]
  	      }
    	      
        if (transpose_table == FALSE) {
          formatted_temp <- formatted
          counter <- 1
    	    for (i in 1:dim(formatted)[1]) {
      	    if (formatted[i, 1] != " ") {
      	      formatted_temp[counter, ] <- c(formatted[i, 1], rep("", times = dim(formatted)[2] - 1))
      	      formatted_temp[counter + 1, ] <- c("", formatted[i, 2:dim(formatted)[2]])
      	      counter <- counter + 2
    	        }
    	      if (formatted[i, 1] == " ") {
    	        formatted_temp[counter, ] <- formatted[i,]
    	        counter <- counter + 1
    	        }
    	      }
    	   
          formatted <- formatted_temp
          }
        
        formatted <- rbind(c(title, rep(NA, times = dim(formatted)[2] - 1)), c(rep(NA, 6)), formatted, c(paste("Adjusted for ", paste(adjustment_var_labels, collapse = ", "), " (where appropriate).", sep = ""), rep(NA, times = dim(formatted)[2])))
        row.names(formatted) <- NULL
        formatted[is.na(formatted)] <- ""
  
        return(formatted)
        } else {
            return(table_dataframe)
            }

    }
