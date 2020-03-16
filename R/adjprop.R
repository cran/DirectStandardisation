adjprop <-
function(dataset, outcome_vars, categorical_vars, outcome_var_labels = NULL, categorical_var_labels = NULL, adjustment_vars = c("age", "sex"), adjustment_var_labels = NULL, format_table = FALSE, transpose_table = FALSE, percentage = FALSE, ndigits = 2, title = "") {

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
            outcome_var_labels[[oo]] <- list(oo, levels(as.factor(dataset[,oo])))
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
             var_standardised_proportion <- standardised_proportion <- matrix(NA, nrow = length(categorical_var_labels[[cc]][[2]]), ncol = length(outcome_var_labels[[oo]][[2]]), dimnames = list(levels(as.factor(dataset[,cc])), levels(as.factor(dataset[,oo]))))

	           # loop over levels of outcome variable
             for (lo in levels(as.factor(dataset[,oo]))) {
                weights_df[[lo]] <- list()
		            # loop over levels of categorical variable
             	  for (lc in levels(as.factor(dataset[,cc]))) {
		                 # calculate weights
		                 weights <- table(adjustment_variables)
		                 # proportions of outcome level by adjustment variable within that level of categorical variable
                     table_temp <- tapply(X = dataset[which(dataset[,cc] == lc), oo], 
					  INDEX = lapply(adjustment_variables, function(x) x[which(dataset[,cc] == lc)]), 
					  FUN = function(x) length(which(x[which(!is.na(x))] == lo)) / length(x[which(!is.na(x))]))
                     n_temp <- tapply(X = dataset[which(dataset[,cc] == lc), oo], 
					  INDEX = lapply(adjustment_variables, function(x) x[which(dataset[,cc] == lc)]), 
					  FUN = function(x) length(x[which(!is.na(x))]))
                     weights_temp <- data.frame(expand.grid(dimnames(weights)), expand.grid(weights))
                     names(weights_temp) <- c(adjustment_vars_temp, "weight")
                     prop_table_temp <- data.frame(expand.grid(dimnames(table_temp)), expand.grid(table_temp))
                     names(prop_table_temp) <- c(adjustment_vars_temp, "proportion")
                     n_table_temp <- data.frame(expand.grid(dimnames(n_temp)), expand.grid(n_temp))
                     names(n_table_temp) <- c(adjustment_vars_temp, "number")
                     weights_df[[lo]][[lc]] <- merge(weights_temp, prop_table_temp)
                     weights_df[[lo]][[lc]] <- merge(weights_df[[lo]][[lc]], n_table_temp)                     
		                 weights_df[[lo]][[lc]]$variance_prop <- weights_df[[lo]][[lc]]$proportion * (1 - weights_df[[lo]][[lc]]$proportion) / weights_df[[lo]][[lc]]$number
		
                     standardised_proportion[lc, lo] <- sum(weights_df[[lo]][[lc]]$proportion * weights_df[[lo]][[lc]]$weight, na.rm = TRUE) / sum(weights_df[[lo]][[lc]]$weight, na.rm = TRUE)
                     var_standardised_proportion[lc, lo] <- sum(weights_df[[lo]][[lc]]$variance_prop * (weights_df[[lo]][[lc]]$weight)^2, na.rm = TRUE)/(sum(weights_df[[lo]][[lc]]$weight, na.rm = TRUE)^2)
              	     } # end lc loop
          	  } # end lo loop

            tables_df[[oo]][[cc]] <- data.frame(variable = categorical_var_labels[[cc]][[1]], levels = categorical_var_labels[[cc]][[2]], N = table(dataset[,cc]), proportion = standardised_proportion, variance = var_standardised_proportion, se = sqrt(var_standardised_proportion))
            tables_df[[oo]][[cc]] <- tables_df[[oo]][[cc]][1:length(categorical_var_labels[[cc]][[2]]),]
      	    } # end cc loop
      	} # end oo loop

    table_dataframe <- do.call(cbind, lapply(tables_df, function(x) do.call(rbind, x)))
      
    # table formatting
    if (format_table == TRUE) {
	      formatted <- table_dataframe[,-grep("N.Var1", names(table_dataframe), fixed = TRUE)]
	      formatted <- formatted[,-grep(".variance", names(formatted), fixed = TRUE)]
	      formatted <- formatted[,-grep(".se", names(formatted), fixed = TRUE)]
	      # remove duplicated columns with variable, levels and number of individuals
	      # if statement to avoid error if only one variable
	      if (length(grep(".variable", names(formatted), fixed = TRUE)[which(grep(".variable", names(formatted), fixed = TRUE) > 4)]) != 0) {
  	      formatted <- formatted[,-grep(".variable", names(formatted), fixed = TRUE)[which(grep(".variable", names(formatted), fixed = TRUE) > 4)]]
  	      formatted <- formatted[,-grep(".levels", names(formatted), fixed = TRUE)[which(grep(".levels", names(formatted), fixed = TRUE) > 4)]]
  	      formatted <- formatted[,-grep(".N.Freq", names(formatted), fixed = TRUE)[which(grep(".N.Freq", names(formatted), fixed = TRUE) > 4)]]
  	      }
	      
	      if (percentage == TRUE) {
            formatted[,4:(dim(formatted)[2])] <- sapply(formatted[,4:(dim(formatted)[2])], function(x) as.numeric(as.character(x)) * 100)
            names(formatted) <- gsub("proportion", "%", names(formatted))
            }
	      
	      formatted[,1:3] <- sapply(formatted[,1:3], as.character)
	      formatted[,4:(dim(formatted)[2])] <- sapply(formatted[,4:(dim(formatted)[2])], function(x) as.numeric(as.character(x)))
	      formatted[,4:(dim(formatted)[2])] <- format(formatted[,4:(dim(formatted)[2])], digits = ndigits, nsmall = ndigits)

	      names(formatted)[grep(".variable", names(formatted))] <- "Variable"
	      
	      formatted$Variable <- as.character(formatted$Variable)
#	      try(formatted[duplicated(formatted$Variable), ]$Variable <- " ")
	      if (TRUE %in% duplicated(formatted$Variable)) {
	        formatted[duplicated(formatted$Variable), ]$Variable <- " "
	        }
	      
	      names(formatted)[grep(".levels", names(formatted))] <- "Levels"
	      names(formatted)[grep(".N.Freq", names(formatted))] <- "N"

	      for (oo in outcome_vars) {
  	      names(formatted)[grep(oo, names(formatted))] <- paste(outcome_var_labels[[oo]][[1]], outcome_var_labels[[oo]][[2]], sep = " - ")
	        }
	      
        # transpose table
        if (transpose_table == TRUE) {
          formatted <- data.frame(t(formatted), stringsAsFactors = FALSE)
          }

	    if (!("Variable" %in% names(formatted))) {
	      formatted$Variable <- sapply(row.names(formatted), function(x) strsplit(x, " - ")[[1]][1])
	      formatted$Levels <- sapply(row.names(formatted), function(x) strsplit(x, " - ")[[1]][2])
	      formatted <- formatted[,c("Variable", "Levels", names(formatted)[1:(dim(formatted)[2] - 2)])]
	      }

      formatted$Variable <- as.character(formatted$Variable)
      if (TRUE %in% duplicated(formatted$Variable)) {
        formatted[duplicated(formatted$Variable), ]$Variable <- " "
        }
      
      formatted_temp <- formatted
      counter <- 2
	    for (i in 2:dim(formatted)[1]) {
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
      formatted <- rbind(c(title, rep(NA, times = dim(formatted)[2] - 1)), c(rep(NA, 6)), formatted, c(paste("Adjusted for ", paste(adjustment_var_labels, collapse = ", "), " (where appropriate).", sep = ""), rep(NA, times = dim(formatted)[2])))
      row.names(formatted) <- NULL
      formatted[is.na(formatted)] <- ""

      return(formatted)
      } else {
          return(table_dataframe)
          }

    }


