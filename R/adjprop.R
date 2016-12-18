adjprop <-
function(dataset, outcome_var_name, categorical_vars, outcome_label = outcome_var_name, outcome_categories_labels = NULL, categorical_var_labels = NULL, adjustment_vars = c("age", "sex"), adjustment_var_labels = NULL, ndigits = 2, percentage = FALSE, title = "") {

	# outcome_var_name should be a character representing a factor variable
	# categorical_variables: vector of characters representing factors for which numbers and percentages will be calculated
	# outcome_label: character for the label to be printed
	# outcome_categories_labels: character vector for the categories of the outcome variable; if null, the levels of the variable are used
	# categorical_var_labels: list of character variables (labels for the categorical variables)
	# adjustment vars: character vector of variables for which to adjust

	vars <- list()
	for (i in 1:length(categorical_vars)) {
		vars[[i]] <- dataset[,paste(categorical_vars[[i]])]
		}

	if (is.null(categorical_var_labels)) {
		categorical_var_labels <- list()
		for (i in 1:length(categorical_vars)) {
			categorical_var_labels[[i]] <- list(categorical_vars[i], levels(as.factor(vars[[i]])))
			}		
		}

	if (is.null(adjustment_var_labels)) {
		adjustment_var_labels <- adjustment_vars
		}
		
	tables <- list()
	tables_df <- list()
	adjustment_vars_temp <- adjustment_vars
	for (i in 1:length(categorical_vars)) {
		tables[[i]] <- table(vars[[i]], dataset[,outcome_var_name])
		if (categorical_vars[i] %in% adjustment_vars) {
			adjustment_vars_temp <- adjustment_vars[-which(adjustment_vars == categorical_vars[i])]
			}		
		adjustment_variables <- list()
		for (a in 1:length(adjustment_vars_temp)) {
			adjustment_variables[[a]] <- dataset[,paste(adjustment_vars_temp[a])]
			}

		# adjusted prevalence by direct standardisation
		weights_df <- list()
		standardised_proportion <- matrix(NA, nrow = length(categorical_var_labels[[i]][[2]]), ncol = length(levels(as.factor(dataset[,outcome_var_name]))))
		var_standardised_proportion <- matrix(NA, nrow = length(categorical_var_labels[[i]][[2]]), ncol = length(levels(as.factor(dataset[,outcome_var_name]))))

		# c: category (level) of outcome variable
		for (c in 1:length(levels(as.factor(dataset[,outcome_var_name])))) {
			weights_df[[c]] <- list()

			# for each level of the categorical variable i
			for (j in 1:length(categorical_var_labels[[i]][[2]])) {
				weights <- table(adjustment_variables)
				table_temp <- tapply(dataset[which(vars[[i]] == levels(as.factor(vars[[i]]))[j]), outcome_var_name], 
lapply(adjustment_variables, function(x) x[which(vars[[i]] == levels(as.factor(vars[[i]]))[j])]), function(x) length(which(x[which(!is.na(x))] == levels(as.factor(dataset[,outcome_var_name]))[c]))/length(x[which(!is.na(x))]))					
				weights_temp <- data.frame(expand.grid(dimnames(weights)), expand.grid(weights))
				names(weights_temp) <- c(adjustment_vars_temp, "weight")
				perc_table_temp <- data.frame(expand.grid(dimnames(table_temp)), expand.grid(table_temp))
				names(perc_table_temp) <- c(adjustment_vars_temp, "proportion")
				weights_df[[c]][[j]] <- merge(weights_temp, perc_table_temp)
						
				weights_df[[c]][[j]]$variance_prop <- weights_df[[c]][[j]]$proportion * (1 - weights_df[[c]][[j]]$proportion)
				standardised_proportion[j, c] <- sum(weights_df[[c]][[j]]$proportion * weights_df[[c]][[j]]$weight, na.rm = TRUE) / sum(weights_df[[c]][[j]]$weight, na.rm = TRUE)
				var_standardised_proportion[j, c] <- sum(weights_df[[c]][[j]]$variance_prop * (weights_df[[c]][[j]]$weight)^2, na.rm = TRUE) / (sum(weights_df[[c]][[j]]$weight, na.rm = TRUE)^2)
			
				} # end j loop
			} # end c loop

 		tables_df[[i]] <- data.frame("Variable" = categorical_var_labels[[i]][[1]], "Levels" = categorical_var_labels[[i]][[2]], "N" = table(vars[[i]]), "Proportion" = standardised_proportion, "Variance" = var_standardised_proportion, "se" = sqrt(var_standardised_proportion))
		# keep only as many lines as there are levels of the categorical variable
		tables_df[[i]] <- tables_df[[i]][1:length(categorical_var_labels[[i]][[2]]),]
		} # end i loop

	table_dataframe <- do.call(rbind, tables_df)
	if (is.null(outcome_categories_labels)) {
		outcome_categories_labels <- levels(as.factor(dataset[,outcome_var_name]))
		}
	table_dataframe <- table_dataframe[,-3]
	names(table_dataframe)[3:(3 + length(levels(as.factor(dataset[,outcome_var_name]))))] <- c("N", paste(outcome_label, ": ", outcome_categories_labels, ", ", "proportion", sep = ""))
	names(table_dataframe)[(4 + length(levels(as.factor(dataset[,outcome_var_name])))):(3 + 2*length(levels(as.factor(dataset[,outcome_var_name]))))] <- c(paste("Variance of proportion", outcome_categories_labels, sep = " "))
	names(table_dataframe)[(4 + 2*length(levels(as.factor(dataset[,outcome_var_name])))):(3 + 3*length(levels(as.factor(dataset[,outcome_var_name]))))] <- c(paste("se of proportion", outcome_categories_labels, sep = " "))
	if (percentage == TRUE) {
		table_dataframe[,4:(dim(table_dataframe)[2])] <- sapply(table_dataframe[,4:(dim(table_dataframe)[2])], function(x) as.numeric(as.character(x)) * 100)
		names(table_dataframe) <- gsub("proportion", "%", names(table_dataframe))
		}
	table_dataframe[,1:3] <- sapply(table_dataframe[,1:3], as.character)
	table_dataframe[,4:(dim(table_dataframe)[2])] <- sapply(table_dataframe[,4:(dim(table_dataframe)[2])], function(x) as.numeric(as.character(x)))
	table_dataframe[,4:(dim(table_dataframe)[2])] <- format(table_dataframe[,4:(dim(table_dataframe)[2])], digits = ndigits, nsmall = ndigits)
	table_dataframe$Variable <- as.character(table_dataframe$Variable)
	table_dataframe[duplicated(table_dataframe$Variable),]$Variable <- " "

	table_formatted <- table_dataframe
	counter <- 2
	for (i in 2:dim(table_dataframe)[1]) {
		if (table_dataframe[i, 1] != " ") {
			table_formatted[counter,] <- c(table_dataframe[i, 1], rep("", times = dim(table_dataframe)[2] - 1))
			table_formatted[counter+1,] <- c("", table_dataframe[i, 2:dim(table_dataframe)[2]])	
			counter <- counter + 2
			}
		if (table_dataframe[i, 1] == " ") {
			table_formatted[counter,] <- table_dataframe[i,]	
			counter <- counter + 1
			}
		}
	
	table_formatted <- rbind(c(title, rep(NA, times = dim(table_formatted)[2] - 1)), c(rep(NA, 6)), table_formatted, c(paste("Adjusted by ", paste(adjustment_var_labels, collapse = ", "), " (where appropriate).", sep = ""), rep(NA, times = dim(table_formatted)[2])))
#	table_formatted <- rbind(c(title, rep(NA, times = dim(table_formatted)[2])), c(rep(NA, 3), table(dataset[,outcome_var_name])), table_formatted, c(paste("Adjusted by ", paste(adjustment_var_labels, collapse = ", "), " (where appropriate).", sep = ""), rep(NA, times = dim(table_formatted)[2])))
	row.names(table_formatted) <- NULL
	table_formatted[is.na(table_formatted)] <- ""
	return(table_formatted)
	
	}


