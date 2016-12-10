adjprop <-
function(dataset, outcome_var_name, categorical_vars, outcome_label, categorical_var_labels, adjustment_vars = c("age", "sex"), adjustment_var_labels = c("age", "sex"), title = "") {

	# outcome_var_name should be a character representing a factor variable
	# categorical_variables should be a vector of characters representing factors for which numbers and percentages will be calculated
	# outcome_label: character for the label to be printed
	# categorical_var_labels: list of character variables (labels for the categorical variables)
	# adjustement vars: character vector of variables for which to adjust

	vars <- list()
	for (i in 1:length(categorical_vars)) {
		vars[[i]] <- dataset[,paste(categorical_vars[[i]])]
		}

	tables <- list()
	tables_df <- list()
	adjustment_vars_temp <- adjustment_vars

	for (i in 1:length(categorical_vars)) {
		tables[[i]] <- table(vars[[i]])

		# if variable is the categorical variable do not standardise by it
		if (categorical_vars[i] %in% adjustment_vars) {
			adjustment_vars_temp <- adjustment_vars[-which(adjustment_vars == categorical_vars[i])]
			}		

		adjustment_variables <- list()
		for (a in 1:length(adjustment_vars_temp)) {
			adjustment_variables[[a]] <- dataset[,paste(adjustment_vars_temp[a])]
			}

		weights_df <- list()
		weights <- table(adjustment_variables)
		standardised_mean <- rep(NA, times = length(categorical_var_labels[[i]][[2]]))
		var_standardised_mean <- rep(NA, times = length(categorical_var_labels[[i]][[2]]))
		# for each level of the categorical variable i
		for (j in 1:length(categorical_var_labels[[i]][[2]])) {
			table_temp <- tapply(dataset[which(vars[[i]] == levels(as.factor(vars[[i]]))[j]), outcome_var_name], 
lapply(adjustment_variables, function(x) x[which(vars[[i]] == levels(as.factor(vars[[i]]))[j])]), mean, na.rm = TRUE)					
			variance_temp <- tapply(dataset[which(vars[[i]] == levels(as.factor(vars[[i]]))[j]), outcome_var_name], lapply(adjustment_variables, function(x) x[which(vars[[i]] == levels(as.factor(vars[[i]]))[j])]), function(y) (mean(y, na.rm = TRUE) * (1 - (mean(y, na.rm = TRUE)))))					
			weights_temp <- data.frame(expand.grid(dimnames(weights)), expand.grid(weights))
			names(weights_temp) <- c(adjustment_vars_temp, "weight")
			means_table_temp <- data.frame(expand.grid(dimnames(table_temp)), expand.grid(table_temp), expand.grid(variance_temp))
			names(means_table_temp) <- c(adjustment_vars_temp, "mean", "variance")
			weights_df[[j]] <- merge(weights_temp, means_table_temp)
			standardised_mean[j] <- sum(weights_df[[j]]$mean * weights_df[[j]]$weight, na.rm = TRUE) / sum(weights_df[[j]]$weight, na.rm = TRUE)
			var_standardised_mean[j] <- sum(weights_df[[j]]$variance * (weights_df[[j]]$weight^2), na.rm = TRUE) / (sum(weights_df[[j]]$weight, na.rm = TRUE)^2)
			}
				
 		tables_df[[i]] <- data.frame("Variable" = categorical_var_labels[[i]][[1]], "Levels" = categorical_var_labels[[i]][[2]], "N" = as.numeric(tables[[i]]), "Proportion" = format(as.numeric(prop.table(tables[[i]])), digits = 2), "Mean" = standardised_mean, "Variance" = var_standardised_mean, "se" = sqrt(var_standardised_mean))
		# keep only as many lines as there are levels of the categorical variable
		tables_df[[i]] <- tables_df[[i]][1:length(categorical_var_labels[[i]][[2]]),]

		}

	table_dataframe <- do.call(rbind, tables_df)
	names(table_dataframe)[3:(dim(table_dataframe)[2])] <- c("N", "proportion", outcome_label, "variance", "se")
	table_dataframe[,1:3] <- sapply(table_dataframe[,1:3], as.character)
	table_dataframe[,4:(dim(table_dataframe)[2])] <- sapply(table_dataframe[,4:(dim(table_dataframe)[2])], function(x) as.numeric(as.character(x)))
	table_dataframe[,4:(dim(table_dataframe)[2])] <- format(table_dataframe[,4:(dim(table_dataframe)[2])], digits = 2, nsmall = 2)
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
	row.names(table_formatted) <- NULL
	table_formatted[is.na(table_formatted)] <- ""
	return(table_formatted)
	
	}
