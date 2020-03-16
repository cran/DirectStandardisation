\name{adjprop}
\alias{adjprop}

\title{
Calculate adjusted proportions using direct standardisation
}
\description{
Calculates adjusted proportions of a variable by groups defined by another variable using direct standardisation to the structure of the dataset, as defined by one or more variables.
}
\usage{
adjprop(dataset, outcome_vars, categorical_vars, outcome_var_labels = NULL, 
categorical_var_labels = NULL, adjustment_vars = c("age", "sex"), 
adjustment_var_labels = NULL, format_table = FALSE,
transpose_table = FALSE, percentage = FALSE, ndigits = 2, title = "")
}

\arguments{
  \item{dataset}{
A data frame containing all variables to be used.
}
  \item{outcome_vars}{
The names of the outcome variables in \code{dataset} (character vector). These are the variables of which adjusted proportions will be calculated.
}
  \item{categorical_vars}{
A character vector containing the names of categorical variables which define the groups by which adjusted proportions will be calculated. They must exist in \code{dataset}.
}
  \item{outcome_var_labels}{
Labels for the outcome variable to be printed in the table produced. This must be a list of length equal to the number of outcome variables, with each element a list of length two, the first element of which is a character with a label for the variable and the second element a character vector with labels for the levels of the variable. For example for two variables, the first of which has 3 levels and the second 2, 
\code{list(list("Variable 1", c("Group 1", "Group 2", "Group 3")),}
\code{list("Variable 2", c("Group 1", "Group 2")))}. 
Note that if there is only one variable it should be 
\code{list(list("Variable 1",} 
\code{c("Group 1", "Group 2", "Group 3")))}.
If null, the names and levels of the variables are used.
}
  \item{categorical_var_labels}{
Labels for the categorical variables by which proportions will be calculated, to be printed in the table produced. This must be a list of length equal to the number of variables by which adjusted proportions will be calculated, with each element a list of length two, the first element of which is a character with a label for the variable and the second element a character vector with labels for the levels of the variable. For example for two variables, the first of which has 3 levels and the second 2, 
\code{list(list("Variable 1", c("Group 1", "Group 2", "Group 3")),}
\code{list("Variable 2", c("Group 1", "Group 2")))}. 
Note that if there is only one variable it should be 
\code{list(list("Variable 1",} 
\code{c("Group 1", "Group 2", "Group 3")))}.
If null, the levels of the variable are used.
}
  \item{adjustment_vars}{
A character vector containing the names of categorical variables to be adjusted for. The default is \code{age} and \code{sex}, which standardises proportions of the subgroups to the age and sex structure of the overall dataset.
}
  \item{adjustment_var_labels}{
A character vector with labels for the variables adjusted for, to be printed in the table produced. If null, the variable names are used.
}
  \item{format_table}{
Whether the output table should be formatted. Defaults to \code{FALSE}.
}
  \item{transpose_table}{
Whether the output table should be transposed. Defaults to \code{FALSE}.
}
  \item{ndigits}{
Number of digits to be printed (defaults to 2).
}
  \item{percentage}{
If \code{TRUE}, percentages instead of proportions are calculated. The default is \code{FALSE}.
}
  \item{title}{
A title for the table (defaults to blank).
}
}

\details{
The function produces a table of proportions of some outcome variable by one or more categorical variables using direct standardisation with target population a population with proportions within each group specified by some variables (default is age and sex) identical for all categories of the categorical variable and equal to the overall proportion in the data.
}
\value{
A data frame of adjusted proportions with categorical variables defining the groupings as rows and outcome categories as columns.
}
\author{
Christiana Kartsonaki <christiana.kartsonaki@gmail.com>
}
\seealso{
\code{\link{adjmeans}}
}
\examples{
# Example 1

# generate a dataframe with sleep deprivation (binary), sex and age group
data <- data.frame("sleep_deprivation" = rbinom(50, size = 1, prob = 0.5), 
"sex" = c(rep("m", 25), rep("f", 25)), 
"age_group" = rep(c("20-29", "30-39", "40-49", "50-59", "60-69"), 5))

adjprop(dataset = data, outcome_vars = "sleep_deprivation", 
categorical_vars = "sex", 
outcome_var_labels = list(list("Sleep deprivation", levels(as.factor(data$sleep_deprivation)))), 
categorical_var_labels = list(list("Sex", c("Female", "Male"))), 
adjustment_vars = "age_group", adjustment_var_labels = "age", 
title = "Proportions of sleep deprivation by sex.")

# Example 2

# generate a dataframe with sleep duration group (3 categories), sex and age group
data <- data.frame("sleep_group" = rbinom(500, size = 2, prob = 0.3), 
"sex" = c(rep("m", 25), rep("f", 25)), 
"age_group" = rep(c("20-29", "30-39", "40-49", "50-59", "60-69"), 5))

adjprop(dataset = data, outcome_vars = "sleep_group", 
categorical_vars = "sex", outcome_var_labels = list(list("Sleep duration group", 
c("Group 1", "Group 2", "Group 3"))),
categorical_var_labels = list(list("Sex", c("Female", "Male"))), 
adjustment_vars = "age_group", adjustment_var_labels = "age", 
title = "Proportions of sleep duration groups by sex.")

# Example 3

# generate a dataframe with sleep duration group (3 categories), sex and age group
data <- data.frame("sleep_group" = rbinom(500, size = 2, prob = 0.3), 
"sex" = c(rep("m", 25), rep("f", 25)), 
"age_group" = rep(c("20-29", "30-39", "40-49", "50-59", "60-69"), 5))

# no labels, more digits
adjprop(dataset = data, outcome_vars = "sleep_group", 
categorical_vars = "sex", adjustment_vars = "age_group", 
adjustment_var_labels = "age", ndigits = 4, 
title = "Proportions of sleep duration groups by sex.")

# Example 4

# generate a dataframe with sleep duration group (4 categories), sex and age group
data <- data.frame("sleep_group" = rbinom(500, size = 3, prob = 0.25), 
"sex" = c(rep("m", 25), rep("f", 25)), 
"age_group" = rep(c("20-29", "30-39", "40-49", "50-59", "60-69"), 5))

# no labels, proportions
adjprop(dataset = data, outcome_vars = "sleep_group", 
categorical_vars = "sex", adjustment_vars = "age_group", 
adjustment_var_labels = "age", title = "Proportions of sleep duration groups by sex.")

# no labels, percentages
adjprop(dataset = data, outcome_vars = "sleep_group", 
categorical_vars = "sex", adjustment_vars = "age_group", 
adjustment_var_labels = "age", percentage = TRUE, 
title = "Proportions of sleep duration groups by sex.")
}
