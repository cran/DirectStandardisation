\name{adjprop}
\alias{adjprop}

\title{
Calculate adjusted proportions using direct standardisation
}
\description{
Calculates adjusted proportions of a variable by groups defined by another variable using direct standardisation to the structure of the dataset, as defined by one or more variables.
}
\usage{
adjprop(dataset, outcome_var_name, categorical_vars, outcome_label, 
categorical_var_labels, adjustment_vars = c("age", "sex"), 
adjustment_var_labels = c("age", "sex"), title = "")
}

\arguments{
  \item{dataset}{
A data frame containing all variables to be used.
}
  \item{outcome_var_name}{
The name of the outcome variable in \code{dataset} (character). This is the variable of which adjusted proportions will be calculated.
}
  \item{categorical_vars}{
A character vector containing the names of categorical variables which define the groups by which adjusted proportions will be calculated. They must exist in \code{dataset}.
}
  \item{outcome_label}{
A label for the outcome variable to be printed in the table produced.
}
  \item{categorical_var_labels}{
Labels for the categorical variables by which proportions will be calculated, to be printed in the table produced.  This must be a list of length equal to the number of variables for which adjusted proportions will be calculated, with each element a list of length two, the first element of which is a character with a label for the variable and the second element a character vector with labels for the levels of the variable. For example for two variables, the first of which has 3 levels and the second 2, 
\code{list(list("Variable 1", c("Group 1", "Group 2", "Group 3")),}
\code{list("Variable 2", c("Group 1", "Group 2")))}. 
Note that if there is only one variable it should be 
\code{list(list("Variable 1",} 
\code{c("Group 1", "Group 2", "Group 3")))}.
}
  \item{adjustment_vars}{
A character vector containing the names of categorical variables to be adjusted for. The default is \code{age} and \code{sex}, which standardises proportions of the subgroups to the age and sex structure of the overall dataset.
}
  \item{adjustment_var_labels}{
A character vector with labels for the variables adjusted for, to be printed in the table produced.
}
  \item{title}{
A title for the table (defaults to blank).
}
}

\details{
The function produces a table of proportions of some outcome variable by one or more categorical variables using direct standardisation with target population a population with proportions within each group specified by some variables (default is age and sex) identical for all categories of the categorical variable and equal to the overall proportion in the data.
}
\value{
A matrix of adjusted proportions with categorical variables defining the groupings as rows and outcome categories as columns.
}
\author{
Christiana Kartsonaki <christiana.kartsonaki@gmail.com>
}
\seealso{
\code{\link{adjmeans}}
}
\examples{
# generate a dataframe with sleep duration, sex and age group
data <- data.frame("sleep_deprivation" = rbinom(50, size = 1, prob = 0.5), 
"sex" = c(rep("m", 25), rep("f", 25)), 
"age_group" = rep(c("20-29", "30-39", "40-49", "50-59", "60-69"), 5))

adjprop(dataset = data, outcome_var_name = "sleep_deprivation", 
categorical_vars = "sex", outcome_label = "Sleep deprivation", 
categorical_var_labels = list(list("Sex", c("Female", "Male"))), 
adjustment_vars = "age_group", adjustment_var_labels = "age", 
title = "Proportions of sleep deprivation by sex.")
}
