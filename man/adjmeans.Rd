\name{adjmeans}
\alias{adjmeans}

\title{
Calculate adjusted means using direct standardisation
}
\description{
Calculates adjusted means of a variable by groups defined by another variable using direct standardisation to the structure of the dataset, as defined by one or more variables.
}
\usage{
adjmeans(dataset, outcome_var_name, categorical_vars, outcome_label = outcome_var_name, 
categorical_var_labels = NULL, adjustment_vars = c("age", "sex"), 
adjustment_var_labels = NULL, ndigits = 2, title = "")
}

\arguments{
  \item{dataset}{
A dataframe containing all variables to be used.
}
  \item{outcome_var_name}{
The name of the outcome variable in \code{dataset} (character). This is the variable of which adjusted means will be calculated.
}
  \item{categorical_vars}{
A character vector containing the names of categorical variables which define the groups by which adjusted means will be calculated. They must exist in \code{dataset}.
}
  \item{outcome_label}{
A label for the outcome variable to be printed in the table produced. Defaults to \code{outcome_var_name}.
}
  \item{categorical_var_labels}{
Labels for the categorical variables by which means will be calculated, to be printed in the table produced. This must be a list of length equal to the number of variables for which adjusted means will be calculated, with each element a list of length two, the first element of which is a character with a label for the variable and the second element a character vector with labels for the levels of the variable. For example for two variables, the first of which has 3 levels and the second 2, 
\code{list(list("Variable 1", c("Group 1", "Group 2", "Group 3")),}
\code{list("Variable 2", c("Group 1", "Group 2")))}. 
Note that if there is only one variable it should be 
\code{list(list("Variable 1",} 
\code{c("Group 1", "Group 2", "Group 3")))}.
If null, the levels of the variable are used.
}
  \item{adjustment_vars}{
A character vector containing the variable names of categorical variables to be adjusted for. The default is \code{age} and \code{sex}, which standardises means of the subgroups to the age and sex structure of the overall dataset.
}
  \item{adjustment_var_labels}{
A character vector with labels for the variables adjusted for, to be printed in the table produced. If null, the variable names are used.
}
  \item{ndigits}{
Number of digits to be printed (defaults to 2).
}
  \item{title}{
A title for the table (defaults to blank).
}
}
\details{
The function produces a table of means of some outcome variable by one or more categorical variables using direct standardisation with target population a population with proportions within each group specified by some variables (default is age and sex) identical for all categories of the categorical variable and equal to the overall proportion in the data.
}
\value{
A data frame of adjusted means with categorical variables defining the groupings as rows and outcome categories as columns.
}
\author{
Christiana Kartsonaki <christiana.kartsonaki@gmail.com>
}

\seealso{
\code{\link{adjprop}}
}
\examples{
# Example 1

# generate a dataframe with sleep duration, sex and age group
data <- data.frame("sleep" = rnorm(50, mean = 8, sd = 1.5), 
"sex" = c(rep("m", 25), rep("f", 25)), 
"age_group" = rep(c("20-29", "30-39", "40-49", "50-59", "60-69"), 5))

adjmeans(dataset = data, outcome_var_name = "sleep", 
categorical_vars = "sex", outcome_label = "Sleep duration", 
categorical_var_labels = list(list("Sex", c("Female", "Male"))), 
adjustment_vars = "age_group", adjustment_var_labels = "age", 
title = "Means of sleep duration by sex.")

# Example 2

# generate a dataframe with sleep duration, sex and age group
data <- data.frame("sleep" = rnorm(50, mean = 8, sd = 1.5), 
"sex" = c(rep("m", 25), rep("f", 25)), 
"age_group" = rep(c("20-29", "30-39", "40-49", "50-59", "60-69"), 5))

# no labels, more digits
adjmeans(dataset = data, outcome_var_name = "sleep", 
categorical_vars = "sex", adjustment_vars = "age_group", 
ndigits = 4, title = "Means of sleep duration by sex.")
}