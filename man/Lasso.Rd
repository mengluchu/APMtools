% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Lasso.R
\name{Lasso}
\alias{Lasso}
\title{Lasso model for LUR, crossvalidation, default fold (10)}
\usage{
Lasso(
  variabledf,
  vis1 = T,
  alpha = 1,
  printlambda = F,
  y_varname,
  training,
  test,
  grepstring
)
}
\arguments{
\item{variabledf}{the dataframe containing predictors and dependent variable}

\item{y_varname}{name of the dependent variable.}

\item{training}{the index for the rows used for training.}

\item{test}{the index for the rows used for testing.}

\item{grepstring}{the variable/column names of predictors in Lasso, grepl stlye, e.g. 'ROAD|pop|temp|wind|Rsp|OMI|eleva|coast'}
}
\value{
error matrix, plot selected (min MSE ) coefficients
}
\description{
Lasso model for LUR, crossvalidation, default fold (10)
}
