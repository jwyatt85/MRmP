% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_margins.R
\name{get_margins}
\alias{get_margins}
\title{MRmP - State Margins}
\usage{
get_margins(states, vars, custom_margins = NULL)
}
\arguments{
\item{states}{Character vector of state abbreviations. Alternatively or you
can pass it \sQuote{ALL} to get margins for all states in DC.}

\item{vars}{List of variables to get marginal proportions for each state:
sex, age, race, education.}

\item{custom_margins}{}
}
\description{
Gets state level marginal proportions for varyings groups such as age, sex,
education, race
}
\examples{
get_margins(states = c('DC', 'FL'), vars = c('sex', 'age', 'education', 'race', 'obama12'))
}
\author{
James Wyatt
}

