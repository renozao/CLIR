\name{cli_arg}
\alias{cli_arg}
\title{Extracting Command Line Arguments}
\usage{
  cli_arg(x, default = NULL, alt = NULL, required = FALSE,
    trailing.only = TRUE)
}
\arguments{
  \item{x}{parameter name, e.g., \code{'-f'}}

  \item{default}{default value to return if parameter is missing}

  \item{alt}{alternative parameter name, e.g. long form code{'--file'}}

  \item{required}{logical that indicates if the parameter is required.}

  \item{trailing.only}{logical that indicates if the parameter should be looked
in the trailing arguments only, or in the arguments meant for \emph{R} or \emph{Rscript}.}
}
\description{
  Extracting Command Line Arguments
}

