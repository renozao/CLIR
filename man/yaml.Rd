\name{write.yaml}
\alias{read.yaml}
\alias{write.yaml}
\title{YAML Utilities}
\usage{
  write.yaml(x, file, append = FALSE, ...)

  read.yaml(input, ...)
}
\arguments{
  \item{x}{ the object to be converted }

  \item{file}{A connection, or a character string naming
  the file to write to.  If \code{""}, print to the
  standard output connection.  If it is \code{"|cmd"}, the
  output is piped to the command given by \file{cmd}.  }

  \item{append}{if \code{TRUE} the data \code{x} are
  appended to the connection.}

  \item{...}{ arguments to pass to yaml.load }

  \item{input}{ a filename or connection }
}
\description{
  YAML Utilities
}

