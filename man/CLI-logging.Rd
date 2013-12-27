\name{cli_stop}
\alias{cli_log}
\alias{cli_message}
\alias{cli_smessage}
\alias{cli_stop}
\alias{cli_warning}
\title{CLI Logging Features}
\usage{
  cli_stop(...)

  cli_warning(...)

  cli_message(..., indent = 0L, item = NULL,
    appendLF = FALSE)

  cli_log(..., appendLF = TRUE, extfile = NULL)

  cli_smessage(..., item = "", appendLF = TRUE)
}
\arguments{
  \item{...}{arguments passed to the corresponding base
  function.}

  \item{indent}{number of indent spaces}

  \item{item}{itemize character to use, e.g., \code{'*'} or
  \code{'-'}.}

  \item{extfile}{external log file where to save log
  messages. Note that messages are still shown in
  \emph{stderr}.}

  \item{appendLF}{logical: should messages given as a
  character string have a newline appended?}
}
\description{
  The function \code{cli_*} wrap calls to the corresponding
  base functions, to allow automatic formating and
  redirection.
}

