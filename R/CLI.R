# Command Line Interface utils
# 
# Author: Renaud Gaujoux
# Created: May 9, 2013
###############################################################################

#' @include utils.R
NULL

CLIfile <- function(full = FALSE){
    pattern <- "--file=(.*)"
    if( !length(f <- grep(pattern, commandArgs(), value = TRUE)) ) ''
    else{
        pf <- gsub(pattern, "\\1", f)
        if( full ) pf
        else basename(pf)
    }
}

#' Package Specific Command Line Interface
#' 
#' @param package package name
#' @param altfile alternative file that defines the main CLI entry point. 
#' That is a function named \code{CLI}, which takes the list of parsed command line 
#' arguments as its first argument.
#' @param local logical that indicates if the main CLI function should be 
#' defined and evaluated in a local environment, or in the user's Global 
#' environment.
#' @param ARGS list of parsed arguments passed to the main CLI function.
#' @param ... extra arugments passed to the package's CLI function. 
#'   
#' @export
packageCLI <- function(package, altfile = NULL, local = TRUE, ARGS = commandArgs(TRUE), ...){
    
    master_cli <- if( !is.null(package) ) system.file('scripts', 'CLI.R', package = package)
    else if( is.null(altfile) ){
        stop('Could not load CLI definition: argument `package` or `altfile` is required')
    }
    if( !length(master_cli) || !nzchar(master_cli) ){
        master_cli <- altfile
    }
    
    # load CLI
    source(master_cli, keep.source = TRUE, chdir = TRUE, local = local)
    if( !exists('CLI', inherits = FALSE) ){
        stop("Could not start command line interface for package '", package, "': main entry point function CLI() not found.")
    }
    CLI <- get('CLI', inherits = !local)
    
    # run CLI
    CLI(ARGS, ...)
}

