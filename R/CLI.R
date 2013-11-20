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

makeCLI <- function(default = '', main = getPackageName(envir), envir = topenv(parent.frame())){
    
    # skip when roxygen
    if( exists('.ROXYGEN', .GlobalEnv) ){
              return( function(ARGS = commandArgs(TRUE)){} )  
    }
    
    if( !is.environment(envir) ) envir <- asNamespace(envir)
    package <- getPackageName(envir)
    
#    str( packagePath(package = package) )
#    print( list.files(packagePath(package = package)) )
#    print( getwd() )
#    print( list.files(getwd()) )
#    cat( system('env', intern = TRUE), file = '~/projects/GEOdb/env.txt', sep = "\n")
    
    
    # detect entry points in environment
    entries <- rd_list.topics(package, pattern = "^CLI_")
    # create
    .CLI_entries <- sapply(entries, function(e){
                      cli <- makeCLIentry(e, main = main, package = package, envir = envir)
#      assign(gsub("^\\.", '', e), cli$entry, envir)
                      cli 
                }, simplify = FALSE)
    
    # define main controller
    function(...){
	    # new CLI argument parser
        parser <- CLIArgumentParser()
        
        # add a command for each entry point
        lapply(.CLI_entries, function(e){
                                parser$add_command(e$cmd, e$title, default = e$cmd == default)
                        })
        
#        cat(parser$python_code, sep = "\n")
        parser$parse_cmd(...)
        
    }
}


## from RGalaxy
#parseSectionFromText <- function(rd, section, required=TRUE)
#{
#    text <- capture.output(tools::Rd2txt(rd))
#    ret <- character()
#    keep <- FALSE
#    found <- FALSE
#    for (line in text)
#    {
#        if (length(grep("^_\b", line)>0) && length(grep(":$", line)>0))
#        {
#            keep <- FALSE ## need this?
#            line <- sub(":$", "", line)
#            line <- gsub("_", "", line, fixed=TRUE)
#            line <- gsub("\b", "", line, fixed=TRUE)
#            if (line == section)
#            {
#                found <- TRUE
#                keep <- TRUE
#            }
#        } else {
#            if (keep)
#            {
#                ret <- c(ret, line)
#            }
#        }
#    }
#    if (!found)
#    {
#        status = "Note: "
#        if (required)
#            status = ""
#        msg <- sprintf("%sDid not find section '%s' in man page.", status, section) 
#        if (required)
#        {
#            stop(msg)
#        } else {
#            message(msg)
#            return("")
#        }
#        
#    }
#    ret <- gsub("^ *", "", ret)
#    if (nchar(ret[1])==0 && length(ret)>2)
#    {
#        ret <- ret[2:length(ret)]
#    }
#    paste(ret, collapse="\n")
#}

makeCLIentry <- function(name, main, package = getPackageName(envir), envir = topenv(parent.frame())){
    
    if( !is.environment(envir) ) envir <- asNamespace(envir)
    
    CLI_fun <- getFunction(name, where = envir)
    rd <- rd_topic(name, package, simplify = FALSE)
    .title <- rd_tag2txt(rd, 'title') 
    .title <- gsub("^CLI: *", '', .title[[1]][[1]])
    .desc <- rd_tag2txt(rd, 'description')
    .details <- rd_tag2txt(rd, 'details')
    .param <- rd_topic_args2txt(name, rd, format = FALSE, quiet = TRUE)
    .cmd <- name <- gsub("^\\.?CLI_", '', name)
    fun <- function(ARGS = commandArgs(TRUE)){
        
        # CMD ARGUMENTS
        parser <- CLIArgumentParser(prog = paste(main, .cmd)
                , description = .desc
                , epilog = .details)
        
        # describe parameters
	    defaults <- formals(CLI_fun)
        mapply(function(p, d){
                                # define specs
                                specs <- list(paste0("--", p))
                                # help string
                                d <- gsub(sprintf('^ *%s: *', p), '', d)
                                if( grepl("^[[]-", d) ){
                                        abv <- gsub("^[[](-[^]]+)[]](.*)", "\\1", d)
                                        specs <- c(abv, specs)
                                        d <- gsub("^[[](-[^]]+)[]](.*)", "\\2", d)
                                }
                                specs$help <- d
                                if( !is.symbol(def <- defaults[[p]]) ) specs$default <- def
                                else specs$required <- TRUE
                                do.call(parser$add_argument, specs)
                        }, names(.param), .param)
        
#        cat(parser$python_code, sep = "\n")
        
        # return parser if no arguments
        if( nargs() == 0L || is.null(ARGS) ) return( parser )
        if( is.character(ARGS) ){ # used in dev/debugging
            # parse arguments if needed
            ARGS <- parser$parse_args(ARGS)
            message('Call: ', parser$call_string(ARGS))
            message('Parsed arguments:')
            str(ARGS)
        }
        
        # call CLI function with arguments
        do.call(CLI_fun, ARGS)
    }  
    list(name = name, cmd = .cmd, title = .title, entry = fun)
}
