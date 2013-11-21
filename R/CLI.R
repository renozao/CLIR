# Command Line Interface utils
# 
# Author: Renaud Gaujoux
# Created: May 9, 2013
###############################################################################

#' @include utils.R
NULL

CLIfile <- function(full = FALSE){
    pattern <- "--file=(.*)"
    if( !length(f <- grep(pattern, commandArgs(FALSE), value = TRUE)) ) ''
    else{
        pf <- gsub(pattern, "\\1", f[1L])
        if( full ) normalizePath(pf)
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
#' @param ... extra arguments passed to the package's CLI function. 
#'   
#' @export
CLI <- function(default=NULL, ARGS = commandArgs(TRUE), ..., package = NULL){
    
    # build main CLI
    CLI <- makeCLI(default = default, package = package)
        
    # run CLI
    CLI(ARGS, ...)
}

makeCLI <- function(default = NULL, package = NULL){
        
    main_exec <- CLIfile(full = TRUE)
    main <- basename(main_exec)
    # detect package name and path
    if( is.null(package) ){
        if( nchar(main_exec) 
                && file_test('-f', dfile <- file.path(dirname(dirname(main_exec)), 'DESCRIPTION')) ){
            path <- dirname(dirname(main_exec))
            package.info <- packageInfo(dfile)
            package <- package.info$Package 
        }
    }else{
        if( is.null(path <- find.package(package, quiet = TRUE)) )
            stop("Could not find package '", package, "'", call.=FALSE)
    }
    
    
    # detect entry points in environment
    entries <- rd_list.topics(path, pattern = "^CLI_")
    # create
    .CLI_entries <- sapply(entries, makeCLIentry, main = main, path = path
                            , simplify = FALSE)
    
    # define main controller
    function(...){
	    # new CLI argument parser
        parser <- CLIArgumentParser(main)
        
        if( length(.CLI_entries) ){
            # use first command if no default was provided
            if( is.null(default) ) default <- .CLI_entries[[1L]]$command
            # add a command for each entry point
            lapply(.CLI_entries, function(e){
                parser$add_command(e$command, e$fun, e$title, default = e$command == default)
            })
        }
        
#        cat(parser$python_code, sep = "\n")
        parser$parse_cmd(...)
        
    }
}

makeCLIentry <- function(name, main, path){
        
    rd <- rd_topic(name, path, simplify = FALSE)
    .title <- rd_tag2txt(rd, 'title') 
    .title <- gsub("^CLI: *", '', .title[[1]][[1]])
    .desc <- rd_tag2txt(rd, 'description')
    .details <- rd_tag2txt(rd, 'details')
    .param <- rd_topic_args2txt(name, rd, format = TRUE, quiet = TRUE)
    .cmd <- gsub("^\\.?CLI_", '', name)
    
    ## PARSER
    parser <- CLIArgumentParser(prog = paste(main, .cmd)
            , description = .desc
            , epilog = .details)
    
    # describe parameters
    CLI_fun <- rd_tag2txt(rd, 'usage')
    CLI_fun <- eval(parse(text = sprintf("function%s{}", gsub(name, '', CLI_fun))))   
	defaults <- formals(CLI_fun)
    mapply(function(p, d){
            # define specs
            specs <- list(paste0("--", p))
            # extract special arguments from help string
            if( grepl("^ *[[] *-[^ ]", d) ){
                abv_pattern <- "^ *[[] *(-[^ ,]+) *,?([^]]*)[]] *(.*)"
                abv <- gsub(abv_pattern, "\\1", d)
                specs <- c(abv, specs)
                # extra arguments
                if( nchar(extra <- gsub(abv_pattern, "\\2", d)) ){
                    specs <- c(specs, eval(parse(text = sprintf("c(%s)", extra))))
                }
                d <- gsub(abv_pattern, "\\3", d)
            }
            specs$help <- d
            if( !is.symbol(def <- defaults[[p]]) ) specs$default <- def
            else specs$required <- TRUE
            
            # push argument into parser stack
            do.call(parser$add_argument, specs)
        }, names(.param), .param)
    #
    
    # WRAPPER
    fun <- function(ARGS = commandArgs(TRUE)){
        
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
        
        # load package from path
        load_package(path)
        # call CLI function with arguments
        invisible(do.call(name, ARGS))
    }  
    list(entry = name, command = .cmd, title = .title, fun = fun)
}
