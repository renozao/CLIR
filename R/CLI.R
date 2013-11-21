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
#' @param default command
#' @param ARGS list of parsed arguments passed to the CLI entry point.
#' @param ... extra arguments passed to the package's CLI function. 
#' @param package name of the package that define the CLI entry points 
#'   
#' @export
CLI <- function(default=NULL, ARGS = commandArgs(TRUE), ..., package = NULL){
    
    # build main CLI
    pkgCLI <- makeCLI(default = default, package = package)
    # run CLI
    pkgCLI(ARGS, ...)
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

#' @importFrom methods is
is.CLIentry <- function(x) is(x, 'CLIentry')
as.CLIentry <- function(x){
    if( is.CLIentry(x) ) return(x)
    
    res <- list()
    rd <- x
    usage_string <- rd_tag2txt(rd, 'usage')
    res$entry <- name <- gsub(" *([^( ]+).*", "\\1", usage_string)
    res$command <- gsub("^CLI_", '', name)
    .title <- rd_tag2txt(rd, 'title')
    res$title <- gsub("^CLI: *", '', .title[[1]][[1]])
    res$description <- rd_tag2txt(rd, 'description')
    res$details <- rd_tag2txt(rd, 'details')
    
    # describe parameters
    .param <- rd_topic_args2txt(name, rd, format = TRUE, quiet = TRUE)
    CLI_fun <- eval(parse(text = sprintf("function%s{}", gsub(name, '', usage_string))))   
	defaults <- formals(CLI_fun)
    res$parameters <- mapply(function(p, d){
            # define specs
            specs <- list(long = p)
            # extract special arguments from help string
            abv_pattern <- "^ *[[] *(-([^ ,]+))? *,?([^]]*)[]] *(.*)"
            if( grepl(abv_pattern, d) ){
                    if( nchar(abv <- gsub(abv_pattern, "\\2", d)) )
                        specs <- c(short = abv, specs)
                    # extra arguments
                    if( nchar(extra <- gsub(abv_pattern, "\\3", d)) ){
                        specs <- c(specs, eval(parse(text = sprintf("c(%s)", extra))))
                    }
                    d <- gsub(abv_pattern, "\\4", d)
            }
            specs$help <- d
            if( !is.symbol(def <- defaults[[p]]) ) specs$default <- def
            else specs$required <- TRUE
            
            specs
    }, names(.param), .param, SIMPLIFY = FALSE)
    #
    
    structure(res, class = 'CLIentry')
}

makeCLIentry <- function(name, main, path){
        
    rd <- rd_topic(name, path, simplify = FALSE)
    res <- as.CLIentry(rd)
    ## PARSER
    parser <- CLIArgumentParser(prog = paste(main, res$command)
                                , description = res$description
                                , epilog = res$details)
    
    lapply(res$parameters, function(x){
        
        if( length(x$short) ) x$short <- paste0('-', x$short)
        x$long <- paste0('--', gsub('_', '-', x$long, fixed = TRUE))
        # remove names
        i <- names(x) %in% c('short', 'long')
        names(x)[i] <- ''
        # push into argument stack
        do.call(parser$add_argument, x)
    })
    
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
    
    res$fun <- fun
    res
#    list(entry = name, command = .cmd, title = .title, fun = fun)
}
