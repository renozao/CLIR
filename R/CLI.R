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

.CLIargs <- sVariable()

#' @export
CLIargs <- function(format = c('parsed', 'raw', 'cmd'), skip = NULL, args = NULL){
    
    format <- match.arg(format)
    res <- if( is.null(args) ) .CLIargs() else args
    
    # skip some arguments if requested
    if( !is.null(skip) && !is.null(names(res)) ) res <- res[!names(res) %in% skip]
    
    # early exit if no arguments
    if( !length(res) ) return(character())
    
    # format if necessary
    if( !is.null(res) && format != 'parsed' ){
        names(res) <- paste0('--', gsub('_', '-', names(res), fixed = TRUE))
        if( format == 'cmd' )
            res <- paste0(names(res), ' ', res, collapse = ' ')
        
    }
    
    res
}

#' Package Specific Command Line Interface
#' 
#' @param commands a character vector of command names that correspond to 
#' functions in the package's namespace -- that are not necessarily exported.
#' If \code{NULL}, then the package's namespace is looked up for function
#' names with prefix \code{'CLI_'}.
#' @param default default command, that should be an element of \code{commands}.
#' If \code{NULL}, then the CLI will have no default, and will require a 
#' command to be specified.
#' @param ARGS list of parsed arguments passed to the command's function.
#' @param ... extra arguments passed to the package's CLI function. 
#' @param package name of the package that define the CLI entry points 
#'   
#' @export
CLI <- function(commands = NULL, default=NULL, ARGS = commandArgs(TRUE), ..., package = NULL){
    
    # build main CLI
    pkgCLI <- makeCLI(commands = commands, default = default, package = package)
    # run CLI
    .CLIargs(ARGS)
    on.exit( .CLIargs(NULL) )
    pkgCLI(ARGS, ...)
}

file_lookup <- function(f, dirs){
    f <- file.path(dirs, f)
    lk <- sapply(f, file_test, op = '-f')
    names(lk[which(lk)])[1L]
}

makeCLI <- function(commands = NULL, default = NULL, package = NULL){
        
    main_exec <- CLIfile(full = TRUE)
    main <- basename(main_exec)
    # detect package name and path
    if( is.null(package) ){
        if( nzchar(main_exec) 
                && length(dfile <- file_lookup('DESCRIPTION', c(dirname(main_exec), dirname(dirname(main_exec))))) ){
            path <- dirname(dfile)
            package.info <- packageInfo(dfile)
            package <- package.info$Package
        }
    }else{
        if( is.null(path <- find.package(package, quiet = TRUE)) )
            stop("Could not find package '", package, "'", call.=FALSE)
    }
    
    
    # detect entry points in package if necessary
    if( is.null(commands) ) commands <- rd_list.topics(path, pattern = "^CLI_")
    
    # create entry CLI entries
    .CLI_entries <- sapply(seq_along(commands), function(i, ...){
                e <- commands[i]
                # use specified entry names if any
                if( identical(name <- names(e), '') ) name <- NULL
                makeCLIentry(e, name = name, ...)
            }, main = main, path = path, simplify = FALSE)
    
    # define main controller
    function(...){
        
	    # new CLI argument parser
        parser <- CLIArgumentParser(main)
        
        # use first command if no default was provided
        # add a command for each entry point    
        lapply(.CLI_entries, function(e){
            parser$add_command(e$command, e$fun, e$title, default = identical(e$command, default))
        })

        
#        cat(parser$python_code, sep = "\n")
        parser$parse_cmd(...)
        
    }
}

#' @importFrom methods is
is.CLIentry <- function(x) is(x, 'CLIentry')
as.CLIentry <- function(x, name = NULL){
    if( is.CLIentry(x) ) return(x)
    
    res <- list()
    rd <- x
    usage_string <- rd_tag2txt(rd, 'usage')
    res$rd <- rd
    res$entry <- cmd <- gsub(" *([^( ]+).*", "\\1", usage_string)
    res$command <- if( is.null(name) ) gsub("^CLI_", '', cmd) else name
    .title <- rd_tag2txt(rd, 'title')
    res$title <- gsub("^CLI: *", '', .title[[1]][[1]])
    res$description <- rd_tag2txt(rd, 'description')
    res$details <- rd_tag2txt(rd, 'details')

    # describe parameters
    .param_raw <- rd_topic_args2txt(cmd, rd, format = FALSE, quiet = TRUE)
    .param <- rd_topic_args2txt(cmd, rd, format = TRUE, quiet = TRUE)
    CLI_fun <- eval(parse(text = sprintf("function%s{}", gsub(cmd, '', usage_string))))   
	defaults <- formals(CLI_fun)
    res$parameters <- mapply(function(p, d, raw){
            # define specs
            specs <- list(long = p)
            # extract special arguments from help string
            abv_pattern <- "^ *%[[] *(-([^ ,]+))? *,?([^]]*)[]] *(.*)"
            if( grepl(abv_pattern, raw) ){
                    if( nchar(abv <- gsub(abv_pattern, "\\2", raw)) )
                        specs <- c(short = abv, specs)
                    # extra arguments
                    if( nchar(extra <- gsub(abv_pattern, "\\3", raw)) ){
                        extra_specs <- try(eval(parse(text = sprintf("c(%s)", extra))), silent = TRUE)
                        if( is(extra_specs, 'try-error') ){
                            warning(sprintf("Dropped invalid parameter specification: %s", extra))
                        }else specs <- c(specs, extra_specs)
                    }
                    d <- gsub(abv_pattern, "\\4", d)
            }
            specs$help <- d
            if( !is.symbol(def <- defaults[[p]]) ) specs$default <- def
            else specs$required <- TRUE
            
            specs
    }, names(.param), .param, .param_raw, SIMPLIFY = FALSE)
    #
    
    structure(res, class = 'CLIentry')
}

makeCLIentry <- function(entry, main, path, name = NULL){
        
    rd <- rd_topic(entry, path, simplify = FALSE)
    res <- as.CLIentry(rd, name = name)
    ## PARSER
    parser <- CLIArgumentParser(prog = paste(main, res$command)
                                , description = res$description
                                , epilog = res$details)
    
    parser$rd <- res$rd
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
        pkg <- load_package(path)
        entry <- getFunction(entry, where = asNamespace(pkg))
        # call CLI function with arguments
        invisible(do.call(entry, ARGS))
    }  
    
    res$fun <- fun
    res
#    list(entry = name, command = .cmd, title = .title, fun = fun)
}
