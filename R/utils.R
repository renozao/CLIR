# Project: CLIR
# 
# Author: Renaud Gaujoux
# Created: Nov 20, 2013
###############################################################################

is_source_package <- function(path){
    !file.exists(file.path(path, 'Meta'))
}

load_package <- function(path){
    if( is_source_package(path) ){
        qlibrary('devtools', character.only = TRUE)
        qlibrary('methods', character.only = TRUE)
        .silenceF(load_all)(path)
        as.package(path)$package
    }else{
        lib <- normalizePath(dirname(path))
        ol <- .libPaths()
        .libPaths(c(lib, ol))
        on.exit(.libPaths(ol))
        qlibrary(basename(path), character.only = TRUE)
        basename(path)
    }
}

isString <- function(x) is.character(x) && length(x) == 1L 
        
packageInfo <- function(file){
    
    res <- read.dcf(file)
    as.list(res[1, ])
}

sstr <- function(x, collapse = "\n"){
    paste0(capture.output(str(x)), collapse = collapse)
}

.silenceF <- function(f, verbose=FALSE){
    
    if( verbose ) f
    else{
        function(...){
            capture.output(suppressPackageStartupMessages(suppressMessages(res <- f(...)))); 
            res
        }
    }
}

qlibrary <- .silenceF(library, verbose = FALSE)


.hasArgument <- function(ARGS){
    function(x) length(ARGS[[x]]) && nzchar(ARGS[[x]])
}

sVariable <- function(default=NULL){
	.val <- default
	function(value){
		if( missing(value) ) .val
		else{
			old <- .val
			.val <<- value
			old
		}
	}
}

isFALSE <- function(x) identical(x, FALSE)

#' YAML Utilities
#' 
#' @inheritParams yaml::as.yaml
#' @inheritParams base::write
#' @param .metaheader logical that indicates if a metadata header tha includes date time, SHA and 
#' \pkg{CLIR} version number should be added as leading comments.
#' 
#' Can also be a character vector, which is then written "as is", one element per line (each line is 
#' prefixed with a "#").
#' 
#' @export
#' @rdname yaml
#' @importFrom digest digest
write.yaml <- function(x, file, append = FALSE, ..., .metaheader = TRUE){
    
    # add metaheader
    if( !isFALSE(.metaheader) ){
        
        if( isTRUE(.metaheader) ){ # build metaheader
            .metaheader <- cli_metaheader(x, ...)
       }
       cat(paste0("# ", .metaheader), file = file, append = append, sep = "\n")
       append <- TRUE
    }
    
    write(as.yaml(x, ...), file = file, append = append)
}

cli_metaheader <- function(...){
    meta <- c(Date = date(), SHA = digest(list(...)))
    c(sprintf("%s: %s", names(meta), meta), sprintf("CLIR: %s", packageVersion('CLIR')))
}

#' @inheritParams yaml::yaml.load_file
#' @export
#' @rdname yaml
#' @import yaml
read.yaml <- yaml.load_file
        

#' Extracting Command Line Arguments
#' 
#' @param x parameter name, e.g., \code{'-f'}
#' @param default default value to return if parameter is missing
#' @param alt alternative parameter name, e.g. long form code{'--file'} 
#' @param required logical that indicates if the parameter is required.
#' @param trailing.only logical that indicates if the parameter should be looked
#' in the trailing arguments only, or in the arguments meant for \emph{R} or \emph{Rscript}.  
#' 
#' @export
cli_arg <- function(x, default = NULL, alt = NULL, required = FALSE, trailing.only = TRUE){
    
    # return all cli arguments if missing(x)
    if( missing(x) ) return( commandArgs(trailing.only) )
    # return running script if NULL
    if( is.null(x) ) return( cli_self(TRUE) )
    
    res <- default
    args <- commandArgs(trailing.only)
    if( is.numeric(x) ){ # positional argument
        pargs <- grep("^-", args, invert = TRUE, value = TRUE)
        if( length(pargs) >= x ) res <- pargs[x]
        else if( required ) stop("Missing required argument ", x, ".", call. = FALSE)
        
    }else{
        if( !length(i <- which(args == x)) && !is.null(alt)){
            i <- which(args == alt)
        }
        if( length(i) ){
            res <- if( length(args) > i && !grepl("^-", args[i+1L]) ) args[i+1L]
                    else TRUE
            
        }else if( required ) stop("Argument '", x, "' is required.", call. = FALSE)
    }
    
    # return value
    res
}

#' @export
cli_self <- function(full = TRUE){
    CLIfile(full)
}

#' @importFrom tools file_path_sans_ext file_ext
#' @export 
cli_spin <- function(outdir, ..., .file = cli_self(), .config = NULL, .log = NULL){
    
    # setup run directory
    if( file.exists(outdir) ){
        unlink(outdir, recursive = TRUE)
    }
    dir.create(outdir, recursive = TRUE)
    .file <- normalizePath(.file, mustWork = TRUE)
    rscript <- normalizePath(file.path(outdir, basename(.file)), mustWork = FALSE)
    # ensure that the generated file does not overwrite the orginal script 
    if( rscript == .file ){
        i <- 1
        while( file.exists(rscript <- sprintf("%s-%i.%s", file_path_sans_ext(.file), i, file_ext(.file))) ){
            i <- i + 1
        }
    }
        
    ## SETUP SCRIPT
    # copy to output dir
    file.copy(.file, rscript, overwrite = TRUE)
    # remove header section
    l <- readLines(rscript)
    if( !length(ih <- grep("#/header", l, fixed = TRUE) ) ){
        ih <- grep("^quit\\(\\)", l)
    }
    if( length(ih) ) l <- l[-seq(1L, ih[1L])]
    
    # determine config file path
    if( is.null(.config) ){
        .config <- 'config.yml'
        i <- 1L
        while( file.exists(.config) ){
            .config <- sprintf('config-%i.yml', i)
            i <- i + 1L
        }
    }
    
    # append initialisation code
    l <- c(sprintf("# /** %s
# Parent: %s 
# */
#+ cli_config, include = FALSE
.CONFIGFILE <- \"%s\"
e <- environment()
list2env(.CONFIG <- CLIR::read.yaml(.CONFIGFILE), envir = e)
", paste0("# ", cli_metaheader(l), collapse = "\n"), .file, .config),l)
    cat(l, file = rscript, sep = "\n")
    #
    
    ## RUN
    # change to output dir
    owd <- setwd(outdir)
    on.exit(setwd(owd))
    
    # write config file
    config_param <- list(...)
    write.yaml(config_param, file = .config)
    
    # run
    out <- system(sprintf('%sRscript -e "knitr::spin(\'%s\')"', file.path(R.home(), 'bin', ''), basename(rscript)), intern = !is.null(.log))
    if( !is.null(.log) )
        cat(out, file = .log, sep = "\n")
    #
    invisible(out)
}



