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
            meta <- c(Date = date(), SHA = digest(list(date(), list(x, ...))))
            .metaheader <- c(sprintf("%s: %s", names(meta), meta), sprintf("CLIR: %s", packageVersion('CLIR')))
       }
       cat(paste0("# ", .metaheader), file = file, append = append, sep = "\n")
       append <- TRUE
    }
    
    write(as.yaml(x, ...), file = file, append = append)
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
    
    # return running script if NULL
    if( is.null(x) ) return( cli_self(TRUE) )
    if( missing(x) ) return( commandArgs(trailing.only) )
    
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

#' @export 
cli_spin <- function(outdir, ..., .config = 'config.yml'){
    
    # setup run directory
    if( file.exists(outdir) ){
        unlink(outdir, recursive = TRUE)
    }
    dir.create(outdir, recursive = TRUE)
    
    # copy script
    self <- cli_self()
    file.copy(self, outdir)
    rscript <- file.path(outdir, basename(self))
    l <- readLines(rscript)
    cat(l[-seq(1L, grep("^quit\\(\\)", l)[1L])], file = rscript, sep = "\n")
    
    owd <- setwd(outdir)
    on.exit(setwd(owd))
    
    # write config file
    write.yaml(list(...), file = .config)
    
    library(knitr)
    spin(basename(rscript))
}



