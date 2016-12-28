# Rd utils
# 
# Author: Renaud Gaujoux
# Created: Nov 20, 2013
###############################################################################

.simplify <- function(x, simplify){
    if( simplify && length(x) == 1L ) x <- x[[1L]]
    x
}

rd_txt <- function(rd, collapse = NULL){
   res <- capture.output(Rd2txt(rd, fragment=TRUE))
   res <- res[nzchar(res)]
   if( isTRUE(collapse) ) collapse <- "\n"
   if( !is.null(collapse) ) res <- paste0(res, collapse = collapse)
   res
}

#' @importFrom tools Rd_db
rd_package <- function(package, ...){
    if( identical(Sys.getenv('R_INSTALL_PKG'), package) ){
        # check if one is installing this very same package
        Rd_db(dir = file.path(getwd(), package))
    }else if( file_test('-d', package) ){
        if( !is_source_package(package) )
            Rd_db(package = basename(package), lib.loc = dirname(package))
        else Rd_db(dir = package)
    }else Rd_db(package = package, ...)
}

rd_list.topics <- function(package, pattern = NULL, ...){
    rd <- rd_package(package)
    res <- lapply(rd, rd_tag, tag = "alias", value = pattern, ...)
    unlist(res, use.names = FALSE)
}

# Borrowed from tools:::RdTags
#' List Rd Tags
#' 
#' List the Rd tags in an Rd object.
#' @param rd Rd object
#' 
#' @export
rd_list.tags <- function(rd){
    res <- sapply(rd, attr, "Rd_tag")
    if (!length(res)) res <- character()
    res
}

rd_tag <- function(rd, tag, value = NULL, ..., exact = FALSE, simplify = TRUE){
    tags <- rd_list.tags(rd)
    if( length(w <- which(tags == sprintf("\\%s", tag))) ){
        rd <- rd[w]
        if( !is.null(value) ){
            if( exact ) f_match <- function(a){ value == a } 
            else f_match <- function(a){ grepl(value, a, ...) }
            w <- which(sapply(rd, f_match))
            rd <- rd[w]
        }
        
        if( length(rd) ){
            .simplify(rd, simplify)
        }
    }
}

#' @importFrom tools Rd2txt 
rd_tag2txt <- function(rd, tag, collapse = TRUE){
    rd_txt(rd_tag(rd[[1]], tag), collapse = collapse)
}
 
rd_topic <- function(x, rd, simplify = TRUE, quiet = FALSE){
    
    # special handling of x
    if( missing(rd) ){
        pattern <- '^([^:]+):(.*)'
        if( !grepl(pattern, x) ) stop("Could not infer package from topic '", x, "'")
        rd <- gsub(pattern, "\\1", x)
        x <- gsub(pattern, "\\2", x)
    }
    
    errmsg <- NULL
    # rd is a package or a directory
    if( isString(rd) ){
        # load package Rd
        errmsg <- sprintf(" in package '%s'", rd)
        rd <- rd_package(rd)
    }
    
    # find topic alias
    sel <- lapply(rd, rd_tag, tag = "alias", value = x, exact = TRUE)
    w <- which(!sapply(sel, is.null))
    # not found?
    if( !length(w) ){
        if( !quiet ) stop("Could not find topic '", x, "'", errmsg)
        return()
    }
    .simplify(rd[w], simplify)
}

rd_topic_tag <- function(x, rd, tag, quiet = FALSE){
    
    t <- rd_topic(x, rd = rd, quiet = quiet, simplify = FALSE)
    # extract specific tag
    res <- lapply(t, rd_tag, tag = tag)
    w <- which(!sapply(res, is.null))
    # not found?
    if( !length(w) ){
        if( !quiet ) stop("Could not find tag '", tag, "' in topic '", x, "'")
        return()
    }
    res[w]
}

rd_topic_args <- function(x, rd, ...){
    rd_topic_tag(x, rd = rd, tag = 'arguments', ...)    
}

#' @importFrom tools parse_Rd
rd_topic_args2txt <- function(x, rd, ..., format = TRUE, collapse = NULL){
    a <- rd_topic_args(x, rd = rd, ...)
    
    if( !is.null(collapse) ){
        res <- rd_txt(a)
        paste0(res, collapse = "\n")
    }else{
        args <- tools:::.Rd_get_argument_table(a)
        # remove ...
        args <- args[args[, 1] != '...', , drop = FALSE]
        
        if( !format ) return( setNames(args[, 2L], args[, 1L]) )
        mapply(function(p, d){        
            tmp <- textConnection(d)
            on.exit( close(tmp) )
            rd <- parse_Rd(tmp, fragment = TRUE)
            rd_txt(rd, collapse = "\n")
        }, args[,1], args[,2], SIMPLIFY = FALSE)
    }
}
