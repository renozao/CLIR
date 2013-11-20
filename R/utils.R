# Project: RCLI
# 
# Author: Renaud Gaujoux
# Created: Nov 20, 2013
###############################################################################


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

smessage <- function(..., indent = 0L, item = NULL, appendLF = FALSE){
    if( is.null(item) ){ # choose item from indent
        .item <- c('*', '*', '-', '-', '>', '>') 
        item <- .item[indent+1]
    }
    indent <- if( indent ) paste0(rep(' ', indent), collapse='') else ''
    if( nzchar(item) ) item <- paste0(item, ' ')
    message(indent, item, ..., appendLF = appendLF)
}

.hasArgument <- function(ARGS){
    function(x) length(ARGS[[x]]) && nzchar(ARGS[[x]])
}

logMessage <- function(..., appendLF = TRUE, extfile = NULL){
    
    # output to external file as well
    if( !is.null(extfile) ){
        cat(..., if( appendLF ) "\n", sep ='', file = extfile, append = TRUE)
    }
    message(..., appendLF = appendLF)
    
}

resMessage <- function(..., item = '', appendLF = TRUE){
    smessage(..., item = item, appendLF = appendLF)
}

tryCatchWarning <- local({
    W <- list()
    w.handler <- function(w){ # warning handler
            W <<- c(W, list(w))
            invokeRestart("muffleWarning")
    }
    function(expr, ..., format. = FALSE)
    {
            if( missing(expr) ){
                    if( isFALSE(format.) ) return(W)
                    else{
                            if( !length(W) ) return(NULL)
                            w <- str_trim(sapply(W, as.character))
                            if( is.na(format.) ) return(w)
                            res <- paste0('## Warnings:\n', paste0("* ", w, collapse = "\n"))
                                return(res)
                        }
                }
                W <<- list()
                withCallingHandlers(tryCatch(expr, ...)
                            , warning = w.handler)
        }
})
