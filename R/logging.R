# Project: RCLI
# Log messages and error utils
#
# Author: Renaud Gaujoux
# Created: Nov 21, 2013
###############################################################################
#' @export
cli_stop <- function(...) stop(..., call. = FALSE)

#' @export
cli_warning <- function(...) warning(..., call. = FALSE)

#' @export
cli_message <- function(..., indent = 0L, item = NULL, appendLF = FALSE){
    if( is.null(item) ){ # choose item from indent
        .item <- c('*', '*', '-', '-', '>', '>') 
        item <- .item[indent+1]
    }
    indent <- if( indent ) paste0(rep(' ', indent), collapse='') else ''
    if( nzchar(item) ) item <- paste0(item, ' ')
    message(indent, item, ..., appendLF = appendLF)
}

#' @export
cli_log <- function(..., appendLF = TRUE, extfile = NULL){
    
    # output to external file as well
    if( !is.null(extfile) ){
        cat(..., if( appendLF ) "\n", sep ='', file = extfile, append = TRUE)
    }
    cli_message(..., appendLF = appendLF)
    
}

#' @export
cli_smessage <- function(..., item = '', appendLF = TRUE){
    cli_message(..., item = item, appendLF = appendLF)
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
