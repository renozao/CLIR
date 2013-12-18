# Project: RCLI
# 
# Author: Renaud Gaujoux
# Created: Nov 20, 2013
###############################################################################


#' Enhanced Command Line Argument Parser
#' 
#' Extends the capabilities of package \pkg{argparse}, e.g., in defining sub commands.
#' 
#' @param prog program name
#' @param description program description
#' @param ... extra arguments passed to \code{\link[argparse]{ArgumentParser}}.
#' @param epilog epilog messages to display at the end of the man pages
#' @param show.defaults logical that indicates if default arugment values should 
#' be displayed. 
#' 
#' @export
#' @import argparse
CLIArgumentParser <- function(prog = CLIfile(), description = '', ..., epilog = '', show.defaults = TRUE){
    
    # load argparse
    suppressMessages( library(argparse, quietly = TRUE) )
    
    .flag_newlines <- function(x){
        gsub("\n", "", x)
    }
    
    # define place holders
    description_ph <- '__@@DESCRIPTION@@__'
    command_ph <- '__@@COMMANDS@@__'
    epilog_ph <- '__@@EPILOG@@__'
    p <- ArgumentParser(prog = prog, description = description_ph, ...
                        , epilog = paste0(command_ph, epilog_ph))
    p$description_str <- description
    p$epilog_str <- epilog
    
    # change argument formatter if required
    if( show.defaults ){
        i <- grep("argparse\\.ArgumentParser", p$python_code)
        inst <- p$python_code[i]
        p$python_code[i] <- paste0(substr(inst, 1, nchar(inst)-1), ', formatter_class=argparse.ArgumentDefaultsHelpFormatter)')
    }
    
    p <- proto(p)
    
    # add add_command function
    p$description_loc <- description_ph
    p$command_loc <- command_ph
    p$epilog_loc <- epilog_ph
    p$prog <- prog
    p$exec <- if( nchar(exec_path <- CLIfile(full = TRUE)) ) normalizePath(CLIfile(full = TRUE)) else '' 
    p$command <- list()
    p$command_idefault <- 0L
    p$command_default <- function(.){
        if( .$command_idefault ) names(.$command)[.$command_idefault]
        else ''
    }
    
    # add a (sub-)command
    p$add_command <- function(., command, fun, help='', ..., default = FALSE){
        # add command argument if necessary
        if( !length(.$command) ){
            .$.super$add_argument('command', help = paste0(.$prog, ' command to run'))
        }
        # store command
        .$command[[command]] <- list(fun = fun, help = help)	
        # store command as default
        if( default ) .$command_idefault <- length(.$command)
    }
    #
    
    p$.dummy_arg_help <- '__@@ARGHELP@@__'
    p$argument_help <- character()
    p$add_argument <- function(., ..., help = ''){
        .flag_newlines <- function(x){
            gsub("\n", "", x)
        }
        .$argument_help <- c(.$argument_help, help) 
        help <- .flag_newlines(help)
        help <- .$.dummy_arg_help
        .$.super$add_argument(..., help = help)
    }
    
    # overload print_usage
    p$print_usage <- function(.){
        .$.super$print_usage()
        if( length(.$command) ){
            cat("\n  Use --help for listing all available commands\n")
        }
    }
    #
    
    # overload print_help to add command descriptions
    p$print_help <- function(.){
        
        # get formatted help
        #usage_string <- sub("usage:", '', paste(capture.output(.$.super$print_usage())))
        h <- paste(capture.output(.$.super$print_help()), collapse="\n")
        h <- sub(sprintf("%s", .$description_loc), .$description_str, h)
        
        # substitute dummy help by actual formated help  
        if( length(.$argument_help) ){
#            rd <- .$rd[[1]]
#            print(rd_list.tags(rd))
#            i <- which(sapply(rd, function(x) attr(x, 'Rd_tag') == "\\usage" ))
#            rd[[i]] <- structure(list(structure(usage_string, Rd_tag='RCODE')), Rd_tag = "\\usage")
#            rd <- rd[-(1:2)]
#            i <- which(sapply(rd, function(x) attr(x, 'Rd_tag') == "\\arguments" ))
#            j <- which(sapply(rd[[i]], function(x) attr(x, 'Rd_tag') == "\\item" ))
#            a <- unlist(sapply(rd[[i]][j], '[[', 1L))
#            str(a)
#            h <- RCLI:::rd_txt(rd)
            p <- sprintf("(.*)%s.*", .$.dummy_arg_help)
            i <- grep(p, hs <- strsplit(h, "\n")[[1]])
            pad <- max(nchar(a <- gsub(p, "\\1", hs[i])))
            pad <- sprintf(paste0("\n  %-", pad, "s"), '')
            sapply(.$argument_help, function(x){
                s <- x
                s <- gsub("\\n", pad, s)
                h <<- sub(.$.dummy_arg_help, s, h, fixed = TRUE)
            })
        }
        # fix new lines if necessary
#		nl <- strsplit(h, "##NL##")[[1]]
#		if( length(nl) > 1L ){
#			indent <- nchar(gsub("^([ ]+).*", "\\1", tail(strsplit(nl[1], "\n")[[1L]], 1L)))
#			i <- 2:length(nl)
#			print(sprintf(paste0("%", indent, 's'), ''))
#			nl[i] <- paste0(sprintf(paste0("%", indent, 's'), ''), nl[i])
#			h <- paste0(nl, collapse="\n")
#		}
        
        cmds <- ''
        if( length(.$command) ){
            # format command help
            lm <- max(nchar(names(.$command)))
            fmt <- paste0("  %-", lm, "s")
            cmds <- lapply(.$command, function(x){
                        strwrap(x$help, indent = 4, exdent = 2 + lm + 4, width = 80, simplify = FALSE)
            })
            cmds <- sapply(cmds, paste, collapse = "\n")
            cmds <- paste0(sprintf(fmt, names(.$command)), cmds)
            cmds <- paste0('Commands:\n', paste(cmds, collapse = "\n"))
        }
        h <- sub(.$command_loc, cmds, h, fixed = TRUE)
        h <- sub(.$epilog_loc, .$epilog_str, h, fixed = TRUE)
        cat(h, sep="\n")
    }
    #
    
    # add function call_string
    p$call_string <- function(., args = commandArgs(TRUE)){
        paste(.$prog, paste0(args, collapse = ' '))
    }
        
    # command parer
    p$parse_cmd <- RCLI::parseCMD
    
    p
}

# combine argument parsers
.combineParser <- function(p1, p2){
    
    if( length(i <- grep("^parser\\.add_argument", p2$python_code)) ){
        p1$.that$python_code <- c(p1$python_code, p2$python_code[i])
    }
    p1
}


#' \code{parseCMD} parse command line arguments for sub-commands, 
#' and dispatch to the associated function.
#' 
#' @param parser parser object as returned by \code{CLIArgumentParser}.
#' @param ARGS command line argument to parse, as a named list or a character string.
#' @param debug logical that indicate if debugging information should be printed.
#' 
#' @export
#' @rdname CLIArgumentParser
parseCMD <- function(parser, ARGS = commandArgs(TRUE), debug = FALSE){
    
    if( is.character(ARGS) && length(ARGS) == 1L  ){ # used in dev/debugging
        ARGS <- strsplit(ARGS, ' ')[[1]]
    }
    # fix quotes to avoid python JSON parsing error
    ARGS <- gsub("'", "\"", ARGS)
    
#    library(pkgmaker, quietly = TRUE)
    # define command line arguments
    prog <- parser$prog
    
    # check validity of command
    # shows usage/help in trivial calls
    if( !length(ARGS) ){
        parser$print_usage()
        return( invisible(parser) )
    }else if( !grepl("^-", ARGS[1L]) ){ # first argument is the command
        command <- ARGS[1L]
        if( !command %in% names(parser$command) ){
            stop("unknown ", prog," command '", command, "'\n"
                    , "  Available commands: ", paste0(names(parser$command), collapse = ', ') 
            #, paste(capture.output(parser$print_usage()), collapse = "\n")
                , call. = FALSE
            )
        }
    }else if( any(ARGS %in% c('-h', '--help')) ){
        parser$print_help()
        return( invisible(parser) )
    }else{
        
        # default command if any
        if( nzchar(parser$command_default()) )
            ARGS <- c(parser$command_default(), ARGS)
        else{
            stop("Missing command:\n  "
                    , paste(capture.output(parser$print_usage()), collapse = "\n")
                    , "\n  Available command(s): ", str_out(names(parser$command), Inf, quote=FALSE)
                    , call. = FALSE)
        }
    }
    
    # get command-specific parser
    cmd_fun <- parser$command[[command]]$fun
    cmd_parser <- cmd_fun(ARGS=NULL)
#    print(cmd_parser$python_code)
    ARGS <- ARGS[-1L]
    
    if( !length(ARGS) ){
        # show command line
        cmd_parser$print_usage()
        invisible(cmd_parser)
    }else if( any(ARGS %in% c('-h', '--help')) ){
        cmd_parser$print_help()
        return( invisible(cmd_parser) )
    }else{
        
        # parse command arguments
        args <- cmd_parser$parse_args(ARGS)
        # update CLI arguments
        .CLIargs(args)
#        str(args)
        
        # log call and parsed arguments		
        if( debug ){
            message('Call: ', parser$call_string(ARGS))
            message('Parsed arguments:')
            str(args)
        }
        #
        
        # call command handler
        cmd_fun(ARGS = args)
    }
}

