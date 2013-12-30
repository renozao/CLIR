# QSUB utils
# 
# Author: Renaud Gaujoux
# Created: Dec 24, 2013
###############################################################################

#' QSUB: Generic Job Submission to HPC Cluster
#' 
#' \code{cli_qsub} provides a generic way of submitting commands as jobs on an
#' high performance computing cluster (HPC cluster).
#' 
#' @param cmd command line command
#' @param job_name Job name, if provided as \code{NULL} or the empty string \code{''}, 
#' then a unique job name is built based on the command and the current date.
#' @param ... single named argument that specifies the command line argument (and its values) 
#' that should be vectorized into a job array.
#' @param skip names of the arguments in \code{args} to skip, i.e. to remove
#' from the list of argument the job will run with.
#' @param args command line arguments to be used by the/each job.
#' @param email email address to which start/end/error notifications are sent. 
#' @export
cli_qsub <- function(cmd, job_name, ..., skip = 'qsub', args = CLIargs(skip = skip), email = NULL){
    
    # get executed command line, dumping some arguments that will be changed the shell script
    main <- CLIfile()
    do_create_wd <- TRUE
    if( !length(job_name) || !nzchar(job_name) ){
        prefix <- paste0(main, '-', Sys.Date(), '_')
        n_old <- length(grep(paste0("^", prefix), basenames(list.dirs(recursive = FALSE, full.names = FALSE), value = TRUE)))
        job_name <- basename(tempfile(sprintf("%s%04i_", prefix, n_old + 1)))
    }else if( job_name == '.' ){
        job_name <- basename(getwd())
        do_create_wd <- FALSE
    }
    
    if( do_create_wd ){
        # create job directory if necessary
        dir.create(job_name)
        owd <- setwd(job_name)
        on.exit( setwd(owd) )    
    }
    
    # determine job array specifications
    array_spec <- ''
    n <- 1L
    if( length(tasks <- list(...)) > 1L )
        stop('Invalid job array variable: only one variable is supported [', length(tasks), ']')
    if( length(tasks) > 0L ){
        if( is.null(names(tasks)) ) array_var <- tasks[[1L]]   
        else{
            array_var <- names(tasks)
            n <- length(tasks[[1L]])
            args[[array_var]] <- tasks[[1L]]
        }
        array_spec <- sprintf("[%s=%%qsub_arrayid%%]", array_var) 
    }
    ##
    
    # add command at beginning of argument list
    args <- c(cmd, args)
    
    # generate job configuration file
    job_config_file <- "config.yml"
    config <- yaml::as.yaml(args) 
    write(config, file = job_config_file)
    job_config_file <- sprintf("%s%s", job_config_file, array_spec)
    
    # generate qsub script
    shfile <- write.qsub(job_name, sprintf("%s %s --config-file=%s", main, cmd, job_config_file), end = n
                        , email = email)
    
    # submit job
    cli_message("Submitting job ... ")
    qopts <- ''
    if( !is.null(qopts <- .CLIopts()$qsub) ){
        qopts <- paste(qopts, '')
    }
    jobid <- system(paste0('qsub ', qopts, shfile), intern = TRUE)
    cli_smessage('OK [', jobid, ']')
    invisible(jobid)
    
}

write.qsub <- function(jobname, cmd, args = NULL, start = 1L, end = 1L, email = NULL){
    
    template <- '#!/bin/sh
#
# GEOdb annotation package qsub script
#
#%qsub_var% -t %job_start%-%job_end%
#%qsub_var% -N %job_name%
#%qsub_var% -M %job_email%
#%qsub_var% -m abe
#%qsub_var% -j oe
            
cd $PBS_O_WORKDIR
echo "Sub-job number: %qsub_arrayid%"

# env > env.txt

log_dir=log
mkdir -p $log_dir
            
# start
echo "*** START ***"
date
START_FILE="$log_dir/START-%qsub_jobid%"
DONE_FILE="$log_dir/DONE-%qsub_jobid%"
if test -f "$DONE_FILE"; then rm "$DONE_FILE"; fi
touch $START_FILE
echo "\n\n"
            
# run
%job_cmd%
            
# done
echo "\n\n*** DONE ***"
date
touch $DONE_FILE
'

    # create qsub script
    cli_message("Generating qsub script:", appendLF = TRUE)
    res <- template
    cli_smessage('Submitter: ', Sys.info()['user'], appendLF = TRUE, indent = 2L)
    if( length(email) ) res <- gsub("%job_email%", email, res)
    #
    cli_smessage('Job: ', jobname, appendLF = TRUE, indent = 2L)
    res <- gsub("%job_name%", jobname, res)
    #
    cli_smessage(sprintf('Job range: %s-%s', start, end), appendLF = TRUE, indent = 2L)
    res <- gsub("%job_start%", start, res)
    res <- gsub("%job_end%", end, res)
    res <- gsub("%job_cmd%", cmd, res)
    
    # final substitution
    if( !length(args) ) args <- '' 
    res <- gsub("%cli_args%", args, res)
    
    # substitute queueing system variables
    if( !length(qsub_path <- system("which qsub", intern = TRUE)) )
        stop("Could not find qsub command")
    qsub_path <- normalizePath(qsub_path)
    map <- qsub_envar('PBS')
    if( !is.na(qsub_var <- map['var']) ) res <- gsub("%qsub_var%", qsub_var, res)
    mapply(function(name, val){
                            res <<- gsub(sprintf("%%qsub_%s%%", name), sprintf("${%s}", val), res)            
                    }
                    , names(map), map)
    shfile <- 'run.sh'
    cat(res, file = shfile)
    cli_smessage('OK')
    
    normalizePath(shfile)
}

qsub_envar <- function(x){
    
    # get env variable for the given queueing system
    map <- UseMethod('qsub_envar')
    
    # filter out if necessary
    if( length(x) ) map <- map[x]
    
    map
}

qsub_envar.default <- function(x){
    
    if( missing(x) ) x <- 'PBS'
    # use value of plain character objects as class specification 
    if( identical(class(x), 'character') ) x <- structure(character(), class = x)

    qsub_envar(x)
}

qsub_envar.PBS <- function(x){
    var <- c(arrayid = 'ARRAYID', jobid = 'JOBID')
    c(var = 'PBS', setNames(paste0('PBS_', var), names(var)))
}

