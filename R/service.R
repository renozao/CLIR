# Utility functions to handle package-based services
# 
# Author: Renaud Gaujoux
###############################################################################


#' Path to Report File
#' 
#' @param name name of the service.
#' It can be the name of the script that implements the service or the name of the directory
#' that contains it.
#' @param all logical that indicates if all the files associated with the service should be 
#' returned.
#' @param full.names logical that indicates if the full path to the services file(s) should 
#' be returned or only the path relative to the service base directory.
#' The base directory itself is always returned as an attribute ('path').
#' @param package name of the package where to look for the givne service.
#' @param basedir path to the directory in the package where to look for the script.
#' The path must be relative to the root package directory.
#' If `NULL` then the default value "scripts" is used.
#' 
#' @export 
script_files <- function(name, all = FALSE, full.names = FALSE, package = topenv(parent.frame()), basedir = 'scripts'){
  
  package <- pkgmaker:::packageName(package)
  serv_path <- packagePath(basedir %||% 'scripts', package = package)
  serv_base <- file.path(serv_path, name)
  # extend to actual file if one found the given service directory
  if( dir.exists(serv_base) ){
    serv_files <- sort(list.files(serv_base, include.dirs = TRUE))
    # find suitable scripts and put them first
    exec_files <- grep("\\.r$", serv_files, ignore.case = TRUE, value = TRUE)
    serv_files <- serv_files[order(match(serv_files, exec_files))]
    
    if( !all ) serv_files <- serv_files[1L] 
  
  }else{
    if( file_ext(serv_base) == '' && file.exists(serv_file_r <- paste0(serv_base, ".r")) ) serv_files <- serv_file_r
    else{
      if( !file.exists(serv_base) ) stop("Could not find script '", name, "' in package ", package)
      serv_files <- serv_base
    }
    serv_base <- dirname(serv_files)
  }
  
  # remove base part if not requested otherwise
  if( !full.names ) serv_files <- sub(paste0(serv_base, '/'), "", serv_files, fixed = TRUE)
  
  attr(serv_files, 'path') <- serv_base
  serv_files
  
}

# copies all the files needed for running a service to a target directory
copy_script_files <- function(name, to, ...){
  
  sfiles <- script_files(name, ..., full.names = FALSE, all = TRUE)
  base <- attr(sfiles, 'path')
  file.copy(file.path(base, sfiles), to, recursive = TRUE, overwrite = TRUE)
  sfiles
  
}

#' Renders Rmarkdown Script Files from Packages
#' 
#' Generates a report.
#' 
#' @param ... report parameters checked against their specifications defined in the report YAML header file
#' and passed down to \code{\link[rmarkdown]{render}}.
#' @param params report parameters passed as a named list, typically from command line arguments.
#' If \var{params} is not \code{NULL}, then any argument passed in \var{...} is ignored and will most likely
#' result in an error from [rmarkdown::render].
#' @param output_dir Output directory, where to generate the report.
#' The default is to generate the report in the current working directory.
#' @param quiet logical that indicates if the report should be generated quietly.
#' If `NULL` then only an overview of the script parameters is shown.
#' @param envir environment where the report is rendered. See \code{\link[rmarkdown]{render}}.
#' @inheritParams script_files
#' @param .extra.files vector of paths to extra files to copy to the output directory.
#'  
#' @seealso \code{\link[rmarkdown]{render}}
#' @export
render_script <- function(name, ..., params = NULL, output_dir = '.', quiet = NULL, envir = parent.frame(), package = topenv(parent.frame()), basedir = NULL, .extra.files = NULL){
  
  if( !requireNamespace('rmarkdown') )
    stop('Missing dependency: package "rmarkdown" is needed to render service ', name, ".")
  package <- pkgmaker:::packageName(package)
  if( !pkgmaker::qrequire(package, character.only = TRUE) )
    stop(sprintf('Missing dependency: service package "%s" is needed to render service %s.', package, name))
    
  # setup working directory with service files
  work_dir <- output_dir
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  work_dir <- normalizePath(work_dir)
  # copy service files
  sfiles <- copy_script_files(name, to = work_dir, package = package, basedir = basedir)
  if( !is.null(.extra.files) ) file.copy(.extra.files, work_dir, recursive = TRUE)
  service_file <- sfiles[1L]
  #
  
  # process parameters
  render_params <- list(...)
  PARAMS <- params
  script_params <- yaml_header(file.path(work_dir, service_file), 'params')
  if( is.null(script_params) ){ # no parameter should be passed to the script
    if( length(PARAMS) )
      warning(sprintf('Script does not allow any parameter: discarding script parameters [%s]', str_out(names(PARAMS), total = TRUE)))
    PARAMS <- NULL
    
  }else {
    # split report and render parameters
    if( !length(PARAMS) ){
      PARAMS <- render_params[intersect(names(render_params), names(script_params))]
      render_params <- render_params[setdiff(names(render_params), names(script_params))]
    }
    
    lapply(names(script_params), function(n){
          p <- PARAMS[[n]]
          input_type <- script_params[[n]]$input %||% ''
          # normalize path to file arguments
          if( !is.null(p) && input_type %in% 'file' && isString(p) ) PARAMS[[n]] <<- normalizePath(p)
        })
  }
  
  
  do.quiet <- quiet %||% TRUE
  if( (is.null(quiet) || !do.quiet) && length(PARAMS) ){
    message(sprintf('# Generating report: %s\n* Using %i parameters:', name, length(PARAMS)))
    pstr <- function(x){
      if( isS4(x) ) class(x)
      else x
    }
    param_str <- capture.output(str(lapply(PARAMS, pstr)))[-1L]
    message(paste0(param_str, collapse = "\n"))
    message("========")
  }
  #
  
  # change to output directory
  owd <- setwd(work_dir)
  # setup cleanup on exit
  on.exit({
    # delete service files
    unlink(file.path(work_dir, sfiles), recursive = TRUE)
    # switch back to old working directory
    setwd(owd) 
  })
  #
    
  # render service script
  report_file <- do.call(rmarkdown::render, c(list(service_file, params = PARAMS, envir = envir, quiet = do.quiet), render_params))
  
  # return result 
  # TODO: params should be those resolved in the script but object 'params' seems to be deleted form the environment
  result <- list(report_file = report_file, params = PARAMS)
  
  # retrieve output data
  output <- yaml_header(service_file, 'return')
  if( length(output) ){
    if( is.list(output) ) output <- names(output)
    res <- sapply(output, function(n) get0(n, envir = envir, inherits = FALSE), simplify = FALSE)
    result <- c(result, res)
  }
  
  # save result as an .rds file
  saveRDS(res, 'results.rds')
  
  # return result invisibly
  invisible(result)
  
}
