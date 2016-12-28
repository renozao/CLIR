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
#' 
#' @importFrom pkgmaker packagePath
#' @export 
service_files <- function(name, all = FALSE, full.names = FALSE, package = topenv(parent.frame())){
  
  serv_path <- packagePath('services', package = package)
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
      if( !file.exists(serv_base) ) stop("Could not find service '", name, "' under ", serv_base)
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
copy_service_files <- function(name, to, ...){
  
  sfiles <- service_files(name, ..., full.names = FALSE, all = TRUE)
  base <- attr(sfiles, 'path')
  file.copy(file.path(base, sfiles), to, recursive = TRUE, overwrite = TRUE)
  sfiles
  
}

#' Renders Service Report File
#' 
#' Generates a service report.
#' 
#' @param ... report parameters checked against their specifications defined in the report YAML header file
#' and passed down to \code{\link[rmarkdown]{render}}.
#' @param args report parameters passed as a character vector, typically from command line arguments.
#' If \var{args} is not \code{NULL}, then any argument passed in \var{...} is ignored.
#' @param work_dir path to working directory
#' @param envir environment where the report is rendered. See \code{\link[rmarkdown]{render}}.
#' @inheritParams service_files
#'  
#' @seealso \code{\link[rmarkdown]{render}}
#' @export
render_service <- function(name, ..., args = NULL, work_dir = '.', envir = parent.frame(), package = topenv(parent.frame())){
  
  if( !requireNamespace('rmarkdown') )
    stop('Missing dependency: package "rmarkdown" is needed to render service documents.')
  
  # setup working directory with service files
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  work_dir <- normalizePath(work_dir)
  
  # copy service files
  sfiles <- copy_service_files(name, to = work_dir, package = package)
  service_file <- sfiles[1L]
  # change to output directory
  owd <- setwd(work_dir)
  # setup cleanup on exit
  on.exit({
    # delete service files
    unlink(file.path(work_dir, sfiles), recursive = TRUE)
    # switch back to old working directory
    setwd(owd) 
  })
  
  # render service script
  PARAMS <- list(...)
  report_file <- rmarkdown::render(service_file, params = PARAMS, envir = envir)
  
  # return result
  result <- list(report_file = report_file, params = PARAMS)
  
  # retrieve output data
  output <- yaml_header(service_file, 'return')
  if( length(output) ){
    if( is.list(output) ) output <- names(output)
    res <- sapply(output, function(n) get0(n, envir = envir, inherits = FALSE), simplify = FALSE)
    result <- c(result, res)
  }
  
  # return result invisibly
  invisible(result)
  
}
