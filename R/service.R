# Utility functions to handle package-based services
# 
# Author: Renaud Gaujoux
###############################################################################


#' Path to Report File
#' 
#' @export 
service_file <- function(pattern, package = NULL){
  
  pe <- parent.frame()
  package <- package %||% packageName(topenv(pe))
  sfiles <- list.files(system.file('services', package = package), pattern = pattern, full.names = TRUE)
  sfiles[1L]
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
#' @param help logical that requests the service man page to be shown.
#' If \code{TRUE}, the service is not run at all, and only show the man page before exiting.
#' @param envir environment where the report is rendered. See \code{\link[rmarkdown]{render}}.
#'  
#' @seealso \code{\link[rmarkdown]{render}}
#' @export
render_service <- function(service, ..., args = NULL, work_dir = '.', save = FALSE, envir = parent.frame(), package = NULL){
  
  if( !requireNamespace('rmarkdown') )
    stop('Missing dependency: package "rmarkdown" is needed to render service documents.')
  
  # retrieve service file
  pe <- parent.frame()
  package <- package %||% packageName(topenv(pe))
  service_file <- service_file(service, package = package)
  
  # setup working directory
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(service_file, work_dir, overwrite = TRUE)
  owd <- setwd(work_dir)
  on.exit( setwd(owd) )
  
  # render service script
  PARAMS <- list(...)
  report_file <- rmarkdown::render(basename(service_file), params = PARAMS, envir = envir)
  
  # return result
  result <- list(report_file = report_file, params = PARAMS)
  
  # retrieve output data
  output <- yaml_header(service_file, 'return')
  print(ls(envir))
  if( length(output) ){
    if( is.list(output) ) output <- names(output)
    res <- sapply(output, function(n) get0(n, envir = envir, inherits = FALSE), simplify = FALSE)
    result <- c(result, res)
  }
  
  # save result if requested
  if( !isFALSE(save) ){
    resfile <- 'results.rds'
    if( isString(save) ) resfile <- save
    saveRDS(res, file = resfile)
  }
  
  # return result invisibly
  invisible(result)
  
}

#' Extract Docopt String from R Markdown YAML Header
#' 
#' @param file rmarkdown file
#' 
#' @return string
#' 
#' @importFrom yaml yaml.load
#' @export
yaml.docopt <- function(file){
  
  .collapse <- function(...) paste0(..., collapse = "\n")
  yml <- gsub("^#' ", "", readLines(file))
  i_yml <- grep("^---", yml)
  if( length(i_yml) < 2 ) stop("Invalid YAML header: must be enclosed between '---' lines.")
  yml <- yml[seq(i_yml[1L]+1L, i_yml[2L]-1L)]
  yml_header <- yaml.load(.collapse(yml))
  
  # find out where the docopt string is in the yaml header
  i_docopt <- which(names(yml_header) == 'docopt')
  if( i_docopt == length(yml_header) ){
    lines_docopt <- seq(grep('^docopt\\s*:', yml)[1L], length(yml))
  }else lines_docopt <- seq(grep('^docopt\\s*:', yml)[1L], grep(sprintf('^%s\\s*:', names(yml_header)[i_docopt+1L]), yml)[1L] - 1L)
  
  docopt_lines <- yml[lines_docopt]
  docopt_lines[1L] <- gsub("^docopt\\s*:\\s*[\"']", '', docopt_lines[1L])
  doc <- .collapse(docopt_lines)
  doc <- gsub('["]\\s*$', "", doc)
  
  # attach parsed yaml header
  attr(doc, 'yaml') <- yml_header
  
  doc
  
}


yaml_header <- function(file, section = NULL, simplify = TRUE){
  
  .collapse <- function(...) paste0(..., collapse = "\n")
  yml <- gsub("^#' ", "", readLines(file))
  i_yml <- grep("^---", yml)
  if( length(i_yml) < 2 ) stop("Invalid YAML header: must be enclosed between '---' lines.")
  yml <- yml[seq(i_yml[1L]+1L, i_yml[2L]-1L)]
  # parse YAML
  yml_header <- yaml.load(.collapse(yml))
  
  # find out where the docopt string is in the yaml header
  if( !is.null(section) ){
    yml_header <- yml_header[which(names(yml_header) %in% section)]
    if( length(yml_header) == 1L && simplify ) yml_header <- yml_header[[1L]]
  }
  yml_header
  
}