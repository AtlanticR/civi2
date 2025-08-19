#' Convenience that helps generate relevant paths to run the 'targets' pipeline
#'
#' @param path character string for the path to return. Defaults to "onedrive"
#' which will return the shared onedrive target store (i.e. the data). Both "pkg"
#' or "inst" will return the path to the "_targets.R" file (i.e. the instructions)
#' in the installed package directory or the inst directory in your local git repo
#' respectively.
#'
#' @return character string
#' @export
#'
#' @examples
#'
#' onedrive_store <- tar_paths()
#'
tar_paths <- function(path="onedrive"){
  if(path=="onedrive"){
    od <- file.path(Sys.getenv("OneDriveCommercial"),"Checking_Phil_Code","CIVI_targets")
    if(dir.exists(od)){
      return(od)
    } else {
      stop(paste("You do not appear to have access to",od))
    }

  } else if(path=="inst"){
    p <- file.path("inst","_targets.R")
    if(file.exists(p)){
      return(p)
    } else {
      stop(paste("You do not appear to have access to",p))
    }
  } else if(path=="pkg"){
    i <- file.path(system.file(package = 'civi'),"_targets.R")
    if(file.exists(i)){
      return(i)
    } else {
      stop(paste("You do not appear to have access to",i))
    }
  } else {
    stop(paste(path,"is not a recognized 'path' argument"))
  }
}
