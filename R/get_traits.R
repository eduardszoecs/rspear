#' Get Trait-data for SPEAR
#' 
#' Download trait-data from SPEAR (http://www.systemecology.eu/spear/spear-calculator/).
#' 
#' @param check logical; should the database be checked if up-to-date?
#' 
#' @details
#' In order to minimize traffic on server trait-data is saved locally. 
#' \code{get_traits()} downloads the trait-data from server to a file 'traits.csv' in the working directory.
#' 
#' If the file already exists in the working directory a check is performed if the file is up-to-date with the database.
#' 
#'
#' @return a data.frame with the trait-database.
#'
#' @note
#' Normally, \code{get_traits()} is not called separately and is the default in \code{\link[=spear]{spear()}}. 
#' Therefore the trait-table is downloaded once into the workspace and checked 
#' if up-to-date with the web-server when \code{\link[=spear]{spear()}} is used.
#' 
#' 'check' should be set to \code{FALSE} if working offline (since the check require an internet connection)
#' 
#' @seealso \code{\link{spear}}
#' 
#' @author Eduard Szoecs \email{szoe8822@@uni-landau.de}
#' @export
get_traits <- function(check = TRUE){
  # Check if file exists
  if(!file.exists("traits.csv")){
    download.file("http://www.systemecology.eu/rspear/traits.csv", "traits.csv")
  }
  # check if file is uptodate
  if(check){
    last_update <- as.numeric(readLines("http://www.systemecology.eu/rspear/traits-update.txt", n=1))
    if(as.POSIXct(last_update, origin="1970-01-01") > file.info("traits.csv")$mtime)
      download.file("http://www.systemecology.eu/rspear/traits.csv", "traits.csv")
  }
  out <- read.table("traits.csv", header = TRUE, sep = ",")
  return(out)
}  