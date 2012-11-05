#' Get Trait-data for SPEAR
#' 
#' Download trait-data from SPEAR (http://www.systemecology.eu/spear/spear-calculator/)
#' 
#' @details
#' Trait data is saved as file ('traits.csv') on the local machine in order to reduce traffic on server.
#' 
get_traits <- function(){
  last_update <- as.numeric(readLines("http://www.systemecology.eu/rspear/traits-update.txt", n=1))
  # Check if file exists
  if(!file.exists("traits.csv")){
    download.file("http://www.systemecology.eu/rspear/traits.csv", "traits.csv")
  }
  # check if file is uptodate
  if(as.POSIXct(last_update, origin="1970-01-01") > file.info("traits.csv")$mtime){
    download.file("http://www.systemecology.eu/rspear/traits.csv", "traits.csv")
  }
  out <- read.table("traits.csv", header = TRUE, sep = ",")
  return(out)
}  