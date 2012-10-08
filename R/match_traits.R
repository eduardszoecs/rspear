#' Match data with Trait-Database
#'
#' @param x abundance-data
#' @param y trait-table
#' @param takex character string: taxa column-name
#' @param takey character string: trait-table taxa column-name
#' @param exact.first logic: should exact matches computed first?
#' 
#' @return a lookuptable, with original and matched data, as well as a match-value
#' 
match_traits <- function(x, y, takex, takey, exact.first = TRUE) {
  ## empty object to fill with matches...
  merged <- NULL
  # which colums should be compared?
  xtake <- unique(as.character(x[ , takex]))
  ytake <- as.character(y[ , takey])
  ## ignore case and remove multiple whitespaces
  xtakel <- gsub(" +"," ",tolower(xtake))
  ytakel <- gsub(" +"," ",tolower(ytake))
  ## create indexes
  x.id <- 1:length(xtake)
  y.id <- 1:length(ytake)
  ## match exactly first
  if (exact.first) {
    tmp <- match(xtakel, ytakel)
    if (length(tmp) > 0) {
      ## save in merged what matches 
      merged <- na.omit(rbind(merged,
                              data.frame(x.id = x.id,
                                         y.id = y.id[tmp],
                                         threshold = -1)))
      ##save the unmatched obs indexes in x.id 
      x.id <- x.id[!(x.id %in% merged[ , 1])]
    }
  }
  ## match approximately for each threshold in s
  for (i in seq(0, 0.5, 0.1)) {
    tmp <- sapply(x.id, function(x) agrep(xtakel[x], ytakel[y.id],
                                         max.distance = i)[1])
    ## xm is a index of tmp with the non missing data
    xm <- !is.na(tmp)
    if (sum(xm) > 0) {
      ## put in merged 
      merged <- na.omit(rbind(merged,
                              data.frame(x.id = x.id[xm],
                                         y.id = y.id[tmp[xm]],
                                         threshold = i)))
      x.id <- x.id[!(x.id %in% merged[ , 1])]
    }   
  }
  merged <- data.frame(merged)
  out <- data.frame(taxa_data = xtake[merged$x.id], 
                    taxa_matched = ytake[merged$y.id], 
                    match_val = merged$threshold, 
                    stringsAsFactors = FALSE)
  if(length(x.id) > 0){
    out <- rbind(out, c(xtake[x.id], NA, NA))
  }
  return(out)
}