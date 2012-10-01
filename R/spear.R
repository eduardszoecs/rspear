#' Calculate SPEAR values
#'
#' @include match_traits.R
#' @import plyr
#' 
#' @param x Abundance data in long format
#' @param taxa character string: columnnames with taxa
#' @param group character vector naming the columns for groups
#' @param abundance character string: columnname of abundances
#' @param region character string: specify region
#' @param traits path to traits database
#' 
#' @return A list with two entries: 
#' 
#' 
#' @export
spear <- function(x, taxa = NULL, group = NULL, abundance = NULL, region = NULL, traits = NULL){
  traits <- read.table(file=traits, 
                   header = TRUE, 
                   sep = ";", 
                   stringsAsFactors = FALSE)
  traits <- traits[traits$region == region, ]
  db_match <- match_traits(x = x, y = traits, takex = taxa, takey = "name")
  trait <- cbind(db_match, traits[match(db_match$taxa_matched, traits$name), -1])
  trait$SPEAR <- ifelse(trait$sensitivity > -0.36 & 
    trait$generationTime >= 0.5 & 
    trait$exposed == 1 & 
    trait$migration == 0, 1, 0)
  if(any(is.na(trait$taxa_matched)))
    warning("There were unmatched species:\n", trait$taxa_data[is.na(trait$taxa_matched)], "\n Set SPEAR to 0")
    trait$SPEAR[is.na(trait$taxa_matched)] <- 0
  if(any(trait$match_val > 0))
    warning("Non-direct taxon matches!\
            Check trait table if match is appropiate!!")
  df <- merge(x, trait, by.x = taxa, by.y = "taxa_data")
  spear <- ddply(df, group, function(x) c(SPEAR = 100 * sum(log(x[ ,abundance] + 1) * x$SPEAR) / sum(log(x[ ,abundance] + 1))))
  out = list(spear = spear, 
             traits = trait[ ,c("taxa_data", "taxa_matched", "match_val", "region", "exposed", "generationTime", "sensitivity", "migration", "SPEAR")])
  class(out$traits) <- "SPEAR_traits"
  return(out)
}