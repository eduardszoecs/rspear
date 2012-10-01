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
#' 
#' @export
spear <- function(x, taxa = NULL, group = NULL, abundance = NULL, region = NULL, traits = NULL){
  traits <- read.table(file=traits, 
                   header = TRUE, 
                   sep = ";", 
                   stringsAsFactors = FALSE)
  traits <- traits[traits$region == region, ]
  bigdf <- match_traits(x = x, y = traits, takex = taxa, takey = "name")
  bigdf$SPEAR <- ifelse(bigdf$sensitivity > -0.36 & 
    bigdf$generationTime >= 0.5 & 
    bigdf$exposed == 1 & 
    bigdf$migration == 0, 1, 0)
  spear <- ddply(bigdf, group, function(x) c(SPEAR = 100 * sum(log(x[ ,abundance] + 1) * x$SPEAR) / sum(log(x[ ,abundance] + 1))))
  trait <- unique(bigdf[ ,c("name", "region", "exposed", "generationTime", "sensitivity", "migration", "SPEAR")])
  out = list(spear = spear, traits = trait)
}