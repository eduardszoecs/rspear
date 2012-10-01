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
#' @details
#' The SPEAR index is based on binary classification of species (or other taxonomic categories) into 'species at risk' and 'species not at risk' according to the following biological traits: 
#' \itemize{
#' \item physiological sensitivity to organic toxicant
#' \item generation time
#' \item presence of aquatic stages in water during the maximum pesticide usage period
#' \item migration abilities. 
#' }
#' A taxon is classified as a 'SPEcies At Risk' only if it has: (i) Sorganic value >-0.36, (ii) generation time >= 0.5 year, aquatic stages (eggs, larvae, pupae) during the periods of intensive pesticide usage, and (iv) low migration abilities. 
#' 
#' The SPEARpesticides index is computed as relative abundance of these taxa for each site and date as follows:
#' 
#' \deqn{SPEAR = \sum log10(x[i] + 1) * y / \sum log10(x[i] + 1)}
#' 
#' where x[i] is the abundance of the taxon i and y is 1 if taxon i is classified as 'at risk', otherwise 0. 
#' 
#' For further details about SPEARpesticides index see References.
#'  
#' @return A list of two data.frames:
#' \item{spear}{SPEARvalues per group}
#' \item{traits}{Trait table for each Taxon}
#' 
#' @references 
#' Liess M, Von der Ohe P, 2005. Analyzing effects of pesticides on invertebrate communities in streams. \emph{Environmental Toxicology and Chemistry}, 24, 954-965.
#' 
#' Liess M, Schaefer R, Schriever C, 2008. The footprint of pesticide stress in communities - species traits reveal community effects of toxicants. \emph{Science of the Total Environment}, 406, 484-490.
#' 
#' @author Eduard Szöcs \email{szoe8822@@uni-landau.de}
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