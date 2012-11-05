\name{spear}
\alias{spear}
\title{Calculate SPEAR values}
\usage{
  spear(x, taxa = NULL, abundance = NULL, group = NULL,
    region = "Eurasia", traits = get_traits(),
    sensitivity = -0.36, generationTime = 0.5, exposed = 1,
    migration = 0)
}
\arguments{
  \item{x}{Abundance data in long format}

  \item{taxa}{character string: columnnames with taxa}

  \item{group}{character vector naming the columns for
  groups}

  \item{abundance}{character string: columnname of
  abundances}

  \item{region}{character string: specify region}

  \item{traits}{a data.frame holding the trait-database.
  Default is set to the data.frame from
  \code{\link[=get_traits]{get_traits()}}. Also a modified
  trait-table can be supplied (see vignette).}

  \item{sensitivity}{numeric; sensitivity-threshold,
  default '-0.36'}

  \item{generationTime}{numeric; Generation Time threshold,
  default '0.5'}

  \item{exposed}{logical; either '1' (exposed) or '0' (not
  exposed), default '1'}

  \item{migration}{logical; either '1' (migration) '0' (no
  migration), default '0'}
}
\value{
  A list of two data.frames: \item{spear}{SPEARvalues per
  group} \item{traits}{Trait table for each Taxon}
}
\description{
  Calculate SPEAR values
}
\details{
  The SPEAR index is based on binary classification of
  species (or other taxonomic categories) into 'species at
  risk' and 'species not at risk' according to the
  following biological traits: \itemize{ \item
  physiological sensitivity to organic toxicant \item
  generation time \item presence of aquatic stages in water
  during the maximum pesticide usage period \item migration
  abilities. } A taxon is classified as a 'SPEcies At Risk'
  only if it has: (i) Sorganic value >-0.36, (ii)
  generation time >= 0.5 year, aquatic stages (eggs,
  larvae, pupae) during the periods of intensive pesticide
  usage, and (iv) low migration abilities.

  The SPEARpesticides index is computed as relative
  abundance of these taxa for each site and date as
  follows:

  \deqn{SPEAR = \sum log10(x[i] + 1) * y / \sum log10(x[i]
  + 1)}

  where x[i] is the abundance of the taxon i and y is 1 if
  taxon i is classified as 'at risk', otherwise 0.

  For further details about SPEARpesticides see References.
}
\note{
  Threshold-values for classification into SPEAR should
  only be changed if there is strong indication that they
  are different than these defaults!
}
\examples{
require(rspear)
data(spear_example)
head(spear_example)
sp <- spear(spear_example ,
   taxa = "Taxon",
   abundance = "Abundance",
   group = c("Year", "Site"),
   region = "Eurasia")
sp$traits
sp$spear
}
\author{
  Eduard Szoecs \email{szoe8822@uni-landau.de}
}
\references{
  Liess M, Von der Ohe P, 2005. Analyzing effects of
  pesticides on invertebrate communities in streams.
  \emph{Environmental Toxicology and Chemistry}, 24,
  954-965.

  Liess M, Schaefer R, Schriever C, 2008. The footprint of
  pesticide stress in communities - species traits reveal
  community effects of toxicants. \emph{Science of the
  Total Environment}, 406, 484-490.
}
\seealso{
  \code{\link{get_traits}}
}

