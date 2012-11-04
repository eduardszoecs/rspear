\name{spear}
\alias{spear}
\title{Calculate SPEAR values}
\usage{
  spear(x, taxa = NULL, group = NULL, abundance = NULL,
    region = "Eurasia", traits = NULL)
}
\arguments{
  \item{x}{Abundance data in long format}

  \item{taxa}{character string: columnnames with taxa}

  \item{group}{character vector naming the columns for
  groups}

  \item{abundance}{character string: columnname of
  abundances}

  \item{region}{character string: specify region}

  \item{traits}{either a path to traits database or a
  data.frame object}
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

  For further details about SPEARpesticides index see
  References.
}
\examples{
require(rspear)
data(traits)
data(spear_example)
head(spear_example)
sp <- spear(spear_example ,
   taxa = "Taxon",
   group = c("Year", "Site"),
   abundance = "Abundance",
   region = "Eurasia",
   traits=traits)
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

