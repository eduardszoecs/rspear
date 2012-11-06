\name{get_traits}
\alias{get_traits}
\title{Get Trait-data for SPEAR}
\usage{
  get_traits(check = TRUE)
}
\arguments{
  \item{check}{logical; should the database be checked if
  up-to-date?}
}
\value{
  a data.frame with the trait-database.
}
\description{
  Download trait-data from SPEAR
  (http://www.systemecology.eu/spear/spear-calculator/).
}
\details{
  In order to minimize traffic on server trait-data is
  saved locally. \code{get_traits()} downloads the
  trait-data from server to a file 'traits.csv' in the
  working directory.

  If the file already exists in the working directory a
  check is performed if the file is up-to-date with the
  database.
}
\note{
  Normally, \code{get_traits()} is not called separately
  and is the default in \code{\link[=spear]{spear()}}.
  Therefore the trait-table is downloaded once into the
  workspace and checked if up-to-date with the web-server
  when \code{\link[=spear]{spear()}} is used.

  'check' should be set to \code{FALSE} if working offline
  (since the check require an internet connection)
}
\author{
  Eduard Szoecs \email{szoe8822@uni-landau.de}
}
\seealso{
  \code{\link{spear}}
}

