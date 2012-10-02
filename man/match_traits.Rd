\name{match_traits}
\alias{match_traits}
\title{Match data with Trait-Database}
\usage{
  match_traits(x, y, takex, takey, exact.first = TRUE)
}
\arguments{
  \item{x}{abundance-data}

  \item{y}{trait-table}

  \item{takex}{character string: taxa column-name}

  \item{takey}{character string: trait-table taxa
  column-name}

  \item{exact.first}{logic: should exact matches computed
  first?}
}
\value{
  a lookuptable, with original and matched data, as well as
  a match-value
}
\description{
  Match data with Trait-Database
}

