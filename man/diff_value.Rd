\name{diff_value}
\alias{diff_value}
\title{Compare Parsed Values}
\description{
Recursively compare two parsed JSON-like values and return format-aware diff
records. Lists with names represent objects; unnamed lists represent arrays.
}
\usage{
diff_value(
  base_value,
  compare_value,
  base_pointer = "",
  compare_pointer = "",
  format = "json"
)
}
\arguments{
  \item{base_value}{The original parsed value.}
  \item{compare_value}{The value to compare with the base.}
  \item{base_pointer}{Initial path for the base value.}
  \item{compare_pointer}{Initial path for the comparison value.}
  \item{format}{Path format: \code{"json"}, \code{"xml"}, or \code{"csv"}.}
}
\value{
A list of diff records containing an operation, paths, and any applicable
values.
}
\seealso{\code{\link{compute_format_diff}}}