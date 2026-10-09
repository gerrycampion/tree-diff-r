\name{compute_format_diff}
\alias{compute_format_diff}
\title{Compare Text Inputs by Format}
\description{
Parse two text inputs as JSON, XML, or CSV and return their structured diff.
}
\usage{
compute_format_diff(base_text, compare_text, format)
}
\arguments{
  \item{base_text}{Text containing the original structured data.}
  \item{compare_text}{Text containing the data to compare with the base.}
  \item{format}{Input format: \code{"json"}, \code{"xml"}, or \code{"csv"}.}
}
\value{
A list of diff records. If parsing or comparison fails, returns a list with an
\code{error} element containing the message.
}
\seealso{\code{\link{diff_value}}}