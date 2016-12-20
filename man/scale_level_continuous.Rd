\name{scale_level_continuous}
\alias{scale_level_continuous}
\title{Gradient for prediction interval levels}
\usage{scale_level_continuous(..., low = "#888888", high = "#BBBBBB", space = "Lab", na.value = "gray30", guide = "colourbar", point = na.value) 
}

\arguments{
\item{...}{Arguments passed to \code{guide_colourbar} or \code{guide_legend}}
\item{low, high}{Colours for low and high ends of the gradient.}
\item{space}{colour space in which to calculate gradient. Must be "Lab" -
other values are deprecated.}
\item{na.value}{Colour to use for missing values}
\item{guide}{Type of legend. Use \code{"colourbar"} for continuous
colour bar, or \code{"legend"} for discrete colour legend.}
\item{point}{shade for point prediction}
}

\description{
Replacement methods for \code{guide_colourbar} and \code{guide_legend}, for use with a level aesthetic.
}

\value{A guide object}

\seealso{\code{\link[ggplot2]{guide_colourbar}}, \code{\link[ggplot2]{guide_legend}}}
