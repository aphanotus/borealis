% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/landmark.plot.R
\name{landmark.plot}
\alias{landmark.plot}
\title{Plot the relative position of landmarks}
\source{
Dave Angelini \email{david.r.angelini@gmail.com} [aut, cre]
}
\usage{
landmark.plot(
  A,
  specimen.number = NULL,
  square = TRUE,
  axes = FALSE,
  def.grids = FALSE,
  landmark.numbers = TRUE,
  links = NULL,
  panels = c(1, 1),
  text.color = "darkred",
  line.color = "darkgray",
  ...
)
}
\arguments{
\item{A}{A 2D matrix of X and Y shape coordinates,
a 3-dimensional array containing XY coordinates for multiple specimens, or
a list containing such as an array.}

\item{specimen.number}{If an array is provided, the specimen number(s) to plot.}

\item{square}{A logical factor specifying whether the aspect ratio of the plot should be equal.}

\item{axes}{A logical factor specifying whether to include x and y axes.}

\item{def.grids}{A logical factor specifying whether to plot deformation grids (thin plate splines) using \code{geomorph::plotRefToTarget}.
If so, the function uses \code{method = "TPS"} and the reference is set to the censusus shape for the entire the coordinate array.}

\item{landmark.numbers}{A logical factor specifying whether landmarks should appear as numbers (if TRUE) or as dots.}

\item{links}{A matrix with two columns indicating landmarks to connect by lines.
Alternatively, enter \code{"chull"} to draw a convex hull
or \code{"ordinal"} to link landmarks in numerical order.}

\item{panels}{A vector with exactly two integers specifying the number of rows and columns of specimens to plot.
If the number of panels exceeds the number of values entered in \code{specimen.number} then the next consecutive specimens will be shown,
starting from specimen 1 if no value is provided for \code{specimen.number}.}

\item{text.color}{Color names or value for the text.}

\item{line.color}{Color names or value for lines.}
}
\description{
Plot the relative position of landmarks
}
\examples{
data(plethodon, package = "geomorph")

# The function will detect whether the input object is
# a 2D set of coordinates or a 3D array
# The following are all equivalent
landmark.plot(plethodon)
landmark.plot(plethodon$land, specimen.number = 1)
landmark.plot(plethodon$land[,,1])

# Convex hull
landmark.plot(plethodon$land, links = "chull")
# Link landmarks in numerical order
landmark.plot(plethodon$land, links = "ordinal")

# Custom landmark connections
pletho.links <- matrix(c(4,5,5,6,6,7,7,8,8,9,9,10,10,11,2,4,12,2,3,5),
                       ncol = 2, byrow = TRUE)
landmark.plot(plethodon$land, links = pletho.links)

# Change colors
landmark.plot(plethodon$land, links = pletho.links,
              text.color = "grey40",
              line.color = "darkblue")

# Multiple panels
landmark.plot(plethodon$land, links = pletho.links, specimen.number = 1:4 )
landmark.plot(plethodon$land, links = pletho.links, panels = c(3,2) )

}
