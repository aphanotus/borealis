% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/align.angle.R
\name{align.angle}
\alias{align.angle}
\title{Align a subset of landmarks by rotation from one point}
\source{
Dave Angelini \email{david.r.angelini@gmail.com} [aut, cre]
}
\usage{
align.angle(
  A,
  art.pt = NULL,
  angle.pts.1,
  angle.pts.2 = NULL,
  rot.pts = NULL,
  angle = 0,
  degrees = TRUE,
  reference.specimen = "all",
  show.plot = TRUE
)
}
\arguments{
\item{A}{A 3-dimensional array containing XY shape coordinates for multiple specimens, or a list containing such as an array and data provenance.}

\item{art.pt}{A number or numeric vector specifying the landmark that define the articulation point between the two landmark subsets.}

\item{angle.pts.1}{A number or numeric vector of landmarks specifying the angle point of one subset.  If more that one value
is provided, the centroid of the landmarks described by the vector will be used; a single value
identifies a specific landmark to use.}

\item{angle.pts.2}{A number or numeric vector of landmarks specifying the angle point of the second subset.
This could be the entire set of points of an articulated structure to be rotated.
If \code{angle.pts.2 = NULL}, then all points other than \code{angle.pts.1} and \code{art.pt} are used.}

\item{rot.pts}{A vector containing numbers specifying which landmarks to rotate.
If \code{NULL}, it is assumed that the points to be rotated are the same as those in \code{angle.pts.2}.}

\item{angle}{An optional value specifying the additional amount by which the rotation should be augmented (in radians).
It might be essential to use a negative angle if centroids from multiple points are used for angle points.  It should be
clear if this is the case, upon plotting results.}

\item{degrees}{A logical value specifying whether the additional rotation angle is expressed in degrees or radians (degrees is the default).}

\item{reference.specimen}{A number or numeric vector specifying which specimens should be taken as the reference for the angle
defined by \code{angle.pts.1}, \code{art.pt}, and \code{angle.pts.2} or provided in \code{angle}.
The default is \code{"all"}, which uses the mean angle of all specimens.}

\item{show.plot}{A logical argument specifying whether to display a plot
comparing the distributions of variance in landmark distances,
corrected for centroid size, before and after alignment to the fixed angle.}
}
\value{
Returns a list with \code{coords}, \code{provenance}, and
   any other potential list elements from the input.
}
\description{
Joints can introduce nuisance variation in landmark-based geometric morphometrics.
This function rotates a subset of landmarks about a pivot point, such that all specimens
will align to a designated reference specimen(s).
This step should be run before Procrustes alignment, and it is robust to differences
in the relative position, orientation and size of specimens.
}
\details{
The input array should contain landmark coordinates with \emph{p}, 2, and \emph{n} dimensions,
where \emph{n} is the number of specimens and \emph{p} is the number of landmarks).
Dimension 2 must have two columns that contain X and Y locations of the landmark.
The function will pass extra arguments to \code{\link[borealis]{landmark.plot}},
including \code{links}. Plotting the landmarks can be useful to ensure that the reference
specimen and pivot point result in sensible alignments.
}
\examples{
data("mantis", package = "borealis")

# Define mantis.lines
{
  x <- 1:16
  mantis.lines <- matrix(c(x[-length(x)],x[-1]), ncol = 2)
  mantis.lines[10,] <- c(10,1)
  mantis.lines[15,] <- c(15,6)
  mantis.lines <- rbind(mantis.lines,
                        matrix(c(5,11, 6,11, 13,16, 14,16), ncol = 2, byrow = TRUE))
}

# angle.pts.1, art.pt (the vertex), and angle.pts.2 define the angle to enforce
# rot.pts defines the landmarks to actually rotate
x <- align.angle(mantis,
                 art.pt = 11,
                 angle.pts.1 = 1:10,
                 angle.pts.2 = 12:15,
                 rot.pts = 12:16)

landmark.plot(mantis, specimen.number = 1:4, links = mantis.lines)
landmark.plot(x, specimen.number = 1:4, links = mantis.lines)

# By default the average of all specimens defines the reference angle
# However reference.specimen can specify one of more specimens to use.
x <- align.angle(mantis,
                 art.pt = 11,
                 angle.pts.1 = 1:10,
                 angle.pts.2 = 12:15,
                 rot.pts = 12:16,
                 reference.specimen = c(1,7) )

landmark.plot(x, specimen.number = 1:4, links = mantis.lines)

}
\references{
Adams, D. C. 1999. Methods for shape analysis of landmark data from articulated structures. \emph{Evolutionary Ecology Research}. 1:959-970.
}
