% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mantis}
\alias{mantis}
\title{Simulated dataset of Mantis forelimb shapes}
\format{
An object of class \code{list} of length 6.
}
\source{
Dave Angelini \email{david.r.angelini@gmail.com} [aut, cre]
}
\usage{
mantis
}
\description{
A simulated dataset of with 16 landmarks on the femur and tibia for 100 Mantis specimens.
    Two species with male and female shape differences are included.
}
\details{
\describe{
  \item{\code{coords}}{An array of XY coordinates for the shape of multiple specimens (16 landmarks by 2 (XY) by 100 specimens).}
  \item{\code{landmark.number}}{The number of landmarks in the shape data.}
  \item{\code{specimen.number}}{The number of specimens in the shape data.}
  \item{\code{metadata}}{A data frame of simulated metadata.}
  \item{\code{provenance}}{Data provenance.}
}
}
\examples{
# Load the dataset
data("mantis", package = "borealis")

# Define connecting lines
{
  x <- 1:16
  mantis.lines <- matrix(c(x[-length(x)],x[-1]), ncol = 2)
  mantis.lines[10,] <- c(10,1)
  mantis.lines[15,] <- c(15,6)
  mantis.lines <- rbind(mantis.lines,
                        matrix(c(5,11, 6,11, 13,16, 14,16), ncol = 2, byrow = TRUE))
}

landmark.plot(mantis)
names(mantis)

}
\keyword{datasets}
