#' Align a subset of landmarks by rotation from one point
#'
#' Joints can introduce nuisance variation in landmark-based geometric morphometrics.
#' This function rotates a subset of landmarks about a pivot point, such that all specimens
#' will align to a designated reference specimen(s).
#' This step should be run before Procrustes alignment, and it is robust to differences
#' in the relative position, orientation and size of specimens.
#'
#' The input array should contain landmark coordinates with \emph{p}, 2, and \emph{n} dimensions,
#' where \emph{n} is the number of specimens and \emph{p} is the number of landmarks).
#' Dimension 2 must have two columns that contain X and Y locations of the landmark.
#' The function will pass extra arguments to \code{\link[borealis]{landmark.plot}},
#' including \code{links}. Plotting the landmarks can be useful to ensure that the reference
#' specimen and pivot point result in sensible alignments.
#'
#' @param A A 3-dimensional array containing XY shape coordinates for multiple specimens, or a list containing such as an array and data provenance.
#' @param art.pt A number specifying which landmark is the articulation point between the two landmark subsets.
#' @param angle.pts.1 A vector or single value specifying the angle point of one subset.  If more that one value
#'     is provided, the centroid of the landmarks described by the vector will be used; a single value
#'     identifies a specific landmark to use.
#' @param angle.pts.2 A vector or single value specifying the angle point of the second subset.
#'     This could be the entire set of points of an articulated structure to be rotated.
#'     If \code{angle.pts.2 = NULL}, then all points other than \code{angle.pts.1} and \code{art.pt} are used.
#' @param rot.pts A vector containing numbers specifying which landmarks are in the subset to be rotated.
#'     If \code{NULL}, it is assumed that the points to be rotated are the same as those in \code{angle.pts.2}.
#' @param angle An optional value specifying the additional amount by which the rotation should be augmented (in radians).
#'     It might be essential to use a negative angle if centroids from multiple points are used for angle points.  It should be
#'     clear if this is the case, upon plotting results.
#' @param degrees A logical value specifying whether the additional rotation angle is expressed in degrees or radians (degrees is the default).
#' @param reference.specimen A number or numeric vector specifying which specimens should be taken as the reference for the angle
#'     defined by \code{angle.pts.1}, \code{art.pt}, and \code{angle.pts.2} or provided in \code{angle}.
#'     The default is \code{"all"}, which uses the mean angle of all specimens.
#' @param show.plot A logical argument specifying whether to display a plot
#'     comparing the distributions of variance in landmark distances,
#'     corrected for centroid size, before and after alignment to the fixed angle.
#'
#' @return Returns a list with \code{coords}, \code{provenance}, and
#'    any other potential list elements from the input.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @references Adams and M.L. Collyer and A. Kaliontzopoulou. 2020. Geomorph: Software for geometric morphometric analyses. R package version 3.2.1. D.C. (\href{https://cran.r-project.org/package=geomorph}{Link})
#' @references Adams, D. C. 1999. Methods for shape analysis of landmark data from articulated structures. \emph{Evolutionary Ecology Research}. 1:959-970. (\href{http://scholar.google.com/scholar?btnG=Search%2BScholar&as_q=%22Methods%2Bfor%2Bshape%2Banalysis%2Bof%2Blandmark%2Bdata%2Bfrom%2Barticulated%2Bstructures%22&as_sauthors=Adams&as_occt=any&as_epq=&as_oq=&as_eq=&as_publication=&as_ylo=&as_yhi=&as_sdtAAP=1&as_sdtp=1}{Link})
#'
#' @export
#'
#' @examples
#' data("mantis", package = "borealis")
#'
#' # Define mantis.lines
#' {
#'   x <- 1:16
#'   mantis.lines <- matrix(c(x[-length(x)],x[-1]), ncol = 2)
#'   mantis.lines[10,] <- c(10,1)
#'   mantis.lines[15,] <- c(15,6)
#'   mantis.lines <- rbind(mantis.lines,
#'                         matrix(c(5,11, 6,11, 13,16, 14,16), ncol = 2, byrow = TRUE))
#' }
#'
#' # angle.pts.1, art.pt (the vertex), and angle.pts.2 define the angle to enforce
#' # rot.pts defines the landmarks to actually rotate
#' x <- align.angle(mantis,
#'                  art.pt = 11,
#'                  angle.pts.1 = 1:10,
#'                  angle.pts.2 = 12:15,
#'                  rot.pts = 12:16)
#'
#' landmark.plot(mantis, specimen.number = 1:4, links = mantis.lines)
#' landmark.plot(x, specimen.number = 1:4, links = mantis.lines)
#'
#' # By default the average of all specimens defines the reference angle
#' # However reference.specimen can specify one of more specimens to use.
#' x <- align.angle(mantis,
#'                  art.pt = 11,
#'                  angle.pts.1 = 1:10,
#'                  angle.pts.2 = 12:15,
#'                  rot.pts = 12:16,
#'                  reference.specimen = c(1,7) )
#'
#' landmark.plot(x, specimen.number = 1:4, links = mantis.lines)
#'

align.angle <- function (
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
{ # Begin the function

  # Don't bother running anything if geomorph isn't installed!
  if (!require("geomorph")) {
    stop("Please run  `install.packages('geomorph')`")
  }

  # Initialize
  shapes <- NULL
  output <- NULL

  # Vet the input
  if (class(A)[1] %in% c("gpagen","list")) {
    if (any(grepl("coords",names(A)))) {
      shapes <- A$coords
      output <- A[-grep("coords",names(A))]
    } else {
      if (any(grepl("land",names(A)))) {
        shapes <- A$land
        output <- A[-grep("land",names(A))]
      } else {
        stop("Error: Input is not a recognized type. (See the help entry: `?align.angle`.)\n")
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shapes <- A
    } else {
      stop("Error: Input is not a recognized type. (See the help entry: `?align.angle`.)\n")
    }
  }
  if (dim(shapes)[2] != 2) {
    stop("Error: requires a matrix of X and Y corrdinates. (See the help entry: `?align.angle`.)\n")
  }
  if (is.null(angle.pts.1) | !is.numeric(angle.pts.1) | (max(angle.pts.1) > dim(shapes)[1]) | (min(angle.pts.1) < 1)) {
    stop("Error: `angle.pts.1` is invalid. See `?align.angle` for usage.\n")
  }
  if (is.null(art.pt) | !is.numeric(art.pt) | (art.pt > dim(shapes)[1]) | (art.pt < 1)) {
    stop("Error: `art.pt` is invalid. See `?align.angle` for usage.\n")
  }
  if (is.null(angle.pts.2)) {
    angle.pts.2 <- 1:(dim(shapes)[1])
    angle.pts.2 <- angle.pts.2[which(!(angle.pts.2 %in% angle.pts.1))]
    angle.pts.2 <- angle.pts.2[-which(angle.pts.2 == art.pt)]
  } else {
    angle.pts.2 <- as.numeric(angle.pts.2)
    if ((max(angle.pts.2) > dim(shapes)[1]) | (min(angle.pts.2) < 1)) {
      stop("Error: `angle.pts.2` is invalid. See `?align.angle` for usage.\n")
    }
  }
  if (is.null(rot.pts)) {
    rot.pts <- angle.pts.2
  } else {
    rot.pts <- as.numeric(rot.pts)
    if ((max(rot.pts) > dim(shapes)[1]) | (min(rot.pts) < 1)) {
      stop("Error: `rot.pts` is invalid. See `?align.angle` for usage.\n")
    }
  }
  if (is.logical(show.plot)) {
    plot.specimen <- sample(1:(dim(shapes)[3]),1)
  } else {
    if (is.numeric(show.plot)) {
      plot.specimen <- show.plot[[1]]
      show.plot <- TRUE
      if ((plot.specimen > dim(shapes)[3]) | (plot.specimen < 1)) {
        stop("Error: Specimen referenced using `show.plot` is invalid. See `?align.angle` for usage.\n")
      }
    }
  }
  if (!((reference.specimen == "all") | is.numeric(reference.specimen))) {
    reference.specimen <- "all"
    warning("`reference.specimen` is invalid. Using `'all'`. See `?align.angle` for usage.\n")
  } else {
    if (length(reference.specimen) < 1) {
      reference.specimen <- "all"
      warning("`reference.specimen` is invalid. Using `'all'`. See `?align.angle` for usage.\n")
    } else {
      if (is.numeric(reference.specimen)) {
        if (any((reference.specimen %% 1) != 0) | any(reference.specimen < 1) | any(reference.specimen > dim(shapes)[3])) {
          reference.specimen <- "all"
          warning("`reference.specimen` is invalid. Using `'all'`. See `?align.angle` for usage.\n")
        }
      }
    }
  }
  # End preliminary vetting of the input and arguments

  x <- apply(shapes, 3, function(m) { na.omit(unlist(centroid.scaled.distances(m))) })
  var.centroid.scaled.distances1 <- apply(x, 1, var)

  # Subfunctions
  get.angle <- function(p0, p1, p2) {
    # For the angle where point P0 is the vertex made with P1 and P2
    # acos((P01^2 + P02^2 - P12^2) / (2 * P01 * P02))
    # where e.g. P01 is the distance from P0 to P1
    p01 <- distance(p0,p1)
    p02 <- distance(p0,p2)
    p12 <- distance(p1,p2)
    theta <- acos((p01^2 + p02^2 - p12^2) / (2 * p01 * p02))
    return(theta)
  }

  rotate.substructure <- function (m, LMs.to.rotate, pivot, theta ) {

    # Trigonometry reminder:
    # Equations for rotating a point by an angle, counter-clockwise, relative to the x-axis
    # x′ = x*cosθ − y*sinθ
    # y′ = y*cosθ + x*sinθ

    # The location of the pivot point
    if (length(pivot)==1) {
      x0 <- m[pivot,1]
      y0 <- m[pivot,2]
    }
    else {
      x0 <- mean(m[pivot,1])
      y0 <- mean(m[pivot,2])
    }

    # Loop for each substructure landmark
    for (j in LMs.to.rotate)
    {
      xj <- m[j,1]
      yj <- m[j,2]

      # Translate the landmark, such that the pivot point becomes the origin
      xj <- xj - x0
      yj <- yj - y0

      # Rotate the landmark
      x.adj <- xj * cos(theta) - yj * sin(theta)
      y.adj <- yj * cos(theta) + xj * sin(theta)

      # Add back the translation values
      x.adj <- x.adj + x0
      y.adj <- y.adj + y0

      # Update shape data
      m[j,1] <- x.adj
      m[j,2] <- y.adj

    } # End Loop for each substructure landmark

    return(m)
  }

  # Find reference angle
  if (reference.specimen=="all") { reference.specimen <- 1:(dim(shapes)[3]) }
  reference.angle <- vector()
  for (i in reference.specimen) {
    if (length(art.pt)==1) { p0 <- shapes[art.pt,,i] }
    else { p0 <- apply(shapes[art.pt,,i], 2, mean) }
    if (length(angle.pts.1)==1) { p1 <- shapes[angle.pts.1,,i] }
    else { p1 <- apply(shapes[angle.pts.1,,i], 2, mean) }
    if (length(angle.pts.2)==1) { p2 <- shapes[angle.pts.2,,i] }
    else { p2 <- apply(shapes[angle.pts.2,,i], 2, mean) }
    reference.angle <- c(reference.angle, get.angle(p0,p1,p2))
  }
  # hist(reference.angle, xlim = c(0,2*pi))
  if (degrees) { angle <- angle * (pi/180) }
  reference.angle <- mean(reference.angle) + angle
  # abline(v=reference.angle)
  # abline(v=(1:3)*(pi/2), col="darkred")

  # #########################################
  # MAIN LOOP
  # #########################################
  for (i in 1:(dim(shapes)[3])) {
    # Find ith specimen's angle
    if (length(art.pt)==1) { p0 <- shapes[art.pt,,i] }
    else { p0 <- apply(shapes[art.pt,,i], 2, mean) }
    if (length(angle.pts.1)==1) { p1 <- shapes[angle.pts.1,,i] }
    else { p1 <- apply(shapes[angle.pts.1,,i], 2, mean) }
    if (length(angle.pts.2)==1) { p2 <- shapes[angle.pts.2,,i] }
    else { p2 <- apply(shapes[angle.pts.2,,i], 2, mean) }
    theta.i <- get.angle(p0,p1,p2)

    shapes[,,i] <- rotate.substructure(shapes[,,i], rot.pts, art.pt, reference.angle - theta.i )

  }
  # End of MAIN LOOP

  # Compare variance
  x <- apply(shapes, 3, function(m) { na.omit(unlist(centroid.scaled.distances(m))) })
  var.centroid.scaled.distances2 <- apply(x, 1, var)
  percent.improvement <- signif(((sum(var.centroid.scaled.distances1) - sum(var.centroid.scaled.distances2)) / sum(var.centroid.scaled.distances1))*100,3)

  if (percent.improvement > 0) {
    s1 <- paste0("Angle alignment reduced variance in centroid size-scaled landmark distances by ",percent.improvement,"%.\n")
  } else {
    percent.improvement <- -1*percent.improvement
    s1 <- paste0("Angle alignment increased variance in centroid size-scaled landmark distances by ",percent.improvement,"%.\n")
  }

  # Viz
  if (show.plot) {
    b <- min(c(var.centroid.scaled.distances1,var.centroid.scaled.distances2)) - 0.0001 # Set the minimum for the breakpoints
    e <- max(c(var.centroid.scaled.distances1,var.centroid.scaled.distances2)) + 0.0001 # Set the maximum for the breakpoints
    options(warn = -1)
    ax <- pretty(c(b,e), n = 9)
    hg1 <- hist(var.centroid.scaled.distances1, breaks = ax, plot = FALSE) # Save first histogram data
    hg2 <- hist(var.centroid.scaled.distances2, breaks = ax, plot = FALSE) # Save 2nd histogram data
    ymax <- max(c(hg1$counts,hg2$counts))
    plot(hg1, col = '#80000080', xlab = "variance in scaled landmark distances", main = NULL, ylim=c(0,ymax)) # Plot 1st histogram using a transparent color
    plot(hg2, col = '#00000050', add = TRUE) # Add 2nd histogram using different color
    legend.text <- c(paste0("before (",signif(sum(var.centroid.scaled.distances1),4),")"),
                     paste0("after (",signif(sum(var.centroid.scaled.distances2),4),")"))
    legend("topright", legend=legend.text, fill=c("#80000080", "#00000050"),
           cex = 0.9, box.lwd = 0)
    options(warn = 0)
    par(mfrow=c(1,1))
  }
  message(s1)

  # Prep the output
  output$coords <- shapes
  if (!any(grepl("provenance",names(output)))) {
    output$provenance <- list()
  }

  s <- paste0(
    paste0("## Angular alignment\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::align.angle` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n"),
    paste0("- angle.pts.1: ",paste0(angle.pts.1, collapse = ", "),"\n"),
    paste0("- angle.pts.2: ",paste0(angle.pts.2, collapse = ", "),"\n"),
    paste0("- pivot point: ",paste0(art.pt, collapse = ", "),"\n"),
    paste0("- rotated points: ",paste0(rot.pts, collapse = ", "),"\n\n"),
    ifelse(is.null(percent.improvement),"\n",paste0(s1,"\n"))
  )

  output$provenance$align.angle <- s

  return(output)

} # End of function
