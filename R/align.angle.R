#' Align a subset of landmarks by rotation from one point
#'
#' A wrapper for \code{\link[geomorph]{fixed.angle}}, which rotates a subset of landmarks so that the articulation angle between subsets is constant. (See \code{?geomorph::fixed.angle})
#' This function adds data provenance and optional vizualization.
#'
#' Joints can introduce nuisance variation in landmark-based geometric morphometrics.
#' This function rotates a subset of landmarks about a pivot point.
#' This step should be run before Procrustes alignment, and it is robust to differences
#' in the relative position, orientation and size of specimens.
#'
#' The input array should contain landmark coordinates with \emph{p}, 2, and \emph{n} dimensions,
#' where \emph{n} is the number of specimens and \emph{p} is the number of landmarks).
#' Dimension 2 must have two columns that contain X and Y locations of the landmark.
#'
#' If \code{show.plot = TRUE}, then landmarks from each specimen will be displayed.
#' Gray points indicate main structure (body) landmarks, which are not moved.
#' The original location of subsructure (limb / jaw) landmarks are shown with open red circles;
#' the adjusted location of these landmarks are shown with black points.
#' The function will pass extra arguments to \code{\link[borealis]{landmark.plot}},
#' including \code{links}. Plotting the landmarks can be useful to ensure that the reference
#' specimen and pivot point result in sensible alignments for the other specimens.
#'
#' If the input is a \code{list}, all elements will be retained in the output.
#' If a \code{provenance} element is present, then it will be expanded to include an entry
#' for this processing step.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @references Adams and M.L. Collyer and A. Kaliontzopoulou. 2020. Geomorph: Software for geometric morphometric analyses. R package version 3.2.1. D.C. (\href{https://cran.r-project.org/package=geomorph}{Link})
#' @references Adams, D. C. 1999. Methods for shape analysis of landmark data from articulated structures. \emph{Evolutionary Ecology Research}. 1:959-970. (\href{http://scholar.google.com/scholar?btnG=Search%2BScholar&as_q=%22Methods%2Bfor%2Bshape%2Banalysis%2Bof%2Blandmark%2Bdata%2Bfrom%2Barticulated%2Bstructures%22&as_sauthors=Adams&as_occt=any&as_epq=&as_oq=&as_eq=&as_publication=&as_ylo=&as_yhi=&as_sdtAAP=1&as_sdtp=1}{Link})
#'
#' @return Returns a list with \code{coords}, \code{provenance}, and
#'    any other potential list elements from the input.
#'
#' @param A A 3-dimensional array containing XY shape corrdinates for multiple specimens, or a list containing such as an array and data provenance.
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
#' @param show.plot A logical factor or index number specifying whether to display a plot showing the alginment of one specimen relative to the reference.
#'     If \code{show.plot = TRUE} (the default), then a specimen is plotted at random.
#'     Alternatively, a specimen can be indicated as in \code{show.plot = 1}.
#'     To suppress the visuakl output, set If \code{show.plot = FALSE}.
#' @param plots.vertical Two plots will be generated if \code{show.plot} and \code{compare.variance} are both
#'     \code{TRUE}. This logical value specifyies whether to arrange them vertically (default) or (if \code{plots.vertical = FALSE}) horizontally.
#' @param compare.variance A logical value specifying whether to compare the distibutions of variance in landmark distances, corrected for centroid size.
#'     before and after alignment to the fixed angle.
#' @param provenance An object that should be retained for data provenance.
#'
#' @return Returns a list with \code{coords}, \code{provenance}, and
#'    any other potential list elements from the input.
#'
#' @export
#'
#' @examples
#' data("plethodon", package = "geomorph")
#'
#' pletho <- align.angle(
#'   plethodon,
#'   art.pt = 1,       # These three variables:
#'   angle.pts.1 = 6,  #   art.pt, angle.pts.1 & angle.pts.2
#'   angle.pts.2 = 5,  #   define the angle to align
#'   rot.pts = c(2:5), # rot.pts defines the points to actually rotate
#'   links = "chull")  # Arguments to landmark.plot are accepted too.
#'

align.angle <- function (
  A,
  art.pt = NULL,
  angle.pts.1,
  angle.pts.2 = NULL,
  rot.pts = NULL,
  angle = 0,
  degrees = TRUE,
  show.plot = TRUE,
  plots.vertical = TRUE,
  compare.variance = TRUE,
  provenance = NULL,
  ...
)
{ # Begin the function

  # Don't bother running anything if geomorph isn't installed!
  if (!require("geomorph")) {
    stop("Please run  install.packages('geomorph'). ")
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
        stop("Error: Input is not a recognized type. (See the help entry: '?align.angle'.)")
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shapes <- A
    } else {
      stop("Error: Input is not a recognized type. (See the help entry: '?align.angle'.)")
    }
  }
  if (dim(shapes)[2] != 2) {
    stop("Error: requires a matrix of X and Y corrdinates.")
  }
  if (is.null(angle.pts.1) | !is.numeric(angle.pts.1) | (max(angle.pts.1) > dim(shapes)[1]) | (min(angle.pts.1) < 1)) {
    stop("Error: angle.pts.1 is invalid. See '?align.angle' for usage.\n ")
  }
  if (is.null(art.pt) | !is.numeric(art.pt) | (art.pt > dim(shapes)[1]) | (art.pt < 1)) {
    stop("Error: art.pt is invalid. See '?align.angle' for usage.\n ")
  }
  if (is.null(angle.pts.2)) {
    angle.pts.2 <- 1:dim(shapes)[1]
    angle.pts.2 <- angle.pts.2[which(!(angle.pts.2 %in% angle.pts.1))]
    angle.pts.2 <- angle.pts.2[-which(angle.pts.2 == art.pt)]
  } else {
    angle.pts.2 <- as.numeric(angle.pts.2)
    if ((max(angle.pts.2) > dim(shapes)[1]) | (min(angle.pts.2) < 1)) {
      stop("Error: angle.pts.2 is invalid. See '?align.angle' for usage.\n ")
    }
  }
  if (is.null(rot.pts)) {
    rot.pts <- angle.pts.2
  } else {
    rot.pts <- as.numeric(rot.pts)
    if ((max(rot.pts) > dim(shapes)[1]) | (min(rot.pts) < 1)) {
      stop("Error: rot.pts is invalid. See '?align.angle' for usage.\n ")
    }
  }
  if (is.logical(show.plot)) {
    reference.specimen <- sample(1:(dim(shapes)[3]),1)
  } else {
    if (is.numeric(show.plot)) {
      reference.specimen <- show.plot[[1]]
      show.plot <- TRUE
      if ((reference.specimen > dim(shapes)[3]) | (reference.specimen < 1)) {
        stop("Error: Specimen referenced using  show.plot  is invalid. See '?align.angle' for usage.\n ")
      }
    }
  }

  distance <- function(xy,XY) { sqrt((xy[1]-XY[1])^2+(xy[2]-XY[2])^2) }

  # if (show.plot) {
  #   ref.shape <- shapes[,,reference.specimen]
  #   ref.centroid <- apply(ref.shape,2,mean)
  #   ref.CS <- sum(apply(ref.shape,1, distance, XY=ref.centroid))
  #   ref.shape[,1] <- ref.shape[,1] / ref.centroid[1]
  #   ref.shape[,2] <- ref.shape[,2] / ref.centroid[2]
  #   ref.shape <- ref.shape / ref.CS
  # }

  if (compare.variance) {
    centroid.scaled.distances <- function(m)
    {
      n <- dim(m)[1]
      df <- data.frame(matrix(nrow = n, ncol = n))
      for (i in 1:n) {
        df[i,] <- apply(m, 1, function(x) { distance(x,m[i,]) } )
      }
      df[upper.tri(df, diag = TRUE)] <- NA
      centroid <- apply(m,2,mean)
      CS <- mean(apply(m,1, distance, XY=centroid))
      df <- df / CS
      return(df)
    } # End centroid.scaled.distances function

    x <- apply(shapes, 3, function(m) { na.omit(unlist(centroid.scaled.distances(m))) })
    var.centroid.scaled.distances1 <- apply(x, 1, var)
    # mean.centroid.scaled.distances1 <- apply(shapes, 3, function(m) { mean(abs(unlist(centroid.scaled.distances(m))), na.rm = TRUE) })
  } else { var.centroid.scaled.distances1 <- NULL }

  # Call to fixed.angle
  shapes <- fixed.angle (
    A = shapes,
    art.pt = art.pt,
    angle.pts.1 = angle.pts.1,
    angle.pts.2 = angle.pts.2,
    rot.pts = rot.pts,
    angle = angle,
    degrees = degrees
  )

  # Viz
  if (show.plot) {
    if (compare.variance) { if (plots.vertical) { par(mfrow=c(2,1)) } else { par(mfrow=c(1,2)) } }
    x <- ifelse(
      is.null(dimnames(shapes)[[3]][reference.specimen]),
      paste("specimen:", reference.specimen),
      dimnames(shapes)[[3]][reference.specimen]
    )
    options(warn = -1)
    # Base plot and landmark numbers based on the new positions
    landmark.plot(shapes[,,reference.specimen], main = x, text.color = "darkgray", ...)
    # New substructure positions
    points(shapes[rot.pts,1,reference.specimen], shapes[rot.pts,2,reference.specimen], pch = 16, col = "black")
    # Pivot
    points(shapes[art.pt,1,reference.specimen], shapes[art.pt,2,reference.specimen], pch = 16, col = "darkred")
    # Legend
    legend("topright",
           legend=c("rot.pts","art.pt", paste0("angle = ",angle,ifelse(degrees,"˚  "," rad  "))),
           col=c("black", "darkred", "white"), pch = c(16,16, 0),
           cex = 0.9, box.lwd = 0, bg="transparent")
    options(warn = 0)
  } # End  if (show.plot)

  if (!is.null(var.centroid.scaled.distances1)) {
    x <- apply(shapes, 3, function(m) { na.omit(unlist(centroid.scaled.distances(m))) })
    var.centroid.scaled.distances2 <- apply(x, 1, var)
    # mean.centroid.scaled.distances2 <- apply(shapes, 3, function(m) { mean(abs(unlist(centroid.scaled.distances(m))), na.rm = TRUE) })
    percent.improvement <- signif(((sum(var.centroid.scaled.distances1) - sum(var.centroid.scaled.distances2)) / sum(var.centroid.scaled.distances1))*100,3)
  } else { percent.improvement <- NULL }

  if (!is.null(percent.improvement)) {
    if (percent.improvement > 0) {
      s1 <- paste0("Angle alignment reduced variance in centroid size-scaled landmark distances by ",percent.improvement,"%.\n")
    } else {
      percent.improvement <- -1*percent.improvement
      s1 <- paste0("Angle alignment increased variance in centroid size-scaled landmark distances by ",percent.improvement,"%.\n")
    }
    if (show.plot) {
      # invisible(readline(prompt="Press [enter] to continue"))
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
  }

  # Prep the output
  output$coords <- shapes
  if (!is.null(provenance) & !any(grepl("provenance",names(output)))) {
    output$provenance <- provenance
  }

  s <- paste0(
    paste0("## Angle alignment\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::align.angle` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n"),
    paste0("- angle.pts.1: ",paste0(angle.pts.1, collapse = ", "),"\n"),
    paste0("- angle.pts.2: ",paste0(angle.pts.2, collapse = ", "),"\n"),
    paste0("- pivot point: ",art.pt,"\n"),
    paste0("- rotated points: ",rot.pts,"\n\n"),
    ifelse(is.null(percent.improvement),"\n",paste0(s1,"\n"))
  )

  output$provenance$align.angle <- s

  return(output)

} # End of function
