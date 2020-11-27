#' Align a subset of landmarks by linear extension
#'
#' Nuisance variation in landmark positions can be introduced by extension of
#' certain structures. Fore example the extension of an insect's head relative
#' to the rest of its body.
#' This function moves a subset of landmarks in or out, such that all specimens
#' will align to a median distance or to a designated reference specimen(s).
#' This step should be run before Procrustes alignment, and it is robust to
#' differences in the relative position, orientation and size of specimens.
#'
#' The input array should contain landmark coordinates with \emph{p}, 2, and
#' \emph{n} dimensions, where \emph{n} is the number of specimens and \emph{p}
#' is the number of landmarks). Dimension 2 must have two columns that contain
#' X and Y locations of the landmark. The function will pass extra arguments
#' to \code{\link[borealis]{landmark.plot}}, including \code{links}.
#'
#' @param A A 3-dimensional array containing XY shape coordinates for multiple specimens, or a list containing such as an array and data provenance.
#' @param pts.1 A vector or single value specifying the angle point of one subset.  If more that one value
#'     is provided, the centroid of the landmarks described by the vector will be used; a single value
#'     identifies a specific landmark to use.
#' @param pts.2 A vector or single value specifying the angle point of the second subset.
#'     This could be the entire set of points of an articulated structure to be rotated.
#'     If \code{pts.2 = NULL}, then all points other than \code{pts.1} and \code{art.pt} are used.
#' @param distance An optional value specifying the additional amount by which the rotation should be augmented (in radians).
#'     It might be essential to use a negative angle if centroids from multiple points are used for angle points.  It should be
#'     clear if this is the case, upon plotting results.
#' @param reference.specimen A number or numeric vector specifying which specimens should be taken as the reference for the angle
#'     defined by \code{pts.1}, \code{art.pt}, and \code{pts.2} or provided in \code{angle}.
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
#' @export
#'
#' @examples
#' # Load the dataset
#' data("Jadera", package = "borealis")
#'
#' # Define connecting lines
#' {
#'   x <- 1:9
#'   jhae.lines <- matrix(c(x[-length(x)], x[length(x)], x[-1], x[1]), ncol = 2)
#'   x <- c(10,11,17,16,15,12,13,14)
#'   jhae.lines <- rbind(jhae.lines, matrix(c(x[-length(x)], x[length(x)], x[-1], x[1]), ncol = 2) )
#'   x <- c(17,18,15,16)
#'   jhae.lines <- rbind(jhae.lines, matrix(c(x[-length(x)], x[length(x)], x[-1], x[1]), ncol = 2) )
#'   x <- c(19,21,22,20,26,25,32:34,31,35:39,29,40:42)
#'   jhae.lines <- rbind(jhae.lines, matrix(c(x[-length(x)], x[length(x)], x[-1], x[1]), ncol = 2) )
#'   x <- c(19,23, 23,24, 23,28, 28,29, 19,24, 24,27, 27,28, 19,25, 27,30)
#'   jhae.lines <- rbind(jhae.lines, matrix(x, ncol = 2, byrow = TRUE) )
#'   x <- c(1,3, 7,9, 14,16, 16,18)
#'   jhae.lines <- rbind(jhae.lines, matrix(x, ncol = 2, byrow = TRUE) )
#' }
#'
#' landmark.plot(Jadera, specimen.number = 1:4, axes = TRUE, links = jhae.lines)
#'
#' j2 <- align.extension(Jadera, pts.1 = 1:9, pts.2 = 10:18)
#'
#' landmark.plot(j2, specimen.number = 1:4, axes = TRUE, links = jhae.lines)
#'

align.extension <- function (
  A,
  pts.1,
  pts.2 = NULL,
  distance = 0,
  reference.specimen = "all",
  show.plot = TRUE
)
{ # Begin the function

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
        stop("Error: Input is not a recognized type. (See the help entry: `?align.extension`.)\n")
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shapes <- A
    } else {
      stop("Error: Input is not a recognized type. (See the help entry: `?align.extension`.)\n")
    }
  }
  if (dim(shapes)[2] != 2) {
    stop("Error: requires a matrix of X and Y corrdinates. (See the help entry: `?align.extension`.)\n")
  }
  if (is.null(pts.1) | !is.numeric(pts.1) | (max(pts.1) > dim(shapes)[1]) | (min(pts.1) < 1)) {
    stop("Error: `pts.1` is invalid. See `?align.extension` for usage.\n")
  }
  if (is.null(pts.2)) {
    pts.2 <- 1:(dim(shapes)[1])
    pts.2 <- pts.2[which(!(pts.2 %in% pts.1))]
  } else {
    pts.2 <- as.numeric(pts.2)
    if ((max(pts.2) > dim(shapes)[1]) | (min(pts.2) < 1)) {
      stop("Error: `pts.2` is invalid. See `?align.extension` for usage.\n")
    }
  }

  if (is.logical(show.plot)) {
    plot.specimen <- sample(1:(dim(shapes)[3]),1)
  } else {
    if (is.numeric(show.plot)) {
      plot.specimen <- show.plot[[1]]
      show.plot <- TRUE
      if ((plot.specimen > dim(shapes)[3]) | (plot.specimen < 1)) {
        stop("Error: Specimen referenced using `show.plot` is invalid. See `?align.extension` for usage.\n")
      }
    }
  }
  if (!((reference.specimen == "all") | is.numeric(reference.specimen))) {
    reference.specimen <- "all"
    warning("`reference.specimen` is invalid. Using `'all'`. See `?align.extension` for usage.\n")
  } else {
    if (length(reference.specimen) < 1) {
      reference.specimen <- "all"
      warning("`reference.specimen` is invalid. Using `'all'`. See `?align.extension` for usage.\n")
    } else {
      if (is.numeric(reference.specimen)) {
        if (any((reference.specimen %% 1) != 0) | any(reference.specimen < 1) | any(reference.specimen > dim(shapes)[3])) {
          reference.specimen <- "all"
          warning("`reference.specimen` is invalid. Using `'all'`. See `?align.extension` for usage.\n")
        }
      }
    }
  }
  # End preliminary vetting of the input and arguments

  x <- apply(shapes, 3, function(m) { na.omit(unlist(centroid.scaled.distances(m))) })
  var.centroid.scaled.distances1 <- apply(x, 1, var)

  translate.substructure <- function (m, pts.1, pts.2, ref.dist ) {
    if (length(pts.1)==1) { p1 <- m[pts.1,] }
    else { p1 <- apply(m[pts.1,], 2, mean) }
    if (length(pts.2)==1) { p2 <- m[pts.2,] }
    else { p2 <- apply(m[pts.2,], 2, mean) }
    dist.i <- borealis::distance(p1,p2)
    CS <- sqrt(sum( (m[,1]-mean(m[,1]))^2 + (m[,2]-mean(m[,2]))^2 ) )
    multiplier <- ((ref.dist * CS) / dist.i)
    dist.x <- p2[1] - p1[1]
    dist.y <- p2[2] - p1[2]
    adj.x <- dist.x - (dist.x * multiplier)
    adj.y <- dist.y - (dist.y * multiplier)
    m[pts.1,1] <- m[pts.1,1] + adj.x
    m[pts.1,2] <- m[pts.1,2] + adj.y
    return(m)
  } # end   translate.substructure function

  # Find reference distance
  if (reference.specimen=="all") { reference.specimen <- 1:(dim(shapes)[3]) }
  reference.distance <- vector()
  for (i in reference.specimen) {
    if (length(pts.1)==1) { p1 <- shapes[pts.1,,i] }
    else { p1 <- apply(shapes[pts.1,,i], 2, mean) }
    if (length(pts.2)==1) { p2 <- shapes[pts.2,,i] }
    else { p2 <- apply(shapes[pts.2,,i], 2, mean) }
    CS <- sqrt(sum( (shapes[,1,i]-mean(shapes[,1,i]))^2 + (shapes[,2,i]-mean(shapes[,2,i]))^2 ) )
    reference.distance <- c(reference.distance, (borealis::distance(p1,p2) / CS))
  }
  reference.distance <- mean(reference.distance)

  # #########################################
  # MAIN LOOP
  # #########################################
  for (i in 1:(dim(shapes)[3])) {
    shapes[,,i] <- translate.substructure(shapes[,,i], pts.1, pts.2, reference.distance )
  }
  # End of MAIN LOOP

  # Compare variance
  x <- apply(shapes, 3, function(m) { na.omit(unlist(centroid.scaled.distances(m))) })
  var.centroid.scaled.distances2 <- apply(x, 1, var)
  percent.improvement <- signif(((sum(var.centroid.scaled.distances1) - sum(var.centroid.scaled.distances2)) / sum(var.centroid.scaled.distances1))*100,3)

  if (percent.improvement > 0) {
    s1 <- paste0("Extension alignment reduced variance in centroid size-scaled landmark distances by ",percent.improvement,"%.\n")
  } else {
    percent.improvement <- -1*percent.improvement
    s1 <- paste0("Extension alignment increased variance in centroid size-scaled landmark distances by ",percent.improvement,"%.\n")
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
    paste0("## Extension alignment\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::align.extension` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n"),
    paste0("- pts.1: ",paste0(pts.1, collapse = ", "),"\n"),
    paste0("- pts.2: ",paste0(pts.2, collapse = ", "),"\n"),
    ifelse(is.null(percent.improvement),"\n",paste0(s1,"\n"))
  )

  output$provenance$align.extension <- s

  return(output)

} # End of function
