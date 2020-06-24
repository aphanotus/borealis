#' Consistently orient specimens in an array of coordinate shape values
#'
#' The input array should contain landmark coordinates with \emph{p}, 2, and \emph{n} dimensions,
#' where \emph{n} is the number of specimens and \emph{p} is the number of landmarks).
#' Dimension 2 must have two columns that contain X and Y locations of the landmark.
#'
#' If no values are given for the top, bottom, left or right landmarks, then all specimens
#' will be oriented to match the first specimen in the array.
#'
#' @return Returns a list with \code{coords}, \code{provenance}, and
#'    any other potential list elements from the input.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param A A 3-dimensional array containing XY shape corrdinates for multiple specimens,
#'     or a list containing such as an array and data provenance.
#' @param topLM The row number of the landmark that should be on top.
#' @param bottomLM The row number of the landmark that should be on the bottom.
#' @param leftLM The row number of the landmark that should be on the left.
#' @param rightLM The row number of the landmark that should be on the right.
#' @param include.plot A logical factor specifying whether to include a plot showing th first specimen in the array, after orientation.
#' @param links A matrix with two columns indicating landmarks to connect by lines in the plot. Or \code{"chull"} can be used to draw a convex hull.
#' @param verbose A logical factor specifying whether to report out corrections to each specimen.
#' @param provenance An object that should be retained for data provenance.
#'
#' @export
#'
#' @examples
#' data(plethodon, package = "geomorph")
#'
#' pletho.links <- matrix(c(4,5,5,6,6,7,7,8,8,9,9,10,10,11,2,4,12,2,3,5),
#'                        ncol = 2, byrow = TRUE)
#'
#' curated.coords <- orient(plethodon$land,
#'                          topLM = 8, bottomLM = 2,
#'                          leftLM = 12, rightLM = 7,
#'                          links = pletho.links )
#'

orient <- function(A, topLM = NULL, bottomLM = NULL, leftLM = NULL, rightLM = NULL, include.plot = TRUE, links = NULL, verbose = TRUE, provenance = NULL)
{
  shape.data <- NULL
  output <- NULL

  # Vet the input
  if (class(A)[1] %in% c("gpagen","list")) {
    if (any(grepl("coords",names(A)))) {
      shape.data <- A$coords
      output <- A[-grep("coords",names(A))]
    } else {
      if (any(grepl("land",names(A)))) {
        shape.data <- A$land
        output <- A[-grep("land",names(A))]
      } else {
        return(cat("Error: Input is not a recognized type. (See the help entry: '?orient'.)"))
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shape.data <- A
    } else {
      return(cat("Error: Input is not a recognized type. (See the help entry: '?orient'.)"))
    }
  }
  if (dim(shape.data)[2] != 2) {
    return(cat("Error: requires a matrix of X and Y corrdinates."))
  }

  # If no landmark extremes are given, default to the marginal landmarks in specimen 1
  if (is.null(topLM) & is.null(bottomLM)) {
    topLM <- which.max(shape.data[,2,1])
    bottomLM <- which.min(shape.data[,2,1])
  } else {
    # If only one of the vertical landmarks is supplied, find another at the opposite extreme
    x <- order(shape.data[,2,1])
    if (is.null(topLM)) {
      topLM <- ifelse( (shape.data[bottomLM,2,1] > median(shape.data[,2,1])), x[length(x)], x[1] )
    } else {
      if (is.null(bottomLM)) {
        bottomLM <- ifelse( (shape.data[topLM,2,1] > median(shape.data[,2,1])), x[1], x[length(x)] )
      }
    }
  }
  if (is.null(rightLM) & is.null(leftLM)) {
    leftLM <- which.min(shape.data[,1,1])
    rightLM <- which.max(shape.data[,1,1])
  } else {
    # If only one of the horizontal landmarks is supplied, find another at the opposite extreme
    x <- order(shape.data[,1,1])
    if (is.null(rightLM)) {
      rightLM <- ifelse( (shape.data[leftLM,1,1] > median(shape.data[,1,1])), x[1], x[length(x)] )
    } else {
      if (is.null(leftLM)) {
        leftLM <- ifelse( (shape.data[rightLM,1,1] > median(shape.data[,1,1])), x[length(x)], x[1] )
      }
    }
  }

  landmark.number <- dim(shape.data)[1]
  total.number.flipped <- 0
  report.string <- NULL
  for (i in 1:(dim(shape.data)[3])) {
    OKright <- (shape.data[rightLM,1,i] > shape.data[leftLM,1,i])
    OKup    <- (shape.data[topLM,2,i] > shape.data[bottomLM,2,i])
    if (OKright & OKup) { if (verbose) { cat(dimnames(shape.data)[[3]][i],' ok\n') } }
    else { total.number.flipped <- total.number.flipped +1 }
    if (!OKright) { # If not, mirror the left side to the right.)
      mean.x <- mean(shape.data[,1,i])
      centered.x <- shape.data[,,i] - cbind(rep(mean.x,landmark.number),
                                            rep(0,landmark.number))
      centered.x <- centered.x * cbind(rep(-1,landmark.number),
                                       rep(1,landmark.number))
      shape.data[,,i] <- centered.x + cbind(rep(mean.x,landmark.number),
                                            rep(0,landmark.number))
      report.string <- c(report.string, paste0(dimnames(shape.data)[[3]][i],' X-flipped\n') )
    }
    if (!OKup) { # If not, mirror the top to the bottom.)
      mean.y <- mean(shape.data[,2,i])
      centered.y <- shape.data[,,i] - cbind(rep(0,landmark.number),
                                            rep(mean.y,landmark.number))
      centered.y <- centered.y * cbind(rep(1,landmark.number),
                                       rep(-1,landmark.number))
      shape.data[,,i] <- centered.y + cbind(rep(0,landmark.number),
                                            rep(mean.y,landmark.number))
      report.string <- c(report.string, paste0(dimnames(shape.data)[[3]][i],' Y-flipped\n') )
    }
  }
  if (verbose) {
    cat(paste0(report.string, collapse = ""))
  }
  cat ("\n",total.number.flipped,"specimens re-oriented.")
  if (include.plot) {
    landmark.plot(shape.data[,,1], links = links)
  }

  # Prep the output
  output$coords <- shape.data
  if (!is.null(provenance) & !any(grepl("provenance",names(output)))) {
      output$provenance <- provenance
  }
  s <- paste0(
    paste0("## Specimen re-orientation\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::orient` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n"),
    paste0("- topLM = ",topLM,"\n"),
    paste0("- bottomLM = ",bottomLM,"\n"),
    paste0("- leftLM = ",leftLM,"\n"),
    paste0("- rightLM = ",rightLM,"\n"),
    paste0("\nSpecimens re-oriented: ",total.number.flipped,"\n\n")
  )

  if (!is.null(report.string)) {
    s <- paste0(s, "- ", paste0(report.string, collapse = "- "), "\n")
  }

  output$provenance$reorientation <- s

  return(output)
} # End of function
