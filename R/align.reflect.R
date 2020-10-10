#' Consistently orient specimens in an array of coordinate shape values
#'
#' The input array should contain landmark coordinates with \emph{p}, 2, and \emph{n} dimensions,
#' where \emph{n} is the number of specimens and \emph{p} is the number of landmarks).
#' Dimension 2 must have two columns that contain X and Y locations of the landmark.
#' The function will pass extra arguments to \code{\link[borealis]{landmark.plot}},
#' including \code{links}. Plotting the landmarks can be useful to ensure that the reference
#' specimen and pivot point result in sensible alignments for the other specimens.
#' If the input is a \code{list}, all elements will be retained in the output.
#' If a \code{provenance} element is present, then it will be expanded to include an entry
#' for this processing step.
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
#' @param top.pt The row number of the landmark that should be on top.
#' @param bottom.pt The row number of the landmark that should be on the bottom.
#' @param left.pt The row number of the landmark that should be on the left.
#' @param right.pt The row number of the landmark that should be on the right.
#' @param show.plot A logical factor or index number specifying whether to display a plot showing the alginment of one specimen relative to the reference.
#'     If \code{show.plot = TRUE} (the default), then a specimen is plotted at random.
#'     Alternatively, a specimen can be indicated as in \code{show.plot = 1}.
#'     To suppress the visual output, set If \code{show.plot = FALSE}.
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
#' curated.coords <- align.reflect(
#'   plethodon,
#'   top.pt = 8, right.pt = 7,
#'   links = pletho.links )
#'

align.reflect <- function(
  A,
  top.pt = NULL, bottom.pt = NULL, left.pt = NULL, right.pt = NULL,
  show.plot = TRUE, verbose = TRUE, provenance = NULL, ... )
{
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
        stop("Error: Input is not a recognized type. (See the help entry: `?align.reflect`.)")
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shapes <- A
    } else {
      stop("Error: Input is not a recognized type. (See the help entry: `?align.reflect`.)")
    }
  }
  if (dim(shapes)[2] != 2) {
    stop("Error: requires a matrix of X and Y corrdinates. (See the help entry: `?align.reflect`.)")
  }

  # If no landmark extremes are given, default to the marginal landmarks in specimen 1
  if (is.null(top.pt) & is.null(bottom.pt)) {
    top.pt <- which.max(shapes[,2,1])
    bottom.pt <- which.min(shapes[,2,1])
  } else {
    # If only one of the vertical landmarks is supplied, find another at the opposite extreme
    x <- order(shapes[,2,1])
    if (is.null(top.pt)) {
      top.pt <- ifelse( (shapes[bottom.pt,2,1] > median(shapes[,2,1])), x[length(x)], x[1] )
    } else {
      if (is.null(bottom.pt)) {
        bottom.pt <- ifelse( (shapes[top.pt,2,1] > median(shapes[,2,1])), x[1], x[length(x)] )
      }
    }
  }
  if (is.null(right.pt) & is.null(left.pt)) {
    left.pt <- which.min(shapes[,1,1])
    right.pt <- which.max(shapes[,1,1])
  } else {
    # If only one of the horizontal landmarks is supplied, find another at the opposite extreme
    x <- order(shapes[,1,1])
    if (is.null(right.pt)) {
      right.pt <- ifelse( (shapes[left.pt,1,1] > median(shapes[,1,1])), x[1], x[length(x)] )
    } else {
      if (is.null(left.pt)) {
        left.pt <- ifelse( (shapes[right.pt,1,1] > median(shapes[,1,1])), x[length(x)], x[1] )
      }
    }
  }

  # Determine the reference specimen
  if (is.logical(show.plot)) {
    reference.specimen <- sample(1:(dim(shapes)[3]),1)
  } else {
    if (is.numeric(show.plot)) {
      reference.specimen <- show.plot[[1]]
      show.plot <- TRUE
      if ((reference.specimen > dim(shapes)[3]) | (reference.specimen < 1)) {
        stop("Error: Specimen referenced using  show.plot  is invalid. See `?align.reflect` for usage.\n ")
      }
    }
  }

  landmark.number <- dim(shapes)[1]
  total.number.flipped <- 0
  report.string <- NULL
  for (i in 1:(dim(shapes)[3])) {
    OKright <- (shapes[right.pt,1,i] > shapes[left.pt,1,i])
    OKup    <- (shapes[top.pt,2,i] > shapes[bottom.pt,2,i])
    if (OKright & OKup) { if (verbose) { message(dimnames(shapes)[[3]][i],' ok\n') } }
    else { total.number.flipped <- total.number.flipped +1 }
    if (!OKright) { # If not, mirror the left side to the right.)
      mean.x <- mean(shapes[,1,i])
      centered.x <- shapes[,,i] - cbind(rep(mean.x,landmark.number),
                                            rep(0,landmark.number))
      centered.x <- centered.x * cbind(rep(-1,landmark.number),
                                       rep(1,landmark.number))
      shapes[,,i] <- centered.x + cbind(rep(mean.x,landmark.number),
                                            rep(0,landmark.number))
      report.string <- c(report.string, paste0(ifelse(is.null(dimnames(shapes)[[3]][i]), paste("specimen", i), dimnames(shapes)[[3]][i] ),' X-flipped\n') )
    }
    if (!OKup) { # If not, mirror the top to the bottom.)
      mean.y <- mean(shapes[,2,i])
      centered.y <- shapes[,,i] - cbind(rep(0,landmark.number),
                                            rep(mean.y,landmark.number))
      centered.y <- centered.y * cbind(rep(1,landmark.number),
                                       rep(-1,landmark.number))
      shapes[,,i] <- centered.y + cbind(rep(0,landmark.number),
                                            rep(mean.y,landmark.number))
      report.string <- c(report.string, paste0(ifelse(is.null(dimnames(shapes)[[3]][i]), paste("specimen", i), dimnames(shapes)[[3]][i] ),' Y-flipped\n') )
    }
  }
  if (verbose) {
    message(paste0(report.string, collapse = ""))
  }
  message(paste(total.number.flipped,"specimens reflected"))

  # Viz
  if (show.plot) {
    x <- ifelse(
      is.null(dimnames(shapes)[[3]][reference.specimen]),
      paste("specimen:", reference.specimen),
      dimnames(shapes)[[3]][reference.specimen]
    )
    landmark.plot(shapes, specimen.number = reference.specimen, ...)
  } # End  if (show.plot)

  # Prep the output
  output$coords <- shapes
  if (!is.null(provenance) & !any(grepl("provenance",names(output)))) {
      output$provenance <- provenance
  }
  s <- paste0(
    paste0("## Reflection alignment\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::align.reflect` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n"),
    paste0("- top.pt = ",top.pt,"\n"),
    paste0("- bottom.pt = ",bottom.pt,"\n"),
    paste0("- left.pt = ",left.pt,"\n"),
    paste0("- right.pt = ",right.pt,"\n"),
    paste0("\nSpecimens re-oriented: ",total.number.flipped,"\n\n")
  )

  if (!is.null(report.string)) {
    s <- paste0(s, "- ", paste0(report.string, collapse = "- "), "\n")
  }

  output$provenance$align.reflect <- s

  return(output)
} # End of function
