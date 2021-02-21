#' Align landmarks across a line of symmetry
#'
#' The input array should contain landmark coordinates with \emph{p}, 2, and \emph{n} dimensions,
#' where \emph{n} is the number of specimens and \emph{p} is the number of landmarks).
#' Dimension 2 must have two columns that contain X and Y locations of the landmark.
#'
#' @param A A 3-dimensional array containing XY shape coordinates for multiple specimens, or a list containing such as an array and data provenance.
#' @param pt1 A number or numeric vector of landmarks specifying one of two points to define the line of symmetry.
#' @param pt2 A number or numeric vector of landmarks specifying the second of two points to define the line of symmetry.
#' @param landmark.pairs A matrix with two columns. Rows pair landmarks. Landmarks in the second column
#'     will be reflected across the line of symmetry and averaged with the paired landmark. Landmark
#'     numbers in the first column will be retained.
#' @param show.plot A logical argument specifying whether to display a plot
#'     comparing the distributions of variance in landmark distances,
#'     corrected for centroid size, before and after alignment to the fixed angle.
#' @param verbose A logical argument specifying whether to display summary statistics
#'     on the degree of asymmetry in the specimens.
#'
#' @return Returns a list with \code{coords}, \code{provenance}, and
#'    any other potential list elements from the input.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#'

align.bilat <- function (
  A,
  pt1,
  pt2,
  landmark.pairs,
  show.plot = TRUE,
  verbose = TRUE
)
{ # Begin the function

  # Initialize
  shapes <- NULL
  output <- NULL

  # Vet the input
  if (missing(A)) {
    stop("Error: Input is not a recognized type. (See the help entry: `?align.bilat`.)\n")
  }
  if (class(A)[1] %in% c("gpagen","list")) {
    if (any(grepl("coords",names(A)))) {
      shapes <- A$coords
      output <- A[-grep("coords",names(A))]
    } else {
      if (any(grepl("land",names(A)))) {
        shapes <- A$land
        output <- A[-grep("land",names(A))]
      } else {
        stop("Error: Input is not a recognized type. (See the help entry: `?align.bilat`.)\n")
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shapes <- A
    } else {
      stop("Error: Input is not a recognized type. (See the help entry: `?align.bilat`.)\n")
    }
  }
  if (dim(shapes)[2] != 2) {
    stop("Error: requires a matrix of X and Y corrdinates. (See the help entry: `?align.bilat`.)\n")
  }
  if (missing(pt1) | !is.numeric(pt1) | (max(pt1) > dim(shapes)[1]) | (min(pt1) < 1)) {
    stop("Error: `pt1` is invalid. See `?align.bilat` for usage.\n")
  }
  if (missing(pt2) | !is.numeric(pt2) | (max(pt2) > dim(shapes)[1]) | (min(pt2) < 1)) {
    stop("Error: `pt2` is invalid. See `?align.bilat` for usage.\n")
  }
  if (missing(landmark.pairs) | !is.numeric(landmark.pairs) | (max(landmark.pairs) > dim(shapes)[1]) | (min(landmark.pairs) < 1)) {
    stop("Error: `landmark.pairs` is invalid. See `?align.bilat` for usage.\n")
  }
  # End preliminary vetting of the input and arguments

  # Collect distances between the paired landmarks after reflection
  # across the line of symmetry
  distance.matrix <- data.frame(matrix(ncol = dim(landmark.pairs)[1],
                                       nrow = dim(shapes)[3]))

  # #########################################
  # MAIN LOOP
  # #########################################
  for (i in 1:(dim(shapes)[3])) {
    if (show.plot) { landmark.plot(shapes, specimen.number = i) }
    # If needed, find centroid points defining the line of symmetry
    if (length(pt1)==1) { p1 <- shapes[pt1,,i] } else { p1 <- apply(shapes[pt1,,i], 2, mean) }
    if (length(pt2)==1) { p1 <- shapes[pt2,,i] } else { p2 <- apply(shapes[pt2,,i], 2, mean) }
    if (show.plot) { points(p1[1],p1[2]); points(p2[1],p2[2]) }
    # Find the line of symmetry
    m <- lm(c(p1[2],p2[2])~c(p1[1],p2[1]))
    if (show.plot) { abline(m) }
    intercept <- m$coefficients[1]
    slope <- m$coefficients[2]
    # Reflect the landmarks in the second column of landmark.pairs
    for (j in landmark.pairs[,2]) {
      d <- (shapes[j,1,i] + (shapes[j,2,i] - intercept)*slope)/(1 + slope^2)
      # x4 = 2*d - x1
      # y4 = 2*d*m - y1 + 2*c
      shapes[j,1,i] <- 2*d - shapes[j,1,i]
      shapes[j,2,i] <- 2*d*slope - shapes[j,2,i] + 2*intercept
      if (show.plot) { text(shapes[j,1,i],shapes[j,2,i], j, col="gray35") }
    }
    # Average the paired landmarks, over-writing the landmarks in column 1
    for (j in 1:(dim(landmark.pairs)[1])) {
      # Collect distances
      distance.matrix[i,j] <- borealis::distance(shapes[landmark.pairs[j,1],,i],shapes[landmark.pairs[j,2],,i])
      # Find the averages
      shapes[landmark.pairs[j,1],1,i] <- mean(shapes[landmark.pairs[j,],1,i])
      shapes[landmark.pairs[j,1],2,i] <- mean(shapes[landmark.pairs[j,],2,i])
      if (show.plot) { text(shapes[j,1,i],shapes[j,2,i], landmark.pairs[j,1] , font = 2) }
    }
    show.plot <- FALSE
  } # End of MAIN LOOP

  # Remove the reflected landmarks
  shapes <- shapes[-landmark.pairs[,2],,]
  output$landmark.number <- dim(shapes)[1]

  # Prep the output
  output$coords <- shapes
  if (!any(grepl("provenance",names(output)))) {
    output$provenance <- list()
  }

  landmark.pairs.txt <- apply(landmark.pairs,1, function(x){paste0(x,collapse = "-")})

  # Identify extremely asymmetrical specimens
  spec.asym <- apply(distance.matrix,1,sum)
  outliers <- which(spec.asym > summary(spec.asym)[5]+1.5*IQR(spec.asym))
  if (length(outliers) > 0) {
    outliers <- outliers[order(spec.asym[outliers], decreasing = TRUE)]
    s <- dimnames(shapes)[3][[1]]
    outlier.txt <- paste0(
      "Outlier specimens with greater than 1.5 IQR more than the 3rd quartile\n- ",
      paste0(s[outliers],collapse = "\n- "),"\n"
    )
    s[outliers]
  } else {
    outlier.txt <- NULL
  }

  if (verbose) {
    cat("Median discrepencies between symetrical landmarks\n")
    cat(paste0(" - landmarks ",landmark.pairs.txt,": ", signif(apply(distance.matrix,2,median),6), collapse = "\n"),
        paste0("\n\n") )
    cat("Summary of the sum of landmark discrepencies for all specimens\n")
    print(summary(spec.asym))
    cat("\n")
    cat(outlier.txt)
  }

  if (!is.null(outlier.txt)) {
    outlier.txt <- paste0("### ",outlier.txt,"\n")
  } else {
    outlier.txt <- ""
  }

  spec.asym <- summary(spec.asym)

  s <- paste0(
    paste0("## Bilateral alignment\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::align.bilat` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n"),
    paste0("- pt1: ",paste0(pt1, collapse = ", "),"\n"),
    paste0("- pt2: ",paste0(pt2, collapse = ", "),"\n"),
    paste0("- landmark pairs: ",paste0(landmark.pairs.txt, collapse = ", "),"\n"),
    paste0("### Median discrepencies between symetrical landmarks\n"),
    paste0("- landmarks ",landmark.pairs.txt,": ", signif(apply(distance.matrix,2,median),6), collapse = "\n"),
    "\n\n",
    paste0("### Summary of the sum of landmark discrepencies for all specimens\n"),
    paste0("- ", names(spec.asym),": ",signif(spec.asym,6), collape = "\n"),
    "\n",
    paste0(outlier.txt)
  )

  output$provenance$align.bilat <- s

  return(output)

} # End of function
