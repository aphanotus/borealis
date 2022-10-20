#' Examine the influence of each landmark in Procrustes alignment using jackknife re-sampling
#'
#' The function will remove each landmark in turn and conduct generalized Procrustes analysis, then
#' determine the distribution of pairwise Procrustes distances between all shapes.
#'
#' @param A A list or 3-dimensional array containing XY shape coordinates for multiple specimens.
#' @param curves An optional matrix defining which landmarks should be treated as semilandmarks by \code{gpagen}.
#' @param show.plot A logical argument specifying whether to display a plot
#'     of the median Procrustes distances, dropping each landmark.
#' @param links A matrix with two columns indicating landmarks to connect by lines.
#' @param verbose A logical argument specifying whether to display the function's progress.
#'
#' @return The median, 5% and 95% quantiles are reported as a data frame. Relatively lower values will be indicative
#'     of landmarks vary more than others. Exercise caution in the interpretation of these results.
#'     Variation may be biologically meaningful or may suggest systematic error in the placement of
#'     the landmark.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @export
#'
#' @examples
#'
#' procrustes.jackknife(Bombus.forewings)
#' procrustes.jackknife(Bombus.forewings, links = "chull")
#'
#'

procrustes.jackknife <- function (
  A,
  curves = NULL,
  show.plot = TRUE,
  links = NULL,
  verbose = TRUE,
  ...
)
{
  # Don't bother running anything if geomorph isn't installed!
  if (!require(geomorph)) {
    stop("Please run  install.packages('geomorph'). ")
  }

  shape.data <- NULL

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
        stop("Error: Input is not a recognized type. (See the help entry: `?procrustes.jackknife`.)")
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shape.data <- A
      if (is.null(provenance)) {
        warning("Warning: No data provenance provided.\n")
      } else {
        output$provenance <- provenance
      }
    } else {
      stop("Error: Input is not a recognized type. (See the help entry: `?align.procrustes`.)")
    }
  }

  if (show.plot) {
    if (verbose) { message("Determining reference GPA alignment") }
    GPA <- gpagen(shape.data, curves = curves, verbose = FALSE, print.progress = FALSE, ...)

    allowed.links.terms <- c("chull","ordinal")
    if (!is.null(links)) {
      links <- as.matrix(links)
      if (((max(links) > dim(shape.data)[1]) | (min(links) < 1)) & !(links[[1]] %in% allowed.links.terms))  {
        warning("Warning: Provided links are out of bounds. (See the help entry: `?procrustes.jackknife`.)")
        links <- NULL
      }
      if (links[[1]] == "chull") {
        links <- grDevices::chull(GPA$consensus)
        links <- matrix(c(links,links[-1],links[1]), ncol=2, byrow = FALSE)
      } else {
        if (links[[1]] == "ordinal") {
          links <- 1:(dim(shape.data)[1])
          links <- matrix(c(links,links[-1],links[1]), ncol=2, byrow = FALSE)
          links <- links[-c(dim(shape.data)[1]),]
        } else {
          if (dim(links)[2] != 2) {
            warning("Warning: Links must be a matrix with two columns of landmark numbers. (See the help entry: `?procrustes.jackknife`.)")
            links <- NULL
          }
        }
      }
      # Get the XY coordinates from GPA consensus shape for the linked landmarks
      links.xy <- t(apply(links, 1, function(x) { c(GPA$consensus[x,1], GPA$consensus[x,2]) })) # x1, x2, y1, y2
      colnames(links.xy) <- c("x","xend","y","yend")
      links.xy <- as.data.frame(links.xy)
    } # if (!is.null(links))

  } # end if (show.plot)

  # Function to find pairwise Procrustes distances among all shapes
  pairwise.procD <- function(A) {
    specimen.number <- dim(A)[3]
    pairwise.PD <- vector(mode = "numeric")
    for (i in 1:(specimen.number-1)) {
      for (j in (i+1):specimen.number) {
        x <- procrustes.distance(A[,,i], A[,,j])
        pairwise.PD <- c(pairwise.PD, x)
      }
    }
    return(pairwise.PD)
  } # End function pairwise.procD

  # Re-sampling with landmark jackknifes
  procD.jk.median <- vector(mode = "numeric")
  procD.jk.q05 <- vector(mode = "numeric")
  procD.jk.q95 <- vector(mode = "numeric")
  for (i in 1:(dim(shape.data)[1])) {
    if (verbose) { message(paste("Repeating GPA and determining Procrustes distances, dropping landmark",i)) }
    shapes.i <- shape.data[-i,,]
    curves.i <- curves
    if (!is.null(curves.i)) { curves.i <- curves.i[-which(curves.i[,2] == i),] }
    gpa.i <- gpagen(shapes.i, curves = curves.i, verbose = FALSE, print.progress = FALSE, ...)
    # gpa.i <- gpagen(shapes.i, curves = curves.i, verbose = FALSE, print.progress = FALSE)
    procD.i <- pairwise.procD(gpa.i$coords)
    procD.jk.median <- c(procD.jk.median, median(procD.i))
    procD.jk.q05 <- c(procD.jk.q05, quantile(procD.i,0.05))
    procD.jk.q95 <- c(procD.jk.q95, quantile(procD.i,0.95))
  }

  output <- data.frame(
    landmark = 1:(dim(shape.data)[1]),
    procD.jk.median = unname(procD.jk.median),
    procD.jk.q05 = unname(procD.jk.q05),
    procD.jk.q95 = unname(procD.jk.q95)
  )

  if (show.plot) {

    df <- as.data.frame(GPA$consensus)
    df$jk50 <- procD.jk.median
    df$jk05 <- procD.jk.q05
    df$jk95 <- procD.jk.q95

    plot.jk <- ggplot(df, aes(x=X, y=Y, color=jk50)) +
      theme_classic()

    if (!is.null(links)) {
      plot.jk <- plot.jk +
        geom_segment(data = links.xy, aes(x = x, y = y, xend = xend, yend = yend), color = "darkgray")
    }

    plot.jk <- plot.jk +
      geom_point(size=10) +
      scale_color_viridis("ProcD JK", option = "magma") +
      geom_text(aes(label=signif(jk50,3)), color = "gray90", size = 2) +
      coord_fixed()

    print(plot.jk)
  } # if (show.plot)

  return(output)

} # end function procrustes.jackknife

