#' Plot the relative position of landmarks
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param A A 2D matrix of X and Y shape coordinates,
#'     a 3-dimensional array containing XY coordinates for multiple specimens, or
#'     a list containing such as an array.
#' @param specimen.number If an array is provided, the specimen number(s) to plot.
#' @param square A logical factor specifying whether the aspect ratio of the plot should be equal.
#' @param axes A logical factor specifying whether to include x and y axes.
#' @param def.grids A logical factor specifying whether to plot deformation grids (thin plate splines) using \code{geomorph::plotRefToTarget}.
#'     If so, the function uses \code{method = "TPS"} and the reference is set to the censusus shape for the entire the coordinate array.
#' @param landmark.numbers A logical factor specifying whether landmarks should appear as numbers (if TRUE) or as dots.
#' @param links A matrix with two columns indicating landmarks to connect by lines.
#'     Alternatively, enter \code{"chull"} to draw a convex hull
#'     or \code{"ordinal"} to link landmarks in numerical order.
#' @param panels A vector with exactly two integers specifying the number of rows and columns of specimens to plot.
#'     If the number of panels exceeds the number of values entered in \code{specimen.number} then the next consecutive specimens will be shown,
#'     starting from specimen 1 if no value is provided for \code{specimen.number}.
#' @param text.color Color names or value for the text.
#' @param line.color Color names or value for lines.
#'
#' @export
#'
#' @examples
#' data(plethodon, package = "geomorph")
#'
#' # The function will detect whether the input object is
#' # a 2D set of coordinates or a 3D array
#' # The following are all equivalant
#' landmark.plot(plethodon)
#' landmark.plot(plethodon$land, specimen.number = 1)
#' landmark.plot(plethodon$land[,,1])
#'
#' # Convex hull
#' landmark.plot(plethodon$land, links = "chull")
#'
#' # Custom landmark connections
#' pletho.links <- matrix(c(4,5,5,6,6,7,7,8,8,9,9,10,10,11,2,4,12,2,3,5),
#'                        ncol = 2, byrow = TRUE)
#' landmark.plot(plethodon$land, links = pletho.links )
#'
#' # Multiple panels
#' landmark.plot(plethodon$land, links = pletho.links, specimen.number = 1:4 )
#' landmark.plot(plethodon$land, links = pletho.links, panels = c(3,2) )
#'

landmark.plot <- function (A,
                           specimen.number = NULL,
                           square = TRUE,
                           axes = FALSE,
                           def.grids = FALSE,
                           landmark.numbers = TRUE,
                           links = NULL,
                           panels = c(1,1),
                           text.color = "darkred",
                           line.color = "darkgray",
                           ...)
{
  no.specimen.numbers.given <- TRUE
  if (is.null(specimen.number)) { specimen.number <- 1 }
  else { no.specimen.numbers.given <- FALSE }

  # Fail safes
  if (!is.numeric(specimen.number)) {
    stop("Error: specimen.number must be a vector of whole numbers. (See the help entry: `?landmark.plot`.)")
    if (specimen.number%%1!=0) {
      stop("Error: specimen.number must be a vector of whole numbers. (See the help entry: `?landmark.plot`.)")
    }
  }

  # Check panels input
  if (length(panels)!=2) {
    panels <- c(1,1)
    warning("Warning: panels requires two integers values for the number of rows and columns to display. (See the help entry: `?landmark.plot`.)")
  }

  # Does specimen.number length match panels?
  if (no.specimen.numbers.given & (prod(panels) > length(specimen.number))) {
    x <- prod(panels) - length(specimen.number)
    specimen.number <- c(specimen.number, (c(1:x) + specimen.number[length(specimen.number)]))
  } else {
    if (prod(panels) < length(specimen.number)) {
      x <- ceiling(sqrt(length(specimen.number)))
      panels <- c(x,x)
      if (prod(panels) - length(specimen.number) >= x) { panels[2] <- panels[2]-1 }
    }
  }

  # Vet the shape data
  if (class(A)[1] %in% c("gpagen","list","geomorph.data.frame")) {
    if (any(names(A)=="land")) {
      specimen.number <- specimen.number[which(specimen.number <= dim(A$land)[3])]
      landmarks <- A$land[,,specimen.number]
      if (def.grids) { consensus <- mshape(A$land) }
      if (length(specimen.number)>1) { x <- dimnames(A$land[,,specimen.number])[[3]] }
      else { x <- dimnames(A$land)[[3]][specimen.number] }
      if (is.null(x)) { main.title <- specimen.number }
      else { main.title <- x }
    } else {
      if (any(names(A)=="gpagen")) {
        specimen.number <- specimen.number[which(specimen.number <= dim(A$gpagen$coords)[3])]
        landmarks <- A$gpagen$coords[,,specimen.number]
        if (def.grids) { consensus <- mshape(A$gpagen$coords) }
        if (length(specimen.number)>1) { x <- dimnames(A$gpagen$coords[,,specimen.number])[[3]] }
        else { x <- dimnames(A$gpagen$coords)[[3]][specimen.number] }
        if (is.null(x)) { main.title <- specimen.number }
        else { main.title <- x }
      } else {
        if (any(names(A)=="gdf")) {
          specimen.number <- specimen.number[which(specimen.number <= dim(A$gdf$coords)[3])]
          landmarks <- A$gdf$coords[,,specimen.number]
          if (def.grids) { consensus <- mshape(A$gdf$coords) }
          if (length(specimen.number)>1) { x <- dimnames(A$gdf$coords[,,specimen.number])[[3]] }
          else { x <- dimnames(A$gdf$coords)[[3]][specimen.number] }
          if (is.null(x)) { main.title <- specimen.number }
          else { main.title <- x }
        } else {
          if (any(names(A)=="coords")) {
            if (!is.null(dim(A$coords))) {
              specimen.number <- specimen.number[which(specimen.number <= dim(A$coords)[3])]
              landmarks <- A$coords[,,specimen.number]
              if (def.grids) { consensus <- mshape(A$coords) }
              if (length(specimen.number)>1) { x <- dimnames(A$coords[,,specimen.number])[[3]] }
              else { x <- dimnames(A$coords)[[3]][specimen.number] }
              if (is.null(x)) { main.title <- specimen.number }
              else { main.title <- x }
            }
          } else {
            stop("Input is not a recognized type. (See the help entry: `?landmark.plot`.)")
          }
        }
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      specimen.number <- specimen.number[which(specimen.number <= dim(A)[3])]
      landmarks <- A[,,specimen.number]
      if (def.grids) { consensus <- mshape(A) }
      if (length(specimen.number)>1) { x <- dimnames(A[,,specimen.number])[[3]] }
      else { x <- dimnames(A)[[3]][specimen.number] }
      if (is.null(x)) { main.title <- specimen.number }
      else { main.title <- x }
    } else {
      if (any(class(A) == "matrix") & (dim(A)[2] == 2)) {
        landmarks <- A
        if (def.grids) { consensus <- A }
        main.title <- ''
      } else {
        stop("Error: Input is not a recognized type. (See the help entry: `?landmark.plot`.)")
      }
    }
  }

  # Make sure even if there's just one specimen, landmarks is an array
  if (any(class(A) == "matrix")) {
    landmarks <- array(rep(unlist(landmarks),2), dim = c(dim(landmarks),2))
  }

  # More fail safes
  if (dim(landmarks)[2] != 2) {
    stop("Error: Requires a matrix of X and Y corrdinates. (See the help entry: `?landmark.plot`.)")
  }
  if (any(is.na(landmarks))) {
    stop("Error: Coordinate data contains NA values. (See the help entry: `?landmark.plot`.)")
  }
  allowed.links.terms <- c("chull","ordinal")
  if (!is.null(links)) {
    if (((max(links) > dim(landmarks)[1]) | (min(links) < 1)) & !(links[[1]] %in% allowed.links.terms))  {
      warning("Warning: Provided links are out of bounds. (See the help entry: `?landmark.plot`.)")
      links <- NULL
    }
  }

  # Setup panels
  par(mfrow=panels)

  # Main loop
  for (i in 1:length(specimen.number)) {

    if (def.grids) {
      if (!is.null(links)) {
        if (links[[1]] == "chull") {
          links <- grDevices::chull(landmarks[,,i])
          links <- matrix(c(links,links[-1],links[1]), ncol=2, byrow = FALSE)
        } else {
          if (links[[1]] == "ordinal") {
            links <- 1:(dim(landmarks)[1])
            links <- matrix(c(links,links[-1],links[1]), ncol=2, byrow = FALSE)
            links <- links[-c(dim(landmarks)[1]),]
          } else {
            if (dim(links)[2] != 2) {
              warning("Warning: Links must be a matrix with two columns of landmark numbers. (See the help entry: `?landmark.plot`.)")
              links <- NULL
            }
          }
        }
      } # End  if (!is.null(links))

      GP <- gridPar(
        txt.col = text.color,
        tar.link.col = line.color,
      )

      plotRefToTarget(
        M1 = consensus,
        M2 = landmarks[,,i],
        method = "TPS",
        links = links,
        axes = axes,
        label = landmark.numbers,
        gridPars = GP,
        ...
      )
      title(main = main.title[i])

    } # End  if (def.grids)
    else {
      # Base R Plot
      if (axes) {
        xlab <- 'x'
        ylab <- 'y'
      } else {
        xlab <- ''
        ylab <- ''
      }
      plot(landmarks[,,i], type='n', axes = axes, asp = square, xlab = xlab, ylab = ylab, main = main.title[i], ...)
      if (!is.null(links)) {
        if (links[[1]] == "chull") {
          links <- grDevices::chull(landmarks[,,i])
          links <- matrix(c(links,links[-1],links[1]), ncol=2, byrow = FALSE)
          for (j in 1:(dim(links)[1])) {
            segments(landmarks[links[j,1],1,i], landmarks[links[j,1],2,i], landmarks[links[j,2],1,i], landmarks[links[j,2],2,i], col = line.color )
          }
        } else {
          if (links[[1]] == "ordinal") {
            links <- 1:(dim(landmarks)[1])
            links <- matrix(c(links,links[-1],links[1]), ncol=2, byrow = FALSE)
            links <- links[-c(dim(landmarks)[1]),]
            for (j in 1:(dim(links)[1])) {
              segments(landmarks[links[j,1],1,i], landmarks[links[j,1],2,i], landmarks[links[j,2],1,i], landmarks[links[j,2],2,i], col = line.color )
            }
          } else {
            if (dim(links)[2] != 2) {
              warning("Warning: Links must be a matrix with two columns of landmark numbers. (See the help entry: `?landmark.plot`.)")
            } else {
              for (j in 1:(dim(links)[1])) {
                segments(landmarks[links[j,1],1,i], landmarks[links[j,1],2,i], landmarks[links[j,2],1,i], landmarks[links[j,2],2,i], col = line.color )
              }
            }
          }
        }
      } # End  if (!is.null(links))

      # Landmark labels
      if (landmark.numbers) {
        for (j in 1:(dim(landmarks)[1])) { text(landmarks[j,1,i], landmarks[j,2,i], labels=j, col = text.color) }
      } else {
        points(landmarks[,,i])
      }
    } # End  else / if (def.grids)

  } # End for loop

  # Return plot to 1x1
  par(mfrow=c(1,1))

} # End of function
