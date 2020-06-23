#' Align a subset of landmarks by rotation from one point
#'
#' Joints can introduce nuisance variation in landmark-based geometric morphometrics.
#' This function rotates a subset of landmarks about a pivot point, such that all specimens
#' will align to a designated reference specimen.
#' This step should be run before Procrustes alignment, and it is robust to differences
#' in the relative position, orientation and size of specimens.
#'
#' Alignment is based on linear regression of X and Y positions for landmarks in a
#' substructure (such as a limb or jaw) and in the rest of the specimen.
#' The angular difference in slopes is used to rotate substructure points relative to the
#' designated pivot point.
#'
#' The input array should contain landmark coordinates with \emph{p}, 2, and \emph{n} dimensions,
#' where \emph{n} is the number of specimens and \emph{p} is the number of landmarks).
#' Dimension 2 must have two columns that contain X and Y locations of the landmark.
#'
#' If \code{include.plot = TRUE}, then landmarks from each specimen will be displayed.
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
#' @return Returns a list with \code{coords}, \code{provenance}, and
#'    any other potential list elements from the input.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param A A 3-dimensional array containing XY shape corrdinates for multiple specimens,
#'     or a list containing such as an array and data provenance.
#' @param substructure.LMs A numerical vector giving the landmark numbers of the substructure to be algined.
#' @param pivot.LM The number of the landmark that should be used as the joint.
#' @param reference.specimen The index number of the reference specimen.
#' @param include.plot A logical factor specifying whether to include a plot showing the alginment of each specimen relative to the reference.
#' @param provenance An object that should be retained for data provenance.
#'
#' @export
#'
#' @examples
#' data(plethodon, package = "geomorph")
#'
#' pletho.links <- matrix(c(4,5,5,6,6,7,7,8,8,9,9,10,10,11,2,4,12,2,3,5),
#'                        ncol = 2, byrow = TRUE)
#' landmark.plot(plethodon$land, links = pletho.links)
#'
#' # Allow alignment of the jaw landmarks, pivoting at the jaw joint
#' plethodon <- align.joint(
#'   plethodon,
#'   substructure.LMs = c(1,2,3,4,5),
#'   pivot.LM = 1,
#'   include.plot = TRUE,
#'   links = pletho.links
#' )
#'
#' names(plethodon)
#' names(plethodon$provenance)
#' cat(plethodon$provenance$align.joint)
#'

align.joint <- function(
  A,
  substructure.LMs = NULL,
  pivot.LM = NULL,
  reference.specimen = 1,
  include.plot = TRUE,
  provenance = NULL,
  ...
)
{ # Begin the function

  # Initialize
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
        return(cat("Error: Input is not a recognized type. (See the help entry: '?align.joint'.)"))
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shape.data <- A
    } else {
      return(cat("Error: Input is not a recognized type. (See the help entry: '?align.joint'.)"))
    }
  }
  if (dim(shape.data)[2] != 2) {
    return(cat("Error: requires a matrix of X and Y corrdinates."))
  }
  if (is.null(substructure.LMs) | !is.numeric(substructure.LMs) | (max(substructure.LMs) > dim(shape.data)[1]) | (min(substructure.LMs) < 0)) {
    return(cat("Error: substructure.LMs is invalid. See '?align.joint' for usage.\n "))
  }
  if (is.null(pivot.LM) | !is.numeric(pivot.LM) | (pivot.LM > dim(shape.data)[1]) | (pivot.LM < 0)) {
    return(cat("Error: pivot.LM is invalid. See '?align.joint' for usage.\n "))
  }

  # Identify the main structure landmarks
  main.structure.LMs <- 1:dim(shape.data)[1]
  main.structure.LMs <- main.structure.LMs[which(!(main.structure.LMs %in% substructure.LMs))]
  if (!(pivot.LM %in% substructure.LMs)) {
    substructure.LMs <- sort(c(substructure.LMs, pivot.LM))
  }
  main.structure.LMs <- sort(c(main.structure.LMs, pivot.LM))

  # Trigonometry reminders:
  #   For positive slopes, inverse tangent values range for zero (flat slope) to pi/2.
  #   They can also be negative (for negative slopes).
  #   Positive radian values indicate a relative clockwise angle.
  #   To convert to degrees multiply radians by (180/pi).

  # A function to comapre the relative angles of two shapes
  # Returns the angle in randians, based on the difference in inverse tangents of the regression slopes
  relative.angles <- function(a1, a2) {
    angle1 <- atan(as.vector(lm(a1[,2] ~ a1[,1])$coefficients[2]))
    angle2 <- atan(as.vector(lm(a2[,2] ~ a2[,1])$coefficients[2]))
    return(angle1-angle2)
  }

  ref.joint.angle <- relative.angles(shape.data[main.structure.LMs,,reference.specimen],
                                     shape.data[substructure.LMs,,reference.specimen])

  # MAIN LOOP
  # For each specimen,
  # find out how much the angle of the substructure deviates from the reference angle,
  # then adjust the substructure accordingly
  specimens.to.adjust <- 1:(dim(shape.data)[3])
  specimens.to.adjust <- specimens.to.adjust[which(specimens.to.adjust != reference.specimen)]
  adjustment.angles <- NULL
  for (i in specimens.to.adjust) {
    old.coords.i <- shape.data[,,i]
    angle.i <- relative.angles(shape.data[main.structure.LMs,,i],
                               shape.data[substructure.LMs,,i])
    theta <- angle.i - ref.joint.angle
    adjustment.angles <- c(adjustment.angles, theta)

    # Trigonometry reminder:
    # Equations for rotating a point by an angle, counter-clockwise, relative to the x-axis
    # x′ = x*cosθ − y*sinθ
    # y′ = y*cosθ + x*sinθ

    # The location of the pivot point for specimen i
    x0 <- shape.data[pivot.LM,1,i]
    y0 <- shape.data[pivot.LM,2,i]

    # Loop for each substructure landmark
    for (j in substructure.LMs[which((substructure.LMs != pivot.LM))])
    {
      xj <- shape.data[j,1,i]
      yj <- shape.data[j,2,i]
      # cat(j,":\t",xj,yj," --> ")

      # Translate the landmark, such that the pivot point becomes the origin
      xj <- xj - x0
      yj <- yj - y0

      # Rotate the landmark
      x.adj <- xj * cos(theta) - yj * sin(theta)
      y.adj <- yj * cos(theta) + xj * sin(theta)

      # Add back the translation values
      x.adj <- x.adj + x0
      y.adj <- y.adj + y0

      # Update shape.data
      shape.data[j,1,i] <- x.adj
      shape.data[j,2,i] <- y.adj

    } # End Loop for each substructure landmark

    if (include.plot) {
      x <- ifelse(
        is.null(dimnames(shape.data)[[3]][i]),
        paste("specimen", i),
        dimnames(shape.data)[[3]][i]
      )
      # Base plot and landmark numbers based on the new positions
      landmark.plot(shape.data[,,i], main = x, text.color = "darkgray", ...)

      # Main structure landmarks
      points(shape.data[main.structure.LMs,1,i], shape.data[main.structure.LMs,2,i], pch = 16, col = "grey50")

      # Old substructure positions
      points(old.coords.i[substructure.LMs,1], old.coords.i[substructure.LMs,2], pch = 1, col = "darkred" )

      # New substructure positions
      points(shape.data[substructure.LMs,1,i], shape.data[substructure.LMs,2,i], pch = 16, col = "black")

      # Pivot
      points(shape.data[pivot.LM,1,i], shape.data[pivot.LM,2,i], pch = 16, col = "darkred")

      if (i != specimens.to.adjust[length(specimens.to.adjust)]) {
        x <- readline("Show next specimen? (ENTER for more. -- Joint alignment will continue for all specimens.)")
        include.plot <- (tolower(x) %in% c("","y","yes","ya","yep"))
      }
    } # End  if (include.plot)

  } # End loop for each specimen

  # Prep the output
  output$coords <- shape.data
  if (!is.null(provenance) & !any(grepl("provenance",names(output)))) {
    output$provenance <- provenance
  }

  adjustment.angles <- round(adjustment.angles*(180/pi),1)
  s <- paste0(
    paste0("## Joint alignment\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::align.joint` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n"),
    paste0("- main structure landmarks: ",paste0(main.structure.LMs, collapse = ", "),"\n"),
    paste0("- substructure landmarks: ",paste0(substructure.LMs, collapse = ", "),"\n"),
    paste0("- pivot point: ",pivot.LM,"\n\n"),
    paste0("Adjustment angles ranged from ",min(adjustment.angles),"˚ to ",max(adjustment.angles),"˚ "),
    paste0("with a mean of ",round(mean(adjustment.angles),1),"˚, relative to the reference specimen, index number ", reference.specimen),
    ifelse(
      is.null(dimnames(shape.data)[[3]][reference.specimen]),
      ".\n\n",
      paste0(": ", dimnames(shape.data)[[3]][reference.specimen],".\n\n")
    )
  )

  output$provenance$align.joint <- s

  return(output)

} # End of function
