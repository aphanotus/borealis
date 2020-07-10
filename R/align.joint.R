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
#' @param barrier.LM The number of a landmark that acts as a barrier to rotation.
#' @param reference.specimen The index number of the reference specimen.
#' @param rotation.limits A vector of two numbers specifying the rotation.limits of rotations to test.
#' @param tolerance A value in radians, below which, the function does not adjust the substructure
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
#'   substructure.LMs = c(4,5),
#'   main.structure.LMs = c(6,7,8),
#'   pivot.LM = 3,
#'   barrier.LM = 6,
#'   reference.specimen = 5,
#'   rotation.limits = c(-20,10)/(180/pi),
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
  main.structure.LMs = NULL,
  pivot.LM = NULL,
  barrier.LM = NULL,
  reference.specimen = 1,
  rotation.limits = c(-90,90)/(180/pi),
  tolerance = 0,
  include.plot = TRUE,
  provenance = NULL,
  ...
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
        stop("Error: Input is not a recognized type. (See the help entry: '?align.joint'.)")
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shapes <- A
    } else {
      stop("Error: Input is not a recognized type. (See the help entry: '?align.joint'.)")
    }
  }
  if (dim(shapes)[2] != 2) {
    stop("Error: requires a matrix of X and Y corrdinates.")
  }
  if (is.null(substructure.LMs) | !is.numeric(substructure.LMs) | (max(substructure.LMs) > dim(shapes)[1]) | (min(substructure.LMs) < 0)) {
    stop("Error: substructure.LMs is invalid. See '?align.joint' for usage.\n ")
  }
  if (is.null(pivot.LM) | !is.numeric(pivot.LM) | (pivot.LM > dim(shapes)[1]) | (pivot.LM < 0)) {
    stop("Error: pivot.LM is invalid. See '?align.joint' for usage.\n ")
  }
  if (is.null(rotation.limits) | length(rotation.limits)!=2 | any(rotation.limits > 2*pi) | any(rotation.limits < -2*pi)) {
    stop("Error: rotation.limits is invalid. See '?align.joint' for usage.\n ")
  }
  rotation.limits <- sort(rotation.limits)

  # Vet the substructure.LMs
  substructure.LMs <- sort(unique(c(substructure.LMs)))
  if (pivot.LM %in% substructure.LMs) {
    substructure.LMs <- substructure.LMs[-which(substructure.LMs == pivot.LM)]
  }

  # Identify the main.structure.LMs
  if (is.null(main.structure.LMs)) {
    main.structure.LMs <- 1:dim(shapes)[1]
    main.structure.LMs <- main.structure.LMs[which(!(main.structure.LMs %in% substructure.LMs))]
    main.structure.LMs <- main.structure.LMs[-which(main.structure.LMs == pivot.LM)]
  } else {
    main.structure.LMs <- as.numeric(main.structure.LMs)
    if ((max(main.structure.LMs) > dim(shapes)[1]) | (min(main.structure.LMs) < 0)) {
      stop("Error: main.structure.LMs is invalid. See '?align.joint' for usage.\n ")
    }
    if (any(main.structure.LMs == pivot.LM)) {
      main.structure.LMs <- main.structure.LMs[-which(main.structure.LMs == pivot.LM)]
    }
  }

  if (require("geomorph")) {
    gpa1.procD <- sum(gpagen(shapes, print.progress = FALSE)$procD)
  } else { gpa1.procD <- NULL }

  # Sub-functions

  # Calculate a matrix of distances b/w substructure LMs and main LMs for reference.
  distance <- function(xy,XY) { sqrt((xy[1]-XY[1])^2+(xy[2]-XY[2])^2) }
  distMatrix <- function(m, lm1, lm2) {
    df <- data.frame()
    for (i in 1:length(lm1)) {
      for (j in 1:length(lm2)) {
        df[i,j] <- distance(m[lm1[i],], m[lm2[j],])
      }
    }
    dimnames(df) <- list(
      paste0("lm",lm1),
      paste0("lm",lm2)
    )
    return((df))
  } # End dMatrix function

  # A function to rotate substructure.LMs within a matrix (one specimen)
  rotate.substructure <- function (m, LMs.to.rotate, pivot, theta ) {

    # Trigonometry reminder:
    # Equations for rotating a point by an angle, counter-clockwise, relative to the x-axis
    # x′ = x*cosθ − y*sinθ
    # y′ = y*cosθ + x*sinθ

    # The location of the pivot point for specimen i
    x0 <- m[pivot,1]
    y0 <- m[pivot,2]

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

  # A function to rotate substructure.LMs and report the mean absolute differences in scaled distance matrices
  rotate.substructure.diff <- function (m, LMs.to.rotate, pivot, other.LMs, ref.dist.matrix, theta ) {
    m.prime <- rotate.substructure(m, LMs.to.rotate, pivot, theta)

    diff <- mean(abs(unlist(scale(ref.distances, center = F) - scale(distMatrix(m.prime, other.LMs, LMs.to.rotate), center = F)))) # mean of absolute differences
    # diff <- sum(unlist(ref.distances - distMatrix(m.prime, other.LMs, LMs.to.rotate))^2) # sum of squared differences
    return(diff)
  }

  # End sub-function definitions

  if (include.plot) {
    i <- reference.specimen
    x <- ifelse(
      is.null(dimnames(shapes)[[3]][i]),
      paste("reference specimen:",i),
      paste("reference specimen:",dimnames(shapes)[[3]][i])
    )
    landmark.plot(shapes[,,i], main = x, ...)

    # Main structure landmarks
    points(shapes[main.structure.LMs,1,i], shapes[main.structure.LMs,2,i], pch = 16, col = "grey50")

    # Substructure positions
    points(shapes[substructure.LMs,1,i], shapes[substructure.LMs,2,i], pch = 16, col = "black")

    # Pivot
    points(shapes[pivot.LM,1,i], shapes[pivot.LM,2,i], pch = 16, col = "darkred")

    readline("Press any key to continue.")
  } # End   if (include.plot)


  # #########################################
  # MAIN LOOP
  # #########################################
  specimens.to.adjust <- 1:(dim(shapes)[3])
  specimens.to.adjust <- specimens.to.adjust[which(specimens.to.adjust != reference.specimen)]
  adjustment.angles <- NULL
  initial.rotation.limits <- rotation.limits
  ref.distances <- distMatrix(shapes[,,reference.specimen], main.structure.LMs, substructure.LMs)

  # Loop for each specimen
  for (i in 1:length(specimens.to.adjust)) {
    original.coords.i <- shapes[,,specimens.to.adjust[i]]

    # Curtail the rotation.limits of rotation based on the barrier
    rotation.limits <- initial.rotation.limits
    if (!is.null(barrier.LM)) {
      if (is.null(pivot.LM) | !is.numeric(pivot.LM) | (pivot.LM > dim(shapes)[1]) | (pivot.LM < 0)) {
        stop("Error: pivot.LM is invalid. See '?align.joint' for usage.\n ")
      }

      # Find the substructure LM closest to the barrier
      crash.LM <- as.numeric(sub("lm","",names(which.min(unlist(distMatrix(original.coords.i, barrier.LM, substructure.LMs))))))

      p12 <- distance(shapes[pivot.LM,,specimens.to.adjust[i]], shapes[barrier.LM,,specimens.to.adjust[i]])
      p13 <- distance(shapes[pivot.LM,,specimens.to.adjust[i]], shapes[crash.LM,,specimens.to.adjust[i]])
      p23 <- distance(shapes[barrier.LM,,specimens.to.adjust[i]], shapes[crash.LM,,specimens.to.adjust[i]])
      barrier.angle <- acos((p12^2 + p13^2 - p23^2) / (2 * p12 * p13))

      if (barrier.angle < 0) {
        rotation.limits[1] <- barrier.angle
      } else {
        rotation.limits[2] <- barrier.angle
      }

    } # End  if (!is.null(barrier.LM))

    # Calculate the matrix of distances b/w substructure LMs and main LMs.
    # Optimze the rotation angle that minimizes the sum of squared differences
    # berween distances in the reference and specimen i
    x <- optimize(
      f = rotate.substructure.diff,
      interval = rotation.limits,
      m = original.coords.i,
      LMs.to.rotate = substructure.LMs,
      pivot = pivot.LM,
      other.LMs = main.structure.LMs,
      ref.dist.matrix = ref.distances)
    theta.i <- x$minimum
    diff.i <- x$objective

    if (abs(theta.i) < tolerance) { theta.i <- 0 }

    # Rotate accordingly
    shapes[,,specimens.to.adjust[i]] <- rotate.substructure(original.coords.i, substructure.LMs, pivot.LM, theta = -theta.i )

    # Record the angle of rotation
    adjustment.angles <- c(adjustment.angles, theta.i)

    if (include.plot) {
      x <- ifelse(
        is.null(dimnames(shapes)[[3]][specimens.to.adjust[i]]),
        paste("specimen:", specimens.to.adjust[i]),
        dimnames(shapes)[[3]][specimens.to.adjust[i]]
      )
      # Base plot and landmark numbers based on the new positions
      landmark.plot(shapes[,,specimens.to.adjust[i]], main = x, text.color = "darkgray", ...)

      # Add text details
      x <- par("usr")[2]
      y <- par("usr")[4]
      s <- paste0("\nlimits: ",signif(rotation.limits[1],3)," (", signif(rotation.limits[1]*(180/pi),3),"˚) to ",signif(rotation.limits[2],3)," (", signif(rotation.limits[2]*(180/pi),3),"˚)  \n rotation: ",signif(theta.i,3)," (", signif(theta.i*(180/pi),3),"˚)  \nmean diff: ", signif(diff.i,4),"  \n")
      if (any(signif(rotation.limits,3) == signif(theta.i,3))) {
        s <- paste0(s,"\n  AT ROTATION LIMIT  \n")
      }
      text(x,y,s, adj = c(1,1), cex = 0.8, offset = 10)

      # Main structure landmarks
      points(shapes[main.structure.LMs,1,specimens.to.adjust[i]], shapes[main.structure.LMs,2,specimens.to.adjust[i]], pch = 16, col = "grey50")

      # Old substructure positions
      points(original.coords.i[substructure.LMs,1], original.coords.i[substructure.LMs,2], pch = 1, col = "darkred" )

      # New substructure positions
      points(shapes[substructure.LMs,1,specimens.to.adjust[i]], shapes[substructure.LMs,2,specimens.to.adjust[i]], pch = 16, col = "black")

      # Pivot
      points(shapes[pivot.LM,1,specimens.to.adjust[i]], shapes[pivot.LM,2,specimens.to.adjust[i]], pch = 16, col = "darkred")

      if (i != length(specimens.to.adjust)) {
        x <- readline("Show next specimen? ENTER = yes (Alignment will continue for all specimens)")
        include.plot <- (tolower(x) %in% c("","y","yes","ya","yep"))
      }
    } # End  if (include.plot)

  } # End loop for each specimen

  if (!is.null(gpa1.procD)) {
    gpa2.procD <- sum(gpagen(shapes, print.progress = FALSE)$procD)
    percent.improvement <- signif(((gpa1.procD - gpa2.procD) / gpa1.procD)*100,3)
  } else { percent.improvement <- NULL }

  if (!is.null(percent.improvement)) {
    if (percent.improvement > 0) {
      s1 <- paste0("Alignment improved (reduced) preliminary Procrustes distances by ",percent.improvement,"%.\n")
    } else {
      percent.improvement <- -1*percent.improvement
      s1 <- paste0("**Warning: Alignment increased preliminary Procrustes distances** by ",percent.improvement,"%.\n")
    }
    message(s1)
  }

  adjustment.angles <- round(adjustment.angles*(180/pi),1)
  if (include.plot) {
    hist(adjustment.angles, col="darkred", xlab="adjustment angles (˚)", main = "Histogram of adjustment angles")
  }

  # Prep the output
  output$coords <- shapes
  if (!is.null(provenance) & !any(grepl("provenance",names(output)))) {
    output$provenance <- provenance
  }

  s <- paste0(
    paste0("## Joint alignment\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::align.joint` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n"),
    paste0("- main structure landmarks: ",paste0(main.structure.LMs, collapse = ", "),"\n"),
    paste0("- substructure landmarks: ",paste0(substructure.LMs, collapse = ", "),"\n"),
    paste0("- pivot point: ",pivot.LM,"\n"),
    ifelse(is.null(barrier.LM),"\n",paste0("- barrier landmark: ",pivot.LM,"\n\n")),
    paste0("Adjustment angles ranged from ",min(adjustment.angles),"˚ to ",max(adjustment.angles),"˚ "),
    paste0("with a mean of ",round(mean(adjustment.angles),1),"˚, relative to the reference specimen, index number ", reference.specimen),
    ifelse(
      is.null(dimnames(shapes)[[3]][reference.specimen]),
      ".\n\n",
      paste0(": ", dimnames(shapes)[[3]][reference.specimen],".\n\n")
    ),
    ifelse(is.null(percent.improvement),"\n",paste0(s1,"\n"))
  )

  output$provenance$align.joint <- s

  return(output)

} # End of function
