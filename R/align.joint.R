#' Align a subset of landmarks by rotation from one point
#'
#'
#'
#'
#' @return Returns a list with \code{coords}, \code{provenance}, and
#'    any other potential list elements from the input.
#'
#' @source Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param
#'
#' @export
#'
#' @examples
#

align.joint <- function(
  A,
  substructure.LMs = NULL,
  pivot.LM = NULL,
  reference.specimen = 1,
  include.plot = TRUE,
  verbose = TRUE,
  max.iter = 10,
  provenance = NULL
)
{ # Begin the function

  # Initialize
  shape.data <- NULL
  output <- NULL

  # Vet the input
  if (class(A)[1] %in% c("gpagen","list")) {
    shape.data <- A$coords
    output <- A[-grep("coords",names(A))]
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shape.data <- A
    } else {
      return(cat("Error: Input is not a recognized type. (See the help entry: '?procrustes.alignment'.)"))
    }
  }
  if (dim(shape.data)[2] != 2) {
    return(cat("Error: requires a matrix of X and Y corrdinates."))
  }
  if (is.null(substructure.LMs) | !is.numeric(substructure.LMs) | (max(substructure.LMs) > dim(shape.data)[1]) | (min(substructure.LMs) < 0)) {
    return(cat("Error: substructure.LMs is invalid. See '?align.join' for usage.\n "))
  }
  if (is.null(pivot.LM) | !is.numeric(pivot.LM) | (pivot.LM > dim(shape.data)[1]) | (pivot.LM < 0)) {
    return(cat("Error: pivot.LM is invalid. See '?align.join' for usage.\n "))
  }

  # Debugging test variales
  substructure.LMs <- c(5,6,7)
  pivot.LM <- 5
  A <- matrix(c(789,1835,1584,1841,798,1118,1581,1100,1599,1517,2046,1496,2454,1796), ncol=2, byrow = TRUE)
  A <- array(rep(A,3), dim =c(7,2,3))
  A <- A + runif(length(unlist(A)), min = -20, max = 20)
  A[6,2,2] <- A[6,2,2] - 100; A[7,2,2] <- A[7,2,2] - 250
  A[6,2,3] <- A[6,2,3] - 200; A[7,2,3] <- A[7,2,3] - 400
  plot(A[,,1])
  points(A[,,2], col = "darkred")
  points(A[,,3], col = "darkblue")
  plot(gpagen(A))
  shape.data <- A

  # Identify the main structure landmarks
  main.structure.LMs <- 1:dim(shape.data)[1]
  main.structure.LMs <- main.structure.LMs[which(!(main.structure.LMs %in% substructure.LMs))]
  main.structure.LMs <- c(main.structure.LMs, pivot.LM)

  # MAIN LOOP
  continue <- TRUE
  i <- 1
  while (continue) {





    i <- i + 1
    if (i >= max.iter) { continue <- FALSE }
  }



  # Prep the output
  output <- list(output, coords = shape.data)
  if (!is.null(provenance) & !any(grepl("provenance",names(output)))) {
    output[["provenance"]] <- provenance
  }
  output[["provenance"]][["align.joint"]] <- paste0(
    "## Joint alignment\n\n",
    paste0("Performed using `borealis::align.joint` by ",toupper(Sys.getenv("LOGNAME"))," on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n"),
    paste0("Substructure landmark numbers: ",paste0(substructure.LMs, collapse = ", "),"\n\n"),
    paste0("Relative rotatation about landmark number ",pivot.LM,"\n\n"),
  )

  return(output)

} # End of function
