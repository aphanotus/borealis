#' Extract landmarks from serial homologs to create a new dataset
#'
#' This function allows comparisons of shape data from serial homologs (e.g. forewing
#' and hindwings). Thew initial dataset must have serially homologous landmarks placed
#' on each serially homologous structure.
#'
#' @param A A 3-dimensional array containing XY shape coordinates for multiple specimens,
#'     or a list containing such as an array and data provenance.
#' @param partitions A matrix or data frame listing landmarks associated with each serial
#'     homolog. Each column should contain the landmarks for each serial homolog.
#' @param verbose A logical argument specifying whether to display metrics each
#'     during each iteration.
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

extract.serial.homologs <- function (
  A,
  partitions,
  verbose = TRUE
)
{ # Begin the function

  warning("This function is in active development!")

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
        stop("Error: Input is not a recognized type. (See the help entry: `?extract.serial.homologs`.)\n")
      }
    }
  } else {
    if ((class(A)[1] == "array") & (length(dim(A)) == 3)) {
      shapes <- A
    } else {
      stop("Error: Input is not a recognized type. (See the help entry: `?extract.serial.homologs`.)\n")
    }
  }
  if (dim(shapes)[2] != 2) {
    stop("Error: requires a matrix of X and Y corrdinates. (See the help entry: `?extract.serial.homologs`.)\n")
  }
  partitions <- as.matrix(partitions)
  if (any(is.na(c(partitions))) | any(!is.numeric(c(partitions)))) {
    stop("Error: `partitions` is not a numerical matrix. (See the help entry: `?extract.serial.homologs`.)\n")
  }
  # End preliminary vetting of the input and arguments

  # Do the thing
  for (i in 1:(dim(partitions)[2])) {
    x <- subset.gmm(shapes, landmarks = partitions[,i])
  }



  # Prep the output
  output$coords <- shapes
  if (!any(grepl("provenance",names(output)))) {
    output$provenance <- list()
  }

  s <- paste0(
    paste0("## Estimate missing landmarks\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::extract.serial.homologs` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n")
  )

  output$provenance$extract.serial.homologs <- s

  return(output)

} # End of function
