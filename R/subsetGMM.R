#' Subset a geometric morphometric shape dataset
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param A A \code{geomorph.data.frame} or a data structure such as those produced by the \code{borealis} package.
#'     A 3-dimensional array of XY coordinates for multiple specimens must reside in a list
#'     element called \code{coords} or \code{gdf$coords}. Associated metadata may exist in
#'     a matrix element called \code{metadata} or as vectors with a length equal to the number
#'     of specimens.
#' @param specimens A vector for subsetting specimens and metadata.
#' @param landmarks A vector for subsetting landmarks.
#'     Subsetting landmarks will not affect metadata.
#'
#' @export
#'
#' @examples
#' data(plethodon, package = "geomorph")
#' x <- subsetGMM(plethodon, landmarks = 1:9, specimens = 1:3)
#' landmark.plot(x, specimen.number = 1:4)
#' x$site
#'

subsetGMM <- function (A, specimens = NULL, landmarks = NULL)
{
  # Vet the shape data
  if (!(class(A)[1] %in% c("geomorph.data.frame","list"))) {
    stop("Error: Input is not a recognized type. (See the help entry: `?subsetGMM`.)")
  }
  if (any(grepl("land",names(A))) & !any(grepl("coords",names(A)))) {
    A$coords <- A$land
    A <- A[which(names(A)!="land")]
  }
  if (!any(grepl("coords",names(A)))) {
    stop("2Error: Input is not a recognized type. (See the help entry: `?subsetGMM`.)")
  }

  # Initialize output
  output <- A
  new.provenance <- paste0(
    paste0("## Data subset\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::subsetGMM` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n")
  )

  # Subset landmarks
  if (!is.null(landmarks) ) {
    if (!is.numeric(landmarks) | length(landmarks)<1) {
      stop("Error: `landmarks` is not a recognized type. (See the help entry: `?subsetGMM`.)")
    }
    output$coords <- output$coords[landmarks,,]
    new.provenance <- paste0(
      new.provenance,
      paste0("### Landmark subset\n\n"),
      paste0("Of ",dim(A$coords)[1]," landmarks, the following ",length(landmarks)," were retained: ",paste(landmarks,collapse = ", "),".\n\n")
    )
  } # End Subset landmarks

 # Subset specimens
  if (!is.null(specimens) ) {
    if (!is.numeric(specimens) | length(specimens)<1) {
      stop("Error: `specimens` is not a recognized type. (See the help entry: `?subsetGMM`.)")
    }
    output$coords <- output$coords[,,specimens]
    new.provenance <- paste0(
      new.provenance,
      paste0("### Specimen subset\n\n"),
      paste0("Of ",dim(A$coords)[3]," specimens, the following ",length(specimens)," were retained: ",paste(specimens,collapse = ", "),".\n\n")
    )
    # If there's a `metadata` table
    if (any(grepl("metadata",names(output)))) {
      output$metadata <- output$metadata[specimens,]
      new.provenance <- paste0(
        new.provenance,
        paste0("Accompanying metadata were also subsetted from a `metadata` element with the following columns: `",
               paste(colnames(output$metadata),collapse = "``, `"),"`.\n\n")
      )
    }
    # Other vector/factor elements of the data structure
    x1 <- (unlist(lapply(output, length)) == dim(A$coords)[3])
    x2 <- (unlist(lapply(output, function(x) { is.vector(x) | is.factor(x) })))
    subsetable.elements <- which(x1 & x2)
    if (length(subsetable.elements) > 0) {
      for (i in subsetable.elements) {
        output[[i]] <- output[[i]][specimens]
      }
      new.provenance <- paste0(
        new.provenance,
        paste0("Accompanying metadata were also subsetted from the following elements: `",
               paste(names(subsetable.elements),collapse = "``, `"),"`.\n\n")
      )
    }
  } # End Subset specimens

  # Output
  output$provenance$subsetGMM <- new.provenance
  return(output)

} # End of function
