#' Subset a geometric morphometric shape dataset
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param A A \code{geomorph.data.frame} or a data structure produced by functions in the \code{borealis} package.
#'     A 3-dimensional array of XY coordinates for multiple specimens must reside in a list
#'     element called \code{coords}, \code{gpagen$coords} or \code{gdf$coords}. Associated metadata may exist in
#'     a matrix element called \code{metadata} or as vectors with a length equal to the number
#'     of specimens.
#' @param specimens A numerical or logical vector for subsetting specimens and metadata.
#' @param landmarks A numerical vector for subsetting landmarks.
#'     Subsetting landmarks will not affect metadata.
#'
#' @export
#'
#' @examples
#' data(plethodon, package = "geomorph")
#' x <- subset.gmm(plethodon, landmarks = 1:9, specimens = 1:3)
#' landmark.plot(x, specimen.number = 1:4)
#' x$site
#'

subset.gmm <- function (A, specimens = NULL, landmarks = NULL)
{
  # Vet the shape data
  if (!(class(A)[1] %in% c("geomorph.data.frame","list"))) {
    stop("Error: Input is not a recognized type. (See the help entry: `?subset.gmm`.)")
  }
  if (any(grepl("land",names(A))) & !any(grepl("coords",names(A)))) {
    A$coords <- A$land
    A <- A[which(names(A)!="land")]
  }
  if (!any(names(A) %in% c("coords","gpagen","gdf"))) {
    stop("Error: Input is not a recognized type. (See the help entry: `?subset.gmm`.)")
  }

  # Initialize output
  output <- A
  new.provenance <- paste0(
    paste0("## Data subset\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::subset.gmm` on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n")
  )

  # Subset landmarks
  if (is.null(landmarks)) {
    if (any(names(output) == "gpagen")) {
      landmarks <- 1:(dim(A$gpagen$coords)[1])
      original.landmark.number <- dim(A$gpagen$coords)[1]
    } else {
      if (any(names(output) == "gdf")) {
        landmarks <- 1:(dim(A$gdf$coords)[1])
        original.landmark.number <- dim(A$gdf$coords)[1]
      }
    }
    if (any(names(output) == "coords")) {
      if (!is.null(dim(output$coords))) {
        landmarks <- 1:(dim(A$coords)[1])
        original.landmark.number <- dim(A$coords)[1]
      }
    }
  } else {
    if (!is.numeric(landmarks) | length(landmarks)<1) {
      stop("Error: `landmarks` is not a recognized type. (See the help entry: `?subset.gmm`.)")
    }
    if (any(names(output) == "gpagen")) {
      output$gpagen$coords <- output$gpagen$coords[landmarks,,]
      original.landmark.number <- dim(A$gpagen$coords)[1]
    } else {
      if (any(names(output) == "gdf")) {
        output$gdf$coords <- output$gdf$coords[landmarks,,]
        original.landmark.number <- dim(A$gdf$coords)[1]
      }
    }
    if (any(names(output) == "coords")) {
      if (!is.null(dim(output$coords))) {
        output$coords <- output$coords[landmarks,,]
        original.landmark.number <- dim(A$coords)[1]
      }
    }
    if (any(names(output) == "landmark.number")) {
      output$landmark.number <- length(landmarks)
    }
    new.provenance <- paste0(
      new.provenance,
      paste0("### Landmark subset\n\n"),
      paste0("Of ",original.landmark.number," landmarks, the following ",length(landmarks)," were retained: ",paste(landmarks,collapse = ", "),".\n\n")
    )
  } # End Subset landmarks

  # Subset specimens
  if (!is.null(specimens) ) {
    if (!(is.numeric(specimens) | is.logical(specimens)) | length(specimens)<1) {
      stop("Error: `specimens` is not a recognized type. (See the help entry: `?subset.gmm`.)")
    }
    if(is.logical(specimens)) {
      specimens <- which(specimens)
    }
    # GPAGEN elements
    if (any(names(output) == "gpagen")) {
      output$gpagen$coords <- output$gpagen$coords[,,specimens]
      original.specimen.number <- dim(A$gpagen$coords)[3]
      new.provenance <- paste0(
        new.provenance,
        paste0("### Specimen subset\n\n"),
        paste0("Of ",original.specimen.number," specimens, the following ",length(specimens)," were retained: ",paste(specimens,collapse = ", "),".\n\n")
      )
      # Other vector/factor elements of the data structure
      x1 <- (unlist(lapply(output$gpagen, length)) == original.specimen.number)
      x2 <- (unlist(lapply(output$gpagen, function(x) { is.vector(x) | is.factor(x) })))
      subsetable.elements <- which(x1 & x2)
      if (length(subsetable.elements) > 0) {
        for (i in subsetable.elements) {
          output$gpagen[[i]] <- output$gpagen[[i]][specimens]
        }
        new.provenance <- paste0(
          new.provenance,
          paste0("Accompanying metadata were subsetted from the following `gpagen` elements: `",
                 paste(names(subsetable.elements),collapse = "``, `"),"`.\n\n")
        )
      } # End # Other vector/factor elements of the data structure
    } else {
      # GDF elements
      if (any(names(output) == "gdf")) {
        output$gdf$coords <- output$gdf$coords[,,specimens]
        original.specimen.number <- dim(A$gdf$coords)[3]
        new.provenance <- paste0(
          new.provenance,
          paste0("### Specimen subset\n\n"),
          paste0("Of ",original.specimen.number," specimens, the following ",length(specimens)," were retained: ",paste(specimens,collapse = ", "),".\n\n")
        )
        # Other vector/factor elements of the data structure
        x1 <- (unlist(lapply(output$gdf, length)) == original.specimen.number)
        x2 <- (unlist(lapply(output$gdf, function(x) { is.vector(x) | is.factor(x) })))
        subsetable.elements <- which(x1 & x2)
        if (length(subsetable.elements) > 0) {
          for (i in subsetable.elements) {
            output$gdf[[i]] <- output$gdf[[i]][specimens]
          }
          new.provenance <- paste0(
            new.provenance,
            paste0("Accompanying metadata were subsetted from the following `gdf` elements: `",
                   paste(names(subsetable.elements),collapse = "``, `"),"`.\n\n")
          )
        } # End # Other vector/factor elements of the data structure
      }
    }
    # COORDS elements
    if (any(names(output) == "coords")) {
      if (!is.null(dim(output$coords))) {
        output$coords <- output$coords[,,specimens]
        original.specimen.number <- dim(A$coords)[3]
      }
    }
    # METADATA elements
    if (any(grepl("metadata",names(output)))) {
      output$metadata <- output$metadata[specimens,]
      new.provenance <- paste0(
        new.provenance,
        paste0("Accompanying metadata were subsetted from a `metadata` element with the following columns: `",
               paste(colnames(output$metadata),collapse = "``, `"),"`.\n\n")
      )
    }
    # Other vector/factor elements of the data structure
    x1 <- (unlist(lapply(output, length)) == original.specimen.number)
    x2 <- (unlist(lapply(output, function(x) { is.vector(x) | is.factor(x) })))
    subsetable.elements <- which(x1 & x2)
    if (length(subsetable.elements) > 0) {
      for (i in subsetable.elements) {
        output[[i]] <- output[[i]][specimens]
      }
      new.provenance <- paste0(
        new.provenance,
        paste0("Accompanying metadata were subsetted from the following list elements: `",
               paste(names(subsetable.elements),collapse = "``, `"),"`.\n\n")
      )
    } # End # Other vector/factor elements of the data structure
    if (any(names(output) == "specimen.number")) {
      output$specimen.number <- length(specimens)
    }
  } # End Subset specimens

  # Output
  output$provenance$subset.gmm <- new.provenance
  return(output)

} # End of function


