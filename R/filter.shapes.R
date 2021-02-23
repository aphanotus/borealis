#' Filter landmark coordinate shape data via a factor
#'
#' The input must be a list containing one or more elements of shape data.
#' The function will determine the dimensionality of each element and subset them accordingly.
#' Matrices (2-dimensional elements) will have rows selected according to \code{index}.
#' Arrays (3-dimensional elements)  will have specimens (the third dimension) selected.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param x A list containing shape data in \code{coords}, \code{gpagen}, or a geomorph.data.frame in \code{gdf}, as well as (optionally) \code{metadata}.
#' @param index A vector for subsetting (numerical or logical). This value should correspond to metadata row numbers and specimemn numbers of the coordinate array.
#' @param index.description A character description of the filter criteria to be added to data provenance.
#'
#' @export
#'
#'

filter.shapes <- function (x, index, index.description = NULL)
{
  .Deprecated("subsetGMM")

  # Vet the input
  if (!any(grepl("list",class(x)))) {
    stop("Error: Input is not a recognized type. (See the help entry: '?filter.shapes'.)")
  }
  index.name <- deparse(substitute(index))
  if (is.logical(index)) { index <- which(index) }
  if (!is.numeric(index)) {
    stop("Error: Index is not a recognized type. (See the help entry: '?filter.shapes'.)")
  }

  output <- x

  # Determine the input data structure
  vector.elements <- which(unname(unlist(lapply(x, function(i) { if (is.vector(i) & !is.list(i)) { return (length(i) > 1) } else { return(FALSE) } }))))
  matrix.elements <- which(unname(lapply(x, function(i) {length(dim(i))}) == 2))
  array.elements <- which(unname(lapply(x, function(i) {length(dim(i))}) == 3))

  # Apply the filter index
  if (length(vector.elements) > 0) { output[[vector.elements]] <- output[[vector.elements]][index] }
  if (length(matrix.elements) > 0) { output[[matrix.elements]] <- output[[matrix.elements]][index,] }
  if (length(array.elements) > 0) { output[[array.elements]] <- output[[array.elements]][,,index] }

  if (any(grepl("gpagen",names(x)))) {
    output$gpagen$coords <- output$gpagen$coords[,,index]
    output$gpagen$Csize <- output$gpagen$Csize[index]
  }

  if (any(grepl("gdf",names(x)))) {
    coord.col <- grep("coords",names(output$gdf))
    other.cols <- grep("coords",names(output$gdf), invert = TRUE)
    output$gdf[[coord.col]] <- output$gdf[[coord.col]][,,index]
    for (i in other.cols) {
      output$gdf[[i]] <- output$gdf[[i]][index]
    }
  }

  # Update the specimen.number element
  if (any(grepl("specimen.number",names(x)))) {
    output$specimen.number <- length(index)
  }

  # Add new provenance
  s <- paste0(
    paste0("## Filter\n\n"),
    paste0("Performed by user `",(Sys.getenv("LOGNAME")),"` with `borealis::filter.shapes` version ",packageVersion("borealis")," on ",format(Sys.time(), "%A, %d %B %Y, %X"),"\n\n")
  )
  if (!is.null(index.description)) { s <- paste0(s, paste0("Filter description: ",paste0(index.description, collapse = " "),"\n\n") ) }
  if (!is.null(index.name)) { s <- paste0(s, paste0("Filter index: ",paste0(index.name, collapse = ", "),"\n\n") ) }
  s <- paste0(s,  paste0("New specimen number: ",length(index),"\n\n") )

  output$provenance$filter.shapes <- s

  return(output)

} # End of function
