#' Write data provenance to a file
#'
#' Data provenance from the \code{provenance} element of the input list will be
#' written to file in \href{https://www.markdownguide.org/getting-started/}{markdown} format.
#'
#' @source   Dave Angelini \email{david.r.angelini@@gmail.com} [aut, cre]
#'
#' @param x List containing an element for data provenance.
#' @param output.filename The file name to export. If done is supplied, defaults to
#'     \code{data.provenance.YYMMDD.md}, where \code{YYMMDD} is the date.
#'
#' @export
#'
#' @examples
#'

write.provenance <- function (x, output.filename = NULL, title = NULL )
{
  # Vet the input
  if (!(class(x)[1] %in% c("gpagen","list"))) {
    return(cat("Error: Input is not a recognized type. (See the help entry: '?write.provenace'.)"))
  }
  if (!any(grepl("provenance",names(x)))) {
    return(cat("Error: Input is not a recognized type. (See the help entry: '?write.provenace'.)"))
  }

  # Vet the output.filename
  if (is.null(output.filename)) {
    output.filename <- paste('data.provenance',format(Sys.time(), "%y%m%d"),'md',sep='.')
  }
  if (!grepl(".md$",output.filename[[1]])) {
    output.filename <- paste0(output.filename[[1]],".md")
  }

  # Header
  header <- paste0(
    ifelse(
      is.null(title),
      paste0("# Data provenance report\n\n"),
      paste0("# ",title,"\n\n")
    ),
    collapse = ""
  )

  # Last entry
  footer <- paste0("Report written to ",output.filename," by ",toupper(Sys.getenv("LOGNAME"))," on ",format(Sys.time(), "%A, %d %B %Y, %X"),".\n\n")
  cat(footer)
  footer <- paste0("---\n\n",footer)

  # Combine everything
  output <- paste(header,paste0(unlist(x$provenance), collapse = "\n\n"),footer, collapse = "")

  write.table(output, file = output.filename, sep="\n", quote = FALSE, row.names = FALSE, col.names = FALSE )

} # End of function

