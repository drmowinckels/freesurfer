#' utility to silently create directories
#' @noRd
mkdir <- function(path) {
  dir.create(
    path,
    showWarnings = FALSE,
    recursive = TRUE
  )
}

#' Create a Temporary File with a Newly Created Directory
#'
#' The `fs_tempfile` function generates a temporary file path and ensures
#' that the directory containing the file exists by creating it if necessary.
#'
#' @param ... Arguments passed to the \code{\link{tempfile}} function, typically
#'        used to customize the prefix, suffix, or pattern of the
#'        temporary file name.
#'
#' @return A character string representing the full file path to the
#'         temporary file. The directory containing the file will be
#'         created if it does not already exist.
#'
#' @details This function leverages \code{\link{tempfile}} to create a unique
#'          file path within the system's temporary directory. Before
#'          returning the file path, it ensures any required
#'          directories are created using the \code{\link{dir.create}} function.
#'
#' @examples
#' # Create a temporary file and its directory
#' file_path <- fs_tempfile(pattern = "example_")
#'
#' # Print the file path
#' print(file_path)
#'
#' # Check if the directory exists
#' dir.exists(dirname(file_path))
#'
#' @export
fs_tempfile <- function(...) {
  x <- tempfile(...)
  mkdir(dirname(x))
  x
}
