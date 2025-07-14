#' Skip test if FreeSurfer is not installed
#'
#' @keywords internal
skip_if_no_freesurfer <- function() {
  if (!have_fs()) {
    testthat::skip(
      "FreeSurfer not found. Skipping FreeSurfer-dependent test(s)."
    )
  }
}

#' Skip test if FSL is not installed
#'
#' @keywords internal
skip_if_no_fsl <- function() {
  if (!fslr::have_fsl()) {
    testthat::skip(
      "FSL not found. Skipping FSL-dependent test(s)."
    )
  }
}

rmdir <- function(path) {
  unlink(path, recursive = TRUE, force = TRUE)
}

mock_freesurfer_env <- function(base_path = TEST_FS_HOME) {
  unlink(base_path, recursive = TRUE, force = TRUE)
  mkdir(file.path(base_path, "bin"))
  file.create(file.path(base_path, "license.txt"))
  writeLines(
    c(
      "export FREESURFER_HOME=\"$0\"",
      "export PATH=\"$FREESURFER_HOME/bin:$PATH\"",
      "export SUBJECTS_DIR=\"$FREESURFER_HOME/subjects\""
    ),
    con = file.path(base_path, "FreeSurferEnv.sh")
  )
  mkdir(file.path(base_path, "subjects"))
}
