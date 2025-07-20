#' Get FreeSurfer Configuration Settings
#'
#' Functions to manage FreeSurfer configuration settings, including determining directories, license files, and output formats. These settings are prioritized based on environment variables, R options, default fallback paths, and other predefined rules.
#'
#' @param env_var [character] Environment variable name to check.
#' @param opt_var [character] R option name to check.
#' @param defaults [character] A vector of default paths to check; used as fallbacks if the environment or options are not set (optional).
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{value}{[character] The resolved configuration value (e.g., directory path, file path, or setting).}
#'   \item{source}{[character] Indicates how the value was determined (in order of search; R option, environment variable, default location).}
#'   \item{exists}{[logical] Whether the value corresponds to an existing file or path.}
#' }
#'
#' @seealso [Sys.getenv()], [getOption()], [file.exists()], [file.path()]
#'
#' @examples
#' # Retrieve FreeSurfer home directory
#' get_fs_home()
#'
#' # Retrieve FreeSurfer license file
#' get_fs_license()
#'
#' # Retrieve FreeSurfer subjects directory
#' get_fs_subdir()
#'
#' # Retrieve FreeSurfer output format
#' get_fs_output()
#' @export
get_fs_setting <- function(env_var, opt_var, defaults = NULL, is_path = TRUE) {
  original_opt <- getOption(opt_var)
  if (!is.null(original_opt)) {
    return(return_setting(
      original_opt,
      "getOption",
      is_path
    ))
  }

  original_env <- Sys.getenv(env_var)
  if (nzchar(original_env)) {
    return(return_setting(
      original_env,
      "Sys.getenv",
      is_path
    ))
  }

  if (!is.null(defaults)) {
    for (def_path in defaults) {
      if (file.exists(def_path)) {
        return(return_setting(
          def_path,
          "Default",
          is_path
        ))
      }
    }
  }

  return_setting(
    value = NA,
    source = NA,
    is_path
  )
}

#' @describeIn get_fs_setting Retrieve the FreeSurfer installation directory
#' @export
get_fs_home <- function() {
  get_fs_setting(
    "FREESURFER_HOME",
    "freesurfer.home",
    c(
      "/usr/freesurfer",
      "/usr/bin/freesurfer",
      "/usr/local/freesurfer",
      "/Applications/freesurfer"
    )
  )
}

#' @describeIn get_fs_setting Retrieve the FreeSurfer license file path
#' @export
get_fs_license <- function() {
  fs_home <- get_fs_home()
  lp <- file.path(fs_home$value, ".license")
  if (file.exists(lp)) {
    return(return_setting(lp, "fs_dir()"))
  }
  lp <- file.path(fs_home$value, "license.txt")
  if (file.exists(lp)) {
    return(return_setting(lp, "fs_dir()"))
  }
  return_setting(NA, "No license found.")
}

#' @describeIn get_fs_setting Retrieve the FreeSurfer "subjects" directory
#' @export
get_fs_subdir <- function() {
  ret <- get_fs_setting(
    "SUBJECTS_DIR",
    "freesurfer.subj_dir",
    file.path(fs_dir(), "subjects")
  )

  if (!is.na(ret$source)) {
    if (ret$source == "Default") {
      ret$source = "fs_dir()"
    }
  }

  ret
}

#' @describeIn get_fs_setting Retrieve the FreeSurfer source script
#' @export
get_fs_source <- function() {
  ret <- get_fs_setting(
    "FREESURFER_SH",
    "freesurfer.sh",
    file.path(fs_dir(), "FreeSurferEnv.sh")
  )
  if (ret$source == "Default") {
    ret$source = "fs_dir()"
  }
  ret
}

#' @describeIn get_fs_setting Retrieve the output format for FreeSurfer
#' @export
get_fs_output <- function() {
  ret <- get_fs_setting(
    "FSF_OUTPUT_FORMAT",
    "freesurfer.output_type",
    is_path = FALSE
  )
  if (is.na(ret$value)) {
    return(
      return_setting(
        "nii.gz",
        "Default",
        FALSE
      )
    )
  }
  ret
}

#' @describeIn get_fs_setting Retrieve mni bin folder
#' @export
get_mni_bin <- function() {
  ret <- get_fs_setting(
    NA,
    "freesurfer.mni_path",
    file.path(fs_dir(), "mni")
  )

  if (!ret$exists) {
    return(ret)
  }

  mni <- list.files(
    pattern = "MNI[.]pm",
    path = ret$value,
    full.names = TRUE,
    recursive = TRUE
  )

  return_setting(
    dirname(mni),
    ret$source
  )
}

#' @noRd
return_setting <- function(value, source, is_path = TRUE) {
  if (all(is.na(value))) {
    exists <- FALSE
  } else {
    exists <- file.exists(value)
  }

  list(
    value = value,
    source = source,
    exists = ifelse(is_path, exists, NA)
  )
}

return_single <- function(setting) {
  idx <- which(setting$exists)[1]
  setting$value <- setting$value[idx]
  setting$exists <- setting$exists[idx]
  setting
}
