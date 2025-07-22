<<<<<<< HEAD
#' Generate FreeSurfer Command Line Environment Setup
#'
#' This function generates a bash command string to set up the environment for using FreeSurfer.
#' It ensures the required FreeSurfer installation, license, and environment setup files are
#' validated and included in the command string. The function handles different FreeSurfer
#' binaries like `bin`, `mni/bin`, and others, while ensuring proper initialization of
#' the MNI environment if required.
#'
#' @param bin_app [character] A vector of options for the binary application directory.
#'   Possible options include:
#'   - `"bin"`: Default FreeSurfer binary directory.
#'   - `"mni/bin"`: Includes MNI initialization.
#'   - `""`: Base directory with no specific subdirectories.
#'
#' @return [character] A bash command string that includes environment setup for FreeSurfer.
#'   If the FreeSurfer environment or required configurations cannot be initialized, the function
#'   throws an error or issues a warning. On success, the returned string can be used directly
#'   in shell operations to load the FreeSurfer environment.
#'
#' @examplesIf have_fs()
#' # Generate a shell command to set up FreeSurfer with the default `bin`
#' cmd <- get_fs(bin_app = "bin")
#' print(cmd)
#'
#' # Generate a shell command to include MNI environment setup
#' cmd_mni <- get_fs(bin_app = "mni/bin")
#' print(cmd_mni)
#'
#' @seealso [get_fs_home()], [get_fs_license()], [get_fs_output()]
#' @export
get_fs = function(bin_app = c("bin", "mni/bin", "")) {
  fs_home_info <- get_fs_home()
  freesurferdir <- fs_home_info$value
  cmd <- NULL

  if (is.null(freesurferdir) || !fs_home_info$exists) {
    cli::cli_abort(
      "Can't find Freesurfer installation. Please set {.code FREESURFER_HOME} environment variable or {.code freesurfer.home} R option."
=======
#' @title Create command declaring FREESURFER_HOME
#' @description Finds the Freesurfer from system environment or \code{getOption("freesurfer.path")}
#' for location of Freesurfer functions
#' @param bin_app Should \code{bin} be added to the freesurfer path?
#' All executables are assumed to be in \code{FREESURFER_HOME/bin/}.  If not, and
#' \code{bin_app = ""}, they will be assumed to be in \code{FREESURFER_HOME/}.
#' @note This will use \code{Sys.getenv("FREESURFER_HOME")} before \code{getOption("freesurfer.path")}.
#' If the directory is not found for Freesurfer in \code{Sys.getenv("FreesurferDIR")} and
#' \code{getOption("freesurfer.path")}, it will try the default directory \code{/usr/local/freesurfer}.
#' @return NULL if Freesurfer in path, or bash code for setting up Freesurfer DIR
#' @export
#' @examplesIf have_fs()
#' get_fs()
get_fs = function(bin_app = c("bin", "mni/bin", "")) {
  cmd = NULL
  freesurferdir = Sys.getenv("FREESURFER_HOME")
  if (
    !is.null(freesurferdir) &&
      !file.exists(freesurferdir) &&
      !freesurferdir %in% ""
  ) {
    warning("FREESURFER_HOME is set but it does not exist!")
  }
  if (is.null(freesurferdir)) {
    freesurferdir = ""
  }
  add_home = FALSE
  if (freesurferdir == "") {
    add_home = TRUE
    bin_app = match.arg(bin_app)
    freesurferdir = getOption("freesurfer.path")
    ## Will try a default directory (/usr/local/freesurfer) if nothing else
    if (is.null(freesurferdir)) {
      #### adding in "/usr/share/freesurfer/5.0" for NeuroDeb
      def_paths = c(
        "/usr/local/freesurfer",
        "/Applications/freesurfer",
        "/usr/freesurfer",
        "/usr/bin/freesurfer"
      )
      for (def_path in def_paths) {
        if (file.exists(def_path)) {
          warning(paste0("Setting freesurfer.path to ", def_path))
          options(freesurfer.path = def_path)
          freesurferdir = def_path
          break
        }
      }
    }
    # bin = "bin"
    bin_app = paste0(bin_app, "/")
    # if (!add_bin) {
    #   bin_app = bin = ""
    # }
    # FSF_OUTPUT_FORMAT
    freesurferout = get_fs_output()
    # lic_file = file.path(freesurferdir, "license.txt")
    # if (!file.exists(lic_file)) {
    #   try_lic_file = file.path(freesurferdir, "LICENSE")
    #   if (file.exists(try_lic_file)) {
    #     file.copy(from = try_lic_file, to = lic_file, overwrite = FALSE)
    #   }
    # }
    cmd = NULL
    ###########################################
    # Need to fix PERL startup
    ###########################################
    if (grepl("mni", bin_app)) {
      mni_dir = file.path(freesurferdir, "mni")
      start_up = list.files(
        pattern = "MNI[.]pm",
        path = mni_dir,
        full.names = TRUE,
        recursive = TRUE
      )
      if (length(start_up) > 1) {
        start_up = start_up[1]
        warning("First MNI.pm file found used")
      }
      if (length(start_up) == 0) {
        warning("MNI startup file not found, trying MNI function anyway ")
        cmd = NULL
      } else {
        start_up = dirname(start_up)
        cmd = paste0("export PERL5LIB=$PERL5LIB:", start_up, " ; ")
      }
    }
  }

  shfile = file.path(freesurferdir, "FreeSurferEnv.sh")
  sourcer = getOption("freesurfer_source_function")
  if (is.null(sourcer) || sourcer %in% "") {
    # Tries to
    # fix https://github.com/muschellij2/freesurfer/issues/9
    try_sourcer = function(sourcer) {
      sh_file_cmd = ifelse(
        file.exists(shfile),
        paste0(
          sourcer,
          " ",
          shQuote(shfile),
          ifelse(grepl('"', sourcer), '"', ""),
          "; "
        ),
        ""
      )
      source_test = paste0(
        "export FREESURFER_HOME=",
        shQuote(freesurferdir),
        "; ",
        sh_file_cmd
      )
      res = suppressWarnings({
        system(
          source_test,
          intern = FALSE,
          ignore.stdout = FALSE,
          ignore.stderr = TRUE
        )
      })
      return(res)
    }
    sourcer_options = c("source", "bash -c \"source", ".")
    sourcer_results = sapply(sourcer_options, try_sourcer) == 0
    if (!any(sourcer_results)) {
      warning(paste0(
        "No sourcing seems to work for Freesurfer, using",
        " source"
      ))
      sourcer = "bash -c \"source"
    } else {
      sourcer = names(sourcer_results)[sourcer_results][1]
    }
    options(freesurfer_source_function = sourcer)
  }

  # if (add_home && !grepl("reesurfer", Sys.getenv("PATH"))) {
  sh_file_cmd = ifelse(
    file.exists(shfile),
    paste0(
      sourcer,
      " ",
      shQuote(shfile),
      ifelse(grepl('"', sourcer), '"', ""),
      " || true ; "
    ),
    ""
  )
  sourcer_test = paste0(
    ifelse(
      add_home,
      paste0("export FREESURFER_HOME=", shQuote(freesurferdir), "; "),
      ""
    ),
    sh_file_cmd
  )
  res = suppressWarnings({
    system(
      sourcer_test,
      intern = FALSE,
      ignore.stdout = FALSE,
      ignore.stderr = TRUE
    )
  })
  if (res != 0) {
    sh_file_cmd = ""
  }
  # } else {
  #   sh_file_cmd = ""
  # }
  cmd <- paste0(
    cmd,
    ifelse(
      add_home,
      paste0("export FREESURFER_HOME=", shQuote(freesurferdir), "; "),
      ""
    ),
    sh_file_cmd
  )

  if (add_home) {
    freesurferout = get_fs_output()
    cmd = paste0(
      cmd,
      "FSF_OUTPUT_FORMAT=",
      freesurferout,
      "; ",
      "export FSF_OUTPUT_FORMAT; ",
      paste0("${FREESURFER_HOME}/", bin_app)
>>>>>>> a9fbf4b (use roxygen @examplesIf for tests)
    )
  }

  # Check license
  if (get_fs_license()$exists) {
    cli::cli_warn(
      "Freesurfer is found, but no license file ({.path license.txt} or {.path .license}) found!"
    )
  }

  bin_app <- match.arg(bin_app)
  bin_app_path <- paste0(bin_app, "/")
  add_home <- ifelse(
    grepl("Default", fs_home_info$source),
    TRUE,
    FALSE
  )

  # Handle MNI Perl startup if 'mni' is in bin_app
  if (grepl("mni", bin_app)) {
    start_up_path <- get_mni_bin() |>
      return_single()
    start_up_path <- start_up_path$value
    if (!is.na(start_up_path)) {
      cmd <- paste0(
        "export PERL5LIB=$PERL5LIB:",
        shQuote(start_up_path),
        " ; "
      )
    }
<<<<<<< HEAD
  }

  # Source FreeSurferEnv.sh
  sourcer <- get_fs_source()

  sh_file_cmd <- ifelse(
    sourcer$exists,
    paste0(
      "source ",
      shQuote(sourcer$value),
      " || true ; "
    ), # Use || true to prevent shell from exiting on error
    ""
  )

  # Construct the main command string
  if (!add_home) {
    return(
      paste0(cmd, sh_file_cmd)
    )
=======
  }
  if (is.null(freesurferdir)) {
    stop("Can't find Freesurfer")
  }
  if (freesurferdir %in% "") {
    stop("Can't find Freesurfer")
  }
  if (!is.null(freesurferdir) && !file.exists(freesurferdir)) {
    stop("Can't find Freesurfer")
  }

  fs_license_txt = file.path(freesurferdir, "license.txt")
  fs_license = file.path(freesurferdir, ".license")
  if (!file.exists(fs_license) && !file.exists(fs_license_txt)) {
    warning("Freesurfer is found, but no license!")
>>>>>>> a9fbf4b (use roxygen @examplesIf for tests)
  }

  paste0(
    cmd,
    sprintf(
      "export FREESURFER_HOME=%s; ",
      shQuote(freesurferdir)
    ),
    "",
    sh_file_cmd,
    "FSF_OUTPUT_FORMAT=",
    get_fs_output()$value,
    "; ",
    "export FSF_OUTPUT_FORMAT; ",
    "${FREESURFER_HOME}/",
    bin_app_path
  )
}


#' @title Get Freesurfer's Directory
<<<<<<< HEAD
#' @description Finds the `FREESURFER_HOME` from system environment or
#' `getOption("freesurfer.home")` for location of Freesurfer functions and returns its value.
#' @return Character path to the Freesurfer home directory.
#' @aliases freesurfer_dir
#' @export
#' @examples
#' if (have_fs()) {
#'  freesurferdir()
#'  freesurfer_dir()
#'  fs_dir()
#' }
freesurferdir = function() {
  get_fs_home()$value
=======
#' @description Finds the FREESURFER_HOME from system environment or
#' \code{getOption("freesurfer.path")}
#' for location of Freesurfer functions and returns it
#' @return Character path
#' @aliases freesurfer_dir
#' @export
#' @examplesIf have_fs()
#' freesurferdir()
#' freesurfer_dir()
#' fs_dir()
freesurferdir = function() {
  freesurferdir = Sys.getenv("FREESURFER_HOME")
  if (freesurferdir == "") {
    x = get_fs()
    freesurferdir = getOption("freesurfer.path")
  }
  return(freesurferdir)
>>>>>>> a9fbf4b (use roxygen @examplesIf for tests)
}

#' @rdname freesurferdir
#' @export
<<<<<<< HEAD
freesurfer_dir = function() {
  freesurferdir()
}

#' @rdname freesurferdir
#' @export
fs_dir = function() {
  freesurferdir()
}

#' @title Determine Freesurfer Subjects Directory
#' @description Finds the `SUBJECTS_DIR` from system environment or
#' `getOption("freesurfer.subj_dir")` and returns its value.
#' @return Character path to the Freesurfer subjects directory.
#' @export
#' @examples
#' if (have_fs()) {
#'    fs_subj_dir()
#' }
fs_subj_dir = function() {
  get_fs_subdir()$value
}

#' @title Logical check if Freesurfer is accessible
#' @description Checks if FreesurferDIR is accessible and optionally if a license file exists.
#' @param check_license [logical] Should a license file be checked for existence?
#' @return Logical `TRUE` if Freesurfer is accessible and license (if checked) is found, `FALSE` otherwise.
#' @export
#' @examples
#' have_fs()
have_fs = function(check_license = TRUE) {
  fs_home <- get_fs_home()$exists
  if (check_license) {
    return(fs_home && get_fs_license()$exists)
=======
freesurfer_dir = freesurferdir()

#' @rdname freesurferdir
#' @export
fs_dir = freesurferdir()


#' @title Logical check if Freesurfer is accessible
#' @description Uses \code{get_fs} to check if FreesurferDIR is accessible or the option
#' \code{freesurfer.path} is set and returns logical
#' @param ... options to pass to \code{\link{get_fs}}
#' @param check_license Should a license file be checked to exist?
#' @return Logical TRUE is Freesurfer is accessible, FALSE if not
#' @export
#' @examples
#' have_fs()
have_fs = function(..., check_license = FALSE) {
  freesurferdir = suppressWarnings(try(fs_dir(...), silent = TRUE))
  if (inherits(freesurferdir, "try-error")) {
    return(FALSE)
  }
  if (!file.exists(freesurferdir)) {
    return(FALSE)
  }
  if (check_license) {
    fs_license = file.path(freesurferdir, "license.txt")
    if (!file.exists(fs_license)) {
      return(FALSE)
    }
  }
  return(TRUE)
}


#' @title Determine Freesurfer output type
#' @description Finds the FSF_OUTPUT_FORMAT from system environment or
#' \code{getOption("fs.outputtype")} for output type (nii.gz, nii, ANALYZE,etc)
#' @return FSF_OUTPUT_FORMAT, such as nii.gz  If none found, uses nii.gz as default
#'
#' @export
#' @examples
#' get_fs_output()
get_fs_output = function() {
  fs_out = Sys.getenv("FSF_OUTPUT_FORMAT")
  if (fs_out == "") {
    fs_out = getOption("fs.outputtype")
  }
  if (is.null(fs_out)) {
    warning("Can't find FSF_OUTPUT_FORMAT, setting to nii.gz")
    fs_out = "nii.gz"
    options(fs.outputtype = fs_out)
  }
  if (fs_out == "") {
    warning("Can't find FSF_OUTPUT_FORMAT, setting to nii.gz")
    fs_out = "nii.gz"
    options(fs.outputtype = "nii.gz")
  }
  return(fs_out)
}

#' @title Determine extension of image based on FSLOUTPUTTYPE
#' @description Runs \code{get_fs_output()} to extract FSLOUTPUTTYPE and then
#' gets corresponding extension (such as .nii.gz)
#' @return Extension for output type
#' @export
#' @examples
#' fs_imgext()
fs_imgext = function() {
  fs_out = get_fs_output()
  ext = switch(fs_out, "hdr" = ".hdr", "nii.gz" = ".nii.gz", "nii" = ".nii")
  return(ext)
}


#' @title Determine Freesurfer Subjects Directory
#' @description Finds the SUBJECTS_DIR from system environment or
#' \code{getOption("fs.subj_dir")} for subjects dir
#' @return SUBJECTS_DIR, such as \code{${FREESURFER_HOME}/subjects}
#'
#' @export
#' @examplesIf have_fs()
#' fs_subj_dir()
fs_subj_dir = function() {
  fs_out = Sys.getenv("SUBJECTS_DIR")
  if (fs_out == "") {
    fs_out = getOption("fs.subj_dir")
  }
  if (is.null(fs_out)) {
    warning(
      "SUBJECTS_DIR not set, setting to ",
      paste0("file.path(set_fs_subj_dir(), 'subjects')")
    )
    res = suppressWarnings(try(
      set_fs_subj_dir(),
      silent = TRUE
    ))
    if (inherits(res, "try-error")) {
      fs_out = NA
    } else {
      fs_out = res
    }

    # fs_out = file.path(fs_dir(), "subjects")
  }
  if (!is.na(fs_out)) {
    if (fs_out == "") {
      fs_out = NA
    }
  }
  return(fs_out)
}

#' @title Set Freesurfer Subjects Directory
#' @description Sets the SUBJECTS_DIR variable in the system environment or
#' \code{options("fs.subj_dir" = x)}
#' @param x path to SUBJECTS_DIR defaults to \code{file.path(fs_dir(), "subjects")}
#' @return No return value, called for side effects (`SUBJECTS_DIR`
#' environment variable set, and `fs.subj_dir` option set)
#'
#' @export
set_fs_subj_dir = function(x = file.path(fs_dir(), "subjects")) {
  if (!file.exists(x)) {
    stop("Path to set subj_dir does not exist, erroring out!")
>>>>>>> a9fbf4b (use roxygen @examplesIf for tests)
  }
  return(fs_home)
}
<<<<<<< HEAD


#' @title Determine extension of image based on Freesurfer output type
#' @description Runs `get_fs_output()` to extract the Freesurfer output type
#' and then gets the corresponding file extension (such as `.nii.gz`).
#' @return Character string representing the file extension for the output type.
#' @export
#' @examples
#' fs_imgext()
fs_imgext = function() {
  paste0(".", get_fs_output()$value)
}
=======
>>>>>>> a9fbf4b (use roxygen @examplesIf for tests)
