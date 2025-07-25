#' @title Reconstruction Helper for recon from Freesurfer
#' @description Wrapper for the \code{recon-all} function in Freesurfer
#'
#' @note If you set \code{infile = NULL}, then you can omit the
#' \code{-i} flag in \code{recon-all}
#' @param infile Input filename (dcm or nii)
#' @template outdir
#' @template subjid
#' @template verbose
#' @template opts
#' @param force Force running of the reconstruction
#'
#' @return Result of \code{\link{system}}
#' @importFrom tools file_path_sans_ext
#' @export
reconner <- function(
  infile = NULL,
  outdir = NULL,
  subjid = NULL,
  verbose = get_fs_verbosity(),
  opts = "-all",
  force = FALSE
) {
  #####################################
  # Checking
  #####################################
  if (is.null(subjid) && is.null(infile)) {
    cli::cli_abort("Either subjid or infile must be specified!")
  }
  if (!is.null(infile)) {
    infile = checknii(infile)
  }
  #####################################
  # Making subjid from filename
  #####################################
  if (is.null(subjid)) {
    subjid = gsub("[.]mg(z|h)$", "", infile)
    subjid = nii.stub(subjid, bn = TRUE)
    subjid = file_path_sans_ext(subjid)
    if (verbose) {
      cli::cli_alert_info("Subject set to: {.val {subjid}}")
    }
  }

  #####################################
  # Checking outdir - otherwise using fs_subj_dir
  #####################################
  if (!is.null(outdir)) {
    sd_opts = paste0(" -sd ", shQuote(outdir))
    subject_directory = file.path(sd_opts, subjid)
  } else {
    sd_opts = ""
    subject_directory = file.path(fs_subj_dir(), subjid)
  }

  #####################################
  # Processing infile
  #####################################
  if (!is.null(infile)) {
    in_opts = paste0("-i ", infile)
    if (dir.exists(subject_directory)) {
      cli::cli_warn(
        "Subject Directory {.path subject_directory} already exists - 
        either use {.code force = TRUE}, or delete directory"
      )
    }
  } else {
    in_opts = ""
  }

  opts = paste(
    in_opts,
    sd_opts,
    paste0(" -subjid ", subjid),
    opts
  )
  if (force) {
    opts = paste(opts, "-force")
  }

  cmd = get_fs()
  cmd = paste0(cmd, "recon-all")
  cmd = paste(cmd, opts)
  if (verbose) {
    cli::cli_code(cmd)
  }
  try_cmd(cmd)
}
