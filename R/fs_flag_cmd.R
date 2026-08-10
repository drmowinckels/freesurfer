#' Run a Flag-Based FreeSurfer Command
#'
#' @description
#' Wrapper for FreeSurfer command-line tools that take `--flag value` style
#' arguments (for example `mri_vol2vol` or `mri_surf2surf`). This is the
#' flag-based counterpart to [fs_cmd()], which handles commands that take
#' positional `<input> <output>` arguments. The command is assembled from a
#' named list of flags and run with the FreeSurfer environment set up via
#' [get_fs()], then the expected `outfile` is checked with [run_check_fs_cmd()].
#'
#' @details
#' Each element of `args` becomes a `--<name> <value>` flag, in the order given.
#' A logical `TRUE` value produces a bare boolean flag (`--<name>`); a `NULL` or
#' `FALSE` value is dropped. Non-boolean values are quoted with
#' [base::shQuote()], so paths containing spaces are handled. Multi-token flag
#' values (for example a coordinate range) are better passed through `opts`.
#'
#' @param func Character; the FreeSurfer command to run, e.g. `"mri_vol2vol"`.
#' @param args Named list of command flags (see Details). Order is preserved.
#' @param outfile Character; the file the command is expected to create. It is
#'   checked after the command runs; supply it even when it is also one of the
#'   `args` (for example the `--o` flag).
#' @template opts
#' @param subj_dir Character; optional `SUBJECTS_DIR` to export for the command.
#'   The previous value is restored on exit.
#' @param bin_app Character; FreeSurfer binary sub-directory, passed to
#'   [get_fs()].
#' @param verbose Logical; print the assembled command before running it.
#' @param ... Additional arguments passed to [run_check_fs_cmd()] (for example
#'   `timeout_seconds`).
#'
#' @return The `outfile`, invisibly.
#'
#' @seealso [fs_cmd()] for positional-argument commands; [mri_vol2vol()] and
#'   [mri_surf2surf()] for wrappers built on this helper.
#'
#' @export
#'
#' @examplesIf have_fs()
#' \dontrun{
#' out <- temp_file(fileext = ".nii.gz")
#' fs_flag_cmd(
#'   "mri_vol2vol",
#'   args = list(
#'     mov = "mov.nii.gz",
#'     targ = "targ.mgz",
#'     regheader = TRUE,
#'     interp = "nearest",
#'     o = out
#'   ),
#'   outfile = out
#' )
#' }
fs_flag_cmd <- function(
  func,
  args = list(),
  outfile = NULL,
  opts = "",
  subj_dir = NULL,
  bin_app = c("bin", "mni/bin"),
  verbose = get_fs_verbosity(),
  ...
) {
  bin_app <- match.arg(bin_app)
  if (!is.list(args)) {
    cli::cli_abort("{.arg args} must be a named list of command flags.")
  }
  if (length(args) > 0 && is.null(names(args))) {
    cli::cli_abort("{.arg args} must be named; each name becomes a `--flag`.")
  }

  flags <- character(0)
  for (nm in names(args)) {
    val <- args[[nm]]
    drop <- is.null(val) || (is.logical(val) && !isTRUE(val))
    if (drop) {
      next
    }
    if (isTRUE(val)) {
      flags <- c(flags, paste0("--", nm))
    } else {
      flags <- c(flags, paste0("--", nm, " ", shQuote(val)))
    }
  }

  cmd_pre <- ""
  if (!is.null(subj_dir)) {
    orig_subj_dir <- Sys.getenv("SUBJECTS_DIR")
    on.exit(Sys.setenv(SUBJECTS_DIR = orig_subj_dir), add = TRUE)
    subj_dir <- path.expand(subj_dir)
    cmd_pre <- sprintf("export SUBJECTS_DIR=%s; ", shQuote(subj_dir))
  }

  cmd <- paste0(cmd_pre, get_fs(bin_app = bin_app), func)
  cmd <- paste(c(cmd, flags, opts), collapse = " ")
  cmd <- trimws(cmd)

  run_check_fs_cmd(
    cmd = cmd,
    outfile = outfile,
    verbose = verbose,
    func_name = func,
    ...
  )

  invisible(outfile)
}
