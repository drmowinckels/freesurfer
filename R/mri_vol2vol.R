#' Resample a Volume into Another Volume's Space with FreeSurfer
#'
#' @description
#' Calls FreeSurfer's `mri_vol2vol` to resample a "moving" volume onto the voxel
#' grid of a "target" volume, using either an explicit registration or the
#' volumes' own headers (`--regheader`).
#'
#' @details
#' Runtime FreeSurfer CLI help is available via the helper function
#' \code{mri_vol2vol.help()}. When called, that helper will attempt to fetch and
#' display the underlying FreeSurfer command-line help if FreeSurfer is
#' installed on the system.
#'
#' This is a thin wrapper around [fs_flag_cmd()]; anything not exposed as an
#' argument can be passed through `opts`.
#'
#' @param mov Character; the moving volume to resample (`--mov`).
#' @param targ Character; the target volume whose grid to resample onto
#'   (`--targ`).
#' @param outfile Character; output volume (`--o`). Defaults to a temporary
#'   `.nii.gz` file.
#' @param reg Character; a registration file (`.lta`/`.dat`) mapping `mov` to
#'   `targ` (`--reg`). Ignored when `regheader = TRUE`.
#' @param regheader Logical; use the volumes' headers to compute the
#'   registration (`--regheader`) instead of a `reg` file. Default `FALSE`.
#' @param interp Character; interpolation method (`--interp`): one of
#'   `"trilin"`, `"nearest"` or `"cubic"`.
#' @template opts
#' @param verbose Logical; print the assembled command before running it.
#' @param ... Additional arguments passed to [fs_flag_cmd()].
#'
#' @return The output filename, invisibly.
#'
#' @section FreeSurfer Command Help:
#' When FreeSurfer is installed and available, detailed command-line help for
#' the underlying `mri_vol2vol` command can be accessed via
#' `mri_vol2vol.help()`.
#'
#' @seealso [fs_flag_cmd()] for the underlying flag-based command wrapper;
#'   [mri_convert()] for format conversion.
#'
#' @name mri_vol2vol
#' @export
#'
#' @examplesIf have_fs()
#' \dontrun{
#' # Resample a parcellation into an aseg's space using the headers
#' mri_vol2vol(
#'   mov = "parcellation.nii.gz",
#'   targ = "aseg.mgz",
#'   outfile = "parcellation_in_aseg.nii.gz",
#'   regheader = TRUE,
#'   interp = "nearest"
#' )
#' }
mri_vol2vol <- function(
  mov,
  targ,
  outfile = NULL,
  reg = NULL,
  regheader = FALSE,
  interp = c("trilin", "nearest", "cubic"),
  opts = "",
  verbose = get_fs_verbosity(),
  ...
) {
  interp <- match.arg(interp)
  if (is.null(outfile)) {
    outfile <- temp_file(fileext = ".nii.gz")
  }
  if (!regheader && is.null(reg)) {
    cli::cli_abort(
      "Provide a registration via {.arg reg} or set {.arg regheader = TRUE}."
    )
  }

  fs_flag_cmd(
    func = "mri_vol2vol",
    args = list(
      mov = mov,
      targ = targ,
      reg = if (regheader) NULL else reg,
      regheader = regheader,
      interp = interp,
      o = outfile
    ),
    outfile = outfile,
    opts = opts,
    verbose = verbose,
    ...
  )
}

#' @describeIn mri_vol2vol Display FreeSurfer help for mri_vol2vol
#' @param ... Additional arguments passed to [fs_help()]
#' @export
mri_vol2vol.help <- function(...) {
  fs_help("mri_vol2vol", ...)
}
