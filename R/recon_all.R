#' @title Reconstruction from Freesurfer for All Steps
#' @description Reconstruction from Freesurfer for All Steps
#'
#' @param infile Input filename (dcm or nii)
#' @template outdir
#' @template subjid
#' @template verbose
#' @template opts
#' @param ... arguments passed to \code{\link{reconner}}
#'
#' @note If you would like to restart a \code{recon-all} run,
#' change opts so that \code{opts = "-make all"}
#' @return Result of \code{\link{system}}
#' @export
recon_all <- function(
  infile = NULL,
  outdir = NULL,
  subjid = NULL,
  verbose = get_fs_verbosity(),
  opts = "-all",
  ...
) {
  check_path(infile)
  reconner(
    infile = infile,
    outdir = outdir,
    subjid = subjid,
    verbose = verbose,
    opts = opts,
    ...
  )
}
