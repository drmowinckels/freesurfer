#' Read Freesurfer annotation file
#'
#' Reads Freesurfer binary annotation files
#' that contain information on vertex labels
#' and colours for use in analyses and
#' brain area lookups.
#'
#' This function is heavily
#' based on Freesurfer's read_annotation.m
#' Original Author: Bruce Fischl
#' CVS Revision Info:
#'     $Author: greve $
#'     $Date: 2014/02/25 19:54:10 $
#'     $Revision: 1.10 $
#'
#' @param path path to annotation file, usually with extension \code{annot}
#' @param verbose logical.
#'
#' @return list of 3 with vertices, labels, and colortable
#' @export
#' @examples
#' if (freesurfer::have_fs()) {
#'     bert_dir = file.path(fs_subj_dir(), "bert")
#'     annot_file = file.path(bert_dir, "label", "lh.aparc.annot")
#'     res = read_annotation(annot_file)
#' }
read_annotation <- function(path, verbose = TRUE) {
  if (!file_exists(path)) {
    stop("File ", path, " does not exist", call. = FALSE)
  }

  ff <- file(path, "rb")
  on.exit(close(ff))

  annot <- readBin(ff, integer(), endian = "big")
  tmp <- readBin(ff, integer(), n = 2 * annot, endian = "big")

  vertices <- tmp[seq(1, by = 2, length.out = length(tmp) / 2)]
  label <- tmp[seq(2, by = 2, length.out = length(tmp) / 2)]

  ctab_flag <- readBin(ff, integer(), endian = "big")

  # Initialize variables for common colortable logic
  numEntriesToRead <- 0L
  colortable.orig_tab <- NA_character_

  # Initialize an empty colortable data frame.
  colortable <- data.frame(matrix(NA, ncol = 6, nrow = 0))
  names(colortable) <- c("label", "R", "G", "B", "A", "code")

  if (length(ctab_flag) == 0 || is.na(ctab_flag) || ctab_flag == 0) {
    if (verbose) message('No colortable in file.\n')
  } else if (ctab_flag == 1L) {
    if (verbose) {
      message('Reading old-style (version 1) colortable (flag 1).\n')
    }
    numEntriesToRead <- readBin(ff, integer(), endian = "big")
  } else if (ctab_flag == -1L) {
    if (verbose) {
      message('Reading version 2 colortable (flag -1).\n')
    }

    orig_tab_name_len <- readBin(ff, integer(), endian = "big")
    colortable.orig_tab <- rawToChar(readBin(
      ff,
      raw(),
      n = orig_tab_name_len,
      endian = "big"
    ))

    numEntriesToRead <- readBin(ff, integer(), endian = "big")
  } else {
    # Case: Unexpected flag value - stop execution
    stop(
      'Error! Unexpected colortable flag: ',
      ctab_flag
    )
  }

  if (numEntriesToRead > 0) {
    # FreeSurfer usually means numEntries as the total number of labels (0 to numEntries-1).
    colortable <- data.frame(matrix(NA, ncol = 6, nrow = numEntriesToRead))
    names(colortable) <- c("label", "R", "G", "B", "A", "code")

    for (i in 1:numEntriesToRead) {
      structure_idx <- readBin(ff, integer(), endian = "big")

      if (structure_idx < 0 && verbose) {
        message(paste(
          'Warning! Read entry, index',
          structure_idx,
          'is negative.\n'
        ))
      }

      len <- readBin(ff, integer(), endian = "big")
      label_name <- rawToChar(readBin(ff, raw(), n = len, endian = "big"))

      if (structure_idx + 1 > nrow(colortable)) {
        stop(
          "Error: Label index (",
          structure_idx,
          ") exceeds expected colortable size (",
          nrow(colortable),
          "). File may be malformed or sparse labels not handled correctly.",
          call. = FALSE
        )
      }

      colortable$label[structure_idx + 1] <- label_name
      colortable$R[structure_idx + 1] <- readBin(ff, integer(), endian = "big")
      colortable$G[structure_idx + 1] <- readBin(ff, integer(), endian = "big")
      colortable$B[structure_idx + 1] <- readBin(ff, integer(), endian = "big")
      colortable$A[structure_idx + 1] <- readBin(ff, integer(), endian = "big")

      colortable$code[structure_idx + 1] <- colortable$R[structure_idx + 1] +
        colortable$G[structure_idx + 1] * 2^8 +
        colortable$B[structure_idx + 1] * 2^16
    }

    if (verbose) {
      msg <- paste0('Colortable with ', numEntriesToRead, ' entries read.')
      if (!is.na(colortable.orig_tab)) {
        msg <- paste0(msg, ' (originally ', colortable.orig_tab, ')')
      }
      message(msg, '\n')
    }
  }

  if (any(is.na(colortable$label))) {
    colortable$label[is.na(colortable$label)] = ""
  }

  return(list(
    vertices = vertices,
    label = label,
    colortable = colortable
  ))
}
