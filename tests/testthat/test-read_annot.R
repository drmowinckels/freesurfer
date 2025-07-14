#' Helper function to create mock annotation files
#'  @param colortable character. Specifies the colortable type/version to include.
#'  Must be one of:
#'  - `"0"`: No colortable will bewritten.
#'  - `"2"`: A FreeSurfer version 1colortable (internal flag 1) will bewritten.
#'  Note: This is often referredto as 'version 2' in some contexts,
#'  but internally FreeSurferuses flag 1 for this older format.
#'  - `"3"`: A FreeSurfer version 2 colortable (internal flag -1) will bewritten.
#'  This is the more modernformat, often used for custom tables.
create_mock_annot_file <- function(
  file_path,
  num_vertices = 10,
  colortable = c(2, 3, 0)
) {
  colortable <- as.character(colortable)
  colortable <- match.arg(colortable, c("2", "3", "0"))
  ctab_flag <- switch(
    colortable,
    "0" = 0L,
    "2" = 1L,
    "3" = -1L
  )

  con <- file(file_path, "wb")
  on.exit(close(con))

  # --- Write Vertices Data ---
  writeBin(as.integer(num_vertices), con, endian = "big")

  # Write mock vertex-label pairs
  vertex_label_pairs <- as.integer(
    c(
      rbind(
        0:(num_vertices - 1),
        sample(0:4, num_vertices, replace = TRUE)
      )
    )
  )
  writeBin(vertex_label_pairs, con, endian = "big")

  # --- Write Colortable Flag and Data ---
  # Write the determined colortable flag to the file
  writeBin(ctab_flag, con, endian = "big")

  if (ctab_flag != 0) {
    if (ctab_flag == -1L) {
      orig_table_name <- "mock_original_table_v2_or_v3"
      writeBin(
        as.integer(nchar(orig_table_name)),
        con,
        endian = "big"
      )
      writeBin(charToRaw(orig_table_name), con)
    }

    # Write number of entries in the colortable
    num_entries <- 5 # Example number of entries
    writeBin(as.integer(num_entries), con, endian = "big")

    # Write mock colortable data for each entry
    for (i in 1:num_entries) {
      label_name <- paste0("Label_", i)
      writeBin(as.integer(i - 1), con, endian = "big")
      writeBin(
        as.integer(nchar(label_name)),
        con,
        endian = "big"
      )
      writeBin(charToRaw(label_name), con)

      # RGBA values
      writeBin(as.integer(sample(0:255, 1)), con, endian = "big")
      writeBin(as.integer(sample(0:255, 1)), con, endian = "big")
      writeBin(as.integer(sample(0:255, 1)), con, endian = "big")
      writeBin(as.integer(0), con, endian = "big")
    }
  }
}

test_that("throws an error if file does not exist", {
  nonexistent_path <- tempfile(fileext = ".annot")
  expect_error(
    read_annotation(nonexistent_path),
    "does not exist"
  )
})

test_that("handles valid annotation files with colortable", {
  # Setup: Create a mock annotation file
  annot_file <- tempfile(fileext = ".annot")
  create_mock_annot_file(annot_file)

  # Test: Read the mock annotation file
  result <- read_annotation(annot_file)

  # Assertions
  expect_named(result, c("vertices", "label", "colortable"))
  expect_length(result$vertices, 10)
  expect_length(result$label, 10)
  expect_true(is.data.frame(result$colortable))
  expect_equal(ncol(result$colortable), 6)

  unlink(annot_file) # Teardown
})

test_that("handles annotation files without colortable", {
  # Setup: Create a mock annotation file without colortable
  annot_file <- tempfile(fileext = ".annot")
  create_mock_annot_file(annot_file, colortable = 0)

  # Test: Read the mock annotation file
  result <- read_annotation(annot_file)

  # Assertions
  expect_named(result, c("vertices", "label", "colortable"))
  expect_length(result$vertices, 10)
  expect_length(result$label, 10)
  expect_true(is.data.frame(result$colortable))
  expect_equal(nrow(result$colortable), 0) # No colortable entries

  unlink(annot_file) # Teardown
})

test_that("handles annotation files with no vertices", {
  # Setup: Create a mock annotation file with no vertices
  annot_file <- tempfile(fileext = ".annot")
  create_mock_annot_file(annot_file, num_vertices = 0)

  # Test: Read the mock annotation file
  result <- read_annotation(annot_file, verbose = FALSE)

  # Assertions
  expect_named(result, c("vertices", "label", "colortable"))
  expect_equal(length(result$vertices), 0)
  expect_equal(length(result$label), 0)
  expect_true(is.data.frame(result$colortable))

  unlink(annot_file) # Teardown
})

test_that("handles missing colortable gracefully", {
  # Setup: Create a mock annotation file with an invalid colortable flag
  annot_file <- tempfile(fileext = ".annot")
  con <- file(annot_file, "wb")
  on.exit(close(con))

  writeBin(as.integer(10), con, endian = "big") # Mock vertex-label pairs
  writeBin(as.integer(rep(c(1, 2), 10)), con, endian = "big")
  writeBin(as.integer(0), con, endian = "big") # Invalid colortable flag

  # Test
  expect_error(
    read_annotation(annot_file),
    "Error! Should not be expecting bool == 0"
  )

  unlink(annot_file) # Teardown
})

test_that("ensures colortable fields are not NA", {
  # Setup: Create a mock annotation without proper colortable labels
  annot_file <- tempfile(fileext = ".annot")
  con <- file(annot_file, "wb")
  on.exit(close(con))

  # Regular content
  writeBin(as.integer(10), con, endian = "big")
  writeBin(as.integer(rep(c(1, 2), 10)), con, endian = "big")

  # Colortable flag
  writeBin(as.integer(1), con, endian = "big")

  writeBin(as.integer(5), con, endian = "big")
  for (i in 1:5) {
    # Missing or invalid labels
    writeBin(as.integer(i - 1), con, endian = "big")
    writeBin(as.integer(0), con, endian = "big") # label field is malformed
    writeBin(as.integer(sample(0:255, 1)), con, endian = "big") # R
    writeBin(as.integer(sample(0:255, 1)), con, endian = "big") # G
    writeBin(as.integer(sample(0:255, 1)), con, endian = "big") # B
    writeBin(as.integer(0), con, endian = "big") # A
  }

  result <- read_annotation(annot_file)

  # Assertions
  expect_false(any(is.na(result$colortable$label)))

  unlink(annot_file) # Teardown
})
