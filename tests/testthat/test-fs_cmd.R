# A simple mock for fs_imgext (you should replace with your actual function)
fs_imgext <- function() {
  ".mgz"
}

# Mock Nifti object for readnii
mock_nifti_image <- function(dims = c(2, 2, 2)) {
  img <- array(1:(prod(dims)), dim = dims)
  attr(img, "dim_") <- c(4, dims, rep(0, 3))
  class(img) <- "nifti"
  return(img)
}

# --- Tests for function ---

test_that("constructs correct command and returns system result when retimg=FALSE", {
  # Setup: Create temporary input and output files
  base_dir <- withr::local_tempdir()
  input_file <- withr::local_tempfile(
    tmpdir = base_dir,
    fileext = ".nii.gz"
  )
  output_file <- withr::local_tempfile(
    tmpdir = base_dir,
    fileext = ".nii.gz"
  )
  writeLines("dummy_input_content", input_file)

  # Prepare variables to capture mocked calls
  mock_cmd_captured <- NULL
  mock_intern_captured <- NULL
  mock_get_fs_bin_app_captured <- NULL

  # Mock external dependencies
  testthat::local_mocked_bindings(
    get_fs = function(bin_app = "bin") {
      mock_get_fs_bin_app_captured <<- bin_app
      "/mock/freesurfer/bin/"
    },
    fs_system = function(cmd, intern, ...) {
      mock_cmd_captured <<- cmd
      mock_intern_captured <<- intern
      "Mock system output success!"
    },
    checkimg = function(file, ...) file,
    check_outfile = function(outfile, ...) outfile,
    fs_imgext = function() ".mgz",
    .env = environment(fs_cmd)
  )

  # Test 1.1: No outfile, retimg=FALSE, default intern=FALSE
  result_no_outfile <- fs_cmd(
    "mri_deface",
    file = input_file,
    retimg = FALSE
  )
  expected_cmd_no_outfile <- sprintf(
    '/mock/freesurfer/bin mri_deface "%s";',
    input_file
  )

  expect_equal(mock_cmd_captured, expected_cmd_no_outfile)
  expect_false(mock_intern_captured)
  expect_equal(result_no_outfile, "Mock system output success!")
  expect_equal(mock_get_fs_bin_app_captured, "bin") # Default bin_app

  # Test 1.2: With outfile, retimg=FALSE, default intern=FALSE
  mock_cmd_captured <- NULL # Reset for next call
  mock_intern_captured <- NULL
  result_with_outfile <- fs_cmd(
    "mri_volumize",
    file = input_file,
    outfile = output_file,
    retimg = FALSE
  )
  expected_cmd_with_outfile <- sprintf(
    '/mock/freesurfer/bin mri_volumize "%s" "%s.mgz";',
    input_file,
    tools::file_path_sans_ext(output_file)
  )

  expect_equal(mock_cmd_captured, expected_cmd_with_outfile)
  expect_false(mock_intern_captured)
  expect_equal(result_with_outfile, "Mock system output success!")

  # Test 1.3: With intern=TRUE
  mock_cmd_captured <- NULL
  mock_intern_captured <- NULL
  result_intern_true <- fs_cmd(
    "mri_info",
    file = input_file,
    intern = TRUE,
    retimg = FALSE
  )
  expected_cmd_intern_true <- sprintf(
    '/mock/freesurfer/bin mri_info "%s";',
    input_file
  )

  expect_equal(mock_cmd_captured, expected_cmd_intern_true)
  expect_true(mock_intern_captured)
  expect_equal(result_intern_true, "Mock system output success!")
})

test_that("handles retimg=TRUE and reorient parameters", {
  base_dir <- withr::local_tempdir()
  input_file <- withr::local_tempfile(
    tmpdir = base_dir,
    fileext = ".nii.gz"
  )
  output_file <- withr::local_tempfile(
    tmpdir = base_dir,
    fileext = ".nii.gz"
  )
  writeLines("dummy_input_content", input_file)

  mock_readnii_file_captured <- NULL
  mock_readnii_reorient_captured <- NULL

  testthat::local_mocked_bindings(
    get_fs = function(...) "/mock/freesurfer/bin",
    fs_system = function(...) "", # Return empty string as output not used when retimg=TRUE
    checkimg = function(file, ...) file,
    check_outfile = function(outfile, ...) outfile,
    nii.stub = function(x) tools::file_path_sans_ext(x), # Mock nii.stub to remove extension
    readnii = function(file, reorient) {
      mock_readnii_file_captured <<- file
      mock_readnii_reorient_captured <<- reorient
      mock_nifti_image() # Return a mock nifti object
    },
    fs_imgext = function() ".mgz",
    .env = environment(fs_cmd)
  )

  # Test 2.1: retimg=TRUE (default), reorient=FALSE (default)
  result_retimg_default <- fs_cmd(
    "mri_binarize",
    file = input_file,
    outfile = output_file,
    retimg = TRUE
  )
  expect_s3_class(result_retimg_default, "nifti")
  expect_equal(
    mock_readnii_file_captured,
    paste0(tools::file_path_sans_ext(output_file), ".mgz")
  )
  expect_false(mock_readnii_reorient_captured)

  # Test 2.2: retimg=TRUE, reorient=TRUE
  mock_readnii_file_captured <- NULL # Reset for next call
  mock_readnii_reorient_captured <- NULL
  result_retimg_reorient <- fs_cmd(
    "mri_binarize",
    file = input_file,
    outfile = output_file,
    retimg = TRUE,
    reorient = TRUE
  )
  expect_s3_class(result_retimg_reorient, "nifti")
  expect_equal(
    mock_readnii_file_captured,
    paste0(tools::file_path_sans_ext(output_file), ".mgz")
  )
  expect_true(mock_readnii_reorient_captured)
})

test_that("handles samefile parameter correctly", {
  base_dir <- withr::local_tempdir()
  input_file <- withr::local_tempfile(
    tmpdir = base_dir,
    fileext = ".nii.gz"
  )
  writeLines("dummy_input_content", input_file)

  mock_cmd_captured <- NULL
  mock_readnii_file_captured <- NULL

  testthat::local_mocked_bindings(
    get_fs = function(...) "/mock/freesurfer/bin",
    fs_system = function(cmd, ...) {
      mock_cmd_captured <<- cmd
      ""
    },
    checkimg = function(file, ...) file,
    check_outfile = function(outfile, ...) outfile,
    nii.stub = function(x) tools::file_path_sans_ext(x),
    readnii = function(file, reorient) {
      mock_readnii_file_captured <<- file
      mock_nifti_image()
    },
    fs_imgext = function() ".mgz",
    .env = environment(fs_cmd)
  )

  # Test 3.1: samefile=TRUE, no outfile, retimg=TRUE
  result_samefile_retimg <- fs_cmd(
    "mri_convert",
    file = input_file,
    samefile = TRUE,
    retimg = TRUE
  )
  # Command should NOT contain an explicit outfile path
  expected_cmd_samefile_no_outfile <- sprintf(
    '/mock/freesurfer/bin mri_convert "%s" ;',
    input_file
  )
  expect_equal(mock_cmd_captured, expected_cmd_samefile_no_outfile)
  # readnii should read from the original input file
  expect_equal(mock_readnii_file_captured, input_file)
  expect_s3_class(result_samefile_retimg, "nifti")

  # Test 3.2: samefile=TRUE, no outfile, retimg=FALSE
  mock_cmd_captured <- NULL
  result_samefile_no_retimg <- fs_cmd(
    "mri_convert",
    file = input_file,
    samefile = TRUE,
    retimg = FALSE
  )
  # Command should still NOT contain an explicit outfile path
  expect_equal(mock_cmd_captured, expected_cmd_samefile_no_outfile)
  expect_equal(result_samefile_no_retimg, "") # Returns system output
})

test_that("correctly incorporates opts and frontopts", {
  base_dir <- withr::local_tempdir()
  input_file <- withr::local_tempfile(
    tmpdir = base_dir,
    fileext = ".nii.gz"
  )
  output_file <- withr::local_tempfile(
    tmpdir = base_dir,
    fileext = ".nii.gz"
  )
  writeLines("dummy_input_content", input_file)

  mock_cmd_captured <- NULL

  testthat::local_mocked_bindings(
    get_fs = function(...) "/mock/freesurfer/bin",
    fs_system = function(cmd, ...) {
      mock_cmd_captured <<- cmd
      ""
    },
    checkimg = function(file, ...) file,
    check_outfile = function(outfile, ...) outfile,
    nii.stub = function(x) tools::file_path_sans_ext(x),
    fs_imgext = function() ".mgz",
    .env = environment(fs_cmd)
  )

  # Test 4.1: opts parameter (default opts_after_outfile=FALSE)
  fs_cmd(
    "mri_convert",
    file = input_file,
    outfile = output_file,
    opts = "-clobber -rt_float",
    retimg = FALSE
  )
  expected_cmd_opts <- sprintf(
    '/mock/freesurfer/bin mri_convert "%s"  -clobber -rt_float "%s.mgz";',
    input_file,
    tools::file_path_sans_ext(output_file)
  )
  expect_equal(mock_cmd_captured, expected_cmd_opts)

  # Test 4.2: opts_after_outfile=TRUE
  mock_cmd_captured <- NULL
  fs_cmd(
    "mri_convert",
    file = input_file,
    outfile = output_file,
    opts = "-no-resample",
    opts_after_outfile = TRUE,
    retimg = FALSE
  )
  expected_cmd_opts_after <- sprintf(
    '/mock/freesurfer/bin mri_convert "%s" "%s.mgz" -no-resample;',
    input_file,
    tools::file_path_sans_ext(output_file)
  )
  expect_equal(mock_cmd_captured, expected_cmd_opts_after)

  # Test 4.3: frontopts parameter
  mock_cmd_captured <- NULL
  fs_cmd(
    "mri_segstats",
    file = input_file,
    frontopts = "-seg brain.mgz",
    retimg = FALSE
  )
  expected_cmd_frontopts <- sprintf(
    '/mock/freesurfer/bin mri_segstats -seg brain.mgz "%s";',
    input_file
  )
  expect_equal(mock_cmd_captured, expected_cmd_frontopts)
})

test_that("handles add_ext and bin_app parameters", {
  base_dir <- withr::local_tempdir()
  input_file <- withr::local_tempfile(
    tmpdir = base_dir,
    fileext = ".nii.gz"
  )
  writeLines("dummy_input_content", input_file)

  mock_cmd_captured <- NULL
  mock_get_fs_bin_app_captured <- NULL

  testthat::local_mocked_bindings(
    get_fs = function(bin_app = "bin") {
      mock_get_fs_bin_app_captured <<- bin_app
      "/mock/freesurfer/custom_bin_path"
    },
    fs_system = function(cmd, ...) {
      mock_cmd_captured <<- cmd
      ""
    },
    checkimg = function(file, ...) file,
    check_outfile = function(outfile, ...) outfile,
    nii.stub = function(x) tools::file_path_sans_ext(x),
    fs_imgext = function() ".mgz",
    .env = environment(fs_cmd)
  )

  # Test 5.1: add_ext=FALSE
  fs_cmd(
    "mri_binarize",
    file = input_file,
    outfile = "custom_output_no_ext",
    add_ext = FALSE,
    retimg = FALSE
  )
  expected_cmd_no_ext <- sprintf(
    '/mock/freesurfer/custom_bin_path mri_binarize "%s" "custom_output_no_ext";',
    input_file
  )
  expect_equal(mock_cmd_captured, expected_cmd_no_ext)

  # Test 5.2: bin_app parameter
  mock_cmd_captured <- NULL
  fs_cmd(
    "mri_binarize",
    file = input_file,
    bin_app = "custom_apps",
    retimg = FALSE
  )
  # The actual command should use the new bin_app in the path returned by get_fs
  expect_equal(mock_get_fs_bin_app_captured, "custom_apps")
  # The command constructed by would still use the *mocked* get_fs path
  expected_cmd_bin_app <- sprintf(
    '/mock/freesurfer/custom_bin_path mri_binarize "%s";',
    input_file
  )
  expect_equal(mock_cmd_captured, expected_cmd_bin_app)
})

test_that("handles verbose output", {
  base_dir <- withr::local_tempdir()
  input_file <- withr::local_tempfile(
    tmpdir = base_dir,
    fileext = ".nii.gz"
  )
  writeLines("dummy_input_content", input_file)

  mock_cmd_captured <- NULL

  testthat::local_mocked_bindings(
    get_fs = function(...) "/mock/freesurfer/bin",
    fs_system = function(cmd, ...) {
      mock_cmd_captured <<- cmd
      ""
    },
    checkimg = function(file, ...) file,
    .env = environment(fs_cmd)
  )

  # Test 6.1: verbose=TRUE (default) - should print message
  expect_message(
    {
      fs_cmd("mri_deface", file = input_file, retimg = FALSE)
    },
    regexp = sprintf('/mock/freesurfer/bin mri_deface "%s";\n', input_file)
  )

  # Test 6.2: verbose=FALSE - should not print message
  expect_no_message({
    fs_cmd("mri_deface", file = input_file, verbose = FALSE, retimg = FALSE)
  })
})

test_that("passes additional arguments to system", {
  base_dir <- withr::local_tempdir()
  input_file <- withr::local_tempfile(
    tmpdir = base_dir,
    fileext = ".nii.gz"
  )
  writeLines("dummy_input_content", input_file)

  mock_system_args_captured <- list()

  testthat::local_mocked_bindings(
    get_fs = function(...) "/mock/freesurfer/bin",
    fs_system = function(cmd, intern, ...) {
      mock_system_args_captured <<- list(
        cmd = cmd,
        intern = intern,
        dots = list(...)
      )
      ""
    },
    checkimg = function(file, ...) file,
    .env = environment(fs_cmd)
  )

  fs_cmd(
    "mri_deface",
    file = input_file,
    retimg = FALSE,
    timeout = 10,
    wait = TRUE
  )

  expect_equal(mock_system_args_captured$dots$timeout, 10)
  expect_true(mock_system_args_captured$dots$wait)
})


# --- Integration Tests for fs_cmd ---

test_that("runs actual fs commands", {
  skip_if_no_freesurfer()

  # Setup: Create a dummy input file (mri_info often expects one, even with --version)
  test_file <- file.path(fs_subj_dir(), "bert", "mri", "wm.mgz")
  tmpout <- withr::local_tempfile()

  result <- fs_cmd(
    func = "mri_info",
    file = test_file,
    outfile = tmpout,
    opts = "--version",
    retimg = FALSE, # We want raw system output
    intern = TRUE # We want to capture the output
  )

  # Assertions: Check the output from the real FreeSurfer command
  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_true(
    any(grepl("FreeSurfer version", result, ignore.case = TRUE)),
    info = "Expected 'FreeSurfer version' string in output from mri_info --version."
  )
})


test_that("fs_cmd integration: basic image binarization with real FreeSurfer file", {
  skip_if_no_freesurfer()

  # Setup: Use the found FreeSurfer sample image as input
  input_nifti_path <- fs_env_info$sample_img

  # Create a temporary output file managed by withr
  temp_dir <- withr::local_tempdir(check = TRUE)
  # fs_cmd will likely add .mgz or .nii.gz, so create without for now
  output_nifti_base <- withr::local_tempfile(tmpdir = temp_dir, check = TRUE)

  # Call fs_cmd to run mri_binarize (NO MOCKING)
  result_img <- fs_cmd(
    func = "mri_binarize",
    file = input_nifti_path,
    outfile = output_nifti_base,
    opts = "--min 1 --bin 1", # Simple binarization: values >= 1 become 1
    retimg = TRUE, # Expect a nifti object back from readnii
    intern = FALSE # Output to console, but we're checking returned image
  )

  # Assertions:
  # 1. Check the type of the returned object (should be from neurobase::readnii)
  expect_s3_class(result_img, "nifti")
  expect_true(inherits(result_img, "nifti"))

  # 2. Check if the output file actually exists on disk (as a side effect)
  # fs_cmd should add the default extension (.mgz or .nii.gz)
  # So, construct the expected path by adding the extension determined by fs_imgext()
  expected_output_path_on_disk <- paste0(
    tools::file_path_sans_ext(output_nifti_base),
    fs_imgext()
  )
  expect_true(
    file.exists(expected_output_path_on_disk),
    info = paste(
      "Expected output file not found at:",
      expected_output_path_on_disk
    )
  )

  # 3. Optionally, check some basic properties of the binarized image
  # This relies on neurobase::readnii working correctly and mri_binarize producing valid output.
  expect_true(all(dim(result_img) > 0)) # Image should have dimensions
  # A more robust check might involve comparing checksums or specific value ranges if known,
  # but 'all(result_img %in% c(0, 1))' is a good basic check for binarized output.
  # Be cautious with exact pixel value checks as different FreeSurfer versions might vary slightly.
  expect_true(
    all(result_img %in% c(0, 1)),
    info = "Binarized image should only contain 0s or 1s."
  )
})
