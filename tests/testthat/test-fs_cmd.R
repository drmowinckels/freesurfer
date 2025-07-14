test_that("fs_cmd constructs command and returns 'image' (retimg=TRUE)", {
  input_file <- "/path/to/input.nii.gz"
  output_file <- "/path/to/output.nii.gz"
  func_name <- "mri_info"
  opts_string <- "-voxvol"
  mock_fs_home <- "fs_home_mock/bin/"

  expected_cmd_pattern <- paste0(
    "^",
    mock_fs_home,
    func_name,
    ' "',
    input_file,
    '" ',
    opts_string,
    ' "',
    output_file,
    '";$'
  )

  # A simple mock return value for 'readnii'
  mock_readnii_result <- "mock_nifti_data"

  local_mocked_bindings(
    `fs_system` = function(command, intern, ...) {
      expect_match(command, expected_cmd_pattern)
      expect_false(intern)
      return(0)
    },
    `get_fs` = function(bin_app) {
      expect_equal(bin_app, "bin")
      return(mock_fs_home)
    },
    `fs_imgext` = function() ".nii.gz",
    # Mocks for neurobase functions
    `neurobase::checkimg` = function(file, ...) {
      expect_equal(file, input_file)
      return(file) # Always return the input file name
    },
    `neurobase::check_outfile` = function(outfile, retimg, fileext) {
      expect_equal(outfile, output_file)
      expect_true(retimg)
      expect_equal(fileext, ".nii.gz")
      return(outfile) # Always return the output file name
    },
    `neurobase::nii.stub` = function(x, ...) {
      # Mock nii.stub to return the base name without extension
      gsub("\\.nii\\.gz$|\\.nii$|\\.img$|\\.hdr$|\\.mgz$", "", x)
    },
    `neurobase::readnii` = function(file, reorient) {
      expect_equal(file, output_file)
      expect_false(reorient)
      return(mock_readnii_result) # Return our simple mock data
    },
    .env = asNamespace("freesurfer"),
    {
      suppressMessages({
        result <- fs_cmd(
          func = func_name,
          file = input_file,
          outfile = output_file,
          opts = opts_string,
          retimg = TRUE,
          verbose = TRUE
        )
      })
      # Assert that the function returned the mocked data from readnii
      expect_equal(result, mock_readnii_result)
    }
  )
})

test_that("fs_cmd returns system result (retimg=FALSE)", {
  input_file <- "/path/to/subject"
  func_name <- "recon-all"
  opts_string <- "-all"
  mock_fs_home <- "fs_home_mock/bin/"
  mock_return_code <- 0

  expected_cmd_pattern <- paste0(
    "^",
    mock_fs_home,
    func_name,
    ' "',
    input_file,
    '" ',
    opts_string,
    ";$"
  )

  local_mocked_bindings(
    `fs_system` = function(command, intern, ...) {
      expect_match(command, expected_cmd_pattern)
      expect_true(intern)
      return(mock_return_code)
    },
    `get_fs` = function(...) mock_fs_home,
    `fs_imgext` = function(...) ".nii.gz",
    # Mocks for neurobase functions (still needed as they are called)
    `neurobase::checkimg` = function(file, ...) file,
    `neurobase::check_outfile` = function(outfile, retimg, fileext) outfile,
    `neurobase::nii.stub` = function(x, ...) x, # Not strictly used here, but good to keep consistent
    .env = asNamespace("freesurfer"),
    {
      suppressMessages({
        result <- fs_cmd(
          func = func_name,
          file = input_file,
          outfile = NULL,
          opts = opts_string,
          retimg = FALSE,
          intern = TRUE,
          verbose = TRUE
        )
      })
      expect_equal(result, mock_return_code)
    }
  )
})

test_that("fs_cmd handles 'samefile = TRUE'", {
  input_file <- "/path/to/volume.mgz"
  func_name <- "mris_calc"
  opts_string <- "-add 1"
  mock_fs_home <- "fs_home_mock/bin/"

  expected_cmd_pattern <- paste0(
    "^",
    mock_fs_home,
    func_name,
    ' "',
    input_file,
    '" ',
    opts_string,
    ";"
  )
  mock_readnii_result <- "mock_samefile_image_data"

  local_mocked_bindings(
    `fs_system` = function(command, intern, ...) {
      expect_match(command, expected_cmd_pattern)
      return(0)
    },
    `get_fs` = function(...) mock_fs_home,
    `fs_imgext` = function(...) ".mgz",
    `neurobase::readnii` = function(file, reorient) {
      expect_equal(file, input_file) # IMPORTANT: readnii should be called with input_file
      return(mock_readnii_result)
    },
    `neurobase::checkimg` = function(file, ...) file,
    `neurobase::check_outfile` = function(outfile, retimg, fileext) {
      expect_equal(outfile, "") # When samefile=TRUE and outfile=NULL
      return("")
    },
    `neurobase::nii.stub` = function(x, ...) x,
    .env = asNamespace("freesurfer"),
    {
      suppressMessages({
        result <- fs_cmd(
          func = func_name,
          file = input_file,
          outfile = NULL,
          opts = opts_string,
          samefile = TRUE,
          retimg = TRUE,
          verbose = TRUE
        )
      })
      expect_equal(result, mock_readnii_result)
    }
  )
})

test_that("fs_cmd handles 'opts_after_outfile = TRUE'", {
  input_file <- "/input.nii"
  output_file <- "/output.nii"
  func_name <- "mri_convert"
  opts_string <- "-odt float"
  mock_fs_home <- "fs_home_mock/bin/"

  expected_cmd_pattern <- paste0(
    "^",
    mock_fs_home,
    func_name,
    ' "',
    input_file,
    '" "',
    output_file,
    '" ',
    opts_string,
    ";"
  )
  mock_readnii_result <- "mock_reordered_image"

  local_mocked_bindings(
    `fs_system` = function(command, intern, ...) {
      expect_match(command, expected_cmd_pattern)
      return(0)
    },
    `get_fs` = function(...) mock_fs_home,
    `fs_imgext` = function(...) ".nii",
    `neurobase::readnii` = function(file, reorient) return(mock_readnii_result),
    `neurobase::checkimg` = function(file, ...) file,
    `neurobase::check_outfile` = function(outfile, retimg, fileext) outfile,
    `neurobase::nii.stub` = function(x, ...) x,
    .env = asNamespace("freesurfer"),
    {
      suppressMessages({
        result <- fs_cmd(
          func = func_name,
          file = input_file,
          outfile = output_file,
          opts = opts_string,
          opts_after_outfile = TRUE,
          retimg = TRUE,
          verbose = TRUE
        )
      })
      expect_equal(result, mock_readnii_result)
    }
  )
})
