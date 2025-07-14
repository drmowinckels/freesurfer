test_that("fs_help returns correct help output for a valid Freesurfer function", {
  # Setup: Mock get_fs and fs_system to simulate Freesurfer behavior
  testthat::local_mocked_bindings(
    get_fs = function(...) "/mock/freesurfer/",
    fs_system = function(cmd, intern) {
      expect_equal(cmd, "/mock/freesurfer/mri_watershed --help ")
      expect_true(intern)
      return(c(
        "Usage: mri_watershed [options]",
        "Example: mri_watershed input.mgz output.mgz"
      ))
    }
  )

  # Test: Simulate valid input and check output
  result <- fs_help(func_name = "mri_watershed")
  expected_output <- c(
    "Usage: mri_watershed [options]",
    "Example: mri_watershed input.mgz output.mgz"
  )
  expect_equal(result, expected_output)
})

test_that("fs_help supports extra arguments", {
  # Setup: Mock get_fs and fs_system to simulate passing extra arguments
  testthat::local_mocked_bindings(
    get_fs = function(...) "/mock/freesurfer/",
    fs_system = function(cmd, intern) {
      expect_equal(cmd, "/mock/freesurfer/mri_convert --help --debug")
      expect_true(intern)
      return(c("Usage: mri_convert [options]", "--debug: Enable debug mode"))
    }
  )

  # Test: Ensure extra arguments are passed correctly
  result <- fs_help(func_name = "mri_convert", extra.args = "--debug")
  expected_output <- c(
    "Usage: mri_convert [options]",
    "--debug: Enable debug mode"
  )
  expect_equal(result, expected_output)
})

test_that("fs_help handles empty output when no help is available", {
  # Setup: Mock get_fs and fs_system to simulate no output
  testthat::local_mocked_bindings(
    get_fs = function(...) "/mock/freesurfer/",
    fs_system = function(cmd, intern) {
      return(character(0)) # Simulates no output from command
    }
  )

  # Test: Ensure empty output is handled gracefully
  result <- fs_help(func_name = "unknown_function")
  expect_equal(result, character(0))
})

test_that("fs_help prints output and returns it invisibly", {
  # Mock get_fs, fs_system, and message
  testthat::local_mocked_bindings(
    get_fs = function(...) "/mock/freesurfer/",
    fs_system = function(cmd, intern) {
      return(c("Usage: mri_info [options]", "Example: mri_info input.mgz"))
    }
  )

  # Test: Ensure output is both printed and returned as invisible
  expect_message(
    res <- fs_help(func_name = "mri_info"),
    "mri_info"
  )
  expect_equal(
    res,
    c("Usage: mri_info [options]", "Example: mri_info input.mgz")
  )
})

test_that("fs_help handles missing Freesurfer path", {
  # Setup: Mock get_fs to simulate missing path
  testthat::local_mocked_bindings(
    get_fs = function(...) "",
    fs_system = function(cmd, intern) {
      expect_equal(cmd, "mri_annotation2label --help --verbose")
      return(c("Usage: mri_annotation2label [options]", "Verbose mode enabled"))
    }
  )

  # Test: Ensure command works without Freesurfer base path
  result <- fs_help(
    func_name = "mri_annotation2label",
    extra.args = "--verbose"
  )
  expected_output <- c(
    "Usage: mri_annotation2label [options]",
    "Verbose mode enabled"
  )
  expect_equal(result, expected_output)
})

test_that("fs_help constructs correct command using custom Freesurfer path", {
  # Setup: Mock get_fs to return a custom path
  testthat::local_mocked_bindings(
    get_fs = function(...) "/custom/path/to/freesurfer/",
    fs_system = function(cmd, intern) {
      expect_equal(
        cmd,
        "/custom/path/to/freesurfer/mri_vol2surf --help --verbose"
      )
      expect_true(intern)
      return(c("Usage: mri_vol2surf [options]", "Details about usage"))
    }
  )

  # Test: Ensure the correct command is constructed with path and arguments
  result <- fs_help(func_name = "mri_vol2surf", extra.args = "--verbose")
  expected_output <- c(
    "Usage: mri_vol2surf [options]",
    "Details about usage"
  )
  expect_equal(result, expected_output)
})

test_that("fs_help does not crash with invalid fs_system response", {
  # Setup: Mock fs_system to simulate an error in command execution
  testthat::local_mocked_bindings(
    get_fs = function(...) "/mock/freesurfer/",
    fs_system = function(cmd, intern) {
      stop("Command not found")
    }
  )

  # Test: Ensure fs_help captures the error gracefully
  expect_error(
    result <- fs_help(func_name = "invalid_command"),
    "Command not found"
  )
})
