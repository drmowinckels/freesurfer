test_that("handles successful command and new outfile", {
  test_cmd <- "mock_freesurfer_command"
  test_outfile <- file.path(tempdir(), "test_output.nii.gz")

  file_exists_counter <- 0
  mock_file_exists <- function(path) {
    file_exists_counter <<- file_exists_counter + 1
    if (path == test_outfile) {
      return(file_exists_counter > 1)
    }
    return(file_exists(path))
  }

  local_mocked_bindings(
    `fs_system` = function(command, ...) {
      expect_equal(command, test_cmd)
      return(0) # Simulate success
    },
    `check_fs_result` = function(res, fe_before, fe_after) {
      expect_equal(res, 0)
      expect_false(fe_before)
      expect_true(fe_after)
    },
    `file_exists` = mock_file_exists,
    .env = asNamespace("freesurfer"),
    {
      expect_message(
        {
          run_check_fs_cmd(
            cmd = test_cmd,
            outfile = test_outfile,
            verbose = TRUE
          )
        },
        regexp = test_cmd
      )
    }
  )
})

test_that("handles successful command with pre-existing outfile", {
  test_cmd <- "mock_freesurfer_command_existing"
  test_outfile <- file.path(tempdir(), "existing_output.nii.gz")

  file_exists_counter <- 0
  mock_file_exists <- function(path) {
    file_exists_counter <<- file_exists_counter + 1
    if (path == test_outfile) {
      return(TRUE)
    }
    return(file_exists(path))
  }

  local_mocked_bindings(
    `fs_system` = function(command, ...) {
      expect_equal(command, test_cmd)
      return(0) # Simulate success
    },
    `check_fs_result` = function(res, fe_before, fe_after) {
      expect_equal(res, 0)
      expect_true(fe_before)
      expect_true(fe_after)
    },
    `file_exists` = mock_file_exists,
    .env = asNamespace("freesurfer"),
    {
      expect_message(
        {
          run_check_fs_cmd(
            cmd = test_cmd,
            outfile = test_outfile,
            verbose = TRUE
          )
        },
        regexp = test_cmd
      )
    }
  )
})

test_that("run_check_fs_cmd handles failed command and no outfile created", {
  test_cmd <- "mock_freesurfer_command_fail"
  test_outfile <- file.path(tempdir(), "failed_output.nii.gz")
  mock_error_code <- 1 # Simulate a non-zero exit status

  # Mock file_exists to simulate file not existing before or after
  file_exists_counter <- 0
  mock_file_exists <- function(path) {
    file_exists_counter <<- file_exists_counter + 1
    if (path == test_outfile) {
      # Both calls should return FALSE
      return(FALSE)
    }
    return(file_exists(path))
  }

  local_mocked_bindings(
    `fs_system` = function(command, ...) {
      expect_equal(command, test_cmd)
      return(mock_error_code) # Simulate failure
    },
    `check_fs_result` = function(res, fe_before, fe_after) {
      expect_equal(res, mock_error_code)
      expect_false(fe_before)
      expect_false(fe_after)
    },
    `file_exists` = mock_file_exists,
    .env = asNamespace("freesurfer"),
    {
      expect_message(
        {
          run_check_fs_cmd(
            cmd = test_cmd,
            outfile = test_outfile,
            verbose = TRUE
          )
        },
        regexp = test_cmd
      )
    }
  )
})

test_that("run_check_fs_cmd works with verbose = FALSE", {
  test_cmd <- "mock_freesurfer_command_no_verbose"
  test_outfile <- file.path(tempdir(), "no_verbose_output.nii.gz")

  # Simple mock for file_exists (doesn't matter much for this test)
  mock_file_exists <- function(path) FALSE

  local_mocked_bindings(
    `fs_system` = function(command, ...) 0,
    `check_fs_result` = function(res, fe_before, fe_after) NULL,
    `file_exists` = mock_file_exists,
    .env = asNamespace("freesurfer"),
    {
      # Expect no message when verbose is FALSE
      expect_no_message({
        run_check_fs_cmd(
          cmd = test_cmd,
          outfile = test_outfile,
          verbose = FALSE
        )
      })
    }
  )
})
