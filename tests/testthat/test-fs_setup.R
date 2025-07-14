# --- Tests for get_fs() ---

test_that("get_fs handles FREESURFER_HOME env var", {
  withr::with_tempdir({
    current_test_fs_home <- file.path(getwd(), "test_fs_home_env_isolated")
    mock_freesurfer_env(base_path = current_test_fs_home)

    # Test with existing FREESURFER_HOME set via withr::with_envvar
    withr::with_envvar(
      new = c(FREESURFER_HOME = current_test_fs_home),
      code = {
        testthat::with_mock(
          `system` = function(command, ...) {
            if (
              grepl("export FREESURFER_HOME=", command) &&
                grepl("source", command)
            ) {
              return(0) # Simulate successful source
            }
            stop("Unexpected system call in test: ", command)
          },
          .local = TRUE,
          {
            cmd <- get_fs()
            expect_true(is.null(cmd) || cmd == "")
          }
        )
      }
    )

    # Test with FREESURFER_HOME set but non-existent
    withr::with_envvar(
      new = c(FREESURFER_HOME = "/non/existent/path/fs"),
      code = {
        expect_warning(
          get_fs(),
          "FREESURFER_HOME is set but it does not exist!"
        )
        expect_error(get_fs(), "Can't find Freesurfer")
      }
    )
  })
})

test_that("get_fs handles options(freesurfer.path)", {
  withr::with_tempdir({
    current_test_fs_home <- file.path(getwd(), "test_fs_home_opt_isolated")
    mock_freesurfer_env(base_path = current_test_fs_home) # Renamed function call

    withr::with_envvar(
      new = c(FREESURFER_HOME = ""),
      code = {
        withr::with_options(
          new = list(
            `freesurfer.path` = current_test_fs_home,
            `freesurfer_source_function` = "bash -c \"source"
          ),
          code = {
            testthat::with_mock(
              `system` = function(command, ...) {
                if (
                  grepl("export FREESURFER_HOME=", command) &&
                    grepl("source", command)
                ) {
                  return(0)
                }
                stop("Unexpected system call in test: ", command)
              },
              .local = TRUE,
              {
                cmd <- get_fs()
                expect_true(is.character(cmd))
                expect_match(
                  cmd,
                  paste0(
                    "export FREESURFER_HOME=",
                    shQuote(current_test_fs_home)
                  )
                )
                expect_match(
                  cmd,
                  paste0(
                    "bash -c \"source ",
                    shQuote(file.path(
                      current_test_fs_home,
                      "FreeSurferEnv.sh"
                    )),
                    " || true ; \""
                  )
                )
                expect_match(cmd, paste0(shQuote(current_test_fs_home), "/bin"))
              }
            )
          }
        )
      }
    )
  })
})


test_that("get_fs finds default paths", {
  withr::with_tempdir({
    default_mock_path <- file.path(getwd(), "usr", "local", "freesurfer")
    mock_freesurfer_env(base_path = default_mock_path) # Renamed function call

    withr::with_envvar(
      new = c(FREESURFER_HOME = ""),
      code = {
        withr::with_options(
          new = list(
            `freesurfer.path` = NULL,
            `freesurfer_source_function` = "bash -c \"source"
          ),
          code = {
            testthat::with_mock(
              `system` = function(command, ...) {
                if (
                  grepl("export FREESURFER_HOME=", command) &&
                    grepl("source", command)
                ) {
                  return(0)
                }
                stop("Unexpected system call in test: ", command)
              },
              .local = TRUE,
              {
                local_mocked_bindings(
                  `base::file.exists` = function(path) {
                    if (path == default_mock_path) {
                      return(TRUE)
                    }
                    return(base::file.exists(path))
                  }
                )
                expect_warning(
                  cmd <- get_fs(),
                  paste0("Setting freesurfer.path to ", default_mock_path)
                )
                expect_true(is.character(cmd))
                expect_match(
                  cmd,
                  paste0("export FREESURFER_HOME=", shQuote(default_mock_path))
                )
              }
            )
          }
        )
      }
    )
  })
})


test_that("get_fs stops if no FreeSurfer found", {
  withr::with_envvar(
    new = c(FREESURFER_HOME = ""),
    code = {
      withr::with_options(
        new = list(`freesurfer.path` = NULL),
        code = {
          local_mocked_bindings(
            `base::file.exists` = function(path) FALSE
          )
          expect_error(get_fs(), "Can't find Freesurfer")
        }
      )
    }
  )
})


test_that("get_fs handles bin_app correctly", {
  withr::with_tempdir({
    current_test_fs_home <- file.path(getwd(), "test_fs_home_binapp_isolated")
    mock_freesurfer_env(base_path = current_test_fs_home) # Renamed function call

    withr::with_envvar(
      new = c(FREESURFER_HOME = ""),
      code = {
        withr::with_options(
          new = list(
            `freesurfer.path` = current_test_fs_home,
            `freesurfer_source_function` = "bash -c \"source"
          ),
          code = {
            testthat::with_mock(
              `system` = function(command, ...) {
                if (
                  grepl("export FREESURFER_HOME=", command) &&
                    grepl("source", command)
                ) {
                  return(0)
                }
                stop("Unexpected system call in test: ", command)
              },
              .local = TRUE,
              {
                cmd_bin <- get_fs(bin_app = "bin")
                expect_match(
                  cmd_bin,
                  paste0(shQuote(current_test_fs_home), "/bin/")
                )

                # Test with "mni/bin" (mocking specific internal file system calls)
                local_mocked_bindings(
                  `base::file.exists` = function(path) {
                    if (
                      grepl(
                        file.path(
                          current_test_fs_home,
                          "mni",
                          "MniExamples",
                          "MNI.pm"
                        ),
                        path,
                        fixed = TRUE
                      )
                    ) {
                      return(TRUE)
                    }
                    return(base::file.exists(path))
                  },
                  `base::list.files` = function(
                    path,
                    pattern,
                    full.names,
                    recursive
                  ) {
                    if (grepl("mni", path) && pattern == "MNI[.]pm") {
                      return(file.path(
                        current_test_fs_home,
                        "mni",
                        "MniExamples",
                        "MNI.pm"
                      ))
                    }
                    return(base::list.files(
                      path,
                      pattern,
                      full.names,
                      recursive
                    ))
                  }
                )
                cmd_mnibin <- get_fs(bin_app = "mni/bin")
                expect_match(
                  cmd_mnibin,
                  paste0(shQuote(current_test_fs_home), "/mni/bin/")
                )
                expect_match(cmd_mnibin, "export PERL5LIB=")

                cmd_empty <- get_fs(bin_app = "")
                expect_match(
                  cmd_empty,
                  paste0(shQuote(current_test_fs_home), "/$/")
                )
              }
            )
          }
        )
      }
    )
  })
})


# --- Tests for freesurferdir(), freesurfer_dir(), fs_dir() ---
test_that("freesurferdir, freesurfer_dir, fs_dir return correct path", {
  withr::with_tempdir({
    current_test_fs_home <- file.path(
      getwd(),
      "test_fs_home_accessors_isolated"
    )
    mock_freesurfer_env(base_path = current_test_fs_home) # Renamed function call

    withr::with_envvar(
      new = c(FREESURFER_HOME = current_test_fs_home),
      code = {
        expect_equal(freesurferdir(), current_test_fs_home)
        expect_equal(freesurfer_dir(), current_test_fs_home)
        expect_equal(fs_dir(), current_test_fs_home)
      }
    )

    withr::with_envvar(
      new = c(FREESURFER_HOME = ""),
      code = {
        withr::with_options(
          new = list(
            `freesurfer.path` = current_test_fs_home,
            `freesurfer_source_function` = "bash -c \"source"
          ),
          code = {
            testthat::with_mock(
              `system` = function(command, ...) {
                if (
                  grepl("export FREESURFER_HOME=", command) &&
                    grepl("source", command)
                ) {
                  return(0)
                }
                stop("Unexpected system call in test: ", command)
              },
              .local = TRUE,
              {
                expect_equal(freesurferdir(), current_test_fs_home)
              }
            )
          }
        )
      }
    )
  })
})

# --- Tests for have_fs() ---
test_that("have_fs reports correctly", {
  withr::with_tempdir({
    current_test_fs_home <- file.path(getwd(), "test_fs_home_havefs_isolated")
    mock_freesurfer_env(base_path = current_test_fs_home) # Renamed function call

    local_mocked_bindings(
      fs_dir = function(...) current_test_fs_home
    )
    testthat::with_mock(
      `system` = function(...) {
        stop("No system calls expected in this have_fs mock path")
      },
      .local = TRUE,
      {
        expect_true(have_fs())
        expect_true(have_fs(check_license = TRUE))

        file.remove(file.path(current_test_fs_home, "license.txt"))
        expect_true(have_fs())
        expect_false(have_fs(check_license = TRUE))
      }
    )

    local_mocked_bindings(
      fs_dir = function(...) stop("Freesurfer not found error")
    )
    testthat::with_mock(
      `system` = function(...) stop("No system calls expected"),
      .local = TRUE,
      {
        expect_false(have_fs())
      }
    )

    local_mocked_bindings(
      fs_dir = function(...) "/non/existent/dir"
    )
    testthat::with_mock(
      `system` = function(...) stop("No system calls expected"),
      .local = TRUE,
      {
        expect_false(have_fs())
      }
    )
  })
})


# --- Tests for get_fs_output() ---
test_that("get_fs_output returns correct format", {
  withr::with_options(
    new = list(fs.outputtype = NULL),
    code = {
      withr::with_envvar(
        new = c(FSF_OUTPUT_FORMAT = ""),
        code = {
          expect_warning(
            output <- get_fs_output(),
            "Can't find FSF_OUTPUT_FORMAT, setting to nii.gz"
          )
          expect_equal(output, "nii.gz")
          expect_equal(getOption("fs.outputtype"), "nii.gz")
        }
      )
    }
  )

  withr::with_envvar(
    new = c(FSF_OUTPUT_FORMAT = "nii"),
    code = {
      expect_equal(get_fs_output(), "nii")
    }
  )

  withr::with_envvar(
    new = c(FSF_OUTPUT_FORMAT = ""),
    code = {
      withr::with_options(
        new = list(fs.outputtype = "mgz"),
        code = {
          expect_equal(get_fs_output(), "mgz")
        }
      )
    }
  )
})


# --- Tests for fs_imgext() ---
test_that("fs_imgext returns correct extension", {
  local_mocked_bindings(get_fs_output = function() "nii.gz")
  expect_equal(fs_imgext(), ".nii.gz")

  local_mocked_bindings(get_fs_output = function() "nii")
  expect_equal(fs_imgext(), ".nii")

  local_mocked_bindings(get_fs_output = function() "hdr")
  expect_equal(fs_imgext(), ".hdr")

  local_mocked_bindings(get_fs_output = function() "unknown_format")
  expect_null(fs_imgext())
})


# --- Tests for fs_subj_dir() and set_fs_subj_dir() ---
test_that("fs_subj_dir and set_fs_subj_dir manage SUBJECTS_DIR", {
  withr::with_tempdir({
    current_test_fs_home <- file.path(getwd(), "test_fs_home_subjdir_isolated")
    mock_freesurfer_env(base_path = current_test_fs_home) # Renamed function call

    withr::with_envvar(
      new = c(SUBJECTS_DIR = ""),
      code = {
        withr::with_options(
          new = list(fs.subj_dir = NULL),
          code = {
            local_mocked_bindings(
              fs_dir = function(...) current_test_fs_home,
              get_fs = function(...) NULL
            )

            expect_warning(subj_dir <- fs_subj_dir(), "SUBJECTS_DIR not set")
            expect_equal(subj_dir, file.path(current_test_fs_home, "subjects"))

            custom_subj_dir <- file.path(
              getwd(),
              "my_custom_subjects_dir_isolated"
            )
            dir.create(custom_subj_dir, recursive = TRUE, showWarnings = FALSE)

            set_fs_subj_dir(custom_subj_dir)
            expect_equal(Sys.getenv("SUBJECTS_DIR"), custom_subj_dir)
            expect_equal(getOption("fs.subj_dir"), custom_subj_dir)
            expect_equal(fs_subj_dir(), custom_subj_dir)

            expect_error(
              set_fs_subj_dir("/non/existent/subjects/dir_fail"),
              "Path to set subj_dir does not exist"
            )

            withr::with_envvar(
              new = c(SUBJECTS_DIR = ""),
              code = {
                withr::with_options(
                  new = list(fs.subj_dir = NULL),
                  code = {
                    local_mocked_bindings(
                      get_fs = function(...) stop("Freesurfer not found"),
                      fs_dir = function(...) stop("Freesurfer not found")
                    )
                    expect_warning(
                      subj_dir_err <- fs_subj_dir(),
                      "SUBJECTS_DIR not set"
                    )
                    expect_true(is.na(subj_dir_err))
                  }
                )
              }
            )
          }
        )
      }
    )
  }) # End withr::with_tempdir
})
