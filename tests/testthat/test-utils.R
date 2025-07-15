test_that("mkdir successfully creates a directory", {
  # Setup: Define a temporary directory
  temp_dir <- file.path(tempdir(), "test_mkdir_dir")

  # Test: Ensure the directory does not initially exist
  expect_false(dir.exists(temp_dir))

  # Run mkdir to create the directory
  mkdir(temp_dir)

  # Assertions: Directory should exist after creation
  expect_true(dir.exists(temp_dir))

  # Teardown: Remove the created directory
  unlink(temp_dir, recursive = TRUE)
})

test_that("mkdir handles already existing directories gracefully", {
  # Setup: Define and create a temporary directory
  temp_dir <- file.path(tempdir(), "test_existing_dir")
  mkdir(temp_dir)

  # Test: Call mkdir again on the same directory
  expect_true(dir.exists(temp_dir))
  expect_silent(mkdir(temp_dir)) # Should not throw any warning

  # Assertions: Directory should still exist
  expect_true(dir.exists(temp_dir))

  # Teardown: Remove the created directory
  unlink(temp_dir, recursive = TRUE)
})

test_that("temp_file creates a temporary file path with its directory", {
  # Test: Generate a temporary file path
  temp_file <- temp_file(pattern = "test_file_")

  # Assertions: Verify the directory and the file path
  expect_type(temp_file, "character")
  expect_true(dir.exists(dirname(temp_file)))
  expect_match(temp_file, "test_file_")

  # Teardown: Remove the created directory
  unlink(dirname(temp_file), recursive = TRUE)
})

test_that("temp_file handles file name pattern customization", {
  # Test: Generate a temporary file path with a customized pattern
  temp_file <- temp_file(pattern = "custom_pattern_", fileext = ".txt")

  # Assertions: Validate the temporary file path follows the given pattern
  expect_match(basename(temp_file), "^custom_pattern_.*\\.txt$")
  expect_true(dir.exists(dirname(temp_file)))

  # Teardown: Remove the created directory
  unlink(dirname(temp_file), recursive = TRUE)
})

test_that("temp_file works without additional custom arguments", {
  # Test: Generate a simple temporary file path
  temp_file <- temp_file()

  # Assertions: Validate the file path and its directory
  expect_true(file_exists(dirname(temp_file)))

  # Teardown: Remove the created directory
  unlink(dirname(temp_file), recursive = TRUE)
})

test_that("temp_file does not create the file, only ensures directory exists", {
  # Test: Generate a temporary file path
  temp_file <- temp_file(pattern = "test_no_file_creation_")

  # Assertions: Verify that the file itself does not exist
  expect_false(file_exists(temp_file))
  expect_true(dir.exists(dirname(temp_file)))

  # Teardown: Remove the created directory
  unlink(dirname(temp_file), recursive = TRUE)
})

test_that("temp_file handles complex directory structures", {
  # Setup: Generate a path with nested structure
  temp_root <- file.path(tempdir(), "nested_dir_1", "nested_dir_2")
  temp_file <- temp_file(tmpdir = temp_root, pattern = "deep_nested_file_")

  # Assertions: Verify the nested directory structure is created
  expect_true(dir.exists(temp_root))
  expect_match(temp_file, "deep_nested_file_")

  # Teardown: Remove the nested directories
  unlink(dirname(temp_file), recursive = TRUE)
})


test_that("file_exists correctly identifies existing files", {
  # Setup: Create a temporary file
  temp_file <- tempfile()
  file.create(temp_file)

  # Test: File exists; should return TRUE
  expect_true(file_exists(temp_file))

  # Teardown: Remove the temporary file
  unlink(temp_file)
})

test_that("file_exists correctly identifies non-existing files when error = FALSE", {
  # Setup: Non-existing file path
  temp_file <- tempfile()

  # Test: File does not exist; should return FALSE without throwing an error
  expect_false(file_exists(temp_file, error = FALSE))
})

test_that("file_exists throws an error when file does not exist and error = TRUE", {
  # Setup: Non-existing file path
  temp_file <- tempfile()

  # Test: File does not exist; should throw an error
  expect_error(file_exists(temp_file, error = TRUE), "File does not exist")
})

test_that("file_exists throws an error when all files in a vector do not exist and error = TRUE", {
  # Setup: Multiple non-existing file paths
  temp_files <- c(tempfile(), tempfile(), tempfile())

  # Test: None of the files exist; should throw an error
  expect_error(file_exists(temp_files, error = TRUE), "File does not exist")
})

test_that("file_exists does not throw an error if at least one file in the vector exists and error = TRUE", {
  # Setup: Create a temporary file and define other non-existing paths
  temp_file <- tempfile() # File exists
  file.create(temp_file)
  non_existing_files <- c(temp_file, tempfile(), tempfile())

  # Test: At least one file exists; should return logical vector without error
  result <- file_exists(non_existing_files, error = TRUE)
  expect_equal(result, c(TRUE, FALSE, FALSE))

  # Teardown: Remove the temporary file
  unlink(temp_file)
})

test_that("file_exists correctly handles empty input", {
  # Test: Empty input should return an empty logical vector
  expect_equal(file_exists(character(0)), logical(0))
})

test_that("file_exists handles directory paths", {
  # Setup: Temporary directory path
  temp_dir <- tempdir()

  # Test: Directory exists and should return TRUE
  expect_true(file_exists(temp_dir))

  # Test: Non-existing directory with error = FALSE
  expect_false(file_exists(file.path(temp_dir, "nonexistent"), error = FALSE))

  # Test: Non-existing directory with error = TRUE
  expect_error(
    file_exists(file.path(temp_dir, "nonexistent"), error = TRUE),
    "File does not exist"
  )
})

test_that("file_exists handles invalid inputs gracefully", {
  # Test: NULL input should throw an error
  expect_error(
    file_exists(NULL),
    "invalid"
  )

  # Test: Non-character input should throw an error
  expect_error(
    file_exists(123, error = TRUE),
    "invalid"
  )
})
