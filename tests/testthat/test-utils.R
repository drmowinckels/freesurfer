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

  # Test: Confirm that file_exists identifies the file
  expect_true(file_exists(temp_file))

  # Teardown: Remove the temporary file
  rmdir(temp_file)
})

test_that("file_exists correctly identifies non-existing files", {
  # Setup: Create a temporary file path that does not exist
  temp_file <- tempfile()

  # Test: Confirm that file_exists identifies the file as not existing
  expect_false(file_exists(temp_file))
})

test_that("file_exists works with multiple file paths", {
  # Setup: Create multiple temporary files
  temp_file1 <- tempfile()
  temp_file2 <- tempfile()
  temp_file3 <- tempfile() # File does not exist

  file.create(temp_file1)
  file.create(temp_file2)

  # Test: Confirm file_exists properly handles a vector of file paths
  result <- file_exists(c(temp_file1, temp_file2, temp_file3))
  expect_equal(result, c(TRUE, TRUE, FALSE))

  # Teardown: Remove the temporary files
  rmdir(temp_file1)
  rmdir(temp_file2)
})

test_that("file_exists handles edge cases for directory paths", {
  # Setup: Create a temporary directory
  temp_dir <- tempdir()

  # Test: Confirm that file_exists identifies the directory as existing
  expect_true(file_exists(temp_dir))
})

test_that("file_exists correctly handles empty input", {
  # Test: Empty input should return a logical(0) (i.e., empty logical vector)
  expect_equal(file_exists(character(0)), logical(0))
})

test_that("file_exists handles invalid input gracefully", {
  # Test: Invalid input (e.g., NULL or non-character input) should error
  expect_error(
    file_exists(NULL),
    "invalid 'file' argument"
  )
  expect_error(
    file_exists(123),
    "invalid 'file' argument"
  )
})
