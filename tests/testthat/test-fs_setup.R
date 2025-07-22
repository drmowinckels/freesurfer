test_that("have_fs returns TRUE when Freesurfer is accessible and license is not checked", {
  local_mocked_bindings(
    get_fs_home = function() list(exists = TRUE),
    get_fs_license = function() list(exists = TRUE)
  )
  expect_true(have_fs())
  expect_true(have_fs(check_license = TRUE))
  expect_true(have_fs(check_license = FALSE))
})

test_that("have_fs returns FALSE when Freesurfer is inaccessible and license is not checked", {
  local_mocked_bindings(
    get_fs_home = function() list(exists = FALSE)
  )
  expect_false(have_fs())
  expect_false(have_fs(check_license = TRUE))
  expect_false(have_fs(check_license = FALSE))
})

test_that("have_fs returns TRUE when Freesurfer is accessible and license exists", {
  local_mocked_bindings(
    get_fs_home = function() list(exists = TRUE),
    get_fs_license = function() list(exists = TRUE)
  )
  expect_true(have_fs(check_license = TRUE))
})

test_that("have_fs returns FALSE when Freesurfer is accessible but license is missing", {
  local_mocked_bindings(
    get_fs_home = function() list(exists = TRUE),
    get_fs_license = function() list(exists = FALSE)
  )
  expect_false(have_fs(check_license = TRUE))
})

test_that("have_fs returns FALSE when Freesurfer is inaccessible and license is checked", {
  local_mocked_bindings(
    get_fs_home = function() list(exists = FALSE),
    get_fs_license = function() list(exists = TRUE)
  )
  expect_false(have_fs(check_license = TRUE))
})
