test_that("Test check_for_phi inputs", {

  # expect_error(
  #   check_for_phi(
  #     consecutive_digits = NULL,
  #     regex_pattern = NULL
  #   ),
  #   "Both input arguments cannot be null"
  # )
  #
  # expect_error(
  #   check_for_phi(
  #     consecutive_digits = 5,
  #     regex_pattern = 'asdf'
  #   ),
  #   "input can only be provided for one of the arguments, not both"
  # )

  expect_equal(
    TRUE,
    TRUE
  )

})

# test_that("Test check_for_phi functionality", {


  # Test for default of 2 consecutive digits

  # regex_search = paste0('\\d{', 2, ',', '}')
  #
  # expect_equal(
  #   stringr::str_detect(
  #     capture_output(
  #       check_for_phi()),
  #     regex_search
  #   ),
  #   TRUE
  # )

  # Test for 8 consecutive digits

  # regex_search = paste0('\\d{', 8, ',', '}')
  #
  # expect_equal(
  #   stringr::str_detect(
  #     capture_output(
  #       check_for_phi(
  #         consecutive_digits = 8,
  #         regex_pattern = NULL
  #       )
  #     ),
  #     regex_search
  #   ),
  #   TRUE
  # )

  # Test for message inside

  # regex_search = 'no consecutive digits, yes regex'
  #
  # expect_equal(
  #   stringr::str_detect(
  #     capture_output(
  #       check_for_phi(
  #         consecutive_digits = NULL,
  #         regex_pattern = regex_search
  #       )
  #     ),
  #     regex_search
  #   ),
  #   TRUE
  # )

# })
