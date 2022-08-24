
test_that("Test assert_distinct inputs", {


  expect_error(assert_distinct(
    data_df = 'asdf',
    group_by_vars = NULL
    )
  )

  expect_error(assert_distinct(
    data_df = qa_tbl,
    group_by_vars = c('asdf')
    )
  )

})

# test_that("Test assert_distinct functionality", {
#
#   #### 1. Test to see if current artifact will be compared to previous artifact (which doesn't exist)
#
#   on.exit(remove_test_dir(dir_to_reset = rtn_init_test_dir$temp_directory,
#                           original_directory = rtn_init_test_dir$original_directory))
#
#   rtn_init_test_dir <- init_test_dir(autosave_state = FALSE)
#
# # introduce duplicate row and 'almost' duplicate row (w/ same mrn  --------
#
#   dup_row <- qa_tbl[1,]
#   dup_row2 <- qa_tbl[2,]
#   # row that has same identifier (mrn) as another record, but 1 or more values for other columns
#
#   qa_tbl2 <- qa_tbl
#
#   qa_tbl2 <- dplyr::bind_rows(qa_tbl2,dup_row,dup_row2)
#
#   # 'almost' duplicate row by changing single LOS value
#   qa_tbl2[2,4] = 5
#
#   #### Because there is both a duplicate record and a record with the same MRN but a different LOS value,
#   #### The unionDf will be of length 2
#
#   #### Thus first error message will be triggered: "Assert distinct failed."
#
#   # 1
#   expect_error(
#     capture_output(
#       assert_distinct(data_df = qa_tbl2,
#                                      group_by_vars = c('mrn')),
#       'Assert distinct failed.'
#     )
#   )
#
#   #### Check size of unionDf, since there are 2 introduced rows, tibble should have 2 rows 25 cols (25 var)
#   2
#   expect_equal(
#     stringr::str_detect(
#       capture_output(
#         expect_error(
#           assert_distinct(data_df = qa_tbl2,
#                           group_by_vars = c('mrn')),
#           'Assert distinct failed.'
#         )
#       ),
#       'A tibble: 2 x 26'
#     ),
#     TRUE
#   )
#
#   # 3
#   # Use capture_output to suppress output
#   capture_output(
#     expect_error(
#       assert_distinct(data_df = qa_tbl2,
#                       group_by_vars = c('mrn')
#                       ),
#       'Assert distinct failed.'
#     )
#   )
# })




































