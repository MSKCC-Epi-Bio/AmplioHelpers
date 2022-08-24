#' An assert statement to determine whether there are multiple rows by grouping id
#'
#' @param data_df A dataframe
#' @param group_by_vars A string or vector of strings of column name(s) to group by in asserting uniqueness
#' @param error_context For internal use
#' @param data_path For internal use
#' @param error_path For internal use
#'
#' @return Non-unique rows by grouping variables
#'
#' @export

assert_distinct <- function(data_df,
                            group_by_vars = NULL,
                            error_context = get_current_file(),
                            data_path = get_data_path(),
                            error_path = get_metadata_path("AMPLIO_ERROR_PATH")) {

  #### NOTESP: Validation check
  # 2 TESTS
  assert_distinct_input_checks(
    data_df = data_df,
    group_by_vars = group_by_vars
  )

  # NOTE: another function would allow for inspect_distinct()
  # TODO: should invisibly return a dataframe that can be inspected

  # check first for any duplicated
  dupDf <- data_df[duplicated(data_df),]

  if (nrow(dupDf) > 0)
    dupDf$errorType <- "Duplicate Rows"

  # check for multiple by group id
  multDf <- data_df %>%
    dplyr::ungroup() %>%
    unique() %>%
    dplyr::group_by_at(c(group_by_vars)) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::arrange_at(group_by_vars) %>%
    dplyr::mutate(errorType = "Multiple Rows Per Identifier")

  unionDf <- dplyr::bind_rows(dupDf, multDf) %>% dplyr::arrange(errorType)

  if (nrow(unionDf) > 0) {

    error_print <- paste0("Assert distinct failed.")

    # TODO: try-catch needed
    file <- error_context
    time <- as.numeric(format(Sys.time(), "%H%M%OS3"))

    error_item <- list(error = error_print,
                       file = error_context,
                       time = time)

    #### TODOSP: saveRDS line below was removed, should it be included?

    # 1 TEST

    saveRDS(error_item,
            glue::glue("{data_path}/{error_path}/assert_distinct_{time}.rds"))

    # TODO: add in the line information, etc.

    # 1 TEST

    print(multDf)

    # TODO: isn't there an error call?

    # 1 TEST

    stop(error_print,
         call. = FALSE)
  }

  # 1 TEST

  return(invisible(unionDf))
}




