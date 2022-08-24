# A function to do input validation on snippet functions

check_is_null <- function(...){
  # print('check1')
  # print(.traceback())

  inputs <- list(...)

  for (i in inputs){
    if (is.null(i)) {
      print(.traceback())
      stop(glue::glue(
        "Input cannot be NULL"
      ))
    }
  }
}

check_string <- function(input){

  if (!is.null(input)){
    if (!is.character(input)){
      stop(glue::glue(
        "{input} must be of type string"
      ))
    }
  }
}

check_dataframe <- function(data_df) {

  # data -----------------------------------------------------------------------
  # data is a data frame

  if(!is.null(data_df)){
    if (!is.data.frame(data_df)) {
      stop("Data input must be a dataframe")
    }

    # cannot be empty data frame
    if (nrow(data_df) == 0L) {
      stop("Dataframe must have at least 1 row")
    }

    # must have at least one column
    if (ncol(data_df) == 0L) {
      stop("Dataframe must have at least 1 column")
    }
  }
}

check_vars <- function(vars, data_df){

  if(!is.null(vars)){
    check_is_null(data_df)
    check_dataframe(data_df)
    for (element in vars){
      if (!(element %in% colnames(data_df))){
        stop(glue::glue(
          "{element} must be a column in dataframe"
        ))
      }
    }
  }

}

#### Below for fields like group_by_vars
check_optional_field_cols <- function(fields, data_df){

  check_is_null(data_df)
  check_dataframe(data_df = data_df)

  if(!is.null(fields)){
    check_vars(vars = fields,
               data_df = data_df)
  }

}

check_type_map_cont <- function(type){

  if(!is.null(type)){
    if (!(type == 'map' | type == 'cont')){
      stop(glue::glue(
        "{type} argument must be map or cont"
      ))
    }
  }
}

check_type <- function(type){

  check_is_null(type)
  check_string(type)
  check_type_map_cont(type)

}

# TODO: just added this for use in assert_vals to  check if logic value evaluates to

check_assert_logic <- function(input){

  tryCatch(
    eval(parse(text = input)),
    error = function(cond) {
      stop(glue::glue(
        "Logic unparsable: {input}"
        )
      )
    }
  )
}

check_eval_func <- function(eval_func){
  if(!is.null(eval_func)){
    if (!is.function(eval_func)){
      stop(glue::glue(
        "eval_func must be a function"
      ))
    }
  }
}

check_by_time <- function(by_time,
                          data_df){
  if (!is.null(by_time)){
    check_dataframe(data_df = data_df)
    check_string(input = by_time)
    check_vars(by_time,
               data_df)
  }

}

check_binary <- function(input){
  if(!is.null(input)){
    if (!(input == 1 | input  == 0)){
      stop(glue::glue(
        "'{input}' must be binary"
      ))
    }
  }
}

check_line_log_type <- function(input){

  if(!is.null(input)){
    check_string(input = input)
    if (!(input == 'COMMENT' | input == 'CALL' | input == 'TODO')){
      stop(glue::glue(
        "'{input}' must indicate type of line_log (COMMENT/CALL/TODO)"
      ))
    }
  }
}

check_rds <- function(to_vars, rds){

  #### logic
  # 1. Take in to_vars, rds name
  ## to_vars has been checked, cannot be NULL
  # 2. If length(to_vars) == 1, rds can be null or string
  # 3. If length(to_vars)  > 1, rds cannot be null, has to be string

  # check_string(rds)

  if (length(to_vars) == 1){
    if (is.null(rds)){
      stop(glue::glue(
        "rds cannot be NULL if length of 'parsed'/'to' variable is 1."
      ))
    }else if (!is.null(rds)){

      check_string(input = rds)

    }
    # if (!is.character(rds) | )
  } else if (length(to_vars) > 1) {
    if (is.null(rds)){
      stop(glue::glue(
        "rds cannot be NULL if length of 'parsed'/'to' variable is >1. rds string value must be assigned."
      ))
    } else if(!is.null(rds)){
      check_string(input = rds)
    }
  }
}

check_minmax <- function(min, max){

  if ((!is.null(min) & !is.numeric(min)) | (!is.null(max) & !is.numeric(max))) {
    stop(
      glue::glue(
        "non-numeric min/max value: {min}/{max} "
      )
    )
  }

  if (!is.null(min) & !is.null(max)) {

    if (max < min){
      stop(
        glue::glue(
          "max value {max} is less than min value {min}"
        )
      )
    }
  }

}

# Below only for use with replace_cases, make sure replace_cases and replace_values
check_replace_vectors_same_length <- function(vector1, vector2){

  if (length(vector1) != length(vector2)){
    stop(
      glue::glue(
        "replace_fields and replace_values vector must be of same length"
      )
    )
  }
}

check_logic <- function(logic, data_df){

  #### logic object doesn't have to be string, can also be logical object
  check_string(input = logic)

  logical_col <- data_df %>% dplyr::mutate(new_col = eval(parse(text = logic))) %>% dplyr::pull(new_col)

  if (!is.logical(typeof(logical_col))){
    stop(
      glue::glue(
        "logic object must parse from string to be of type logical"
      )
    )
  }
}


# AmplioHelpers Functions Checks ------------------------------------------

# Done
replace_cases_input_checks <- function(data_df,
                                       select_ids,
                                       logic,
                                       replace_fields,
                                       replace_values,
                                       issue,
                                       rds,
                                       group_by_vars){

  # everything except rds, group_by_vars cannot be NULL

  #### TODOSP: Check with RW about below comment

  # Actually rds cannot be null either because there is no default value for rds
  # Usually, if there is a single 'to' or 'parsed' variable, then that can be used as default rds value
  # However, since in replace_cases there are no 'to' or 'parsed' variables

  check_is_null(data_df ,
                select_ids,
                logic,
                replace_fields,
                replace_values,
                rds,
                issue)

  check_dataframe(data_df = data_df)
  check_vars(vars = select_ids,
             data_df = data_df)
  check_logic(logic = logic,
              data_df = data_df)
  check_vars(vars = replace_fields, data_df = data_df)

  # Do check below later, right now no easy way to check for types for each column
  # Ryan said sometimes with columns, even NAs can have types, will be complicated to check this, so move on for now

  # check_vector_values(replace_values)

  # One check that has to be done is length(replace_fields) == length(replace_values)

  check_binary(input = issue)
  check_is_null(rds)
  check_string(input = rds)
  # Say we were to check rds using check_rds function. All we need is to check if

  check_optional_field_cols(fields = group_by_vars,
                            fields = data_df)

  check_replace_vectors_same_length(vector1 = replace_fields,
                                    vector2 = replace_values)

}

check_vals_input_checks <- function(data_df,
                                    raw,
                                    parsed,
                                    type,
                                    issue,
                                    eval_func,
                                    rds,
                                    by_year){

  # Everything except rds, by_year cannot be NULL

  check_is_null(data_df,
                raw,
                parsed,
                type,
                issue,
                eval_func)

  check_dataframe(data_df = data_df)

  check_vars(vars = raw,
             data_df = data_df)
  check_vars(vars = parsed,
             data_df = data_df)

  check_type(type = type)

  check_rds(to_vars = parsed,
            rds = rds)

  check_binary(input = issue)

  check_eval_func(eval_func = eval_func)

  check_by_time(by_time = by_year,
                data_df =  data_df)

}

check_derived_input_checks <- function(summary_df,
                                       data_df,
                                       from,
                                       to,
                                       cont_to,
                                       data_feature,
                                       issue,
                                       allowed_val,
                                       check,
                                       rds,
                                       by_year){

  # All except rds, by_year cannot be null

  check_is_null(summary_df,
                data_df,
                from,
                to,
                cont_to,
                data_feature,
                issue,
                allowed_val,
                check)

  check_dataframe(data_df = summary_df)
  check_dataframe(data_df = data_df)
  check_vars(vars = from,
             data_df = data_df)
  check_vars(vars = to,
             data_df = data_df)
  check_rds(to_vars = to,
            rds = rds)
  check_binary(input = cont_to)
  check_by_time(by_time = by_year,
                data_df = data_df)
  check_binary(input = data_feature)
  check_binary(input = issue)
  check_binary(input = allowed_val)
  check_binary(input = check)

}

log_derived_input_checks <- function(data_df,
                                     from,
                                     to,
                                     cont_to,
                                     by_year,
                                     min,
                                     max,
                                     rds){

  check_is_null(data_df,
                from,
                to,
                cont_to)

  check_dataframe(data_df = data_df)
  check_vars(vars = from,
             data_df = data_df)
  check_vars(vars = to,
             data_df = data_df)
  check_rds(to_vars = to,
            rds = rds)
  check_binary(input = cont_to)
  check_by_time(by_time = by_year,
                data_df = data_df)
  check_minmax(min = min,
               max = max)

}


line_log_input_checks <- function(type,
                                  item){

  check_is_null(type,
                item)

  check_line_log_type(input = type)
  check_string(input = item)

}


ques_cases_input_checks <- function(data_df,
                                    select,
                                    logic,
                                    na_field,
                                    issue,
                                    rds,
                                    group_by_vars){

  check_is_null(data_df,
                select,
                logic,
                issue)

  check_dataframe(data_df = data_df)
  check_vars(vars = select, data_df = data_df)
  check_string(input = logic)
  check_optional_field_cols(fields = na_field,
                            data_df = data_df)
  check_binary(input = issue)
  check_string(input = rds)
  check_optional_field_cols(fields = group_by_vars,
                            data_df = data_df)

}

filter_cases_input_checks <- function(data_df,
                                      id_vars,
                                      non_id_vars,
                                      logic,
                                      issue,
                                      rds,
                                      group_by_vars){

  # In the check, perhaps either id_vars or non_id_vars can be NULL, but not both

  id_and_non_id_vars <- c(id_vars, non_id_vars)

  check_is_null(data_df,
                id_and_non_id_vars,
                logic,
                issue)

  check_dataframe(data_df = data_df)
  check_vars(vars = id_vars,
             data_df = data_df)
  check_vars(vars = non_id_vars,
             data_df = data_df)
  check_string(input = logic)
  check_binary(input = issue)
  check_rds(to_vars = non_id_vars,
            rds = rds)
  check_optional_field_cols(fields = group_by_vars,
                            data_df = data_df)


}

save_issue_input_checks <- function(data_df,
                                    rds){

  check_is_null(data_df,
                rds)

  check_dataframe(data_df = data_df)
  check_string(input = rds)

}

save_summary_input_checks <- function(data_df,
                                      rds,
                                      issue){

  check_is_null(data_df,
                rds,
                issue)

  check_dataframe(data_df = data_df)
  check_string(input = rds)
  check_binary(input = issue)

}

assert_vals_input_checks <- function(logic,
                                      rds){


  check_is_null(logic)

  check_assert_logic(input = logic)
  check_string(input = rds)

}

assert_distinct_input_checks <- function(data_df,
                                         group_by_vars){

  check_is_null(data_df)
  check_dataframe(data_df = data_df)

  check_optional_field_cols(fields = group_by_vars,
                            data_df =  data_df)

}

check_for_phi_input_checks <- function(consecutive_digits,
                                       regex_pattern){

  if (is.null(consecutive_digits) & is.null(regex_pattern)){
    stop("Both input arguments cannot be null")
  }

  if (!is.null(consecutive_digits) & !is.null(regex_pattern)){
    stop("input can only be provided for one of the arguments, not both")
  }
}

by_year_check_input_checks <- function(data_df,
                                       parsed_col,
                                       by_year,
                                       by_month = NULL){

  check_is_null(data_df)
  check_dataframe(data_df = data_df)

  check_is_null(parsed_col)
  check_vars(vars = parsed_col,
             data_df = data_df)

  check_is_null(by_year)
  check_by_time(by_time = by_year,
                data_df = data_df )

  check_by_time(by_time = by_month,
                data_df = data_df)


}

inspect_derived_input_checks <- function(summary_df,
                                         data_df,
                                         from,
                                         to,
                                         rds,
                                         cont_to,
                                         by_year,
                                         data_feature,
                                         issue,
                                         allowed_val,
                                         check){

  check_is_null(data_df,
                from,
                to)

  check_dataframe(data_df = summary_df)
  check_dataframe(data_df = data_df)

  check_vars(vars = from,
             data_df = data_df)

  check_vars(vars = to,
             data_df = data_df)


  check_rds(to_vars = to,
            rds = rds)

  check_binary(input = cont_to)

  check_vars(vars = by_year,
             data_df = data_df)

  check_binary(input = data_feature)
  check_binary(input = issue)
  check_binary(input = allowed_val)
  check_binary(input = check)

}



















