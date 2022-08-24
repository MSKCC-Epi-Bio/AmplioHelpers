#' check_for_phi
#'
#' Draft version of searching site for PHI
#'
#' @param consecutive_digits tbd
#' @param regex_pattern tbd
#'
#' @export


#### 18 PHI Types
# 1.	Name
# 2.	Address (all geographic subdivisions smaller than state, including street address, city county, and zip code)
# 3.	All elements (except years) of dates related to an individual (including birthdate, admission date, discharge date, date of death, and exact age if over 89)
# 4.	Telephone numbers
# 5.	Fax number
# 6.	Email address
# 7.	Social Security Number
# 8.	Medical record number
# 9.	Health plan beneficiary number
# 10.	Account number
# 11.	Certificate or license number
# 12.	Vehicle identifiers and serial numbers, including license plate numbers
# 13.	Device identifiers and serial numbers
# 14.	Web URL
# 15.	Internet Protocol (IP) Address
# 16.	Finger or voice print
# 17.	Photographic image - Photographic images are not limited to images of the face.
# 18.	Any other characteristic that could uniquely identify the individual

# num_regex <- "\\d{6,}"
# email_regex <- "@"
# social_security_regex <- "^\\(?!666|000|9\\d{2}\\)\\d{3}-\\(?!00\\)\\d{2}-\\(?!0{4}\\)\\d{4}$"
# license_regex <- "^[^+_=/*?@#$%&()\'\"|â„;:{}.,`~<>}{][^\\]{1,20}$"
#
# scriptSearch(email_regex, getwd())
# scriptSearch(social_security_regex, getwd())
# scriptSearch(license_regex, getwd())

check_for_phi <- function(
  consecutive_digits = 2,
  regex_pattern = NULL
) {

  check_for_phi_input_checks(
    consecutive_digits = consecutive_digits,
    regex_pattern = regex_pattern
    )
  #### TODOSP: include input validation here
  #### input validation cases

  if(!is.null(consecutive_digits) & is.null(regex_pattern)){
    regex_search = paste0('\\d{', consecutive_digits, ',', '}')
    print(paste0('regex pattern: ', regex_search))
    print(BurStMisc::scriptSearch(regex_search, getwd()))
  }

  if (is.null(consecutive_digits) & !is.null(regex_pattern)){
    print('no consecutive digits, yes regex')
    print(paste0('regex pattern: ', regex_pattern))
    print(BurStMisc::scriptSearch(regex_pattern, getwd()))
  }

  # if (is.na(consecutive_digits) & !is.na(regex_pattern)){
  #   print('no condig, yes regexjbkj')
  # }
  # scriptSearch(regex_pattern, getwd())
}







