#' Unit Specific Inputs
#'
#' Function to set up inputs that are specific to units. The function opens the userinput.R file inorder to take in inputs.
#'
#'@param setup_info unit information of class monash_unit
#'
#'@return a special class, monash unit with the unit information as a list
#' @export

user_input <- function(setup_info){

  template <- readLines("app/userinput.R")
  writeLines(whisker::whisker.render(template, setup_info), "app/userinput.R")

}
