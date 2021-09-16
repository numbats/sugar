#' Unit Specific Inputs
#'
#' Function to set up inputs that are specific to units. The function opens the userinput.R file inorder to take in inputs.
#'
#' @export

user_input <- function(){
utils::file.edit("app/userinput.R")
}
