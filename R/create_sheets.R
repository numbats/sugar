#' Create skeleton of Google Sheets
#'
#' Functions to create a google sheets for storing information on Attendance, Grade and Authorisation list.
#'
#' @export
create_student_sheet <- function(){

  students <- googlesheets4::gs4_create("students")

  }
