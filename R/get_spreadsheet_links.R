#' Get spreadsheet links
#'
#' Function to obtain links of the spreadsheet created
#'
#'@export
get_spreadsheet_links <- function(){

  source("app/googlesheets.R")

  spreadsheet_links <- c("Attendance"=attendance_sheet_link,
                         "Grades"= grades_sheet_link,
                         "Students"=students_link,
                         "Authorization"=authorization_link)
  spreadsheet_links
}
