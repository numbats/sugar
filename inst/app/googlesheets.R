library(googlesheets4)
library(googleAuthR)
# Access to Google Sheets


sheet <- tryCatch(
  {
    googlesheets4::gs4_auth(
      cache = ".secrets",
      email='{{maintainer}}'
    )
    attendance_sheets <- gs4_get(as.character(gs4_find(paste0('{{unit}}',"Attendance"))$id))
    grade_sheets <- gs4_find(paste0('{{unit}}',"Grade"))
    authorization_sheets <- gs4_find(paste0('{{unit}}',"Access Authorization"))
    student_sheets <- gs4_find(paste0('{{unit}}',"Students"))

    get_attendance_link<- gs4_find(paste0('{{unit}}',"Attendance"))
    attendance_sheet_link <- get_attendance_link[[3]][[1]][["webViewLink"]]

    get_grades_link<- gs4_find(paste0('{{unit}}',"Grade"))
    grades_sheet_link <- get_grades_link[[3]][[1]][["webViewLink"]]

    get_authorization_link<- gs4_find(paste0('{{unit}}',"Access Authorization"))
    authorization_link <- get_authorization_link[[3]][[1]][["webViewLink"]]

    get_students_link<- gs4_find(paste0('{{unit}}',"Students"))
    students_link <- get_students_link[[3]][[1]][["webViewLink"]]


  },
  error = function(e) {
    message("Access has not been granted, please try again in 5 minutes.")
    return(NULL)
  }
)



