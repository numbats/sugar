#' Create skeleton of Google Sheet for student
#'
#' Function to create a google sheets for storing information on Students.
#' @param code The Unit Code
#' @export
create_student_sheet <- function(code) {
  tbl_colnames <- c("Studentid", "Firstname", "Lastname", "Email", "Tutorialgroup")
  student_tibble <- tbl_colnames %>%
    rlang::rep_named(list(logical())) %>%
    tibble::as_tibble()
  students <- googlesheets4::gs4_create(paste(code, "Students", sep = " "), sheets = student_tibble)
}

#' Create skeleton of Google Sheet for Attendance
#'
#' Function to create a google sheets for storing information on attendance of students.
#' @param schedule A tibble with columns "Class","StartDate" and "EndDate" where "Class" depicts the type of class ie.Lecture A, Lecture B, Tutorial A, Tutorial B etc. "StartDate" and "EndDate" depicts the date of start and end of the class in the semester.
#' @export
create_attendance_sheet <- function(code, schedule) {

  attendance <- googlesheets4::gs4_create(paste(code, "Attendance", sep = ""), sheets = unique(schedule$Class))

  for (i in 1:length(unique(schedule$Class))) {

    tbl_colnames <- c("Student Email",
                      as.character(seq((schedule$StartDate[i]),
                      schedule$EndDate[i], by = "week")))

    attend <- tbl_colnames %>%
      rlang::rep_named(list(logical())) %>%
      tibble::as_tibble()

    attendance <- attendance %>%
      googlesheets4::sheet_write(data = attend, sheet = unique(schedule$Class)[i])
  }
}

#' Create skeleton of Google Sheet for Assessment Grades
#'
#' Function to create a google sheets for storing information on Grade.
#' @param assessment A tibble with columns "Assessment" that depicts the name of the assessment, "Weightage" depicts the weightage of the assessment and "Total Marks" is the total marks alloted for the assessment.
#' @export
create_grade_sheet <- function(code, assessment) {

  grades <- googlesheets4::gs4_create(paste(code, "Grade", sep = ""), sheets = c("Grades", "Assessment Information"))
   tbl_colnames <- c("Student Email", c(assessment$Assessment))

  grade_info <- tbl_colnames %>%
    rlang::rep_named(list(logical())) %>%
    tibble::as_tibble()

  grades <- grades %>%
    googlesheets4::sheet_write(data = grade_info, sheet = "Grades") %>%
    googlesheets4::sheet_write(data = assessment, sheet = "Assessment Information")
}

#' Create skeleton of Google Sheet for Authorisation for web application access
#'
#' Functions to create a google sheets for storing information on the faculty members authorized to access the web application.
#' @export
create_authorization_sheet <- function(code, assessment) {


  tbl_colnames <- c("Faculty Email")

  authorized_list <- tbl_colnames %>%
    rlang::rep_named(list(logical())) %>%
    tibble::as_tibble()
  authorized <- googlesheets4::gs4_create(paste(code, "Access Authorization", sep = ""), sheets = authorized_list)

}
