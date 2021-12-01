#' Create skeleton of Google Sheet for student
#'
#' Function to create a google sheets for storing information on Students.
#' @param unitinfo  unit information of class monash unit
#'
#' @export
create_student_sheet <- function(unitinfo) {
  code=unitinfo$unit
  tbl_colnames <- c("Student Id", "Firstname", "Lastname", "Email")
 Student<-   setNames(data.frame(matrix(ncol = length(tbl_colnames), nrow = 0)), tbl_colnames)
  students <- googlesheets4::gs4_create(paste(code, "Students", sep = ""), sheets = Student)
}

#' Create skeleton of Google Sheet for Attendance
#'
#' Function to create a google sheets for storing information on attendance of students.
#' @param unitinfo  unit information of class monash unit
#' @param schedule a character vector that depicts the unique type of class ie.Lecture A, Lecture B, Tutorial A, Tutorial B etc.
#' @export
create_attendance_sheet <- function(unitinfo,schedule) {
  code=unitinfo$unit
  attendance <- googlesheets4::gs4_create(paste(code, "Attendance", sep = ""),
                                          sheets = unique(schedule))

  for (i in 1:length(schedule)){

    tbl_colnames <- c("Student Email",
                      "DD-MM-YYYY",
                      "DD-MM-YYYY")

    attend <- setNames(data.frame(matrix(ncol = length(tbl_colnames), nrow = 0)), tbl_colnames)

    attendance <- attendance %>%
      googlesheets4::sheet_write(data = attend, sheet = unique(schedule)[i])
  }
}

#' Create skeleton of Google Sheet for Assessment Grades
#'
#' Function to create a google sheets for storing information on Grade.
#' @param unitinfo  unit information of class monash unit
#'@param assessment A tibble with columns "Assessment" that depicts the name of the assessment, "Weightage" depicts the weightage of the assessment and "Total Marks" is the total marks alloted for the assessment.
#' @export
create_grade_sheet <- function(unitinfo,assessment) {
  code=unitinfo$unit
  grades <- googlesheets4::gs4_create(paste(code, "Grade", sep = ""), sheets = c("Grades", "Assessment Information"))
   tbl_colnames <- c("Student Email", c(assessment$Assessment))

  grade_info <- setNames(data.frame(matrix(ncol = length(tbl_colnames), nrow = 0)), tbl_colnames)

  grades <- grades %>%
    googlesheets4::sheet_write(data = grade_info, sheet = "Grades") %>%
    googlesheets4::sheet_write(data = assessment, sheet = "Assessment Information")
}

#' Create skeleton of Google Sheet for Authorisation for web application access
#'
#' Functions to create a google sheets for storing information on the faculty members authorized to access the web application.
#' @param unitinfo  unit information of class monash unit
#'
#' @export
create_authorization_sheet <- function(unitinfo) {
  code=unitinfo$unit

  tbl_colnames <- c("Faculty Email")

  authorized_list <- setNames(data.frame(matrix(ncol = length(tbl_colnames), nrow = 0)), tbl_colnames)
  authorized <- googlesheets4::gs4_create(paste(code, "Access Authorization", sep = ""), sheets = authorized_list)

}
