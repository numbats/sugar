
# Provide Your Google Cloud Credentials

options("googleAuthR.webapp.client_id" = "{{client_id}}")
options("googleAuthR.webapp.client_secret" ="{{client_secret}}")

# DASHBOARD UI & SERVER


header <- shinydashboard::dashboardHeader(title ='{{unit_code}}')
sidebar <- shinydashboard::dashboardSidebar(shinyjs::useShinyjs(), shiny::uiOutput("sidebarpanel"))
body <- shinydashboard::dashboardBody(shinyjs::useShinyjs(), shiny::uiOutput("body"))
ui <- shinydashboard::dashboardPage(header, sidebar, body, skin = "blue")


# Access to Google Sheets

gs4_auth(cache = ".secrets")


options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "{{maintainer}}"
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


