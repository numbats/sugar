# Provide Your Google Cloud Credentials

options("googleAuthR.webapp.client_id" = "732529436396-d4hp01amt4npadrqr99bhk8e6fs6s2sp.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" ="wYIpi6-freVyRSjxm44Tw1m1")

# DASHBOARD UI & SERVER


header <- shinydashboard::dashboardHeader(title ='ETC5521')
sidebar <- shinydashboard::dashboardSidebar(shinyjs::useShinyjs(), shiny::uiOutput("sidebarpanel"))
body <- shinydashboard::dashboardBody(shinyjs::useShinyjs(), shiny::uiOutput("body"))
ui <- shinydashboard::dashboardPage(header, sidebar, body, skin = "blue")


# Access to Google Sheets
# Provide your Email Address and Unit code to google sheets
sheet <- tryCatch(
  {
    googlesheets4::gs4_auth(
      cache = ".secrets",
      email='abab0012@student.monash.edu'
    )
    attendance_sheets <- gs4_get(as.character(gs4_find(paste0('ETC5521S12020',"Attendance"))$id))
    grade_sheets <- gs4_find(paste0('ETC5521S12020',"Grade"))
    authorization_sheets <- gs4_find(paste0('ETC5521S12020',"Access Authorization"))
    student_sheets <- gs4_find(paste0('ETC5521S12020',"Students"))

    get_attendance_link<- gs4_find(paste0('ETC5521S12020',"Attendance"))
    attendance_sheet_link <- get_attendance_link[[3]][[1]][["webViewLink"]]

    get_grades_link<- gs4_find(paste0('ETC5521S12020',"Grade"))
    grades_sheet_link <- get_grades_link[[3]][[1]][["webViewLink"]]

    get_authorization_link<- gs4_find(paste0('ETC5521S12020',"Access Authorization"))
    authorization_link <- get_authorization_link[[3]][[1]][["webViewLink"]]

    get_students_link<- gs4_find(paste0('ETC5521S12020',"Students"))
    students_link <- get_students_link[[3]][[1]][["webViewLink"]]



  },
  error = function(e) {
    message("Access has not been granted, please try again in 5 minutes.")
    return(NULL)
  }
)



