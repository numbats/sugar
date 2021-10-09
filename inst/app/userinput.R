# Provide Your Google Cloud Credentials

options("googleAuthR.webapp.client_id" = {{{client_id}}})
options("googleAuthR.webapp.client_secret" ={{{client_secret}}})

# DASHBOARD UI & SERVER


header <- shinydashboard::dashboardHeader(title =deparse(quote({{unit_code}})))
sidebar <- shinydashboard::dashboardSidebar(shinyjs::useShinyjs(), shiny::uiOutput("sidebarpanel"))
body <- shinydashboard::dashboardBody(shinyjs::useShinyjs(), shiny::uiOutput("body"))
ui <- shinydashboard::dashboardPage(header, sidebar, body, skin = "blue")


# Access to Google Sheets
# Provide your Email Address and Unit code to google sheets
sheet <- tryCatch(
  {
    googlesheets4::gs4_auth(
      cache = ".secrets",
      email=deparse(quote({{maintainer}})),
      token = readRDS("authentication.rds")
    )
    attendance_sheets <- gs4_get(as.character(gs4_find(paste0(deparse(quote({{unit}})),"Attendance"))$id))
    grade_sheets <- gs4_find(paste0(deparse(quote({{unit}})),"Grade"))
    authorization_sheets <- gs4_find(paste0(deparse(quote({{unit}})),"Access Authorization"))

    get_attendance_link<- gs4_find(paste0(deparse(quote({{unit}})),"Attendance"))
    attendance_sheet_link <- get_attendance_link[[3]][[1]][["webViewLink"]]

    get_grades_link<- gs4_find(paste0(deparse(quote({{unit}})),"Grade"))
    grades_sheet_link <- get_grades_link[[3]][[1]][["webViewLink"]]



  },
  error = function(e) {
    message("Access has not been granted, please try again in 5 minutes.")
    return(NULL)
  }
)



