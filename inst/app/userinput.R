# Provide Your Google Cloud Credentials

options("googleAuthR.webapp.client_id" = "732529436396-d4hp01amt4npadrqr99bhk8e6fs6s2sp.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "wYIpi6-freVyRSjxm44Tw1m1")

# DASHBOARD UI & SERVER

header <- dashboardHeader(title = "ETCXXXX")
sidebar <- dashboardSidebar(shinyjs::useShinyjs(), uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")


# Access to Google Sheets
# Provide your Email Address and Unit code to google sheets
sheet <- tryCatch(
  {
    googlesheets4::gs4_auth(
      cache = ".secrets",
      email="abab0012@student.monash.edu",
      token = "authentication.rds"
    )
    attendance_sheets <- gs4_get(as.character(gs4_find(paste0("ETCXXX","Attendance"))$id))
    grade_sheets <- gs4_find(paste0("ETCXXXX","Grade"))
    authorization_sheets <- gs4_find(paste0("ETCXXXX","Access Authorization"))
  },
  error = function(e) {
    message("Access has not been granted, please try again in 5 minutes.")
    return(NULL)
  }
)

