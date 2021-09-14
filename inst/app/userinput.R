

# DASHBOARD UI & SERVER

header <- dashboardHeader(title = "ETCXXXX")
sidebar <- dashboardSidebar(shinyjs::useShinyjs(), uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")


# Access to Google Sheets
# Provide your Email Address and URL to google sheets
sheet <- tryCatch(
  {
    googlesheets4::gs4_auth(
      cache = ".secrets",
      email="abab0012@student.monash.edu",
      token = "authentication.rds"
    )
    attendance_sheets <- gs4_get(as.character(gs4_find("ETCXXX Attendance")$id))
    grade_sheets <- gs4_find("ETCXXXX Grade")
    authorization_sheets <- gs4_find("ETCXXXX Access Authorization")
  },
  error = function(e) {
    message("Access has not been granted, please try again in 5 minutes.")
    return(NULL)
  }
)


# Provide your shinyapp.io link

# LOG OUT SERVER FUNCTION

observe({
  if (USER$login) {
    shinyjs::onclick(
      "gauth_login-googleAuthUi",
      shinyjs::runjs("window.location.href = 'https://ebsmonash.shinyapps.io/sugar-demo-app/';")
    )
  }
})




# UNAUTHORIZED ACCESS

observeEvent(input$back, {
  shinyjs::onclick(
    "back",
    shinyjs::runjs("window.location.href = 'https://ebsmonash.shinyapps.io/sugar-demo-app/';")
  )
})
