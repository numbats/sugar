library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(googlesheets4)
library(tidyverse)
library(googleAuthR)
library(googleID)
# cred

options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options("googleAuthR.webapp.client_id" = "316001170774-dl9b831srbr2qtjaga85vrlcmjj65jqq.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "K7reQm34Pla-9DjEhPjSPL88")


sheet <- tryCatch({
  ## survey answers
  gs4_auth(
    cache = ".secrets",
    email = "abab0012@student.monash.edu",
    token = "authentication.rds"
  )
  lec <- gs4_get("125VrIIShEBJ2Xp5YgkzZN3uT-lxg_5c9C5nCgYYlv3M")
}, error = function(e){
  message("Access has not been granted, please try again in 5 minutes.")
  return(NULL)
})




header <- dashboardHeader(title = "AG checker",  googleAuthUI("gauth_login"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")



server <- function(input, output, session) {


  login <- FALSE
  USER <- reactiveValues(login = login)

  ## Authentication
  accessToken <- callModule(googleAuth, "gauth_login",
                            login_class = "btn btn-primary",
                            logout_class = "btn btn-primary")

  userDetails <- reactive({
    validate(
      need(accessToken(),"")
    )
    USER$login <- TRUE

  })


  output$sidebarpanel <- renderUI({

    validate(
      need(userDetails(), "getting user details")
    )
    sidebarMenu(
      menuItem("Attendance", tabName = "dashboard", icon = icon("fas fa-bell")),
      menuItem("Grade", tabName = "second", icon = icon("fas fa-book-open"))
    )

  })

  output$body <- renderUI({
    validate(
      need(userDetails(), "getting user details")
    )
    tabItems(

      # First tab
      tabItem(
        tabName = "dashboard", class = "active",
        fluidRow(
          br(),
          selectInput(
            "type",
            "Select class",
            c("Lecture", "Tutorial"),
            selected = NULL
          ),
          box(width = 12, dataTableOutput("results"))
        )
      ),

      # Second tab
      tabItem(
        tabName = "second",
        fluidRow(
          box(width = 12, dataTableOutput("results2"))
        )
      )
    )

  })

  output$results <- DT::renderDataTable({
    datatable(lecn %>%
                filter(Lecture == "s123"), options = list(
                  autoWidth = TRUE,
                  searching = FALSE
                ))
  })

  output$results2 <- DT::renderDataTable({
    datatable(lecn %>%
                filter(Lecture == "s123"), options = list(
                  autoWidth = TRUE,
                  searching = FALSE
                ))
  })

  observe({
    if (USER$login) {
      shinyjs::onclick("gauth_login-googleAuthUi",
                       shinyjs::runjs("window.location.href = 'http://127.0.0.1:4560';"))
    }
  })



}

runApp(list(ui = ui, server = server))


