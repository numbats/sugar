library(shiny)
library(fullcalendar)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(googlesheets4)
library(tidyverse)
library(googleAuthR)
library(monash)

source("Global.R")
source("tabs.R")




# DASHBOARD UI & SERVER

header <- dashboardHeader(title = "SUGAR")

sidebar <- dashboardSidebar(shinyjs::useShinyjs(), uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")


# SERVER FUNCTION

server <- function(input, output, session) {

  USER <- reactiveValues(login = FALSE)

  # Google Authentication
  accessToken <- callModule(googleAuth, "gauth_login",
    login_text = "Sign in via Google",
    logout_text = "Sign Out",
    login_class = "btn btn-success",
    logout_class = "btn btn-success"
  )
  userDetails <- reactive({
    validate(
      need(accessToken(), "")
    )

    with_shiny(get_email, shiny_access_token = accessToken())
  })


  # get user name

  output$user_name <- renderText({
    validate(
      need(userDetails(), "")
    )

    paste0("  ", userDetails())
  })


  # Side bar panel

  output$sidebarpanel <- renderUI({
    if ((is.null(accessToken()) == FALSE)) {
      if ((is.element(as.character(userDetails()), authorised_list$value) == TRUE)) {
        sidebarMenu(
          # menuItem("Student", tabName = "student", icon = icon("fas fa-user")),
          br(),
          p("  Welcome ! ", textOutput("user_name")),
          br(),
          menuItem("Attendance", tabName = "attendance", icon = icon("fas fa-bell")),
          menuItem("Grade", tabName = "grade", icon = icon("fas fa-book-open")),
          br(),
          googleAuthUI("gauth_login")
        )
      } else {
        addClass(selector = "body", class = "sidebar-collapse")
      }
    } else {
      addClass(selector = "body", class = "sidebar-collapse")
    }
  })

    # Monash University Picture

  output$picture <- renderImage(
    {

      return(list(
        src = "www/blc.png",
        contentType = "image/png",
        width = 420,
        height = 150
      ))
    },
    deleteFile = FALSE
  )


    # Retrieving hd of email id : @student.monash.edu / monash.edu to check if the user is student/staff

  userhd <- reactive({
    validate(
      need(accessToken(), "")
    )

    with_shiny(get_hd, shiny_access_token = accessToken())
  })

  `%notin%` <- Negate(`%in%`)
  authorised_list <- as.tibble(c(pivot$email, "aarathy.babu@monash.edu"))

      # Body of app

  output$body <- renderUI({
    if (is.null(accessToken()) == FALSE) {
      if (is.element(as.character(userDetails()), authorised_list$value) == TRUE) {
        if (as.character(userhd()) == "student.monash.edu") {
          tabItems(
            # first tab
            first_tab,

            # second tab
            second_tab
          )
        } else {
          tabItems(
            # first tab
            staff_first_tab,

            # second tab

            staff_second_tab
          )
        }
      } else {
        error_page
      }
    } else {
      landing_page
    }
  })

  source(file.path("server","student_server.R"),  local = TRUE)$value
  source(file.path("server","staff_server.R"),  local = TRUE)$value



  # LOG OUT SERVER FUNCTION

  observe({
    if (USER$login) {
      shinyjs::onclick(
        "gauth_login-googleAuthUi",
        shinyjs::runjs("window.location.href = 'http://127.0.0.1:4549';")
      )
    }
  })


  # UNAUTHORIZED ACCESS

  observeEvent(input$back, {
    shinyjs::onclick(
      "back",
      shinyjs::runjs("window.location.href = 'http://127.0.0.1:4549';")
    )
  })
}

runApp(list(ui = ui, server = server), launch.browser = TRUE, port = 4549)
