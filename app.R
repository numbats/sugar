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


lecdf <- read_sheet(lec, skip = 1)
lecn <- lecdf[-1, ]
studentidlist <- lecn %>%
  dplyr::select(Lecture)

tutpatdf <- read_sheet(lec, sheet = 2, skip = 1)
tutpatn <- tutpatdf[-1, ]

pat_list <- tutpatn %>%
  select("Tutorial A") %>%
  rename(student_id = "Tutorial A") %>%
  mutate(tutorial = "A")


tutsherdf <- read_sheet(lec, sheet = 3, skip = 1)
tutshern <- tutsherdf[-1, ]

sher_list <- tutshern %>%
  select("Tutorial B") %>%
  rename(student_id = "Tutorial B") %>%
  mutate(tutorial = "B")

tut_student_list <- bind_rows(pat_list, sher_list)


header <- dashboardHeader(title = "SUGAR",googleAuthUI("gauth_login"))

sidebar <- dashboardSidebar(shinyjs::useShinyjs(),uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header,sidebar, body,skin = "purple")


server <- function(input, output, session) {


  login <- FALSE
  USER <- reactiveValues(login = login)

  ## Authentication
  accessToken <- callModule(googleAuth, "gauth_login",
                            login_class = "btn btn-success",
                            logout_class = "btn btn-success")

  userDetails <- reactive({

    if(is.null(accessToken())== FALSE)
    {
      USER$login <- TRUE
    }
    else loginpage

  })


  output$sidebarpanel <- renderUI({
    if(is.null(accessToken())== FALSE)
    {

    sidebarMenu(
      menuItem("Attendance", tabName = "dashboard", icon = icon("fas fa-bell")),
      menuItem("Grade", tabName = "second", icon = icon("fas fa-book-open"))
    )
    }
     else
       addClass(selector = "body", class = "sidebar-collapse")


  })


  output$body <- renderUI({
    if(is.null(accessToken())== FALSE)
    {
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

    }

    else
      fluidRow(
        setBackgroundImage(src = sample("https://images.unsplash.com/photo-1557683316-973673baf926?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=715&q=80"), shinydashboard = TRUE),

          h2("Welcome to Sugar !", style = "text-align:center;color:white;"),
          h3("Shiny Unit Grade and Attendance Reviewer", style = "text-align:center;color:white;"),
          br(),
          p("Shiny Unit Grade and Attendance Reviewer, or SUGAR, is a shiny web app that allows students to see their grade and attendance of a unit. Please Sign in via Google",
            style = "text-align:center;color:white;padding:50px;border-radius:20px"
          ),

          br()



)

  }
  )

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


