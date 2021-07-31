library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(tidyverse)
library(googlesheets4)
# cred

lec <- "https://docs.google.com/spreadsheets/d/125VrIIShEBJ2Xp5YgkzZN3uT-lxg_5c9C5nCgYYlv3M/edit#gid=186001220"
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




# Main login screen
loginpage <- div(
  id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
  wellPanel(
    tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
    textInput("studentid", placeholder = "studentid", label = tagList(icon("user"), "Student ID")),
    br(),
    div(
      style = "text-align: center;",
      actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
      shinyjs::hidden(
        div(
          id = "nomatch",
          tags$p("Oops! Incorrect username!",
                 style = "color: red; font-weight: 600;
                                            padding-top: 5px;font-size:16px;",
                 class = "text-center"
          )
        )
      ),
      br()
    )
  )
)

credentials <- tibble(
  student_id = studentidlist$Lecture
)

header <- dashboardHeader(title = "AG checker", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  login <- FALSE
  USER <- reactiveValues(login = login)

  observe({
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          studentid <- isolate(input$studentid)

          if (studentid %in% credentials$student_id) {
            USER$login <- TRUE
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        }
      }
    }
  })

  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout",
              href = "javascript:window.location.reload(true)"
    ),
    class = "dropdown",
    style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;"
    )
  })

  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE) {
      sidebarMenu(
        menuItem("Attendance", tabName = "dashboard", icon = icon("fas fa-bell")),
        menuItem("Grade", tabName = "second", icon = icon("fas fa-book-open"))
      )
    }
  })

  output$body <- renderUI({
    if (USER$login == TRUE) {
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
    } else {
      loginpage
    }
  })

  output$results <- DT::renderDataTable({
    datatable(lecn %>%
                filter(Lecture == input$studentid), options = list(
                  autoWidth = TRUE,
                  searching = FALSE
                ))
  })

  output$results2 <- DT::renderDataTable({
    datatable(lecn %>%
                filter(Lecture == input$studentid), options = list(
                  autoWidth = TRUE,
                  searching = FALSE
                ))
  })
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
