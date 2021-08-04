library(shiny)
#library(semantic.dashboard)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(googlesheets4)
library(tidyverse)
library(googleAuthR)

# cred

# options(googleAuthR.scopes.selected =
#   c( "https://www.googleapis.com/auth/userinfo.email",
#    "https://www.googleapis.com/auth/userinfo.profile"))

# options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
#
#                                         "https://www.googleapis.com/auth/userinfo.profile"))

options("googleAuthR.scopes.selected" = "email")

 options("googleAuthR.webapp.client_id" = "1044705167382-idbbqfmpian2ea30gmdc2alktbt133ou.apps.googleusercontent.com")
  options("googleAuthR.webapp.client_secret" = "1EvRhB6JovB_fxON8cuKx6lz")


get_email <- function(){
  e_id<-  gar_api_generator(
    #"https://www.googleapis.com/auth/userinfo.email",
    "https://openidconnect.googleapis.com/v1/userinfo",
                         "POST",
                         data_parse_function = function(x) x$email,
                         checkTrailingSlash = FALSE)
e_id()
}


sheet <- tryCatch({

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


header <- dashboardHeader(title = "SUGAR")

sidebar <- dashboardSidebar(shinyjs::useShinyjs(),uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header,sidebar,body,skin ="purple")



server <- function(input, output, session) {


  login <- FALSE
  USER <- reactiveValues(login = login)

  ## Authentication
  accessToken <- callModule(googleAuth, "gauth_login",
                            login_text = "Sign in via Google",
                            logout_text="Sign Out",
                            login_class = "btn btn-success",
                            logout_class = "btn btn-success")

  logged <- reactive({

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

      menuItem("Student",tabName = "student",icon = icon("fas fa-user")),
      menuItem("Attendance", tabName = "attendance", icon = icon("fas fa-bell")),
      menuItem("Grade", tabName = "grade", icon = icon("fas fa-book-open")),
      googleAuthUI("gauth_login")
    )
    }
     else
       addClass(selector = "body", class = "sidebar-collapse")


  })
    output$picture<- renderImage({
      return(list(
        src = "www/monash-logoar5.png",
        contentType = "image/png",
        width = 420,
        height = 200
      ))
    }, deleteFile = FALSE)

  output$body <- renderUI({
    if(is.null(accessToken())== FALSE)
    {
      tabItems(


        # First tab
        tabItem(
          tabName = "student", class = "active",
          fluidRow(
            br(),
            p("Logged in as: ", textOutput("user_name"))

          )
        ),

        # second tab
        tabItem(
          tabName = "attendance", class = "active",
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

        # third tab
        tabItem(
          tabName = "grade",
          fluidRow(
            box(width = 12, dataTableOutput("results2"))
          )
        )
      )

    }

    else
      fluidRow(
        setBackgroundImage(src = "https://media2.giphy.com/media/dAWZiSMbMvObDWP3aA/giphy.gif?cid=790b76112ebc2cd4920e9b8eee5c21b875d80efb65eac044&rid=giphy.gif&ct=g", shinydashboard = TRUE),
br(),
br(),
          h2("Welcome to Sugar !", style = "text-align:center;color:white;"),
          h3("Shiny Unit Grade and Attendance Reviewer", style = "text-align:center;color:white;"),
          br(),
          column(12,align="center",imageOutput("picture",width="100%",height="200px")),
          p("Shiny Unit Grade and Attendance Reviewer, or SUGAR,
            is a shiny web app that allows students to see their grade and attendance of a unit", style = "text-align:center;color:white;"),
br(),
       column(12,googleAuthUI("gauth_login"),align="center")


)

  }
  )

  userDetails <- reactive({
    validate(
      need(accessToken(), "")
    )

    with_shiny(get_email(), shiny_access_token = accessToken())
  })


output$user_name <- renderText({
  validate(
    need(userDetails(), "")
  )

em <- get_email()
str(em)
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

runApp(list(ui = ui, server = server),port=4549)


