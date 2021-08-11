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

# google credentials & scopes

options(googleAuthR.scopes.selected = c(
  "https://www.googleapis.com/auth/userinfo.email",
  "https://www.googleapis.com/auth/userinfo.profile"
))


options("googleAuthR.webapp.client_id" = "1044705167382-idbbqfmpian2ea30gmdc2alktbt133ou.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "1EvRhB6JovB_fxON8cuKx6lz")


# Function for retrieving email id

get_email <- function() {
  f <- gar_api_generator(
    # "https://www.googleapis.com/auth/userinfo.email",
    "https://openidconnect.googleapis.com/v1/userinfo",
    "POST",
    data_parse_function = function(x) x$email,
    checkTrailingSlash = FALSE
  )
  f()
}


# Get hd

get_hd <- function() {
  f <- gar_api_generator(
    # "https://www.googleapis.com/auth/userinfo.email",
    "https://openidconnect.googleapis.com/v1/userinfo",
    "POST",
    data_parse_function = function(x) x$hd,
    checkTrailingSlash = FALSE
  )
  f()
}

# Retrieve google sheets

sheet <- tryCatch(
  {
    gs4_auth(
      cache = ".secrets",
      email = "abab0012@student.monash.edu",
      token = "authentication.rds"
    )
    lec <- gs4_get("125VrIIShEBJ2Xp5YgkzZN3uT-lxg_5c9C5nCgYYlv3M")
    grade <- gs4_get("13dQxAtrtr-0NyMsK9Ex0UOypLeTqgq12oKRkvY7fEXY")
  },
  error = function(e) {
    message("Access has not been granted, please try again in 5 minutes.")
    return(NULL)
  }
)

# Lecture attendance

lecdf <- read_sheet(lec, skip = 1)
lecn <- lecdf[-1, ]
studentidlist <- lecn %>%
  dplyr::select(Lecture)

lecn <- lecn %>%
  mutate_all(funs(type.convert(as.character(.))))

pivot <- lecn %>%
  pivot_longer(!c(Lecture, `Student Email`,
                  `Away for portion`, `Excused absence`,
                  `Unexcused absence`),
    names_to = "date",
    values_to = "attendance"
  ) %>%
  mutate(Present = case_when(
    attendance == "P" ~ 1,
    TRUE ~ 0
  )) %>%
  select(`Student Email`, `Away for portion`,
         `Excused absence`, `Unexcused absence`,
         Present) %>%
  rename(email = `Student Email`) %>%
  # filter(email=="abab0012@student.monash.edu")%>%
  group_by(email) %>%
  summarise(
    Present = sum(Present),
    `Away for portion` = max(`Away for portion`),
    `Excused absence` = max(`Excused absence`),
    `Unexcused absence` = max(`Unexcused absence`)
  )
# Tutorial attendance
tutpatdf <- read_sheet(lec, sheet = 2, skip = 1)
tutpatn <- tutpatdf[-1, ]
tutpatn <- tutpatn %>%
  mutate_all(funs(type.convert(as.character(.))))

tutpatn <- tutpatn %>%
  pivot_longer(!c(`Tutorial A`, `Student Email`,
                  `Away for portion`, `Excused absence`,
                  `Unexcused absence`),
    names_to = "date", values_to = "attendance"
  ) %>%
  mutate(Present = case_when(
    attendance == "P" ~ 1,
    TRUE ~ 0
  )) %>%
  select(`Student Email`, `Away for portion`,
         `Excused absence`, `Unexcused absence`,
         Present) %>%
  rename(email = `Student Email`) %>%
  group_by(email) %>%
  summarise(
    Present = sum(Present),
    `Away for portion` = max(`Away for portion`),
    `Excused absence` = max(`Excused absence`),
    `Unexcused absence` = max(`Unexcused absence`)
  )



tutsherdf <- read_sheet(lec, sheet = 3, skip = 1)
tutshern <- tutsherdf[-1, ]

tutshern <- tutshern %>%
  mutate_all(funs(type.convert(as.character(.))))

tutshern <- tutshern %>%
  pivot_longer(!c(`Tutorial B`, `Student Email`, `Away for portion`,
                  `Excused absence`, `Unexcused absence`),
    names_to = "date", values_to = "attendance"
  ) %>%
  mutate(Present = case_when(
    attendance == "P" ~ 1,
    TRUE ~ 0
  )) %>%
  select(`Student Email`, `Away for portion`,
         `Excused absence`, `Unexcused absence`, Present) %>%
  rename(email = `Student Email`) %>%
  group_by(email) %>%
  summarise(
    Present = sum(Present),
    `Away for portion` = max(`Away for portion`),
    `Excused absence` = max(`Excused absence`),
    `Unexcused absence` = max(`Unexcused absence`)
  )



## Grade
gradedf <- read_sheet(grade, sheet = 2, skip = 1)
graden <- gradedf[-1, ]
graden <- graden[-1, ]

grades_list <- graden %>%
  select(c(`Student Email`, `ASSESS 1`:PRESENTATION)) %>%
  rename(email = `Student Email`)


# Calender

data <- data.frame(
  title = paste("Event", 1:3),
  start = c("2021-08-01", "2021-08-02", "2021-08-03"),
  end = c("2021-08-02", "2021-08-03", "2021-08-04"),
  color = c("red", "blue", "green")
)


# Dashboard ui & server

header <- dashboardHeader(title = "SUGAR")

sidebar <- dashboardSidebar(shinyjs::useShinyjs(), uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")

# first tab
first_tab <- tabItem(
  tabName = "attendance", class = "active",
  fluidRow(
    br(),
    p("Welcome ! ", textOutput("user_name")),
    selectInput(
      "type",
      "Select class",
      c("Lecture", "Tutorial"),
      selected = NULL
    ),
    valueBoxOutput("present",width = 3),
    valueBoxOutput("absent",width = 3),
    valueBoxOutput("excused",width = 3),
    valueBoxOutput("unexcused",width = 3),
    box(width = 12, dataTableOutput("results")),
    column(12,
           align = "center",
           fullcalendarOutput("calendar", width = "50%", height = "50%")
    )
  )
)

second_tab <- tabItem(
  tabName = "grade",
  fluidRow(
    box(width = 12, dataTableOutput("results2"))
  )
)

staff_second_tab <- tabItem(
  tabName = "grade",
  fluidRow(
    box(width = 12, dataTableOutput("full"))
  )
)

staff_first_tab <-  tabItem(
  tabName = "attendance", class = "active",
  fluidRow(
    br(),
    p("Welcome ! ", textOutput("user_name")),
    selectInput(
      "type",
      "Select class",
      c("Lecture", "Tutorial A","Tutorial B"),
      selected = NULL
    ),
    box(width = 12, dataTableOutput("results"))
  )
)

landing_page <- fluidRow(
  # setBackgroundImage(src ="https://www.colorhexa.com/006dae.png"
  #                      # "https://ohgm.co.uk/wp-content/uploads/2015/09/500x500-hor.gif"
  #                     # "https://media2.giphy.com/media/dAWZiSMbMvObDWP3aA/giphy.gif?cid=790b76112ebc2cd4920e9b8eee5c21b875d80efb65eac044&rid=giphy.gif&ct=g"
  #                    , shinydashboard = TRUE),
  br(),
  br(),
  h2("Welcome to Sugar !", style = "text-align:center;color:black;"),
  h3("Shiny Unit Grade and Attendance Reviewer", style = "text-align:center;color:black;"),
  br(),
  column(12, align = "center", imageOutput("picture", width = "100%", height = "230px")),
  p("Shiny Unit Grade and Attendance Reviewer, or SUGAR,
            is a shiny web app that allows students to see their grade and attendance of a unit", style = "text-align:center;color:black;"),
  br(),
  column(12, googleAuthUI("gauth_login"), align = "center")
)
`%notin%` <- Negate(`%in%`)
authorised_list <- as.tibble(c(pivot$email,"aarathy.babu@monash.edu"))



## error page

error_page <- fluidRow(
  # setBackgroundImage(src ="https://www.colorhexa.com/006dae.png"
  #                      # "https://ohgm.co.uk/wp-content/uploads/2015/09/500x500-hor.gif"
  #                      #"https://media2.giphy.com/media/dAWZiSMbMvObDWP3aA/giphy.gif?cid=790b76112ebc2cd4920e9b8eee5c21b875d80efb65eac044&rid=giphy.gif&ct=g"
  #                    , shinydashboard = TRUE),
  br(),
  br(),
  h3("Unauthorized Access", style = "text-align:center;color:black;"),
  column(12, actionBttn(
    inputId = "back",
    label = "Go back to home page",
    color = "success",
    style = "simple",
    block = FALSE
  ),
        # actionButton("back", "Go back to home page"),
  align = "center"),
  br())


server <- function(input, output, session) {


  USER <- reactiveValues(login = FALSE)

 # Authentication
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


  output$user_name <- renderText({
    validate(
      need(userDetails(), "")
    )

    userDetails()
  })
#& (is.element(as.character(userDetails()), authorised_list$value)==TRUE)


  output$sidebarpanel <- renderUI({
    if ((is.null(accessToken()) == FALSE )) {
      if((is.element(as.character(userDetails()), authorised_list$value)==TRUE)){
      sidebarMenu(
       # menuItem("Student", tabName = "student", icon = icon("fas fa-user")),
        menuItem("Attendance", tabName = "attendance", icon = icon("fas fa-bell")),
        menuItem("Grade", tabName = "grade", icon = icon("fas fa-book-open")),
        googleAuthUI("gauth_login")
      )
      } else
      addClass(selector = "body", class = "sidebar-collapse")
      }
    else {
      addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  output$picture <- renderImage(
    {
    #  monash::logo_get(path="www/",color = "black")
      return(list(
        src = "www/blc.png",
        contentType = "image/png",
        width = 420,
        height = 200
      ))
    },
    deleteFile = FALSE
  )



  userhd <- reactive({
    validate(
      need(accessToken(), "")
    )

    with_shiny(get_hd, shiny_access_token = accessToken())
  })


  output$body <- renderUI({

if ((is.null(accessToken()) == FALSE )) {
if((is.element(as.character(userDetails()), authorised_list$value)==TRUE)){
      if(as.character(userhd())=="student.monash.edu")
        {
      tabItems(
        # # student tab
        # tabItem(
        #   tabName = "student", class = "active",
        #   fluidRow(
        #     br(),
        #     p("Welcome ! ", textOutput("user_name"))
        #   )
        # ),
        # first tab
        first_tab,

        # second tab
        second_tab
      )
    }

    else {

      tabItems(
        # first tab
        staff_first_tab,

        # second tab

        staff_second_tab
      )

    }}
  else
    error_page

  }
    else {landing_page}
  })





  output$present <- renderValueBox({

    if (input$type == "Lecture") {
    presentnum <-  pivot %>%
        filter(email == as.character(userDetails()))%>%
        pull(Present)

    }

      else {
        if (as.character(userDetails()) %in% tutpatn$email) {
          presentnum <-  tutpatn%>%
            filter(email == as.character(userDetails()))%>%
            pull(Present)
        } else {
          presentnum <-  tutshern %>%
            filter(email == as.character(userDetails()))%>%
            pull(Present)
        }
      }

    valueBox(
      paste0(presentnum), "Present", icon = icon("list"),
      color = "purple"
    )
  })


  output$results <- DT::renderDataTable({

    if (input$type == "Lecture") {
      datatable(pivot%>%
                  filter(!is.na(email)))
    }
    else if (input$type == "Tutorial A") {
        datatable(tutpatn%>%
                    filter(!is.na(email)))
      }
    else {
        datatable(tutshern%>%
                    filter(!is.na(email)))
      }

  })


  output$absent <- renderValueBox({

    if (input$type == "Lecture") {
      absentnum <-  pivot %>%
        filter(email == as.character(userDetails()))%>%
        pull(`Away for portion`)

    }

      else {
        if (as.character(userDetails()) %in% tutpatn$email) {
          absentnum <-  tutpatn %>%
            filter(email == as.character(userDetails()))%>%
            pull(`Away for portion`)
        } else {
          absentnum <-  tutshern %>%
            filter(email == as.character(userDetails()))%>%
            pull(`Away for portion`)
        }
      }

    valueBox(
      paste0(absentnum), "Absent", icon = icon("list"),
      color = "yellow"
    )
  })

  output$excused <- renderValueBox({

    if (input$type == "Lecture") {
      excusednum <-  pivot %>%
        filter(email == as.character(userDetails()))%>%
        pull(`Excused absence`)

    }

      else {
        if (as.character(userDetails()) %in% tutpatn$email) {
          excusednum <-  tutpatn %>%
            filter(email == as.character(userDetails()))%>%
            pull(`Excused absence`)
        } else {
          excusednum <-  tutshern%>%
            filter(email == as.character(userDetails()))%>%
            pull(`Excused absence`)
        }
      }

    valueBox(
      paste0(excusednum), "Excused Absence", icon = icon("list"),
      color = "aqua"
    )
  })

  output$unexcused <- renderValueBox({

    if (input$type == "Lecture") {
      unexcusednum <-  pivot %>%
        filter(email == as.character(userDetails()))%>%
        pull(`Unexcused absence`)

    }
       else {
        if (as.character(userDetails()) %in% tutpatn$email) {
          unexcusednum <-  tutpatn %>%
            filter(email == as.character(userDetails()))%>%
            pull(`Unexcused absence`)
        } else {
          unexcusednum <- tutshern %>%
            filter(email == as.character(userDetails()))%>%
            pull(`Unexcused absence`)
        }
      }

    valueBox(
      paste0(unexcusednum), "Unexcused Absence", icon = icon("list"),
      color = "purple"
    )
  })

  output$results2 <- DT::renderDataTable({
    datatable(grades_list %>%
      filter(email == as.character(userDetails())))
  })

  output$full <- DT::renderDataTable({
    datatable(grades_list%>%
                filter(!is.na(email)))
  })
  output$calendar <- renderFullcalendar({
    fullcalendar(data)
  })

  observe({
    if (USER$login) {
      shinyjs::onclick(
        "gauth_login-googleAuthUi",
        shinyjs::runjs("window.location.href = 'http://127.0.0.1:4549';")
      )
    }
  })
  observeEvent(input$back, {
    shinyjs::onclick(
      "back",
      shinyjs::runjs("window.location.href = 'http://127.0.0.1:4549';")
    )
  })


}

runApp(list(ui = ui, server = server),launch.browser=TRUE, port = 4549)
