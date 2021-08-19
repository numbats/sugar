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


# Dashboard ui & server

header <- dashboardHeader(title = "SUGAR")

sidebar <- dashboardSidebar(shinyjs::useShinyjs(), uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")

# server function
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


    # Retrieving hd of email id : @student.monash.edu / monash.edu

  userhd <- reactive({
    validate(
      need(accessToken(), "")
    )

    with_shiny(get_hd, shiny_access_token = accessToken())
  })


      # Body of app

  output$body <- renderUI({
    if ((is.null(accessToken()) == FALSE)) {
      if ((is.element(as.character(userDetails()), authorised_list$value) == TRUE)) {
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

  # STUDENT TAB SERVER FUNCTIONS

  ## Student Attendance Calender

  output$calendar <- renderFullcalendar({
    if (input$type == "Lecture") {

      # Calender_lecture
      cal_date <- pivot_date %>%
        filter(`Student Email` == as.character(userDetails())) %>%
        drop_na()
    } else {
      if (as.character(userDetails()) %in% tutpatn$email) {
        cal_date <- tutpatn_date %>%
          filter(`Student Email` == as.character(userDetails())) %>%
          drop_na()
      } else {
        cal_date <- tutshern_date %>%
          filter(`Student Email` == as.character(userDetails())) %>%
          drop_na()
      }
    }

    data <- data.frame(
      title = cal_date$attendance,
      start = c(as.Date(unique(cal_date$date), "%d/%m/%Y")),
      end = c(as.Date(unique(cal_date$date), "%d/%m/%Y")),
      color = c(cal_date$color)
    )
    fullcalendar(data)
  })



  ## Present ValueBox

  output$present <- renderValueBox({
    if (input$type == "Lecture") {
      presentnum <- pivot %>%
        filter(email == as.character(userDetails())) %>%
        pull(Present)
    } else {
      if (as.character(userDetails()) %in% tutpatn$email) {
        presentnum <- tutpatn %>%
          filter(email == as.character(userDetails())) %>%
          pull(Present)
      } else {
        presentnum <- tutshern %>%
          filter(email == as.character(userDetails())) %>%
          pull(Present)
      }
    }

    valueBox(
      paste0(presentnum), "Present",
      color = "light-blue"
    )
  })

  ## Absent  ValueBox

  output$absent <- renderValueBox({
    if (input$type == "Lecture") {
      absentnum <- pivot %>%
        filter(email == as.character(userDetails())) %>%
        pull(`Away for portion`)
    } else {
      if (as.character(userDetails()) %in% tutpatn$email) {
        absentnum <- tutpatn %>%
          filter(email == as.character(userDetails())) %>%
          pull(`Away for portion`)
      } else {
        absentnum <- tutshern %>%
          filter(email == as.character(userDetails())) %>%
          pull(`Away for portion`)
      }
    }

    valueBox(
      paste0(absentnum), "Absent",
      color = "light-blue"
    )
  })

  ## Excused ValueBox

  output$excused <- renderValueBox({
    if (input$type == "Lecture") {
      excusednum <- pivot %>%
        filter(email == as.character(userDetails())) %>%
        pull(`Excused absence`)
    } else {
      if (as.character(userDetails()) %in% tutpatn$email) {
        excusednum <- tutpatn %>%
          filter(email == as.character(userDetails())) %>%
          pull(`Excused absence`)
      } else {
        excusednum <- tutshern %>%
          filter(email == as.character(userDetails())) %>%
          pull(`Excused absence`)
      }
    }

    valueBox(
      paste0(excusednum), "Excused Absence",
      color = "light-blue"
    )
  })

  ## Unexcused ValueBox

  output$unexcused <- renderValueBox({
    if (input$type == "Lecture") {
      unexcusednum <- pivot %>%
        filter(email == as.character(userDetails())) %>%
        pull(`Unexcused absence`)
    } else {
      if (as.character(userDetails()) %in% tutpatn$email) {
        unexcusednum <- tutpatn %>%
          filter(email == as.character(userDetails())) %>%
          pull(`Unexcused absence`)
      } else {
        unexcusednum <- tutshern %>%
          filter(email == as.character(userDetails())) %>%
          pull(`Unexcused absence`)
      }
    }

    valueBox(
      paste0(unexcusednum), "Unexcused Absence",
      color = "light-blue"
    )
  })

  ## Retrieval of Marks of Student User

  student_grade_show <- reactive({
    grade_student <- grades_list %>%
      filter(email == as.character(userDetails())) %>%
      pivot_longer(cols = `ASSESS 1`:PRESENTATION, names_to = "Assessment", values_to = "Marks") %>%
      select(-email)
    return(grade_student)
  })

  ## Printing Grades of the student user

  output$student_user_grades <- DT::renderDataTable({
    grade_table <- student_grade_show()
    datatable(grade_table, selection = "single", options = list(dom = "ft", initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
      "}"
    )))
  })



  ## Class Performance in each Assessment

  output$histogram <- renderPlot({
    if (n_students > 15) {
      if (is.null(input$results2_rows_selected) == TRUE) {
        assessment_type <- "ASSESS 1"
      } else {
        selected <- input$results2_rows_selected
        grade_table <- student_grade_show()
        grade_table$ID <- seq.int(nrow(grade_table))
        assessment_type <- grade_table %>%
          filter(ID == selected) %>%
          pull(Assessment)
      }
      grade_table <- student_grade_show()
      total_marks_assessment <- grades_list %>%
        drop_na() %>%
        pivot_longer(cols = `ASSESS 1`:PRESENTATION, names_to = "Assessment", values_to = "Marks") %>%
        filter(Assessment == as.character(assessment_type))

      max_count <- total_marks_assessment %>%
        group_by(Marks) %>%
        count() %>%
        pull(n) %>%
        max()

      total_marks_assessment %>%
        ggplot(aes(x = Marks)) +
        geom_histogram(fill = "#006DAE") +
        geom_vline(colour = "red", linetype = "dashed", xintercept = grade_table %>%
                     filter(Assessment == as.character(assessment_type)) %>%
                     pull(Marks)) +
        geom_text(
          mapping = aes(
            x = Marks,
            y = 1 + nrow(total_marks_assessment %>%
                           filter(Marks == (grade_table %>%
                                              filter(Assessment == as.character(assessment_type)) %>%
                                              pull(Marks)))),
            label = "Your score", angle = 90
          ), nudge_x = 0.03, hjust = 0,
          data = grade_table %>%
            filter(Assessment == as.character(assessment_type))
        ) +
        theme_bw() +
        labs(
          title = "Marks Distribution",
          y = "Number of Students",
          x = "Marks"
        ) +
        ylim(0, 5 + max_count)
    }
  })

  # STAFF TAB SERVER FUNCTIONS

  ## Attendance for Staff tab

  output$attendance_list <- DT::renderDataTable({
    if (input$type == "Lecture") {
      datatable(pivot %>%
        filter(!is.na(email)))
    } else if (input$type == "Tutorial A") {
      datatable(tutpatn %>%
        filter(!is.na(email)))
    } else {
      datatable(tutshern %>%
        filter(!is.na(email)))
    }
  })




  ## Performance of Class in the Unit for Staff Tab

  output$unit_performance <- renderPlot({
    grades_list %>%
      drop_na() %>%
      pivot_longer(cols = `ASSESS 1`:PRESENTATION, names_to = "Assessment", values_to = "Marks") %>%
      ggplot(aes(x = Assessment, y = Marks)) +
      geom_violin() +
      theme_bw() +
      labs(
        title = "Marks Distribution of Unit Assessments",
        y = "Marks",
        x = "Assessment"
      )
  })

   ## Grades of Students for Staff viewing

  output$grades_full <- DT::renderDataTable({
    datatable(grades_list %>%
      filter(!is.na(email)), options = list(initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
      "}"
    )))
  })



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
