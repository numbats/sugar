
# Landing Page


landing_page <- fluidRow(
  br(),
  br(),
  h2("Welcome to Sugar !", style = "text-align:center;color:black;"),
  h3("Shiny Unit Grade and Attendance Reviewer", style = "text-align:center;color:black;"),
  br(),
  column(12, align = "center", imageOutput("picture", width = "100%", height = "200px")),
  p("Shiny Unit Grade and Attendance Reviewer, or SUGAR,
            is a shiny web app that allows students to see their grade and attendance of a unit", style = "text-align:center;color:black;"),
  br(),
  column(12, googleAuthUI("gauth_login"), align = "center")
)
`%notin%` <- Negate(`%in%`)
authorised_list <- as.tibble(c(pivot$email, "aarathy.babu@monash.edu"))



# Error page for unauthorized access

error_page <- fluidRow(
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
  align = "center"
  ),
  br()
)


# STUDENT TABS

## Attendance tab for student users

first_tab <- tabItem(
  tabName = "attendance", class = "active",
  fluidRow(
    br(),
    box(
      status = "primary", width = 12, solidHeader = TRUE,
      h3("Attendance", style = "text-align:center;color:black;"),
      br(),
      selectInput(
        width = "40%",
        "type",
        "Select class",
        c("Lecture", "Tutorial"),
        selected = NULL
      ),
      fluidRow(
        br(),
        column(5,
          align = "center",
          fullcalendarOutput("calendar", width = "100%", height = "100%")
        ),
        column(5,
          offset = 1,
          fluidRow(
            valueBoxOutput("present", width = 6),
            valueBoxOutput("absent", width = 6)
          ),
          fluidRow(
            valueBoxOutput("excused", width = 6),
            valueBoxOutput("unexcused", width = 6)
          )
        )

      )
    )
  )
)


# Grades tab for student users adjusted to class strength

second_tab <- tabItem(
  tabName = "grade",
  fluidRow(
    br(),
    if (n_students > 15) {
      box(
        status = "primary", width = 12,
        fluidRow(
          h3("Grade", style = "text-align:center;color:black;"),
          br(),
          column(width = 6, dataTableOutput("student_user_grades", width = "100%", height = "auto")),
          column(width = 6, plotOutput("histogram"))
        )
      )
    } else {
      box(
        status = "primary", width = 12, height = "600px", solidHeader = TRUE,
        fluidRow(
          h3("Grade", style = "text-align:center;color:black;"),
          br(),
          column(
            width = 6, offset = 3,
            dataTableOutput("student_user_grades", width = "100%", height = "auto")
          )
        )
      )
    }
  )
)

# STAFF TABS

## Class attendance tab for staff users
staff_first_tab <- tabItem(
  tabName = "attendance", class = "active",
  fluidRow(
    br(),
    box(
      status = "primary", width = 12, solidHeader = TRUE,
      h3("Attendance", style = "text-align:center;color:black;"),
      selectInput(
        width = "40%",
        "type",
        "Select class",
        c("Lecture", "Tutorial A", "Tutorial B"),
        selected = NULL
      ),
      box(width = 12, dataTableOutput("attendance_list"))
    )
  )
)

## Class grades tab for staff users

staff_second_tab <- tabItem(
  tabName = "grade",
  tabsetPanel(
    tabPanel("Students", box(
      status = "primary", width = 12, solidHeader = TRUE, height = "600px",
      h3("Grade", style = "text-align:center;color:black;"), fluidRow(
        box(width = 12, dataTableOutput("grades_full"))
      )
    )),
    tabPanel(
      "Summary Statistics",
      box(
        status = "primary", width = 12, solidHeader = TRUE,
        fluidRow(column(6,
          align = "center", plotOutput("unit_performance")
        ))
      )
    ),
    tabPanel(
      "Percentile Student",
      box(
        status = "primary", width = 12, solidHeader = TRUE, height = "600px",
        fluidRow(box(width = 12, dataTableOutput("percentile")))
      )
    )
  )
)


