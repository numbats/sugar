
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
authorised_list <- as.tibble(c(pivot$email, "aarathy.babu@monash.edu","emi.tanaka@monash.edu"))



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
  tabName = "student_attendance", class = "active",
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
        column(10,
               offset=1,
          fluidRow(
            valueBoxOutput("present", width = 3),
            valueBoxOutput("absent", width = 3),
            valueBoxOutput("excused", width = 3),
            valueBoxOutput("unexcused", width = 3)
          )
        )

      )
    )
  )
)


# Grades tab for student users adjusted to class strength

second_tab <- tabItem(
  tabName = "student_grade",
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
  h3("Grade", style = "text-align:center;color:black;"),
  tabsetPanel(
    tabPanel("Students",
             br(),
      fluidRow(
        box(width = 12, status = "primary", solidHeader = TRUE,dataTableOutput("grades_full"))
    )),
    tabPanel(
      "Unit Information",
      br(),
      box(
        status = "primary", width = 12, solidHeader = TRUE,
        fluidRow(column(6,
          align = "center", plotOutput("unit_performance")),
        column(6,
               align = "center", dataTableOutput("unit_assessment_info")
        ))
      ))

      )
    )


# VIEW AS STUDENT TABS

## Attendance tab

view_first_tab <- tabItem(
  tabName = "student_attendance", class = "active",
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
        column(10,offset=1,
               fluidRow(
                 valueBoxOutput("present", width = 3),
                 valueBoxOutput("absent", width = 3),
                 valueBoxOutput("excused", width = 3),
                 valueBoxOutput("unexcused", width = 3)
               )
        )

      )
    )
  )
)


# Grades tab for student users adjusted to class strength

view_second_tab <- tabItem(
  tabName = "student_grade",
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

