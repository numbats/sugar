# STUDENT TAB SERVER FUNCTIONS

## Student Attendance Calender

example_student <- sample(pivot_date$`Student Email`,1)

output$calendar <- renderFullcalendar({
  if (input$type == "Lecture") {

    # Calender_lecture
    cal_date <- pivot_date %>%
      filter(`Student Email` == as.character(example_student)) %>%
      drop_na()
  } else {
    if (as.character(example_student) %in% tutpatn$email) {
      cal_date <- tutpatn_date %>%
        filter(`Student Email` == as.character(example_student)) %>%
        drop_na()
    } else {
      cal_date <- tutshern_date %>%
        filter(`Student Email` == as.character(example_student)) %>%
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
      filter(email == as.character(example_student)) %>%
      pull(Present)
  } else {
    if (as.character(example_student) %in% tutpatn$email) {
      presentnum <- tutpatn %>%
        filter(email == as.character(example_student)) %>%
        pull(Present)
    } else {
      presentnum <- tutshern %>%
        filter(email == as.character(example_student)) %>%
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
      filter(email == as.character(example_student)) %>%
      pull(`Away for portion`)
  } else {
    if (as.character(example_student) %in% tutpatn$email) {
      absentnum <- tutpatn %>%
        filter(email == as.character(example_student)) %>%
        pull(`Away for portion`)
    } else {
      absentnum <- tutshern %>%
        filter(email == as.character(example_student)) %>%
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
      filter(email == as.character(example_student)) %>%
      pull(`Excused absence`)
  } else {
    if (as.character(example_student) %in% tutpatn$email) {
      excusednum <- tutpatn %>%
        filter(email == as.character(example_student)) %>%
        pull(`Excused absence`)
    } else {
      excusednum <- tutshern %>%
        filter(email == as.character(example_student)) %>%
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
      filter(email == as.character(example_student)) %>%
      pull(`Unexcused absence`)
  } else {
    if (as.character(example_student) %in% tutpatn$email) {
      unexcusednum <- tutpatn %>%
        filter(email == as.character(example_student)) %>%
        pull(`Unexcused absence`)
    } else {
      unexcusednum <- tutshern %>%
        filter(email == as.character(example_student)) %>%
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
    filter(email == as.character(example_student)) %>%
    pivot_longer(cols = `ASSESS 1`:PRESENTATION, names_to = "Assessment", values_to = "Marks") %>%
    select(-email)
  return(grade_student)
})

## Printing Grades of the student user

output$student_user_grades <- DT::renderDataTable({
  grade_table <- student_grade_show()
  datatable(grade_table, selection = "single", options = list(pageLength = 10,initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
    "}"
  )))
})



## Class Performance in each Assessment

output$histogram <- renderPlot({
  if (n_students > 15) {
    if (is.null(input$student_user_grades_rows_selected) == TRUE) {
      assessment_type <- "ASSESS 1"
    } else {
      selected <- input$student_user_grades_rows_selected
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

