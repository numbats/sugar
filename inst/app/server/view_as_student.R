# STUDENT TAB SERVER FUNCTIONS

## Student Attendance Calender

example_student <- sample(unique(all_class_attendance$`Student Email`),1)


output$present <- renderValueBox({
  present_data<- all_class_attendance%>%
    dplyr::filter(`Student Email`== as.character(example_student)) %>%
    dplyr::filter(Class == input$type)%>%
    pull(Present)



  valueBox(
    paste0(present_data), "Present",
    color = "light-blue"
  )
})

## Absent  ValueBox

output$absent <- renderValueBox({
  absent_data<- all_class_attendance%>%
    dplyr::filter(`Student Email`== as.character(example_student)) %>%
    dplyr::filter(Class == input$type)%>%
    pull(`Away for portion`)


  valueBox(
    paste0(absent_data), "Away From Portion",
    color = "light-blue"
  )
})

## Excused ValueBox

output$excused <- renderValueBox({
  excused_data<- all_class_attendance%>%
    dplyr::filter(`Student Email`== as.character(example_student)) %>%
    dplyr::filter(Class == input$type)%>%

    pull(`Excused Absence`)


  valueBox(
    paste0(excused_data), "Excused Absence",
    color = "light-blue"
  )
})

## Unexcused ValueBox

output$unexcused <- renderValueBox({
  unexcused_data<- all_class_attendance%>%
    dplyr::filter(`Student Email`== as.character(example_student)) %>%
    dplyr::filter(Class == input$type)%>%
    pull(`Unexcused Absence`)


  valueBox(
    paste0(unexcused_data), "Unexcused Absence",
    color = "light-blue"
  )
})

## Retrieval of Marks of Student User

student_grade_show <- reactive({
  grade_student <- staff_student_grades%>%
    dplyr::filter(`Student Email`== as.character(example_student)) %>%
    pivot_longer(cols = c(!`Student Email`), names_to = "Assessment", values_to = "Marks") %>%
    select(-`Student Email`)
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
      assessment_type <- colnames(staff_student_grades[2])
    } else {
      selected <- input$student_user_grades_rows_selected
      grade_table <- student_grade_show()
      grade_table$ID <- seq.int(nrow(grade_table))
      assessment_type <- grade_table %>%
        dplyr::filter(ID == selected) %>%
        pull(Assessment)
    }
    grade_table <- student_grade_show()
    total_marks_assessment <- staff_student_grades %>%
      drop_na() %>%
      pivot_longer(cols =  c(!`Student Email`) , names_to = "Assessment", values_to = "Marks") %>%
      dplyr::filter(Assessment == as.character(assessment_type))

    max_count <- total_marks_assessment %>%
      group_by(Marks) %>%
      count() %>%
      pull(n) %>%
      max()

    total_marks_assessment %>%
      ggplot(aes(x = Marks)) +
      geom_histogram(fill = "#006DAE") +
      geom_vline(colour = "red", linetype = "dashed", xintercept = grade_table %>%
                   dplyr::filter(Assessment == as.character(assessment_type)) %>%
                   pull(Marks)) +
      geom_text(
        mapping = aes(
          x = Marks,
          y = 1 + nrow(total_marks_assessment %>%
                         dplyr::filter(Marks == (grade_table %>%
                                            dplyr::filter(Assessment == as.character(assessment_type)) %>%
                                            pull(Marks)))),
          label = "Your score", angle = 90
        ), nudge_x = 0.03, hjust = 0,
        data = grade_table %>%
          dplyr::filter(Assessment == as.character(assessment_type))
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


