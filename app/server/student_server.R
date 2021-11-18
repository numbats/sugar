# STUDENT TAB SERVER FUNCTIONS




output$lecturepresent <- renderValueBox({
 present_data<- all_class_attendance%>%
   filter(`Student Email` == as.character(userDetails())) %>%
   filter(Class == input$type)%>%
      pull(Present)



  valueBox(
    paste0(present_data), "Present",
    color = "light-blue"
  )
})

## Absent  ValueBox

output$lectureabsent <- renderValueBox({
  absent_data<- all_class_attendance%>%
    filter(`Student Email` == as.character(userDetails())) %>%
    filter(Class == input$type)%>%
        pull(`Away for portion`)


  valueBox(
    paste0(absent_data), "Away From Portion",
    color = "light-blue"
  )
})

## Excused ValueBox

output$lectureexcused <- renderValueBox({
  excused_data<- all_class_attendance%>%
    filter(`Student Email` == as.character(userDetails())) %>%
    filter(Class == input$type)%>%
    pull(`Excused Absence`)


  valueBox(
    paste0(excused_data), "Excused Absence",
    color = "light-blue"
  )
})

## Unexcused ValueBox

output$lectureunexcused <- renderValueBox({
  unexcused_data<- all_class_attendance%>%
    filter(`Student Email` == as.character(userDetails())) %>%
    filter(Class == input$type)%>%
        pull(`Unexcused Absence`)


  valueBox(
    paste0(unexcused_data), "Unexcused Absence",
    color = "light-blue"
  )
})


# Tutorial attendance

output$tutorialpresent <- renderValueBox({
  tpresent_data<- all_class_attendance%>%
    filter(`Student Email` == as.character(userDetails())) %>%
    filter(grepl('Tutorial|tutorial', Class))%>%
    pull(Present)



  valueBox(
    paste0(tpresent_data), "Present",
    color = "light-blue"
  )
})

## Absent  ValueBox

output$tutorialabsent <- renderValueBox({
  tabsent_data<- all_class_attendance%>%
    filter(`Student Email` == as.character(userDetails())) %>%
    filter(grepl('Tutorial|tutorial', Class))%>%
    pull(`Away for portion`)


  valueBox(
    paste0(tabsent_data), "Away From Portion",
    color = "light-blue"
  )
})

## Excused ValueBox

output$tutorialexcused <- renderValueBox({
  texcused_data<- all_class_attendance%>%
    filter(`Student Email` == as.character(userDetails())) %>%
    filter(grepl('Tutorial|tutorial', Class))%>%

    pull(`Excused Absence`)


  valueBox(
    paste0(texcused_data), "Excused Absence",
    color = "light-blue"
  )
})

## Unexcused ValueBox

output$tutorialunexcused <- renderValueBox({
  tunexcused_data<- all_class_attendance%>%
    filter(`Student Email` == as.character(userDetails())) %>%
    filter(grepl('Tutorial|tutorial', Class))%>%
    pull(`Unexcused Absence`)


  valueBox(
    paste0(tunexcused_data), "Unexcused Absence",
    color = "light-blue"
  )
})


## Retrieval of Marks of Student User

student_grade_show <- reactive({
  grade_student <- staff_student_grades%>%
    filter(`Student Email` == as.character(userDetails())) %>%
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
        filter(ID == selected) %>%
        pull(Assessment)
    }
    grade_table <- student_grade_show()
    total_marks_assessment <- staff_student_grades %>%
      drop_na() %>%
      pivot_longer(cols =  c(!`Student Email`) , names_to = "Assessment", values_to = "Marks") %>%
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


