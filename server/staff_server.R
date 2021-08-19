
# STAFF TAB SERVER FUNCTIONS

## Attendance for Staff tab



output$attendance_list <- DT::renderDataTable({
  if (input$type == "Lecture") {
    datatable(pivot %>%
                filter(!is.na(email)), options = list(pageLength = 10,initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                  "}"
                )))
  } else if (input$type == "Tutorial A") {
    datatable(tutpatn %>%
                filter(!is.na(email)), options = list(pageLength = 10,initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                  "}"
                )))
  } else {
    datatable(tutshern %>%
                filter(!is.na(email)), options = list(pageLength = 10,initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                  "}"
                )))
  }
})




## Performance of Class in the Unit for Staff Tab

output$unit_performance <- renderPlot({
  grades_list %>%
    drop_na() %>%
    pivot_longer(cols = `ASSESS 1`:PRESENTATION, names_to = "Assessment", values_to = "Marks") %>%
    ggplot(aes(x = Assessment, y = Marks)) +
    geom_boxplot(fill="#006DAE") +
    theme_bw() +
    labs(
      title = "Marks Distribution of Unit Assessments",
      y = "Marks",
      x = "Assessment"
    )
})

## Grades of Students for Staff viewing

output$grades_full <- DT::renderDataTable({
  datatable(staff_student_grades_final,caption="Student Assessment Grades", options = list(order = list(2, 'asc'),
                                                       pageLength = 10,initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                "}"
              )))%>%
    formatStyle(2, backgroundColor = JS("value < 49 ? 'red' : 'lightgreen'"))
})

output$unit_assessment_info <- DT::renderDataTable({

  datatable(assessment_info%>%
              select(Assessment,`Total Marks`,Weightage),caption = "Unit Assessment Information" ,options = list(
                                                                         pageLength = 10,initComplete = JS(
                                                                           "function(settings, json) {",
                                                                           "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                                                                           "}"
                                                                         )))
})


output$percentile_list_students <- DT::renderDataTable({

percentile_full_list<- staff_student_grades_final%>%
    pivot_longer(cols = 3:ncol(staff_student_grades_final), names_to = "Assessment", values_to = "Marks")%>%
    group_by(Assessment)%>%
    summarise(Threshold = quantile(Marks, c(0.25, 0.5, 0.75,0.95)), quantile = c(25, 50, 75,95))%>%
  mutate(threshold_marks=as.numeric(Threshold))%>%
  ungroup()

percentile_threshold_list <- percentile_full_list%>%
  filter((Assessment==as.character(quote(!!input$assessment)))&(quantile==as.numeric(input$percentile)))%>%
  pull(as.numeric(threshold_marks))


students_below_percentile <- staff_student_grades_final%>%
  filter((!!input$assessment<percentile_threshold_list))


datatable(students_below_percentile %>%
            arrange(!!input$assessment),caption = paste0( "Students below \n",as.character(input$percentile),"\nPercentile") ,options = list(
              pageLength = 10,initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                "}"
              )))%>%
  formatStyle(2, backgroundColor = JS("value < 49 ? 'red' : 'lightgreen'"))
})



