
# STAFF TAB SERVER FUNCTIONS

## Attendance for Staff tab

student_attendance <- all_class_attendance %>%
  filter(!is.na(`Student Email`))

call_select_class <- reactive({
  select_class_attendance <- all_class_attendance %>%
    filter(!is.na(`Student Email`)) %>%
    filter(Class == input$type)
  return(select_class_attendance)

})



output$attendance_list <- DT::renderDataTable({

  select_attendance <- call_select_class()
  class_attendance_table <- select_attendance%>%select(-Class)
  datatable(class_attendance_table, options = list(pageLength = 10, initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
    "}"
  )))
})


output$downloadattendance <- downloadHandler(

  filename = function() {
    paste(input$type, "_attendance.csv", sep = "")
  },
  content = function(file) {
    select_attendance <- call_select_class()
    class_attendance_table <- select_attendance%>%select(-Class)
    write.csv(class_attendance_table, file, row.names = FALSE)
  }
)



## Performance of Class in the Unit for Staff Tab

output$unit_performance <- renderPlot({
  staff_student_grades %>%
    drop_na() %>%
    pivot_longer(cols = c(!`Student Email`), names_to = "Assessment", values_to = "Marks") %>%
    ggplot(aes(x = Assessment, y = Marks)) +
    geom_quasirandom(color = "#006DAE") +
    theme_bw() +
    labs(
      title = "Marks Distribution of Unit Assessments",
      y = "Marks",
      x = "Assessment"
    )
})

## Grades of Students for Staff viewing

output$grades_full <- DT::renderDataTable({



  tab <- formattable(
    full_student_grades_list,
    list(
      `ASSESS 1`= formatter("span",style = ~ style(color = ifelse(`ASSESS 1.y` > 49,"green", "red"))),
      `ASSESS 2`= formatter("span",style = ~ style(color = ifelse(`ASSESS 2.y` > 49,"green", "red"))),
      `ASSESS 3`= formatter("span",style = ~ style(color = ifelse(`ASSESS 3.y` > 49,"green", "red"))),
      `BLOG 1`= formatter("span",style = ~ style(color = ifelse(`BLOG 1.y` > 49,"green", "red"))),
      `BLOG 2`= formatter("span",style = ~ style(color = ifelse(`BLOG 2.y` > 49,"green", "red"))),
      `DISCUSS 1`= formatter("span",style = ~ style(color = ifelse(`DISCUSS 1.y` > 49,"green", "red"))),
      `DISCUSS 2`= formatter("span",style = ~ style(color = ifelse(`DISCUSS 2.y` > 49,"green", "red"))),
      `PRESENTATION`= formatter("span",style = ~ style(color = ifelse(`PRESENTATION.y` > 49,"green", "red")))
    )
  )



  tab %>%
    as.datatable(
      caption = "Student Assessment Grades",
      options = list(
        columnDefs = list(list(targets = c(13:20), visible = FALSE)),
        order = list(4, "asc"),
        pageLength = 10,initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
          "}"
        )
      )
    ) %>%
    formatStyle(4, color = JS("value < 49 ? 'red' : 'green'"))
})



output$downloadgrades <- downloadHandler(
  filename = function() {
    paste("Grades.csv", sep = "")
  },
  content = function(file) {
    write.csv(full_student_grades_list, file, row.names = FALSE)
  }
)


output$unit_assessment_info <- DT::renderDataTable({
  datatable(assessment_info %>%
    select(Assessment, `Total Marks`, Weightage), caption = "Unit Assessment Information", options = list(
    pageLength = 10, initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
      "}"
    )
  ))
})
