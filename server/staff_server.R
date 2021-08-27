
# STAFF TAB SERVER FUNCTIONS

## Attendance for Staff tab

lecture_attendance <- pivot %>%
  filter(!is.na(email))
tutorial_a_attendance <- tutpatn %>%
  filter(!is.na(email))
tutorial_b_attendance<-   tutshern %>%
  filter(!is.na(email))

output$attendance_list <- DT::renderDataTable({
  if (input$type == "Lecture") {

    datatable(lecture_attendance, options = list(pageLength = 10,initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                  "}"
                )))
  } else if (input$type == "Tutorial A") {

    datatable(tutorial_a_attendance, options = list(pageLength = 10,initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                  "}"
                )))
  } else {


    datatable(tutorial_b_attendance, options = list(pageLength = 10,initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                  "}"
                )))
  }
})


output$downloadattendance <- downloadHandler(


    filename = function() {
    paste(input$type, "_attendance.csv", sep = "")
  },
  content = function(file) {
    if(input$type=="Lecture")
    {
    write.csv(lecture_attendance, file, row.names = FALSE)
    }
  else if (input$type=="Tutorial A")
  {
    write.csv(tutorial_a_attendance, file, row.names = FALSE)

  }
  else
    write.csv(tutorial_b_attendance, file, row.names = FALSE)
}
)



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

 total<-  formatter("span",
            style = x ~ style(color = ifelse(x > 49, "green","red" )))
  tab<- formattable(full_student_grades_list,
     list(

    `ASSESS 1` = formatter("span",
                             style = ~ style(color = ifelse(`ASSESS 1.y` > 49, "green", "red"))),

    `ASSESS 2` = formatter("span",
                             style = ~ style(color = ifelse(`ASSESS 2.y` > 49, "green", "red"))),
    `ASSESS 3` = formatter("span",
                             style = ~ style(color = ifelse(`ASSESS 3.y` > 49, "green", "red"))),
    `BLOG 1` = formatter("span",
                           style = ~ style(color = ifelse(`BLOG 1.y` > 49, "green", "red"))),
    `BLOG 2` = formatter("span",
                           style = ~ style(color = ifelse(`BLOG 2.y` > 49, "green", "red"))),
    `DISCUSS 1` = formatter("span",
                              style = ~ style(color = ifelse(`DISCUSS 1.y` > 49, "green", "red"))),
    `DISCUSS 2` = formatter("span",
                              style = ~ style(color = ifelse(`DISCUSS 2.y` > 49, "green", "red"))),
    `PRESENTATION` = formatter("span",
                                 style = ~ style(color = ifelse(`PRESENTATION.y` > 49, "green", "red")))
  ))

   tab%>%
     as.datatable(caption="Student Assessment Grades",
            options = list(
              columnDefs = list(list(targets = c(11:18), visible = FALSE)),
              order = list(2, 'asc'),
            pageLength = 10,initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                "}"
              ))) %>% formatStyle(2, color = JS("value < 49 ? 'red' : 'green'"))


})





output$downloadgrades <- downloadHandler(


  filename = function() {
    paste("Grades.csv", sep = "")
  },
  content = function(file) {

      write.csv(staff_student_grades_final, file, row.names = FALSE)
  }
)


output$unit_assessment_info <- DT::renderDataTable({

  datatable(assessment_info%>%
              select(Assessment,`Total Marks`,Weightage),caption = "Unit Assessment Information" ,options = list(
                                                                         pageLength = 10,initComplete = JS(
                                                                           "function(settings, json) {",
                                                                           "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                                                                           "}"
                                                                         )))
})



