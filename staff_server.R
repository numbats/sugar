
# STAFF TAB SERVER FUNCTIONS

## Attendance for Staff tab



output$attendance_list <- DT::renderDataTable({
  if (input$type == "Lecture") {
    datatable(pivot %>%
                filter(!is.na(email)), options = list(initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                  "}"
                )))
  } else if (input$type == "Tutorial A") {
    datatable(tutpatn %>%
                filter(!is.na(email)), options = list(initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#006DAE', 'color': '#fff'});",
                  "}"
                )))
  } else {
    datatable(tutshern %>%
                filter(!is.na(email)), options = list(initComplete = JS(
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


