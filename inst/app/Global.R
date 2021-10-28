
# google credentials & scopes

#
#
# # set the scopes required
# scopes <- c(
#   "https://www.googleapis.com/auth/userinfo.email",
#   "https://www.googleapis.com/auth/userinfo.profile"
# )
#
# # set the client
# gar_set_client(scopes = scopes, activate = "web")

options(googleAuthR.scopes.selected = c(
  "https://www.googleapis.com/auth/userinfo.email",
  "https://www.googleapis.com/auth/userinfo.profile"
))


# Function for retrieving email id

get_email <- function() {
  f <- gar_api_generator(
    # "https://www.googleapis.com/auth/userinfo.email",
    "https://openidconnect.googleapis.com/v1/userinfo",
    "POST",
    data_parse_function = function(x) x$email,
    checkTrailingSlash = FALSE
  )
  f()
}


# Get hd

get_hd <- function() {
  f <- gar_api_generator(
    # "https://www.googleapis.com/auth/userinfo.email",
    "https://openidconnect.googleapis.com/v1/userinfo",
    "POST",
    data_parse_function = function(x) x$hd,
    checkTrailingSlash = FALSE
  )
  f()
}

# Retrieve google sheets

# Student details

student_details <- read_sheet(student_sheets)%>%
  mutate(Name=paste(Firstname,Lastname,sep = " "))%>%
  select(c(-Firstname,-Lastname))%>%
  select(`Student Id`,Name,Email)

# Lecture attendance

attendance_sheets_name <- (attendance_sheets$sheets) %>% as_tibble()

attendance_data <- list()
pivot_attendance_data <- list()

for (i in 1:nrow(attendance_sheets_name))
{
  attendance_data[[i]] <- read_sheet(attendance_sheets, sheet = i) %>%
    mutate_all(funs(type.convert(as.character(.), as.is = TRUE)))

  pivot_attendance_data[[i]] <- attendance_data[[i]] %>%
    pivot_longer(!`Student Email`,
      names_to = "Date",
      values_to = "Attendance"
    ) %>%
    mutate(
      Date = as.Date(Date),
      Attendance = as_factor(Attendance)
    ) %>%
    mutate(Attendance = toupper(Attendance)) %>%
    mutate(
      Present = case_when(
        Attendance == "P" ~ 1,
        TRUE ~ 0
      ),
      `Away for portion` = case_when(
        Attendance == "A" ~ 1,
        TRUE ~ 0
      ),
      `Unexcused Absence` = case_when(
        Attendance == "U" ~ 1,
        TRUE ~ 0
      ),
      `Excused Absence` = case_when(
        Attendance == "E" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    drop_na() %>%
    group_by(`Student Email`) %>%
    summarise(
      Present = sum(Present),
      `Away for portion` = sum(`Away for portion`),
      `Excused Absence` = sum(`Excused Absence`),
      `Unexcused Absence` = sum(`Unexcused Absence`)
    ) %>%
    mutate(Class = attendance_sheets_name$name[i])
}
all_class_attendance <- do.call("rbind", pivot_attendance_data)

all_class_attendance<- all_class_attendance%>%
  left_join(student_details,by=c("Student Email"="Email"))%>%
  select(`Student Id`,Name,`Student Email`,
         `Present`,`Away for portion`,
         `Excused Absence`,`Unexcused Absence`,Class)


# authorization list

auth_list <- read_sheet(authorization_sheets)

authorised_list <- as_tibble(c(unique(all_class_attendance$`Student Email`), auth_list$`Faculty Email`))

`%notin%` <- Negate(`%in%`)

# Assessment Info
assessment_info <- read_sheet(grade_sheets, sheet = 2)

## Grade
grades_data <- read_sheet(grade_sheets, sheet = 1)


n_students <- grades_data %>%
  filter(!is.na(`Student Email`)) %>%
  nrow()

staff_student_grades <- grades_data %>%
  filter(!is.na(`Student Email`))



staff_grades_prefinal <- staff_student_grades %>%
  pivot_longer(cols = c(!`Student Email`), values_to = "Obtained Marks", names_to = "Assessment") %>%
  left_join(assessment_info, by = "Assessment") %>%
  mutate(
    Obtained_Percentage = (`Obtained Marks` / `Total Marks`) * Weightage,
    Percentage = (`Obtained Marks` / `Total Marks`) * 100
  ) %>%
  select(`Student Email`, Assessment, `Obtained Marks`, Percentage, Obtained_Percentage)

staff_student_grades_final <- staff_grades_prefinal %>%
  group_by(`Student Email`) %>%
  mutate(Total = sum(Obtained_Percentage)) %>%
  select(`Student Email`, Assessment, `Obtained Marks`, Total) %>%
  pivot_wider(`Student Email`:Total, names_from = "Assessment", values_from = `Obtained Marks`) %>%
  rename(`Total Marks Obtained` = "Total") %>%
  ungroup()


obt_percent <- staff_grades_prefinal %>%
  select(-c(`Obtained Marks`, Obtained_Percentage)) %>%
  pivot_wider(`Student Email`:Percentage,
    names_from = "Assessment",
    values_from = "Percentage"
  )


full_student_grades_list <- staff_student_grades_final %>%
  left_join(obt_percent, by = "Student Email")


colnames(full_student_grades_list) <- gsub(".x", "", colnames(full_student_grades_list))

full_student_grades_list<- student_details%>%
  full_join(full_student_grades_list,by=c("Email"="Student Email"))
