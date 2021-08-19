library(shiny)
library(fullcalendar)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(googlesheets4)
library(tidyverse)
library(googleAuthR)
library(monash)

# google credentials & scopes

options(googleAuthR.scopes.selected = c(
  "https://www.googleapis.com/auth/userinfo.email",
  "https://www.googleapis.com/auth/userinfo.profile"
))


options("googleAuthR.webapp.client_id" = "1044705167382-idbbqfmpian2ea30gmdc2alktbt133ou.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "1EvRhB6JovB_fxON8cuKx6lz")


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

sheet <- tryCatch(
  {
    gs4_auth(
      cache = ".secrets",
      email = "abab0012@student.monash.edu",
      token = "authentication.rds"
    )
    lec <- gs4_get("125VrIIShEBJ2Xp5YgkzZN3uT-lxg_5c9C5nCgYYlv3M")
    grade <- gs4_get("13dQxAtrtr-0NyMsK9Ex0UOypLeTqgq12oKRkvY7fEXY")
  },
  error = function(e) {
    message("Access has not been granted, please try again in 5 minutes.")
    return(NULL)
  }
)

# Lecture attendance

lecdf <- read_sheet(lec, skip = 1)
lecn <- lecdf[-1, ]
studentidlist <- lecn %>%
  dplyr::select(Lecture)

lecn <- lecn %>%
  mutate_all(funs(type.convert(as.character(.))))

pivot_date <- lecn %>%
  pivot_longer(!c(Lecture, `Student Email`,
                  `Away for portion`, `Excused absence`,
                  `Unexcused absence`),
               names_to = "date",
               values_to = "attendance"
  ) %>%
  mutate(Present = case_when(
    attendance == "P" ~ 1,
    TRUE ~ 0
  ),
  date=paste0(date,"/2021"),
  color=case_when(
    attendance == "P" ~ "#107e3e",
    attendance == "A" ~ "#e9730c",
    attendance == "E" ~ "#0a6ed1",
    attendance == "U" ~ "#bb0000"
  ))





pivot <- pivot_date%>%
  select(`Student Email`, `Away for portion`,
         `Excused absence`, `Unexcused absence`,
         Present) %>%
  rename(email = `Student Email`) %>%
  # filter(email=="abab0012@student.monash.edu")%>%
  group_by(email) %>%
  summarise(
    Present = sum(Present),
    `Away for portion` = max(`Away for portion`),
    `Excused absence` = max(`Excused absence`),
    `Unexcused absence` = max(`Unexcused absence`)
  )
# Tutorial attendance
tutpatdf <- read_sheet(lec, sheet = 2, skip = 1)
tutpatn_new <- tutpatdf[-1, ]
tutpatn_new <- tutpatn_new %>%
  mutate_all(funs(type.convert(as.character(.))))

tutpatn_date <- tutpatn_new %>%
  pivot_longer(!c(`Tutorial A`, `Student Email`,
                  `Away for portion`, `Excused absence`,
                  `Unexcused absence`),
               names_to = "date", values_to = "attendance"
  ) %>%
  mutate(Present = case_when(
    attendance == "P" ~ 1,
    TRUE ~ 0
  ),
  date=paste0(date,"/2021"),
  color=case_when(
    attendance == "P" ~ "#107e3e",
    attendance == "A" ~ "#e9730c",
    attendance == "E" ~ "#0a6ed1",
    attendance == "U" ~ "#bb0000"
  ))

tutpatn <- tutpatn_date%>%
  select(`Student Email`, `Away for portion`,
         `Excused absence`, `Unexcused absence`,
         Present) %>%
  rename(email = `Student Email`) %>%
  group_by(email) %>%
  summarise(
    Present = sum(Present),
    `Away for portion` = max(`Away for portion`),
    `Excused absence` = max(`Excused absence`),
    `Unexcused absence` = max(`Unexcused absence`)
  )



tutsherdf <- read_sheet(lec, sheet = 3, skip = 1)
tutshern_new <- tutsherdf[-1, ]

tutshern_new <- tutshern_new %>%
  mutate_all(funs(type.convert(as.character(.))))

tutshern_date <- tutshern_new %>%
  pivot_longer(!c(`Tutorial B`, `Student Email`, `Away for portion`,
                  `Excused absence`, `Unexcused absence`),
               names_to = "date", values_to = "attendance"
  ) %>%
  mutate(Present = case_when(
    attendance == "P" ~ 1,
    TRUE ~ 0
  ),
  date=paste0(date,"/2021"),
  color=case_when(
    attendance == "P" ~ "#107e3e",
    attendance == "A" ~ "#e9730c",
    attendance == "E" ~ "#0a6ed1",
    attendance == "U" ~ "#bb0000"
  ))


tutshern <- tutshern_date%>%
  select(`Student Email`, `Away for portion`,
         `Excused absence`, `Unexcused absence`, Present) %>%
  rename(email = `Student Email`) %>%
  group_by(email) %>%
  summarise(
    Present = sum(Present),
    `Away for portion` = max(`Away for portion`),
    `Excused absence` = max(`Excused absence`),
    `Unexcused absence` = max(`Unexcused absence`)
  )

`%notin%` <- Negate(`%in%`)
authorised_list <- as.tibble(c(pivot$email, "aarathy.babu@monash.edu"))

# Assessment Info
assessment_info <- read_sheet(grade, sheet = 3)

## Grade
gradedf <- read_sheet(grade, sheet = 2, skip = 1)
graden <- gradedf[-1, ]
graden <- graden[-1, ]

grades_list <- graden %>%
  select(c(`Student Email`, `ASSESS 1`:PRESENTATION)) %>%
  rename(email = `Student Email`)


n_students <- grades_list%>%
  filter(!is.na(email))%>%
  nrow()

staff_student_grades <- grades_list %>%
  filter(!is.na(email))

staff_grades_prefinal<- staff_student_grades%>%
 pivot_longer(cols = `ASSESS 1`:PRESENTATION, values_to = "Obtained Marks",names_to = "Assessment")%>%
  left_join(assessment_info,by="Assessment")%>%
  mutate(Obtained_Percentage=(`Obtained Marks`/`Total Marks`)*Weightage)%>%
  select(email,Assessment,`Obtained Marks`,Obtained_Percentage)

staff_student_grades_final<- staff_grades_prefinal%>%
 group_by(email)%>%
  mutate(Total=sum(Obtained_Percentage))%>%
  select(email,Assessment,`Obtained Marks`,Total)%>%
  pivot_wider(email:Total,names_from = "Assessment",values_from = `Obtained Marks`)%>%
  rename(`Total Marks Obtained` = "Total")%>%
  ungroup()



