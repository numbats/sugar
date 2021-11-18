## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(sugar)

## -----------------------------------------------------------------------------
create_app_skeleton()

## -----------------------------------------------------------------------------
unit_details<- unit_info(unit = "ETC5521", 
                         name = "Exploratory Data Analysis",
                         semester=1,year=2020,
                         lecturer = c("Emi Tanaka" = "emi.tanaka@monash.edu",
                         "Di Cook" = "dicook@monash.edu"))

## -----------------------------------------------------------------------------

 setup_info <- setup_info(unit_details,
              client_id="xxxxxxx.apps.googleusercontent.com",
              client_secret="xxxxxx-xxxxxx",
              maintainer="xxxxx@.monash.edu")

