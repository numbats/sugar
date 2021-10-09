#' Initiate the authorization
#'
#' Function to set up googlesheets authorisation
#' @param setup_info app set up information
#'@export
 initiate_auth <- function(setup_info)
 {
# designate project-specific cache
options(gargle_oauth_cache = ".secrets")

# check the value of the option, if you like
gargle::gargle_oauth_cache()


# trigger auth on purpose --> store a token in the specified cache
# a broswer will be opened
googlesheets4::gs4_auth()


# deauth
googlesheets4::gs4_deauth()

# sheets reauth with specified token and email address
googlesheets4::gs4_auth(
  cache = ".secrets",
  email = as.character(setup_info$maintainer),
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  token = "authentication.rds"

)

saveRDS(googlesheets4::gs4_auth(), here::here("app/authentication.rds"))
}

