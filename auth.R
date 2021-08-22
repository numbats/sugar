library(googlesheets4)
library(googleAuthR)
# designate project-specific cache
options(gargle_oauth_cache = ".secrets")

# check the value of the option, if you like
gargle::gargle_oauth_cache()

# trigger auth on purpose --> store a token in the specified cache
# a broswer will be opened

googlesheets4::gs4_auth()

# see your token file in the cache, if you like
list.files(".secrets/")

# deauth
gs4_deauth()

# sheets reauth with specified token and email address
gs4_auth(
  cache = ".secrets",
  email = "abab0012@student.monash.edu",
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  token = "authentication.rds"

)

saveRDS(gs4_auth(), "authentication.rds")
#
# gar_auth_configure(path="client.json")
# gar_auth(cache = ".httr-oauth",
#          email = "aarathy.babu@monash.edu",
#          scopes = c("https://www.googleapis.com/auth/userinfo.email",
#                     "https://www.googleapis.com/auth/userinfo.profile"))
# saveRDS(gar_auth(), "garauth.rds")

