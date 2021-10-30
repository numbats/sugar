#' Create app skeleton
#'
#' Function to set up app skeleton.
#'
#' @export
 create_app_skeleton <- function(){
   dir.create(here::here("app"))
   appDir <- system.file("app", package = "sugar")
   R.utils::copyDirectory(appDir,to=here::here("app"))

#file.copy()

 }
