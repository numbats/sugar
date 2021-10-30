#' Create app set up information
#'
#' Function to recieve unit information
#'
#'@param unitinfo  unit information
#'@param maintainer  email id of the maintainer of the application
#'@param client_id Google cloud platform client id
#'@param client_secret Google cloud platform client secret
#'
#'@return a special class, app setup with the set up as a list
#'
#'
#' @export
setup_info <- function(unitinfo,
                       maintainer,
                       client_id,
                       client_secret){



  setupinfo <- list(unit = unitinfo$unit,
                    unit_code=unitinfo$unit_code,
                    name=unitinfo$name,
                    maintainer=maintainer,
                    client_id=(as.character(client_id)),
                    client_secret=(as.character(client_secret)))

  class(setupinfo)<- "app_setup"
  setupinfo

}
