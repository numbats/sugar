#' Create app set up information
#'
#' Function to recieve unit information
#'
#'@param maintainer  email id of the maintainer of the application
#'@param client_id Google cloud platform client id
#'@param client_secret Google cloud platform client secret
#'
#'@return a special class, app setup with the set up as a list
#'
#'@examples
#'setup_info(unit,maintainer=c("Aarathy Babu"="abab0012@student.monash.edu"))
#'
#' @export
setup_info <- function(unit=unitinfo$unit,unit_code=unitinfo$unit_code,name=unitinfo$name,maintainer,client_id,client_secret){

  options(useFancyQuotes = FALSE)

  setupinfo <- list(unit = unit,
                    unit_code=unitinfo$unit_code,
                    name=name,
                    maintainer=maintainer,
                    client_id=sQuote(as.character(client_id)),
                    client_secret=sQuote(as.character(client_secret)))

  class(setupinfo)<- "app_setup"
  setupinfo

}
