#' Create unit information
#'
#' Function to recieve unit information
#'@param unit the unit code
#'@param name string that denotes the name of the unit
#'@param semester Integer that denotes teaching period
#'@param year Year
#'@param lecturer character vector with name and email id of lecturers
#'
#'@return a special class, monash unit with the unit information as a list
#'
#'
#' @export
unit_info <- function(unit, name, semester,year, lecturer){



   unitinfo <- list(unit = paste0(unit,"S",semester,year),
                    unit_code=as.character(unit),
                    name = (as.character(name)),
                    lecturer=lecturer)

   class(unitinfo)<- "monash_unit"
   unitinfo



}


