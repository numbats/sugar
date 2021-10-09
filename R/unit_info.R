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
#'@examples
#'unit_info(unit = "ETC5521",
#'name = "Exploratory Data Analysis",
#'semester=1,year=2020,lecturer =
#'c("Emi Tanaka" = "emi.tanaka@monash.edu",
#'"Di Cook" = "dicook@monash.edu"))
#'
#' @export
unit_info <- function(unit, name, semester,year, lecturer){

  options(useFancyQuotes = FALSE)

   unitinfo <- list(unit = paste0(unit,"S",semester,year),
                    unit_code=as.character(unit),
                    name = sQuote(as.character(name)),
                    lecturer=lecturer)

   class(unitinfo)<- "monash_unit"
assign("unitinfo",unitinfo,envir = .GlobalEnv)


}


