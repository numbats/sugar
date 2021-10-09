
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sugar

<!-- badges: start -->
<!-- badges: end -->

Shiny Unit Grade and Attendance Reviewer, or SUGAR, is a shiny web app
that allows students to see their grade and attendance of a unit.

## Installation

Install the package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("numbats/sugar")
```

# Package flow

1.  Set up Google Cloud Platform credentials and save the json.  
2.  create app skeleton
3.  set the unit info using unit\_info()
4.  set the app setup info using setup\_info()
5.  initialize auth
6.  create googlesheets for the unit
7.  pass the user specific input, setup\_info in user\_input()
8.  launch app
9.  Get google spreadsheet links using get\_spreadsheets\_link
