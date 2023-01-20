#' information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_information_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$div(br(),
               "Contact the maintainer of the app, Guillaume Sapriel: ", icon("envelope"), tags$b("guillaume.sapriel@uvsq.fr"),
               br(),br(),
               "We are grateful to the INRAE MIGALE bioinformatics facility (MIGALE, INRAE, 2020. Migale bioinformatics Facility, doi: 10.15454/1.5572390655343293E12) for providing help and storage resources."
      )
    )
  )
}

#' information Server Functions
#'
#' @noRd
mod_information_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_information_ui("information_1")

## To be copied in the server
# mod_information_server("information_1")
