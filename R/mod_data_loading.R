#' data_loading UI Function
#'
#' @param id
#'
#' @description A shiny Module.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom utils read.csv
#' @useDynLib labelcolor, .registration = TRUE
mod_data_loading_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Data",status = "primary",solidHeader = TRUE,
        helpText(
        "Choisissez si vous voulez utiliser le dataset demo, importer votre dataset (format .csv avce Header et nom des lignes en premiere colonne)
                                    ou une matrice de distance (format .rds). Puis appuyez sur valider"
        ),
        radioButtons(ns("data"),"",choices = c(
          "demo (RameauEnv_Foret2UNIMARC2.csv)",
          "Dataset binaire (.csv)",
          "Matrice de distance (.rds)"),
        selected = "demo",inline = TRUE),
        br(),
        fileInput(ns("file"), "Importer", accept = c(".csv", ".rds")),
        radioButtons(ns("sep"),"csv separateur",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),
        selected = ","),
        actionButton(ns("val"), "valider"),
        width = 12
    )
   )
  )
}

#' data_loading Server Functions
#'
#' @noRd
#'
# mod_data_loading_server <- function(input, output, session, r){
mod_data_loading_server <- function(id,r=r) {
moduleServer(id, function(input, output, session) {
   ns <- session$ns
   r$df <- eventReactive(input$val,{
      if(input$data == "demo (RameauEnv_Foret2UNIMARC2.csv)"){
        datf <- labelcolor::my_dataset
        return(datf)
      }else if(input$data == "Dataset binaire (.csv)"){
        req(input$file)
        if(tools::file_ext(input$file$name)=="csv"){
          datf <- utils::read.csv(input$file$datapath,
                                  header = TRUE,
                                  sep = input$sep,
                                  row.names =1
          )
          return(datf)
        }else{
          stop("Ce n'est pas un .csv")
        }
      }else{
        req(input$file)
        if(tools::file_ext(input$file$name)=="rds"){
          datf <- readRDS(file(input$file$datapath))
          return(datf)
        }else{
          stop("Ce n'est pas un .rds")
        }
      }
    })
    r$data <- reactive({input$data})
   })

}

## To be copied in the UI
# mod_data_loading_ui("data_loading_1")

## To be copied in the server
# mod_data_loading_server("data_loading_1")
