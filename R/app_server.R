#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dynamicTreeCut
#' @import ape
#' @import circlize
#' @import cluster
#' @import DendSer
#' @import seriation
#' @import stats
#' @import grDevices
#' @import dendextend
#' @import shinycustomloader
#' @importFrom utils read.csv
#' @useDynLib labelcolor, .registration = TRUE
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  r <- reactiveValues()
  # callModule(mod_data_loading_server,"data_loading_1", session = session, r = r)
  # callModule(mod_phylo_server,"phylo_1", session=session, r=r)
  mod_data_loading_server("data_loading_1",r=r)
  mod_phylo_server("phylo_1",r=r)
  mod_information_server("information_1")

  output$down <- downloadHandler(
    filename =  function() {
      paste(input$fname,input$ext,sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$ext == "png")
        grDevices::png(file) # open the png device
      else
        grDevices::pdf(file) # open the pdf device
      ape::plot.phylo(as.phylo(r$ch()), type = r$ptype(), cex=r$cex(), tip.color = unlist(unname(r$labr()))) # draw the plot
      grDevices::dev.off()  # turn the device off
      })
}
