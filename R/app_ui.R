#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  options(shiny.maxRequestSize = 55 * 1024^2)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    ui <- dashboardPage(skin = "black",
      dashboardHeader(title = "labelcolor"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Dataset", tabName = "dataset", icon = icon("fas fa-file-arrow-down")),
          menuItem("Plot", tabName = "plot", icon = icon("fas fa-tree")),
          menuItem("Information", tabName = "information", icon = icon("fas fa-info-circle")),
          style = "font-size:20px"
        ),
        column(12,
              downloadButton(outputId = "down", label = "Download the plot", style="color:#000000; display: block"),
              textInput("fname", "Nom du fichier", value = "Plot"),
              radioButtons("ext","Type de fichier", c("png","pdf"), selected = "pdf", inline = TRUE)
              )
      ),
      dashboardBody(
        fluidRow(
          tabItems(
            tabItem(tabName="dataset",
                    mod_data_loading_ui("data_loading_1")
              ),
            tabItem(tabName= "plot",
                    mod_phylo_ui("phylo_1")
              ),
            tabItem(tabName= "information",
                    mod_information_ui("information_1")
              )
            )
          )
        )
      )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext="png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "labelcolor"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
