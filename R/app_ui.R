#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
options(shiny.maxRequestSize = 55 * 1024^2)

app_ui <- function(request) {
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
          style = "font-size:20px"
        ),
        br(),
        br(),
        br(),
        br(),
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
              box(title = "Data", status = "primary", solidHeader = TRUE,
                helpText("Choisissez si vous voulez utiliser le dataset demo, importer votre dataset (format .csv avce Header et nom des lignes en premiere colonne)
                                    ou une matrice de distance (format .rds). Puis appuyez sur valider"),
                radioButtons("data","",choices = c("demo (RameauEnv_Foret2UNIMARC2.csv)","Dataset binaire (.csv)","Matrice de distance (.rds)"), selected = "demo", inline=TRUE),
                br(),
                fileInput("file","Importer", accept = c(".csv",".rds")),
                radioButtons("sep", "csv separateur",
                              choices = c(Comma = ",",
                              Semicolon = ";",
                              Tab = "\t"),
                              selected = ","),
                actionButton("val","valider"),
                width = 12
                )
              ),
            tabItem(tabName= "plot",
                    box(title = "Inputs", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                        helpText(h2("Creation Phylogenie")),
                        column(4,
                               selectInput('inDist',"Distance", c("euclidean","maximum",
                                                                  "manhattan","canberra",
                                                                  "binary","minkowski"), selected = "binary"),
                        ),
                        column(4,
                               selectInput('inHC',"Clustering hierarchique", c("ward.D","ward.D2",
                                                                               "single","complete",
                                                                               "average","mcquitty",
                                                                               "median","centroid","diana")),
                        ),
                        column(4,
                          radioButtons("ser","Seriation", choices = c("Oui","Non"), selected="Oui", inline = TRUE),
                        ),
                        br(),
                        br(),
                        br(),
                        helpText(h2("Decoupe de l'arbre")),
                        radioButtons("coupe", "methode", choices = c("cutree","cutreeHybrid"),
                                     selected = "cutreeHybrid", inline = TRUE),
                        column(6,
                               helpText(h4("Parametre cutree")),
                               numericInput("K","k",value=3,min=1)
                               ),
                        column(6,
                               helpText(h4("Parametres cutreeHybrid")),
                               numericInput("minsize", "MinClusterSize", value = 1, min = 1),
                               sliderInput("ds", "deepSplit", value = 0, min = 0, max = 4),
                        ),
                        helpText(h2("Affichage Phylogenie")),
                        column(6,
                               selectInput('ptype',"type", c("radial", "phylogram",
                                                      "cladogram", "fan",
                                                      "unrooted","tidy")
                                    ),
                        ),
                        column(6,
                               numericInput("cex", "Cex", value = 0.3, min = 0),
                        ),
                        width=12
                      ),
                    box(title = "Plot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        plotOutput("pphylo", height = "600px"),
                        width=12
                    ),
                    box(title = "clusters", status = "success", solidHeader = TRUE,
                        verbatimTextOutput("clust"),
                        width=12)
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
