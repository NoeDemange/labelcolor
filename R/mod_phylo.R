#' phylo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_phylo_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Inputs", status = "warning", solidHeader = TRUE, collapsible = TRUE,
          helpText(h2("Creation Phylogenie")),
          column(4,
                 selectInput(ns('inDist'),"Distance", c("euclidean","maximum",
                                                    "manhattan","canberra",
                                                    "binary","minkowski"), selected = "binary"),
          ),
          column(4,
                 selectInput(ns('inHC'),"Clustering hierarchique", c("ward.D","ward.D2",
                                                                 "single","complete",
                                                                 "average","mcquitty",
                                                                 "median","centroid","diana")),
          ),
          column(4,
                 radioButtons(ns('ser'),"Seriation", choices = c("Oui","Non"), selected="Oui", inline = TRUE),
          ),
          br(),
          br(),
          br(),
          helpText(h2("Decoupe de l'arbre")),
          radioButtons(ns('coupe'), "methode", choices = c("cutree","cutreeHybrid"),
                       selected = "cutreeHybrid", inline = TRUE),
          column(6,
                 helpText(h4("Parametre cutree")),
                 numericInput(ns("K"),"k",value=3,min=1)
          ),
          column(6,
                 helpText(h4("Parametres cutreeHybrid")),
                 numericInput(ns("minsize"), "MinClusterSize", value = 1, min = 1),
                 sliderInput(ns("ds"), "deepSplit", value = 0, min = 0, max = 4),
          ),
          helpText(h2("Affichage Phylogenie")),
          column(6,
                 selectInput(ns('ptype'),"type", c("radial", "phylogram",
                                               "cladogram", "fan",
                                               "unrooted","tidy")
                 ),
          ),
          column(6,
                 numericInput(ns("cex"), "Cex", value = 0.3, min = 0),
          ),
          width=12
      ),
      box(title = "Plot", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          withLoader(plotOutput(ns("pphylo"), height = "600px")),
          width=12
      ),
      box(title = "clusters", status = "success", solidHeader = TRUE,
          verbatimTextOutput(ns("clust")),
          width=12)
    )
  )
}

#' phylo Server Functions
#'
#' @noRd
# mod_phylo_server <- function(input, output, session, r=r){
mod_phylo_server <- function(id, r=r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dm <- reactive({
      if(r$data() == "Objet R de type dist (.rds)"){
        dMat <- stats::as.dist(r$df())
        return(dMat)
      }
      else {
        dMat <- stats::dist(x = t(as.matrix(r$df())), method = input$inDist)
        return(dMat)
      }
    })


    r$ch <- reactive({
      if(input$inHC != "diana"){
        HC <- stats::hclust(dm(), method= input$inHC)
      } else{
        HC <- stats::as.hclust(diana(dm())) #HC avec diana du package cluster
      }
      if(input$ser=="Oui"){
        OrdSer <- DendSer(HC, dm(), cost= costARc) #calcul de la seriation avec DendSer du package DendSer
        HC <-  permute(HC, OrdSer)
      }
      return(HC)
    })

    cutch <- reactive({
      if(input$coupe=="cutree"){
        cth <- stats::cutree(r$ch(),k=input$K)
      }else{
        cth <- dynamicTreeCut::cutreeHybrid(r$ch(), as.matrix(dm()), minClusterSize = input$minsize, deepSplit = input$ds, verbose = 0)$labels
      }
      cth <- cth[get_order(r$ch())]
      cth <- cth_numgroup(cth)
    })

    group <- reactive({
      names <- colnames(r$df()) #on recupere les noms des colonnes du data.frame
      namesO <- names[get_order(r$ch())] #on organise les noms d'apres l'ordre du clustering hierarchique
      gr <- list()
      for(i in unique(cutch())){
        if(i!=0){
        gr[[i]] <- names[which(cutch()==i)]
        }
      }
      return(gr)
    })

    output$clust <- renderPrint({
      group()
    })

    r$labr <- reactive({
      VmA <- c(1:length(cutch()))
      mrep <- c(length(cutch()),0)
      vrep <- rep(mrep,length(cutch()))
      vcutch <- cutch()
      for(j in VmA){vcutch[which(vcutch==j)]<-j+vrep[j]}
      pal = colorRamp2(as.integer(levels(as.factor(vcutch))), grDevices::rainbow(n = nlevels(as.factor(vcutch))))
      lab <- list(vcutch)
      for(i in unique(vcutch)){
        if(i!=0) {
          lab[which(vcutch==i)]<-pal(i)
        }
      }
      lab[which(vcutch==0)]<-"black"
      names <- colnames(r$df()) #on recupere les noms des colonnes du data.frame
      namesO <- names[get_order(r$ch())] #on organise les noms d'apres l'ordre du clustering hierarchique
      names(lab) <- namesO #on attribue les noms au vecteur lab
      lab <- lab[names]
    })

    output$pphylo <- renderPlot({
      req(r$data())
      ape::plot.phylo(as.phylo(r$ch()), type = input$ptype, cex=input$cex, tip.color = unlist(unname(r$labr())))
    })

    r$ptype <- reactive({input$ptype})
    r$cex <- reactive({input$cex})

   })
}

## To be copied in the UI
# mod_phylo_ui("phylo_1")

## To be copied in the server
# mod_phylo_server("phylo_1")
