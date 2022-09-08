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
#' @importFrom utils read.csv
#' @useDynLib labelcolor, .registration = TRUE
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  df <- eventReactive(input$val,{
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

  dm <- reactive({
    if(input$data == "Objet R de type dist (.rds)"){
      dMat <- stats::as.dist(df())
      return(dMat)
    }
    else {
      dMat <- stats::dist(x = t(as.matrix(df())), method = input$inDist)
      return(dMat)
    }
  })


  ch <- reactive({
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
      cth <- stats::cutree(ch(),k=input$K)
    }else{
      cth <- dynamicTreeCut::cutreeHybrid(ch(), as.matrix(dm()), minClusterSize = input$minsize, deepSplit = input$ds, verbose = 0)$labels
    }
    cth <- cth[get_order(ch())]
    cth <- cth_numgroup(cth)
    })

  group <- reactive({
    names <- colnames(df()) #on recupere les noms des colonnes du data.frame
    namesO <- names[get_order(ch())] #on organise les noms d'apres l'ordre du clustering hierarchique
    gr <- list()
    for(i in unique(cutch())){
      gr[[i]] <- names[which(cutch()==i)]
    }
    return(gr)
  })

  output$clust <- renderPrint({
    group()
  })

  labr <- reactive({
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
    names <- colnames(df()) #on recupere les noms des colonnes du data.frame
    namesO <- names[get_order(ch())] #on organise les noms d'apres l'ordre du clustering hierarchique
    names(lab) <- namesO #on attribue les noms au vecteur lab
    lab <- lab[names]
  })

  output$pphylo <- renderPlot({
    ape::plot.phylo(as.phylo(ch()), type = input$ptype, cex=input$cex, tip.color = unlist(unname(labr())))
    })

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
      ape::plot.phylo(as.phylo(ch()), type = input$ptype, cex=input$cex, tip.color = unlist(unname(labr()))) # draw the plot
      grDevices::dev.off()  # turn the device off
      })
}
