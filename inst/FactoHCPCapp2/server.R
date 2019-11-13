# server script for HCPC
  function(input, output) {
	values <- reactive({
      if (input$metric=="Manhattan") res <- HCPC(resultsHCPCshiny,nb.clust=if (is.null(input$clust)){resClusHCPCshiny } else{input$clust},consol=input$consoli,graph=FALSE,metric="manhattan")
      else res <- HCPC(resultsHCPCshiny,nb.clust=if (is.null(input$clust)){resClusHCPCshiny} else{input$clust},consol=input$consoli,graph=FALSE)
    })
        
  output$NB1 <- renderUI({
     selectInput("nb1",label=NULL, choices=1:nbcolHCPCshiny,selected=nb1dfHCPCshiny,width='51px')
  })

  output$NB2 <- renderUI({
    selectInput("nb2",label=NULL, choices=1:nbcolHCPCshiny,selected=nb2dfHCPCshiny,width='51px')
  })

    output$clusters=renderUI({
      sliderInput("clust",gettext("Number of clusters",domain="R-Factoshiny"),min=2,max=min(10,nbindivHCPCshiny-1),value=if (!is.null(input$clust)) {input$clust} else {resClusHCPCshiny},step=1)
    })
    
    CodeHCPC <- reactive({
      paste0("res.HCPC<-HCPC(",nomDataHCPCshiny,",nb.clust=",input$clust,",consol=",input$consoli,",graph=FALSE",if (input$metric=="Manhattan") {",metric='manhattan'"},")")
    })
    
    PlotTree <- reactive({
      Code <- paste0("plot.HCPC(res.HCPC,choice='tree',title='",input$title3HCPCshiny,"')")
	  res.HCPC <- values()
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))
    })
        
    Plot2Dmap <- reactive({
      Code <- paste0("plot.HCPC(res.HCPC,choice='map',draw.tree=",input$drawtree,",title='",input$title2HCPCshiny,"'",if (!is.null(input$nb1)) {if (as.numeric(input$nb1)!=1 | as.numeric(input$nb2)!=2) paste0(",axes=c(",as.numeric(input$nb1),",",as.numeric(input$nb2),")")},")") 
	  res.HCPC <- values()
	  Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))
    })
    
    Plot3D <- reactive({
      Code <- paste0("plot.HCPC(res.HCPC,choice='3D.map',ind.names=",input$nom3D,",centers.plot=",input$center,",angle=",input$num,",title='",input$title1HCPCshiny,"'",if (!is.null(input$nb1)) {if (as.numeric(input$nb1)!=1 | as.numeric(input$nb2)!=2) paste0(",axes=c(",as.numeric(input$nb1),",",as.numeric(input$nb2),")")},")") 
	  res.HCPC <- values()
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))
    })
    
    output$mapTree <- renderPlot({
      if (!is.null(PlotTree()$Plot)) p <- print(PlotTree()$Plot)
    })
    
    output$map2D <- renderPlot({
      if (!is.null(Plot2Dmap()$Plot)) p <- print(Plot2Dmap()$Plot)
    })
    
    output$map3D <- renderPlot({
      if (!is.null(Plot3D()$Plot)) p <- print(Plot3D()$Plot)
    })
        
    output$sorties=renderTable({
      if(input$out=="axe") return(as.data.frame(values()$desc.axes))
      if(input$out=="para") return(as.data.frame(values()$ind.desc))
    },rownames=TRUE)

    output$JDD=DT::renderDataTable({
      cbind(Names=rownames(x),x)},
      options = list( "orderClasses" = TRUE, "responsive" = TRUE, "pageLength" = 10))
    
  observe({
    if(input$Investigatehtml!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveHCPCshiny)
        FactoInvestigate::Investigate(values(), openFile=TRUE, file = input$titleFile, language= substr(tolower(input$choixLANG),1,2))
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$Investigatedoc!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveHCPCshiny)
        FactoInvestigate::Investigate(values(),document="word_document",openFile=TRUE, file = input$titleFile, language= substr(tolower(input$choixLANG),1,2))
        setwd(path.aux)
      })
    }
  })

    output$downloadInvestigateRmd <- downloadHandler(
     filename = function() {
      paste(input$titleFile, ".Rmd", sep="")
    },
    content = function(file) {
        path.aux <- getwd()
        setwd(pathsaveHCPCshiny)
	    FactoInvestigate::Investigate(values(), openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = "Investigate", language= substr(tolower(input$choixLANG),1,2))
	    print(paste0(gettext("The file ",domain="R-Factoshiny"),input$titleFile,gettext(" as well as the RData objects are available in the sub-directory: ",domain="R-Factoshiny"),getwd()))
        setwd(path.aux)
    }
  )

  output$downloadData = downloadHandler(
      filename = function() { 
        paste('Plot2Dmap','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,Plot2Dmap()$Plot)
      },
      contentType='image/png')
    
    output$downloadData1 = downloadHandler(
      filename = function() { 
        paste('Plot2Dmap','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,Plot2Dmap()$Plot)
      },
      contentType='image/jpg')
    
    output$downloadData2 = downloadHandler(
      filename = function() { 
        paste('Plot2Dmap','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,Plot2Dmap()$Plot)
      },
      contentType=NA)
    
    output$downloadData3 = downloadHandler(
      filename = function() { 
        paste('Plot3D','.png', sep='')
      },
      content = function(file) {
        ggplot2::ggsave(file,Plot3D()$Plot)
      },
      contentType='image/png')
    
    output$downloadData4 = downloadHandler(
      filename = function() { 
        paste('Plot3D','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,Plot3D()$Plot)
      },
      contentType='image/jpg')
    
    output$downloadData5 = downloadHandler(
      filename = function() { 
        paste('Plot3D','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,Plot3D()$Plot)
      },
      contentType=NA)
    
    output$downloadData6 = downloadHandler(
      filename = function() { 
        paste('PlotTree','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,PlotTree()$Plot)
      },
      contentType='image/png')
    
    output$downloadData7 = downloadHandler(
      filename = function() { 
        paste('PlotTree','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,PlotTree()$Plot)
      },
      contentType='image/jpg')
    
    output$downloadData8 = downloadHandler(
      filename = function() { 
        paste('PlotTree','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,PlotTree()$Plot)
      },
      contentType=NA)
        
    ### Fonction permettant d'afficher la description des classes par les variables
    output$descript=renderTable({
      write.infile(X=values()$desc.var$quanti,file=paste(getwd(),"essai.csv"),sep=";")
      baba=read.csv(paste(getwd(),"essai.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      b=which(baba[,1]=="format non affichable")
      file.remove(paste(getwd(),"essai.csv")) 
      baba[,-ncol(baba)]
    },
    rownames=FALSE)
    
    ### Fonction permettant d'afficher les parangons des classes
     output$parangons=renderTable({
       bibi=list()
       for (i in 1:input$clust){
         bibi[[i]]=rbind(colnames(values()$desc.ind$para[[i]]),values()$desc.ind$para[[i]])
		 rownames(bibi[[i]])="Distance"
       }
       write.infile(X=bibi,file=paste(getwd(),"essai3.csv"),sep=";",nb.dec=8)
       baba=read.csv(paste(getwd(),"essai3.csv"),sep=";",header=FALSE)
       colnames(baba)=NULL
       file.remove(paste(getwd(),"essai3.csv"))
      baba[,-ncol(baba)]
     },
     rownames=FALSE)
    
    ### Fonction permettant d'afficher la description des classes par les axes 
    output$axes=renderTable({
      write.infile(X=values()$desc.axes$quanti,file=paste(getwd(),"essai2.csv"),sep=";",nb.dec=8)
      baba=read.csv(paste(getwd(),"essai2.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      file.remove(paste(getwd(),"essai2.csv"))
      baba[,-ncol(baba)]
    },
    rownames=FALSE)  

    observe({
      if(input$HCPCcode!=0){
        isolate({
          if (!is.null(lignecodeHCPCshiny)) print(lignecodeHCPCshiny)
          cat(CodeHCPC(),sep="\n")
          cat(PlotTree()$Code,sep="\n")
          cat(Plot2Dmap()$Code,sep="\n")
          cat(Plot3D()$Code,sep="\n")
        })
      }
    })
    
    observe({
      if(input$Quit!=0){
        isolate({
          res <- list()
          res$nomDataHCPCshiny <- nomDataHCPCshiny
          # res$anafact <- anafact
          res$anafact <- lignecodeHCPCshiny
          res$resultsHCPCshiny <- values()
          res$classx <- c("PCA", "list")
          class(res) <- c("HCPCshiny")
          res$clust <- input$clust
          res$consoli <- input$consoli
          res$metric <- input$metric
          res$drawtree <- input$drawtree 
          res$nom3D <- input$nom3D
          res$center <- input$center
          res$num <- input$num
          res$Code <- CodeHCPC()
          res$CodeTree <- PlotTree()$Code
          res$Code2Dmap <- Plot2Dmap()$Code
          res$Code3D <- Plot3D()$Code
          res$nb1 <- as.numeric(input$nb1)
          res$nb2 <- as.numeric(input$nb2)
          res$title1HCPCshiny <- input$title1HCPCshiny
          res$title2HCPCshiny <- input$title2HCPCshiny
          res$title3HCPCshiny <- input$title3HCPCshiny
          stopApp(returnValue=res)
        })
      }
    })
	
  }
