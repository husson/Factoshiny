# server script for HCPC
  function(input, output) {
	values <- reactive({
	  codeHCPC <- paste0("res.HCPC<-HCPC(",nomDataHCPCshiny,",nb.clust=",input$clust,if (input$kkparam==TRUE) paste0(",kk=",if (length(input$kk)==0){kkInit} else{input$kk}),",consol=",if (length(input$consoli)==0){consolidfHCPCshiny} else{input$consoli},if (length(input$clustCA)>0) {if (input$clustCA==gettext("Columns",domain="R-Factoshiny")) ",cluster.CA='columns'"},",graph=FALSE", if(input$pvalueDimdesc!=0.05) paste0(",proba=",input$pvalueDimdesc), if (input$metric=="Manhattan") {",metric='manhattan'"},")")
	  if (!is.null(lignecodeHCPCshiny)) list(res.HCPC=eval(parse(text=paste(lignecodeHCPCshiny,"\n",codeHCPC))), codeHCPC=codeHCPC)
	  else  list(res.HCPC=eval(parse(text=codeHCPC)), codeHCPC=codeHCPC)
    })
        
  output$NB1 <- renderUI({
     selectInput("nb1",label=NULL, choices=1:nbcolHCPCshiny,selected=nb1dfHCPCshiny,width='51px')
  })

  output$NB2 <- renderUI({
    selectInput("nb2",label=NULL, choices=1:nbcolHCPCshiny,selected=nb2dfHCPCshiny,width='51px')
  })

    output$clusters=renderUI({
      sliderInput("clust",gettext("Number of clusters",domain="R-Factoshiny"),min=2,max=min(10,nbindivHCPCshiny-1,as.integer(input$kk)-1),value=if(!is.null(input$clust)) {input$clust} else {resClusHCPCshiny},step=1)
    })
    
    output$kkInt=renderUI({
      textInput("kk", label = gettext("Number of clusters for Kmeans preprocessing",domain="R-Factoshiny"), value=if(!is.null(input$kk)) {input$kk} else {kkInit})
    })

    output$clusterCA=renderUI({
      if (!is.null(clusterOnCA)) radioButtons("clustCA",gettext("Clustering on",domain="R-Factoshiny"),choices=list(gettext("Rows",domain="R-Factoshiny"),gettext("Columns",domain="R-Factoshiny")),inline=TRUE,select=clusterOnCA)
    })

    PlotTree <- reactive({
    validate(
      need( as.integer(input$kk) < nbindivHCPCshiny, paste(gettext("Choose a number of clusters for preprocessing less than the number of individuals, i.e. strictly less than",domain="R-Factoshiny"),nbindivHCPCshiny)),
      need( as.integer(input$kk) > as.integer(input$clust), gettext("Choose a number of clusters for preprocessing greater than the number of clusters",domain="R-Factoshiny"))
    )
      Code <- paste0("plot.HCPC(res.HCPC,choice='tree',title='",input$title3HCPCshiny,"')")
	  res.HCPC <- values()$res.HCPC
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))
    })
        
    Plot2Dmap <- reactive({
    validate(
      need( as.integer(input$kk) < nbindivHCPCshiny, paste(gettext("Choose a number of clusters for preprocessing less than the number of individuals, i.e. strictly less than",domain="R-Factoshiny"),nbindivHCPCshiny)),
      need( as.integer(input$kk) > as.integer(input$clust), gettext("Choose a number of clusters for preprocessing greater than the number of clusters",domain="R-Factoshiny"))
    )
      Code <- paste0("plot.HCPC(res.HCPC,choice='map',draw.tree=",input$drawtree,",title='",input$title2HCPCshiny,"'",if (!is.null(input$nb1)) {if (as.numeric(input$nb1)!=1 | as.numeric(input$nb2)!=2) paste0(",axes=c(",as.numeric(input$nb1),",",as.numeric(input$nb2),")")},")") 
	  res.HCPC <- values()$res.HCPC
	  Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))
    })
    
    Plot3D <- reactive({
    validate(
      need( as.integer(input$kk) < nbindivHCPCshiny, paste(gettext("Choose a number of clusters for preprocessing less than the number of individuals, i.e. strictly less than",domain="R-Factoshiny"),nbindivHCPCshiny)),
      need( as.integer(input$kk) > as.integer(input$clust), gettext("Choose a number of clusters for preprocessing greater than the number of clusters",domain="R-Factoshiny"))
    )
      Code <- paste0("plot.HCPC(res.HCPC,choice='3D.map',ind.names=",input$nom3D,",centers.plot=",input$center,",angle=",input$num,",title='",input$title1HCPCshiny,"'",if (!is.null(input$nb1)) {if (as.numeric(input$nb1)!=1 | as.numeric(input$nb2)!=2) paste0(",axes=c(",as.numeric(input$nb1),",",as.numeric(input$nb2),")")},")") 
	  res.HCPC <- values()$res.HCPC
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
      options = list( "orderClasses" = TRUE, "responsive" = TRUE, "pageLength" = 10), rownames=FALSE)
    
  observe({
    if(input$Investigatehtml!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveHCPCshiny)
        if (substr(tolower(input$choixLANG),1,2)=="fr") FactoInvestigate::Investigate(values()$res.HCPC, openFile=TRUE, file = input$titleFile, language= "fr")
        else FactoInvestigate::Investigate(values()$res.HCPC, openFile=TRUE, file = input$titleFile, language= "en")
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$Investigatedoc!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveHCPCshiny)
        if (substr(tolower(input$choixLANG),1,2)=="fr") FactoInvestigate::Investigate(values()$res.HCPC,document="word_document",openFile=TRUE, file = input$titleFile, language= "fr")
        else FactoInvestigate::Investigate(values()$res.HCPC,document="word_document",openFile=TRUE, file = input$titleFile, language= "en")
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
	    if (substr(tolower(input$choixLANG),1,2)=="fr") FactoInvestigate::Investigate(values()$res.HCPC, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = "Investigate", language= "fr")
	    else FactoInvestigate::Investigate(values()$res.HCPC, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = "Investigate", language= "en")
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
        
    output$printDescVar=renderPrint({
      print.catdes(values()$res.HCPC$desc.var)
    })

    output$descriptquantivar=renderTable({
      write.infile(X=values()$res.HCPC$desc.var$quanti.var, file=paste(getwd(),"essaib.csv"),sep=";")
	  baba=read.csv(paste(getwd(),"essaib.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      file.remove(paste(getwd(),"essaib.csv")) 
      baba[,-ncol(baba)]
    },
    rownames=FALSE)
    
    output$descriptquanti=renderTable({
      write.infile(X=values()$res.HCPC$desc.var$quanti,file=paste(getwd(),"essai.csv"),sep=";")
      baba=read.csv(paste(getwd(),"essai.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      # b=which(baba[,1]=="format non affichable")
      file.remove(paste(getwd(),"essai.csv")) 
      baba[,-ncol(baba),drop=FALSE]
    },
    rownames=FALSE)

    output$descriptqualivar=renderTable({
      write.infile(X=values()$res.HCPC$desc.var$test.chi2, file=paste(getwd(),"essaic.csv"),sep=";")
	  baba=read.csv(paste(getwd(),"essaic.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      file.remove(paste(getwd(),"essaic.csv")) 
      baba[,-ncol(baba),drop=FALSE]
    },
    rownames=FALSE)
    
    output$descriptquali=renderTable({
      write.infile(X=values()$res.HCPC$desc.var$category,file=paste(getwd(),"essai.csv"),sep=";")
      baba=read.csv(paste(getwd(),"essai.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      file.remove(paste(getwd(),"essai.csv")) 
      baba[,-ncol(baba),drop=FALSE]
    },
    rownames=FALSE)

     output$parangons=renderTable({
       bibi=list()
       for (i in 1:input$clust){
         bibi[[i]]=rbind(colnames(values()$res.HCPC$desc.ind$para[[i]]),values()$res.HCPC$desc.ind$para[[i]])
		 rownames(bibi[[i]])="Distance"
       }
       write.infile(X=bibi,file=paste(getwd(),"essai3.csv"),sep=";",nb.dec=8)
       baba=read.csv(paste(getwd(),"essai3.csv"),sep=";",header=FALSE)
       colnames(baba)=NULL
       file.remove(paste(getwd(),"essai3.csv"))
      baba[,-ncol(baba)]
     },
     rownames=FALSE)
    
    output$distind=renderTable({
       bibi=list()
       for (i in 1:input$clust){
         bibi[[i]]=rbind(colnames(values()$res.HCPC$desc.ind$dist[[i]]),values()$res.HCPC$desc.ind$dist[[i]])
		 rownames(bibi[[i]])="Distance"
       }
       write.infile(X=bibi,file=paste(getwd(),"essai3b.csv"),sep=";",nb.dec=8)
       baba=read.csv(paste(getwd(),"essai3b.csv"),sep=";",header=FALSE)
       colnames(baba)=NULL
       file.remove(paste(getwd(),"essai3b.csv"))
      baba[,-ncol(baba)]
     },
     rownames=FALSE)

    ### Fonction permettant d'afficher la description des classes par les axes 
    output$axes=renderTable({
      write.infile(X=values()$res.HCPC$desc.axes$quanti,file=paste(getwd(),"essai2.csv"),sep=";",nb.dec=8)
      baba=read.csv(paste(getwd(),"essai2.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      file.remove(paste(getwd(),"essai2.csv"))
      baba[,-ncol(baba)]
    },
    rownames=FALSE)  

    output$CodePrinted <- renderPrint({
       if (input$HCPCcode!=0){
          if (!is.null(lignecodeHCPCshiny)) cat(lignecodeHCPCshiny,sep="\n")
          cat(values()$codeHCPC,sep="\n")
          cat(PlotTree()$Code,sep="\n")
          cat(Plot2Dmap()$Code,sep="\n")
          cat(Plot3D()$Code,sep="\n")
       }
    })

    output$CodePrintedSummary <- renderPrint({
       if (input$HCPCcode!=0){
          if (!is.null(lignecodeHCPCshiny)) cat(lignecodeHCPCshiny,sep="\n")
          cat(values()$codeHCPC,sep="\n")
        cat("summary(res.HCPC)",sep="\n")
       }
    })










  resultat <- reactive({
    don <- values()$res.HCPC$data.clust
	levels(don$clust) <- paste(gettext("Cluster",domain="R-Factoshiny"),levels(don$clust))
    return(catdes(donnee = don, num.var = ncol(don), proba = 1))
  })
    
  tableau_vtest <- reactive({
    if(!is.null(resultat()$quanti)){
      lvl <- length(resultat()$quanti)
      x <- resultat()$quanti[[1]][,"v.test", drop = FALSE]
      tabvtest <- x[sort(rownames(x)),,drop=FALSE]
      
      x <- resultat()$quanti[[1]][,"p.value", drop = FALSE]
      tabpvalue <- x[sort(rownames(x)),,drop=FALSE]
      
      x <- resultat()$quanti[[1]][,2, drop = FALSE]
      tabmean <- x[sort(rownames(x)),,drop=FALSE]
      
      for(i in 2:lvl){
        x <- resultat()$quanti[[i]][,"v.test", drop = FALSE]
        tabvtest <- cbind(tabvtest,as.data.frame(x[sort(rownames(x)),]))
        x <- resultat()$quanti[[i]][,"p.value", drop = FALSE]
        tabpvalue <- cbind(tabpvalue,as.data.frame(x[sort(rownames(x)),]))
        x <- resultat()$quanti[[i]][,2, drop = FALSE]
        tabmean <- cbind(tabmean,as.data.frame(x[sort(rownames(x)),]))
      }
      colnames(tabpvalue) <- colnames(tabvtest) <- colnames(tabmean) <- names(resultat()$quanti)
      tabmean$overall <- resultat()$quanti[[1]][sort(rownames(resultat()$quanti[[1]])),3]
      
      validate(
        need(as.numeric(input$select_proba_plot) > 0, paste(gettext("The p-value should be greater than",domain="R-Factoshiny"),0))
      )
      if (input$select_proba_plot <= min(tabpvalue)) return(NULL)
      else {
	    sortie <- signif(t(as.matrix(tabvtest[apply(tabpvalue,1,min) <= input$select_proba_plot ,])),3)
        return(sortie)
      }
    }
  })
  
  tableau_quali <- reactive({
    if(!is.null(resultat()$category)){
      
      rows <- NULL
      for (i in 1:nrow(resultat()$category[[1]]))  rows <- c(rows,rownames(resultat()$category[[1]])[i])
      lvl <- length(resultat()$category)
      x <- resultat()$category[[1]][rows,"v.test", drop = FALSE]
      tabvtest <- as.data.frame(x[sort(rownames(x)),,drop=FALSE])
      x <- resultat()$category[[1]][rows,"p.value", drop = FALSE]
      tabpvalue <- as.data.frame(x[sort(rownames(x)),,drop=FALSE])
      for(i in 2:lvl){
        x <- resultat()$category[[i]][rows,"v.test", drop = FALSE]
        tabvtest <- cbind(tabvtest,as.data.frame(x[sort(rownames(x)),,drop=FALSE]))
        x <- resultat()$category[[i]][rows,"p.value", drop = FALSE]
        tabpvalue <- cbind(tabpvalue,as.data.frame(x[sort(rownames(x)),,drop=FALSE]))
      }
      colnames(tabpvalue) <- colnames(tabvtest) <- names(resultat()$category)
      validate(
        need(as.numeric(input$select_proba_plot) > 0, paste(gettext("The p-value should be greater than",domain="R-Factoshiny"),0))
      )
      if (input$select_proba_plot <= min(tabpvalue)) return(NULL)
	  else {
	    sortie <- signif(tabvtest[apply(tabpvalue,1,min) <= input$select_proba_plot,,drop = FALSE],3)
        return(sortie)
      }
    }
    
  })
  
  # output$barplot <-  renderPlot({
    # plot.catdes(x = resultat(), 
                # level = input$select_proba_plot,
                # col.upper = "#F00000",
                # col.lower = "#03F7FF",
                # barplot = TRUE
    # )
  # })
  
  # tab_quanti <- reactive({
    # if(!is.null(resultat()$quanti)){
      # if(!is.null(input$select_quanti_var)){
        # lvl <- length(resultat()$category)
        # tabVtest <- tableau_vtest()
		# mini <- 1
		# if (!is.null(resultat()$quanti.var)) mini <- min(resultat()$quanti.var[,2])
        # validate(
          # need(!is.null(tabVtest), paste(gettext("The p-value should be greater than",domain="R-Factoshiny"),mini,"."))
        # )
        # quant <- seq(min(tabVtest, na.rm = T), max(tabVtest, na.rm = T),length.out = 100)
        # color <- grDevices::colorRampPalette(c("#03F7FF","white","#F00000"))(length(quant)+1)
        
        # a <- DT::formatStyle(
          # DT::datatable(t(as.matrix(tabVtest)),
                        # extensions = c('Buttons','FixedColumns','FixedHeader'),
                        # options = list( pageLength = ncol(tabVtest),
                            # dom = 'Bfrtip', buttons = c('csv'), fixedColumns = TRUE, fixedHeader = TRUE)
          # ),
          # rownames(tabVtest),
          # backgroundColor = DT::styleInterval(quant, color)
        # )
        # return(a)
      # }
    # }
  # })
  
  
  # output$tableau_df_quanti <- DT::renderDataTable({
    # if(!is.null(resultat()$quanti)) return(tab_quanti())
  # })
  
  
  # tab_quali <- reactive({
    # if(!is.null(input$select_quali_var_test)){
      
      # if(!(input$select_categorical_var %in% input$select_quali_var_test)){
        # tabQuali <- tableau_quali()
        # validate(
          # need(!is.null(tabQuali), paste(gettext("The p-value is too small. You should increase the p-value.",domain="R-Factoshiny")))
        # )
        # quant <- seq(min(tabQuali,na.rm = TRUE), max(tabQuali, na.rm = TRUE), length.out = 100)
        # color <- grDevices::colorRampPalette(c("#03F7FF","white","#F00000"))(length(quant)+1)
        
        # a <- DT::formatStyle(
          # DT::datatable(tabQuali, extensions = c('Buttons','FixedColumns','FixedHeader'),
            # options = list(pageLength = nrow(tabQuali), dom = 'Bfrtip',
              # buttons = c('csv'), fixedColumns = TRUE, fixedHeader = TRUE)
          # ),
          # colnames(tabQuali), backgroundColor = DT::styleInterval(quant, color)
        # )  
        # return(a)
      # }
    # }
    
  # })
  
  # output$tableau_df_quali <- DT::renderDataTable({
    # if (!is.null(tab_quali())) return(tab_quali())
  # })
  
  tab_both <- reactive({
    tab <- NULL
    if(!is.null(tableau_quali())) tab <- rbind(tab,tableau_quali())
    if(!is.null(tableau_vtest())) tab <- rbind(tab,t(tableau_vtest()))
	mini <- 1
	if (!is.null(resultat()$quanti.var)) mini <- min(resultat()$quanti.var[,2])
	if (!is.null(resultat()$category)) mini <- min(mini,resultat()$test.chi2[,1])
    validate(
      need(!is.null(tab), paste(gettext("The p-value should be greater than",domain="R-Factoshiny"),signif(mini,3),"."))
    )

    if (!is.null(tab)){
      quant <- seq(min(tab, na.rm = T), max(tab, na.rm = T), length.out = 100)
      color <- grDevices::colorRampPalette(c("#03F7FF","white","#F00000"))(length(quant)+1)
      
      a <- DT::formatStyle(
        DT::datatable(tab,extensions = c('Buttons','FixedColumns','FixedHeader'),
          options = list(pageLength = nrow(tab),dom = 'Bfrtip',
            buttons = c('csv'),fixedColumns = TRUE,fixedHeader = TRUE)
        ), colnames(tab),backgroundColor = DT::styleInterval(quant, color)
      )
      return(a)
    }
  })
  
  output$tableau_df_both <- DT::renderDataTable({
    if (!is.null(tab_both())) return(tab_both())
  })
  
  observe({ input$catdesMAJ
      don <- values()$res.HCPC$data.clust
	  levels(don$clust) <- paste(gettext("Cluster",domain="R-Factoshiny"),levels(don$clust))
    output$resu_catdes <- renderPrint({ isolate(
      catdes(donnee = don, num.var = ncol(don), proba = input$select_proba_plot)
	  # catdes(donnee = values()$res.HCPC$data.clust, num.var = ncol(values()$res.HCPC$data.clust), proba = input$select_proba_plot)
	  )
    }) 
  })
  
  tableau_link_quanti <- reactive({
    if(!is.null(which(sapply(values()$res.HCPC$data.clust,is.numeric)))){
      validate(
        need(input$select_proba_plot > min(resultat()$quanti.var[,"P-value"]), paste(gettext("The p-value should be greater than",domain="R-Factoshiny"),signif(min(resultat()$quanti.var[,"P-value"]),3)))
      )
      tab <- as.data.frame(resultat()$quanti.var[ resultat()$quanti.var[,"P-value"] <= input$select_proba_plot,c("Eta2","P-value"), drop = FALSE])
      return(tab)
    }
  })
  
  df_link_quanti <- reactive({
    if(!is.null(which(sapply(values()$res.HCPC$data.clust,is.numeric)))){
      quant <- seq(min(tableau_link_quanti()[,1]), max(tableau_link_quanti()[,1]), length.out = 100)
      color <- grDevices::colorRampPalette(c("#03F7FF","white","#F00000"))(length(quant)+1)
      a <- DT::formatStyle(
        DT::datatable(signif(tableau_link_quanti(),3),
                      extensions = c('Buttons','FixedColumns','FixedHeader'),
                      options = list(pageLength = nrow(tableau_link_quanti()),
                        dom = 'Bfrtip',buttons = c('csv'),fixedColumns = TRUE,fixedHeader = TRUE)
        ),
        columns = colnames(tableau_link_quanti()),
        valueColumns = 'P-value',
        backgroundColor = DT::styleInterval(quant, color)
      )
    }
    return(a)
  })
  
  output$table_link_quanti <- DT::renderDataTable({
    if (!is.null(df_link_quanti())) return(df_link_quanti())
  })
  
  tableau_link_chisquare <- reactive({
    if(!is.null(resultat()$test.chi2)){
      validate(
        need(input$select_proba_plot > min(resultat()$test.chi2[,"p.value"]), paste(gettext("The p-value should be greater than",domain="R-Factoshiny"),signif(min(resultat()$test.chi2[,"p.value"]),3)))
      )
      tab <- (resultat()$test.chi2[resultat()$test.chi2[,"p.value"] <= input$select_proba_plot,"p.value", drop = FALSE])
      return(tab)
    }
  })
  
  tab_chisquare <- reactive({
    if(!is.null(resultat()$test.chi2)){
      if(nrow(tableau_link_chisquare()) > 0){
        
        quant <- seq(min(tableau_link_chisquare()[,"p.value"]), max(tableau_link_chisquare()[,"p.value"]), length.out = 100)
        color <- grDevices::colorRampPalette(c("#F00000","white","#03F7FF"))(length(quant)+1)
        
        a <- DT::formatStyle(
          DT::datatable(
            signif(tableau_link_chisquare(),3),
            extensions = c('Buttons','FixedColumns','FixedHeader'),
            options = list(pageLength = nrow(tableau_link_chisquare()),
                           dom = 'Bfrtip', buttons = c('csv'), fixedColumns = TRUE, fixedHeader = TRUE)
          ),
          
          columns = colnames(tableau_link_chisquare()),
          valueColumns = "p.value",
          backgroundColor = DT::styleInterval(quant, color)
        )
        return(a)
      }
    }
  })
  
  output$table_link_chisquare <- DT::renderDataTable({
    if(!is.null(tab_chisquare())) return(tab_chisquare())
  })
  
  # output$quanti_quali_both1 <- renderUI({
    # xx <- gettext("Quantitative",domain="R-Factoshiny")
    # old.x <- gettext("Both",domain="R-Factoshiny")
    # if(!is.null(input$quanti_quali_both)) old.x <- input$quanti_quali_both
    # if(!is.null(input$select_quali_var_test) & !is.null(input$select_quanti_var)) xx <- c(gettext("Both",domain="R-Factoshiny"),gettext("Quantitative",domain="R-Factoshiny"),gettext("Qualitative",domain="R-Factoshiny"))
    # if(is.null(input$select_quali_var_test) & !is.null(input$select_quanti_var)) xx <- c(gettext("Quantitative",domain="R-Factoshiny"))
    # if(!is.null(input$select_quali_var_test) & is.null(input$select_quanti_var)) xx <- c(gettext("Qualitative",domain="R-Factoshiny"))
    
    # if (old.x%in%xx){
      # radioButtons(inputId = "quanti_quali_both",choices = xx,
        # inline = TRUE,label = gettext("Describe by ... variables",domain="R-Factoshiny"),selected = old.x)
    # } else {
      # radioButtons(inputId = "quanti_quali_both", choices = xx,
        # inline = TRUE,label = gettext("Describe by ... variables",domain="R-Factoshiny"))
    # }
  # })






































    observe({
      if(input$Quit!=0){
        isolate({
          res <- list()
          res$nomDataHCPCshiny <- nomDataHCPCshiny
          # res$anafact <- anafact
          res$anafact <- lignecodeHCPCshiny
          res$resultsHCPCshiny <- values()$res.HCPC
          res$classx <- c("PCA", "list")
          class(res) <- c("HCPCshiny")
          res$clust <- input$clust
		  res$clusterOnCA <- input$clustCA
          res$consoli <- input$consoli
          res$metric <- input$metric
          res$drawtree <- input$drawtree 
          res$nom3D <- input$nom3D
          res$center <- input$center
          res$num <- input$num
          res$Code <- values()$codeHCPC
          res$CodeTree <- PlotTree()$Code
          res$Code2Dmap <- Plot2Dmap()$Code
          res$Code3D <- Plot3D()$Code
          res$nb1 <- as.numeric(input$nb1)
          res$nb2 <- as.numeric(input$nb2)
          res$title1HCPCshiny <- input$title1HCPCshiny
          res$title2HCPCshiny <- input$title2HCPCshiny
          res$title3HCPCshiny <- input$title3HCPCshiny
		  res$kk <- input$kk
		  res$kkparam <- input$kkparam
          stopApp(returnValue=res)
        })
      }
    })
	
  }
