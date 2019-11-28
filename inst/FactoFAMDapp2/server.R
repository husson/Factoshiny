  function(input, output) {
      
  output$NB1 <- renderUI({
    validate(
      need(length(allVariables)-length(input$supvar)- length(input$supvar1)>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    if(length(allVariables)-length(input$supvar)- length(input$supvar1)>5){
       return(textInput("nb1", label = NULL, axe1,width='41px'))
    } else{
       return(selectInput("nb1",label=NULL, choices=1:(length(allVariables)-length(input$supvar)- length(input$supvar1)),selected=axe1,width='41px'))
    }
  })
  
  output$NB2=renderUI({
    validate(
      need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    if((length(allVariables)-length(input$supvar)- length(input$supvar1))>5){
       return(textInput("nb2", label = NULL, axe2,width='41px'))
    } else{
       return(selectInput("nb2",label=NULL, choices=1:(length(allVariables)-length(input$supvar)- length(input$supvar1)),selected=axe2,width='41px'))
    }
  })
  
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
        return(tags$div( 
            div(gettext("Number of dimensions kept for clustering",domain="R-Factoshiny"), style="display: inline-block; padding: 0px 0px 0px 0px"),
		    div(numericInput(inputId = "nbDimClustering", label = NULL,value=if(is.null(nbdimclustFAMDshiny)){5} else {nbdimclustFAMDshiny},min=1), style="display: inline-block;width: 70px; padding: 0px 0px 0px 10px"))
		)
    }
  })

  output$imputeData <- renderUI({
    if(any(is.na(newdata))){
	  return(radioButtons("impute",gettext("Handling missing values",domain="R-Factoshiny"),choices=list(gettext("Impute by means and proportions (fast but not recommended)",domain="R-Factoshiny"),gettext("Impute with 2-dimensional FAMD-model (good compromise)",domain="R-Factoshiny"),gettext("Impute with k-dimensional FAMD-model (estime k, time consuming)",domain="R-Factoshiny")),selected=gettext("Impute by means and proportions (fast but not recommended)",domain="R-Factoshiny")))
	} else {
      return(tags$div(tags$label(class="control-label", "Handling missing values"),
	   tags$div(HTML("No missing values"))))
	}
  })

  values <- reactive({
	 if (length(input$nb1)>0){
	   if (max(input$nb1,input$nb2)>5) return(isolate(valeur()))
	 }
	 if (length(input$nbDimClustering)>0){
	   if (input$nbDimClustering >5) return(isolate(valeur()))
	 }
     if (length(input$famdparam)==0){
	   return(valeur())
	 } else {
        if (input$submit>=0) isolate(valeur())
     }
 })
 
  valeur=function(){
    choixsup=NULL
	if (length(input$supvar)+length(input$supvar1)>=0) choixsup <- which(allVariables%in%c(input$supvar,input$supvar1))
    if(length(input$indsup)==0){
      suple=NULL
    } else{
	  suple=which(nom%in%input$indsup)
    }

	boolImpute <- FALSE
	codeFAMD <- NULL
    if (any(is.na(newdata))){
	  boolImpute <- TRUE
      if(length(input$impute>0)){
	    if (input$impute==gettext("Impute with k-dimensional FAMD-model (estime k, time consuming)",domain="R-Factoshiny")){
 	      codeFAMD <- paste0(codeFAMD,"nb <- missMDA::estim_ncpFAMD(",nomData,if (length(choixsup)!=0) paste0(",sup.var=c(",paste0(choixsup,collapse=","),")"),if (length(suple)!=0) paste0(",ind.sup=c(",paste0(suple,collapse=","),")"),")$ncp\n")
	      Nbncp <- "nb"
	    }
        if (input$impute==gettext("Impute by means and proportions (fast but not recommended)",domain="R-Factoshiny")) Nbncp <- 0
        if (input$impute==gettext("Impute with 2-dimensional FAMD-model (good compromise)",domain="R-Factoshiny")) Nbncp <- 2
      } else {
        Nbncp <- 0
	  }
	  codeFAMD <- paste0(codeFAMD, "dfcompleted <- missMDA::imputeFAMD(",nomData,",ncp=",Nbncp,if (length(choixsup)!=0) paste0(",sup.var=c(",paste0(choixsup,collapse=","),")"),if (length(suple)!=0) paste0(",ind.sup=c(",paste0(suple,collapse=","),")"),")\n")	
	}
	codeFAMD <- paste0(codeFAMD,"res.FAMD<-FAMD(",nomData)
	if (boolImpute) codeFAMD <- paste0(codeFAMD,",tab.comp=dfcompleted$tab.disj")
	if (length(input$nb1)>0) codeFAMD <- paste0(codeFAMD,if(max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))!=5) paste0(",ncp=",max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))),if(length(choixsup)!=0) paste0(",sup.var=c(",paste(choixsup,collapse=","),")"),if(length(suple)!=0) paste0(",ind.sup=c(",paste(suple,collapse=","),")"),",graph=FALSE)")
	else codeFAMD <- paste0(codeFAMD,if(max(axe1,axe2,nbdimclustFAMDshiny)!=5) paste0(",ncp=",max(axe1,axe2,nbdimclustFAMDshiny)),if(length(choixsup)!=0) paste0(",sup.var=c(",paste(choixsup,collapse=","),")"),if(length(suple)!=0) paste0(",ind.sup=c(",paste(suple,collapse=","),")"),",graph=FALSE)")
	list(res.FAMD=eval(parse(text=codeFAMD)), codeFAMD=codeFAMD)
    }
    

    codeGraphVar <- reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny")),
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables",domain="R-Factoshiny"))
      )
      if(input$select0=="cos2"){
        if(input$slider00!=1){
          selecindiv=paste("cos2 ",input$slider00)
        }
        else{
          selecindiv="cos2 0.999"
        }
        selecindivText=paste("'",selecindiv,"'",sep="")
      }
      if(input$select0=="NONE"){
        selecindiv=NULL
        selecindivText="NULL"
      }
      if(input$select0=="contrib"){
        selecindiv=paste("contrib ",input$slider4)
        selecindivText=paste("'",selecindiv,"'",sep="")
      }
	  res.FAMD <- values()$res.FAMD
      Code <- paste0("plot.FAMD(res.FAMD,axes=c(",input$nb1,",",input$nb2,"),choix='var'",if(!is.null(selecindiv)) paste0(",select='",selecindiv,"',unselect=0"),if (input$cex2!=1) paste0(",cex=",input$cex2,",cex.main=",input$cex2,",cex.axis=",input$cex2),if(input$title2!="Graph of variables") paste0(',title="',input$title2,'"'),")")
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))      
    })
    
    output$map <- renderPlot({
      p <- print(codeGraphVar()$Plot)
    })
    
  output$choixindmod <- renderUI({
    choix <- gettext("Individuals",domain="R-Factoshiny")
    if(length(input$indsup)>0) choix <- c(choix,gettext("Supplementary individuals",domain="R-Factoshiny"))
    choix <- c(choix,gettext("Active categories",domain="R-Factoshiny"))
    if (length(input$supvar1)>0) choix <- c(choix,gettext("Supplementary categories",domain="R-Factoshiny"))
    div(align="left",checkboxGroupInput("ind_mod",gettext("Points to draw",domain="R-Factoshiny"), choices=choix, selected = indmodFAMDshiny))
  })
    
    codeGraphInd <- reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny")),
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables",domain="R-Factoshiny"))
      )
      hab="none"
      colquali="magenta"
      if(length(QualiChoice)==1 && input$habi==TRUE){
        if(input$habi==TRUE){
          hab=QualiChoice
          colquali="blue"
        }
      }
      if (length(QualiChoice)>1){
        if(input$habi==TRUE){
          if(!is.null(input$habiller)) hab=input$habiller
        }
      }
      if(hab!="none"){
        hab=which(allVariables==hab)
        hab=as.numeric(hab)
      }
      selecindiv <- NULL
      selecindivtext <- "NULL"
    if(input$select=="cos2"){
      if(input$slider1!=1){
        selecindiv <- paste("cos2 ",input$slider1)
      } else{
        selecindiv <- "cos2 0.999999"
      }
      selecindivtext <- paste0("'",selecindiv,"'")
    }
    if(input$select=="contrib"){
      selecindiv <- paste("contrib ",input$slider0)
      selecindivtext <- paste0("'",selecindiv,"'")
    }

    if(input$select==gettext("Manual",domain="R-Factoshiny")){
      selecindiv <- c(input$indiv)
    }
    if(input$select==gettext("Manual",domain="R-Factoshiny")){
      if(length(input$indiv)==0) selecindivtext <- "NULL"
      if(length(input$indiv)>1){
        vec<- paste("'",paste(selecindiv,collapse="','"),"'",sep="")
        selecindivtext<-paste("c(",vec,")",sep="")
      }
      else if (length(input$indiv)==1){
        selecindivtext <- paste0("'",c(input$indiv),"'")
      }
    }
    inv <- NULL
    if(!is.null(input$ind_mod) & input$graph==TRUE) {
      if(sum(gettext("Individuals",domain="R-Factoshiny")==input$ind_mod)==0) inv<- "ind"
      if(sum(gettext("Active categories",domain="R-Factoshiny")==input$ind_mod)==0) inv<-c(inv,"quali")
      if(sum(gettext("Supplementary categories",domain="R-Factoshiny")==input$ind_mod)==0) inv<-c(inv,"quali.sup")
      if(sum(gettext("Supplementary individuals",domain="R-Factoshiny")==input$ind_mod)==0) inv<-c(inv,"ind.sup")
	}
	res.FAMD <- values()$res.FAMD
    Code <- paste0("plot.FAMD(res.FAMD",if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if(!is.null(inv)){paste0(",invisible=c(",paste0("'",paste(inv,collapse="','"),"'"),")")}, if(selecindivtext!="NULL"){paste0(",select=",selecindivtext)},if (hab!="none" & hab!="''"){paste0(",habillage=",hab)},if(input$title1!="Graph of individuals")paste0(',title="',input$title1,'"'),if(input$cex!=1)paste0(",cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex),")")
    Plot <- eval(parse(text=Code))
	return(list(Code=Code,Plot=Plot))      
    })
    
    output$map2 <- renderPlot({
      p <- print(codeGraphInd()$Plot)
    })
   
    
    codeGraphQuanti <- reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions")),
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
      )
      if(input$selecti=="cos2"){
       if(input$slider000!=1){
        selecindiv=paste("cos2 ",input$slider000)
      }
      else{
       selecindiv="cos2 0.999"
      }
      selecindivText=paste("'",selecindiv,"'",sep="")
      }
      if(input$selecti=="NONE"){
       selecindiv=NULL
      selecindivText="NULL"
      }
      if(input$selecti=="contrib"){
       selecindiv=paste("contrib ",input$slider6)
      selecindivText=paste("'",selecindiv,"'",sep="")
      }
	res.FAMD <- values()$res.FAMD
    Code <- paste0("plot.FAMD(res.FAMD, choix='quanti'",if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if(selecindivText!="NULL"){paste0(",select=",selecindivText)},if(input$title3!="Graph of quantitative variables")paste0(',title="',input$title3,'"'),if(input$cex3!=1)paste0(",cex=",input$cex3,",cex.main=",input$cex3,",cex.axis=",input$cex3),")")
    Plot <- eval(parse(text=Code))
	return(list(Code=Code,Plot=Plot))      
    })
    
    output$map4 <- renderPlot({
      p <- print(codeGraphQuanti()$Plot)
    })
        
    
    output$out22=renderUI({
      choix=list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results of the variables"),gettext("Results of the individuals"))
      if(!is.null(values()$choixsuple)){
        choix=c(choix,gettext("Results of the supplementary individuals"))
      }
      if(!is.null(values()$varsup)){
        choix=c(choix,gettext("Results of the supplementary variables"))
      }
      radioButtons("out",gettext("Which outputs do you want?"),
                   choices=choix,inline=TRUE)
    })
    
    output$sorties=renderTable({
        return(as.data.frame(values()$res.FAMD$eig))
    },rownames=TRUE)
    
    output$sorties12=renderTable({
        validate(
          need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
        )
        validate(
          need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
        )
        return(as.data.frame(values()$res.FAMD$quali.sup$coord))
    },rownames=TRUE)
    
    output$sorties13=renderTable({
      validate(
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
      )
      validate(
        need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
      )
      return(as.data.frame(values()$res.FAMD$quali.sup$v.test))
    },rownames=TRUE)
    
    output$sorties2=renderTable({
      validate(
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$var$coord))
    },rownames=TRUE)
    
    output$sorties22=renderTable({
      validate(
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$ind$coord))
    },rownames=TRUE)
    
    output$sorties23=renderTable({
      validate(
        need(length(input$supvar)!=0, gettext("No supplementary quantitative variables")),
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$var$coord.sup))
    },rownames=TRUE)
    
    output$sorties32=renderTable({
      validate(
        need(length(input$supvar)!=0, gettext("No supplementary quantitative variables")),
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$var$cos2.sup))
    },rownames=TRUE)
    
    output$sorties36=renderTable({
      validate(
        need(length(input$indsup)!=0, "No supplementary individuals"),
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$ind.sup$coord))
    },rownames=TRUE)
    
    output$sorties37=renderTable({
      validate(
        need(length(input$indsup)!=0, gettext("No supplementary individuals")),
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$ind.sup$cos2))
    },rownames=TRUE)
    
    
    output$sorties3=renderTable({
      validate(
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$var$contrib))
    },rownames=TRUE)
    
    output$sorties33=renderTable({
      validate(
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$ind$contrib))
    },rownames=TRUE)
    
    output$sorties4=renderTable({
      validate(
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$var$cos2))
    },rownames=TRUE)
    
    output$sorties44=renderTable({
      validate(
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$ind$cos2))
    },rownames=TRUE)
  
  CalculDimdesc <- reactive({
    validate(
      need((length(allVariables)-length(input$supvar)- length(input$supvar1))>2,gettext("Please select at least two active variables")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0"))
	)
    return(dimdesc(values()$res.FAMD,proba = if (length(input$pvalueDimdesc)!=0) {input$pvalueDimdesc} else {0.05}))
  })

  output$sortieDimdesc3=renderTable({
    validate(
      need((length(allVariables)-length(input$supvar)- length(input$supvar1))>2,gettext("Please select at least two active variables")),
      need(length(CalculDimdesc()[[1]]$quanti)>0,gettext("No quantitative variable describes axis 1")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0"))
    )
    return(as.data.frame(CalculDimdesc()[[1]]$quanti))
    },rownames=TRUE,digits=-3)
  
  output$sortieDimdesc4=renderTable({
    validate(
      need((length(allVariables)-length(input$supvar)- length(input$supvar1))>2,gettext("Please select at least three active variables")),
      need(length(CalculDimdesc()[[1]]$quali)>0,gettext("No qualitative variable describes axis 1")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0"))
    )
    return(as.data.frame(CalculDimdesc()[[1]]$quali))
    },rownames=TRUE,digits=-3)
  
  #DIM2
  
  output$sortieDimdesc33=renderTable({
    validate(
      need((length(allVariables)-length(input$supvar)- length(input$supvar1))>2,gettext("Please select at least three active variables")),
      need(length(CalculDimdesc()[[2]]$quanti)>0,gettext("No quantitative variable describes axis 2")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0"))
    )
    return(as.data.frame(CalculDimdesc()[[2]]$quanti))
    },rownames=TRUE,digits=-3)
  
  output$sortieDimdesc44=renderTable({
    validate(
      need((length(allVariables)-length(input$supvar)- length(input$supvar1))>2,gettext("Please select at least three active variables")),
      need(length(CalculDimdesc()[[2]]$quali)>0,gettext("No qualitative variable describes axis 2")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0"))
    )
    return(as.data.frame(CalculDimdesc()[[2]]$quali))
    },rownames=TRUE,digits=-3)
  
  #DIM3
  
  output$sortieDimdesc333=renderTable({
    validate(
      need((length(allVariables)-length(input$supvar)- length(input$supvar1))>2,gettext("Please select at least three active variables")),
      need(length(CalculDimdesc()[[3]]$quanti)>0,gettext("No quantitative variable describes axis 3")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0"))
    )
    return(as.data.frame(CalculDimdesc()[[3]]$quanti))
    },rownames=TRUE,digits=-3)
  
  output$sortieDimdesc444=renderTable({
    validate(
      need((length(allVariables)-length(input$supvar)- length(input$supvar1))>2,gettext("Please select at least three active variables")),
      need(length(CalculDimdesc()[[3]]$quali)>0,gettext("No qualitative variable describes axis 3")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0"))
    )
    return(as.data.frame(CalculDimdesc()[[3]]$quali))
    },rownames=TRUE,digits=-3)
    
    
    output$map3=renderPlot({
    print(ggplot2::ggplot(cbind.data.frame(x=1:nrow(values()$res.FAMD$eig),y=values()$res.FAMD$eig[,2])) + ggplot2::aes(x=x, y=y)+ ggplot2::geom_col(fill="blue") + ggplot2::xlab("Dimension") + ggplot2::ylab(gettext("Percentage of variance")) + ggplot2::ggtitle(gettext("Decomposition of the total inertia")) + ggplot2::theme_light() + ggplot2::theme(plot.title = ggplot2::element_text(hjust =0.5))  + ggplot2::scale_x_continuous(breaks=1:nrow(values()$res.FAMD$eig)))
      # return(barplot(values()$res.FAMD$eig[,1],names.arg=rownames(values()$res.FAMD$eig),las=2))
    })
    
    output$JDD=DT::renderDataTable({
      cbind(Names=rownames(newdata),newdata)},
      options = list(    "orderClasses" = TRUE,
                         "responsive" = TRUE,
                         "pageLength" = 10))
  
    output$summary=renderPrint({
      summary(newdata)
    })
  
    output$summaryFAMD=renderPrint({
      validate(
        need(input$nbele!=0, gettext("Please select at least one element"))
      )
      a<-values()$res.FAMD
      a$call$call<-code()
      summary.FAMD(a,nbelements=input$nbele)
    })
  
    output$summary2=downloadHandler(filename = function() { 
      paste('summaryofFAMD','.txt', sep='') 
    },
    content = function(file) {
      summary.FAMD(values()$res.FAMD,nbelements=input$nbele,file=file)
    },
    contentType='text/csv')
  
    
    output$slider3=renderUI({
      validate(
        need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select 2 active variables"))
      )
      maxvar=(length(allVariables)-length(input$supvar)- length(input$supvar1))
      if(selection3=="contrib"){
        return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables"),
                                              min=1,max=maxvar,value=selection4,step=1)))  
      }
      else{
      return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables"),
                  min=1,max=maxvar,value=maxvar,step=1)))}
    })
  
  output$slider5=renderUI({
    validate(
      need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select 2 active variables"))
    )
    maxvar=nrow(values()$res.FAMD$quanti.var$coord)
    if(selection5=="contrib"){
      return(div(align="center",sliderInput("slider6",label=gettext("Number of the most contributive variables"),
                                            min=1,max=maxvar,value=selection6,step=1)))  
    }
    else{
      return(div(align="center",sliderInput("slider6",label=gettext("Number of the most contributive variables"),
                                            min=1,max=maxvar,value=maxvar,step=1)))}
  })
  
  output$slider7=renderUI({
    validate(
      need((length(allVariables)-length(input$supvar)- length(input$supvar1))>1,gettext("Please select 2 active variables"))
    )
    if(is.null(input$indsup)) maxi=length(nom)
    if(!is.null(input$indsup)) maxi=length(nom)-length(input$indsup)
    if(selection=="contrib"){
      return(div(align="center",sliderInput("slider0", label = gettext("Number of the most contributive individuals"),
                                     min = 1, max = maxi, value =as.numeric(selection2),step=1)))
    } else{
      return(div(align="center",sliderInput("slider0", label = gettext("Number of the most contributive individuals"),
                                     min = 1, max = maxi, value =maxi,step=1))) 
    }
  })

    
    output$habillage2=renderUI({
      if(length(QualiChoice)==0){
        return(p(gettext("No categorical variable")))
      }
      if(length(QualiChoice)>1){
        if(is.null(habillageind)){
        num=c(1:length(QualiChoice))
        return(selectInput("habiller","", choices=list(num=QualiChoice),multiple=FALSE))
        }
        else{
          num=c(1:length(QualiChoice))
          return(selectInput("habiller","", choices=list(num=QualiChoice),multiple=FALSE,selected=habillageind))
        }
      }
      if(length(QualiChoice)==1){
        if(is.null(habillageind)){
          return(selectInput("habiller","", choices=QualiChoice,multiple=FALSE))
        }
        else{
          return(selectInput("habiller","", choices=QualiChoice,multiple=FALSE,selected=habillageind))
        }
      }
    })
      
    output$histo=renderPlot({
     if(input$bam%in%quanti) ggplot2::ggplot(newdata) + aes(x= newdata[,input$bam]) + geom_histogram(bins=nrow(newdata)/5)  + labs(x=input$bam,y="Count")
      else{
        barplot(table(newdata[,input$bam]),cex.names=0.8)
      }
    })
    
    output$downloadData = downloadHandler(
      filename = function() { 
        paste('GraphVar','.png', sep='') 
      },
      content = function(file) {
         ggplot2::ggsave(file,codeGraphVar()$Plot)
      },
      contentType='image/png')
    
    output$downloadData1 = downloadHandler(
      filename = function() { 
        paste('GraphVar','.jpg', sep='') 
      },
      content = function(file) {
         ggplot2::ggsave(file,codeGraphVar()$Plot)
      },
      contentType='image/jpg')
    
    output$downloadData2 = downloadHandler(
      filename = function() { 
        paste('GraphVar','.pdf', sep='') 
      },
      content = function(file) {
         ggplot2::ggsave(file,codeGraphVar()$Plot)
      },
      contentType=NA)
    
    output$downloadData3 = downloadHandler(
      filename = function() { 
        paste('GraphInd','.png', sep='') 
      },
      content = function(file) {
         ggplot2::ggsave(file,codeGraphInd()$Plot)
      },
      contentType='image/png')
    
    output$downloadData4 = downloadHandler(
      filename = function() { 
        paste('GraphInd','.jpg', sep='') 
      },
      content = function(file) {
         ggplot2::ggsave(file,codeGraphInd()$Plot)
      },
      contentType='image/jpg')
    
    output$downloadData5 = downloadHandler(
      filename = function() { 
        paste('GraphInd','.pdf', sep='') 
      },
      content = function(file) {
         ggplot2::ggsave(file,codeGraphInd()$Plot)
      },
      contentType=NA)
  
  output$downloadData6 = downloadHandler(
    filename = function() { 
      paste('GraphQuanti','.jpg', sep='') 
    },
    content = function(file) {
         ggplot2::ggsave(file,codeGraphQuanti()$Plot)
    },
    contentType='image/jpg')
  
  output$downloadData7 = downloadHandler(
    filename = function() { 
      paste('GraphQuanti','.png', sep='') 
    },
    content = function(file) {
         ggplot2::ggsave(file,codeGraphQuanti()$Plot)
    },
    contentType='image/png')
  
  output$downloadData8 = downloadHandler(
    filename = function() { 
      paste('GraphQuanti','.pdf', sep='') 
    },
    content = function(file) {
         ggplot2::ggsave(file,codeGraphQuanti()$Plot)
    },
    contentType=NA)
  
    output$CodePrinted <- renderPrint({
       if (input$FAMDcode!=0){
          if (length(input$habiller)==2 & input$habi==TRUE){
            cat(paste("newCol<-paste(",nomData,"[,'",input$habiller[1],"'],",nomData,"[,'",input$habiller[2],"'],","sep='/')",sep=""),sep="\n")
          }
          cat(values()$codeFAMD,sep="\n")
          cat(codeGraphInd()$Code,sep="\n")
          cat(codeGraphVar()$Code,sep="\n")
          cat(codeGraphQuanti()$Code,sep="\n")
       }
    })

    output$CodePrintedDimdesc <- renderPrint({
       if (input$FAMDcode!=0){
        cat(values()$codeFAMD,sep="\n")
        cat("dimdesc(res.FAMD)",sep="\n")
       }
    })

    output$CodePrintedSummary <- renderPrint({
       if (input$FAMDcode!=0){
        cat(values()$codeFAMD,sep="\n")
        cat("summary(res.FAMD)",sep="\n")
       }
    })

    # observe({
      # if(input$FAMDcode!=0){
        # isolate({
          # if (length(input$habiller)==2 & input$habi==TRUE){
            # cat(paste("newCol<-paste(",nomData,"[,'",input$habiller[1],"'],",nomData,"[,'",input$habiller[2],"'],","sep='/')",sep=""),sep="\n")
          # }
          # cat(values()$codeFAMD,sep="\n")
          # cat(codeGraphInd()$Code,sep="\n")
          # cat(codeGraphVar()$Code,sep="\n")
          # cat(codeGraphQuanti()$Code,sep="\n")
        # })
      # }
    # })

 observe({
   if(input$Quit!=0){
     isolate({
      res=list()
      res$nomData=nomData
      res$data=newdata
      res$anafact <- values()$res.FAMD
      res$b=input$supvar
      res$c=input$supvar1
      res$d=input$indsup
      res$e=input$nb1
      res$f=input$nb2
      habillage=NULL
      if(input$habi==TRUE) habillage=input$habiller
      res$g=habillage
      if(input$select=="cos2") selecindiv=input$slider1
      if(input$select=="NONE") selecindiv=NULL
      if(input$select=="contrib") selecindiv=input$slider0
      if(input$select=="Manuel") selecindiv=input$indiv
      res$h=input$select
      res$i=selecindiv
      selecindiv2=NULL
      if(input$select0=="cos2") selecindiv2=input$slider00
      if(input$select0=="contrib") selecindiv2=input$slider4
      res$j=input$select0
      res$k=selecindiv2
      selecindiv3=NULL
      if(input$selecti=="cos2") selecindiv3=input$slider000
      if(input$selecti=="contrib") selecindiv3=input$slider6
	  res$indmodFAMDshiny <- input$ind_mod
      res$o=input$selecti
      res$p=selecindiv3
      res$l=input$cex
      res$m=input$cex2
      res$n=input$cex3
      res$codeFAMD=values()$codeFAMD
      res$codeGraphVar=codeGraphVar()$Code
      res$codeGraphInd=codeGraphInd()$Code
      res$codeGraphQuanti=codeGraphQuanti()$Code
      res$labind=input$labels2
      res$labvar=input$labels
      res$hcpcparam <- input$hcpcparam
      res$nbdimclustFAMDshiny <- input$nbDimClustering
	  if (length(input$pvalueDimdesc)) res$pvalueDimdescInit <- input$pvalueDimdesc
	  else res$pvalueDimdescInit <- 0.05
      class(res) <- "FAMDshiny"
      stopApp(returnValue=res)
     })
    }
 })
	
}
