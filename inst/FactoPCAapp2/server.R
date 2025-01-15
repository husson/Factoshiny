function(input, output,session) {

  output$NB1 <- renderUI({
    validate(
      need(length(VariableChoicesPCAshiny)-length(input$supvar)>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    if(length(VariableChoicesPCAshiny)-length(input$supvar)>5){
       return(textInput("nb1", label = NULL, axe1PCAshiny,width='41px'))
    } else{
       return(selectInput("nb1",label=NULL, choices=1:(length(VariableChoicesPCAshiny)-length(input$supvar)),selected=axe1PCAshiny,width='41px'))
    }
  })
  
  output$NB2=renderUI({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    if((length(VariableChoicesPCAshiny)-length(input$supvar))>5){
       return(textInput("nb2", label = NULL, axe2PCAshiny,width='41px'))
    } else{
       return(selectInput("nb2",label=NULL, choices=(1:(length(VariableChoicesPCAshiny)-length(input$supvar))),selected=axe2PCAshiny,width='41px'))
    }
  })
  
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
        return(tags$div( 
            div(gettext("Number of dimensions kept for clustering",domain="R-Factoshiny"), style="display: inline-block; padding: 0px 0px 0px 0px"),
		    div(numericInput(inputId = "nbDimClustering", label = NULL,value=if(is.null(nbdimclustPCAshiny)){5} else {nbdimclustPCAshiny},min=1), style="display: inline-block;width: 70px; padding: 0px 0px 0px 10px"))
		)
    }
  })

  output$imputeData <- renderUI({
    if(any(is.na(newdataPCAshiny[,quantiPCAshiny]))){
	  return(radioButtons("impute",gettext("Handling missing values",domain="R-Factoshiny"),choices=list(gettext("Impute by the mean (fast but not recommended)",domain="R-Factoshiny"),gettext("Impute with 2-dimensional PCA-model (good compromise)",domain="R-Factoshiny"),gettext("Impute with k-dimensional PCA-model (estime k, time consuming)",domain="R-Factoshiny")),selected=imputeInit))
	} else {
      return(tags$div(tags$label(class="control-label", gettext("Handling missing values",domain="R-Factoshiny")),
	   tags$div(HTML(gettext("No missing values",domain="R-Factoshiny")))))
	}
  })

  values <- reactive({
     if (length(input$habiller2)==2 & input$color_point==gettext("qualitative variable",domain="R-Factoshiny")) return(isolate(valeur()))
	 if (length(input$nb1)>0){
	   if (max(input$nb1,input$nb2)>5) return(isolate(valeur()))
	 }
	 if (length(input$nbDimClustering)>0){
	   if (input$nbDimClustering >5) return(isolate(valeur()))
	 }
     if (length(input$pcaparam)==0){
	   return(valeur())
	 } else {
       if (input$submit>=0) return(isolate(valeur()))
     }
 })

  # values <- reactive({
  valeur <- function(){
    NomCol <- colnames(newdataPCAshiny)
	SuppressCol <- NULL
    if (length(QualiChoicePCAshiny)!=0){
	  if (length(QualiChoicePCAshiny)!=length(input$supquali)) {
	    SuppressCol <- which(NomCol%in%setdiff(QualiChoicePCAshiny,input$supquali))
	    NomCol <- NomCol[-SuppressCol]
	  }
	}
    QuantiSup <- which(NomCol%in%input$supvar)
	QualiSup <- which(NomCol%in%input$supquali)
	if (length(QuantiSup)==0) QuantiSup <- NULL
	if (length(QualiSup)==0) QualiSup <- NULL
	
    if(length(input$indsup)==0){
      suple <- NULL
    } else{
      suple <- which(nomPCAshiny%in%input$indsup)
    }
	codePCA <- NULL
	nomTabDon <- paste0(nomDataPCAshiny, if (!is.null(SuppressCol)){paste0("[,-c(",paste0(SuppressCol,collapse=","),")]")})
    if (length(input$habiller2)==2){
	  codePCA <- paste0("dfaux <- data.frame(",nomTabDon,",",paste0(input$habiller2[1],"_",input$habiller2[2]),"=paste0(",nomDataPCAshiny,"[,'",input$habiller2[1],"'],",nomDataPCAshiny,"[,'",input$habiller2[2],"']))\n")
	  QualiSup <- c(QualiSup,length(NomCol)+1)
    }
	
	boolImpute <- FALSE
    if(length(input$impute>0)){
	 if (input$impute!=gettext("Impute by the mean (fast but not recommended)",domain="R-Factoshiny")){
	  boolImpute <- TRUE
	  if (input$impute==gettext("Impute with k-dimensional PCA-model (estime k, time consuming)",domain="R-Factoshiny")){
		codePCA <- paste0(codePCA,"nb <- missMDA::estim_ncpPCA(",if (length(input$habiller2)==2){"dfaux"} else {nomTabDon},if (!is.null(QuantiSup)) paste0(",quanti.sup=c(",paste0(QuantiSup,collapse=","),")"),if (!is.null(QualiSup)) paste0(",quali.sup=c(",paste0(QualiSup,collapse=","),")"),if (!is.null(suple)) paste0(",ind.sup=c(",paste0(suple,collapse=","),")"),")$ncp\n")
		codePCA <- paste0(codePCA, "dfcompleted <- missMDA::imputePCA(",if (length(input$habiller2)==2){"dfaux"} else {nomTabDon},",ncp=nb",if (!is.null(QuantiSup)) paste0(",quanti.sup=c(",paste0(QuantiSup,collapse=","),")"),if (!is.null(QualiSup)) paste0(",quali.sup=c(",paste0(QualiSup,collapse=","),")"),if (!is.null(suple)) paste0(",ind.sup=c(",paste0(suple,collapse=","),")"),")$completeObs\n")
	  } else {
		codePCA <- paste0(codePCA, "dfcompleted <- missMDA::imputePCA(",if (length(input$habiller2)==2){"dfaux"} else {nomTabDon},", ncp=2",if (!is.null(QuantiSup)) paste0(",quanti.sup=c(",paste0(QuantiSup,collapse=","),")"),if (!is.null(QualiSup)) paste0(",quali.sup=c(",paste0(QualiSup,collapse=","),")"),if (!is.null(suple)) paste0(",ind.sup=c(",paste0(suple,collapse=","),")"),")$completeObs\n")
	  }
	  codePCA <- paste0(codePCA,"res.PCA<-PCA(dfcompleted", if (!is.null(SuppressCol)){paste0("[,-c(",paste0(SuppressCol,collapse=","),")]")})
	 }
    }
	if (!boolImpute) codePCA <- paste0(codePCA,"res.PCA<-PCA(",if (length(input$habiller2)==2){"dfaux"} else {nomTabDon})
	codePCA <- paste0(codePCA,if(max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))!=5) paste0(",ncp=",max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))),if(!is.null(QualiSup)) paste0(",quali.sup=c(",paste(QualiSup,collapse=","),")"),if(!is.null(QuantiSup)) paste0(",quanti.sup=c(",paste(QuantiSup,collapse=","),")"),if(!is.null(suple)) paste0(",ind.sup=c(",paste(suple,collapse=","),")"),if (!is.null(poids1PCAshiny)) paste0(",row.w=c(",paste(poids1PCAshiny,collapse=","),")"),if (!is.null(poids2PCAshiny)) paste0(",col.w=c(",paste(poids2PCAshiny,collapse=","),")"),if(input$nor!="TRUE") paste0(",scale.unit=",input$nor),",graph=FALSE)")
    list(res.PCA= eval(parse(text=codePCA)), codePCA=codePCA)
  }
  
  output$colourn2 <- renderUI({
    if(length(input$indsup)!=0){
      return(tags$div( 
        div(colourpicker::colourInput("colorsup", label=NULL, if(!is.null(input$colorsup)){if (input$colorsup!="blue") input$colorsup} else{supindPCAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		div(gettext("supplementary individuals",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"))
	  )
    }
  })
 
  output$colourn3 <- renderUI({
	if (length(input$supquali)>0){
        return(tags$div( 
            div(colourpicker::colourInput("colorquali", label=NULL, if(!is.null(input$colorquali)){if (input$colorquali!="magenta") input$colorquali} else{categPCAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		    div(gettext("categories",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"))
		)
    }
  })

  output$pointlabel <- renderUI({
    choix <- list()
    if (length(input$ind_mod)!=0) reponse <- input$ind_mod
    else {
	  reponse <- gettext("Individuals",domain="R-Factoshiny")
	  if (length(qualiPCAshiny)!=0) reponse <- c(reponse,gettext("Supplementary categories",domain="R-Factoshiny"))
    }
    if(sum(gettext("Individuals",domain="R-Factoshiny")==reponse)==1) choix <- c(choix,gettext("Individuals",domain="R-Factoshiny"))
    if(sum(gettext("Supplementary individuals",domain="R-Factoshiny")==reponse)==1) choix <- c(choix,gettext("Supplementary individuals",domain="R-Factoshiny"))
    if(sum(gettext("Supplementary categories",domain="R-Factoshiny")==reponse)==1) choix <- c(choix,gettext("Supplementary categories",domain="R-Factoshiny"))
    div(align="left",checkboxGroupInput("indmodpoint",gettext("Labels for",domain="R-Factoshiny"),choices=choix,selected=labmodPCAshiny))
  })

  observe({
    if(input$color_point != gettext("qualitative variable",domain="R-Factoshiny")) updateCheckboxInput(session, "elip", value = FALSE)
  })
  
  output$slider3 <- renderUI({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
	maxvar <- (length(VariableChoicesPCAshiny)-length(input$supvar))
    if(selection3PCAshiny=="contrib"){
      return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables",domain="R-Factoshiny"),
                                            min=1,max=maxvar,value=selection4,step=1)))  
    } else{
      return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables",domain="R-Factoshiny"),
                                            min=1,max=maxvar,value=maxvar,step=1)))}
  })
  
  output$habillage2 <- renderUI({
   if (input$color_point == gettext("quantitative variable",domain="R-Factoshiny")) return(selectizeInput("habiller",gettext("select the variable",domain="R-Factoshiny"), choices=c(VariableChoicesPCAshiny,input$supvar),multiple=FALSE,selected=habillageindPCAshiny))
   if (input$color_point == gettext("qualitative variable",domain="R-Factoshiny")){
#     if(length(QualiChoicePCAshiny)==0 || input$supquali==FALSE || length(input$supquali)==0) return(p(gettext("No categorical variable",domain="R-Factoshiny")))
     if(length(QualiChoicePCAshiny)==0 || length(input$supquali)==0) return(p(gettext("No categorical variable",domain="R-Factoshiny")))
     if(length(input$supquali)>=1) return(selectizeInput("habiller",gettext("select the variable",domain="R-Factoshiny"), choices=input$supquali, multiple=FALSE, selected=habillageindPCAshiny))
   }
   if (input$color_point == gettext("2 qualitative variables",domain="R-Factoshiny")){
     if(length(QualiChoicePCAshiny)==0 || length(input$supquali)<2) return(p(gettext("Not enough categorical variable",domain="R-Factoshiny")))
     if(length(input$supquali)>=1) return(selectizeInput("habiller2",gettext("select 2 variables",domain="R-Factoshiny"), choices=input$supquali, multiple=TRUE, selected=habillageindPCAshiny2))
   }
  })
  
  output$ellipsesPCAshiny <- renderUI({
    if(length(input$supquali)>=1 & (input$color_point == gettext("qualitative variable",domain="R-Factoshiny"))) return(checkboxInput("elip",gettext("Draw the confidence ellipses around the categories",domain="R-Factoshiny"),ellipsesPCAshiny))
  })
  
  output$choixindmod <- renderUI({
    choix <- gettext("Individuals",domain="R-Factoshiny")
    bool1 <- FALSE
    if(length(input$indsup)>0){
      choix <- c(choix,gettext("Supplementary individuals",domain="R-Factoshiny"))
      bool1 <- TRUE
    }
    if (length(input$supquali)>0){
      if(length(QualiChoicePCAshiny)>0 & sum(input$supquali!=FALSE)>0){
        choix <- c(choix,gettext("Supplementary categories",domain="R-Factoshiny"))
        bool1 <- TRUE
      }}
    if(bool1==TRUE) div(align="left",checkboxGroupInput("ind_mod",gettext("Points to draw",domain="R-Factoshiny"), choices=choix, selected = indmodPCAshiny))
  })
    
  codeGraphInd <- reactive({
    validate(
      need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny")),
      need(((length(input$indsup)+length(input$supquali))==0 | length(input$ind_mod)>=1),gettext("Please select the objects you want to plot: Individuals, categories or both",domain="R-Factoshiny")),
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny")),
      need(!is.null(input$habiller) || length(input$habiller)<=2,gettext("Please select maximum 2 variables as habillage",domain="R-Factoshiny"))
    )
	
    selecindiv <- NULL
    selecindivtext <- NULL
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
    # if(!is.null(input$plot_brush_ind)){
	  # selecindiv <- rownames(brushedPoints(as.data.frame(values()$res.PCA$ind$coord), input$plot_brush_ind,xvar=paste("Dim",as.numeric(input$nb1),sep="."),yvar=paste("Dim",as.numeric(input$nb2),sep=".")))
	  # selecindiv <- setdiff(rownames(values()$res.PCA$ind$coord),selecindiv)
	  # selecindivText <- paste("'",selecindiv,"'",sep="")
	  # session$resetBrush("plot_brush_ind")
    # }
	
	#### Autre facon de faire
    # if(!is.null(input$plot_brush_ind)){
      # brush <- NULL
      # makeReactiveBinding("brush")

      # observeEvent(input$plot_brush_ind, {
        # brush <<- input$plot_brush_ind
	    # selecindiv <- rownames(brushedPoints(as.data.frame(values()$res.PCA$ind$coord), brush,xvar=paste("Dim",as.numeric(input$nb1),sep="."),yvar=paste("Dim",as.numeric(input$nb2),sep=".")))
		# selecindiv <- setdiff(rownames(values()$res.PCA$ind$coord),selecindiv)
	    # selecindivText <- paste("'",selecindiv,"'",sep="")
       # session$resetBrush("plot_brush_ind")
      # })
	# print(selecindiv)
	# }

    hab <- "none"
    if(input$color_point == gettext("quantitative variable",domain="R-Factoshiny")) hab <- paste0("'",input$habiller,"'")
    if(input$color_point == "cos2") hab <- "'cos2'"
    if(input$color_point == "contribution") hab <- "'contrib'"
    if(input$color_point==gettext("qualitative variable",domain="R-Factoshiny")){
      if(length(input$supquali)>=1){
        if (length(input$habiller)==1) hab <- which(colnames(values()$res.PCA$call$X)==input$habiller)
      }
	}
    if(input$color_point==gettext("2 qualitative variables",domain="R-Factoshiny")){
      if(length(input$supquali)>=2){
        if (length(input$habiller2)==2) hab <- ncol(values()$res.PCA$call$X)
      }
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

    label <- c()
    if(!is.null(input$ind_mod) & input$graph==TRUE) {
      if(sum(gettext("Individuals",domain="R-Factoshiny")==input$indmodpoint)==1) label=c(label,"'ind'")
      if(sum(gettext("Supplementary individuals",domain="R-Factoshiny")==input$indmodpoint)==1) label=c(label,"'ind.sup'")
      if(sum(gettext("Supplementary categories",domain="R-Factoshiny")==input$indmodpoint)==1) label=c(label,"'quali'")
	  if (length(label)==0) label <- "'none'"
	  else label <- paste0("c(",paste0(label,collapse=","),")")
	}

    inv <- NULL
    if(!is.null(input$ind_mod) & input$graph==TRUE) {
      if(sum(gettext("Individuals",domain="R-Factoshiny")==input$ind_mod)==0) inv<- "ind"
      if(sum(gettext("Supplementary categories",domain="R-Factoshiny")==input$ind_mod)==0) inv<-c(inv,"quali")
      if(sum(gettext("Supplementary individuals",domain="R-Factoshiny")==input$ind_mod)==0) inv<-c(inv,"ind.sup")
	}
    bool <- (!is.null(input$elip) && input$elip==TRUE && input$color_point == gettext("qualitative variable",domain="R-Factoshiny"))

	res.PCA <- values()$res.PCA
    Code <- paste0(if (bool){"plotellipses"} else{"plot.PCA"},"(res.PCA", if (bool) paste0(", keepvar=",if (is.numeric(hab)){hab} else {which(colnames(values()$res.PCA$call$X)==hab)}),if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if(!is.null(inv)){paste0(",invisible=c(",paste0("'",paste(inv,collapse="','"),"'"),")")}, if(!is.null(selecindivtext)){paste0(",select=",selecindivtext)},if (!bool & hab!="none" & hab!="''"){paste0(",habillage=",hab)},if(input$title1!="PCA graph of individuals")paste0(',title="',input$title1,'"'),if(input$cex!=1)paste0(",cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex),if(input$coloract!="#000000")paste0(",col.ind='",input$coloract,"'"), if(!is.null(input$colorsup)){ if(input$colorsup!="#0000FF") paste0(",col.ind.sup='",input$colorsup,"'")},if(!is.null(input$colorquali)) { if (input$colorquali!="#FF00FF")paste0(",col.quali='",input$colorquali,"'")},if (!is.null(label)) paste0(",label =",label),")")
    Plot <- eval(parse(text=Code))
	return(list(Code=Code,Plot=Plot))      
  })
  
  output$map2 <- renderPlot({
    p <- print(codeGraphInd()$Plot)
  })
  
  output$colourv2 <- renderUI({
    if(!is.null(input$supvar)){
        return(tags$div( 
            div(colourpicker::colourInput("colorsupvar", label=NULL, if(!is.null(input$colorsupvar)){if (input$colorsupvar!="blue") input$colorsupvar} else{colorsupvarPCAshiny},allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		    div(gettext("supplementary variables",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px")
			)
		)
    }
  })
  
  codeGraphVar <- reactive({
    validate(
      need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny")),
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    selecindiv <- NULL
    if(input$select0=="cos2"){
      if(input$slider00!=1){    # else if cos2 = 1, one point is drawn
        selecindiv <- paste("cos2 ",input$slider00)
      }
    }
    if(input$select0=="contrib") selecindiv <- paste("contrib ",input$slider4)

	hab <- "none"
	if(input$color_arrow == "cos2") hab <- "cos2"
    if(input$color_arrow == "contribution") hab <- "contrib"

	res.PCA <- values()$res.PCA
    Code <- paste("plot.PCA(res.PCA",if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),",choix='var'",if (hab!="none"){paste0(",habillage = '",hab,"'")},if(!is.null(selecindiv)) paste0(",select='",selecindiv,"',unselect=0"),if (input$cex2!=1) paste0(",cex=",input$cex2,",cex.main=",input$cex2,",cex.axis=",input$cex2),if(input$title2!="PCA graph of variables") paste0(',title="',input$title2,'"'),if(!is.null(input$colorsupvar)) paste0(",col.quanti.sup='",input$colorsupvar,"'"),if(input$coloractvarPCAshiny!="#000000") paste0(",col.var='",input$coloractvarPCAshiny,"'"),")",sep="")
    Plot <- eval(parse(text=Code))
	return(list(Code=Code, Plot=Plot))
  })
  
  output$map <- renderPlot({
    p <- print(codeGraphVar()$Plot)
  })
  
  
  output$out22 <- renderUI({
    choix <- c(gettext("Summary of outputs",domain="R-Factoshiny"),gettext("Eigenvalues",domain="R-Factoshiny"),gettext("Results of the variables",domain="R-Factoshiny"),gettext("Results of the individuals",domain="R-Factoshiny"))
    if(length(input$indsup)>0) choix <- c(choix,gettext("Results of the supplementary individuals",domain="R-Factoshiny"))
	if(length(input$supvar)>0) choix <- c(choix,gettext("Results of the supplementary variables",domain="R-Factoshiny"))
	if (length(input$supquali)>0) choix <- c(choix,gettext("Results of the categorical variables",domain="R-Factoshiny"))
    radioButtons("out",gettext("Which outputs do you want?",domain="R-Factoshiny"), choices=choix, selected=gettext("Summary of outputs",domain="R-Factoshiny"),inline=TRUE)
  })
  
  output$sorties <- renderTable({
    return(as.data.frame(values()$res.PCA$eig))
  },rownames <- TRUE)
  
  output$sorties12 <- renderTable({
    validate(
      need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected",domain="R-Factoshiny")),
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least one supplementary variable",domain="R-Factoshiny"))
    )
    return(as.data.frame(values()$res.PCA$quali.sup$coord))
  },rownames=TRUE)
  
  output$sorties13 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny")),
      need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected",domain="R-Factoshiny"))
    )
    return(as.data.frame(values()$res.PCA$quali.sup$v.test))
  },rownames=TRUE)
  
  output$sorties2 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    return(as.data.frame(values()$res.PCA$var$coord))
  },rownames=TRUE)
  
  output$sorties22 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    return(as.data.frame(values()$res.PCA$ind$coord))
  },rownames=TRUE)
  
  output$sorties23 <- renderTable({
    validate(
      need(length(input$supvar)!=0, gettext("No supplementary quantitative variables",domain="R-Factoshiny")),
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    return(as.data.frame(values()$res.PCA$quanti.sup$coord))
  },rownames=TRUE)
  
  output$sorties32 <- renderTable({
    validate(
      need(length(input$supvar)!=0, gettext("No supplementary quantitative variables",domain="R-Factoshiny")),
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    return(as.data.frame(values()$res.PCA$quanti.sup$cor))
  },rownames=TRUE)
  
  output$sorties36 <- renderTable({
    validate(
      need(length(input$indsup)!=0, gettext("No supplementary individuals",domain="R-Factoshiny")),
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    return(as.data.frame(values()$res.PCA$ind.sup$coord))
  },rownames=TRUE)
  
  output$sorties37 <- renderTable({
    validate(
      need(length(input$indsup)!=0, gettext("No supplementary individuals",domain="R-Factoshiny")),
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    return(as.data.frame(values()$res.PCA$ind.sup$cos2))
  },rownames=TRUE)
  
  
  output$sorties3 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    return(as.data.frame(values()$res.PCA$var$contrib))
  },rownames=TRUE)
  
  output$sorties33 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    return(as.data.frame(values()$res.PCA$ind$contrib))
  },rownames=TRUE)
  
  output$sorties4 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    return(as.data.frame(values()$res.PCA$var$cos2))
  },rownames=TRUE)
  
  output$sorties44 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    return(as.data.frame(values()$res.PCA$ind$cos2))
  },rownames=TRUE)
  
  CalculDimdesc <- reactive({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
	)
    return(dimdesc(values()$res.PCA,proba = if (length(input$pvalueDimdesc)!=0) {input$pvalueDimdesc} else {0.05}))
  })

  output$sortieDimdesc3 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny")),
      need(length(CalculDimdesc()[[1]]$quanti)>0,gettext("No quantitative variable describes axis 1",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
	)
    return(as.data.frame(CalculDimdesc()[[1]]$quanti))
  },rownames=TRUE,digits=-3)
  
  output$sortieDimdesc4 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny")),
      need(length(CalculDimdesc()[[1]]$quali)>0,gettext("No categorical variable describes axis 1",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
	)
    return(as.data.frame(CalculDimdesc()[[1]]$quali))
  },rownames=TRUE,digits=-3)
  
  #DIM2
  
  output$sortieDimdesc33 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny")),
      need(length(CalculDimdesc()[[2]]$quanti)!=0,gettext("No quantitative variable describes axis 2",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
	)
    return(as.data.frame(CalculDimdesc()[[2]]$quanti))
  },rownames=TRUE,digits=-3)
  
  output$sortieDimdesc44 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny")),
      need(length(CalculDimdesc()[[2]]$quali)>0,gettext("No categorical variable describes axis 2",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
	)
    return(as.data.frame(CalculDimdesc()[[2]]$quali))
  },rownames=TRUE,digits=-3)
  
  #DIM3
  
  output$sortieDimdesc333 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>2 ,gettext("There are not three dimensions",domain="R-Factoshiny")),
      need(length(CalculDimdesc()[[3]]$quanti)>0,gettext("No quantitative variable describes axis 3",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
	)
    return(as.data.frame(CalculDimdesc()[[3]]$quanti))
  },rownames=TRUE,digits=-3)
  
  output$sortieDimdesc444 <- renderTable({
    validate(
      need((length(VariableChoicesPCAshiny)-length(input$supvar))>2 ,gettext("There are not three dimensions",domain="R-Factoshiny")),
      need(length(CalculDimdesc()[[3]]$quali)>0,gettext("No categorical variable describes axis 3",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
	)
    return(as.data.frame(CalculDimdesc()[[3]]$quali))
  },rownames=TRUE,digits=-3)
  
  
  output$map3 <- renderPlot({
    print(ggplot2::ggplot(cbind.data.frame(x=1:nrow(values()$res.PCA$eig),y=values()$res.PCA$eig[,2])) + ggplot2::aes(x=x, y=y)+ ggplot2::geom_col(fill="blue") + ggplot2::xlab("Dimension") + ggplot2::ylab(gettext("Percentage of variance",domain="R-Factoshiny")) + ggplot2::ggtitle(gettext("Decomposition of the total inertia",domain="R-Factoshiny")) + ggplot2::theme_light() + ggplot2::theme(plot.title = ggplot2::element_text(hjust =0.5))  + ggplot2::scale_x_continuous(breaks=1:nrow(values()$res.PCA$eig)))
    # return(barplot(values()$res.PCA$eig[,1],names.arg=rownames(values()$res.PCA$eig),las=2))
  })
  
  output$JDD <- DT::renderDataTable({
    cbind(Names=rownames(newdataPCAshiny),newdataPCAshiny)},
    options = list("orderClasses" = TRUE, "responsive" = TRUE, "pageLength" = 10), rownames=FALSE)
  
  output$summary <- renderPrint({
    summary(newdataPCAshiny)
  })
  
  output$summaryPCA <- renderPrint({
    validate(
      need(input$nbele!=0, gettext("Please select at least one element"))
    )
    a <- values()$res.PCA
    a$call$call<-values()$codePCA
    summary.PCA(a, nbelements=input$nbele)
  })
  
  output$summary2 <- downloadHandler(filename = function() { 
    paste('summaryofPCA','.txt', sep='') 
  },
  content = function(file) {
    summary.PCA(values()$res.PCA,nbelements=input$nbele,file=file)
  },
  contentType='text/csv')
  
  
  output$histo <- renderPlot({
    ggplot2::ggplot(newdataPCAshiny) + aes(x= newdataPCAshiny[,input$bam]) + geom_histogram(bins=nrow(newdataPCAshiny)/5)  + labs(x=input$bam,y="Count")
  })
  
  observe({
    if(input$Investigatehtml!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsavePCAshiny)
        if (substr(tolower(input$choixLANG),1,2)=="fr") FactoInvestigate::Investigate(values()$res.PCA, codeGraphInd = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {paste0(values()$codePCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {codeGraphVar()$Code} else {NULL}, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "fr")
        else FactoInvestigate::Investigate(values()$res.PCA, codeGraphInd = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {paste0(values()$codePCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {codeGraphVar()$Code} else {NULL}, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "en")
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$Investigatedoc!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsavePCAshiny)
        if (substr(tolower(input$choixLANG),1,2)=="fr") FactoInvestigate::Investigate(values()$res.PCA,codeGraphInd = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {paste0(values()$codePCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {codeGraphVar()$Code} else {NULL}, document="word_document",openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="fr")
        else FactoInvestigate::Investigate(values()$res.PCA,codeGraphInd = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {paste0(values()$codePCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {codeGraphVar()$Code} else {NULL}, document="word_document",openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="en")
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$InvestigateRmd!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsavePCAshiny)
	    if (substr(tolower(input$choixLANG),1,2)=="fr") FactoInvestigate::Investigate(values()$res.PCA, codeGraphInd = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {paste0(values()$codePCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {codeGraphVar()$Code} else {NULL},  openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "fr")
	    else FactoInvestigate::Investigate(values()$res.PCA, codeGraphInd = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {paste0(values()$codePCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {codeGraphVar()$Code} else {NULL},  openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "en")
        setwd(path.aux)
      })
    }
  })
    
  output$downloadData  <-  downloadHandler(
    filename = function() { 
      paste('graphVar','.png', sep='') 
    },
    content = function(file) {
        ggplot2::ggsave(file,print(codeGraphVar()$Plot))
    },
    contentType='image/png')
  
  output$downloadData1  <-  downloadHandler(
    filename = function() { 
      paste('graphVar','.jpg', sep='') 
    },
    content = function(file) {
        ggplot2::ggsave(file,print(codeGraphVar()$Plot))
    },
    contentType='image/jpg')
  
  output$downloadData2  <-  downloadHandler(
    filename = function() { 
      paste('graphVar','.pdf', sep='') 
    },
    content = function(file) {
        ggplot2::ggsave(file,codeGraphVar()$Plot)
    },
    contentType=NA)
  
  output$downloadData3  <-  downloadHandler(
    filename = function() { 
      paste('graphInd','.png', sep='') 
    },
    content = function(file) {
         ggplot2::ggsave(file,codeGraphInd()$Plot)
    },
    contentType='image/png')
  
  output$downloadData4  <-  downloadHandler(
    filename = function() { 
      paste('graphInd','.jpg', sep='') 
    },
    content = function(file) {
         ggplot2::ggsave(file,codeGraphInd()$Plot)
    },
    contentType='image/jpg')
  
  output$downloadData5  <-  downloadHandler(
    filename = function() { 
      paste0('graphInd','.pdf') 
    },
    content = function(file) {
         ggplot2::ggsave(file,codeGraphInd()$Plot)
    },
    contentType=NA)
	
    output$CodePrinted <- renderPrint({
       if (input$PCAcode!=0){
        cat(values()$codePCA,sep="\n")
        cat(codeGraphVar()$Code,sep="\n")
        cat(codeGraphInd()$Code,sep="\n")
       }
    })

    output$CodePrintedDimdesc <- renderPrint({
       if (input$PCAcode!=0){
        cat(values()$codePCA,sep="\n")
        cat("dimdesc(res.PCA)",sep="\n")
       }
    })

    output$CodePrintedSummary <- renderPrint({
       if (input$PCAcode!=0){
        cat(values()$codePCA,sep="\n")
        cat("summary(res.PCA)",sep="\n")
       }
    })

  # observe({
    # if(input$PCAcode!=0){
      # isolate({
        # cat(values()$codePCA,sep="\n")
        # cat(codeGraphVar()$Code,sep="\n")
        # cat(codeGraphInd()$Code,sep="\n")
      # })
    # }
  # })

  ### Bouton pour quitter l'application
  ### Recuperation parametres
  observe({
    if(input$Quit!=0){
      isolate({
        res <- list()
        res$nomDataPCAshiny <- nomDataPCAshiny
        res$newdataPCAshiny <- newdataPCAshiny
        if (!is.null(input$supquali)) res$b <- input$supquali
        res$c <- input$supvar
        res$d <- input$indsup
        res$y <- input$ind_mod
        res$nb1 <- input$nb1
        res$nb2 <- input$nb2
        res$habiller <- input$habiller
		res$selectionPCAshiny <- input$select
        if (length(input$select)>0){
		  if(input$select=="cos2") res$selection2PCAshiny <- input$slider1
          if(input$select==gettext("No selection",domain="R-Factoshiny")) res$selection2PCAshiny <- NULL
          if(input$select=="contrib") res$selection2PCAshiny <- input$slider0
          if(input$select==gettext("Manual",domain="R-Factoshiny")) res$selection2PCAshiny <- input$indiv
        }
        selecindiv2 <- 0
        if (length(input$select0)>0){
		  if(input$select0=="cos2") selecindiv2 <- input$slider00
          if(input$select0=="contrib") selecindiv2 <- input$slider4
        }
		res$j <- input$select0
        res$k <- selecindiv2
        res$l <- input$cex
        res$m <- input$cex2
	    res$color_point <- input$color_point
	    res$color_arrow <- input$color_arrow
        # res$codePCAp <- values()$codePCAp
        res$codePCA <- values()$codePCA
        res$codeGraphVar <- codeGraphVar()$Code
        res$codeGraphInd <- codeGraphInd()$Code
        res$title1 <- input$title1
        res$title2 <- input$title2
        res$anafact <- values()$res.PCA
        res$ellipsesPCAshiny <- input$elip
        res$supin <- input$colorsup
        res$categPCAshiny <- input$colorquali
        res$activeindPCAshiny <- input$coloract
        res$coloractvarPCAshiny <- input$coloractvarPCAshiny
        res$colorsupvarPCAshiny <- input$colorsupvarPCAshiny
        res$normePCAshiny <- input$nor
        res$poids1PCAshiny <- values()$res.PCA$call$row.W
        res$poids2PCAshiny <- values()$res.PCA$call$col.W
        res$hcpcparam <- input$hcpcparam
        res$nbdimclustPCAshiny <- input$nbDimClustering
		res$imputeInit <- input$impute
		if (length(input$pvalueDimdesc)) res$pvalueDimdescInit <- input$pvalueDimdesc
		else res$pvalueDimdescInit <- 0.05
        class(res) <- "PCAshiny"
        stopApp(returnValue=res)
      })
    }
  })
  
 }
