function(input, output,session) {

  getactive <- function(){
      if(length(input$supvar)==0){
        activevar <- VariableChoicesPCAshiny
      } else{
        activevar <- VariableChoicesPCAshiny[-which(VariableChoicesPCAshiny%in%input$supvar)]
      }
      return(activevar)
  }
  
  output$NB1 <- renderUI({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    if(length(getactive())>5){
       return(textInput("nb1", label = NULL, axe1PCAshiny,width='41px'))
    } else{
       return(selectInput("nb1",label=NULL, choices=1:length(getactive()),selected=axe1PCAshiny,width='41px'))
    }
  })
  
  output$NB2=renderUI({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    if(length(getactive())>5){
       return(textInput("nb2", label = NULL, axe2PCAshiny,width='41px'))
    } else{
       return(selectInput("nb2",label=NULL, choices=(1:length(getactive())),selected=axe2PCAshiny,width='41px'))
    }
  })
  
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
        return(tags$div( 
            div(gettext("Number of dimensions kept for clustering"), style="display: inline-block; padding: 0px 0px 0px 0px"),
		    div(numericInput(inputId = "nbDimClustering", label = NULL,value=if(is.null(nbdimclustPCAshiny)){5} else {nbdimclustPCAshiny},min=1), style="display: inline-block;width: 70px; padding: 0px 0px 0px 10px"))
		)
    }
  })

  output$imputeData <- renderUI({
    # if(any(is.na(newdataPCAshiny[,quantiPCAshiny]))) return(checkboxInput("impute",gettext("Impute the dataset (recommended)"),FALSE))
    if(any(is.na(newdataPCAshiny[,quantiPCAshiny]))){
	  return(radioButtons("impute",gettext("Handling missing values"),choices=list(gettext("Impute by the mean (fast but not recommended)"),gettext("Impute with 2-dimensional PCA-model (good compromise)"),gettext("Impute with k-dimensional PCA-model (estime k, time consuming)")),selected=gettext("Impute by the mean (fast but not recommended)")))
	} else {
      return(tags$div(tags$label(class="control-label", "Handling missing values"),
	   tags$div(HTML("No missing values"))))
	}
  })

  values <- reactive({
    if(length(input$supvar)==0) {
	  data.selec <- newdataPCAshiny[,VariableChoicesPCAshiny]
	} else {
	  data.selec <- newdataPCAshiny[,c(getactive())]
	}
    
    if(length(QualiChoicePCAshiny)==0 || length(input$supquali)==0){
      choixquali <- NULL
    } else {
      data.selec <- cbind(data.selec,newdataPCAshiny[,if(!is.null(input$supquali)){input$supquali} else{QualiChoicePCAshiny},drop=FALSE])
      choixquali <- seq((ncol(data.selec)-length(input$supquali)+1),ncol(data.selec))
      colnames(data.selec)[choixquali] <- input$supquali
    }

    if(length(input$supvar)==0){
      choixquanti <- NULL
    } else {
      data.selec <- cbind(data.selec,newdataPCAshiny[,input$supvar])
      if(length(input$supvar)==1){
        choixquanti <- length(data.selec)
        colnames(data.selec)[choixquanti]<-input$supvar
      } else{
        choixquanti <- seq((ncol(data.selec)-length(input$supvar)+1),ncol(data.selec))
      }
    }
    if (length(input$habiller)==2 && input$color_point==gettext("qualitative variable")){
	  codePCAp <- paste0(nomDataPCAshiny," <- data.frame(",nomDataPCAshiny,paste0("[,c(",paste0("'",paste(colnames(data.selec),collapse="','"),"'"),")],newCol=paste(",nomDataPCAshiny,"[,'",input$habiller[1],"'],",nomDataPCAshiny,"[,'",input$habiller[2],"']))\n"))
      data.selec <- data.frame(data.selec,newCol=paste(newdataPCAshiny[,input$habiller[1]],newdataPCAshiny[,input$habiller[2]],sep="/"))
      choixquali <- c(choixquali,ncol(data.selec))
    }
    if(length(input$indsup)==0){
      suple <- NULL
    } else{
      suple <- which(nomPCAshiny%in%input$indsup)
    }
	boolImpute <- FALSE
    if(length(input$impute>0)){
	 if (input$impute!=gettext("Impute by the mean (fast but not recommended)")){
	  boolImpute <- TRUE
	  codePCA <- NULL
	  if (!is.null(suple)) codePCA <- paste0("row.w <- rep(1,nrow(",nomDataPCAshiny,"))\n row.w[",paste0("c(",paste0(suple,collapse=","),")"),"] <- 1e-06\n")
	  if (input$impute==gettext("Impute with k-dimensional PCA-model (estime k, time consuming)")){
		codePCA <- paste0(codePCA,"nb <- missMDA::estim_ncpPCA(",nomDataPCAshiny,")$ncp\n")
		codePCA <- paste0(codePCA, "dfcompleted <- missMDA::imputePCA(",nomDataPCAshiny,", ncp=nb",if (!is.null(suple)) paste0(",row.w=c(",paste0(row.w,collapse=","),")"),")$completeObs\n")
	  } else {
		codePCA <- paste0(codePCA, "dfcompleted <- missMDA::imputePCA(",nomDataPCAshiny,", ncp=2",if (!is.null(suple)) paste0(",row.w=c(",paste0(row.w,collapse=","),")"),")$completeObs\n")
	  }
	  codePCA <- paste0(codePCA,"res.PCA<-PCA(dfcompleted", if (!identical(newdataPCAshiny,data.selec)){paste0("[,c(",paste0("'",paste(colnames(data.selec),collapse="','"),"'"),")]")})
	 }
    }
	if (!boolImpute) codePCA <- paste0("res.PCA<-PCA(",nomDataPCAshiny, if (!identical(newdataPCAshiny,data.selec)){paste0("[,c(",paste0("'",paste(colnames(data.selec),collapse="','"),"'"),")]")})
	codePCA <- paste0(codePCA,if(max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))!=5) paste0(",ncp=",max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))),if(!is.null(choixquali)) paste0(",quali.sup=c(",paste(choixquali,collapse=","),")"),if(!is.null(choixquanti)) paste0(",quanti.sup=c(",paste(choixquanti,collapse=","),")"),if(!is.null(suple)) paste0(",ind.sup=c(",paste(suple,collapse=","),")"),if (!is.null(poids1PCAshiny)) paste0(",row.w=c(",paste(poids1PCAshiny,collapse=","),")"),if (!is.null(poids2PCAshiny)) paste0(",col.w=c(",paste(poids2PCAshiny,collapse=","),")"),if(input$nor!="TRUE") paste0(",scale.unit=",input$nor),",graph=FALSE)")
    if (length(input$habiller)==2 && input$color_point==gettext("qualitative variable")) list(res.PCA=eval(parse(text=paste0(codePCAp,"\n",codePCA))), codePCA=codePCA, codePCAp=codePCAp)
	else list(res.PCA=eval(parse(text=codePCA)), codePCA=codePCA)
  })
  
  output$colourn2 <- renderUI({
    if(length(input$indsup)!=0){
      return(tags$div( 
        div(colourpicker::colourInput("colorsup", label=NULL, if(!is.null(input$colorsup)){if (input$colorsup!="blue") input$colorsup} else{supindPCAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		div(gettext("supplementary individuals"), style="display: inline-block;padding: 0px 0px 0px 10px"))
	  )
    }
  })
 
  output$colourn3 <- renderUI({
	if (length(input$supquali)>0){
        return(tags$div( 
            div(colourpicker::colourInput("colorquali", label=NULL, if(!is.null(input$colorquali)){if (input$colorquali!="magenta") input$colorquali} else{categPCAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		    div(gettext("categories"), style="display: inline-block;padding: 0px 0px 0px 10px"))
		)
    }
  })

  output$pointlabel <- renderUI({
    validate(need(!is.null(input$ind_mod),""))
    choix <- list()
    reponse <- input$ind_mod
    if(sum(gettext("Individuals")==reponse)==0) choix <- c(choix,gettext("Individuals"))
    if(sum(gettext("Supplementary individuals")==reponse)==0) choix <- c(choix,gettext("Supplementary individuals"))
    if(sum(gettext("Supplementary categories")==reponse)==0) choix <- c(choix,gettext("Supplementary categories"))
    div(align="center",checkboxGroupInput("indmodpoint","",choices=choix,selected=labmodPCAshiny))
  })
  
  observe({
    if(input$color_point != gettext("qualitative variable")) updateCheckboxInput(session, "elip", value = FALSE)
  })
  
  output$slider3 <- renderUI({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    if (is.null(getactive())){ 
	  maxvar <- length(VariableChoicesPCAshiny)
    } else {
	  maxvar <- length(getactive())
    }
    if(selection3PCAshiny=="contrib"){
      return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables"),
                                            min=1,max=maxvar,value=selection4,step=1)))  
    } else{
      return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables"),
                                            min=1,max=maxvar,value=maxvar,step=1)))}
  })
  
  output$habillage2 <- renderUI({
   if (input$color_point == gettext("quantitative variable")) return(selectizeInput("habiller",gettext("select the variable"), choices=c(getactive(),input$supvar),multiple=FALSE,selected=habillageindPCAshiny))
   if (input$color_point == gettext("qualitative variable")){
     if(length(QualiChoicePCAshiny)==0 || input$supquali==FALSE || length(input$supquali)==0) return(p(gettext("No categorical variable")))
     if(length(input$supquali)>=1) return(selectizeInput("habiller",gettext("select 1 or 2 variables"), choices=input$supquali, multiple=TRUE, selected=habillageindPCAshiny))
   }
  })
  
  output$ellipsesPCAshiny <- renderUI({
    if(length(input$supquali)>=1 & (input$color_point == gettext("qualitative variable"))) return(checkboxInput("elip",gettext("Draw the confidence ellipses around the categories"),ellipsesPCAshiny))
  })
  
  output$choixindmod <- renderUI({
    choix <- gettext("Individuals")
    bool1 <- FALSE
    if(length(input$indsup)>0){
      choix <- c(choix,gettext("Supplementary individuals"))
      bool1 <- TRUE
    }
    if (length(input$supquali)>0){
      if(length(QualiChoicePCAshiny)>0 & sum(input$supquali!=FALSE)>0){
        choix <- c(choix,gettext("Supplementary categories"))
        bool1 <- TRUE
      }}
    if(bool1==TRUE) div(align="left",checkboxGroupInput("ind_mod",gettext("Points to draw"), choices=choix, selected = indmodPCAshiny))
  })
    

  codeGraphInd <- reactive({
    validate(
      need(input$nb1 != input$nb2, gettext("Please select two different dimensions")),
      need(length(getactive())>1 ,gettext("Please select at least two active variables")),
      need(!is.null(input$habiller) || length(input$habiller)<=2,gettext("Please select maximum 2 variables as habillage"))
    )
    if(input$select=="cos2"){
      if(input$slider1!=1){
        selecindiv <- paste("cos2 ",input$slider1)
      } else{
        selecindiv <- "cos2 0.999999"
      }
      selecindivtext <- paste0("'",selecindiv,"'")
    }
    if(input$select==gettext("No selection")){
      selecindiv <- NULL
      selecindivtext <- "NULL"
    }
    if(input$select=="contrib"){
      selecindiv <- paste("contrib ",input$slider0)
      selecindivtext <- paste0("'",selecindiv,"'")
    }

    if(input$select==gettext("Manual")){
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
    if(input$color_point == gettext("quantitative variable")) hab <- paste0("'",input$habiller,"'")
    if(input$color_point == "cos2") hab <- "'cos2'"
    if(input$color_point == "contribution") hab <- "'contrib'"
    if(input$color_point==gettext("qualitative variable")){
      if(length(input$supquali)>=1){
        if (length(input$habiller)==1) hab <- which(colnames(values()$res.PCA$call$X)==input$habiller)
        if (length(input$habiller)==2) hab <- ncol(values()$res.PCA$call$X)
      }
	}
	
    if(input$select==gettext("Manual")){
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
      if(sum(gettext("Individuals")==input$ind_mod)==0) inv<- "ind"
      if(sum(gettext("Supplementary categories")==input$ind_mod)==0) inv<-c(inv,"quali")
      if(sum(gettext("Supplementary individuals")==input$ind_mod)==0) inv<-c(inv,"ind.sup")
	}
    bool <- (!is.null(input$elip) && input$elip==TRUE && input$color_point == gettext("qualitative variable"))

	res.PCA <- values()$res.PCA
    Code <- paste0(if (bool){"plotellipses"} else{"plot.PCA"},"(res.PCA", if (bool) paste0(", keepvar=",if (is.numeric(hab)){hab} else {which(colnames(values()$res.PCA$call$X)==hab)}),if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if(!is.null(inv)){paste0(",invisible=c(",paste0("'",paste(inv,collapse="','"),"'"),")")}, if(selecindivtext!="NULL"){paste0(",select=",selecindivtext)},if (!bool & hab!="none" & hab!="''"){paste0(",habillage=",hab)},if(input$title1!="PCA graph of individuals")paste0(',title="',input$title1,'"'),if(input$cex!=1)paste0(",cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex),if(input$coloract!="#000000")paste0(",col.ind='",input$coloract,"'"), if(!is.null(input$colorsup)){ if(input$colorsup!="#0000FF") paste0(",col.ind.sup='",input$colorsup,"'")},if(!is.null(input$colorquali)) { if (input$colorquali!="#FF00FF")paste0(",col.quali='",input$colorquali,"'")},")")
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
		    div(gettext("supplementary variables"), style="display: inline-block;padding: 0px 0px 0px 10px")
			)
		)
    }
  })
  
  codeGraphVar <- reactive({
    validate(
      need(input$nb1 != input$nb2, gettext("Please select two different dimensions")),
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    if(input$select0=="cos2"){
      if(input$slider00!=1){    # else if cos2 = 1, one point is drawn
        selecindiv <- paste("cos2 ",input$slider00)
      } else{
        selecindiv <- NULL
      }
    }
    if(input$select0==gettext("No selection")) selecindiv <- NULL
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
    choix <- list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results of the variables"),gettext("Results of the individuals"))
    if(length(input$indsup)!=0) choix <- c(choix,gettext("Results of the supplementary individuals"))
	if(length(input$supvar)!=0) choix <- c(choix,gettext("Results of the supplementary variables"))
	if (length(input$supquali)!=0) choix <- c(choix,gettext("Results of the categorical variables"))
    radioButtons("out",gettext("Which outputs do you want?"),
                 choices=choix,selected=gettext("Summary of outputs"),inline=TRUE)
  })
  
  output$sorties <- renderTable({
    return(as.data.frame(values()$res.PCA$eig))
  },rownames <- TRUE)
  
  output$sorties12 <- renderTable({
    validate(
      need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected")),
      need(length(getactive())>1 ,gettext("Please select at least one supplementary variable"))
    )
    return(as.data.frame(values()$res.PCA$quali.sup$coord))
  },rownames=TRUE)
  
  output$sorties13 <- renderTable({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables")),
      need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
    )
    return(as.data.frame(values()$res.PCA$quali.sup$v.test))
  },rownames=TRUE)
  
  output$sorties2 <- renderTable({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    return(as.data.frame(values()$res.PCA$var$coord))
  },rownames=TRUE)
  
  output$sorties22 <- renderTable({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    return(as.data.frame(values()$res.PCA$ind$coord))
  },rownames=TRUE)
  
  output$sorties23 <- renderTable({
    validate(
      need(length(input$supvar)!=0, gettext("No supplementary quantitative variables")),
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    return(as.data.frame(values()$res.PCA$quanti.sup$coord))
  },rownames=TRUE)
  
  output$sorties32 <- renderTable({
    validate(
      need(length(input$supvar)!=0, gettext("No supplementary quantitative variables")),
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    return(as.data.frame(values()$res.PCA$quanti.sup$cor))
  },rownames=TRUE)
  
  output$sorties36 <- renderTable({
    validate(
      need(length(input$indsup)!=0, gettext("No supplementary individuals")),
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    return(as.data.frame(values()$res.PCA$ind.sup$coord))
  },rownames=TRUE)
  
  output$sorties37 <- renderTable({
    validate(
      need(length(input$indsup)!=0, gettext("No supplementary individuals")),
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    return(as.data.frame(values()$res.PCA$ind.sup$cos2))
  },rownames=TRUE)
  
  
  output$sorties3 <- renderTable({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    return(as.data.frame(values()$res.PCA$var$contrib))
  },rownames=TRUE)
  
  output$sorties33 <- renderTable({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    return(as.data.frame(values()$res.PCA$ind$contrib))
  },rownames=TRUE)
  
  output$sorties4 <- renderTable({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    return(as.data.frame(values()$res.PCA$var$cos2))
  },rownames=TRUE)
  
  output$sorties44 <- renderTable({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    return(as.data.frame(values()$res.PCA$ind$cos2))
  },rownames=TRUE)
  
  output$sortieDimdesc3 <- renderTable({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables")),
      need(length(dimdesc(values()$res.PCA))>0,gettext("No quantitative variable describes axis 1"))
	)
    return(as.data.frame(dimdesc(values()$res.PCA)[[1]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc4 <- renderTable({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables")),
      need(length(dimdesc(values()$res.PCA))>0,gettext("No categorical variable describes axis 1"))
	)
    return(as.data.frame(dimdesc(values()$res.PCA)[[1]]$quali))
  },rownames=TRUE)
  
  #DIM2
  
  output$sortieDimdesc33 <- renderTable({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables")),
      need(length(dimdesc(values()$res.PCA))>1,gettext("No quantitative variable describes axis 2"))
	)
    return(as.data.frame(dimdesc(values()$res.PCA)[[2]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc44 <- renderTable({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables")),
      need(length(dimdesc(values()$res.PCA))>1,gettext("No categorical variable describes axis 2"))
	)
    return(as.data.frame(dimdesc(values()$res.PCA)[[2]]$quali))
  },rownames=TRUE)
  
  #DIM3
  
  output$sortieDimdesc333 <- renderTable({
    validate(
      need(length(getactive())>2 ,gettext("There are not three dimensions")),
      need(length(dimdesc(values()$res.PCA))>2,gettext("No quantitative variable describes axis 3"))
	)
    return(as.data.frame(dimdesc(values()$res.PCA)[[3]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc444 <- renderTable({
    validate(
      need(length(getactive())>2 ,gettext("There are not three dimensions")),
      need(length(dimdesc(values()$res.PCA))>2,gettext("No categorical variable describes axis 3"))
	)
    return(as.data.frame(dimdesc(values()$res.PCA)[[3]]$quali))
  },rownames=TRUE)
  
  
  output$map3 <- renderPlot({
    print(ggplot2::ggplot(cbind.data.frame(x=1:nrow(values()$res.PCA$eig),y=values()$res.PCA$eig[,2])) + ggplot2::aes(x=x, y=y)+ ggplot2::geom_col(fill="blue") + ggplot2::xlab("Dimension") + ggplot2::ylab(gettext("Percentage of variance")) + ggplot2::ggtitle(gettext("Decomposition of the total inertia")) + ggplot2::theme_light() + ggplot2::theme(plot.title = ggplot2::element_text(hjust =0.5))  + ggplot2::scale_x_continuous(breaks=1:nrow(values()$res.PCA$eig)))
    # return(barplot(values()$res.PCA$eig[,1],names.arg=rownames(values()$res.PCA$eig),las=2))
  })
  
  output$JDD <- renderDataTable({
    cbind(Names=rownames(newdataPCAshiny),newdataPCAshiny)},
    options = list(    "orderClasses" = TRUE, "responsive" = TRUE, "pageLength" = 10))
  
  output$summary <- renderPrint({
    summary(newdataPCAshiny)
  })
  
  output$summaryPCA <- renderPrint({
    validate(
      need(input$nbele!=0, gettext("Please select at least one element"))
    )
    a<-values()$res.PCA
    a$call$call<-values()$codePCA
    summary.PCA(a,nbelements=input$nbele)
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
        FactoInvestigate::Investigate(values()$res.PCA, codeGraphInd = if (input$choixGRAPH==gettext("Graphs done")) {paste0(values()$codePCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done")) {codeGraphVar()$Code} else {NULL}, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= substr(tolower(input$choixLANG),1,2))
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$Investigatedoc!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsavePCAshiny)
        FactoInvestigate::Investigate(values()$res.PCA,codeGraphInd = if (input$choixGRAPH==gettext("Graphs done")) {paste0(values()$codePCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done")) {codeGraphVar()$Code} else {NULL}, document="word_document",openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language=substr(tolower(input$choixLANG),1,2))
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$InvestigateRmd!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsavePCAshiny)
	    FactoInvestigate::Investigate(values()$res.PCA, codeGraphInd = if (input$choixGRAPH==gettext("Graphs done")) {paste0(values()$codePCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done")) {codeGraphVar()$Code} else {NULL},  openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= substr(tolower(input$choixLANG),1,2))
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
	
  observe({
    if(input$PCAcode!=0){
      isolate({
        if (length(input$habiller)==2 & input$color_point==gettext("qualitative variable")) cat(values()$codePCAp,sep="\n")
        cat(values()$codePCA,sep="\n")
        cat(codeGraphVar()$Code,sep="\n")
        cat(codeGraphInd()$Code,sep="\n")
      })
    }
  })

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
        if(input$select=="cos2") selecindiv <- input$slider1
        if(input$select==gettext("No selection")) selecindiv <- NULL
        if(input$select=="contrib") selecindiv <- input$slider0
        if(input$select==gettext("Manual")) selecindiv <- input$indiv
        res$selectionPCAshiny <- input$select
        res$selection2PCAshiny <- selecindiv
        selecindiv2 <- NULL
        if(input$select0=="cos2") selecindiv2 <- input$slider00
        if(input$select0=="contrib") selecindiv2 <- input$slider4
        res$j <- input$select0
        res$k <- selecindiv2
        res$l <- input$cex
        res$m <- input$cex2
	    res$color_point <- input$color_point
	    res$color_arrow <- input$color_arrow
        res$codePCAp <- values()$codePCAp
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
        class(res) <- "PCAshiny"
        stopApp(returnValue=res)
      })
    }
  })
  
 }
