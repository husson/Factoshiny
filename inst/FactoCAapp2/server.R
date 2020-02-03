# server scipt for CA2
  function(input, output) {
  
  output$NB1 <- renderUI({
    validate(
      need((length(VariableChoicesCAshiny)-length(input$supvar)-length(input$quantisupvar))>2 ,gettext("Please select at least three active columns",domain="R-Factoshiny"))
    )
    if((length(VariableChoicesCAshiny)-length(input$supvar)-length(input$quantisupvar))>5){
       return(textInput("nb1", label = NULL, axe1CAshiny,width='41px'))
    } else{
       return(selectInput("nb1",label=NULL, choices=1:(min(nrow(values()$res.CA$row$coord),nrow(values()$res.CA$col$coord))-1),selected=axe1CAshiny,width='51px'))
    }
  })

  output$NB2 <- renderUI({
    validate(
      need((length(VariableChoicesCAshiny)-length(input$supvar)-length(input$quantisupvar))>2 ,gettext("Please select at least three active rows",domain="R-Factoshiny"))
    )
    if((length(VariableChoicesCAshiny)-length(input$supvar)-length(input$quantisupvar))>5){
       return(textInput("nb2", label = NULL, axe2CAshiny,width='41px'))
    } else{
       return(selectInput("nb2",label=NULL, choices=1:(min(nrow(values()$res.CA$row$coord),nrow(values()$res.CA$col$coord))-1),selected=axe2CAshiny,width='51px'))
    }
  })

  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
        return(tags$div( 
            div(gettext("Number of dimensions kept for clustering",domain="R-Factoshiny"), style="display: inline-block; padding: 0px 0px 0px 0px"),
		    div(numericInput(inputId = "nbDimClustering", label = NULL,value=if(is.null(nbdimclustCAshiny)){5} else {nbdimclustCAshiny},min=1), style="display: inline-block;width: 70px; padding: 0px 0px 0px 10px"))
		)
    }
  })
    
  output$imputeData <- renderUI({
    if(any(is.na(newdataCAshiny[,VariableChoicesCAshiny]))){
	  return(radioButtons("impute",gettext("Handling missing values",domain="R-Factoshiny"),choices=list(gettext("Consider NA as supplementary",domain="R-Factoshiny"),gettext("Impute by the independance model",domain="R-Factoshiny"),gettext("Impute with 2-dimensional CA-model (preferred)",domain="R-Factoshiny")),selected=gettext("Consider NA as supplementary",domain="R-Factoshiny")))
	} else {
      return(tags$div(tags$label(class="control-label", gettext("Handling missing values",domain="R-Factoshiny")),
	   tags$div(HTML(gettext("No missing values",domain="R-Factoshiny")))))
	}
  })

  values <- reactive({
	 if (length(input$nb1)>0){
	   if (max(input$nb1,input$nb2)>5) return(isolate(valeur()))
	 }
	 if (length(input$nbDimClustering)>0){
	   if (input$nbDimClustering >5) return(isolate(valeur()))
	 }
     if (length(input$caparam)==0){
	   return(valeur())
	 } else {
        if (input$submit>=0) isolate(valeur())
     }
 })

  valeur <- function(){
      validate(
        need((length(VariableChoicesCAshiny)-length(input$supvar)-length(input$quantisupvar))>2, gettext("Please select at least three active columns",domain="R-Factoshiny"))
      )
	  
    NomCol <- colnames(newdataCAshiny)
	SuppressCol <- NULL
    if (length(QualiChoiceCAshiny)!=0){
	  if (length(QualiChoiceCAshiny)!=length(input$supquali)) {
	    SuppressCol <- which(NomCol%in%setdiff(QualiChoiceCAshiny,input$supquali))
	    NomCol <- NomCol[-SuppressCol]
	  }
	}
    ColSup <- which(NomCol%in%input$supvar)
	QuantiSup <- which(NomCol%in%input$quantisupvar)
	QualiSup <- which(NomCol%in%input$supquali)
    RowSup <- which(nomCAshiny%in%input$rowsupl)
	if (length(ColSup)==0) ColSup <- NULL
	if (length(QuantiSup)==0) QuantiSup <- NULL
	if (length(QualiSup)==0) QualiSup <- NULL
    if(length(input$rowsupl)==0) RowSup <- NULL
	codeCA <- NULL
	nomTabDon <- paste0(nomDataCAshiny, if (!is.null(SuppressCol)){paste0("[,-c(",paste0(SuppressCol,collapse=","),")]")})

	boolImpute <- FALSE
    if(any(is.na(newdataCAshiny[,VariableChoicesCAshiny]))){
	 if (length(input$impute)==0){
       RowSup <- c(RowSup,which(apply(is.na(newdataCAshiny),1,sum)>0))
       ColSup <- c(ColSup,which(apply(is.na(newdataCAshiny),2,sum)>0))
	 } else {
	   if (input$impute==gettext("Consider NA as supplementary",domain="R-Factoshiny")){
         RowSup <- c(RowSup,which(apply(is.na(newdataCAshiny),1,sum)>0))
         ColSup <- c(ColSup,which(apply(is.na(newdataCAshiny),2,sum)>0))
	   } else {
	     boolImpute <- TRUE
	     codeCA <- paste0(codeCA, "dfcompleted <- missMDA::imputeCA(",nomTabDon,", ncp=",if (input$impute==gettext("Impute by independence model",domain="R-Factoshiny")) {"0"} else {"2"},if (!is.null(QuantiSup)) paste0(",quanti.sup=c(",paste0(QuantiSup,collapse=","),")"),if (!is.null(QualiSup)) paste0(",quali.sup=c(",paste0(QualiSup,collapse=","),")"),if (!is.null(ColSup)) paste0(",col.sup=c(",paste0(ColSup,collapse=","),")"),if (!is.null(RowSup)) paste0(",row.sup=c(",paste0(RowSup,collapse=","),")"),")\n")
	     codeCA <- paste0(codeCA,"res.CA<-CA(dfcompleted", if (!is.null(SuppressCol)){paste0("[,-c(",paste0(SuppressCol,collapse=","),")]")})
	   }
     }
	}
	if (!boolImpute) codeCA <- paste0(codeCA,"res.CA<-CA(",nomTabDon)

	codeCA <- paste0(codeCA,if(max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))!=5) paste0(",ncp=",max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))),if(!is.null(QualiSup)) paste0(",quali.sup=c(",paste(QualiSup,collapse=","),")"),if(!is.null(ColSup)) paste0(",col.sup=c(",paste(ColSup,collapse=","),")"),if(!is.null(QuantiSup)) paste0(",quanti.sup=c(",paste(QuantiSup,collapse=","),")"),if(!is.null(RowSup)) paste0(",row.sup=c(",paste(RowSup,collapse=","),")"),",graph=FALSE)")
    list(res.CA=eval(parse(text=codeCA)), codeCA=codeCA)
    }
    
  output$choixinvis <- renderUI({
     listechoix <- c(gettext("Rows",domain="R-Factoshiny"),gettext("Columns",domain="R-Factoshiny"))
	 if (!is.null(input$rowsupl)) listechoix <- c(listechoix,gettext("Supplementary rows",domain="R-Factoshiny"))
	 if (!is.null(input$supvar)) listechoix <- c(listechoix,gettext("Supplementary columns",domain="R-Factoshiny"))
	 if (!is.null(input$supquali)) listechoix <- c(listechoix,gettext("Supplementary qualitative variables",domain="R-Factoshiny"))
     return(selectInput("invis",gettext("Invisible elements",domain="R-Factoshiny"),choices=as.list(listechoix),multiple=TRUE,selected=InvisibleCAshiny))
  })

  output$col1CAshiny=renderUI({
    if(sum(gettext("Rows",domain="R-Factoshiny")==input$invis)==0){
      return(tags$div( 
        div(colourpicker::colourInput("colrow", label=NULL, if(!is.null(input$colrow)){if (input$colrow!="blue") input$colrow} else{col1CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
        div(gettext("active rows",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"))
 	  )
    }
  })
  output$col2CAshiny=renderUI({
    if(sum(gettext("Columns",domain="R-Factoshiny")==input$invis)==0){
      return(tags$div( 
        div(colourpicker::colourInput("colcol", label=NULL, if(!is.null(input$colcol)){if (input$colcol!="red") input$colcol} else{col2CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
	    div(gettext("active columns",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"))
	  )
    }
  })
  output$col3CAshiny=renderUI({
    if(!is.null(values()$res.CA$row.sup) & sum(gettext("Supplementary rows",domain="R-Factoshiny")==input$invis)==0){
      return(tags$div( 
        div(colourpicker::colourInput("colrowsup", label=NULL, if(!is.null(input$colrowsup)){if (input$colrowsup!="darkblue") input$colrowsup} else{col3CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
	    div(gettext("supplementary rows",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"))
	  )
    }
  })
  output$col4CAshiny=renderUI({
    if(!is.null(values()$res.CA$col.sup) & sum(gettext("Supplementary columns",domain="R-Factoshiny")==input$invis)==0){
      return(tags$div( 
        div(colourpicker::colourInput("colcolsup", label=NULL, if(!is.null(input$colcolsup)){if (input$colcolsup!="darkred") input$colcolsup} else{col4CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
	    div(gettext("supplementary columns",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"))
	  )
    }
  })
    
  output$col5CAshiny=renderUI({
    if(!is.null(values()$res.CA$quali.sup) & sum(gettext("Supplementary categories",domain="R-Factoshiny")==input$invis)==0){
      return(tags$div( 
        div(colourpicker::colourInput("colqualisup", label=NULL, if(!is.null(input$colqualisup)){if (input$colqualisup!="magenta") input$colqualisup} else{col5CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
	    div(gettext("supplementary categories",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"))
	  )
    }
  })

  output$ellipsesCAshiny=renderUI({
    values1=c()
    if(sum(gettext("Rows",domain="R-Factoshiny")==input$invis)==0){
      values1=c(values1,gettext("Rows",domain="R-Factoshiny"))
    }
    if(sum(gettext("Columns",domain="R-Factoshiny")==input$invis)==0){
      values1=c(values1,gettext("Columns",domain="R-Factoshiny"))
    }
    if(length(values)!=0){
      return(checkboxGroupInput("ellip",gettext("Draw confidence ellipses around",domain="R-Factoshiny"),choices=values1,selected=ellipsesCAshiny,inline=TRUE))
    }
  })
    
    output$contribcol=renderUI({
      maxx=nrow(values()$res.CA$col$coord)
      if(selec1CAshiny=="contrib"){
        return(sliderInput("contrib1",gettext("Number of the most contributive active columns",domain="R-Factoshiny"),min=1,max=maxx,value=valueselec2CAshiny,step=1))
      } else{
        return(sliderInput("contrib1",gettext("Number of the most contributive active columns",domain="R-Factoshiny"),min=1,max=maxx,value=maxx,step=1))
      }
      
    })
    
    output$contribrow=renderUI({
      maxx=nrow(values()$res.CA$row$coord)
      if(selec2CAshiny=="contrib"){
        return(sliderInput("contrib2",gettext("Number of the most contributive active rows",domain="R-Factoshiny"),min=1,max=maxx,value=valueselec2CAshiny,step=1))
      }
      else{
        return(sliderInput("contrib2",gettext("Number of the most contributive active rows",domain="R-Factoshiny"),min=1,max=maxx,value=maxx,step=1))
      }
    })
    
  codeGraph <- reactive({
    validate(
      need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny")),
      need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows",domain="R-Factoshiny")),
      need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns",domain="R-Factoshiny"))
    )
    if(length(input$invis)==0){
      invisiText <- NULL
    } else {
      invisi <- NULL
      if(sum(gettext("Rows",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"row")
      if(sum(gettext("Columns",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"col")
      if(sum(gettext("Supplementary rows",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"row.sup")
      if(sum(gettext("Supplementary columns",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"col.sup")
      if(sum(gettext("Supplementary qualitative variables",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"quali.sup")
      if(sum(gettext("Supplementary quantitative variables",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"quanti.sup")
      invisiText <- paste0("c(",paste(paste("'",invisi,"'",sep=""),collapse = ","),")")
    }
    sel=NULL
    if(input$seleccol=="cos2"){
      if(input$slider3!=1){
        sel=paste0("'cos2 ",input$slider3,"'")
      } else{
        sel="'cos2 0.9999999'"
      }
    }
    if(input$seleccol=="contrib") sel=paste0("'contrib ",input$contrib1,"'")
    sel2=NULL
    if(input$selecrow=="cos2"){
      if(input$slider4!=1){
        sel2=paste0("'cos2 ",input$slider4,"'")
      } else{
        sel2="'cos2 0.9999999'"
      }
    }
    if(input$selecrow=="contrib") sel2=paste0("'contrib ",input$contrib2,"'")
    values2=c()
    if(!is.null(input$ellip)){
      if(gettext("Columns",domain="R-Factoshiny")%in%input$ellip) values2=c(values2,"col")
      if(gettext("Rows",domain="R-Factoshiny")%in%input$ellip) values2=c(values2,"row")
    }

    hab <- "none"
    if(input$color_point == gettext("quantitative variable",domain="R-Factoshiny")) hab <- paste0("'",input$habiller,"'")
    if(input$color_point == "cos2") hab <- "'cos2'"
    if(input$color_point == "contribution") hab <- "'contrib'"
    if(input$color_point==gettext("qualitative variable",domain="R-Factoshiny")) hab <- which(colnames(values()$res.CA$call$X)==input$habiller)

      if(is.null(input$ellip)||length(input$ellip)==0){
	    myellip=NULL
      }else{
        vect=c()
        if(gettext("Columns",domain="R-Factoshiny")%in%input$ellip) vect=c(vect,"col")
        if(gettext("Rows",domain="R-Factoshiny")%in%input$ellip) vect=c(vect,"row")
        myellip=paste(paste("'",vect,"'",sep=""),collapse=",")
      }
      Code <- paste0(if(is.null(myellip)){"plot.CA"}else{"ellipseCA"},"(res.CA",if(!is.null(myellip)) paste0(",ellipse=c(",myellip,")"),if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (!is.null(sel)) paste0(",selectCol=",sel),if (!is.null(sel2)) paste0(",selectRow=",sel2),if (!is.null(sel2) | !is.null(sel)) paste0(',unselect=0'),if (input$cex!=1) paste0(',cex=',input$cex,',cex.main=',input$cex,',cex.axis=',input$cex),if(input$title1CAshiny!="CA factor map")paste0(',title="',input$title1CAshiny,'"'),if (hab!="none" & hab!="''"){paste0(",habillage=",hab)},if (!is.null(input$colrow)) {if (input$colrow!="#0000FF") paste0(",col.row='",input$colrow,"'")},if (!is.null(input$colcol)){ if (input$colcol!="#FF0000") paste0(",col.col='",input$colcol,"'")},if (!is.null(input$colrowsup)){if (input$colrowsup!="#0C2B94") paste0(",col.row.sup='",input$colrowsup,"'")},if (!is.null(input$colcolsup)){ if (input$colcolsup!="#8B0000") paste0(",col.col.sup='",input$colcolsup,"'")},if (!is.null(input$colqualisup)) paste0(",col.quali.sup='",input$colqualisup,"'"),if (!is.null(invisiText)) paste0(',invisible=',invisiText),')')
	  res.CA <- values()$res.CA
	  Plot <- eval(parse(text=Code))
      return(list(Code=Code, Plot=Plot))
    })
    
    output$map <- renderPlot({
        p <- print(codeGraph()$Plot)
    })
    
  codeGraphQuanti <- function(){
    validate(
      need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny")),
      need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows",domain="R-Factoshiny")),
      need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns",domain="R-Factoshiny"))
    )
      if (!is.null(values()$res.CA$quanti.sup)) {
		Code <- paste0("plot.CA(res.CA, choix='quanti.sup'",if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),paste0(',title="',if (length(input$title2CAshiny)==0) {title2CAshiny} else {input$title2CAshiny},'"'),")")
		res.CA <- values()$res.CA
		Plot <- eval(parse(text=Code))
	    return(list(Code=Code,Plot=Plot))
	  } else {return(NULL)}
  }
    
  output$Titre2 <- renderUI({
	  if (!is.null(values()$res.CA$quanti.sup)) {
	    textInput("title2CAshiny",gettext("Title of the graph for quantitative variables:",domain="R-Factoshiny"),title2CAshiny)
	  }
  })

  output$map2 <- renderPlot({
    validate(
      need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny")),
      need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows",domain="R-Factoshiny")),
      need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns",domain="R-Factoshiny"))
    )
	  if (!is.null(codeGraphQuanti()$Plot)) print(codeGraphQuanti()$Plot)
    })  
    
    output$map22=renderUI({
      validate(
        need( (length(VariableChoicesCAshiny)-length(input$supvar)-length(input$quantisupvar))>2,gettext("Please more active columns",domain="R-Factoshiny"))
        # need( length(input$quantisupvar)==0 | (input$submit>0 | input$caparam==FALSE) ,gettext("Submit or close the parameters window",domain="R-Factoshiny"))
      )
      
      if(is.null(codeGraphQuanti()$Plot)){
	    p()
      } else{
        column(width = 5,shinyjqui::jqui_resizable(plotOutput("map2", height="500")),
           br(),
           p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData4",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData3",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData5",gettext("pdf",domain="R-Factoshiny")),align="center")
		)
	  }
    })

    output$out22=renderUI({
      choix=list(gettext("Summary of outputs",domain="R-Factoshiny"),gettext("Eigenvalues",domain="R-Factoshiny"),gettext("Results for the columns",domain="R-Factoshiny"),gettext("Results for the rows",domain="R-Factoshiny"))
      if(length(input$rowsupl)!=0){
        choix=c(choix,gettext("Results for the supplementary rows",domain="R-Factoshiny"))
      }
      if(!is.null(values()$res.CA$col.sup)){
        choix=c(choix,gettext("Results for the supplementary columns",domain="R-Factoshiny"))
      }
      if(!is.null(values()$res.CA$quali.sup)){
        choix=c(choix,gettext("Results for the categorical variables",domain="R-Factoshiny"))
      }
      radioButtons("out",gettext("Which outputs do you want?",domain="R-Factoshiny"),
                   choices=choix,selected=gettext("Summary of outputs",domain="R-Factoshiny"),inline=TRUE)
    })
    
    output$warn <- renderPrint({
       if (any(is.na(newdataCAshiny))){
         rowNA=paste0(gettext("Warning: ",domain="R-Factoshiny"), paste(rownames(newdataCAshiny)[which(apply(is.na(newdataCAshiny),1,sum)>0)],collapse=", "), gettext(" have NA : they are considered as supplementary rows",domain="R-Factoshiny"))
         colNA=paste0(gettext("Warning: ",domain="R-Factoshiny"), paste(colnames(newdataCAshiny)[which(apply(is.na(newdataCAshiny),2,sum)>0)],collapse=", "), gettext(" have NA : they are considered as supplementary columns",domain="R-Factoshiny"))
	     if (length(input$impute)==0) { 
		   return(cat(rowNA,colNA,sep="\n"))
		 } else {
	       if (input$submit>=0) isolate({
		     if (input$impute==gettext("Consider NA as supplementary",domain="R-Factoshiny")) return(cat(rowNA,colNA,sep="\n"))
			 })
		 }
       }
    })
    
    output$sorties=renderTable({
      return(as.data.frame(values()$res.CA$eig))
    },rownames=TRUE)
    
    output$sorties1=renderTable({
      validate(
      need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.CA$col$coord))
    },rownames=TRUE)
    
    output$sorties2=renderTable({
      validate(
      need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.CA$col$cos2))
    },rownames=TRUE)
    
    output$sorties3=renderTable({
      validate(
      need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.CA$col$contrib))
    },rownames=TRUE)
    
    output$sorties4=renderTable({
      validate(
        need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows",domain="R-Factoshiny"))
	  )
      return(as.data.frame(values()$res.CA$row$coord))
    },rownames=TRUE)
    
    output$sorties5=renderTable({
      validate(
        need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows",domain="R-Factoshiny"))
	  )
      return(as.data.frame(values()$res.CA$row$cos2))
    },rownames=TRUE)
    
    output$sorties6=renderTable({
      validate(
        need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows",domain="R-Factoshiny"))
	  )
      return(as.data.frame(values()$res.CA$row$contrib))
    },rownames=TRUE)
    
    output$sorties7=renderTable({
      validate(
        need((length(input$rowsupl)>0), gettext("No supplementary rows selected",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.CA$row.sup$coord))
    },rownames=TRUE)
    
    output$sorties8=renderTable({
      return(as.data.frame(values()$res.CA$row.sup$cos2))
    },rownames=TRUE)
    
    output$sorties9=renderTable({
      return(as.data.frame(values()$res.CA$col.sup$coord))
    },rownames=TRUE)
    
    output$sorties10=renderTable({
      return(as.data.frame(values()$res.CA$col.sup$cos2))
    },rownames=TRUE)
    
    output$sorties11=renderTable({
      validate(
        need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.CA$quali.sup))
    },rownames=TRUE)
    
    
    output$map3=renderPlot({
    print(ggplot2::ggplot(cbind.data.frame(x=1:nrow(values()$res.CA$eig),y=values()$res.CA$eig[,2])) + ggplot2::aes(x=x, y=y)+ ggplot2::geom_col(fill="blue") + ggplot2::xlab("Dimension") + ggplot2::ylab(gettext("Percentage of variance",domain="R-Factoshiny")) + ggplot2::ggtitle(gettext("Decomposition of the total inertia",domain="R-Factoshiny")) + ggplot2::theme_light() + ggplot2::theme(plot.title = ggplot2::element_text(hjust =0.5))  + ggplot2::scale_x_continuous(breaks=1:nrow(values()$res.CA$eig)))
      # return(barplot(values()$res.CA$eig[,1],names.arg=rownames(values()$res.CA$eig),las=2))
    })
    
    output$JDD=DT::renderDataTable({
      cbind(Names=rownames(newdataCAshiny),newdataCAshiny)},
      options = list(    "orderClasses" = TRUE,
                         "responsive" = TRUE,
                         "pageLength" = 10), rownames=FALSE)
    
    output$summary=renderPrint({
      summary(newdataCAshiny)
    })
    
    output$summaryCA=renderPrint({
      a<-values()$res.CA
      a$call$call<- values()$codeCA
      summary.CA(a,nbelements=input$nbele)
    })
    
    output$summary2=downloadHandler(filename = function() { 
      paste('summaryofCA','.txt', sep='') 
    },
    content = function(file) {
      summary.CA(values()$res.CA,nbelements=input$nbele,file=file)
    },
    contentType='text/csv')
    
  observe({
    if(input$Investigatehtml!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveCAshiny)
        if (substr(tolower(input$choixLANG),1,2)=="fr") FactoInvestigate::Investigate(values()$res.CA, codeGraphCA = if (input$choixGRAPH==gettext("Graph done",domain="R-Factoshiny")){paste0(values()$codeCA,"\n",codeGraph()$Code)} else {NULL}, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "fr")
        else FactoInvestigate::Investigate(values()$res.CA, codeGraphCA = if (input$choixGRAPH==gettext("Graph done",domain="R-Factoshiny")){paste0(values()$codeCA,"\n",codeGraph()$Code)} else {NULL}, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "en")
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$Investigatedoc!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveCAshiny)
        if (substr(tolower(input$choixLANG),1,2)=="fr") FactoInvestigate::Investigate(values()$res.CA, codeGraphCA = if (input$choixGRAPH==gettext("Graph done",domain="R-Factoshiny")){paste0(values()$codeCA,"\n",codeGraph()$Code)} else {NULL}, document="word_document", openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "fr")
        else FactoInvestigate::Investigate(values()$res.CA, codeGraphCA = if (input$choixGRAPH==gettext("Graph done",domain="R-Factoshiny")){paste0(values()$codeCA,"\n",codeGraph()$Code)} else {NULL}, document="word_document", openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "en")
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$InvestigateRmd!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveCAshiny)
        if (substr(tolower(input$choixLANG),1,2)=="fr") FactoInvestigate::Investigate(values()$res.CA, codeGraphCA = if (input$choixGRAPH==gettext("Graph done",domain="R-Factoshiny")){paste0(values()$codeCA,"\n",codeGraph()$Code)} else {NULL}, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "fr")
        else FactoInvestigate::Investigate(values()$res.CA, codeGraphCA = if (input$choixGRAPH==gettext("Graph done",domain="R-Factoshiny")){paste0(values()$codeCA,"\n",codeGraph()$Code)} else {NULL}, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "en")
	    print(paste0(gettext("The file ",domain="R-Factoshiny"),input$titleFile,gettext(" as well as the RData objects are available in the sub-directory: ",domain="R-Factoshiny"),getwd()))
        setwd(path.aux)
      })
    }
  })

    output$downloadData = downloadHandler(
      filename = function() { 
        paste('graphCA','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraph()$Plot)
      },
      contentType='image/png')
    
    output$downloadData1 = downloadHandler(
      filename = function() { 
        paste('graphCA','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraph()$Plot)
      },
      contentType='image/jpg')
    
    output$downloadData2 = downloadHandler(
      filename = function() { 
        paste('graphCA','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraph()$Plot)
      },
      contentType=NA)
    
    output$downloadData3 = downloadHandler(
      filename = function() { 
        paste('graphCAquanti','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraphQuanti()$Plot)
      },
      contentType='image/png')
    
    output$downloadData4 = downloadHandler(
      filename = function() { 
        paste('graphCAquanti','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraphQuanti()$Plot)
      },
      contentType='image/jpg')
    
    output$downloadData5 = downloadHandler(
      filename = function() { 
        paste('graphCAquanti','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraphQuanti()$Plot)
      },
      contentType=NA)

    output$CodePrinted <- renderPrint({
       if (input$CAcode!=0){
          cat(values()$codeCA,sep="\n")
          cat(codeGraph()$Code,sep="\n")
          if (!is.null(input$quantisupvar)) cat(codeGraphQuanti()$Code,sep="\n")
       }
    })

    output$CodePrintedDimdesc <- renderPrint({
       if (input$CAcode!=0){
        cat(values()$codeCA,sep="\n")
        cat("dimdesc(res.CA)",sep="\n")
       }
    })

    output$CodePrintedSummary <- renderPrint({
       if (input$CAcode!=0){
        cat(values()$codeCA,sep="\n")
        cat("summary(res.CA)",sep="\n")
       }
    })

    # observe({
      # if(input$CAcode!=0){
        # isolate({
          # cat(values()$codeCA,sep="\n")
          # cat(codeGraph()$Code,sep="\n")
          # if (!is.null(input$quantisupvar)) cat(codeGraphQuanti()$Code,sep="\n")
        # })
      # }
    # })
    
    observe({
      if(input$Quit!=0){
        isolate({
          res <- list()
          res$data <- newdataCAshiny
          res$nomDataCAshiny <- nomDataCAshiny
          res$supvar <- input$supvar
          res$quantisupvar <- input$quantisupvar
          res$rowsupl <- input$rowsupl
          res$color_point <- input$color_point
          res$supquali=input$supquali
          res$nb1=input$nb1
          res$nb2=input$nb2
          invisi=NULL
          if(length(input$invis)!=0){
            invisi=NULL
            if(sum(gettext("Rows",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"row")
            if(sum(gettext("Columns",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"col")
                if(sum(gettext("Supplementary rows",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"row.sup")
            if(sum(gettext("Supplementary columns",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"col.sup")
            if(sum(gettext("Supplementary qualitative variables",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"quali.sup")
          }
          res$invisi=invisi
          res$seleccol=input$seleccol
          res$selecrow=input$selecrow
          res$selec1CAshiny=NULL
          if(input$seleccol=="cos2") res$selec1CAshiny=input$slider3
          if(input$seleccol=="contrib") res$selec1CAshiny=input$contrib1
          res$selec2CAshiny=NULL
          if(input$selecrow=="cos2") res$selec2CAshiny=input$slider4
          if(input$seleccol=="contrib") res$selec2CAshiny=input$contrib2
          res$taille=input$cex
          res$codeCA=values()$codeCA
          res$codeGraph=codeGraph()$Code
          res$codeGraphQuanti=codeGraphQuanti()$Code
          res$title1CAshiny=input$title1CAshiny
          res$title2CAshiny=input$title2CAshiny
          res$anafact=values()$res.CA
          if(is.null(input$colrow)){
            col1CAshiny="blue"
          }else{
            col1CAshiny=input$colrow
          }
          if(is.null(input$colcol)){
            col2CAshiny="red"
          }else{
            col2CAshiny=input$colcol
          }
          if(is.null(input$colrowsup)){
            col3CAshiny="#0C2B94"
          }else{
            col3CAshiny=input$colrowsup
          }
          if(is.null(input$colcolsup)){
            col4CAshiny="darkred"
          }else{
            col4CAshiny=input$colcolsup
          }
          if(is.null(input$colqualisup)){
            col5CAshiny="magenta"
          }else{
            col5CAshiny=input$colqualisup
          }
          res$col1CAshiny=col1CAshiny
          res$col2CAshiny=col2CAshiny
          res$col3CAshiny=col3CAshiny
          res$col4CAshiny=col4CAshiny
	      res$col5CAshiny <- col5CAshiny
          res$ellip=input$ellip
          res$hcpcparam <- input$hcpcparam
          res$nbdimclustCAshiny <- input$nbDimClustering
		  # res$ellipseCA <- input$ellipseCA
          class(res) <- "CAshiny"
          stopApp(returnValue=res)
        })
      }
    })


  }