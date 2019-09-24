# server scipt for CA2
  function(input, output) {
  
  
  getactive <- function(){
      if(length(input$supvar)==0){
        activevar <- VariableChoicesCAshiny
      } else{
        activevar <- VariableChoicesCAshiny[-which(VariableChoicesCAshiny%in%input$supvar)]
      }
      return(activevar)
  }	

    values=reactive({
      if (length(input$supvar)==0){
        data.selec=newdataCAshiny[,VariableChoicesCAshiny]
      } else{
        data.selec=newdataCAshiny[,c(getactive())]
      }
      validate(
        need(length(getactive()>2), gettext("Please select at least three active columns"))
      )
	  
      if(length(QualiChoiceCAshiny)==0 | length(input$supquali)==0){
        choixquali=NULL
      } else {
          data.selec=cbind(data.selec,newdataCAshiny[,input$supquali,drop=FALSE])
          choixquali=seq((ncol(data.selec)-length(input$supquali)+1),ncol(data.selec))
          colnames(data.selec)[choixquali]=input$supquali
      }

      if(length(input$supvar)==0){
        choixquanti=NULL
      } else {
        data.selec=cbind(data.selec,newdataCAshiny[,input$supvar,drop=FALSE])
        choixquanti=seq((ncol(data.selec)-length(input$supvar)+1),ncol(data.selec))
      }
	  
      if(length(input$rowsupl)!=0){
	    indexes=which(nomCAshiny%in%input$rowsupl)
	    if (length(indexes)==0) indexes=NULL
      } else{
        indexes=NULL
      }
      indexes=c(indexes,rownaCAshiny)
      choixquanti2=choixquanti
      if(length(withnaCAshiny)>0){
        data.selec=cbind(data.selec,newdataCAshiny[,withnaCAshiny,drop=FALSE])
        choixquanti2=c(choixquanti2,seq((ncol(data.selec)-length(withnaCAshiny)+1),ncol(data.selec)))
      }
    codeCA <- paste0("res.CA<-CA(",nomDataCAshiny, if (!identical(newdataCAshiny,data.selec)){paste0("[,c(",paste0("'",paste(colnames(data.selec),collapse="','"),"'"),")]")} )	
	codeCA <- paste0(codeCA,if(max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))!=5) paste0(",ncp=",max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))),if(!is.null(choixquali)) paste0(",quali.sup=c(",paste(choixquali,collapse=","),")"),if(!is.null(choixquanti2)) paste0(",col.sup=c(",paste(choixquanti2,collapse=","),")"),if(!is.null(indexes)) paste0(",row.sup=c(",paste(indexes,collapse=","),")"),",graph=FALSE)")
    list(res.CA=eval(str2expression(codeCA)), codeCA=codeCA)
      # list(res.CA=(CA(data.selec,quali.sup=choixquali,col.sup=choixquanti2,row.sup=indexes,graph=FALSE,ncp=max(5,as.numeric(input$nb1),as.numeric(input$nb2)))),DATA=(data.selec),CHOIXQUALI=(choixquali),CHOIXQUANTI=(choixquanti2),INDEXES=(indexes))
    })
    
  output$NB1 <- renderUI({
    validate(
      need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows")),
      need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns"))
    )
    if((nrow(values()$res.CA$row$coord)>5) & (nrow(values()$res.CA$col$coord)>5)){
       return(textInput("nb1", label = NULL, axe1CAshiny,width='41px'))
    } else{
       return(selectInput("nb1",label=NULL, choices=1:(min(nrow(values()$res.CA$row$coord),nrow(values()$res.CA$col$coord))-1),selected=axe1CAshiny,width='51px'))
    }
  })

  output$NB2 <- renderUI({
    validate(
      need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows")),
      need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns"))
    )
    if((nrow(values()$res.CA$row$coord)>5) & (nrow(values()$res.CA$col$coord)>5)){
       return(textInput("nb2", label = NULL, axe2CAshiny,width='41px'))
    } else{
       return(selectInput("nb2",label=NULL, choices=1:(min(nrow(values()$res.CA$row$coord),nrow(values()$res.CA$col$coord))-1),selected=axe2CAshiny,width='51px'))
    }
  })

    
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
      # fluidRow(
        # tags$head(
          # tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle; }  #inline .form-group { display: table-row;}")
          # ),
        # return(tags$div(id = "inline", numericInput(inputId = "nbDimClustering", label = gettext("Number of dimensions kept for clustering:"),value=nbdimclustCAshiny,min=1)))
      # )
        return(tags$div( 
            div(gettext("Number of dimensions kept for clustering:"), style="display: inline-block; width: 200px; padding: 0px 0px 0px 0px"),
		    div(numericInput(inputId = "nbDimClustering", label = NULL,value=if(is.null(nbdimclustCAshiny)){5} else {nbdimclustCAshiny},min=1), style="display: inline-block;width: 70px; padding: 0px 0px 0px 10px"))
		)
    }
  })
    
    output$col1CAshiny=renderUI({
      if(sum(gettext("Rows")==input$invis)==0){
        return(tags$div( 
            div(colourpicker::colourInput("colrow", label=NULL, if(!is.null(input$colrow)){if (input$colrow!="blue") input$colrow} else{col1CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		    div(gettext("active rows"), style="display: inline-block;padding: 0px 0px 0px 10px"))
		)
        # return(colourpicker::colourInput("colrow",h5(gettext("Colour of row points")),col1CAshiny,allowTransparent=TRUE))
      }
    })
    output$col2CAshiny=renderUI({
      if(sum(gettext("Columns")==input$invis)==0){
        return(tags$div( 
            div(colourpicker::colourInput("colcol", label=NULL, if(!is.null(input$colcol)){if (input$colcol!="red") input$colcol} else{col2CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		    div(gettext("active columns"), style="display: inline-block;padding: 0px 0px 0px 10px"))
		)
        # return(colourpicker::colourInput("colcol",h5(gettext("Colour of column points")),col2CAshiny,allowTransparent=TRUE))
      }
    })
    output$col3CAshiny=renderUI({
      if(!is.null(values()$res.CA$row.sup) & sum(gettext("Supplementary rows")==input$invis)==0){
        return(tags$div( 
            div(colourpicker::colourInput("colrowsup", label=NULL, if(!is.null(input$colrowsup)){if (input$colrowsup!="darkblue") input$colrowsup} else{col3CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		    div(gettext("supplementary rows"), style="display: inline-block;padding: 0px 0px 0px 10px"))
		)
        # return(colourpicker::colourInput("colrowsup",h5(gettext("Colour of supplementary row points")),col3CAshiny,allowTransparent=TRUE))
      }
    })
    output$col4CAshiny=renderUI({
      if(!is.null(values()$res.CA$col.sup) & sum(gettext("Supplementary columns")==input$invis)==0){
        return(tags$div( 
            div(colourpicker::colourInput("colcolsup", label=NULL, if(!is.null(input$colcolsup)){if (input$colcolsup!="darkred") input$colcolsup} else{col4CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		    div(gettext("supplementary columns"), style="display: inline-block;padding: 0px 0px 0px 10px"))
		)
        # return(colourpicker::colourInput("colcolsup",h5(gettext("Colour of supplementary column points")),col4CAshiny,allowTransparent=TRUE))
      }
    })
    
    output$col5CAshiny=renderUI({
      if(!is.null(values()$res.CA$quali.sup) & sum(gettext("Supplementary categories")==input$invis)==0){
        return(tags$div( 
            div(colourpicker::colourInput("colqualisup", label=NULL, if(!is.null(input$colqualisup)){if (input$colqualisup!="magenta") input$colqualisup} else{col5CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		    div(gettext("supplementary categories"), style="display: inline-block;padding: 0px 0px 0px 10px"))
		)
      }
    })

    output$ellipsesCAshiny=renderUI({
      values1=c()
      if(sum(gettext("Rows")==input$invis)==0){
        values1=c(values1,gettext("Rows"))
      }
      if(sum(gettext("Columns")==input$invis)==0){
        values1=c(values1,gettext("Columns"))
      }
      if(length(values)!=0){
          return(checkboxGroupInput("ellip",gettext("Draw confidence ellipses around"),choices=values1,selected=ellipsesCAshiny,inline=TRUE))
      }
    })
    
    
    codeGraph=reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions")),
        need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows")),
        need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns"))
      )
      if(length(input$invis)==0){
        invisiText <- NULL
      } else {
        invisi <- NULL
        if(sum(gettext("Rows")==input$invis)>0) invisi<-c(invisi,"row")
        if(sum(gettext("Columns")==input$invis)>0) invisi<-c(invisi,"col")
        if(sum(gettext("Supplementary rows")==input$invis)>0) invisi<-c(invisi,"row.sup")
        if(sum(gettext("Supplementary columns")==input$invis)>0) invisi<-c(invisi,"col.sup")
        if(sum(gettext("Supplementary qualitative variables")==input$invis)>0) invisi<-c(invisi,"quali.sup")
        if(sum(gettext("Supplementary quantitative variables")==input$invis)>0) invisi<-c(invisi,"quanti.sup")
        invisiText <- paste0("c(",paste(paste("'",invisi,"'",sep=""),collapse = ","),")")
      }
      sel=NULL
      if(input$seleccol=="cos2"){
        if(input$slider3!=1){
          sel=paste0("'cos2 ",input$slider3,"'")
        }
        else{
          sel="'cos2 0.9999999'"
        }
      }
      if(input$seleccol=="contrib"){
        sel=paste0("'contrib ",input$contrib1,"'")
      }
      sel2=NULL
      if(input$selecrow=="cos2"){
        if(input$slider4!=1){
          sel2=paste0("'cos2 ",input$slider4,"'")
        }
        else{
          sel2="'cos2 0.9999999'"
        }
      }
      if(input$selecrow=="contrib"){
        sel2=paste0("'contrib ",input$contrib2,"'")
      }
      values2=c()
      if(!is.null(input$ellip)){
        if(gettext("Columns")%in%input$ellip) values2=c(values2,"col")
        if(gettext("Rows")%in%input$ellip) values2=c(values2,"row")
      }

    hab <- "none"
    if(input$color_point == gettext("quantitative variable")) hab <- paste0("'",input$habiller,"'")
    if(input$color_point == "cos2") hab <- "'cos2'"
    if(input$color_point == "contribution") hab <- "'contrib'"
    if(input$color_point==gettext("qualitative variable")) hab <- which(colnames(values()$res.PCA$call$X)==input$habiller)

      if(is.null(input$ellip)||length(input$ellip)==0){
	    myellip=NULL
      }else{
        vect=c()
        if(gettext("Columns")%in%input$ellip) vect=c(vect,"col")
        if(gettext("Rows")%in%input$ellip) vect=c(vect,"row")
        myellip=paste(paste("'",vect,"'",sep=""),collapse=",")
      }
      codeGraph=paste0(if(is.null(myellip)){"plot.CA"}else{"ellipseCA"},"(res.CA",if(!is.null(myellip)) paste0(",ellipse=c(",myellip,")"),if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (!is.null(sel)) paste0(",selectCol=",sel),if (!is.null(sel2)) paste0(",selectRow=",sel2),if (!is.null(sel2) | !is.null(sel)) paste0(',unselect=0'),if (input$cex!=1) paste0(',cex=',input$cex,',cex.main=',input$cex,',cex.axis=',input$cex),if(input$title1CAshiny!="CA factor map")paste0(',title="',input$title1CAshiny,'"'),if (hab!="none" & hab!="''"){paste0(",habillage=",hab)},if (!is.null(input$colrow)) paste0(",col.row='",input$colrow,"'"),if (!is.null(input$colcol)) paste0(",col.col='",input$colcol,"'"),if (!is.null(input$colrowsup)) paste0(",col.row.sup='",input$colrowsup,"'"),if (!is.null(input$colcolsup)) paste0(",col.col.sup='",input$colcolsup,"'"),if (!is.null(input$colqualisup)) paste0(",col.quali.sup='",input$colqualisup,"'"),if (!is.null(invisiText)) paste0(',invisible=',invisiText),')')
      return(codeGraph)
    })
    
    output$map <- renderPlot({
	    res.CA <- values()$res.CA
        p <- print(eval(str2expression(codeGraph())))
    })
    
    
    output$contribcol=renderUI({
      maxx=nrow(values()$res.CA$col$coord)
      if(selec1CAshiny=="contrib"){
        return(sliderInput("contrib1",h6(gettext("Number of the most contributive active columns")),min=1,max=maxx,value=valueselec2CAshiny,step=1))
      } else{
        return(sliderInput("contrib1",h6(gettext("Number of the most contributive active columns")),min=1,max=maxx,value=maxx,step=1))
      }
      
    })
    
    output$contribrow=renderUI({
      maxx=nrow(values()$res.CA$row$coord)
      if(selec2CAshiny=="contrib"){
        return(sliderInput("contrib2",h6(gettext("Number of the most contributive active rows")),min=1,max=maxx,value=valueselec2CAshiny,step=1))
      }
      else{
        return(sliderInput("contrib2",h6(gettext("Number of the most contributive active rows")),min=1,max=maxx,value=maxx,step=1))
      }
    })
    
    output$out22=renderUI({
      choix=list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results for the columns"),gettext("Results for the rows"))
      if(!is.null(values()$INDEXES)){
        choix=c(choix,gettext("Results for the supplementary rows"))
      }
      if(!is.null(values()$CHOIXQUANTI)){
        choix=c(choix,gettext("Results for the supplementary columns"))
      }
      if(!is.null(values()$CHOIXQUALI)){
        choix=c(choix,gettext("Results for the categorical variables"))
      }
      radioButtons("out",gettext("Which outputs do you want?"),
                   choices=choix,selected=gettext("Summary of outputs"),inline=TRUE)
    })
    
    output$warn=renderPrint({
      if(length(withnaCAshiny)!=0){
        baba=paste(withnaCAshiny,collapse=", ")
        bibi=paste(nomrowCAshiny,collapse=", ")
        a=paste0(gettext("Warning: "), baba, gettext(" have NA : they are considered as supplementary columns"))
        b=paste0(gettext("Warning: "), bibi, gettext(" have NA : they are considered as supplementary rows"))
        return(cat(a,b,sep="\n"))
      }
    })
    
    
    output$sorties=renderTable({
      return(as.data.frame(values()$res.CA$eig))
    },rownames=TRUE)
    
    output$sorties1=renderTable({
      validate(
      need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns"))
      )
      return(as.data.frame(values()$res.CA$col$coord))
    },rownames=TRUE)
    
    output$sorties2=renderTable({
      validate(
      need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns"))
      )
      return(as.data.frame(values()$res.CA$col$cos2))
    },rownames=TRUE)
    
    output$sorties3=renderTable({
      validate(
      need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns"))
      )
      return(as.data.frame(values()$res.CA$col$contrib))
    },rownames=TRUE)
    
    output$sorties4=renderTable({
      validate(
        need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows"))
	  )
      return(as.data.frame(values()$res.CA$row$coord))
    },rownames=TRUE)
    
    output$sorties5=renderTable({
      validate(
        need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows"))
	  )
      return(as.data.frame(values()$res.CA$row$cos2))
    },rownames=TRUE)
    
    output$sorties6=renderTable({
      validate(
        need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows"))
	  )
      return(as.data.frame(values()$res.CA$row$contrib))
    },rownames=TRUE)
    
    output$sorties7=renderTable({
      validate(
        need((length(input$rowsupl)>0), gettext("No supplementary rows selected"))
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
        need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
      )
      return(as.data.frame(values()$res.CA$quali.sup))
    },rownames=TRUE)
    
    
    output$map3=renderPlot({
      return(barplot(values()$res.CA$eig[,1],names.arg=rownames(values()$res.CA$eig),las=2))
    })
    
    ### Fonction permettant l'affichage du JDD sous la forme d'un DataTable, qui permet la recherche de donnes. 
    output$JDD=renderDataTable({
      cbind(Names=rownames(newdataCAshiny),newdataCAshiny)},
      options = list(    "orderClasses" = TRUE,
                         "responsive" = TRUE,
                         "pageLength" = 10))
    
    ### Fonction permettant l'affichage du summary du JDD
    output$summary=renderPrint({
      summary(newdataCAshiny)
    })
    
    ### Fonction permettant l'affichage du summary de la fonction CA sur le JDD
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
    
    output$downloadInvestigateRmd <- downloadHandler(
     filename = function() {
      paste(input$titleFile, ".Rmd", sep="")
    },
    content = function(file) {
        path.aux <- getwd()
        setwd(pathsaveCAshiny)
        FactoInvestigate::Investigate(values()$res.CA, codeGraph = if (input$choixGRAPH==gettext("Graph done")) {paste0(values()$codeCA,"\n",codeGraph())} else {NULL}, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= substr(tolower(input$choixLANG),1,2))
	    # if (gettext(input$choixLANG)==gettext("English")) FactoInvestigate::Investigate(values()$res.CA, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = "Investigate", display.HCPC =input$hcpcparam, language="en")
	    # if (gettext(input$choixLANG)==gettext("French")) FactoInvestigate::Investigate(values()$res.CA, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = "Investigate", display.HCPC =input$hcpcparam, language="fr")
        file.rename(paste0(pathsaveCAshiny,"/Investigate.Rmd"), file)
        setwd(path.aux)
    }
  )

  observe({
    if(input$Investigatehtml!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveCAshiny)
        FactoInvestigate::Investigate(values()$res.CA, codeGraph = if (input$choixGRAPH==gettext("Graph done")) {paste0(values()$codeCA,"\n",codeGraph())} else {NULL}, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= substr(tolower(input$choixLANG),1,2))
        # if (gettext(input$choixLANG)==gettext("English")) FactoInvestigate::Investigate(values()$res.CA, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="en")
        # if (gettext(input$choixLANG)==gettext("French")) FactoInvestigate::Investigate(values()$res.CA, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="fr")
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$Investigatedoc!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveCAshiny)
        FactoInvestigate::Investigate(values()$res.CA, codeGraph = if (input$choixGRAPH==gettext("Graph done")) {paste0(values()$codeCA,"\n",codeGraph())} else {NULL},document="word_document", openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= substr(tolower(input$choixLANG),1,2))
        # if (gettext(input$choixLANG)==gettext("English")) FactoInvestigate::Investigate(values()$res.CA,document="word_document",openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="en")
        # if (gettext(input$choixLANG)==gettext("French")) FactoInvestigate::Investigate(values()$res.CA,document="word_document",openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="fr")
        setwd(path.aux)
      })
    }
  })
  

    ## Creation des fonctions permettant l'enregistrement des graphs sous les formats : png, jpeg, pdf et emf
    output$downloadData = downloadHandler(
      filename = function() { 
        paste('graph1','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(eval(str2expression(codeGraphVar()))))
      },
      contentType='image/png')
    
    output$downloadData1 = downloadHandler(
      filename = function() { 
        paste('graph1','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(eval(str2expression(codeGraphVar()))))
      },
      contentType='image/jpg')
    
    output$downloadData2 = downloadHandler(
      filename = function() { 
        paste('graph1','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(eval(str2expression(codeGraphVar()))))
      },
      contentType=NA)
    
    observe({
      if(input$CAcode!=0){
        isolate({
          cat(values()$codeCA,sep="\n")
          cat(codeGraph(),sep="\n")
        })
      }
    })
    
    observe({
      if(input$Quit!=0){
        isolate({
          res=list()
          res$data=newdataCAshiny
          res$nomDataCAshiny=nomDataCAshiny
          # a : colonnes supplementaires
          res$a=input$supvar
          # b : lignes supplementaires
          res$b=input$rowsupl
          # c : colonnes quali
          choixquali=NULL
          if (length(QualiChoiceCAshiny)==1){
            if(input$supquali==TRUE){
              choixquali=QualiChoiceCAshiny
            }
          }
          else{
            if(length(input$supquali)!=0){
              choixquali=input$supquali
            }
          }
          res$c=choixquali
          # d et e : axes
          res$d=input$nb1
          res$e=input$nb2
          # f : invisible points 
          invisi=NULL
          if(length(input$invis)!=0){
            invisi=NULL
            if(sum(gettext("Rows")==input$invis)>0) invisi<-c(invisi,"row")
            if(sum(gettext("Columns")==input$invis)>0) invisi<-c(invisi,"col")
                if(sum(gettext("Supplementary rows")==input$invis)>0) invisi<-c(invisi,"row.sup")
            if(sum(gettext("Supplementary columns")==input$invis)>0) invisi<-c(invisi,"col.sup")
            if(sum(gettext("Supplementary qualitative variables")==input$invis)>0) invisi<-c(invisi,"quali.sup")
          }
          res$f=invisi
          res$type1=input$seleccol
          res$type2=input$selecrow
          res$selec1CAshiny=NULL
          if(input$seleccol=="cos2") res$selec1CAshiny=input$slider3
          if(input$seleccol=="contrib") res$selec1CAshiny=input$contrib1
          res$selec2CAshiny=NULL
          if(input$selecrow=="cos2") res$selec2CAshiny=input$slider4
          if(input$seleccol=="contrib") res$selec2CAshiny=input$contrib2
          res$taille=input$cex
          res$code1=values()$codeCA
          res$code2=codeGraph()
          res$title1CAshiny=input$title1CAshiny
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
            col4CAshiny="magenta"
          }else{
            col4CAshiny=input$colqualisup
          }
          res$col1CAshiny=col1CAshiny
          res$col2CAshiny=col2CAshiny
          res$col3CAshiny=col3CAshiny
          res$col4CAshiny=col4CAshiny
	      res$col5CAshiny <- col5CAshiny
          res$ellip=input$ellip
          res$hcpcparam <- input$hcpcparam
          res$nbdimclustCAshiny <- input$nbDimClustering
          class(res) <- "CAshiny"
          stopApp(returnValue=res)
        })
      }
    })


  }