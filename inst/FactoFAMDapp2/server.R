  function(input, output) {
  
  getactive <- function(){
	  sup=NULL
      if(length(input$supvar)==0 & length(input$supvar1)==0){
        activevar <- allVariables
      } else{
		if (length(input$supvar)>=0) sup <- input$supvar
		if (length(input$supvar1)>=0) sup <- c(sup,input$supvar1)
		sup <- which(allVariables%in%c(input$supvar,input$supvar1))
        activevar <- allVariables[-sup]
      }
      return(list(activevar=activevar,sup=sup))
  }

    # getactive=function(){
      # if(input$selecactive==gettext("Choose")){
      # sup<-sup2<-sup3<-NULL
      # if(length(input$supvar)==0&&length(input$supvar1)==0){
        # activevar<-all
        # supl<-NULL
      # }
      # else if(length(input$supvar1)==0&&length(input$supvar)!=0){
	    # sup<-which(all%in%input$supvar)
        # activevar<-all[-sup]
        # supl<-VariableChoices[sup]
        # quanti<-VariableChoices[-sup]
      # }
      # else if(length(input$supvar)==0&&length(input$supvar1)!=0){
	    # sup=which(all%in%input$supvar1)
        # activevar=all[-sup]
        # supl=QualiChoice[sup]
        # quali=QualiChoice[-sup]
      # }
      # else if(length(input$supvar)!=0&&length(input$supvar1)!=0){
	    # sup=which(all%in%c(input$supvar,input$supvar1))
        # activevar=all[-sup]
        # supl=all[sup]
	    # sup2=which(QualiChoice%in%input$supvar1)
	    # sup3=which(VariableChoices%in%input$supvar)
      # quali=QualiChoice[-sup2]
      # quanti=VariableChoices[-sup3]
      # }
      # return(list(activevar=activevar,supl=supl,quanti=quanti,quali=quali,sup=sup))
    # }
  # }
    
  output$NB1 <- renderUI({
    validate(
      need(length(getactive()$activevar)>1 ,gettext("Please select at least two active variables"))
    )
    if(length(getactive()$activevar)>5){
       return(textInput("nb1", label = NULL, axe1,width='41px'))
    } else{
       return(selectInput("nb1",label=NULL, choices=1:length(getactive()$activevar),selected=axe1,width='41px'))
    }
  })
  
  output$NB2=renderUI({
    validate(
      need(length(getactive()$activevar)>1 ,gettext("Please select at least two active variables"))
    )
    if(length(getactive()$activevar)>5){
       return(textInput("nb2", label = NULL, axe2,width='41px'))
    } else{
       return(selectInput("nb2",label=NULL, choices=1:length(getactive()$activevar),selected=axe2,width='41px'))
    }
  })
  
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
        return(tags$div( 
            div(gettext("Number of dimensions kept for clustering"), style="display: inline-block; padding: 0px 0px 0px 0px"),
		    div(numericInput(inputId = "nbDimClustering", label = NULL,value=if(is.null(nbdimclustFAMDshiny)){5} else {nbdimclustFAMDshiny},min=1), style="display: inline-block;width: 70px; padding: 0px 0px 0px 10px"))
		)
    }
  })

  values=reactive({
    data.selec=newdata
    choixsup=getactive()$sup
    if(length(input$indsup)==0){
      suple=NULL
    } else{
	  suple=which(nom%in%input$indsup)
    }
    # list(res.FAMD=(FAMD(data.selec,sup.var=choixsup,ind.sup=suple,graph=FALSE,ncp=max(5,as.numeric(input$nb1),as.numeric(input$nb2)))),DATA=(data.selec),choixsuple=(suple),varsup=(choixsup))
    codeFAMD <- paste0("res.FAMD<-FAMD(",nomData, if (!identical(newdata,data.selec)){paste0("[,c(",paste0("'",paste(colnames(data.selec),collapse="','"),"'"),")]")})
	codeFAMD <- paste0(codeFAMD,if(max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))!=5) paste0(",ncp=",max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))),if(!is.null(choixsup)) paste0(",sup.var=c(",paste(choixsup,collapse=","),")"),if(!is.null(suple)) paste0(",ind.sup=c(",paste(suple,collapse=","),")"),",graph=FALSE)")
	list(res.FAMD=eval(parse(text=codeFAMD)), codeFAMD=codeFAMD)
    })
    
    codeGraphVar <- reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions")),
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
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
    choix <- gettext("Individuals")
    if(length(input$indsup)>0) choix <- c(choix,gettext("Supplementary individuals"))
    choix <- c(choix,gettext("Active categories"))
    if (length(input$supvar1)>0) choix <- c(choix,gettext("Supplementary categories"))
    div(align="left",checkboxGroupInput("ind_mod",gettext("Points to draw"), choices=choix, selected = indmodFAMDshiny))
  })
    
    codeGraphInd <- reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions")),
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
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

    if(input$select==gettext("Manual")){
      selecindiv <- c(input$indiv)
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
      if(sum(gettext("Active categories")==input$ind_mod)==0) inv<-c(inv,"quali")
      if(sum(gettext("Supplementary categories")==input$ind_mod)==0) inv<-c(inv,"quali.sup")
      if(sum(gettext("Supplementary individuals")==input$ind_mod)==0) inv<-c(inv,"ind.sup")
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
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
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
      paste("'",selecindiv,"'",sep="")
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
          need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
        )
        return(as.data.frame(values()$res.FAMD$quali.sup$coord))
    },rownames=TRUE)
    
    output$sorties13=renderTable({
      validate(
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
      )
      validate(
        need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
      )
      return(as.data.frame(values()$res.FAMD$quali.sup$v.test))
    },rownames=TRUE)
    
    output$sorties2=renderTable({
      validate(
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$var$coord))
    },rownames=TRUE)
    
    output$sorties22=renderTable({
      validate(
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$ind$coord))
    },rownames=TRUE)
    
    output$sorties23=renderTable({
      validate(
        need(length(input$supvar)!=0, gettext("No supplementary quantitative variables")),
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$var$coord.sup))
    },rownames=TRUE)
    
    output$sorties32=renderTable({
      validate(
        need(length(input$supvar)!=0, gettext("No supplementary quantitative variables")),
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$var$cos2.sup))
    },rownames=TRUE)
    
    output$sorties36=renderTable({
      validate(
        need(length(input$indsup)!=0, "No supplementary individuals"),
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$ind.sup$coord))
    },rownames=TRUE)
    
    output$sorties37=renderTable({
      validate(
        need(length(input$indsup)!=0, gettext("No supplementary individuals")),
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$ind.sup$cos2))
    },rownames=TRUE)
    
    
    output$sorties3=renderTable({
      validate(
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$var$contrib))
    },rownames=TRUE)
    
    output$sorties33=renderTable({
      validate(
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$ind$contrib))
    },rownames=TRUE)
    
    output$sorties4=renderTable({
      validate(
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$var$cos2))
    },rownames=TRUE)
    
    output$sorties44=renderTable({
      validate(
        need(length(getactive()$activevar)>1,gettext("Please select at least two active variables"))
      )
      return(as.data.frame(values()$res.FAMD$ind$cos2))
    },rownames=TRUE)
  
  output$sortieDimdesc3=renderTable({
    validate(
      need(length(getactive()$activevar)>2,gettext("Please select at least two active variables"))
    )
    return(as.data.frame(dimdesc(values()$res.FAMD)[[1]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc4=renderTable({
    validate(
      need(length(getactive()$activevar)>2,gettext("Please select at least three active variables"))
    )
    return(as.data.frame(dimdesc(values()$res.FAMD)[[1]]$quali))
  },rownames=TRUE)
  
  #DIM2
  
  output$sortieDimdesc33=renderTable({
    validate(
      need(length(getactive()$activevar)>2,gettext("Please select at least three active variables"))
    )
    return(as.data.frame(dimdesc(values()$res.FAMD)[[2]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc44=renderTable({
    validate(
      need(length(getactive()$activevar)>2,gettext("Please select at least three active variables"))
    )
    return(as.data.frame(dimdesc(values()$res.FAMD)[[2]]$quali))
  },rownames=TRUE)
  
  #DIM3
  
  output$sortieDimdesc333=renderTable({
    validate(
      need(length(getactive()$activevar)>2,gettext("Please select at least three active variables"))
    )
    return(as.data.frame(dimdesc(values()$res.FAMD)[[3]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc444=renderTable({
    validate(
      need(length(getactive()$activevar)>2,gettext("Please select at least three active variables"))
    )
    return(as.data.frame(dimdesc(values()$res.FAMD)[[3]]$quali))
  },rownames=TRUE)
    
    
    output$map3=renderPlot({
    print(ggplot2::ggplot(cbind.data.frame(x=1:nrow(values()$res.FAMD$eig),y=values()$res.FAMD$eig[,2])) + ggplot2::aes(x=x, y=y)+ ggplot2::geom_col(fill="blue") + ggplot2::xlab("Dimension") + ggplot2::ylab(gettext("Percentage of variance")) + ggplot2::ggtitle(gettext("Decomposition of the total inertia")) + ggplot2::theme_light() + ggplot2::theme(plot.title = ggplot2::element_text(hjust =0.5))  + ggplot2::scale_x_continuous(breaks=1:nrow(values()$res.FAMD$eig)))
      # return(barplot(values()$res.FAMD$eig[,1],names.arg=rownames(values()$res.FAMD$eig),las=2))
    })
    
    output$JDD=renderDataTable({
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
        need(length(getactive()$activevar)>1,gettext("Please select at least one supplementary variable"))
      )
      maxvar=length(getactive()$activevar)
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
      need(length(getactive()$activevar)>1,gettext("Please select at least one supplementary variable"))
    )
    maxvar=length(getactive()$quanti)
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
      need(length(getactive()$activevar)>1,gettext("Please select at least one supplementary variable"))
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
  
    observe({
      if(input$FAMDcode!=0){
        isolate({
          if (length(input$habiller)==2 & input$habi==TRUE){
            cat(paste("newCol<-paste(",nomData,"[,'",input$habiller[1],"'],",nomData,"[,'",input$habiller[2],"'],","sep='/')",sep=""),sep="\n")
          }
          cat(values()$codeFAMD,sep="\n")
          cat(codeGraphInd()$Code,sep="\n")
          cat(codeGraphVar()$Code,sep="\n")
          cat(codeGraphQuanti()$Code,sep="\n")
        })
      }
    })

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
      class(res) <- "FAMDshiny"
      stopApp(returnValue=res)
     })
    }
 })
	
}
