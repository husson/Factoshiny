  function(input, output) {
    
  output$NB1 <- renderUI({
    return(textInput("nb1", label = NULL, axe1,width='41px'))
  })
  
  output$NB2=renderUI({
    return(textInput("nb2", label = NULL, axe2,width='41px'))
  })
  
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
        return(tags$div( 
            div(gettext("Number of dimensions kept for clustering",domain="R-Factoshiny"), style="display: inline-block; padding: 0px 0px 0px 0px"),
		    div(numericInput(inputId = "nbDimClustering", label = NULL,value=if(is.null(nbdimclustMFAshiny)){5} else {nbdimclustMFAshiny},min=1), style="display: inline-block;width: 70px; padding: 0px 0px 0px 10px"))
		)
    }
  })

  output$choixindvar=renderUI({
    choix=gettext("Individuals",domain="R-Factoshiny")
    if(!(is.null(anafact[["quali.var"]]))) choix <- c(choix,gettext("Categories",domain="R-Factoshiny"))
    if(!(is.null(anafact$ind.sup))) choix <- c(choix,gettext("Supplementary individuals",domain="R-Factoshiny"))
    if(!(is.null(anafact$quali.var.sup))) choix <- c(choix,gettext("Supplementary categories",domain="R-Factoshiny"))
    div(align="left",checkboxGroupInput("ind_var",gettext("Points to draw",domain="R-Factoshiny"), choices=choix, selected = indvarMFAshiny))
  })

  output$choixgraphic=renderUI({
    choix=gettext("Individuals",domain="R-Factoshiny")
    if(!(is.null(anafact$quanti.var))) choix <- c(choix,gettext("Quantitative variables",domain="R-Factoshiny"))
    choix <- c(choix,gettext("Groups",domain="R-Factoshiny"),gettext("Partial axes",domain="R-Factoshiny"))
    if(!(is.null(anafact$freq))) choix <- c(choix,gettext("Frequencies",domain="R-Factoshiny"))
    div(align="center",selectInput("choixgraph",gettext("Which graph would you like to modify?",domain="R-Factoshiny"), choices=choix,selected=gettext("Individuals",domain="R-Factoshiny")))
})
    
  output$drawindiv=renderUI({
    if(input$choixpartial==gettext("None",domain="R-Factoshiny")){
      if (is.null(anafact$quali.var) & is.null(anafact$quali.var.sup)) return(radioButtons("drawind",gettext("Drawing by",domain="R-Factoshiny"),choices=list(gettext("No selection",domain="R-Factoshiny"),gettext("individual",domain="R-Factoshiny")),selected=gettext("No selection",domain="R-Factoshiny"),inline=TRUE))
      else return(radioButtons("drawind",gettext("Drawing by",domain="R-Factoshiny"),choices=list(gettext("No selection",domain="R-Factoshiny"),gettext("individual",domain="R-Factoshiny"),gettext("categorical variable",domain="R-Factoshiny")),selected=gettext("No selection",domain="R-Factoshiny"),inline=TRUE))
    } else{
      if (is.null(anafact$quali.var) & is.null(anafact$quali.var.sup)) return(radioButtons("drawind",gettext("Drawing by",domain="R-Factoshiny"),choices=list(gettext("group",domain="R-Factoshiny"),gettext("individual",domain="R-Factoshiny")),selected=drawing,inline=TRUE))
      else return(radioButtons("drawind",gettext("Drawing by",domain="R-Factoshiny"),choices=list(gettext("group",domain="R-Factoshiny"),gettext("individual",domain="R-Factoshiny"),gettext("categorical variable",domain="R-Factoshiny")),selected=drawing,inline=TRUE))
    }
  })
    
  output$habillagequali=renderUI({
      if(!(is.null(anafact$quali.var))){
        return(selectInput("habiquali"," ",choices=quali))
      } else{
      p(gettext("No groups of categorical variable",domain="R-Factoshiny"))
    }
  })
    
    output$indivpartiel2=renderUI({
      if(is.null(partial2)){
        return(selectInput("indivpartiel",label=gettext("Select individuals",domain="R-Factoshiny"),
                           choices=rownames(data),multiple=TRUE))
      }
      else{
        return(selectInput("indivpartiel",label=gettext("Select individuals",domain="R-Factoshiny"),
                           choices=rownames(data),multiple=TRUE,selected=partial2))
      }
    })
    
    output$slider1=renderUI({
      if(inherits(x,"MFA")){
        maxlength=dim(anafact$quanti.var$coord)[1]
        if(input$selection=="contrib"){
          return(sliderInput("slider2",gettext("Number of the most contributive variables",domain="R-Factoshiny"),min=1, max=maxlength, value=maxlength, step=1))
        }
        if(input$selection=="cos2"){
          return(sliderInput("slider3",gettext("Variables with cos2 highest than",domain="R-Factoshiny"),min=0, max=1, value=0, step=0.01))
        }
      }
      if(inherits(x,"MFAshiny")){
        maxlength=dim(anafact$quanti.var$coord)[1]
        if(input$selection=="contrib"){
            return(sliderInput("slider2",gettext("Number of the most contributive variables",domain="R-Factoshiny"),min=1, max=maxlength, value=if (is.null(selectvar2)){maxlength} else {selectvar2}, step=1))  
        }
        if(input$selection=="cos2"){
            return(sliderInput("slider3",gettext("Variables with cos2 highest than",domain="R-Factoshiny"),min=0, max=1, value=if (is.null(selectvar2)){0} else {selectvar2}, step=0.01))  
        }  
      }
    })
    
    output$hide2=renderUI({
      if(!(is.null(anafact$quanti.var.sup))){
        if(!is.null(hide)){
          return(radioButtons("hides",gettext("Hide:",domain="R-Factoshiny"),choices=list(gettext("Nothing",domain="R-Factoshiny"),gettext("Active variables",domain="R-Factoshiny"),gettext("Supplementary variables",domain="R-Factoshiny")),selected=hide))
        } else{
          return(radioButtons("hides",gettext("Hide:",domain="R-Factoshiny"),choices=list(gettext("Nothing",domain="R-Factoshiny"),gettext("Active variables",domain="R-Factoshiny"),gettext("Supplementary variables",domain="R-Factoshiny")),selected=gettext("Nothing",domain="R-Factoshiny")))
        }
      }
    })
    
    CodeGraphInd <- function(){
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny")),
        need(input$nb1 <= ncol(anafact$ind$coord), paste(gettext("The number of dimensions must be less than",domain="R-Factoshiny"),ncol(anafact$ind$coord))),
        need(input$nb2 <= ncol(anafact$ind$coord), paste(gettext("The number of dimensions must be less than",domain="R-Factoshiny"),ncol(anafact$ind$coord)))
      )      
    if(input$select=="cos2"){
      if(input$sliderind1!=1){
        selecindiv <- paste("cos2 ",input$sliderind1)
      }
      else{
        selecindiv <- "cos2 0.999999"
      }
      selecindivtext <- paste0("'",selecindiv,"'")
    }
    if(input$select==gettext("No selection",domain="R-Factoshiny")){
      selecindiv <- NULL
      selecindivtext <- "NULL"
    }
    if(input$select=="contrib"){
      selecindiv <- paste("contrib ",input$sliderind0)
      selecindivtext <- paste0("'",selecindiv,"'")
    }

    if(input$select==gettext("Manual",domain="R-Factoshiny")){
      selecindiv <- c(input$indiv)
      if(length(input$indiv)==0) selecindivtext <- "NULL"
      if(length(input$indiv)>1){
        vec<- paste("'",paste(selecindiv,collapse="','"),"'",sep="")
        selecindivtext<-paste("c(",vec,")",sep="")
      }
      else if (length(input$indiv)==1){
        selecindivtext <- paste0("'",c(input$indiv),"'")
      }
    }

      part=NULL
      if(input$choixpartial==gettext("All",domain="R-Factoshiny")) part="'all'"
      if(input$choixpartial==gettext("Choose",domain="R-Factoshiny") & !is.null(input$indivpartiel)) part=paste0("c('",paste0(input$indivpartiel,collapse="','"),"')")
      lapbar=TRUE
      if(input$choixpartial!=gettext("None",domain="R-Factoshiny") && input$partind==FALSE) lapbar=FALSE
      habi="none"
      if(!(is.null(input$drawind))){
        if(input$drawind==gettext("individual",domain="R-Factoshiny")) habi="ind"
        if((input$choixpartial==gettext("All",domain="R-Factoshiny") || input$choixpartial==gettext("Choose",domain="R-Factoshiny")) && input$drawind==gettext("group",domain="R-Factoshiny")) habi="group"
        if(input$drawind==gettext("categorical variable",domain="R-Factoshiny")) habi=input$habiquali
      }
	  inv <- c()
      if(sum(gettext("Individuals",domain="R-Factoshiny")==input$ind_var)==0)  inv<-c(inv,"'ind'")
      if(!(is.null(anafact[["quali.var"]])) & sum(gettext("Categories",domain="R-Factoshiny")==input$ind_var)==0) inv<-c(inv,"'quali'")
      if(!(is.null(anafact$quali.var.sup)) & sum(gettext("Supplementary categories",domain="R-Factoshiny")==input$ind_var)==0) inv<-c(inv,"'quali.sup'")
      if(!(is.null(anafact$ind.sup)) & sum(gettext("Supplementary individuals",domain="R-Factoshiny")==input$ind_var)==0) inv<-c(inv,"'ind.sup'")
      if(length(inv)>1) vecinv<-paste0("c(",paste0(inv,collapse=","),")")
      if(length(inv)==1) vecinv <- inv
      if(length(inv)==0 | length(input$ind_var)==0) vecinv<-"NULL"
      Code <- paste0('plot.MFA(',nomObjectMFA,', choix="ind"',if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if(!is.null(part)) paste0(",partial=",part),if(!is.null(input$partind)) {if (input$partind==TRUE) ",lab.par=TRUE"},if (vecinv!="NULL") paste(",invisible=",vecinv), if(selecindivtext!="NULL"){paste0(",select=",selecindivtext)}, if (habi!="none" & habi!="''"){paste0(",habillage='",habi,"'")},if(input$titleInd!="MFA graph of individuals")paste0(',title="',input$titleInd,'"'),if(input$cexInd!=1)paste0(",cex=",input$cexInd,",cex.main=",input$cexInd,",cex.axis=",input$cexInd),")")
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))      
    }
    
    output$map <- renderPlot({
      p <- print(CodeGraphInd()$Plot)
    })
    
    CodeGraphVar=function(){
      if (is.null(anafact$quanti.var) & is.null(anafact$quanti.sup)) return(NULL)
	  habi <- NULL
	  if(input$colorgroup==TRUE) habi="'group'"
      if(input$selection==gettext("No selection",domain="R-Factoshiny")) selec=NULL
      if(input$selection=="contrib") selec=paste0("'contrib ",input$slider2,"'")
      if(input$selection=="cos2"){
        if (is.null(input$slider3)) {
          selec <- "'cos2 0'"
		} else {
		  if (input$slider3==1) selec <- "'cos2 0.999'"
          if(input$slider3!=1) selec=paste0("'cos2 ",input$slider3,"'")
		}
      }

      if(is.null(input$hides)){
        invi="none"
	  }else{
	    if (input$hides==gettext("Nothing",domain="R-Factoshiny")) invi="none"
	    if (input$hides==gettext("Active variables",domain="R-Factoshiny")) invi="quanti"
	    if (input$hides==gettext("Supplementary variables",domain="R-Factoshiny")) invi="quanti.sup"
      }
      Code <- paste0('plot.MFA(',nomObjectMFA,', choix="var"',if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (!is.null(selec)) paste0(",select=",selec),if(invi!="none"){paste0(",invisible=c(",paste0("'",paste(invi,collapse="','"),"'"),")")}, if (!is.null(habi)){paste0(",habillage=",habi)},if(input$titleVar!="Graph of quantitative variables")paste0(',title="',input$titleVar,'"'),if(input$cexVar!=1)paste0(",cex=",input$cexVar,",cex.main=",input$cexVar,",cex.axis=",input$cexVar),")")
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))      
    }
    
    output$map2 <- renderPlot({
      p=print(CodeGraphVar()$Plot)
    })
    
    output$map22=renderUI({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny"))
      )
      
      if(is.null(anafact$quanti.var)){
        return(p())
      } else{
        column(width = 6,shinyjqui::jqui_resizable(plotOutput("map2", height="500")),
           br(),
           p(gettext("Download as"),downloadButton("downloadData4",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData3",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData5",gettext("pdf",domain="R-Factoshiny")),align="center")
		)
	  }
    })
    
    CodeGraphGroup <- function(){
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny"))
      )
      Code <- paste0('plot.MFA(',nomObjectMFA,', choix="group"',if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"), if(input$titleGroup!="Groups representation")paste0(',title="',input$titleGroup,'"'),if(input$cexGroup!=1)paste0(",cex=",input$cexGroup,",cex.main=",input$cexGroup,",cex.axis=",input$cexGroup),")")
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))      
    }
    
    output$map5 <- renderPlot({
      p <- print(CodeGraphGroup()$Plot)
    })
    
    CodeGraphPartial <- function(){
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny"))
      )
      habi="'group'"
      Code <- paste0('plot.MFA(',nomObjectMFA,', choix="axes"',if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"), if(input$titlePartial!="Graph of the partial axes")paste0(',title="',input$titlePartial,'"'),if (!is.null(habi)){paste0(",habillage=",habi)},if(input$cexPartial!=1)paste0(",cex=",input$cexPartial,",cex.main=",input$cexPartial,",cex.axis=",input$cexPartial),")")
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))      
    }
    
    output$map4 <- renderPlot({
      p <- print(CodeGraphPartial()$Plot)
    })

  output$choixindvarfreq=renderUI({
    choix <- indvarMFAshinyfreq
      if(is.null(anafact$ind)) choix <- setdiff(choix,gettext("Individuals",domain="R-Factoshiny"))
      if(is.null(anafact$ind.sup)) choix <- setdiff(choix,gettext("Supplementary individuals",domain="R-Factoshiny"))
      if(is.null(anafact[["quali.var"]])) choix <- setdiff(choix,gettext("Categories",domain="R-Factoshiny"))
      if(is.null(anafact$quali.var.sup)) choix <- setdiff(choix,gettext("Supplementary categories",domain="R-Factoshiny"))
      if(is.null(anafact[["freq"]])) choix <- setdiff(choix,gettext("Frequencies",domain="R-Factoshiny"))
      if(is.null(anafact$freq.sup)) choix <- setdiff(choix,gettext("Supplementary frequencies",domain="R-Factoshiny"))
    div(align="left",checkboxGroupInput("ind_varfreq",gettext("Points to draw",domain="R-Factoshiny"), choices=choix, selected = indvarMFAshinyfreq))
  })

    CodeGraphFreq <- function(){
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny"))
      )
	  if (is.null(anafact$freq)) return(NULL)
	  habi="'group'"
# invisible = row, row.sup, col ou col.sup

	  inv <- c()
      if(sum(gettext("Individuals")==input$ind_varfreq)==0)  inv<-c(inv,"'row'")
      if(length(QualiChoice)>0 & sum(gettext("Categories",domain="R-Factoshiny")==input$ind_varfreq)==0) inv<-c(inv,"'quali'")
      if(length(input$indsup)>0 & sum(gettext("Supplementary individuals",domain="R-Factoshiny")==input$ind_varfreq)==0) inv<-c(inv,"'row.sup'")
      if(length(QualiChoice)>0 & sum(gettext("Supplementary categories",domain="R-Factoshiny")==input$ind_varfreq)==0) inv<-c(inv,"'quali.sup'")
      if(sum(gettext("Frequencies",domain="R-Factoshiny")==input$ind_varfreq)==0) inv<-c(inv,"'col'")
      if(sum(gettext("Supplementary frequencies",domain="R-Factoshiny")==input$ind_varfreq)==0) inv<-c(inv,"'col.sup'")
      if(length(inv)>1) vecinv<-paste0("c(",paste0(inv,collapse=","),")")
      if(length(inv)==1) vecinv <- inv
      if(length(inv)==0 | length(input$ind_varfreq)==0) vecinv<-"NULL"

      Code <- paste0('plot.MFA(',nomObjectMFA,', choix="freq"',if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (!is.null(habi)){paste0(",habillage=",habi)},if (vecinv!="NULL") paste(",invisible=",vecinv), if (length(input$affichcol)>0){ if (input$affichcol==FALSE) ",lab.col=FALSE"},if (length(input$affichind)>0){if (input$affichind==FALSE) paste0(",lab.ind=FALSE")}, if(input$titleFreq!="Graph of the frequencies")paste0(',title="',input$titleFreq,'"'),if(input$cexFreq!=1)paste0(",cex=",input$cexFreq,",cex.main=",input$cexFreq,",cex.axis=",input$cexFreq),")")
      # Code <- paste0('plot.MFA(',nomObjectMFA,', choix="freq"',if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (!is.null(habi)){paste0(",habillage=",habi)}, if (col==TRUE) ",lab.col=col",if(input$titleFreq!="Graph of the frequencies")paste0(',title="',input$titleFreq,'"'),if(input$cexFreq!=1)paste0(",cex=",input$cexFreq,",cex.main=",input$cexFreq,",cex.axis=",input$cexFreq),")")
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))
    }
    
    output$map6 <- renderPlot({
      p <- print(CodeGraphFreq()$Plot)
    })
    
    output$map66=renderUI({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny"))
      )
      if(is.null(anafact$freq)){
        return(p())
      } else{
        column(width = 6,shinyjqui::jqui_resizable(plotOutput("map6", height="500")),
           br(),
           p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData19",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData20",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData21",gettext("pdf",domain="R-Factoshiny")),align="center")
		)
	  }
    })

    

  output$sorties=renderTable({
      return(as.data.frame(anafact$eig))
    },rownames=TRUE)
    
    output$map3=renderPlot({
      print(ggplot2::ggplot(cbind.data.frame(x=1:nrow(anafact$eig),y=anafact$eig[,2])) + ggplot2::aes(x=x, y=y)+ ggplot2::geom_col(fill="blue") + ggplot2::xlab("Dimension") + ggplot2::ylab(gettext("Percentage of variance",domain="R-Factoshiny")) + ggplot2::ggtitle(gettext("Decomposition of the total inertia",domain="R-Factoshiny")) + ggplot2::theme_light() + ggplot2::theme(plot.title = ggplot2::element_text(hjust =0.5))  + ggplot2::scale_x_continuous(breaks=1:nrow(anafact$eig)))
      # return(barplot(anafact$eig[,1],names.arg=rownames(anafact$eig),las=2))
    })
    output$JDD=DT::renderDataTable({
      tab=cbind(Names=rownames(anafact$global.pca$call$X),anafact$global.pca$call$X)
      quanti=names(which(sapply(tab,is.numeric)))
      tab[quanti]=round(tab[quanti],5)
      tab
      },
      options = list( "orderClasses" = TRUE, "responsive" = TRUE, "pageLength" = 10))
    
    output$summary=renderPrint({
      summary(anafact$global.pca$call$X)
    })
    
    output$summaryMFA=renderPrint({
      validate(
        need(input$nbele!=0, gettext("Please select at least one element"))
      )
      summary.MFA(anafact,nbelements=input$nbele)
    })  
    
      
    # output$histo=renderPlot({
      # par(mfrow=c(1,2))
      # boxplot(x[,input$bam])
      # plot(density(x[,input$bam]),main="",xlab="")
    # })
    
    
    output$downloadData = downloadHandler(
      filename = function() { 
        paste('GraphInd','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphInd()$Plot))
      },
      contentType='image/png')
    
    output$downloadData3 = downloadHandler(
      filename = function() { 
        paste('GraphVar','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphVar()$Plot))
      },
      contentType='image/png')
    
    output$download3=renderUI({
      if(is.null(anafact$quanti.var)){
        return()
      }
      else{
        return(downloadButton("downloadData3",gettext("Download as png",domain="R-Factoshiny")))
      }
    })
    
    output$downloadData11 = downloadHandler(
      filename = function() { 
        paste('GraphGroup','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphGroup()$Plot))
      },
      contentType='image/jpg')
    
    output$downloadData12 = downloadHandler(
      filename = function() { 
        paste('GraphGroup','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphGroup()$Plot))
      },
      contentType='image/png')
    
    output$downloadData13 = downloadHandler(
      filename = function() { 
        paste('GraphGroup','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphGroup()$Plot))
      },
      contentType=NA)
    
    
    output$downloadData15 = downloadHandler(
      filename = function() { 
        paste('GraphPartial','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphPartial()$Plot))
      },
      contentType='image/jpg')
    
    output$downloadData16 = downloadHandler(
      filename = function() { 
        paste('GraphPartial','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphPartial()$Plot))
      },
      contentType='image/png')
    
    output$downloadData17 = downloadHandler(
      filename = function() { 
        paste('GraphPartial','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphPartial()$Plot))
      },
      contentType=NA)
    
    
    output$downloadData19 = downloadHandler(
      filename = function() { 
        paste('GraphFreq','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphFreq()$Plot))
      },
      contentType='image/jpg')
    
    output$download19=renderUI({
      if(is.null(anafact$freq)){
        return()
      }
      else{
        return(downloadButton("downloadData19",gettext("Download as jpg",domain="R-Factoshiny")))
      }
    })
    
    output$downloadData20 = downloadHandler(
      filename = function() { 
        paste('GraphFreq','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphFreq()$Plot))
      },
      contentType='image/png')
    
    output$download20=renderUI({
      if(is.null(anafact$freq)){
        return()
      }
      else{
        return(downloadButton("downloadData20",gettext("Download as png",domain="R-Factoshiny")))
      }
    })
    
    output$downloadData21 = downloadHandler(
      filename = function() { 
        paste('GraphFreq','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphFreq()$Plot))
      },
      contentType=NA)
    
    output$download21=renderUI({
      if(is.null(anafact$freq)){
        return()
      }
      else{
        return(downloadButton("downloadData21",gettext("Download as pdf",domain="R-Factoshiny")))
      }
    })
    
    # output$downloadData22 = downloadHandler(
      # filename = function() { 
        # paste('GraphFreq','.emf', sep='') 
      # },
      # content = function(file) {
        # ggplot2::ggsave(file,print(CodeGraphFreq()$Plot))
      # },
      # contentType=NA)
    
    output$downloadData1 = downloadHandler(
      filename = function() { 
        paste('GraphInd','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphInd()$Plot))
      },
      contentType='image/jpg')
    
    output$downloadData2 = downloadHandler(
      filename = function() { 
        paste('GraphInd','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphInd()$Plot))
      },
      contentType=NA)
    
    output$downloadData4 = downloadHandler(
      filename = function() { 
        paste('GraphVar','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphVar()$Plot))
      },
      contentType='image/jpg')
    
    output$download4=renderUI({
      if(is.null(anafact$quanti.var)){
        return()
      }
      else{
        return(downloadButton("downloadData4",gettext("Download as jpg",domain="R-Factoshiny")))
      }
    })
    
    output$downloadData5 = downloadHandler(
      filename = function() { 
        paste('GraphVar','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,print(CodeGraphVar()$Plot))
      },
      contentType=NA)
    
    output$download5=renderUI({
      if(is.null(anafact$quanti.var)){
        return()
      }
      else{
        return(downloadButton("downloadData5",gettext("Download as pdf",domain="R-Factoshiny")))
      }
    })
        
    output$sorties1=renderTable({
      return(as.data.frame(anafact$ind$coord))
    },rownames=TRUE)
    
    output$sorties2=renderTable({
      return(as.data.frame(anafact$ind$contrib))
    },rownames=TRUE)
    
    output$sorties3=renderTable({
      return(as.data.frame(anafact$ind$cos2))
    },rownames=TRUE)
    
    output$sorties4=renderTable({
      return(as.data.frame(anafact$ind$within.inertia))
    },rownames=TRUE)
    
    output$sorties5=renderTable({
      return(as.data.frame(anafact$ind$coord.partiel))
    },rownames=TRUE)
    
    output$sorties6=renderTable({
      return(as.data.frame(anafact$ind$within.partial.inertia))
    },rownames=TRUE)
    
    output$sorties11=renderTable({
      return(as.data.frame(anafact$quanti.var$coord))
    },rownames=TRUE)
    
    output$sorties22=renderTable({
      return(as.data.frame(anafact$quanti.var$contrib))
    },rownames=TRUE)
    
    output$sorties33=renderTable({
      return(as.data.frame(anafact$quanti.var$cos2))
    },rownames=TRUE)
    
    output$sorties44=renderTable({
      return(as.data.frame(anafact$quanti.var$cor))
    },rownames=TRUE)
    
    output$sorties12=renderTable({
      return(as.data.frame(anafact$partial.axes$coord))
    },rownames=TRUE)
    
    output$sorties23=renderTable({
      return(as.data.frame(anafact$partial.axes$cor))
    },rownames=TRUE)
    
    output$sorties34=renderTable({
      return(as.data.frame(anafact$partial.axes$contrib))
    },rownames=TRUE)
    
    output$sorties45=renderTable({
      return(as.data.frame(anafact$partial.axes$cor.between))
    },rownames=TRUE)    
    
    output$sortiegroup=renderTable({
      write.infile(X=anafact$group,file=paste(getwd(),"fichgroup.csv"),sep=";",nb.dec=5)
      baba=read.csv(paste(getwd(),"fichgroup.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      file.remove(paste(getwd(),"fichgroup.csv"))
      baba
    },
    rownames=FALSE)
    
    output$CodePrinted <- renderPrint({
       if (input$MFAcode!=0){
          cat(ligne,sep="\n")
          cat(CodeGraphInd()$Code,sep="\n")
          if(!is.null(anafact$quanti.var)) cat(CodeGraphVar()$Code,sep="\n")
          cat(CodeGraphGroup()$Code,sep="\n")
          cat(CodeGraphPartial()$Code,sep="\n")
          if(!is.null(anafact$freq)) cat(CodeGraphFreq()$Code,sep="\n")
       }
    })

    output$CodePrintedDimdesc <- renderPrint({
       if (input$MFAcode!=0){
        cat(codeMFA()$Code,sep="\n")
        cat("dimdesc(res.MFA)",sep="\n")
       }
    })

    output$CodePrintedSummary <- renderPrint({
       if (input$MFAcode!=0){
        cat(codeMFA()$Code,sep="\n")
        cat("summary(res.MFA)",sep="\n")
       }
    })

    # observe({
      # if(input$MFAcode!=0){
        # isolate({
          # print(ligne)
          # cat(CodeGraphInd()$Code,sep="\n")
          # if(!is.null(anafact$quanti.var)) cat(CodeGraphVar()$Code,sep="\n")
          # cat(CodeGraphGroup()$Code,sep="\n")
          # cat(CodeGraphPartial()$Code,sep="\n")
          # if(!is.null(anafact$freq)) cat(CodeGraphFreq()$Code,sep="\n")
        # })
      # }
    # })
    
  observe({
   if(input$Quit!=0){
     isolate({
      res=list()
      # res$codeMFA=ligne
      res$nomObjectMFA <- nomObjectMFA
	  res$codeMFA <- paste(nomObjectMFA,"<-",as.character(as.expression(ligne)))
      res$anafact=anafact
      res$data=newdataMFAshiny
      res$axe1=input$nb1
      res$axe2=input$nb2
      res$ind1=input$meanind1
      res$ind2=input$meanind
      res$ind3=input$qualind1
      res$ind4=input$qualind
      res$sizeInd <- input$cexInd
      res$sizeVar <- input$cexVar
      res$sizeGroup <- input$cexGroup
      res$sizePartial <- input$cexPartial
      res$sizeFreq <- input$cexFreq
      res$drawing=input$drawind
      res$drawing2=input$habiquali
      res$partial=input$choixpartial
      res$partial2=input$indivpartiel
      res$partial3=input$partind
      res$selectionMFAshiny <- input$select
      sel=NULL
      if (length(input$selection)>0){
	    if(input$selection=="contrib") sel=input$slider2
        if(input$selection=="cos2") sel=input$slider3
      }
	  if (length(input$select)>0){
	    if(input$select=="cos2") res$selection2MFAshiny <- input$sliderind1
        if(input$select==gettext("No selection",domain="R-Factoshiny")) res$selection2MFAshiny <- NULL
        if(input$select=="contrib") res$selection2MFAshiny <- input$sliderind0
        if(input$select==gettext("Manual",domain="R-Factoshiny")) res$selection2MFAshiny <- input$indiv
      }
	  res$selectvar2=sel
      res$hide=input$hides
      res$colorvar=input$colorgroup
      res$freq1=input$affichind
      res$freq2=input$affichcol
      res$partaxe=input$coloraxe
      res$nomData=nomData
      res$CodeGraphInd=CodeGraphInd()$Code
      res$CodeGraphVar=CodeGraphVar()$Code
      res$CodeGraphPartial=CodeGraphPartial()$Code
      res$CodeGraphGroup=CodeGraphGroup()$Code
      res$CodeGraphFreq=CodeGraphFreq()$Code
      res$titleInd=input$titleInd
      res$titleVar=input$titleVar
      res$titleGroup=input$titleGroup
      res$titleFreq=input$titleFreq
      res$titlePartial=input$titlePartial
      res$hcpcparam <- input$hcpcparam
      res$nbdimclustMFAshiny <- input$nbDimClustering
	  res$ind_var <- input$ind_var
	  res$ind_varfreq <- input$ind_varfreq
      class(res)="MFAshiny"
      stopApp(returnValue=res)
     })
    }
 })

}
