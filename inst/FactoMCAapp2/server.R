# server script for MCA2

  function(input, output,session) {

    getactive=function(){
        if(length(input$supvar)==0){
          activevar=VariableChoicesMCAshiny
        } else{
          activevar=VariableChoicesMCAshiny[-which(VariableChoicesMCAshiny%in%input$supvar)]
        }
        return(activevar)
    }
    
  output$NB1 <- renderUI({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    if(length(getactive())>5){
       return(textInput("nb1", label = NULL, axe1MCAshiny,width='41px'))
    } else{
       return(selectInput("nb1",label=NULL, choices=1:length(getactive()),selected=axe1MCAshiny,width='41px'))
    }
  })
  
  output$NB2=renderUI({
    validate(
      need(length(getactive())>1 ,gettext("Please select at least two active variables"))
    )
    if(length(getactive())>5){
       return(textInput("nb2", label = NULL, axe2MCAshiny,width='41px'))
    } else{
       return(selectInput("nb2",label=NULL, choices=(1:length(getactive())),selected=axe2MCAshiny,width='41px'))
    }
  })
  
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
        return(tags$div( 
            div(gettext("Number of dimensions kept for clustering"), style="display: inline-block; padding: 0px 0px 0px 0px"),
		    div(numericInput(inputId = "nbDimClustering", label = NULL,value=if(is.null(nbdimclustMCAshiny)){5} else {nbdimclustMCAshiny},min=1), style="display: inline-block;width: 70px; padding: 0px 0px 0px 10px"))
		)
    }
  })

    #Realisation de l'ACM    
    values=reactive({
      
    if(length(input$supvar)==0) {
	  data.selec <- newdataMCAshiny[,VariableChoicesMCAshiny]
	} else {
	  data.selec <- newdataMCAshiny[,c(getactive())]
	}

    if(length(QuantiChoiceMCAshiny)==0 || length(input$supquanti)==0){
      choixquanti <- NULL
    } else {
      data.selec <- cbind(data.selec,newdataMCAshiny[,if(!is.null(input$supquanti)){input$supquanti} else{QuantiChoiceMCAshiny},drop=FALSE])
      choixquanti <- seq((ncol(data.selec)-length(input$supquanti)+1),ncol(data.selec))
      colnames(data.selec)[choixquanti] <- input$supquanti
    }

    if(length(input$supvar)==0){
      choixquali <- NULL
    } else {
      data.selec <- cbind(data.selec,newdataMCAshiny[,input$supvar])
      if(length(input$supvar)==1){
        choixquali <- length(data.selec)
        colnames(data.selec)[choixquali]<-input$supvar
      } else{
        choixquali <- seq((ncol(data.selec)-length(input$supvar)+1),ncol(data.selec))
      }
    }
    if (length(input$habiller)==2 && input$color_point==gettext("qualitative variable")){
	  codeMCAp <- paste0(nomDataMCAshiny," <- data.frame(",nomDataMCAshiny,paste0("[,c(",paste0("'",paste(colnames(data.selec),collapse="','"),"'"),")],Interaction=paste(",nomDataMCAshiny,"[,'",input$habiller[1],"'],",nomDataMCAshiny,"[,'",input$habiller[2],"']))\n"))
      data.selec <- data.frame(data.selec,Interaction=paste(newdataMCAshiny[,input$habiller[1]],newdataMCAshiny[,input$habiller[2]],sep="/"))
      choixquali <- c(choixquali,ncol(data.selec))
    }
    if(length(input$indsup)==0){
      suple <- NULL
    } else{
      suple <- which(nomMCAshiny%in%input$indsup)
    }
    codeMCA <- paste0(paste0("res.MCA<-MCA(",nomDataMCAshiny, if (!identical(newdataMCAshiny,data.selec)){paste0("[,c(",paste0("'",paste(colnames(data.selec),collapse="','"),"'"),")]")}))
	codeMCA <- paste0(codeMCA,if(max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))!=5) paste0(",ncp=",max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))),if(!is.null(choixquanti)) paste0(",quanti.sup=c(",paste(choixquanti,collapse=","),")"),if(!is.null(choixquali)) paste0(",quali.sup=c(",paste(choixquali,collapse=","),")"),if(!is.null(suple)) paste0(",ind.sup=c(",paste(suple,collapse=","),")"),if (!is.null(poids1MCAshiny)) paste0(",row.w=c(",paste(poids1MCAshiny,collapse=","),")"),",graph=FALSE)")
    if (length(input$habiller)==2 && input$color_point==gettext("qualitative variable")) list(res.MCA=eval(parse(text=paste0(codeMCAp,"\n",codeMCA))), codeMCA=codeMCA, codeMCAp=codeMCAp, choixqual=choixquali, choixquant=choixquanti)
    else list(res.MCA=eval(parse(text=codeMCA)), codeMCA=codeMCA, choixqual=choixquali, choixquant=choixquanti)
  })

    output$col1=renderUI({
      if(!is.null(input$indsup)){
       return(tags$div(
        div(colourpicker::colourInput("colindsup", label=NULL, value = if (!is.null(input$colindsup)){if (input$colindsup!="blue") input$colindsup  else{color2MCAshiny}} else{color2MCAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		div(gettext("supplementary individuals"), style="display: inline-block;padding: 0px 0px 0px 10px")))
      }
    })
    
    output$choixindvar=renderUI({
      choix=list(gettext("Individuals"),gettext("Categories"))
      if(!(is.null(input$indsup))) choix <- c(choix,gettext("Supplementary individuals"))
      if(!(is.null(input$supvar))) choix <- c(choix,gettext("Supplementary categories"))
      div(align="left",checkboxGroupInput("ind_var",gettext("Points to draw"), choices=choix, selected = indvarMCAshiny))
    })
    
    output$pointlabel=renderUI({
      validate(
        need(!is.null(input$ind_var),""))
      choix=list()
      reponse=input$ind_var
      if(sum(gettext("Individuals")==reponse)==0) choix=c(choix,gettext("Individuals"))
      if(sum(gettext("Categories")==reponse)==0) choix=c(choix,gettext("Categories"))
      if(sum(gettext("Supplementary individuals")==reponse)==0) choix=c(choix,gettext("Supplementary individuals"))
      if(sum(gettext("Supplementary categories")==reponse)==0) choix=c(choix,gettext("Supplementary categories"))
      div(align="center",checkboxGroupInput("indvarpoint","",choices=choix,selected=labvarMCAshiny))
    })
    
    output$out22=renderUI({
      choix=list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results of the variables"),gettext("Results of the individuals"))
      if(!is.null(input$indsup)) choix=c(choix,gettext("Results of the supplementary individuals"))
      if(!is.null(input$supquanti)) choix=c(choix,gettext("Results of the supplementary quantitative variables"))
      if(!is.null(values()$choixqual)) choix=c(choix,gettext("Results of the supplementary categorical variables"))
      radioButtons("out",gettext("Which outputs do you want?"), choices=choix,selected=gettext("Summary of outputs"),inline=TRUE)
    })
    
    output$colquanti12=renderUI({
      # if(is.null(color7MCAshiny)){
        # return(colourpicker::colourInput("colquanti",gettext("Supplementary quantitative variables"),"blue",allowTransparent=TRUE))
      # }else{
        # return(colourpicker::colourInput("colquanti",gettext("Supplementary quantitative variables"),color7MCAshiny,allowTransparent=TRUE))
      # }
       return(tags$div( 
        div(colourpicker::colourInput("colquanti", label=NULL, if (!is.null(input$colquanti)) input$colquanti else{color7MCAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		div(gettext("supplementary quantitative variables"), style="display: inline-block;padding: 0px 0px 0px 10px")))
    })
    
    output$colquantib=renderUI({
      if(!is.null(values()$choixquant)){
       return(tags$div( 
          # return(colourpicker::colourInput("colli",gettext("Supplementary quantitative variables"),color8MCAshiny,allowTransparent=TRUE))
        div(colourpicker::colourInput("colli", label=NULL, if(!is.null(input$colli)){if (input$colli!="blue") input$colli} else{color8MCAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		div(gettext("supplementary quantitative variables"), style="display: inline-block;padding: 0px 0px 0px 10px")))
      }
    })
	
    # getinv=function(){
      
      # inv<-c()
      # if(sum(gettext("Individuals")==input$ind_var)==0)  inv<-c(inv,"ind")
      # if(sum(gettext("Categories")==input$ind_var)==0) inv<-c(inv,"var")
      # if(!(is.null(values()$choixqual)) & sum(gettext("Supplementary categories")==input$ind_var)==0) inv<-c(inv,"quali.sup")
      # if(!(is.null(input$indsup)) & sum(gettext("Supplementary individuals")==input$ind_var)==0) inv<-c(inv,"ind.sup")
      # }

      # vecinv <- paste0("'",paste(inv,collapse="','"),"'")
      # if(length(inv)>1){
        # vecinv<-paste0("c(",vecinv,")")
      # }
      # else if(length(inv)==1){
        # vecinv<-paste("'",inv,"'",sep="")
      # }
      # else if(length(inv)==0){
        # vecinv<-"NULL"
      # }
      
      # list(inv=(inv),vecinv=(vecinv))
    # }
    
    # getinv2=function(){
      # inv<-c()
      # if(sum(gettext("Supplementary qualitative variables")==input$var_sup)==0) inv<-c(inv,"quali.sup")
      # if(sum(gettext("Supplementary quantitative variables")==input$var_sup)==0) inv<-c(inv,"quanti.sup")
      # if(sum(gettext("Active qualitative variables")==input$var_sup)==0) inv<-c(inv,"var")
      
      # vecinv <- paste("'",paste(inv,collapse="','"),"'",sep="")
      
      # if(length(inv)>1){
        # vecinv<-paste("c(",vecinv,")",sep="")
      # }
      # else if(length(inv)==1){
        # vecinv<-paste("'",inv,"'",sep="")
      # }
      # else if(length(inv)==0){
        # vecinv<-"NULL"
      # }
      # list(inv=(inv),vecinv=(vecinv))
    # }
    
    #GRAPHIQUE 3: Variables
    
    codeGraphVar <- reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions")),
        need(length(getactive())>2 ,gettext("Please select more variables")),
		need(length(input$var_sup)>0, gettext("Select at least 1 choice"))
      )
      inv<-c()
      if(sum(gettext("Supplementary qualitative variables")==input$var_sup)==0) inv<-c(inv,"'quali.sup'")
      if(sum(gettext("Supplementary quantitative variables")==input$var_sup)==0) inv<-c(inv,"'quanti.sup'")
      if(sum(gettext("Active qualitative variables")==input$var_sup)==0) inv<-c(inv,"'var'")
      if(length(inv)>1) vecinv<-paste("c(",paste0(inv,collapse=","),")",sep="")
      if(length(inv)==1) vecinv<- inv

      if(input$eachvar==TRUE){
        colouract2 <- 1:length(values()$res.MCA$call$quali)
		colouract2 <- paste0("c(",paste0(colouract2,collapse=','),")")
        if (!is.null(values()$res.MCA$call$quali.sup)) {
		  coloursup2 <- (1+length(values()$res.MCA$call$quali)) : (length(values()$res.MCA$call$quali)+length(values()$res.MCA$call$quali.sup))
		  coloursup2 <- paste0("c(",paste0(coloursup2,collapse=','),")")
		}
	  }
      Code <- paste0("plot.MCA(res.MCA, choix='var'",if (length(inv)>0){paste0(",invisible=",vecinv)},if (input$title2MCAshiny!="Variables representation") paste0(',title="',input$title2MCAshiny,'"'),if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if(!is.null(input$colvaract1) & input$colvaract1 != "#FF0000" & input$eachvar!=TRUE) paste0(",col.var='",input$colvaract1,"'"),if(!is.null(input$eachvar) & input$eachvar==TRUE) paste0(",col.var=",colouract2),if(!is.null(input$colvarsup1) & input$eachvar!=TRUE) paste0(",col.quali.sup='",input$colvarsup1,"'"),if(!is.null(input$eachvar) & input$eachvar==TRUE & !is.null(input$colvarsup1)) paste0(",col.quali.sup=",coloursup2),if(!is.null(input$colli)) {if (input$colli!="#0000FF") paste0(",col.quanti.sup='",input$colli,"'")},if(input$cex2!=1){paste0(",cex=",input$cex2,",cex.main=",input$cex2,",cex.axis=",input$cex2)},")") 
	  res.MCA <- values()$res.MCA
	  Plot <- eval(parse(text=Code))
      return(list(Code=Code,Plot=Plot))
    })
    
    output$map4 <- renderPlot({
      return(print(codeGraphVar()$Plot))
    })
    
    output$col3=renderUI({
      if(!is.null(values()$choixqual)){
       return(tags$div( 
        div(colourpicker::colourInput("colvarsup1", label=NULL, if (!is.null(input$colvarsup1)) input$colvarsup1 else{color6MCAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		div(gettext("supplementary categorical variables"), style="display: inline-block;padding: 0px 0px 0px 10px")))
        # if(is.null(color6MCAshiny)){
          # return(colourpicker::colourInput("colvarsup1",gettext("supplementary categorical variables"),"darkgreen",allowTransparent=TRUE))
        # }else{
          # return(colourpicker::colourInput("colvarsup1",gettext("supplementary categorical variables"),color6MCAshiny,allowTransparent=TRUE))
        # }
      }
    })
    
    observe({
      if(input$color_point==gettext("qualitative variable")) updateCheckboxInput(session, "drawconf", value = FALSE)
    })

    output$ellips=renderUI({
      if(length(input$habiller)>0)  return(checkboxInput("drawconf","Draw confidence ellipses around center of caregories",FALSE))
    })

    codeGraphInd <- reactive({
      
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions")),
        need(length(getactive())>2 ,gettext("Please select more variables")),
        need(length(input$ind_var)>=1,gettext("Please select the object you want to plot: Individuals, variables or both")),
        need(input$habiller == TRUE || input$habiller == FALSE || length(input$habiller)<=2,gettext("Please select maximum 2 variables as habillage"))
      )
      
      inv<-c()
      if(sum(gettext("Individuals")==input$ind_var)==0)  inv<-c(inv,"'ind'")
      if(sum(gettext("Categories")==input$ind_var)==0) inv<-c(inv,"'var'")
      if(!(is.null(values()$choixqual)) & sum(gettext("Supplementary categories")==input$ind_var)==0) inv<-c(inv,"'quali.sup'")
      if(!(is.null(input$indsup)) & sum(gettext("Supplementary individuals")==input$ind_var)==0) inv<-c(inv,"'ind.sup'")
      if(length(inv)>1) vecinv<-paste0("c(",paste0(inv,collapse=","),")")
      if(length(inv)==1) vecinv <- inv
      if(length(inv)==0) vecinv<-"NULL"

      if(input$select==gettext("Manual")) selecindivText <- paste0("c(",paste0(input$indiv,collapse=","),")")
      selecindivText="NULL"
      if(input$select=="cos2"){
        if(input$slider1!=1){
          selecindivText=paste0("'cos2 ",input$slider1,"'")
        } else{
          selecindivText="'cos2 0.999999'"
        }
      }
      if(input$select=="Contrib") selecindivText=paste0("'contrib ",input$sliderContrib,"'") 

      selecModText <- "NULL"
      if(input$selectMod=="cos2"){
        if(input$sliderCosMod!=1){
          selecModText=paste0("'cos2 ",input$sliderCosMod,"'")
        } else{
          selecModText="'cos2 0.999999'"
        }
      }
      if(input$selectMod=="Contrib") selecModText=paste("'contrib ",input$slider4,"'")
      
      hab <- "none"
      if(input$color_point == gettext("quantitative variable")) hab <- paste0("'",input$habiller,"'")
      if(input$color_point == "cos2") hab <- "'cos2'"
      if(input$color_point == "contribution") hab <- "'contrib'"
      if(input$color_point==gettext("qualitative variable")){
      if (length(input$habiller)==1) hab <- which(colnames(values()$res.MCA$call$X)==input$habiller)
      if (length(input$habiller)==2) hab <- ncol(values()$res.MCA$call$X)
	}
      
      validate(
        need(length(input$ind_var)!="",gettext("Please select which object you would like to print"))
      )
      if(input$eachvar==TRUE){
        colouract2 <- rep(1:length(values()$res.MCA$call$quali),sapply(values()$res.MCA$call$X[,values()$res.MCA$call$quali],nlevels))
		colouract2 <- paste0("c(",paste0(colouract2,collapse=','),")")
        if (!is.null(values()$res.MCA$call$quali.sup)) {
		  coloursup2=rep((1+length(values()$res.MCA$call$quali)) : (length(values()$res.MCA$call$quali)+length(values()$res.MCA$call$quali.sup)),sapply(values()$res.MCA$call$X[,values()$res.MCA$call$quali.sup],nlevels))
		  coloursup2 <- paste0("c(",paste0(coloursup2,collapse=','),")")
		}
	  }
      Code <- paste0(if(!is.null(input$drawconf)&&input$drawconf==TRUE){paste0("plotellipses(res.MCA,keepvar=",hab)}else{"plot.MCA(res.MCA"},if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (vecinv!="NULL") paste(",invisible=",vecinv),if (selecindivText!="NULL") paste(",select=",selecindivText),if (selecModText!="NULL") paste(",selectMod=",selecModText),if (hab != "none") paste0(",habillage=",hab),if(input$colindact!="#000000") paste0(",col.ind='",input$colindact,"'"),if(input$eachvar==TRUE & hab!="'cos2'" & hab!="'contrib'") paste0(",col.var=",colouract2),if(input$eachvar==TRUE & hab!="'cos2'" & hab!="'contrib'" & !is.null(values()$res.MCA$call$quali.sup)) paste0(",col.quali.sup=",coloursup2), if(input$colvaract1!="#FF0000" & input$eachvar==FALSE) paste0(",col.var='",input$colvaract1,"'"), if(!is.null(input$colindsup)) {if(input$colindsup!="blue") paste0(",col.ind.sup='",input$colindsup,"'")},if(!is.null(input$colvarsup1)& input$eachvar!=TRUE) {if(input$colvarsup1!="darkgreen") paste0(",col.quali.sup='",input$colvarsup1,"'")},if(input$title1MCAshiny!="MCA factor map") paste0(',title="',input$title1MCAshiny,'"'),if (input$cex!=1) paste0(",cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex),")")
	  res.MCA <- values()$res.MCA
	  Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))
    })
    
    output$map <- renderPlot({
      p <- print(codeGraphInd()$Plot)
    })
       
        
    #GRAPHIQUE 2
    
    codeGraphQuanti <- function(){
      if(is.null(input$colquanti)){
        colquanti="blue"
      }else{
        colquanti=input$colquanti
      }
      if (!is.null(values()$choixquant)) {
	    Code <- paste0("plot.MCA(res.MCA, choix='quanti.sup'",if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (input$title3MCAshiny!="Supplementary variables on the MCA map") paste0(',title="',input$title3MCAshiny,'"'),if(input$colquanti!="#0000FF") paste0(",col.quanti.sup='",input$colquanti,"'"), if (input$cex3!=1) paste0(",cex=",input$cex3,",cex.main=",input$cex3,",cex.axis=",input$cex3),")")
		res.MCA <- values()$res.MCA
	    Plot <- eval(parse(text=Code))
	    return(list(Code=Code,Plot=Plot))
      }
    }
    
    output$map2 <- renderPlot({
      p <- print(codeGraphQuanti()$Plot)
    })  
    
    output$map22=renderUI({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions")),
        need( length(getactive())>2,gettext("Please more active variables"))
      )
      
      if(length(QuantiChoiceMCAshiny)==0 || length(input$supquanti)==0){
        return(p())
      }
      else{
        column(width = 6,shinyjqui::jqui_resizable(plotOutput("map2", height="500")),
           br(),
           p(gettext("Download as"),downloadButton("downloadData4",gettext("jpg")),downloadButton("downloadData3",gettext("png")),downloadButton("downloadData5",gettext("pdf")),align="center")
		)
	  }
    })

    
    output$choixchange=renderUI({
      if(length(QuantiChoiceMCAshiny)==0 || length(input$supquanti)==0){
        return(radioButtons("MCAgraph",gettext("Which graph do you want to modify?"),
                   choices=list(gettext("Individuals and categories"),"Variables"="var"),inline=TRUE))
      } else{
        return(radioButtons("MCAgraph",gettext("Which graph do you want to modify?"),
                  choices=list(gettext("Individuals and categories"),"Variables"="var",gettext("Quantitative variables")),inline=TRUE))
      }
    })
    
    
       
    output$habillage2=renderUI({
      if (input$color_point == gettext("qualitative variable")){
        return(selectizeInput("habiller",gettext("select 1 or 2 variables"), choices=qualiMCAshiny, multiple=TRUE, selected=habillageindMCAshiny))
      }
      # if(is.null(habillageindMCAshiny)){
        # num=c(1:length(qualiMCAshiny))
        # return(selectInput("habiller","Select 1 or 2 variables", choices=list(num=qualiMCAshiny),multiple=TRUE))
      # }
      # else{
        # num=c(1:length(qualiMCAshiny))
        # return(selectInput("habiller","Select 1 or 2 variables", choices=list(num=qualiMCAshiny),multiple=TRUE,selected=habillageindMCAshiny))
      # }
    }) 
    
    #CALCUL DE LA CONTRIBUTION DES MODALITES
    
    output$slider3=renderUI({
      validate(
        need(length(getactive())>2,gettext("Please select more variables"))
      )
      maxvar=dim(values()$res.MCA$var$coord)[1]

      if(selection3MCAshiny=="Contrib"){return(sliderInput("slider4",label="Contribution",
                                                   min=1,max=maxvar,value=as.numeric(selection4MCAshiny),step=1)) }
      else{
        return(sliderInput("slider4",label="Contribution",
                           min=1,max=maxvar,value=maxvar,step=1))
      }
    })
    
    ###
    
    
    ###
    #SUMMARY
    
    output$summary=renderPrint({
      summary(newdataMCAshiny)
    })
    
    
    #Histogramme des valeurs propres
    output$map3=renderPlot({
      print(ggplot2::ggplot(cbind.data.frame(x=1:nrow(values()$res.MCA$eig),y=values()$res.MCA$eig[,2])) + ggplot2::aes(x=x, y=y)+ ggplot2::geom_col(fill="blue") + ggplot2::xlab("Dimension") + ggplot2::ylab(gettext("Percentage of variance")) + ggplot2::ggtitle(gettext("Decomposition of the total inertia")) + ggplot2::theme_light() + ggplot2::theme(plot.title = ggplot2::element_text(hjust =0.5))  + ggplot2::scale_x_continuous(breaks=1:nrow(values()$res.MCA$eig)))
      # return(barplot(values()$res.MCA$eig[,1],names.arg=rownames(values()$res.MCA$eig),las=2,density=TRUE))
    })
    
    #Histogramme du summary
    output$histo=renderPlot({
      ggplot2::ggplot(newdataMCAshiny) + aes(x=newdataMCAshiny[,input$bam]) + geom_bar() + labs(y="Count",x="")
      # barplot(prop.table(table(newdataMCAshiny[,input$bam]))*100)
    })
    
    #Summary de l'ACM
    
    
    output$summaryMCA=renderPrint({
      validate(
        need(input$nbele!=0, gettext("Please select at least one element"))
      )
      a<-values()$res.MCA  
      a$call$call<-values()$codeMCA
      
      summary.MCA(a,nbelements=input$nbele)
    })
    
    output$summary2=downloadHandler(filename = function() { 
      paste('summaryofMCA','.txt', sep='') 
    },
    content = function(file) {
      summary.MCA(values()$res.MCA,nbelements=input$nbele,file=file)
    },
    contentType='text/csv')
    
    
    output$sorties=renderTable({
      return(as.data.frame(values()$res.MCA$eig))
    },rownames=TRUE)
    
    output$sorties2=renderTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      return(as.data.frame(values()$res.MCA$var$coord))
    },rownames=TRUE)
    
    output$sorties3=renderTable({
      
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      return(as.data.frame(values()$res.MCA$var$contrib))
    },rownames=TRUE)
    
    output$sorties4=renderTable({
      
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      return(as.data.frame(values()$res.MCA$var$cos2))
    },rownames=TRUE)
    
    output$sorties22=renderDataTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      tab<-as.data.frame(values()$res.MCA$ind$coord)
      tab<-round(tab, 3)
      tab<-cbind(Names=rownames(tab),tab)
      return(tab)
    })
    
    output$sorties33=renderDataTable({
      
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      tab1<-as.data.frame(values()$res.MCA$ind$contrib)
      tab1<-round(tab1,3)
      tab1<-cbind(Names=rownames(tab1),tab1)
      return(tab1)
    })
    
    output$sorties44=renderDataTable({
      
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      tab2<-as.data.frame(values()$res.MCA$ind$cos2)
      tab2<-round(tab2,3)
      tab2<-cbind(Names=rownames(tab2),tab2)
      return(tab2)
    })
    
    output$sorties22s=renderDataTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      tab<-as.data.frame(values()$res.MCA$ind.sup$coord)
      tab<-round(tab, 3)
      tab<-cbind(Names=rownames(tab),tab)
      return(tab)
    })
    
    output$sorties44s=renderDataTable({
      
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      tab2<-as.data.frame(values()$res.MCA$ind.sup$cos2)
      tab2<-round(tab2,3)
      tab2<-cbind(Names=rownames(tab2),tab2)
      return(tab2)
    })

    output$sorties23=renderTable({
      
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables")),
        need(length(input$supvar)!=0, gettext("No supplementary categorical variables"))
      )
      return(as.data.frame(values()$res.MCA$quali.sup$coord))
    })
    
    output$sorties232=renderTable({
      
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables")),
        need(length(input$supvar)!=0, gettext("No supplementary categorical variables"))
      )
      return(as.data.frame(values()$res.MCA$quali.sup$cos2))
    },rownames=TRUE)
    
    output$sorties233=renderTable({
      
      validate(
        need(length(getactive())>2 ,gettext("Please select more variable")),
        need(length(input$supvar)!=0, gettext("No supplementary categorical variables"))
      )
      return(as.data.frame(values()$res.MCA$quali.sup$v.test))
    },rownames=TRUE)
    
    output$sorties43=renderTable({
      validate(
        need(length(getactive())>2,gettext("Please select more variable")),
        need(length(input$supquanti)!=0 || input$supquanti==TRUE, gettext("No supplementary quantitative variables"))
      )
      return(as.data.frame(values()$res.MCA$quanti.sup$coord))
    },rownames=TRUE)
    
    output$sortiesIsupC=renderTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables")),
        need(length(input$indsup)!=0,gettext("No supplementary individuals"))
      )
      return(as.data.frame(values()$res.MCA$ind.sup$coord))
    })
    
    output$sortiesIsupCos=renderTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables")),
        need(length(input$indsup)!=0,gettext("No supplementary individuals"))
      )
      return(as.data.frame(values()$res.MCA$ind.sup$cos2))
    },rownames=TRUE)
    
    #DIM1
    
    output$sortieDimdesc=renderTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[1]]$category))
    },rownames=TRUE)
    
    output$sortieDimdesc2=renderTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[1]]$quali))
    },rownames=TRUE)
    output$sortieDimdesc3=renderTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables")),
        need(length(input$supquanti)>0,gettext("No quantitative variable")),
        need(length(dimdesc(values()$res.MCA)[[1]]$quanti)!=0,"")
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[1]]$quanti))
    },rownames=TRUE)
    
    #DIM2
    output$sortieDimdesc00=renderTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[2]]$category))
    },rownames=TRUE)
    output$sortieDimdesc22=renderTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[2]]$quali))
    },rownames=TRUE)
    output$sortieDimdesc33=renderTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables")),
        need(length(dimdesc(values()$res.MCA)[[2]]$quanti)!=0,""),
        need(length(input$supquanti)>0,gettext("No quantitative variable"))
	  )
      return(as.data.frame(dimdesc(values()$res.MCA)[[2]]$quanti))
    },rownames=TRUE)
    
    #DIM3
    output$sortieDimdesc000=renderTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[3]]$category))
    },rownames=TRUE)
    output$sortieDimdesc222=renderTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables"))
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[3]]$quali))
    },rownames=TRUE)
    output$sortieDimdesc333=renderTable({
      validate(
        need(length(getactive())>2 ,gettext("Please select more variables")),
        need(length(dimdesc(values()$res.MCA)[[3]]$quanti)!=0,""),
        need(length(input$supquanti)>0,gettext("No quantitative variable")))
      return(as.data.frame(dimdesc(values()$res.MCA)[[3]]$quanti))
    },rownames=TRUE)
    
    output$JDD=renderDataTable({
      cbind(Names=rownames(newdataMCAshiny),newdataMCAshiny)},      
      options = list("orderClasses" = TRUE, "responsive" = TRUE, "pageLength" = 10))
    ####
  observe({
    if(input$Investigatehtml!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveMCAshiny)
        FactoInvestigate::Investigate(values()$res.MCA,codeGraphInd = if (input$choixGRAPH==gettext("Graphs done")) {paste0(values()$codeMCAp,"\n",values()$codeMCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done")) {codeGraphVar()$Code} else {NULL}, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= substr(tolower(input$choixLANG),1,2))
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$Investigatedoc!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveMCAshiny)
        FactoInvestigate::Investigate(values()$res.MCA,codeGraphInd = if (input$choixGRAPH==gettext("Graphs done")) {paste0(values()$codeMCAp,"\n",values()$codeMCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done")) {codeGraphVar()$Code} else {NULL},document="word_document",openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= substr(tolower(input$choixLANG),1,2))
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$InvestigateRmd!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveMCAshiny)
	    FactoInvestigate::Investigate(values()$res.MCA,codeGraphInd = if (input$choixGRAPH==gettext("Graphs done")) {paste0(values()$codeMCAp,"\n",values()$codeMCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done")) {codeGraphVar()$Code} else {NULL}, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= substr(tolower(input$choixLANG),1,2))
	    print(paste0(gettext("The file "),input$titleFile,gettext(" as well as the RData objects are available in the sub-directory: "),getwd()))
        setwd(path.aux)
      })
    }
  })
    

  output$downloadData0 = downloadHandler(
      filename = function() { 
        paste('graphVarMCA','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraphVar()$Plot)
      },
      contentType='image/png')
    
    output$downloadData10 = downloadHandler(
      filename = function() { 
        paste('graphVarMCA','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraphVar()$Plot)
      },
      contentType='image/jpg')
    
    output$downloadData20 = downloadHandler(
      filename = function() { 
        paste('graphVarMCA','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraphVar()$Plot)
      },
      contentType=NA)
    
    ####
    
    
    output$downloadData = downloadHandler(
      filename = function() { 
        paste('graphMCA','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraphInd()$Plot)
      },
      contentType='image/png')
    
    output$downloadData1 = downloadHandler(
      filename = function() { 
        paste('graphMCA','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraphInd()$Plot)
      },
      contentType='image/jpg')
    
    output$downloadData2 = downloadHandler(
      filename = function() { 
        paste('graphMCA','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraphInd()$Plot)
      },
      contentType=NA)
    
    
    output$download3 = renderUI({
      if(length(values()$choixquant)==0){
        return()
      }
      else{
        return(downloadButton("downloadData3",gettext("Download as png")))
      }
    })
    
    output$downloadData3 = downloadHandler(
      filename = function() { 
        paste('graphQuantiMCA','.png', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraphQuanti()$Plot)
      },
      contentType='image/png')
    
    output$download4 = renderUI({
      if(length(values()$choixquant)==0){
        return()
      }
      else{
        return(downloadButton("downloadData4",gettext("Download as jpg")))
      }
    })
    
    output$downloadData4 = downloadHandler(
      filename = function() { 
        paste('graphQuantiMCA','.jpg', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraphQuanti()$Plot)
      },
      contentType='image/jpg')
    
    
    output$download5 = renderUI({
      if(length(values()$choixquant)==0){
        return()
      }
      else{
        return(downloadButton("downloadData5",gettext("Download as pdf")))
      }
    })
    
    output$downloadData5 = downloadHandler(
      filename = function() { 
        paste('graphQuantiMCA','.pdf', sep='') 
      },
      content = function(file) {
        ggplot2::ggsave(file,codeGraphQuanti()$Plot)
      },
      contentType=NA)    
        
    observe({
      if(input$MCAcode!=0){
        isolate({
          if (length(input$habiller)==2 & input$color_point==gettext("qualitative variable")) cat(values()$codeMCAp,sep="\n")
          cat(values()$codeMCA,sep="\n")
          cat(codeGraphVar()$Code,sep="\n")
          cat(codeGraphInd()$Code,sep="\n")
          if((length(values()$choixquant)!=0)) cat(codeGraphQuanti()$Code,sep="\n") 
        })
      }
    })
    
    observe({
      if(input$Quit!=0){
        isolate({
	     res <- list()
         res$nomDataMCAshiny <- nomDataMCAshiny
         res$data <- newdataMCAshiny
        if (length(QuantiChoiceMCAshiny)==1){
          if(input$supquanti==FALSE){
            quantiMCAshiny=NULL
          } else{
            quantiMCAshiny=QuantiChoiceMCAshiny
          }
        }
      else{
        if(length(input$supquanti)==0){
          quantiMCAshiny=NULL
        }
        else{
          quantiMCAshiny=input$supquanti
        }
      }
      res$b=quantiMCAshiny
     
      res$c=input$supvar
      res$z=input$var_sup
      res$y=input$ind_var
      res$lab=input$indvarpoint
      res$d=input$indsup
      
      res$e=input$nb1
      res$f=input$nb2
      
      hab <- "none"
      if(length(input$supvar)>1 & length(input$habiller)>=1){
        if (length(input$habiller)==1) hab <- as.character(input$habiller)
        if (length(input$habiller)==2) hab <- ncol(values()$res.MCA$call$X)
      }

      if (length(input$supvar)==1) hab=values()$choixqual
 
      res$g=hab
      
      if(input$select==gettext("Manual")){
        selecindiv=input$indiv 
      }
      else if(input$select=="cos2"){
        selecindiv=input$slider1
      }
      else if(input$select=="Contrib"){
        selecindiv=input$sliderContrib 
        }
      else if(input$select==gettext("No selection")){
        selecindiv=NULL
      }
      res$h=input$select
      res$i=selecindiv
    
    if(input$selectMod=="cos2"){
      selecMod=input$sliderCosMod
    }
    else if(input$selectMod=="Contrib"){
      selecMod=input$slider4
    }
    else if(input$selectMod==gettext("No selection")){
      selecMod=NULL
    }
    res$j=input$selectMod
    res$k=selecMod
    res$codeMCA=values()$codeMCA
    res$codeGraphVar=codeGraphVar()$Code
    res$codeGraphInd=codeGraphInd()$Code
    if((length(values()$choixquant)!=0)) res$codeGraphQuanti=codeGraphQuanti()$Code 
    res$title1MCAshiny=input$title1MCAshiny
    res$title2MCAshiny=input$title2MCAshiny
    res$title3MCAshiny=input$title3MCAshiny
    res$anafact=values()$res.MCA
    res$color1MCAshiny=input$colindact
    res$color2MCAshiny=input$colindsup
	res$habillageindMCAshiny <- input$habiller
    # res$color3MCAshiny=input$colvaract
    # res$color4MCAshiny=input$colvarsup1
    res$color5MCAshiny=input$colvaract1
    res$color6MCAshiny=input$colvarsup1
    res$color7MCAshiny=input$colquanti
    res$color8MCAshiny=input$colli
    res$color_point=input$color_point
    res$color_Mod=input$color_Mod
    res$hcpcparam <- input$hcpcparam
    res$nbdimclustMCAshiny <- input$nbDimClustering
	res$poids1MCAshiny <- poids1MCAshiny
    class(res)<-"MCAshiny"

          stopApp(returnValue=res)
        })
      }
    })
  }
