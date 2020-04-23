# server script for MCA2

  function(input, output,session) {

  output$NB1 <- renderUI({
    validate(
      need(length(VariableChoicesMCAshiny)-length(input$supvar)>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    if((length(VariableChoicesMCAshiny)-length(input$supvar))>5){
       return(textInput("nb1", label = NULL, axe1MCAshiny,width='41px'))
    } else{
       return(selectInput("nb1",label=NULL, choices=1:(length(VariableChoicesMCAshiny)-length(input$supvar)),selected=axe1MCAshiny,width='41px'))
    }
  })
  
  output$NB2=renderUI({
    validate(
      need((length(VariableChoicesMCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny"))
    )
    if((length(VariableChoicesMCAshiny)-length(input$supvar))>5){
       return(textInput("nb2", label = NULL, axe2MCAshiny,width='41px'))
    } else{
       return(selectInput("nb2",label=NULL, choices=(1:(length(VariableChoicesMCAshiny)-length(input$supvar))),selected=axe2MCAshiny,width='41px'))
    }
  })
  
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
        return(tags$div( 
            div(gettext("Number of dimensions kept for clustering",domain="R-Factoshiny"), style="display: inline-block; padding: 0px 0px 0px 0px"),
		    div(numericInput(inputId = "nbDimClustering", label = NULL,value=if(is.null(nbdimclustMCAshiny)){5} else {nbdimclustMCAshiny},min=1), style="display: inline-block;width: 70px; padding: 0px 0px 0px 10px"))
		)
    }
  })

  values <- reactive({
     if (length(input$habiller)==2 & input$color_point==gettext("2 qualitative variables",domain="R-Factoshiny")) return(isolate(valeur()))
	 if (length(input$nb1)>0){
	   if (max(input$nb1,input$nb2)>5) return(isolate(valeur()))
	 }
	 if (length(input$nbDimClustering)>0){
	   if (input$nbDimClustering >5) return(isolate(valeur()))
	 }
     if (length(input$mcaparam)==0){
	   return(valeur())
	 } else {
        if (input$submit>=0) isolate(valeur())
     }
 })

    #Realisation de l'ACM    
    valeur=function(){
      
    NomCol <- colnames(newdataMCAshiny)
	SuppressCol <- NULL
    if (length(QuantiChoiceMCAshiny)!=0){
	  if (length(QuantiChoiceMCAshiny)!=length(input$supquanti)) {
	    SuppressCol <- which(NomCol%in%setdiff(QuantiChoiceMCAshiny,input$supquanti))
	    NomCol <- NomCol[-SuppressCol]
	  }
	}
    QualiSup <- which(NomCol%in%input$supvar)
	QuantiSup <- which(NomCol%in%input$supquanti)
	if (length(QuantiSup)==0) QuantiSup <- NULL
	if (length(QualiSup)==0) QualiSup <- NULL

    if(length(input$indsup)==0){
      suple <- NULL
    } else{
      suple <- which(nomMCAshiny%in%input$indsup)
    }
	codeMCA <- NULL
	nomTabDon <- paste0(nomDataMCAshiny, if (!is.null(SuppressCol)){paste0("[,-c(",paste0(SuppressCol,collapse=","),")]")})
    if (length(input$habiller)==2 && input$color_point==gettext("2 qualitative variables",domain="R-Factoshiny")){
	  codeMCA <- paste0("dfaux <- data.frame(",nomTabDon,",",paste0(input$habiller[1],"_",input$habiller[2]),"=paste0(",nomDataMCAshiny,"[,'",input$habiller[1],"'],",nomDataMCAshiny,"[,'",input$habiller[2],"']))\n")
	  QualiSup <- c(QualiSup,length(NomCol)+1)
	}

	boolImpute <- FALSE
    if(length(input$impute>0)){
      # validate(
        # need(length(input$supvar)==0 | input$impute==gettext("Consider NA as new category",domain="R-Factoshiny"),gettext("No supplementary variables are allowed with imputation methods",domain="R-Factoshiny")),
		# need(length(input$supquanti)==0 | input$impute==gettext("Consider NA as new category",domain="R-Factoshiny"), gettext("No supplementary variables are allowed with imputation methods",domain="R-Factoshiny"))
      # )
	 if (input$impute!=gettext("Consider NA as new category",domain="R-Factoshiny")){
	  boolImpute <- TRUE
	  if (input$impute==gettext("Impute with k-dimensional MCA-model (estime k, time consuming)",domain="R-Factoshiny")){
 	    codeMCA <- paste0(codeMCA,"nb <- estim_ncpMCA(",if (length(input$habiller)==2){"dfaux"} else {nomTabDon},if (!is.null(QuantiSup)) paste0(",quanti.sup=c(",paste0(QuantiSup,collapse=","),")"),if (!is.null(QualiSup)) paste0(",quali.sup=c(",paste0(QualiSup,collapse=","),")"),if (!is.null(suple)) paste0(",ind.sup=c(",paste0(suple,collapse=","),")"),")$ncp\n")
	    Nbncp <- "nb"
	  }
      if (input$impute==gettext("Impute with the proportions",domain="R-Factoshiny")) Nbncp <- 0
      if (input$impute==gettext("Impute with 2-dimensional MCA-model (good compromise)",domain="R-Factoshiny")) Nbncp <- 2
	  codeMCA <- paste0(codeMCA, "dfcompleted <- missMDA::imputeMCA(",if (length(input$habiller)==2){"dfaux"} else {nomTabDon},",ncp=",Nbncp,if (!is.null(QuantiSup)) paste0(",quanti.sup=c(",paste0(QuantiSup,collapse=","),")"),if (!is.null(QualiSup)) paste0(",quali.sup=c(",paste0(QualiSup,collapse=","),")"),if (!is.null(suple)) paste0(",ind.sup=c(",paste0(suple,collapse=","),")"),")\n")
	 }
    }
	codeMCA <- paste0(codeMCA,"res.MCA<-MCA(",if (length(input$habiller)==2  && input$color_point==gettext("2 qualitative variables",domain="R-Factoshiny")){"dfaux"} else {nomTabDon})
	if (boolImpute) codeMCA <- paste0(codeMCA,",tab.disj = dfcompleted$tab.disj")
	codeMCA <- paste0(codeMCA,if(max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))!=5) paste0(",ncp=",max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))),if(!is.null(QuantiSup)) paste0(",quanti.sup=c(",paste(QuantiSup,collapse=","),")"),if(!is.null(QualiSup)) paste0(",quali.sup=c(",paste(QualiSup,collapse=","),")"),if(!is.null(suple)) paste0(",ind.sup=c(",paste(suple,collapse=","),")"),if (!is.null(poids1MCAshiny)) paste0(",row.w=c(",paste(poids1MCAshiny,collapse=","),")"),",graph=FALSE)")
    list(res.MCA=eval(parse(text=codeMCA)), codeMCA=codeMCA)
  }

    output$col1=renderUI({
      if(length(input$indsup)>0){
       return(tags$div(
        div(colourpicker::colourInput("colindsup", label=NULL, value = if (!is.null(input$colindsup)){if (input$colindsup!="blue") input$colindsup  else{color2MCAshiny}} else{color2MCAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		div(gettext("supplementary individuals",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px")))
      }
    })
    
    output$choixindvar=renderUI({
      choix=list(gettext("Individuals",domain="R-Factoshiny"),gettext("Categories",domain="R-Factoshiny"))
      if(length(input$indsup)>0) choix <- c(choix,gettext("Supplementary individuals",domain="R-Factoshiny"))
      if(length(input$supvar)>0) choix <- c(choix,gettext("Supplementary categories",domain="R-Factoshiny"))
      div(align="left",checkboxGroupInput("ind_var",gettext("Points to draw",domain="R-Factoshiny"), choices=choix, selected = indvarMCAshiny))
    })
    
    output$pointlabel=renderUI({
      choix=list()
      if (!is.null(input$ind_var)) reponse <- input$ind_var
	  else reponse <- c(gettext("Individuals",domain="R-Factoshiny"),gettext("Categories",domain="R-Factoshiny"))
      if(sum(gettext("Individuals",domain="R-Factoshiny")==reponse)==1) choix=c(choix,gettext("Individuals",domain="R-Factoshiny"))
      if(sum(gettext("Categories",domain="R-Factoshiny")==reponse)==1) choix=c(choix,gettext("Categories",domain="R-Factoshiny"))
      if(sum(gettext("Supplementary individuals",domain="R-Factoshiny")==reponse)==1) choix=c(choix,gettext("Supplementary individuals",domain="R-Factoshiny"))
      if(sum(gettext("Supplementary categories",domain="R-Factoshiny")==reponse)==1) choix=c(choix,gettext("Supplementary categories",domain="R-Factoshiny"))
      div(align="left",checkboxGroupInput("indvarpoint",gettext("Labels for",domain="R-Factoshiny"),choices=choix,selected=choixLabelInit))
    })
    
    output$out22=renderUI({
      choix=list(gettext("Summary of outputs",domain="R-Factoshiny"),gettext("Eigenvalues",domain="R-Factoshiny"),gettext("Results of the variables",domain="R-Factoshiny"),gettext("Results of the individuals",domain="R-Factoshiny"))
      if(length(input$indsup)>0) choix=c(choix,gettext("Results of the supplementary individuals",domain="R-Factoshiny"))
      if(length(input$supquanti)>0) choix=c(choix,gettext("Results of the supplementary quantitative variables",domain="R-Factoshiny"))
      if(length(input$supvar)>0) choix=c(choix,gettext("Results of the supplementary categorical variables",domain="R-Factoshiny"))
      radioButtons("out",gettext("Which outputs do you want?",domain="R-Factoshiny"), choices=choix,selected=gettext("Summary of outputs",domain="R-Factoshiny"),inline=TRUE)
    })
    
    output$colquanti12=renderUI({
       return(tags$div( 
        div(colourpicker::colourInput("colquanti", label=NULL, if (!is.null(input$colquanti)) input$colquanti else{color7MCAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		div(gettext("supplementary quantitative variables",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px")))
    })
    
    output$colquantib=renderUI({
      if(length(input$supquanti)>0){
       return(tags$div( 
        div(colourpicker::colourInput("colli", label=NULL, if(!is.null(input$colli)){if (input$colli!="blue") input$colli} else{color8MCAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		div(gettext("supplementary quantitative variables",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px")))
      }
    })
	    
  output$imputeData <- renderUI({
    if(any(is.na(newdataMCAshiny[,qualiMCAshiny]))){
	  return(radioButtons("impute",gettext("Handling missing values",domain="R-Factoshiny"),choices=list(gettext("Consider NA as new category",domain="R-Factoshiny"),gettext("Impute with the proportions",domain="R-Factoshiny"),gettext("Impute with 2-dimensional MCA-model (good compromise)",domain="R-Factoshiny"),gettext("Impute with k-dimensional MCA-model (estime k, time consuming)",domain="R-Factoshiny")),selected=gettext("Consider NA as new category",domain="R-Factoshiny")))
	} else {
      return(tags$div(tags$label(class="control-label", gettext("Handling missing values",domain="R-Factoshiny")),
	   tags$div(HTML(gettext("No missing values",domain="R-Factoshiny")))))
	}
  })

    codeGraphVar <- reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny")),
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
		need(length(input$var_sup)>0, gettext("Select at least 1 choice",domain="R-Factoshiny"))
      )
      inv<-c()
      if(sum(gettext("Supplementary qualitative variables",domain="R-Factoshiny")==input$var_sup)==0) inv<-c(inv,"'quali.sup'")
      if(sum(gettext("Supplementary quantitative variables",domain="R-Factoshiny")==input$var_sup)==0) inv<-c(inv,"'quanti.sup'")
      if(sum(gettext("Active qualitative variables",domain="R-Factoshiny")==input$var_sup)==0) inv<-c(inv,"'var'")
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
      if(length(input$supvar)>0){
       return(tags$div( 
        div(colourpicker::colourInput("colvarsup1", label=NULL, if (!is.null(input$colvarsup1)) input$colvarsup1 else{color6MCAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		div(gettext("supplementary categorical variables",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px")))
        # if(is.null(color6MCAshiny)){
          # return(colourpicker::colourInput("colvarsup1",gettext("supplementary categorical variables"),"darkgreen",allowTransparent=TRUE))
        # }else{
          # return(colourpicker::colourInput("colvarsup1",gettext("supplementary categorical variables"),color6MCAshiny,allowTransparent=TRUE))
        # }
      }
    })
    
    observe({
      updateCheckboxInput(session, "drawconf", value = FALSE)
    })

    output$ellips=renderUI({
      if(length(input$habiller)>0 & !(input$color_point == gettext("active/supplementary",domain="R-Factoshiny") || input$color_point == "cos2" || input$color_point == "contribution"))  return(checkboxInput("drawconf",gettext("Draw confidence ellipses around center of caregories",domain="R-Factoshiny"),if (length(input$drawconf)==0){drawconfInit}else{input$drawconf}))
      else return(NULL)
    })

    codeGraphInd <- reactive({
      
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny")),
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(input$ind_var)>=1,gettext("Please select the objects you want to plot: Individuals, categories or both",domain="R-Factoshiny")),
        need(length(input$habiller)<=2,gettext("Please select maximum 2 variables as habillage",domain="R-Factoshiny"))
      )
      
      inv<-c()
      if(sum(gettext("Individuals",domain="R-Factoshiny")==input$ind_var)==0)  inv<-c(inv,"'ind'")
      if(sum(gettext("Categories",domain="R-Factoshiny")==input$ind_var)==0) inv<-c(inv,"'var'")
      if(length(input$supvar)>0 & sum(gettext("Supplementary categories",domain="R-Factoshiny")==input$ind_var)==0) inv<-c(inv,"'quali.sup'")
      if(length(input$indsup)>0 & sum(gettext("Supplementary individuals",domain="R-Factoshiny")==input$ind_var)==0) inv<-c(inv,"'ind.sup'")
	  if(length(inv)>1) vecinv<-paste0("c(",paste0(inv,collapse=","),")")
      if(length(inv)==1) vecinv <- inv
      if(length(inv)==0) vecinv<-"NULL"
      selecindivText="NULL"
      if(input$select==gettext("Manual",domain="R-Factoshiny")) selecindivText <- paste0("c('",paste0(input$indiv,collapse="','"),"')")
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
      if(input$color_point == gettext("quantitative variable",domain="R-Factoshiny")) hab <- which(colnames(values()$res.MCA$call$X)==input$habiller)
      if(input$color_point == "cos2") hab <- "'cos2'"
      if(input$color_point == "contribution") hab <- "'contrib'"
      if(input$color_point==gettext("1 qualitative variable",domain="R-Factoshiny")) hab <- which(colnames(values()$res.MCA$call$X)==input$habiller)
      if(input$color_point==gettext("2 qualitative variables",domain="R-Factoshiny")) hab <- ncol(values()$res.MCA$call$X)
	  if (length(hab)==0) hab <- "none"

      label <- c()
	  keepn=FALSE
      if(sum(gettext("Individuals",domain="R-Factoshiny")==input$indvarpoint)==1) {
	    label=c(label,"'ind'")
		keepn=TRUE   # useful if plotellipses
	  }
      if(sum(gettext("Categories",domain="R-Factoshiny")==input$indvarpoint)==1) label=c(label,"'var'")
      if(sum(gettext("Supplementary individuals",domain="R-Factoshiny")==input$indvarpoint)==1) label=c(label,"'ind.sup'")
      if(sum(gettext("Supplementary categories",domain="R-Factoshiny")==input$indvarpoint)==1) label=c(label,"'quali.sup'")
	  if (length(label)==0) label <- "'none'"
	  else label <- paste0("c(",paste0(label,collapse=","),")")
	  # if (label == input$indvarpoint) label <- NULL
      
      validate(
        need(length(input$ind_var)!="",gettext("Please select which object you would like to print",domain="R-Factoshiny"))
      )
      if(input$eachvar==TRUE){
        colouract2 <- rep(1:length(values()$res.MCA$call$quali),sapply(values()$res.MCA$call$X[,values()$res.MCA$call$quali],nlevels))
		colouract2 <- paste0("c(",paste0(colouract2,collapse=','),")")
        if (!is.null(values()$res.MCA$call$quali.sup)) {
		  coloursup2 <- rep((1+length(values()$res.MCA$call$quali)) : (length(values()$res.MCA$call$quali)+length(values()$res.MCA$call$quali.sup)),sapply(values()$res.MCA$call$X[,values()$res.MCA$call$quali.sup],nlevels))
		  coloursup2 <- paste0("c(",paste0(coloursup2,collapse=','),")")
		}
	  }
	  if(!is.null(input$drawconf)&&input$drawconf==TRUE&& (length(input$habiller)>0 & !(input$color_point == gettext("active/supplementary",domain="R-Factoshiny") || input$color_point == "cos2" || input$color_point == "contribution"))){
		Code <- paste0(paste0("plotellipses(res.MCA,keepvar=",hab),if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (selecindivText!="NULL") paste(",select=",selecindivText),if(input$colindact!="#000000") paste0(",col.ind='",input$colindact,"'"), if(!is.null(input$colindsup)) {if(input$colindsup!="blue") paste0(",col.ind.sup='",input$colindsup,"'")},if(input$title1MCAshiny!="MCA factor map") paste0(',title="',input$title1MCAshiny,'"'),if (input$cex!=1) paste0(",cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex),if (keepn==FALSE) paste0(",label ='quali'"),")")
      } else {
		Code <- paste0("plot.MCA(res.MCA",if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (vecinv!="NULL") paste(",invisible=",vecinv),if (selecindivText!="NULL") paste(",select=",selecindivText),if (selecModText!="NULL") paste(",selectMod=",selecModText),if (hab != "none" & length(hab)!=0) paste0(",habillage=",hab),if(input$colindact!="#000000") paste0(",col.ind='",input$colindact,"'"),if(input$eachvar==TRUE & hab!="'cos2'" & hab!="'contrib'") paste0(",col.var=",colouract2),if(input$eachvar==TRUE & hab!="'cos2'" & hab!="'contrib'" & !is.null(values()$res.MCA$call$quali.sup)) paste0(",col.quali.sup=",coloursup2), if(input$colvaract1!="#FF0000" & input$eachvar==FALSE) paste0(",col.var='",input$colvaract1,"'"), if(!is.null(input$colindsup)) {if(input$colindsup!="blue") paste0(",col.ind.sup='",input$colindsup,"'")},if(!is.null(input$colvarsup1)& input$eachvar!=TRUE) {if(input$colvarsup1!="darkgreen") paste0(",col.quali.sup='",input$colvarsup1,"'")},if(input$title1MCAshiny!="MCA factor map") paste0(',title="',input$title1MCAshiny,'"'),if (input$cex!=1) paste0(",cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex),if (!is.null(label)) paste0(",label =",label),")")
	  }
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
      if (length(QuantiChoiceMCAshiny)>0 & length(input$supquanti)>0) {
	    Code <- paste0("plot.MCA(res.MCA, choix='quanti.sup'",if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (input$title3MCAshiny!="Supplementary variables on the MCA map") paste0(',title="',input$title3MCAshiny,'"'),if(input$colquanti!="#0000FF") paste0(",col.quanti.sup='",input$colquanti,"'"), if (input$cex3!=1) paste0(",cex=",input$cex3,",cex.main=",input$cex3,",cex.axis=",input$cex3),")")
		res.MCA <- values()$res.MCA
	    Plot <- eval(parse(text=Code))
	    return(list(Code=Code,Plot=Plot))
      }
    }
    
    output$map2 <- renderPlot({
      if (!is.null(codeGraphQuanti()$Plot)) p <- print(codeGraphQuanti()$Plot)
    })  
    
    output$map22=renderUI({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny")),
        need( (length(VariableChoicesMCAshiny)-length(input$supvar))>2,gettext("Please more active variables",domain="R-Factoshiny"))
      )
      
      if(length(QuantiChoiceMCAshiny)==0 || length(input$supquanti)==0){
        return(p())
      }
      else{
        column(width = 6,shinyjqui::jqui_resizable(plotOutput("map2", height="500")),
           br(),
           p(gettext("Download as"),downloadButton("downloadData4",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData3",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData5",gettext("pdf",domain="R-Factoshiny")),align="center")
		)
	  }
    })
    
    output$choixchange=renderUI({
      if(length(QuantiChoiceMCAshiny)==0 || length(input$supquanti)==0){
        return(radioButtons("MCAgraph",gettext("Which graph do you want to modify?",domain="R-Factoshiny"),
                   choices=list(gettext("Individuals and categories",domain="R-Factoshiny"),"Variables"="var"),inline=TRUE))
      } else{
        return(radioButtons("MCAgraph",gettext("Which graph do you want to modify?",domain="R-Factoshiny"),
                  choices=list(gettext("Individuals and categories",domain="R-Factoshiny"),"Variables"="var",gettext("Quantitative variables",domain="R-Factoshiny")),inline=TRUE))
      }
    })
    
    output$habillage2=renderUI({
      if (input$color_point == gettext("1 qualitative variable",domain="R-Factoshiny")){
        return(selectizeInput("habiller",gettext("select the variable",domain="R-Factoshiny"), choices=qualiMCAshiny, multiple=FALSE, selected=habillageindMCAshiny))
      } else {
        if (input$color_point == gettext("2 qualitative variables",domain="R-Factoshiny")){
         return(selectizeInput("habiller",gettext("select 2 variables",domain="R-Factoshiny"), choices=qualiMCAshiny, multiple=TRUE, selected=habillageindMCAshiny))
        } else {return(NULL)}
      }
    }) 
        
    output$slider3=renderUI({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2,gettext("Please select more variables",domain="R-Factoshiny"))
      )
      # maxvar=nrow(values()$res.MCA$var$coord)
     maxvar=length(VariableChoicesMCAshiny)-length(input$supvar)
      if(selection3MCAshiny=="Contrib"){return(sliderInput("slider4",label="Contribution",
                                                   min=1,max=maxvar,value=as.numeric(selection4MCAshiny),step=1)) }
      else{
        return(sliderInput("slider4",label="Contribution",
                           min=1,max=maxvar,value=maxvar,step=1))
      }
    })
    
    output$summary=renderPrint({
      summary(newdataMCAshiny)
    })
    
    output$map3=renderPlot({
      print(ggplot2::ggplot(cbind.data.frame(x=1:nrow(values()$res.MCA$eig),y=values()$res.MCA$eig[,2])) + ggplot2::aes(x=x, y=y)+ ggplot2::geom_col(fill="blue") + ggplot2::xlab("Dimension") + ggplot2::ylab(gettext("Percentage of variance",domain="R-Factoshiny")) + ggplot2::ggtitle(gettext("Decomposition of the total inertia",domain="R-Factoshiny")) + ggplot2::theme_light() + ggplot2::theme(plot.title = ggplot2::element_text(hjust =0.5))  + ggplot2::scale_x_continuous(breaks=1:nrow(values()$res.MCA$eig)))
      # return(barplot(values()$res.MCA$eig[,1],names.arg=rownames(values()$res.MCA$eig),las=2,density=TRUE))
    })
    
    output$histo=renderPlot({
      ggplot2::ggplot(newdataMCAshiny) + aes(x=newdataMCAshiny[,input$bam]) + geom_bar() + labs(y="Count",x="")
      # barplot(prop.table(table(newdataMCAshiny[,input$bam]))*100)
    })
        
    output$summaryMCA=renderPrint({
      validate(
        need(input$nbele!=0, gettext("Please select at least one element",domain="R-Factoshiny"))
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
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.MCA$var$coord))
    },rownames=TRUE)
    
    output$sorties3=renderTable({
      
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.MCA$var$contrib))
    },rownames=TRUE)
    
    output$sorties4=renderTable({
      
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.MCA$var$cos2))
    },rownames=TRUE)
    
    output$sorties22=DT::renderDataTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny"))
      )
      tab<-as.data.frame(values()$res.MCA$ind$coord)
      tab<-round(tab, 3)
      tab<-cbind(Names=rownames(tab),tab)
      return(tab)
    })
    
    output$sorties33=DT::renderDataTable({
      
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny"))
      )
      tab1<-as.data.frame(values()$res.MCA$ind$contrib)
      tab1<-round(tab1,3)
      tab1<-cbind(Names=rownames(tab1),tab1)
      return(tab1)
    })
    
    output$sorties44=DT::renderDataTable({
      
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny"))
      )
      tab2<-as.data.frame(values()$res.MCA$ind$cos2)
      tab2<-round(tab2,3)
      tab2<-cbind(Names=rownames(tab2),tab2)
      return(tab2)
    })
    
    output$sorties22s=DT::renderDataTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny"))
      )
      tab<-as.data.frame(values()$res.MCA$ind.sup$coord)
      tab<-round(tab, 3)
      tab<-cbind(Names=rownames(tab),tab)
      return(tab)
    })
    
    output$sorties44s=DT::renderDataTable({
      
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny"))
      )
      tab2<-as.data.frame(values()$res.MCA$ind.sup$cos2)
      tab2<-round(tab2,3)
      tab2<-cbind(Names=rownames(tab2),tab2)
      return(tab2)
    })

    output$sorties23=renderTable({
      
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(input$supvar)!=0, gettext("No supplementary categorical variables"))
      )
      return(as.data.frame(values()$res.MCA$quali.sup$coord))
    })
    
    output$sorties232=renderTable({
      
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(input$supvar)!=0, gettext("No supplementary categorical variables",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.MCA$quali.sup$cos2))
    },rownames=TRUE)
    
    output$sorties233=renderTable({
      
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variable",domain="R-Factoshiny")),
        need(length(input$supvar)!=0, gettext("No supplementary categorical variables",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.MCA$quali.sup$v.test))
    },rownames=TRUE)
    
    output$sorties43=renderTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2,gettext("Please select more variable",domain="R-Factoshiny")),
        need(length(input$supquanti)!=0 || input$supquanti==TRUE, gettext("No supplementary quantitative variables",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.MCA$quanti.sup$coord))
    },rownames=TRUE)
    
    output$sortiesIsupC=renderTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(input$indsup)!=0,gettext("No supplementary individuals",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.MCA$ind.sup$coord))
    })
    
    output$sortiesIsupCos=renderTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(input$indsup)!=0,gettext("No supplementary individuals",domain="R-Factoshiny"))
      )
      return(as.data.frame(values()$res.MCA$ind.sup$cos2))
    },rownames=TRUE)
    
  CalculDimdesc <- reactive({
    validate(
      need((length(VariableChoicesMCAshiny)-length(input$supvar))>1 ,gettext("Please select at least two active variables",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
	)
    return(dimdesc(values()$res.MCA,proba = if (length(input$pvalueDimdesc)!=0) {input$pvalueDimdesc} else {0.05}))
  })
    #DIM1
    
    output$sortieDimdesc=renderTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(CalculDimdesc()[[1]]$category)>0,gettext("No category describes axis 1",domain="R-Factoshiny")),
        need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
      )
      return(as.data.frame(CalculDimdesc()[[1]]$category))
    },rownames=TRUE,digits=-3)
    
    output$sortieDimdesc2=renderTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(CalculDimdesc()[[1]]$quali)>0,gettext("No qualitative variable describes axis 1",domain="R-Factoshiny")),
        need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
      )
      return(as.data.frame(CalculDimdesc()[[1]]$quali))
    },rownames=TRUE,digits=-3)
    output$sortieDimdesc3=renderTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(input$supquanti)>0,gettext("No quantitative variable",domain="R-Factoshiny")),
        need(length(CalculDimdesc()[[1]]$quanti)!=0,gettext("No quantitative variable describes axis 1",domain="R-Factoshiny")),
        need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
      )
      return(as.data.frame(CalculDimdesc()[[1]]$quanti))
    },rownames=TRUE,digits=-3)
    
    #DIM2
    output$sortieDimdesc00=renderTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(CalculDimdesc()[[2]]$category)>0,gettext("No category describes axis 2",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
      )
      return(as.data.frame(CalculDimdesc()[[2]]$category))
    },rownames=TRUE,digits=-3)
    output$sortieDimdesc22=renderTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(CalculDimdesc()[[2]]$quali)>0,gettext("No qualitative variable describes axis 2",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
      )
      return(as.data.frame(CalculDimdesc()[[2]]$quali))
    },rownames=TRUE,digits=-3)
    output$sortieDimdesc33=renderTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(CalculDimdesc()[[2]]$quanti)!=0,gettext("No quantitative variable describes axis 2",domain="R-Factoshiny")),
        need(length(input$supquanti)>0,gettext("No quantitative variable",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
	  )
      return(as.data.frame(CalculDimdesc()[[2]]$quanti))
    },rownames=TRUE,digits=-3)
    
    #DIM3
    output$sortieDimdesc000=renderTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(CalculDimdesc()[[3]]$category)>0,gettext("No category describes axis 3",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
      )
      return(as.data.frame(CalculDimdesc()[[3]]$category))
    },rownames=TRUE,digits=-3)
    output$sortieDimdesc222=renderTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(CalculDimdesc()[[3]]$quali)>0,gettext("No qualitative variable describes axis 3",domain="R-Factoshiny")),
      need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
      )
      return(as.data.frame(CalculDimdesc()[[3]]$quali))
    },rownames=TRUE,digits=-3)
    output$sortieDimdesc333=renderTable({
      validate(
        need((length(VariableChoicesMCAshiny)-length(input$supvar))>2 ,gettext("Please select more variables",domain="R-Factoshiny")),
        need(length(CalculDimdesc()[[3]]$quanti)!=0,gettext("No quantitative variable describes axis 3",domain="R-Factoshiny")),
        need(length(input$supquanti)>0,gettext("No quantitative variable",domain="R-Factoshiny")),
        need(input$pvalueDimdesc>0,gettext("P-value should be strictly greater than 0",domain="R-Factoshiny"))
	  )
      return(as.data.frame(CalculDimdesc()[[3]]$quanti))
    },rownames=TRUE,digits=-3)
    
    output$JDD=DT::renderDataTable({
      cbind(Names=rownames(newdataMCAshiny),newdataMCAshiny)},      
      options = list("orderClasses" = TRUE, "responsive" = TRUE, "pageLength" = 10), rownames=FALSE)
    ####
  observe({
    if(input$Investigatehtml!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveMCAshiny)
        if (substr(tolower(input$choixLANG),1,2)=="fr") FactoInvestigate::Investigate(values()$res.MCA,codeGraphInd = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {paste0(values()$codeMCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {codeGraphVar()$Code} else {NULL}, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "fr")
        else FactoInvestigate::Investigate(values()$res.MCA,codeGraphInd = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {paste0(values()$codeMCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {codeGraphVar()$Code} else {NULL}, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "en")
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$Investigatedoc!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveMCAshiny)
        if (substr(tolower(input$choixLANG),1,2)=="fr") FactoInvestigate::Investigate(values()$res.MCA,codeGraphInd = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {paste0(values()$codeMCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {codeGraphVar()$Code} else {NULL},document="word_document",openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "fr")
        else FactoInvestigate::Investigate(values()$res.MCA,codeGraphInd = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {paste0(values()$codeMCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {codeGraphVar()$Code} else {NULL},document="word_document",openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "en")
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$InvestigateRmd!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveMCAshiny)
	    if (substr(tolower(input$choixLANG),1,2)=="fr") FactoInvestigate::Investigate(values()$res.MCA,codeGraphInd = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {paste0(values()$codeMCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {codeGraphVar()$Code} else {NULL}, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "fr")
	    else FactoInvestigate::Investigate(values()$res.MCA,codeGraphInd = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {paste0(values()$codeMCA,"\n",codeGraphInd()$Code)} else {NULL}, codeGraphVar = if (input$choixGRAPH==gettext("Graphs done",domain="R-Factoshiny")) {codeGraphVar()$Code} else {NULL}, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language= "en")
	    print(paste0(gettext("The file ",domain="R-Factoshiny"),input$titleFile,gettext(" as well as the RData objects are available in the sub-directory: ",domain="R-Factoshiny"),getwd()))
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
      if(length(input$supquanti)==0){
        return()
      }
      else{
        return(downloadButton("downloadData3",gettext("Download as png",domain="R-Factoshiny")))
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
      if(length(input$supquanti)==0){
        return()
      }
      else{
        return(downloadButton("downloadData4",gettext("Download as jpg",domain="R-Factoshiny")))
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
      if(length(input$supquanti)==0){
        return()
      }
      else{
        return(downloadButton("downloadData5",gettext("Download as pdf",domain="R-Factoshiny")))
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
        
    output$CodePrinted <- renderPrint({
       if (input$MCAcode!=0){
          cat(values()$codeMCA,sep="\n")
          cat(codeGraphVar()$Code,sep="\n")
          cat(codeGraphInd()$Code,sep="\n")
          if((length(input$supquanti)!=0)) cat(codeGraphQuanti()$Code,sep="\n") 
       }
    })

    output$CodePrintedDimdesc <- renderPrint({
       if (input$MCAcode!=0){
        cat(values()$codeMCA,sep="\n")
        cat("dimdesc(res.MCA)",sep="\n")
       }
    })

    output$CodePrintedSummary <- renderPrint({
       if (input$MCAcode!=0){
        cat(values()$codeMCA,sep="\n")
        cat("summary(res.MCA)",sep="\n")
       }
    })

    # observe({
      # if(input$MCAcode!=0){
        # isolate({
          # cat(values()$codeMCA,sep="\n")
          # cat(codeGraphVar()$Code,sep="\n")
          # cat(codeGraphInd()$Code,sep="\n")
          # if((length(input$supquanti)!=0)) cat(codeGraphQuanti()$Code,sep="\n") 
        # })
      # }
    # })
    
    observe({
      if(input$Quit!=0){
        isolate({
	     res <- list()
         res$nomDataMCAshiny <- nomDataMCAshiny
         res$data <- newdataMCAshiny
        res$b=input$supquanti
     
      res$c=input$supvar
      res$z=input$var_sup
      res$y=input$ind_var
      res$choixLabelInit=input$indvarpoint
      res$d=input$indsup
      
      res$e=input$nb1
      res$f=input$nb2
      
      hab <- "none"
      if (length(input$habiller)==1) hab <- as.character(input$habiller)
      if (length(input$habiller)==2) hab <- ncol(values()$res.MCA$call$X)
 
      res$g=hab
      
      if(input$select==gettext("Manual",domain="R-Factoshiny")){
        res$i=input$indiv 
      }
      else if(input$select=="cos2"){
        res$i=input$slider1
      }
      else if(input$select=="Contrib"){
        res$i=input$sliderContrib 
      }
      res$h=input$select
    
    if(input$selectMod=="cos2"){
      res$k=input$sliderCosMod
    }
    else if(input$selectMod=="Contrib"){
      res$k=input$slider4
    }
    res$j=input$selectMod
    res$codeMCA=values()$codeMCA
    res$codeGraphVar=codeGraphVar()$Code
    res$codeGraphInd=codeGraphInd()$Code
    if((length(input$supquanti)!=0)) res$codeGraphQuanti=codeGraphQuanti()$Code 
    res$title1MCAshiny=input$title1MCAshiny
    res$title2MCAshiny=input$title2MCAshiny
    res$title3MCAshiny=input$title3MCAshiny
    res$anafact=values()$res.MCA
    res$color1MCAshiny=input$colindact
    res$color2MCAshiny=input$colindsup
	res$habillageindMCAshiny <- input$habiller
	res$drawconf <- input$drawconf
	res$valdefMCA <- input$eachvar
	res$ellips <- input$ellips
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
	if (length(input$pvalueDimdesc)) res$pvalueDimdescInit <- input$pvalueDimdesc
	else res$pvalueDimdescInit <- 0.05
    class(res)<-"MCAshiny"

          stopApp(returnValue=res)
        })
      }
    })
  }
