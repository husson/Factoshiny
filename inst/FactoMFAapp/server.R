# server2. AFM
  function(input, output) {

  codeMFA <- reactive({
	 if (max(input$nb1>5) | max(input$nb2>5)) return(isolate(valeur()))
	 if (length(input$nbDimClustering)>0){
	   if (input$nbDimClustering >5) return(isolate(valeur()))
	 }
     if (length(input$mfaparam)==0){
	   return(valeur())
	 } else {
        if (input$submit>=0) isolate(valeur())
		if (input$activemodif==gettext("Validate the groups")) isolate(valeur())
     }
 })

  valeur <- function(){
    gsup=c()
    groupe=length(input$variables1)
    if(input$typeG1==gettext("Quantitative")){
      if(input$scale1==gettext("Scaled")) typ="s"
      if(input$scale1==gettext("Unscaled")) typ="c"
    }
    else if(input$typeG1==gettext("Qualitative")){
      typ="n"
    }
    else if(input$typeG1==gettext("Frequencies")){
      typ="f"
    }
    if(input$typeG12==gettext("Supplementary")) gsup=c(gsup,1)
    types=typ
    nom=input$nameG1
    if(input$activeG2==TRUE && length(input$variables2)>0){
      groupe=c(groupe,length(input$variables2))
      if(input$typeG2==gettext("Quantitative")){
        if(input$scale2==gettext("Scaled")) typ="s"
        if(input$scale2==gettext("Unscaled")) typ="c"
      }
      else if(input$typeG2==gettext("Qualitative")){
        typ="n"
      }
      else if(input$typeG2==gettext("Frequencies")){
        typ="f"
      }
      if(input$typeG22==gettext("Supplementary")) gsup=c(gsup,2)
      types=c(types,typ)
      nom=c(nom,input$nameG2)
      if(input$activeG3==TRUE && length(input$variables3)>0){
        groupe=c(groupe,as.numeric(length(input$variables3)))
        if(input$typeG3==gettext("Quantitative")){
          if(input$scale3==gettext("Scaled")) typ="s"
          if(input$scale3==gettext("Unscaled")) typ="c"
        }
        else if(input$typeG3==gettext("Qualitative")){
          typ="n"
        }
        else if(input$typeG3==gettext("Frequencies")){
          typ="f"
        }
        if(input$typeG32==gettext("Supplementary")) gsup=c(gsup,3)
        types=c(types,typ)
        nom=c(nom,input$nameG3)
        if(input$activeG4==TRUE && length(input$variables4)>0){
          groupe=c(groupe,length(input$variables4))
          if(input$typeG4==gettext("Quantitative")){
            if(input$scale4==gettext("Scaled")) typ="s"
            if(input$scale4==gettext("Unscaled")) typ="c"
          }
          else if(input$typeG4==gettext("Qualitative")){
            typ="n"
          }
          else if(input$typeG4==gettext("Frequencies")){
            typ="f"
          }
          if(input$typeG42==gettext("Supplementary")) gsup=c(gsup,4)
          types=c(types,typ)
          nom=c(nom,input$nameG4)
          if(input$activeG5==TRUE && length(input$variables5)>0){
            groupe=c(groupe,length(input$variables5))
            if(input$typeG5==gettext("Quantitative")){
              if(input$scale5==gettext("Scaled")) typ="s"
              if(input$scale5==gettext("Unscaled")) typ="c"
            }
            else if(input$typeG5==gettext("Qualitative")){
              typ="n"
            }
            else if(input$typeG5==gettext("Frequencies")){
              typ="f"
            }
            if(input$typeG52==gettext("Supplementary")) gsup=c(gsup,5)
            types=c(types,typ)
            nom=c(nom,input$nameG5)
            if(input$activeG6==TRUE && length(input$variables6)>0){
              groupe=c(groupe,length(input$variables6))
              nbgroupe=6
              if(input$typeG6==gettext("Quantitative")){
                if(input$scale6==gettext("Scaled")) typ="s"
                if(input$scale6==gettext("Unscaled")) typ="c"
              }
              else if(input$typeG6==gettext("Qualitative")){
                typ="n"
              }
              else if(input$typeG6==gettext("Frequencies")){
                typ="f"
              }
              if(input$typeG62==gettext("Supplementary")) gsup=c(gsup,6)
              types=c(types,typ)
              nom=c(nom,input$nameG6)
              if(input$activeG7==TRUE && length(input$variables7)>0){
                groupe=c(groupe,length(input$variables7))
                if(input$typeG7==gettext("Quantitative")){
                  if(input$scale7==gettext("Scaled")) typ="s"
                  if(input$scale7==gettext("Unscaled")) typ="c"
                }
                else if(input$typeG7==gettext("Qualitative")){
                  typ="n"
                }
                else if(input$typeG7==gettext("Frequencies")){
                  typ="f"
                }
                if(input$typeG72==gettext("Supplementary")) gsup=c(gsup,7)
                types=c(types,typ)
                nom=c(nom,input$nameG7)
                if(input$activeG8==TRUE && length(input$variables8)>0){
                  groupe=c(groupe,length(input$variables8))
                  if(input$typeG8==gettext("Quantitative")){
                    if(input$scale8==gettext("Scaled")) typ="s"
                    if(input$scale8==gettext("Unscaled")) typ="c"
                  }
                  else if(input$typeG8==gettext("Qualitative")){
                    typ="n"
                  }
                  else if(input$typeG8==gettext("Frequencies")){
                    typ="f"
                  }
                  if(input$typeG82==gettext("Supplementary")) gsup=c(gsup,8)
                  types=c(types,typ)
                  nom=c(nom,input$nameG8)
                  if(input$activeG9==TRUE && length(input$variables9)>0){
                    groupe=c(groupe,length(input$variables9))
                    if(input$typeG9==gettext("Quantitative")){
                      if(input$scale9==gettext("Scaled")) typ="s"
                      if(input$scale9==gettext("Unscaled")) typ="c"
                    }
                    else if(input$typeG9==gettext("Qualitative")){
                      typ="n"
                    }
                    else if(input$typeG9==gettext("Frequencies")){
                      typ="f"
                    }
                    if(input$typeG92==gettext("Supplementary")) gsup=c(gsup,9)
                    types=c(types,typ)
                    nom=c(nom,input$nameG9)
                    if(input$activeG10==TRUE && length(input$variables10)>0){
                      groupe=c(groupe,length(input$variables10))
                      if(input$typeG10==gettext("Quantitative")){
                        if(input$scale10==gettext("Scaled")) typ="s"
                        if(input$scale10==gettext("Unscaled")) typ="c"
                      }
                      else if(input$typeG10==gettext("Qualitative")){
                        typ="n"
                      }
                      else if(input$typeG10==gettext("Frequencies")){
                        typ="f"
                      }
                      if(input$typeG102==gettext("Supplementary")) gsup=c(gsup,10)
                      types=c(types,typ)
                      nom=c(nom,input$nameG10)
                    }
                  }
                }
              }
            }
		  }
        }
      }
    }
    if(length(gsup)==0) gsup=NULL
    if(length(input$indsup)==0){
      suple=NULL
    } else{
	  suple=which(nomMFAshiny%in%input$indsup)
    }

    boolImpute <- FALSE
	Code <- paste0("newDF <- ",nomData,'[,c("',paste0(c(input$variables1,input$variables2,if (!is.null(input$variables3)) input$variables3,if (!is.null(input$variables4)) input$variables4),if (!is.null(input$variables5)) input$variables5,if (!is.null(input$variables6)) input$variables6,if (!is.null(input$variables7)) input$variables7,if (!is.null(input$variables8)) input$variables8,if (!is.null(input$variables9)) input$variables9,if (!is.null(input$variables10)) input$variables10,collapse='","'),'")]\n')
  if(!is.null(input$variables1)){
	if (any(is.na(x[,which(colnames(x)%in%c(input$variables1,input$variables2,input$variables3,input$variables4,input$variables5,input$variables6,input$variables7,input$variables8,input$variables9,input$variables10))]))){
	  boolImpute <- TRUE
      if(length(input$impute>0)){
	    # if (input$impute==gettext("Impute with k-dimensional MFA-model (estime k, time consuming)")){
 	      # Code <- paste0(Code,"nb <- missMDA::estim_ncpMFA(",nomData,if (length(choixsup)!=0) paste0(",sup.var=c(",paste0(choixsup,collapse=","),")"),if (length(suple)!=0) paste0(",ind.sup=c(",paste0(suple,collapse=","),")"),")$ncp\n")
	      # Nbncp <- "nb"
	    # }
        if (input$impute==gettext("Impute by means and proportions (fast but not recommended)")) Nbncp <- 0
        if (input$impute==gettext("Impute with 2-dimensional MFA-model (good compromise)")) Nbncp <- 2
      } else {
        Nbncp <- 0
	  }
	  Code <- paste0(Code, "dfcompleted <- missMDA::imputeMFA(newDF, ncp=",Nbncp,',group=c(',paste0(groupe,collapse=','),'), type=c("',paste0(types,collapse='","'),'")',if (!is.null(gsup)) paste0(',num.group.sup=c(',paste0(gsup,collapse=','),')'),if (length(suple)!=0) paste0(",ind.sup=c(",paste0(suple,collapse=","),")"),")\n")	
	}
   }
	Code <- paste0(Code,"res.MFA<-MFA(newDF")
	if (boolImpute) Code <- paste0(Code,",tab.comp=dfcompleted")
	Code <- paste0(Code,',group=c(',paste0(groupe,collapse=','),'), type=c("',paste0(types,collapse='","'),'")',if(max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))!=5) paste0(',ncp=',max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))), if(length(suple)!=0) paste0(",ind.sup=c(",paste(suple,collapse=","),")"), if (!is.null(nom)) paste0(',name.group=c("',paste0(nom, collapse='","'),'")'),if (!is.null(gsup)) paste0(',num.group.sup=c(',paste0(gsup,collapse=','),')'),',graph=FALSE)')
	if (length(groupe)>1) return(list(res.MFA=eval(parse(text=Code)), Code=Code))
	else return(NULL)
    }
    
    output$listvarG1=renderUI({
      if(input$typeG1==gettext("Quantitative") || input$typeG1==gettext("Frequencies")){
        if(length(VariableChoices)>=1) choix=VariableChoices
      }
      if(input$typeG1==gettext("Qualitative")){
        if(length(QualiChoice)>=1) choix=QualiChoice
      }
      return(selectInput("variables1",label=gettext("Choose variables"),
                         choices=choix,multiple=TRUE,selectize=TRUE))
    })
    
    output$listvarG2=renderUI({
      if(input$typeG2==gettext("Quantitative") || input$typeG2==gettext("Frequencies")){
        if(length(VariableChoices)>=1) choix=VariableChoices
      }
      if(input$typeG2==gettext("Qualitative")){
        if(length(QualiChoice)>=1) choix=QualiChoice
      }
      return(selectInput("variables2",label=gettext("Choose variables"),
                         choices=choix,multiple=TRUE,selectize=TRUE))
    })
    
    output$listvarG3=renderUI({
      if(input$typeG3==gettext("Quantitative") || input$typeG3==gettext("Frequencies")){
        if(length(VariableChoices)>=1) choix=VariableChoices
      }
      if(input$typeG3==gettext("Qualitative")){
        if(length(QualiChoice)>=1) choix=QualiChoice
      }
      return(selectInput("variables3",label=gettext("Choose variables"),
                         choices=choix,multiple=TRUE,selectize=TRUE))
    })
    
    output$listvarG4=renderUI({
      if(input$typeG4==gettext("Quantitative") || input$typeG4==gettext("Frequencies")){
        if(length(VariableChoices)>=1) choix=VariableChoices
      }
      if(input$typeG4==gettext("Qualitative")){
        if(length(QualiChoice)>=1) choix=QualiChoice
      }
      return(selectInput("variables4",label=gettext("Choose variables"),
                         choices=choix,multiple=TRUE,selectize=TRUE))
    })
    
    output$listvarG5=renderUI({
      if(input$typeG5==gettext("Quantitative") || input$typeG5==gettext("Frequencies")){
        if(length(VariableChoices)>=1) choix=VariableChoices
      }
      if(input$typeG5==gettext("Qualitative")){
        if(length(QualiChoice)>=1) choix=QualiChoice
      }
      return(selectInput("variables5",label=gettext("Choose variables"),
                         choices=choix,multiple=TRUE,selectize=TRUE))
    })
    
    output$listvarG6=renderUI({
      if(input$typeG6==gettext("Quantitative") || input$typeG6==gettext("Frequencies")){
        if(length(VariableChoices)>=1) choix=VariableChoices
      }
      if(input$typeG6==gettext("Qualitative")){
        if(length(QualiChoice)>=1) choix=QualiChoice
      }
      return(selectInput("variables6",label=gettext("Choose variables"),
                         choices=choix,multiple=TRUE,selectize=TRUE))
    })
    
    output$listvarG7=renderUI({
      if(input$typeG7==gettext("Quantitative") || input$typeG7==gettext("Frequencies")){
        if(length(VariableChoices)>=1) choix=VariableChoices
      }
      if(input$typeG7==gettext("Qualitative")){
        if(length(QualiChoice)>=1) choix=QualiChoice
      }
      return(selectInput("variables7",label=gettext("Choose variables"),
                         choices=choix,multiple=TRUE,selectize=TRUE))
    })
    
    output$listvarG8=renderUI({
      if(input$typeG8==gettext("Quantitative") || input$typeG8==gettext("Frequencies")){
        if(length(VariableChoices)>=1) choix=VariableChoices
      }
      if(input$typeG8==gettext("Qualitative")){
        if(length(QualiChoice)>=1) choix=QualiChoice
      }
      return(selectInput("variables8",label=gettext("Choose variables"),
                         choices=choix,multiple=TRUE,selectize=TRUE))
    })
    
    output$listvarG9=renderUI({
      if(input$typeG9==gettext("Quantitative") || input$typeG9==gettext("Frequencies")){
        if(length(VariableChoices)>=1) choix=VariableChoices
      }
      if(input$typeG9==gettext("Qualitative")){
        if(length(QualiChoice)>=1) choix=QualiChoice
      }
      return(selectInput("variables9",label=gettext("Choose variables"),
                         choices=choix,multiple=TRUE,selectize=TRUE))
    })
    
    output$listvarG10=renderUI({
      if(input$typeG10==gettext("Quantitative") || input$typeG10==gettext("Frequencies")){
        if(length(VariableChoices)>=1) choix=VariableChoices
      }
      if(input$typeG10==gettext("Qualitative")){
        if(length(QualiChoice)>=1) choix=QualiChoice
      }
      return(selectInput("variables10",label=gettext("Choose variables"),
                         choices=choix,multiple=TRUE,selectize=TRUE))
    })
  
    error=function(){
      if(length(input$variables1)!=0 && length(input$variables2)!=0){
        etat <- "ok"
      } else{
       etat <- "not"
      }
      return(etat)
    }
      
  output$NB1 <- renderUI({
    return(textInput("nb1", label = NULL, axe1,width='41px'))
  })
  
  output$NB2=renderUI({
    return(textInput("nb2", label = NULL, axe2,width='41px'))
  })
  
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
        return(tags$div( 
            div(gettext("Number of dimensions kept for clustering"), style="display: inline-block; padding: 0px 0px 0px 0px"),
		    div(numericInput(inputId = "nbDimClustering", label = NULL,value=if(is.null(nbdimclustMFAshiny)){5} else {nbdimclustMFAshiny},min=1), style="display: inline-block;width: 70px; padding: 0px 0px 0px 10px"))
		)
    }
  })

  output$imputeData <- renderUI({
    if(any(is.na(newdataMFAshiny))){
	  return(radioButtons("impute",gettext("Handling missing values"),choices=list(gettext("Impute by means and proportions (fast but not recommended)"),gettext("Impute with 2-dimensional MFA-model (good compromise)")),selected=gettext("Impute by means and proportions (fast but not recommended)")))
	} else {
      return(tags$div(tags$label(class="control-label", "Handling missing values"),
	   tags$div(HTML("No missing values"))))
	}
  })

  output$choixindvar=renderUI({
    choix <- indvarMFAshiny
      if(is.null(codeMFA()$res.MFA$ind)) choix <- setdiff(choix,gettext("Individuals"))
      if(is.null(codeMFA()$res.MFA$ind.sup)) choix <- setdiff(choix,gettext("Supplementary individuals"))
      if(is.null(codeMFA()$res.MFA[["quali.var"]])) choix <- setdiff(choix,gettext("Categories"))
      if(is.null(codeMFA()$res.MFA$quali.var.sup)) choix <- setdiff(choix,gettext("Supplementary categories"))
	# choix <- gettext("Individuals")
    # if(length(input$indsup)>0) choix <- c(choix,gettext("Supplementary individuals"))
    # if(length(QualiChoice)>0) choix <- c(choix,gettext("Categories"),gettext("Supplementary categories"))
    ## if(!(is.null(codeMFA()$res.MFA[["quali.var"]]))) choix <- c(choix,gettext("Categories"))
    ## if(!(is.null(codeMFA()$res.MFA$quali.var.sup))) choix <- c(choix,gettext("Supplementary categories"))
    div(align="left",checkboxGroupInput("ind_var",gettext("Points to draw"), choices=choix, selected = indvarMFAshiny))
  })

  output$choixindvarfreq=renderUI({
    choix <- indvarMFAshinyfreq
      if(is.null(codeMFA()$res.MFA$ind)) choix <- setdiff(choix,gettext("Individuals"))
      if(is.null(codeMFA()$res.MFA$ind.sup)) choix <- setdiff(choix,gettext("Supplementary individuals"))
      if(is.null(codeMFA()$res.MFA[["quali.var"]])) choix <- setdiff(choix,gettext("Categories"))
      if(is.null(codeMFA()$res.MFA$quali.var.sup)) choix <- setdiff(choix,gettext("Supplementary categories"))
      if(is.null(codeMFA()$res.MFA[["freq"]])) choix <- setdiff(choix,gettext("Frequencies"))
      if(is.null(codeMFA()$res.MFA$freq.sup)) choix <- setdiff(choix,gettext("Supplementary frequencies"))
    div(align="left",checkboxGroupInput("ind_varfreq",gettext("Points to draw"), choices=choix, selected = indvarMFAshinyfreq))
  })

  output$choixgraphic=renderUI({
    choix=gettext("Individuals")
    if(!(is.null(codeMFA()$res.MFA$quanti.var))) choix <- c(choix,gettext("Quantitative variables"))
    choix <- c(choix,gettext("Groups"),gettext("Partial axes"))
    if(!(is.null(codeMFA()$res.MFA$freq))) choix <- c(choix,gettext("Frequencies"))
    div(align="center",selectInput("choixgraph",gettext("Which graph would you like to modify?"), choices=choix,selected=gettext("Individuals")))
})
    
  output$drawindiv=renderUI({
    if(input$choixpartial==gettext("None")){
      if (is.null(codeMFA()$res.MFA$quali.var) & is.null(codeMFA()$res.MFA$quali.var.sup)) return(radioButtons("drawind",gettext("Drawing by"),choices=list(gettext("No selection"),gettext("individual")),selected=gettext("No selection"),inline=TRUE))
      else return(radioButtons("drawind",gettext("Drawing by"),choices=list(gettext("No selection"),gettext("individual"),gettext("categorical variable")),selected=gettext("No selection"),inline=TRUE))
    } else{
      if (is.null(codeMFA()$res.MFA$quali.var) & is.null(codeMFA()$res.MFA$quali.var.sup)) return(radioButtons("drawind",gettext("Drawing by"),choices=list(gettext("group"),gettext("individual")),selected=drawing,inline=TRUE))
      else return(radioButtons("drawind",gettext("Drawing by"),choices=list(gettext("group"),gettext("individual"),gettext("categorical variable")),selected=drawing,inline=TRUE))
    }
  })
    
  output$habillagequali=renderUI({
      if(!(is.null(codeMFA()$res.MFA$quali.var))){
        return(selectInput("habiquali"," ",choices=quali))
      } else{
      p(gettext("No groups of categorical variable"))
    }
  })
    
    output$indivpartiel2=renderUI({
      if(is.null(partial2)){
        return(selectInput("indivpartiel",label=gettext("Select individuals"),
                           choices=rownames(data),multiple=TRUE))
      }
      else{
        return(selectInput("indivpartiel",label=gettext("Select individuals"),
                           choices=rownames(data),multiple=TRUE,selected=partial2))
      }
    })
    
    output$slider1=renderUI({
        maxlength=nrow(codeMFA()$res.MFA$quanti.var$coord)
        if(input$selection=="contrib"){
          return(sliderInput("slider2",gettext("Number of the most contributive variables"),min=1, max=maxlength, value=maxlength, step=1))
        }
        if(input$selection=="cos2"){
          return(sliderInput("slider3",gettext("Variables with cos2 highest than"),min=0, max=1, value=0, step=0.01))
        }
    })
    
    output$hide2=renderUI({
      if(!(is.null(codeMFA()$res.MFA$quanti.var.sup))){
        if(!is.null(hide)){
          return(radioButtons("hides",gettext("Hide:"),choices=list(gettext("Nothing"),gettext("Active variables"),gettext("Supplementary variables")),selected=hide))
        } else{
          return(radioButtons("hides",gettext("Hide:"),choices=list(gettext("Nothing"),gettext("Active variables"),gettext("Supplementary variables")),selected=gettext("Nothing")))
        }
      }
    })
    
    CodeGraphInd <- function(){
	  res.MFA <- codeMFA()$res.MFA
      validate(
        need(error()!="not",gettext("Please select at least 2 groups")),
        need(!is.null(codeMFA()$res.MFA),gettext("You must validate the groups")),
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions")),
        need(input$nb1 <= ncol(codeMFA()$res.MFA$ind$coord), paste(gettext("The number of dimensions must be less than"),ncol(codeMFA()$res.MFA$ind$coord))),
        need(input$nb2 <= ncol(codeMFA()$res.MFA$ind$coord), paste(gettext("The number of dimensions must be less than"),ncol(codeMFA()$res.MFA$ind$coord)))
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
    if(input$select==gettext("No selection")){
      selecindiv <- NULL
      selecindivtext <- "NULL"
    }
    if(input$select=="contrib"){
      selecindiv <- paste("contrib ",input$sliderind0)
      selecindivtext <- paste0("'",selecindiv,"'")
    }

    if(input$select==gettext("Manual")){
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
      if(input$choixpartial==gettext("All")) part="'all'"
      if(input$choixpartial==gettext("Choose") & !is.null(input$indivpartiel)) part=paste0("c('",paste0(input$indivpartiel,collapse="','"),"')")
      lapbar=TRUE
      if(input$choixpartial!=gettext("None") && input$partind==FALSE) lapbar=FALSE
      habi="none"
      if(!(is.null(input$drawind))){
        if(input$drawind==gettext("individual")) habi="ind"
        if((input$choixpartial==gettext("All") || input$choixpartial==gettext("Choose")) && input$drawind==gettext("group")) habi="group"
        if(input$drawind==gettext("categorical variable")) habi=input$habiquali
      }
	  inv <- c()
      if(sum(gettext("Individuals")==input$ind_var)==0)  inv<-c(inv,"'ind'")
      if(length(QualiChoice)>0 & sum(gettext("Categories")==input$ind_var)==0) inv<-c(inv,"'quali'")
      if(length(input$indsup)>0 & sum(gettext("Supplementary individuals")==input$ind_var)==0) inv<-c(inv,"'ind.sup'")
      if(length(QualiChoice)>0 & sum(gettext("Supplementary categories")==input$ind_var)==0) inv<-c(inv,"'quali.sup'")
      if(length(inv)>1) vecinv<-paste0("c(",paste0(inv,collapse=","),")")
      if(length(inv)==1) vecinv <- inv
      if(length(inv)==0 | length(input$ind_var)==0) vecinv<-"NULL"
	  # res.MFA <- codeMFA()$res.MFA
      Code <- paste0('plot.MFA(res.MFA, choix="ind"',if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if(length(part)>0) paste0(",partial=",part),if(length(input$partind)>0) {paste0(",lab.par=",input$partind)},if (vecinv!="NULL") paste(",invisible=",vecinv), if(selecindivtext!="NULL"){paste0(",select=",selecindivtext)}, if (habi!="none" & habi!="''"){paste0(",habillage='",habi,"'")},if(input$titleInd!="MFA graph of individuals")paste0(',title="',input$titleInd,'"'),if(input$cexInd!=1)paste0(",cex=",input$cexInd,",cex.main=",input$cexInd,",cex.axis=",input$cexInd),")")
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))      
    }
    
    output$map <- renderPlot({
      p <- print(CodeGraphInd()$Plot)
    })
    
    CodeGraphVar=function(){
      if (is.null(codeMFA()$res.MFA$quanti.var) & is.null(codeMFA()$res.MFA$quanti.sup)) return(NULL)
	  habi <- NULL
	  if(input$colorgroup==TRUE) habi="'group'"
      if(input$selection==gettext("No selection")) selec=NULL
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
	    if (input$hides==gettext("Nothing")) invi="none"
	    if (input$hides==gettext("Active variables")) invi="quanti"
	    if (input$hides==gettext("Supplementary variables")) invi="quanti.sup"
      }
	  res.MFA <- codeMFA()$res.MFA
      Code <- paste0('plot.MFA(res.MFA, choix="var"',if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (!is.null(selec)) paste0(",select=",selec),if(invi!="none"){paste0(",invisible=c(",paste0("'",paste(invi,collapse="','"),"'"),")")}, if (!is.null(habi)){paste0(",habillage=",habi)},if(input$titleVar!="Graph of quantitative variables")paste0(',title="',input$titleVar,'"'),if(input$cexVar!=1)paste0(",cex=",input$cexVar,",cex.main=",input$cexVar,",cex.axis=",input$cexVar),")")
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))      
    }
    
    output$map2 <- renderPlot({
      p=print(CodeGraphVar()$Plot)
    })
    
    output$map22=renderUI({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups")),
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      
      if(is.null(codeMFA()$res.MFA$quanti.var)){
        return(p())
      } else{
        column(width = 6,shinyjqui::jqui_resizable(plotOutput("map2", height="500")),
           br(),
           p(gettext("Download as"),downloadButton("downloadData4",gettext("jpg")),downloadButton("downloadData3",gettext("png")),downloadButton("downloadData5",gettext("pdf")),align="center")
		)
	  }
    })
    
    CodeGraphGroup <- function(){
      validate(
        need(error()!="not",gettext("Please select at least 2 groups")),
        need(!is.null(codeMFA()$res.MFA),gettext("You must validate the groups")),
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
	  res.MFA <- codeMFA()$res.MFA
      Code <- paste0('plot.MFA(res.MFA, choix="group"',if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"), if(input$titleGroup!="Groups representation")paste0(',title="',input$titleGroup,'"'),if(input$cexGroup!=1)paste0(",cex=",input$cexGroup,",cex.main=",input$cexGroup,",cex.axis=",input$cexGroup),")")
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))      
    }
    
    output$map5 <- renderPlot({
      p <- print(CodeGraphGroup()$Plot)
    })
    
    CodeGraphPartial <- function(){
      validate(
        need(error()!="not",gettext("Please select at least 2 groups")),
        need(!is.null(codeMFA()$res.MFA),gettext("You must validate the groups")),
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      habi="'group'"
	  res.MFA <- codeMFA()$res.MFA
      Code <- paste0('plot.MFA(res.MFA, choix="axes"',if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"), if(input$titlePartial!="Graph of the partial axes")paste0(',title="',input$titlePartial,'"'),if (!is.null(habi)){paste0(",habillage=",habi)},if(input$cexPartial!=1)paste0(",cex=",input$cexPartial,",cex.main=",input$cexPartial,",cex.axis=",input$cexPartial),")")
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))      
    }
    
    output$map4 <- renderPlot({
      p <- print(CodeGraphPartial()$Plot)
    })

    CodeGraphFreq <- function(){
      validate(
        need(error()!="not",gettext("Please select at least 2 groups")),
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
	  if (is.null(codeMFA()$res.MFA$freq)) return(NULL)
	  habi="'group'"
	  res.MFA <- codeMFA()$res.MFA
# invisible = row, row.sup, col ou col.sup

	  inv <- c()
      if(sum(gettext("Individuals")==input$ind_varfreq)==0)  inv<-c(inv,"'row'")
      if(length(QualiChoice)>0 & sum(gettext("Categories")==input$ind_varfreq)==0) inv<-c(inv,"'quali'")
      if(length(input$indsup)>0 & sum(gettext("Supplementary individuals")==input$ind_varfreq)==0) inv<-c(inv,"'row.sup'")
      if(length(QualiChoice)>0 & sum(gettext("Supplementary categories")==input$ind_varfreq)==0) inv<-c(inv,"'quali.sup'")
      if(sum(gettext("Frequencies")==input$ind_varfreq)==0) inv<-c(inv,"'col'")
      if(sum(gettext("Supplementary frequencies")==input$ind_varfreq)==0) inv<-c(inv,"'col.sup'")
      if(length(inv)>1) vecinv<-paste0("c(",paste0(inv,collapse=","),")")
      if(length(inv)==1) vecinv <- inv
      if(length(inv)==0 | length(input$ind_varfreq)==0) vecinv<-"NULL"

      Code <- paste0('plot.MFA(res.MFA, choix="freq"',if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (!is.null(habi)){paste0(",habillage=",habi)},if (vecinv!="NULL") paste(",invisible=",vecinv), if (length(input$affichcol)>0){ if (input$affichcol==FALSE) ",lab.col=FALSE"},if (length(input$affichind)>0){if (input$affichind==FALSE) paste0(",lab.ind=FALSE")}, if(input$titleFreq!="Graph of the frequencies")paste0(',title="',input$titleFreq,'"'),if(input$cexFreq!=1)paste0(",cex=",input$cexFreq,",cex.main=",input$cexFreq,",cex.axis=",input$cexFreq),")")
      Plot <- eval(parse(text=Code))
	  return(list(Code=Code,Plot=Plot))      
    }
    
    output$map6 <- renderPlot({
      p <- print(CodeGraphFreq()$Plot)
    })
    
    output$map66=renderUI({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups")),
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      if(is.null(codeMFA()$res.MFA$freq)){
        return(p())
      } else{
        column(width = 6,shinyjqui::jqui_resizable(plotOutput("map6", height="500")),
           br(),
           p(gettext("Download as"),downloadButton("downloadData19",gettext("jpg")),downloadButton("downloadData20",gettext("png")),downloadButton("downloadData21",gettext("pdf")),align="center")
		)
	  }
    })

  output$sorties=renderTable({
      return(as.data.frame(codeMFA()$res.MFA$eig))
    },rownames=TRUE)
    
    output$map3=renderPlot({
      print(ggplot2::ggplot(cbind.data.frame(x=1:nrow(codeMFA()$res.MFA$eig),y=codeMFA()$res.MFA$eig[,2])) + ggplot2::aes(x=x, y=y)+ ggplot2::geom_col(fill="blue") + ggplot2::xlab("Dimension") + ggplot2::ylab(gettext("Percentage of variance")) + ggplot2::ggtitle(gettext("Decomposition of the total inertia")) + ggplot2::theme_light() + ggplot2::theme(plot.title = ggplot2::element_text(hjust =0.5))  + ggplot2::scale_x_continuous(breaks=1:nrow(codeMFA()$res.MFA$eig)))
      # return(barplot(codeMFA()$res.MFA$eig[,1],names.arg=rownames(codeMFA()$res.MFA$eig),las=2))
    })
    output$JDD=renderDataTable({
      tab=cbind(Names=rownames(codeMFA()$res.MFA$global.pca$call$X),codeMFA()$res.MFA$global.pca$call$X)
      quanti=names(which(sapply(tab,is.numeric)))
      tab[quanti]=round(tab[quanti],5)
      tab
      },
      options = list( "orderClasses" = TRUE, "responsive" = TRUE, "pageLength" = 10))
    
    output$summary=renderPrint({
      summary(codeMFA()$res.MFA$global.pca$call$X)
    })
    
    output$summaryMFA=renderPrint({
      summary.MFA(codeMFA()$res.MFA)
    })  
    
      
    output$histo=renderPlot({
      par(mfrow=c(1,2))
      boxplot(x[,input$bam])
      plot(density(x[,input$bam]),main="",xlab="")
    })
    
    
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
      if(is.null(codeMFA()$res.MFA$quanti.var)){
        return()
      }
      else{
        return(downloadButton("downloadData3",gettext("Download as png")))
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
      if(is.null(codeMFA()$res.MFA$freq)){
        return()
      }
      else{
        return(downloadButton("downloadData19",gettext("Download as jpg")))
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
      if(is.null(codeMFA()$res.MFA$freq)){
        return()
      }
      else{
        return(downloadButton("downloadData20",gettext("Download as png")))
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
      if(is.null(codeMFA()$res.MFA$freq)){
        return()
      }
      else{
        return(downloadButton("downloadData21",gettext("Download as pdf")))
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
      if(is.null(codeMFA()$res.MFA$quanti.var)){
        return()
      }
      else{
        return(downloadButton("downloadData4",gettext("Download as jpg")))
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
      if(is.null(codeMFA()$res.MFA$quanti.var)){
        return()
      }
      else{
        return(downloadButton("downloadData5",gettext("Download as pdf")))
      }
    })
        
    output$sorties1=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$ind$coord))
    },rownames=TRUE)
    
    output$sorties2=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$ind$contrib))
    },rownames=TRUE)
    
    output$sorties3=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$ind$cos2))
    },rownames=TRUE)
    
    output$sorties4=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$ind$within.inertia))
    },rownames=TRUE)
    
    output$sorties5=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$ind$coord.partiel))
    },rownames=TRUE)
    
    output$sorties6=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$ind$within.partial.inertia))
    },rownames=TRUE)
    
    output$sorties11=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$quanti.var$coord))
    },rownames=TRUE)
    
    output$sorties22=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$quanti.var$contrib))
    },rownames=TRUE)
    
    output$sorties33=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$quanti.var$cos2))
    },rownames=TRUE)
    
    output$sorties44=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$quanti.var$cor))
    },rownames=TRUE)
    
    output$sorties12=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$partial.axes$coord))
    },rownames=TRUE)
    
    output$sorties23=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$partial.axes$cor))
    },rownames=TRUE)
    
    output$sorties34=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$partial.axes$contrib))
    },rownames=TRUE)
    
    output$sorties45=renderTable({
      validate(
        need(error()!="not",gettext("Please select at least 2 groups"))
      )
      return(as.data.frame(codeMFA()$res.MFA$partial.axes$cor.between))
    },rownames=TRUE)    
    
    output$sortiegroup=renderTable({
      write.infile(X=codeMFA()$res.MFA$group,file=paste(getwd(),"fichgroup.csv"),sep=";",nb.dec=5)
      baba=read.csv(paste(getwd(),"fichgroup.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      file.remove(paste(getwd(),"fichgroup.csv"))
      baba
    },
    rownames=FALSE)
    
    observe({
      if(input$MFAcode!=0){
        isolate({
          cat(codeMFA()$Code,sep="\n")
          cat(CodeGraphInd()$Code,sep="\n")
          if(!is.null(codeMFA()$res.MFA$quanti.var)) cat(CodeGraphVar()$Code,sep="\n")
          cat(CodeGraphGroup()$Code,sep="\n")
          cat(CodeGraphPartial()$Code,sep="\n")
          if(!is.null(codeMFA()$res.MFA$freq)) cat(CodeGraphFreq()$Code,sep="\n")
        })
      }
    })
    
  observe({
   if(input$Quit!=0){
     isolate({
      res=list()
      res$nomObjectMFA <- "res.MFA"
      res$codeMFA=codeMFA()$Code
      res$data=newdataMFAshiny
	  res$anafact <- codeMFA()$res.MFA
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
      res$selectvar=input$selection
      res$selectionMFAshiny <- input$select
      sel=NULL
      if(input$selection=="contrib") sel=input$slider2
      if(input$selection=="cos2") sel=input$slider3
      if(input$select=="cos2") res$selection2MFAshiny <- input$sliderind1
      if(input$select==gettext("No selection")) res$selection2MFAshiny <- NULL
      if(input$select=="contrib") res$selection2MFAshiny <- input$sliderind0
      if(input$select==gettext("Manual")) res$selection2MFAshiny <- input$indiv
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
