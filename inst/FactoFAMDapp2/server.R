# server script for FAMD2
shinyServer(
  function(input, output) {
    values=reactive({
    if (input$selecactive==gettext("All")){
      data.selec=newdata
    }
    else{
      validate(
        need(length(input$supvar)>0 || length(input$supvar1)>0, gettext("Please select at least one supplementary variable"))
      )
      data.selec=newdata
    }
    choixsup=getactive()$sup
    if(length(input$indsup)==0){
      suple=NULL
    }
    else{
	  suple=which(nom%in%input$indsup)
      # suple=c()
      # for (i in 1:length(nom)){
        # if(nom[i]%in%input$indsup){
          # suple=c(suple,i)
        # }
      # }
    }
    list(res.FAMD=(FAMD(data.selec,sup.var=choixsup,ind.sup=suple,graph=FALSE,ncp=max(5,as.numeric(input$nb1),as.numeric(input$nb2)))),DATA=(data.selec),choixsuple=(suple),varsup=(choixsup))
    })
    
    Plot1 <- reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variables"))
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
      list(PLOT=(plot.FAMD(values()$res.FAMD,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="var",cex=input$cex2,cex.main=input$cex2,cex.axis=input$cex2,title=input$title2,select=selecindiv)),selecindivText=selecindivText)
    })
    
    output$map <- renderPlot({
      p <- Plot1()$PLOT
    })
    
    Plot2 <- reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      hab="none"
      if(length(QualiChoice)==0){
        hab="none"
      }
      else if(length(QualiChoice)==1 && input$habi==TRUE){
        if(input$habi==TRUE){
          hab=QualiChoice
          colquali="blue"
        }
        else{
          hab="none"
          colquali="magenta"
        }
      }
      else if (length(QualiChoice)>1){
        if(input$habi==TRUE){
          if(is.null(input$habiller)){
            hab="none"
          }
          else{
          hab=input$habiller
          }
        }
        else{
          hab="none"
          colquali="magenta"
        }
      }
      if(hab!="none"){
        hab=which(all==hab)
        hab=as.numeric(hab)
      }
      if(input$select=="cos2"){
        if(input$slider1!=1){
          selecindiv=paste("cos2 ",input$slider1)
        }
        else{
          selecindiv="cos2 0.999"
        }
        selecindivText=paste0("'",selecindiv,"'")
      }
      if(input$select=="NONE"){
        selecindiv=NULL
        selecindivText="NULL"
      }
      if(input$select=="contrib"){
        selecindiv=paste("contrib ",input$slider0)
        selecindivText=paste0("'",selecindiv,"'")
      }
      if(input$select=="Manuel"){
        selecindiv=c(input$indiv)
        selecindivText=paste0("'",selecindiv,"'")
      }
      list(PLOT=(plot.FAMD(values()$res.FAMD,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="ind",select=selecindiv,lab.var=input$labels,lab.ind=input$labels2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,habillage=hab,title=input$title1)),selecindivText=selecindivText,HABILLAGE=hab)
      
    })
    
    output$map2 <- renderPlot({
      p <- Plot2()$PLOT
    })
   
    
    Plot4 <- reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
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
      list(PLOT=(plot.FAMD(values()$res.FAMD,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="quanti",cex=input$cex3,cex.main=input$cex3,cex.axis=input$cex3,title=input$title3,select=selecindiv)))      
    })
    
    output$map4 <- renderPlot({
      p <- Plot4()$PLOT
    })
    
    ### Boutton pour quitter l'application
    ### Recuperation parametres
    observe({
      if(input$Quit==0){
      }
      else{
        isolate({
          stopApp(returnValue=valeuretour())
        })
      }
    })
    
    valeuretour=function(){
      res=list()
      res$nomData=nomData
      res$data=values()$DATA
      res$anafact <- values()$res.FAMD
      res$b=input$supvar
      res$c=input$supvar1
      res$d=input$indsup
      res$e=input$nb1
      res$f=input$nb2
      habillage=NULL
      if(input$habi==TRUE){
        habillage=input$habiller
      }
      res$g=habillage
      if(input$select=="cos2"){
        selecindiv=input$slider1
      }
      if(input$select=="NONE"){
        selecindiv=NULL
      }
      if(input$select=="contrib"){
        selecindiv=input$slider0
      }
      if(input$select=="Manuel"){
        selecindiv=input$indiv
      }
      res$h=input$select
      res$i=selecindiv
      selecindiv2=NULL
      if(input$select0=="cos2"){
        selecindiv2=input$slider00
      }
      if(input$select0=="contrib"){
        selecindiv2=input$slider4
      }
      res$j=input$select0
      res$k=selecindiv2
      selecindiv3=NULL
      if(input$selecti=="cos2"){
        selecindiv3=input$slider000
      }
      if(input$selecti=="contrib"){
        selecindiv3=input$slider6
      }
      res$o=input$selecti
      res$p=selecindiv3
      res$l=input$cex
      res$m=input$cex2
      res$n=input$cex3
      res$code1=code()
      res$code2=codeGraphVar()
      res$code3=codeGraphInd()
      res$labind=input$labels2
      res$labvar=input$labels
    res$hcpcparam <- input$hcpcparam
    res$nbdimclustFAMDshiny <- input$nbDimClustering
      class(res) <- "FAMDshiny"
      return(res)
    }
    
    #### Fonction recuperation de code
    
    observe({
      if(input$FAMDcode==0){
      }
      else {
        isolate({
          if (length(input$habiller)==2 & input$habi==TRUE){
            cat(paste("newCol<-paste(",nomData,"[,'",input$habiller[1],"'],",nomData,"[,'",input$habiller[2],"'],","sep='/')",sep=""),sep="\n")
          }
          cat(code(),sep="\n")
          cat(codeGraphVar(),sep="\n")
          cat(codeGraphInd(),sep="\n")
        })
      }
    })
  
    
    
    codeGraphVar<-function(){
      
      if(length(input$slider4)==0){
        selection="NULL"
      }
      else{
        selection=Plot1()$selecindivText
      }
      Call1=paste("plot.FAMD(res.FAMD,axes=c(",input$nb1,",",input$nb2,"),choix='var',select=",selection,",cex=",input$cex2,",cex.main=",input$cex2,",cex.axis=",input$cex2,",unselect=0)",sep="")
      return(Call1)
    }
    
    codeGraphInd<-function(){
      hab='none'
      if (length(input$habiller)<=1 & input$habi==TRUE || input$habi==FALSE){
        hab=paste(Plot2()$HABILLAGE,sep="")
      }
      if (hab=="none") Call2=paste("plot.FAMD(res.FAMD, axes=c(",input$nb1,",",input$nb2,"),choix='ind',select=",Plot2()$selecindivText,",cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex,",lab.var=",input$labels,",lab.ind=",input$labels2,",title='",input$title1,"')",sep="")
	  else Call2=paste("plot.FAMD(res.FAMD, axes=c(",input$nb1,",",input$nb2,"),choix='ind',select=",Plot2()$selecindivText,",habillage=",hab,",cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex,",lab.var=",input$labels,",lab.ind=",input$labels2,",title='",input$title1,"')",sep="")
      return(Call2)
    }
    
    ##### Fin de la fonction recuperation du code
    
    output$out22=renderUI({
#      choix=list("Summary of FAMD"="ACP","Eigenvalues"="eig","Results of the variables"="resvar","Results of the individuals"="resind")
      choix=list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results of the variables"),gettext("Results of the individuals"))
      if(!is.null(values()$choixsuple)){
#        choix=c(choix,"Results of the supplementary individuals"="supind")
        choix=c(choix,gettext("Results of the supplementary individuals"))
      }
      if(!is.null(values()$varsup)){
#        choix=c(choix,"Results of the supplementary variables"="varsup")
        choix=c(choix,gettext("Results of the supplementary variables"))
      }
      radioButtons("out",gettext("Which outputs do you want?"),
                   choices=choix,inline=TRUE)
    })
    
    getactive=function(){
      if(input$selecactive==gettext("Choose")){
      sup<-sup2<-sup3<-NULL
      if(length(input$supvar)==0&&length(input$supvar1)==0){
        activevar<-all
        supl<-NULL
      }
      else if(length(input$supvar1)==0&&length(input$supvar)!=0){
        # for (i in 1:length(all)){
          # if(all[i]%in%input$supvar){
            # sup=c(sup,i)
          # }
        # }
	    sup<-which(all%in%input$supvar)
        activevar<-all[-sup]
        supl<-VariableChoices[sup]
        quanti<-VariableChoices[-sup]
      }
      else if(length(input$supvar)==0&&length(input$supvar1)!=0){
        # for (i in 1:length(all)){
          # if(all[i]%in%input$supvar1){
            # sup=c(sup,i)
          # }
        # }
	    sup=which(all%in%input$supvar1)
        activevar=all[-sup]
        supl=QualiChoice[sup]
        quali=QualiChoice[-sup]
      }
      else if(length(input$supvar)!=0&&length(input$supvar1)!=0){
        # for (i in 1:length(all)){
          # if(all[i]%in%input$supvar1 || all[i]%in%input$supvar){
            # sup=c(sup,i)
          # }
        # }
	    sup=which(all%in%c(input$supvar,input$supvar1))
        activevar=all[-sup]
        supl=all[sup]
        # for (i in 1:length(QualiChoice)){
          # if(QualiChoice[i]%in%input$supvar1){
            # sup2=c(sup2,i)
          # }
        # }
	    sup2=which(QualiChoice%in%input$supvar1)
        # for (i in 1:length(VariableChoices)){
          # if(VariableChoices[i]%in%input$supvar){
            # sup3=c(sup3,i)
          # }
        # }
	    sup3=which(VariableChoices%in%input$supvar)
      quali=QualiChoice[-sup2]
      quanti=VariableChoices[-sup3]
      }
      # ind=NULL
	  # ind=which(all%in%supl)
	  # if (length(ind)==0) ind=NULL
      # if(!is.null(supl)){
        # for(i in 1:length(supl)){
          # ind=c(ind,which(all==supl[i]))
        # }
      # }
      return(list(activevar=activevar,supl=supl,quanti=quanti,quali=quali,sup=sup))
    }
  }
    
    
    output$NB1=renderUI({
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variables"))
      )
      if(input$selecactive==gettext("All") || length(getactive()$activevar)>5){
        return(textInput("nb1", label = h6(gettext("x axis")), axe1,width='44px'))
      }
      else{
        baba=c(1:length(getactive()$activevar))
        return(selectInput("nb1",label=h6(gettext("x axis")), choices=baba,selected=axe1,width='44px'))
      }
    })
    
    output$NB2=renderUI({
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variables"))
      )
      if(input$selecactive==gettext("All") || length(getactive()$activevar)>5){
        return(textInput("nb2", label = h6(gettext("y axis")), axe2,width='44px'))
      }
      else{
        baba=c(1:length(getactive()$activevar))
        return(selectInput("nb2",label=h6(gettext("y axis")), choices=baba,selected=axe2,width='44px'))
      }
    })
    
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
      fluidRow(
        tags$head(
          tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle; } 
                     #inline .form-group { display: table-row;}")
          ),
        return(tags$div(id = "inline", numericInput(inputId = "nbDimClustering", label = gettext("Number of dimensions kept for clustering:"),value=nbdimclustFAMDshiny,min=1)))
      )
    }
  })


    output$sorties=renderTable({
        return(as.data.frame(values()$res.FAMD$eig))
    },rownames=TRUE)
    
    output$sorties12=renderTable({
        validate(
          need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
        )
        validate(
          need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
        )
        return(as.data.frame(values()$res.FAMD$quali.sup$coord))
    },rownames=TRUE)
    
    output$sorties13=renderTable({
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variables"))
      )
      validate(
        need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
      )
      return(as.data.frame(values()$res.FAMD$quali.sup$v.test))
    },rownames=TRUE)
    
    output$sorties2=renderTable({
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variables"))
      )
      return(as.data.frame(values()$res.FAMD$var$coord))
    },rownames=TRUE)
    
    output$sorties22=renderTable({
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variables"))
      )
      return(as.data.frame(values()$res.FAMD$ind$coord))
    },rownames=TRUE)
    
    output$sorties23=renderTable({
      validate(
        need(length(input$supvar)!=0, gettext("No supplementary quantitative variables"))
      )
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.FAMD$var$coord.sup))
    },rownames=TRUE)
    
    output$sorties32=renderTable({
      validate(
        need(length(input$supvar)!=0, gettext("No supplementary quantitative variables"))
      )
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.FAMD$var$cos2.sup))
    },rownames=TRUE)
    
    output$sorties36=renderTable({
      validate(
        need(length(input$indsup)!=0, "No supplementary individuals")
      )
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.FAMD$ind.sup$coord))
    },rownames=TRUE)
    
    output$sorties37=renderTable({
      validate(
        need(length(input$indsup)!=0, gettext("No supplementary individuals"))
      )
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.FAMD$ind.sup$cos2))
    },rownames=TRUE)
    
    
    output$sorties3=renderTable({
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.FAMD$var$contrib))
    },rownames=TRUE)
    
    output$sorties33=renderTable({
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.FAMD$ind$contrib))
    },rownames=TRUE)
    
    output$sorties4=renderTable({
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.FAMD$var$cos2))
    },rownames=TRUE)
    
    output$sorties44=renderTable({
      validate(
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.FAMD$ind$cos2))
    },rownames=TRUE)
  
  output$sortieDimdesc3=renderTable({
    validate(
      need(length(getactive()$activevar)>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    return(as.data.frame(dimdesc(values()$res.FAMD)[[1]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc4=renderTable({
    validate(
      need(length(getactive()$activevar)>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    return(as.data.frame(dimdesc(values()$res.FAMD)[[1]]$quali))
  },rownames=TRUE)
  
  #DIM2
  
  output$sortieDimdesc33=renderTable({
    validate(
      need(length(getactive()$activevar)>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    return(as.data.frame(dimdesc(values()$res.FAMD)[[2]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc44=renderTable({
    validate(
      need(length(getactive()$activevar)>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    return(as.data.frame(dimdesc(values()$res.FAMD)[[2]]$quali))
  },rownames=TRUE)
  
  #DIM3
  
  output$sortieDimdesc333=renderTable({
    validate(
      need(length(getactive()$activevar)>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    return(as.data.frame(dimdesc(values()$res.FAMD)[[3]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc444=renderTable({
    validate(
      need(length(getactive()$activevar)>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    return(as.data.frame(dimdesc(values()$res.FAMD)[[3]]$quali))
  },rownames=TRUE)
    
    
    output$map3=renderPlot({
      return(barplot(values()$res.FAMD$eig[,1],names.arg=rownames(values()$res.FAMD$eig),las=2))
    })
    
    output$JDD=renderDataTable({
      cbind(Names=rownames(newdata),newdata)},
      options = list(    "orderClasses" = TRUE,
                         "responsive" = TRUE,
                         "pageLength" = 10))
  
    output$summary=renderPrint({
      summary(newdata)
    })
  
  
  code<-function(){
    vec=nomData
    part2=""
    if(!is.null(input$supvar)||!is.null(input$supvar1)){
      choixsup=getactive()$sup
      # vect3<-choixsup[1]
      # if (length(choixsup)>1){
	    # for(i in 2:length(choixsup)){
          # vect3<-paste(vect3,paste(choixsup[i],sep=""),sep=",")
        # }
	  # }
      vect3 <- paste(choixsup,collapse=",")
      part2=paste(",sup.var=c(",vect3,")",sep="")
    }
    part3=""
    if(!is.null(input$indsup)){
      # suple=c()
      # for (i in 1:length(nom)){
        # if(nom[i]%in%input$indsup){
          # suple=c(suple,i)
        # }
      # }
	    suple=which(nom%in%input$indsup)
        # vect4=NULL
        # vect4<-paste(vect4,suple[1],sep="")
        # if(length(suple)>1){
        # for(i in 2:length(suple)){
          # vect4<-paste(vect4,paste(suple[i],sep=""),sep=",")
        # }
        # }
		vect4 <- paste(suple,collapse=",")
      if(part2!=""){
        part3=paste("ind.sup=c(",vect4,")",sep="")
      }
       else{
         part3=paste(",ind.sup=c(",vect4,")",sep="")
       }
    }
    Call1=as.name(paste("res.FAMD<-FAMD(",vec,part2,part3,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep=""))
    return(Call1)
  }
  
  # Attention, si le nombre d'individus passe en dessous de 10, bug
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
        need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      if(input$selecactive==gettext("All")){
        maxvar=length(all)
      }
      if(input$selecactive==gettext("Choose")){
        maxvar=length(getactive()$activevar)
      }
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
      need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    if(input$selecactive==gettext("All")){
      maxvar=length(quanti)
    }
    if(input$selecactive==gettext("Choose")){
      maxvar=length(getactive()$quanti)
    }
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
      need(length(getactive()$activevar)>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    if(is.null(input$indsup)){
      maxi=length(nom)
    }
    if(!is.null(input$indsup)){
      maxi=length(nom)-length(input$indsup)
    }
    if(selection=="contrib"){
      return(div(align="center",sliderInput("slider0", label = gettext("Number of the most contributive individuals"),
                                     min = 1, max = maxi, value =as.numeric(selection2),step=1)))
    }
    else{
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
      if(input$bam%in%quanti){
      par(mfrow=c(1,2))
      boxplot(newdata[,input$bam])
      hist(newdata[,input$bam],main="",xlab="")
      }
      else{
        barplot(table(newdata[,input$bam]),cex.names=0.8)
      }
    })
    
    
    
    output$downloadData = downloadHandler(
      filename = function() { 
        paste('graph1','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot11()
        dev.off()
      },
      contentType='image/png')
    
    output$downloadData1 = downloadHandler(
      filename = function() { 
        paste('graph1','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot11()
        dev.off()
      },
      contentType='image/jpg')
    
    output$downloadData2 = downloadHandler(
      filename = function() { 
        paste('graph1','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot11()
        dev.off()
      },
      contentType=NA)
    
    output$downloadData3 = downloadHandler(
      filename = function() { 
        paste('graph2','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot22()
        dev.off()
      },
      contentType='image/png')
    
    output$downloadData4 = downloadHandler(
      filename = function() { 
        paste('graph1','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot22()
        dev.off()
      },
      contentType='image/jpg')
    
    output$downloadData5 = downloadHandler(
      filename = function() { 
        paste('graph1','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot22()
        dev.off()
      },
      contentType=NA)
  
  output$downloadData6 = downloadHandler(
    filename = function() { 
      paste('graph3','.jpg', sep='') 
    },
    content = function(file) {
      jpeg(file)
      Plot33()
      dev.off()
    },
    contentType='image/jpg')
  
  output$downloadData7 = downloadHandler(
    filename = function() { 
      paste('graph3','.png', sep='') 
    },
    content = function(file) {
      png(file)
      Plot33()
      dev.off()
    },
    contentType='image/png')
  
  output$downloadData8 = downloadHandler(
    filename = function() { 
      paste('graph3','.pdf', sep='') 
    },
    content = function(file) {
      pdf(file)
      Plot33()
      dev.off()
    },
    contentType=NA)
  
    
    Plot11=function(){
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
      plot.FAMD(values()$res.FAMD,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="var",cex=input$cex2,cex.main=input$cex2,cex.axis=input$cex2,title=input$title2,select=selecindiv)
    }
    Plot22=function(){
      hab="none"
      if(length(QualiChoice)==0){
        hab="none"
      }
      else if(length(QualiChoice)==1 && input$habi==TRUE){
        if(input$habi==TRUE){
          hab=QualiChoice
          colquali="blue"
        }
        else{
          hab="none"
          colquali="magenta"
        }
      }
      else if (length(QualiChoice)>1){
        if(input$habi==TRUE){
          if(is.null(input$habiller)){
            hab="none"
          }
          else{
            hab=input$habiller
          }
        }
        else{
          hab="none"
          colquali="magenta"
        }
      }
      if(hab!="none"){
        hab=which(all==hab)
        hab=as.numeric(hab)
      }
      if(input$select=="cos2"){
        if(input$slider1!=1){
          selecindiv=paste("cos2 ",input$slider1)
        }
        else{
          selecindiv="cos2 0.999"
        }
        selecindivtext=paste0("'",selecindiv,"'")
      }
      if(input$select=="NONE"){
        selecindiv=NULL
      }
      if(input$select=="contrib"){
        selecindiv=paste("contrib ",input$slider0)
      }
      if(input$select=="Manuel"){
        selecindiv=c(input$indiv)
      }
      plot.FAMD(values()$res.FAMD,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="ind",select=selecindiv,lab.var=input$labels,lab.ind=input$labels2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,habillage=hab,title=input$title1)
    }
    Plot33=function(){
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
    plot.FAMD(values()$res.FAMD,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="quanti",cex=input$cex3,cex.main=input$cex3,cex.axis=input$cex3,title=input$title3,select=selecindiv)
    }
  }
)
      

