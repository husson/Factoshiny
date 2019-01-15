# server script for PCA2
shinyServer(
  function(input, output,session) {
    values=reactive({
    if (input$selecactive==gettext("All")){
      data.selec=newdata[,VariableChoices]
    }
    else{
      validate(
        need(length(input$supvar)>0, gettext("Please select at least one supplementary variable"))
      )
      data.selec=newdata[,c(getactive())]
    }

    if(length(QualiChoice)==0){
      choixquali=NULL
    }
    else if (length(QualiChoice)==1){
      if(input$supquali==FALSE){
        choixquali=NULL
      }
      else{
        data.selec=cbind(data.selec,newdata[,QualiChoice])
        colnames(data.selec)[dim(data.selec)[2]]=QualiChoice
        choixquali=length(data.selec)
      }
    }
    else{
      if(length(input$supquali)==0){
        choixquali=NULL
      }
      else{
        data.selec=cbind(data.selec,newdata[,input$supquali])
        if(length(input$supquali)==1){
          choixquali=length(data.selec)
          colnames(data.selec)[choixquali]=input$supquali
        }
        else{
          choixquali=seq((dim(data.selec)[2]-length(input$supquali)+1),dim(data.selec)[2])
          colnames(data.selec)[choixquali]=input$supquali
        }
      }
    }
    if(length(input$supvar)==0){
      choixquanti=NULL
    }
    else {
      data.selec=cbind(data.selec,newdata[,input$supvar])
	  if(length(input$supvar)==1){
        choixquanti=length(data.selec)
        colnames(data.selec)[choixquanti]<-input$supvar
      }
      else{
        choixquanti=seq((dim(data.selec)[2]-length(input$supvar)+1),dim(data.selec)[2])
      }
    }
    if (length(input$habiller)==2 && input$habi==TRUE){
      data.selec <- data.frame(data.selec,newCol=paste(newdata[,input$habiller[1]],newdata[,input$habiller[2]],sep="/"))
      choixquali=c(choixquali,dim(data.selec)[2])
    }
    if(length(input$indsup)==0){
      suple=NULL
    }
    else{
      # suple=c()
      # for (i in 1:length(nom)){
        # if(nom[i]%in%input$indsup){
          # suple=c(suple,i)
        # }
      # }
	  suple=which(nom%in%input$indsup)
    }
    list(res.PCA=(PCA(data.selec,quali.sup=choixquali,quanti.sup=choixquanti,scale.unit=input$nor,graph=FALSE,ncp=max(5,as.numeric(input$nb1),as.numeric(input$nb2)),ind.sup=suple,row.w=poids1,col.w=poids2)),DATA=(data.selec),choixquant=(choixquanti),choixqual=(choixquali),choixsuple=(suple))
    })
    
    Plot1 <- reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
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
      if(input$select0==gettext("No selection")){
        selecindiv=NULL
        selecindivText="NULL"
      }
      if(input$select0=="contrib"){
        selecindiv=paste("contrib ",input$slider4)
        selecindivText=paste("'",selecindiv,"'",sep="")
      }
      if(is.null(input$colorsupvar)){
        colo="blue"
      }else{
        colo=input$colorsupvar
      }
      list(PLOT=(plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="var",select=selecindiv,unselect=0,col.quanti.sup=colo,col.var=input$coloractvar,cex=input$cex2,cex.main=input$cex2,cex.axis=input$cex2,title=input$title2)),SELECTION=(selecindiv),selecindivText=(selecindivText))
    })
    
    output$map <- renderPlot({
      p <- Plot1()$PLOT
    })
    
    observe({
      if(input$habi==FALSE){
      updateCheckboxInput(session, "elip", value = FALSE)
      }
    })
    
    Plot2 <- reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      validate(
        need(input$habiller == TRUE || input$habiller == FALSE || length(input$habiller)<=2,gettext("Please select maximum 2 variables as habillage"))
      )
      if(!is.null(input$elip)){
      validate(
        need(!(input$habi==FALSE&&input$elip==TRUE),"")
        )
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
      if(input$select==gettext("No selection")){
        selecindiv=NULL
        selecindivtext="NULL"
      }
      if(input$select=="contrib"){
        selecindiv=paste("contrib ",input$slider0)
        selecindivtext=paste0("'",selecindiv,"'")
      }
      if(input$select==gettext("Manual")){
        selecindiv=c(input$indiv)
      }
      if(input$supquali==FALSE || length(QualiChoice)==0 || length(input$supquali)==0 || input$habi==FALSE){
        hab="none"
#        colquali="magenta"
      }
      else if(length(QualiChoice)==1 && input$supquali==TRUE){
        if(input$habi==TRUE){
          hab=QualiChoice
#          colquali="blue"
        }
        else{
          hab="none"
#          colquali="magenta"
        }
      }
      else if (length(input$supquali)==1){
        if(input$habi==TRUE){
          hab=input$supquali
#          colquali="blue"
        }
        else{
          hab="none"
#          colquali="magenta"
        }
      }
      if(length(input$supquali)>1){
        if(length(input$habiller)==0){
          hab="none"
#          colquali="magenta"
        }
        if (length(input$habiller)==1 & input$habi==TRUE){
          hab=as.character(input$habiller)
#          colquali="blue"
        }
        if (length(input$habiller)==2 & input$habi==TRUE){
          hab=dim(values()$DATA)[2]
#          colquali="blue"
        }
      }
      
      if(input$select==gettext("Manual")){
        if(length(input$indiv)==0){
          selecindivtext="NULL"
        }
        if(length(input$indiv)>1){
          # vec<-NULL
          # vec<-paste(vec,"'",selecindiv[1],"'",sep="")
          # for (i in 2:(length(selecindiv))){
            # vec<-paste(vec,paste("'",selecindiv[i],"'",sep=""),sep=",")
          # }
		  vec<- paste("'",paste(selecindiv,collapse="','"),"'",sep="")
          selecindivtext<-paste("c(",vec,")",sep="")
        }
        else if (length(input$indiv)==1){
          selecindivtext=paste0("'",c(input$indiv),"'")
        }
      }
      if(!is.null(input$colorsup)){
        colors=input$colorsup
      }else{
        colors="blue"
      }
      if(!is.null(input$colorquali)){
        colorss=input$colorquali
      }else{
        colorss="magenta"
      }
      if(!is.null(input$elip)&&input$elip==TRUE){
        if(!is.null(values()$res.PCA$call$ind.sup)){
        aa=cbind.data.frame(values()$DATA[-c(values()$res.PCA$call$ind.sup),hab],values()$res.PCA$ind$coord)
        }else{
          aa=cbind.data.frame(values()$DATA[,hab],values()$res.PCA$ind$coord)
        }
        bb=coord.ellipse(aa,bar=TRUE)
        formula=plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="ind",cex=input$cex,cex.main=input$cex,cex.axis=input$cex,select=selecindiv,habillage=hab,title=input$title1,ellipse=bb,col.ind=input$coloract,col.ind.sup=colors,col.quali=colorss)
        
      }else{
        formula=plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="ind",cex=input$cex,cex.main=input$cex,cex.axis=input$cex,select=selecindiv,habillage=hab,title=input$title1,col.ind=input$coloract,col.ind.sup=colors,col.quali=colorss)
      }
      colquali=colorss
      list(PLOT=(formula),SELECTION2=(selecindiv),SELECTION3=(selecindivtext),HABILLAGE=(hab),colquali=(colorss),colindsup=(colors), text="")      
    })
   
    output$map2 <- renderPlot({
         p <- Plot2()$PLOT

    })
    
    output$colourn2=renderUI({
      sup=values()$choixsuple
      if(!is.null(sup)){
        if(!is.null(supind)){
        return(colourpicker::colourInput("colorsup", h6(gettext("Choose colour for supplementary individuals")), supind))
        }else{
          return(colourpicker::colourInput("colorsup", h6(gettext("Choose colour for supplementary individuals")), "blue")) 
        }
      }
    })
    
    output$colourn3=renderUI({
      sup=values()$choixqual
      if(!is.null(sup)){
        if(!is.null(categ)){
        return(colourpicker::colourInput("colorquali", h6(gettext("Choose colour for the categories")), categ))
        }else{
          return(colourpicker::colourInput("colorquali", h6(gettext("Choose colour for the categories")), "magenta"))
        }
      }
    })
    
    ### Bouton pour quitter l'application
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
      res$data=newdata
      res$a=values()$DATA
      if (length(QualiChoice)==1){
        if(input$supquali==FALSE){
          quali=NULL
        }
        else{
          quali=QualiChoice
        }
      }
      else{
        if(length(input$supquali)==0){
          quali=NULL
        }
        else{
          quali=input$supquali
        }
      }
      res$b=quali
      res$c=input$supvar
      res$d=input$indsup
      res$e=input$nb1
      res$f=input$nb2
      hab=NULL
      if(length(QualiChoice)==1 && input$supquali==TRUE){
        if(input$habi==TRUE){
          hab=QualiChoice
        }
      }
      else if (length(input$supquali)==1){
        if(input$habi==TRUE){
          hab=input$supquali
        }
      }
      if(length(input$supquali)>1){
        if (length(input$habiller)==1 & input$habi==TRUE){
          hab=as.character(input$habiller)
        }
        if (length(input$habiller)==2 & input$habi==TRUE){
          hab=input$habiller
        }
      }
      res$g=hab
      if(input$select=="cos2"){
        selecindiv=input$slider1
      }
      if(input$select==gettext("No selection")){
        selecindiv=NULL
      }
      if(input$select=="contrib"){
        selecindiv=input$slider0
      }
      if(input$select==gettext("Manual")){
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
      res$l=input$cex
      res$m=input$cex2
      res$code1=code()
      res$code2=codeGraphVar()
      if(!is.null(input$elip)&&input$elip==TRUE){
        res$codeellipse=codeellipses()
        phrase2="bb=coord.ellipse(aa,bary=TRUE)"
        res$codeellipse2=phrase2
      }
      res$code3=codeGraphInd()
      res$title1=input$title1
      res$title2=input$title2
      res$anafact=values()$res.PCA
      res$ellipses=input$elip
      res$supin=input$colorsup
      res$categ=input$colorquali
      res$activeind=input$coloract
      res$coloractvar=input$coloractvar
      res$colorsupvar=input$colorsupvar
      res$norme=input$nor
      res$poids1=values()$res.PCA$call$row.W
      res$poids2=values()$res.PCA$call$col.W
      class(res) <- "PCAshiny"
      return(res)
    }
    
    #### Fonction recuperation de code
    
    observe({
      if(input$PCAcode==0){
      }
      else {
        isolate({
          if (length(input$habiller)==2 & input$habi==TRUE){
            cat(paste("newCol<-paste(",nomData,"[,'",input$habiller[1],"'],",nomData,"[,'",input$habiller[2],"'],","sep='/')",sep=""),sep="\n")
          }
          cat(code(),sep="\n")
          cat(codeGraphVar(),sep="\n")
          if(!is.null(input$elip)&&input$elip==TRUE){
            cat(codeellipses(),sep="\n")
            phrase2="bb=coord.ellipse(aa,bary=TRUE)"
            cat(phrase2,sep="\n")
          }
          cat(codeGraphInd(),sep="\n")
        })
      }
    })
  
    
    code<-function(){
      vecquant<-values()$choixquant
      choixqual<-values()$choixqual
      Datasel<-values()$DATA
      indsupl<-values()$choixsuple
      data1=newdata
      data2=Datasel
      test=identical(data1,data2)
      # vec<-NULL
      # for (i in 1:length(colnames(Datasel))){
        # vec<-c(vec,colnames(Datasel)[i])
      # }
	  vec <-colnames(Datasel)
      # vec2<-NULL
      # vec2<-paste(vec2,"'",vec[1],"'",sep="")
      # for (i in 2:(length(vec))){
        # vec2<-paste(vec2,paste("'",vec[i],"'",sep=""),sep=",")
      # }
      vec2<-paste("'",paste(colnames(Datasel),collapse="','"),"'",sep="")
      if(test==FALSE){
      vecfinal<-paste(nomData,"[,c(",vec2,")","]",sep="")
      }else{
        vecfinal=nomData
      }
      
      # vec4<-NULL
      # vec4<-paste(vec4,vecquant[1],sep="")
      # for (i in 2:(length(vecquant))){
        # vec4<-paste(vec4,vecquant[i],sep=",")
      # }
      vec4 <- paste(vecquant,collapse=",")
      vecquant1<-paste("c(",vec4,")",sep="")
      vecquant2<-vecquant
      
      vecqual<-choixqual
      # vec5<-NULL
      # vec5<-paste(vec5,vecqual[1],sep="")
      # for (i in 2:(length(vecqual))){
        # vec5<-paste(vec5,vecqual[i],sep=",")
      # }
      vec5 <- paste(vecqual,collapse=",")
      vecqual1<-paste("c(",vec5,")",sep="")
      vecqual2<-vecqual
      
      # vecind<-NULL
      # vecind<-paste(vecind,indsupl[1],sep="")
      # for (i in 2:(length(indsupl))){
        # vecind<-paste(vecind,indsupl[i],sep=",")
      # }
      vecind <- paste(indsupl,collapse=",")
      vecind1<-paste("c(",vecind,")",sep="")
      vecind2<-indsupl
      vec<-vecfinal
      
      if(length(input$indsup)==0){
        indsupl<-"NULL"
      }
      else if(length(input$indsup)==1){
        indsupl<-vecind2
      }
      else if(length(input$indsup)>1){
        indsupl<-vecind1
      }
      
      
      if(length(input$supvar)>1){
        vecquant<-vecquant1
      }
      else if(length(input$supvar)==1){
        vecquant<-vecquant2
      }
      else if(length(input$supvar)==0){
        vecquant<-"NULL"
      }
      
      if (length(input$supquali)>1){ 
        vecqual<-vecqual1
      }
      if(length(QualiChoice)==1){
        if(input$supquali==TRUE){
          vecqual<-vecqual2 
        }
        else{
          vecqual<-"NULL"  
        }
      }
      
      else if(length(QualiChoice)>1){
        if(length(input$supquali)==1){
          vecqual<-vecqual2  
        }
        else if (length(input$supquali)>1){ 
          vecqual<-vecqual1
        }
        else if (length(input$supquali)==0){ 
          vecqual<-"NULL"
        }  
      }
      else if(length(QualiChoice)==0){
        vecqual<-"NULL"
      }
      if(!is.null(poids1)){
        prow=paste(",row.w=c(",paste(poids1,collapse=","),")",sep="")
      }
      if(!is.null(poids2)){
        pcol=paste(",col.w=c(",paste(poids2,collapse=","),")",sep="")
      }
      if(!is.null(poids1)&&!is.null(poids2)){
        Call1=as.name(paste("res.PCA<-PCA(",vec,",quali.sup=",vecqual,",","quanti.sup=",vecquant,",ind.sup=",indsupl,prow,pcol,",scale.unit=",input$nor,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep="")) 
      }else if (!is.null(poids1)&&is.null(poids2)){
        Call1=as.name(paste("res.PCA<-PCA(",vec,",quali.sup=",vecqual,",","quanti.sup=",vecquant,",ind.sup=",indsupl,prow,",scale.unit=",input$nor,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep="")) 
      }else if (is.null(poids1)&&!is.null(poids1)){
        Call1=as.name(paste("res.PCA<-PCA(",vec,",quali.sup=",vecqual,",","quanti.sup=",vecquant,",ind.sup=",indsupl,pcol,",scale.unit=",input$nor,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep="")) 
      }else{
      Call1=as.name(paste("res.PCA<-PCA(",vec,",quali.sup=",vecqual,",","quanti.sup=",vecquant,",ind.sup=",indsupl,",scale.unit=",input$nor,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep=""))
      }
      return(Call1)
    }
    
    
    codeGraphVar<-function(){
      
      if(length(input$slider4)==0){
        selection="NULL"
      }
      else{
        selection=Plot1()$selecindivText
      }
      if(is.null(input$colorsupvar)){
        colo="blue"
      }else{
        colo=input$colorsupvar
      }
      Call1=paste("plot.PCA(res.PCA,axes=c(",input$nb1,",",input$nb2,"),choix='var',select=",selection,",cex=",input$cex2,",cex.main=",input$cex2,",cex.axis=",input$cex2,",title='",input$title2,"',unselect=0,col.quanti.sup='",colo,"',col.var='",input$coloractvar,"')",sep="")
      return(Call1)
    }
    
    codeellipses=function(){
      Datasel<-values()$DATA
      indsupl<-values()$choixsuple
      
      vec<-NULL
      for (i in 1:length(colnames(Datasel))){
        vec<-c(vec,colnames(Datasel)[i])
      }
      vec2<-NULL
      vec2<-paste(vec2,"'",vec[1],"'",sep="")
      for (i in 2:(length(vec))){
        vec2<-paste(vec2,paste("'",vec[i],"'",sep=""),sep=",")
      }
      vecfinal<-paste(nomData,"[,c(",vec2,")","]",sep="")
      vec=vecfinal
      if(input$supquali==FALSE || length(QualiChoice)==0 || length(input$supquali)==0 || input$habi==FALSE){
        hab="none"
        colquali="magenta"
      }
      else if(length(QualiChoice)==1 && input$supquali==TRUE){
        if(input$habi==TRUE){
          hab=QualiChoice
          colquali="blue"
        }
        else{
          hab="none"
          colquali="magenta"
        }
      }
      else if (length(input$supquali)==1){
        if(input$habi==TRUE){
          hab=input$supquali
          colquali="blue"
        }
        else{
          hab="none"
          colquali="magenta"
        }
      }
      if(length(input$supquali)>1){
        if(length(input$habiller)==0){
          hab="none"
          colquali="magenta"
        }
        if (length(input$habiller)==1 & input$habi==TRUE){
          hab=as.character(input$habiller)
          colquali="blue"
        }
        if (length(input$habiller)==2 & input$habi==TRUE){
          hab=dim(values()$DATA)[2]
          colquali="blue"
        }
      }
      phrase1=paste("aa=cbind.data.frame(",vec,"[,'",hab,"'],res.PCA$ind$coord)",sep="")
      #phrase2="bb=coord.ellipse(aa,bar=TRUE)"
      #phrasefinal=paste(phrase1,phrase2,sep="\n")
      return(phrase1)
    }

    codeGraphInd<-function(){
      if (length(input$habiller)<=1 & input$habi==TRUE || input$habi==FALSE){
        hab=paste("'",Plot2()$HABILLAGE,"'",sep="")
      }
      else if (length(input$habiller)==2 & input$habi==TRUE){
        hab=Plot2()$HABILLAGE
      }
      if(!is.null(input$elip)&&input$elip==TRUE){
      Call2=paste("plot.PCA(res.PCA,","axes=c(",input$nb1,",",input$nb2,"),choix='ind',select=",Plot2()$SELECTION3,",habillage=",hab,",title='",input$title1,"',cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex,",col.ind='",input$coloract,"',col.ind.sup='",Plot2()$colindsup,"',col.quali='",Plot2()$colquali,"',ellipse=bb)",sep="")
      }else{
        Call2=paste("plot.PCA(res.PCA,","axes=c(",input$nb1,",",input$nb2,"),choix='ind',select=",Plot2()$SELECTION3,",habillage=",hab,",title='",input$title1,"',cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex,",col.ind='",input$coloract,"',col.ind.sup='",Plot2()$colindsup,"',col.quali='",Plot2()$colquali,"')",sep="")
      }
      return(Call2)
    }
    
    ##### Fin de la fonction recuperation du code
    
    
    output$out22=renderUI({
#      choix=list("Summary of PCA"="ACP","Eigenvalues"="eig","Results of the variables"="resvar","Results of the individuals"="resind")
      choix=list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results of the variables"),gettext("Results of the individuals"))
      if(!is.null(values()$choixsuple)){
#        choix=c(choix,"Results of the supplementary individuals"="supind")
        choix=c(choix,gettext("Results of the supplementary individuals"))
      }
      if(!is.null(values()$choixquant)){
#        choix=c(choix,"Results of the supplementary variables"="varsup")
        choix=c(choix,gettext("Results of the supplementary variables"))
      }
      if(!is.null(values()$choixqual)){
#        choix=c(choix,"Results of the categorical variables"="qualico")
        choix=c(choix,gettext("Results of the categorical variables"))
      }
      radioButtons("out",gettext("Which outputs do you want?"),
#                   choices=choix,selected="ACP",inline=TRUE)
                   choices=choix,selected=gettext("Summary of outputs"),inline=TRUE)
    })
    
    getactive=function(){
      if(input$selecactive==gettext("Choose")){
      sup=NULL
      if(length(input$supvar)==0){
        activevar=VariableChoices
      }
      else{
         # for (i in 1:length(VariableChoices)){
          # if(VariableChoices[i]%in%input$supvar){
            # sup=c(sup,i)
          # }
        # }
	    sup=which(VariableChoices%in%input$supvar)
	    if (length(sup)==0) sup=NULL
        activevar=VariableChoices[-sup]
      }
      return(activevar)
    }
  }
    
    
    output$NB1=renderUI({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      if(input$selecactive==gettext("All") || length(getactive())>5){
        # return(selectInput("nb1", label = h6(gettext("x axis")), 
                            # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5,"6"=6,"7"=7), selected = axe1,width='80%'))
        return(textInput("nb1", label = h6(gettext("x axis")), axe1,width='50%'))
      }
      else{
        baba=c(1:length(getactive()))
        return(selectInput("nb1",label=h6(gettext("x axis")), choices=baba,selected=axe1,width='80%'))
      }
    })
    
    output$NB2=renderUI({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      if(input$selecactive==gettext("All") || length(getactive())>5){
        # return(selectInput("nb2", label = h6(gettext("y axis")), 
                           # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5,"6"=6,"7"=7), selected = axe2,width='80%'))
        return(textInput("nb2", label = h6(gettext("y axis")), axe2,width='50%'))
      }
      else{
        baba=c(1:length(getactive()))
        return(selectInput("nb2",label=h6(gettext("y axis")), choices=baba,selected=axe2,width='80%'))
      }
    })
    
    output$sorties=renderTable({
        return(as.data.frame(values()$res.PCA$eig))
    },rownames=TRUE)
    
    output$sorties12=renderTable({
        validate(
          need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
        )
        validate(
          need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
        )
        return(as.data.frame(values()$res.PCA$quali.sup$coord))
    },rownames=TRUE)
    
    output$sorties13=renderTable({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      validate(
        need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
      )
      return(as.data.frame(values()$res.PCA$quali.sup$v.test))
    },rownames=TRUE)
    
    output$sorties2=renderTable({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.PCA$var$coord))
    },rownames=TRUE)
    
    output$sorties22=renderTable({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.PCA$ind$coord))
    },rownames=TRUE)
    
    output$sorties23=renderTable({
      validate(
        need(length(input$supvar)!=0, gettext("No supplementary quantitative variables"))
      )
      validate(
        need(length(input$getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.PCA$quanti.sup$coord))
    },rownames=TRUE)
    
    output$sorties32=renderTable({
      validate(
        need(length(input$supvar)!=0, gettext("No supplementary quantitative variables"))
      )
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.PCA$quanti.sup$cor))
    },rownames=TRUE)
    
    output$sorties36=renderTable({
      validate(
        need(length(input$indsup)!=0, gettext("No supplementary individuals"))
      )
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variables"))
      )
      return(as.data.frame(values()$res.PCA$ind.sup$coord))
    },rownames=TRUE)
    
    output$sorties37=renderTable({
      validate(
        need(length(input$indsup)!=0, gettext("No supplementary individuals"))
      )
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.PCA$ind.sup$cos2))
    },rownames=TRUE)
    
    
    output$sorties3=renderTable({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.PCA$var$contrib))
    },rownames=TRUE)
    
    output$sorties33=renderTable({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.PCA$ind$contrib))
    },rownames=TRUE)
    
    output$sorties4=renderTable({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.PCA$var$cos2))
    },rownames=TRUE)
    
    output$sorties44=renderTable({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      return(as.data.frame(values()$res.PCA$ind$cos2))
    },rownames=TRUE)
  
  output$sortieDimdesc3=renderTable({
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    validate(need(length(dimdesc(values()$res.PCA))>0,gettext("No quantitative variable describes axis 1")))
    return(as.data.frame(dimdesc(values()$res.PCA)[[1]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc4=renderTable({
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    validate(need(length(dimdesc(values()$res.PCA))>0,gettext("No categorical variable describes axis 1")))
    return(as.data.frame(dimdesc(values()$res.PCA)[[1]]$quali))
  },rownames=TRUE)
  
  #DIM2
  
  output$sortieDimdesc33=renderTable({
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    validate(need(length(dimdesc(values()$res.PCA))>1,gettext("No quantitative variable describes axis 2")))
    return(as.data.frame(dimdesc(values()$res.PCA)[[2]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc44=renderTable({
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    validate(need(length(dimdesc(values()$res.PCA))>1,gettext("No categorical variable describes axis 2")))
    return(as.data.frame(dimdesc(values()$res.PCA)[[2]]$quali))
  },rownames=TRUE)
  
  #DIM3
  
  output$sortieDimdesc333=renderTable({
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    validate(need(length(dimdesc(values()$res.PCA))>2,gettext("No quantitative variable describes axis 3")))
    return(as.data.frame(dimdesc(values()$res.PCA)[[3]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc444=renderTable({
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    validate(need(length(dimdesc(values()$res.PCA))>2,"No categorical variable describes axis 3"))
    return(as.data.frame(dimdesc(values()$res.PCA)[[3]]$quali))
  },rownames=TRUE)
    
    
    output$map3=renderPlot({
      return(barplot(values()$res.PCA$eig[,1],names.arg=rownames(values()$res.PCA$eig),las=2))
    })
    
    output$JDD=renderDataTable({
      cbind(Names=rownames(newdata),newdata)},
      options = list(    "orderClasses" = TRUE,
                         "responsive" = TRUE,
                         "pageLength" = 10))
  
    output$summary=renderPrint({
      summary(newdata)
    })
  
    output$summaryPCA=renderPrint({
      validate(
        need(input$nbele!=0, gettext("Please select at least one element"))
      )
      a<-values()$res.PCA
      a$call$call<-code()
      summary.PCA(a,nbelements=input$nbele)
    })
  
    output$summary2=downloadHandler(filename = function() { 
      paste('summaryofPCA','.txt', sep='') 
    },
    content = function(file) {
      summary.PCA(values()$res.PCA,nbelements=input$nbele,file=file)
    },
    contentType='text/csv')
  
    
    output$slider3=renderUI({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
      )
      if(input$selecactive==gettext("All")){
        maxvar=length(VariableChoices)
      }
      if(input$selecactive==gettext("Choose")){
        maxvar=length(getactive())
      }
      if(selection3=="contrib"){
        return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables"),
                                              min=1,max=maxvar,value=selection4,step=1)))  
      }
      else{
      return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables"),
                  min=1,max=maxvar,value=maxvar,step=1)))}
    })

    
    output$habillage2=renderUI({
      if(length(QualiChoice)==0 || input$supquali==FALSE || length(input$supquali)==0){
        return(p(gettext("No categorical variable")))
      }
      if(length(input$supquali)>1){
        if(is.null(habillageind)){
        num=c(1:length(input$supquali))
        return(selectInput("habiller",gettext("Select 1 or 2 variables"), choices=list(num=input$supquali),multiple=TRUE))
        }
        else{
          num=c(1:length(input$supquali))
          return(selectInput("habiller",gettext("Select 1 or 2 variables"), choices=list(num=input$supquali),multiple=TRUE,selected=habillageind))
        }
      }
    })

  output$ellipses=renderUI({
    #validate(need(!is.null(input$habiller),""))
    if(length(QualiChoice)==0 || input$supquali==FALSE || length(input$supquali)==0){
      return(p(" "))
    }else{
    return(checkboxInput("elip",gettext("Draw the confidence ellipses around the categories"),ellipses))
    }
  })
  
  output$varsu=renderUI({
    test=values()$choixquant
    if(!is.null(test)){
      if(!is.null(colorsupvar)){
        return(colourpicker::colourInput("colorsupvar", h6(gettext("Choose colour for supplementary variables")), colorsupvar))
      }else{
      return(colourpicker::colourInput("colorsupvar", h6(gettext("Choose colour for supplementary variables")), "blue"))
      }
    }
  })
	
    output$histo=renderPlot({
      par(mfrow=c(1,2))
      boxplot(newdata[,input$bam])
      hist(newdata[,input$bam],main="",xlab="")
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
    
    Plot11=function(){
      if(input$select0=="cos2"){
        if(input$slider00!=1){
          selecindiv=paste("cos2 ",input$slider00)
        }
        else{
          selecindiv="cos2 0.999"
        }
      }
      if(input$select0==gettext("No selection")){
        selecindiv=NULL
      }
      if(input$select0=="contrib"){
        selecindiv=paste("contrib ",input$slider4)
      }
      if(is.null(input$colorsupvar)){
        colo="blue"
      }else{
        colo=input$colorsupvar
      }
      plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="var",select=selecindiv,unselect=0,col.quanti.sup=colo,cex=input$cex2,cex.main=input$cex2,cex.axis=input$cex2,title=input$title2,col.var=input$coloractvar)
    }
    Plot22=function(){
      if(input$select=="cos2"){
        if(input$slider1!=1){
          selecindiv=paste("cos2 ",input$slider1)
        }
        else{
          selecindiv="cos2 0.999"
        }
      }
      if(input$select==gettext("No selection")){
        selecindiv=NULL
      }
      if(input$select=="contrib"){
        selecindiv=paste("contrib ",input$slider0)
      }
      if(input$select==gettext("Manual")){
        selecindiv=c(input$indiv)
      }
      if(input$supquali==FALSE || length(QualiChoice)==0 || length(input$supquali)==0 || input$habi==FALSE){
        hab="none"
        colquali="magenta"
      }
      else if(length(QualiChoice)==1 && input$supquali==TRUE){
        if(input$habi==TRUE){
          hab=QualiChoice
          colquali="blue"
        }
        else{
          hab="none"
          colquali="magenta"
        }
      }
      else if (length(input$supquali)==1){
        if(input$habi==TRUE){
          hab=input$supquali
          colquali="blue"
        }
        else{
          hab="none"
          colquali="magenta"
        }
      }
      if(length(input$supquali)>1){
        if(length(input$habiller)==0){
          hab="none"
          colquali="magenta"
        }
        if (length(input$habiller)==1 & input$habi==TRUE){
          hab=as.character(input$habiller)
          colquali="blue"
        }
        if (length(input$habiller)==2 & input$habi==TRUE){
          hab=dim(values()$DATA)[2]
          colquali="blue"
        }
      }
      if(!is.null(input$colorsup)){
        colors=input$colorsup
      }else{
        colors="blue"
      }
      if(!is.null(input$colorquali)){
        colorss=input$colorquali
      }else{
        colorss="magenta"
      }
      if(!is.null(input$elip)&&input$elip==TRUE){
        aa=cbind.data.frame(values()$DATA[,hab],values()$res.PCA$ind$coord)
        bb=coord.ellipse(aa,bar=TRUE)
        plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="ind",cex=input$cex,cex.main=input$cex,cex.axis=input$cex,select=selecindiv,habillage=hab,col.quali=colorss,col.ind.sup=colors,title=input$title1,ellipse=bb,col.ind = input$coloract)
        
      }else{
        plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="ind",cex=input$cex,cex.main=input$cex,cex.axis=input$cex,select=selecindiv,habillage=hab,col.quali=colorss,col.ind.sup=colors,title=input$title1,col.ind = input$coloract)
      }
      #plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="ind",cex=input$cex,cex.main=input$cex,cex.axis=input$cex,select=selecindiv,habillage=hab,col.quali=colquali,col.ind.sup="blue",title=input$title1)    
    }
    
  }
)
      

