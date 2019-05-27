# server script for MCA2

  function(input, output,session) {
    
    #Realisation de l'ACM    
    values=reactive({
      
      if (input$selecactive==gettext("All")){
        data.selec=newdataMCAshiny[,VariableChoicesMCAshiny]
      }
      else{
        validate(
          need(getactive()!= "", gettext("Please select active variables"))
        )
        data.selec=newdataMCAshiny[,c(getactive())]
      }
      
      
      if(length(QuantiChoiceMCAshiny)==0){
        choixquanti=NULL
      }
      else if (length(QuantiChoiceMCAshiny)==1){
        if(input$supquanti==FALSE){
          choixquanti=NULL
        }
        else{
          data.selec=cbind(data.selec,newdataMCAshiny[,QuantiChoiceMCAshiny])
          colnames(data.selec)[dim(data.selec)[2]]=QuantiChoiceMCAshiny
          #Renomme les colonnes
          choixquanti=length(data.selec)
        }
      }
      #Si plusieurs quanti existent
      else{
        if(length(input$supquanti)==0){
          choixquanti=NULL
        }
        else{
          data.selec=cbind(data.selec,newdataMCAshiny[,input$supquanti])
          if(length(input$supquanti)==1){
            choixquanti=length(data.selec)
            colnames(data.selec)[choixquanti]=input$supquanti
          }
          else{
            choixquanti=seq((dim(data.selec)[2]-length(input$supquanti)+1),dim(data.selec)[2])
            colnames(data.selec)[choixquanti]=input$supquanti
          }
        }
      }
      if(length(input$supvar)==0){
        choixquali=NULL
      }
      else {
        data.selec=cbind(data.selec,newdataMCAshiny[,input$supvar])
        if(length(input$supvar)==1){
          choixquali=length(data.selec)
          #modif
          colnames(data.selec)[choixquali]=input$supvar
        }
        else{
          choixquali=seq((dim(data.selec)[2]-length(input$supvar)+1),dim(data.selec)[2])
        }
      }
      if (length(input$habiller)==2){
        data.selec <- data.frame(data.selec,newCol=paste(newdataMCAshiny[,input$habiller[1]],newdataMCAshiny[,input$habiller[2]],sep="/"))
        choixquali=c(choixquali,dim(data.selec)[2])
      }
      
      if(is.null(input$indsup)){
        indsuplem<-NULL
      }
      else {
	    indsuplem=which(rownames(newdataMCAshiny)%in%input$indsup)
      }
      list(res.MCA=(MCA(data.selec,quanti.sup=choixquanti,quali.sup=choixquali,ind.sup=indsuplem,graph=FALSE,ncp=max(5,as.numeric(input$nb1),as.numeric(input$nb2)))),DATA=(data.selec),choixquant=(choixquanti),choixqual=(choixquali),indsup=(indsuplem))     
    })
    
    output$col1=renderUI({
      sup=values()$indsup
      if(!is.null(sup)){
        if(is.null(color2MCAshiny)){
          return(colourpicker::colourInput("colindsup",h6(gettext("Colour of supplementary individuals")),"darkblue"))
        }else{
          return(colourpicker::colourInput("colindsup",h6(gettext("Colour of supplementary individuals")),color2MCAshiny))
        }
      }
    })
    
    output$col2=renderUI({
      sup=values()$choixqual
      if(!is.null(sup)){
        if(is.null(color4MCAshiny)){
          return(colourpicker::colourInput("colvarsup",h6(gettext("Colour of supplementary categories")),"darkgreen"))
        }else{
          return(colourpicker::colourInput("colvarsup",h6(gettext("Colour of supplementary categories")),color4MCAshiny))
        }
      }
    })
    
    observe({
      if(input$MCAcode==0){
      }
      else {
        isolate({
          if (length(input$habiller)==2 & input$habi==TRUE){
            cat(paste("newCol=paste(",nomDataMCAshiny,"['",input$habiller[1],"'],x[,'",input$habiller[2],"'],sep='/'))",sep=""),sep="\n")
          }
          cat(code(),sep="\n")
          cat(codeGraphVar(),sep="\n")
          cat(codeGraphInd(),sep="\n")
          
          if((length(values()$choixquant)!=0)){
            cat(codeGraphQuanti(),sep="\n") 
          }
        })
      }
    })
    
    createVec=function(arg){
      vec<-NULL
      vec<-paste(vec,arg[1],sep="")
      for (i in 2:(length(arg))){
        vec<-paste(vec,arg[i],sep=",")
      }
      vec<-paste("c(",vec,")",sep="")
      return(vec)
    }
    
    code<-function(){
      vecquant<-values()$choixquant
      choixqual<-values()$choixqual
      Datasel<-values()$DATA
      indsup<-values()$indsup
      
      
      # vec<-NULL
      # for (i in 1:length(colnames(Datasel))){
        # vec<-c(vec,colnames(Datasel)[i])
      # }
      vec <- colnames(Datasel)
      vec<-paste0("'",vec,"'")
      vec <- paste("c(",paste(vec,collapse=","),")",sep="")
#      vec<-createVec(vec)
      
#      vecquant1<-createVec(vecquant)
      vecquant1 <- paste("c(",paste(vecquant,collapse=","),")",sep="")
      vecquant2<-vecquant
      
      vecqual<-choixqual
#      vecqual1<-createVec(vecqual)
      vecqual1 <- paste("c(",paste(vecqual,collapse=","),")",sep="")
      vecqual2<-vecqual
            
#      indsup1<-createVec(indsup)
      indsup1 <- paste("c(",paste(indsup,collapse=","),")",sep="")
      indsup2<-indsup
      
      if(length(input$supvar)>1){
        vecqual<-vecqual1
      }
      else if(length(input$supvar)==1){
        vecqual<-vecqual2
      }
      else if(length(input$supvar)==0){
        vecqual<-"NULL"
      }
      
      if(length(input$indsup)==0){
        indsuplem<-"NULL"
      }
      else if(length(input$indsup)==1){
        indsuplem<-indsup2
      }
      else if(length(input$indsup)>1){
        indsuplem<-indsup1
      }
      
      if(length(QuantiChoiceMCAshiny)==0){
        vecquant<-"NULL"
      }
      
      else if(length(QuantiChoiceMCAshiny)==1){
        if(input$supquanti==TRUE){
          vecquant<-vecquant2 
        }
        else{
          vecquant<-"NULL"  
        }
      }
      
      else if(length(QuantiChoiceMCAshiny)>1){
        if(length(input$supquanti)==1){
          vecquant<-vecquant2  
        }
        else if (length(input$supquanti)>1){ 
          vecquant<-vecquant1
        }
        else if (length(input$supquanti)==0){ 
          vecquant<-"NULL"
        }  
      }
      Call1=as.name(paste("res.MCA<-MCA(",nomDataMCAshiny,"[,",vec,"],quali.sup=",vecqual,",","quanti.sup=",vecquant,",ind.sup=",indsuplem,",graph=FALSE, ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep=""))  
      return(Call1)
    }
    
    
    codeGraphVar<-function(){
      Call2=paste('plot.MCA(res.MCA,choix="var",invisible=',Plot4()$invisible,',title="',input$title2MCAshiny,'",axes=c(',as.numeric(input$nb1),',',as.numeric(input$nb2),'))',sep='')  
      return(Call2)
    }
    
    codeGraphInd<-function(){
      hab=Plot1()$habill
      if(input$eachvar==TRUE){
      colouract2=paste("c(",paste(Plot1()$colouract2,collapse = ","),")",sep="")
      colouract2=as.character(colouract2)
      }else{
        colouract2=paste("'",Plot1()$colouract2,"'",sep="")
      }
      if(hab!="none"){
        Call3=cat(paste('plot.MCA(res.MCA,choix="ind",invisible=',Plot1()$inv,',axes=c(',as.numeric(input$nb1),',',as.numeric(input$nb2),'),selectMod=',Plot1()$selm,',selec=',Plot1()$sel,',habillage=',Plot1()$hab,',title="',input$title1MCAshiny,'",col.quali="',Plot1()$colquali,'",col.var=',colouract2,',col.ind="',Plot1()$colouract,'",col.ind.sup="',Plot1()$colindsup,'")',sep=''),'\n',paste('plotellipses(res.MCA,keepvar="',hab,'")',sep=''),'\n')
        
      }else{
      Call3=paste('plot.MCA(res.MCA,choix="ind",invisible=',Plot1()$inv,',axes=c(',as.numeric(input$nb1),',',as.numeric(input$nb2),'),selectMod=',Plot1()$selm,',selec=',Plot1()$sel,',habillage=',Plot1()$hab,',title="',input$title1MCAshiny,'",col.quali="',Plot1()$colquali,'",col.var=',colouract2,',col.ind="',Plot1()$colouract,'",col.ind.sup="',Plot1()$colindsup,'")',sep='')
      }
      return(Call3)
    }
    
    codeGraphQuanti<-function(){
      Call4=paste("plot.MCA(res.MCA,axes=c(",as.numeric(input$nb1),",",as.numeric(input$nb2),"),choix='quanti.sup',title='",input$title3MCAshiny,"')",sep="")
      return(Call4)
    }
    
    ### Recuperation des parametres
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
      res$nomDataMCAshiny=nomDataMCAshiny
      res$data=newdataMCAshiny
      res$a=values()$DATA#data of the factorial analysis
      class(res)<-"MCAshiny"#Class of the result
      
      #Supplementary quantitative variables selected
      if (length(QuantiChoiceMCAshiny)==1){
        if(input$supquanti==FALSE){
          quantiMCAshiny=NULL
        }
        else{
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
      
      res$c=input$supvar#suplementary qualitative variables
      res$z=input$var_sup#1st graph multiple choice selected
      res$y=input$ind_var#2nd graph multiple choice selected
      res$lab=input$indvarpoint
      res$d=input$indsup#supplementary individuals selected
      
      res$e=input$nb1#axes selected
      res$f=input$nb2#
      
      #Selected habillage
      if(length(input$supvar)==0 || input$habi==FALSE){
        hab="none"
      }
      
      if(length(input$supvar)>1){
        if(length(input$habiller)==0){
          hab="none"
        }
        
        if (length(input$habiller)==1 & input$habi==TRUE){
          hab=as.character(input$habiller)
        }
        
        if (length(input$habiller)==2 & input$habi==TRUE){
          hab=dim(values()$DATA)[2]
        }
      }
      else if (length(input$supvar)==1){
        if(input$habi==TRUE){
          hab=values()$choixqual
        }
        else{
          hab="none"
        }
      } 
     
      res$g=hab
      
      #Selection for individuals
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
      res$h=input$select#Type of selections
      res$i=selecindiv#selection
    
    #Selection for modalities
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
    res$code1=code()
    res$code2=codeGraphVar()
    res$code3=codeGraphInd()
    if((length(values()$choixquant)!=0)){
      res$code4=codeGraphQuanti() 
    }
    else{
      res$code4=NULL
    }
    res$title1MCAshiny=input$title1MCAshiny
    res$title2MCAshiny=input$title2MCAshiny
    res$title3MCAshiny=input$title3MCAshiny
    res$anafact=values()$res.MCA
    res$color1MCAshiny=input$colindact
    res$color2MCAshiny=input$colindsup
    res$color3MCAshiny=input$colvaract
    res$color4MCAshiny=input$colvarsup
    res$color5MCAshiny=input$colvaract1
    res$color6MCAshiny=input$colvarsup1
    res$color7MCAshiny=input$colquanti
    res$color8MCAshiny=input$colli
    res$hcpcparam <- input$hcpcparam
    res$nbdimclustPCAshiny <- input$nbDimClustering
    return(res)
    }
    
    #Getactive
    getactive=function(){
      if(input$selecactive==gettext("Choose")){
        sup=NULL
        if(length(input$supvar)==0){
          activevar=VariableChoicesMCAshiny
        }
        else{
	      sup=which(VariableChoicesMCAshiny%in%input$supvar)
          activevar=VariableChoicesMCAshiny[-sup]
        }
        return(activevar)
      }
    }
    
    output$choixindvar=renderUI({
      choix=list(gettext("Individuals"),gettext("Categories"))
      if(!(is.null(input$indsup))){
        choix=c(choix,gettext("Supplementary individuals"))
      }
      if(!(is.null(input$supvar))){
        choix=c(choix,gettext("Supplementary categories"))
      }
      div(align="center",checkboxGroupInput("ind_var","", choices=choix,
                                                   selected = indvarMCAshiny))
    })
    
    output$pointlabel=renderUI({
      validate(
        need(!is.null(input$ind_var),""))
      choix=list()
      reponse=input$ind_var
      if(sum(gettext("Individuals")==reponse)==0){
        choix=c(choix,gettext("Individuals"))
      }
      if(sum(gettext("Categories")==reponse)==0){
        choix=c(choix,gettext("Categories"))
      }
      if(sum(gettext("Supplementary individuals")==reponse)==0){
        choix=c(choix,gettext("Supplementary individuals"))
      }
      if(sum(gettext("Supplementary categories")==reponse)==0){
        choix=c(choix,gettext("Supplementary categories"))
      }
      div(align="center",checkboxGroupInput("indvarpoint","",choices=choix,selected=labvarMCAshiny))
    })
    
    output$out22=renderUI({
      choix=list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results of the variables"),gettext("Results of the individuals"))
      if(!is.null(values()$indsup)){
        choix=c(choix,gettext("Results of the supplementary individuals"))
      }
      if(!is.null(values()$choixquant)){
        choix=c(choix,gettext("Results of the supplementary quantitative variables"))
      }
      if(!is.null(values()$choixqual)){
        choix=c(choix,gettext("Results of the supplementary categorical variables"))
      }
      radioButtons("out",gettext("Which outputs do you want?"),
                   choices=choix,selected=gettext("Summary of outputs"),inline=TRUE)
    })
    
    output$colquanti12=renderUI({
      if(is.null(color7MCAshiny)){
        return(colourpicker::colourInput("colquanti",h6(gettext("Colour of supplementary quantitative variables")),"blue"))
      }else{
        return(colourpicker::colourInput("colquanti",h6(gettext("Colour of supplementary quantitative variables")),color7MCAshiny))
      }
    })
    
    output$colquantib=renderUI({
      sup=values()$choixquant
      if(!is.null(sup)){
        if(is.null(color8MCAshiny)){
          return(colourpicker::colourInput("colli",h6(gettext("Colour of supplementary quantitative variables")),"blue"))
        }else{
          return(colourpicker::colourInput("colli",h6(gettext("Colour of supplementary quantitative variables")),color8MCAshiny))
        }
      }
      
    })
    #Getinv
    getinv=function(){
      
      inv<-c()
      # if(!("Ind"%in%input$ind_var)){
        # inv<-c(inv,"ind")
      # }
      # if(!("Mod"%in%input$ind_var)){
        # inv<-c(inv,"var")
      # }
#      if(!(gettext("Individuals")%in%input$ind_var)){
      if(sum(gettext("Individuals")==input$ind_var)==0){
        inv<-c(inv,"ind")
      }
      
#      if(!(gettext("Categories")%in%input$ind_var)){
      if(sum(gettext("Categories")==input$ind_var)==0){
        inv<-c(inv,"var")
      }
      if(!(is.null(values()$choixqual))){
#      if(!(gettext("Supplementary categories")%in%input$ind_var)){
      if(sum(gettext("Supplementary categories")==input$ind_var)==0){
        inv<-c(inv,"quali.sup")
      }
      }
      if(!(is.null(values()$indsup))){
#      if(!(gettext("Supplementary individuals")%in%input$ind_var)){
      if(sum(gettext("Supplementary individuals")==input$ind_var)==0){
        inv<-c(inv,"ind.sup")
      }
      }
      # vecinv<-NULL
      # vecinv<-paste("'",vecinv,inv[1],"'",sep="")
      # for (i in 2:(length(inv))){
        # vecinv<-paste(vecinv,paste("'",inv[i],"'",sep=""),sep=",")
      # }
      vecinv <- paste("'",paste(inv,collapse="','"),"'",sep="")
      if(length(inv)>1){
        vecinv<-paste("c(",vecinv,")",sep="")
      }
      else if(length(inv)==1){
        vecinv<-paste("'",inv,"'",sep="")
      }
      else if(length(inv)==0){
        vecinv<-"NULL"
      }
      
      list(inv=(inv),vecinv=(vecinv))
    }
    
    getinv2=function(){
      inv<-c()
      if(sum(gettext("Supplementary qualitative variables")==input$var_sup)==0){
#      if(!(gettext("Supplementary qualitative variables")%in%input$var_sup)){
        inv<-c(inv,"quali.sup")
      }
      
      if(sum(gettext("Supplementary quantitative variables")==input$var_sup)==0){
#      if(!(gettext("Supplementary quantitative variables")%in%input$var_sup)){
        inv<-c(inv,"quanti.sup")
      }
      
      if(sum(gettext("Active qualitative variables")==input$var_sup)==0){
#      if(!(gettext("Active qualitative variables")%in%input$var_sup)){
        inv<-c(inv,"var")
      }
      
      # vecinv<-NULL
      # vecinv<-paste("'",vecinv,inv[1],"'",sep="")
      # for (i in 2:(length(inv))){
        # vecinv<-paste(vecinv,paste("'",inv[i],"'",sep=""),sep=",")
      # }
      vecinv <- paste("'",paste(inv,collapse="','"),"'",sep="")
      
      if(length(inv)>1){
        vecinv<-paste("c(",vecinv,")",sep="")
      }
      else if(length(inv)==1){
        vecinv<-paste("'",inv,"'",sep="")
      }
      else if(length(inv)==0){
        vecinv<-"NULL"
      }
      
      list(inv=(inv),vecinv=(vecinv))
    }
    
    #GRAPHIQUE 3: Variables
    
    Plot4=reactive({
      validate(
        need(input$nb1 != input$nb2, "Please select two different dimensions")
      )
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),"Please select more variable")
      )
      inv=getinv2()$inv
      invtext=getinv2()$vecinv
      if(is.null(input$colvaract1)){
        coll1="red"
      }else{
        coll1=input$colvaract1
      }
      if(is.null(input$colvarsup1)){
        coll2="darkgreen"
      }else{
        coll2=input$colvarsup1
      }
      if(is.null(input$colli)){
        coll3="blue"
      }else{
        coll3=input$colli
      }
      list(PLOT4=(plot.MCA(values()$res.MCA,choix="var",invisible=inv,title=input$title2MCAshiny,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),col.var=coll1,col.quali.sup = coll2,col.quanti.sup=coll3,cex=input$cex2,cex.main=input$cex2,cex.axis=input$cex2)),invisible=(invtext))    
    })
    
    output$map4 <- renderPlot({
      p <- Plot4()$PLOT4
    })
    
    output$col3=renderUI({
      sup=values()$choixqual
      if(!is.null(sup)){
        if(is.null(color6MCAshiny)){
          return(colourpicker::colourInput("colvarsup1",h6(gettext("Colour of supplementary categorical variables")),"darkgreen"))
        }else{
          return(colourpicker::colourInput("colvarsup1",h6(gettext("Colour of supplementary categorical variables")),color6MCAshiny))
        }
      }
    })
    
    #GRAPIQUE 1   
    
    Plot1=reactive({
      
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      
      validate(
        need(length(input$ind_var)>=1,gettext("Please select the object you want to plot: Individuals, variables or both"))
      )
      validate(
        need(input$habiller == TRUE || input$habiller == FALSE || length(input$habiller)<=2,gettext("Please select maximum 2 variables as habillage"))
      )
      
      
      #Selection des individus
      if(input$select==gettext("Manual")){
        selecindiv=c(input$indiv) 
        selecindivText=createVec(selecindiv)
      }
      else if(input$select=="cos2"){
        if(input$slider1!=1){
          selecindiv=paste("cos2",input$slider1)
        }
        else{
          selecindiv="cos2 0.999"
        }
        selecindivText=paste0("'",selecindiv,"'")
      }
      else if(input$select=="Contrib"){
        selecindiv=paste("contrib ",input$sliderContrib) 
        selecindivText=paste0("'",selecindiv,"'")
      }
      else if(input$select==gettext("No selection")){
        selecindiv=NULL
        selecindivText="NULL"
      }
      
      #Selection des modalites
      
      if(input$selectMod=="cos2"){
        if(input$sliderCosMod!=1){
          selecMod=paste("cos2",input$sliderCosMod)
        }
        else{
          selecMod="cos2 0.999"
        }
        selecModText=paste0("'",selecMod,"'")
      }
      else if(input$selectMod=="Contrib"){
        selecMod=paste("contrib ",input$slider4)
        selecModText=paste0("'",selecMod,"'")
      }
      else if(input$selectMod==gettext("No selection")){
        selecMod=NULL
        selecModText="NULL"
      }
      
      
      if(length(input$supvar)==0 || input$habi==FALSE){
        hab="none"
        habText<-"'none'"
        colquali="magenta"
      }
      
      if(length(qualiMCAshiny)>1){
        if(length(input$habiller)==0){
          hab="none"
          habText<-"'none'"
          colquali="magenta"
        }
        
        if (length(input$habiller)==1 & input$habi==TRUE){
          hab=as.character(input$habiller)
          habText<-paste("'",input$habiller,"'",sep="")
          colquali="blue"
        }
        
        if (length(input$habiller)==2 & input$habi==TRUE){
          hab=dim(values()$DATA)[2]
          habText<-hab
          colquali="blue"
        }
      }
      ###
#       else if (length(input$supvar)==1){
#         if(input$habi==TRUE){
#           hab=values()$choixqual
#           habText<-hab
#           colquali="blue"
#         }
#         else{
#           hab="none"
#           habText<-"'non'"
#           colquali="magenta"
#         }
#       }
      ###
      
      validate(
        need(length(input$ind_var)!="",gettext("Please select which object you would like to print"))
      )
      choixText="NULL"
      if(is.null(input$ind_var)){
        inv="none"
      }else{
      inv<-getinv()$inv
      }
      invText<-getinv()$vecinv
      sel<-selecindiv
      selm<-selecMod
      colindsup<-"darkgreen"
      if(is.null(input$colindact)){
        colouract="blue"
      }else{
        colouract=input$colindact
      }
      if(is.null(input$colvaract)){
        colouract2="red"
      }else{
        colouract2=input$colvaract
      }
      if(is.null(input$colindsup)){
        coloursup="darkblue"
      }else{
        coloursup=input$colindsup
      }
      if(is.null(input$colvarsup)){
        coloursup2="darkgreen"
      }else{
        coloursup2=input$colvarsup
      }
      if(input$eachvar==TRUE){
        colouract2=rep(1:length(c(values()$res.MCA$call$quali,values()$res.MCA$call$quali.sup)),unlist(lapply(values()$res.MCA$call$X[,c(values()$res.MCA$call$quali,values()$res.MCA$call$quali.sup)],nlevels)))
      }
      list(PLOT1=(plot.MCA(values()$res.MCA,choix="ind",invisible=inv,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),selectMod=selm,selec=sel,habillage=hab,col.var=colouract2,col.ind.sup=coloursup,title=input$title1MCAshiny,col.ind=colouract,col.quali.sup = coloursup2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex)),choix=(choixText),inv=(invText),selm=(selecModText),sel=(selecindivText),hab=(habText),colquali=(colquali),colindsup=(colindsup),habill=(hab),colouract2=(colouract2),colouract=(colouract))  
    })
    
    output$map <- renderPlot({
      hab=Plot1()$habill
      if(is.null(input$habi)||input$habi==FALSE){
        p=Plot1()$PLOT1
      }else{
      if(!is.null(input$drawconf)&&input$drawconf==TRUE){
        plotellipses(values()$res.MCA,keepvar=hab)
      }else if (!is.null(input$drawconf)&&input$drawconf==FALSE){
        p=Plot1()$PLOT1
      }
      }
    })
    
    output$ellips=renderUI({
      hab=input$habiller
      if(length(hab)>0){
        return(checkboxInput("drawconf","Draw confidence ellipses around center of caregories",FALSE))
      }
    })
    
    
    observe({
      x_even <- input$habi
      if(input$habi==FALSE){
      updateCheckboxInput(session, "drawconf", value = FALSE)
      }
    })
    
    
#     output$widgetind=renderUI({
#       if(!("Individuals"%in%input$ind_var)){
#         return()
#       }
#     })
    
    #GRAPHIQUE 2
    
    Plot2=function(){
      if(is.null(input$colquanti)){
        colquanti="blue"
      }else{
        colquanti=input$colquanti
      }
      plot.MCA(values()$res.MCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="quanti.sup",title=input$title3MCAshiny,col.quanti.sup=colquanti,cex=input$cex3,cex.main=input$cex3,cex.axis=input$cex3) 
    }
    
    output$map2 <- renderPlot({
      p=Plot2()
    })  
    
    output$map22=renderUI({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      ) 
      validate(
        need(input$selecactive==gettext("All") || length(getactive())>2,gettext("Please select quantitative variable and more active variables"))
      )
      
      if(length(values()$choixquant)==0){
        return(p())
      }
      else{
        plotOutput("map2", width = 500, height=500)
      }
    })
    ####
    
    
    output$choixchange=renderUI({
      if(length(values()$choixquant)==0){
        return(radioButtons("MCAgraph",h6(gettext("Which graph do you want to modify?")),
#                            choices=list("Individuals and categories"="ind","Variables"="var"),inline=TRUE))
                            choices=list(gettext("Individuals and categories"),"Variables"="var"),inline=TRUE))
      }
      else{
        return(radioButtons("MCAgraph",h6(gettext("Which graph do you want to modify?")),
#                            choices=list("Individuals and categories"="ind","Variables"="var","Quantitative variables"="quant"),inline=TRUE))
                            choices=list(gettext("Individuals and categories"),"Variables"="var",gettext("Quantitative variables")),inline=TRUE))
      }
    })
    
    
       
    output$habillage2=renderUI({
      if(is.null(habillageindMCAshiny)){
        num=c(1:length(qualiMCAshiny))
        return(selectInput("habiller","Select 1 or 2 variables", choices=list(num=qualiMCAshiny),multiple=TRUE))
      }
      else{
        num=c(1:length(qualiMCAshiny))
        return(selectInput("habiller","Select 1 or 2 variables", choices=list(num=qualiMCAshiny),multiple=TRUE,selected=habillageindMCAshiny))
      }
      #}
    }) 
    
    #CALCUL DE LA CONTRIBUTION DES MODALITES
    
    output$slider3=renderUI({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
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
      return(barplot(values()$res.MCA$eig[,1],names.arg=rownames(values()$res.MCA$eig),las=2,density=TRUE))
    })
    
    #Histogramme du summary
    output$histo=renderPlot({
      barplot(prop.table(table(newdataMCAshiny[,input$bam]))*100)
    })
    
    #Summary de l'ACM
    
    
    output$summaryMCA=renderPrint({
      validate(
        need(input$nbele!=0, gettext("Please select at least one element"))
      )
      a<-values()$res.MCA  
      a$call$call<-code()
      
      summary.MCA(a,nbelements=input$nbele)
    })
    
    output$summary2=downloadHandler(filename = function() { 
      paste('summaryofMCA','.txt', sep='') 
    },
    content = function(file) {
      summary.MCA(values()$res.MCA,nbelements=input$nbele,file=file)
    },
    contentType='text/csv')
    
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
      fluidRow(
        tags$head(
          tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle; } 
                     #inline .form-group { display: table-row;}")
          ),
        return(tags$div(id = "inline", numericInput(inputId = "nbDimClustering", label = gettext("Number of dimensions kept for clustering:"),value=nbdimclustMCAshiny,min=1)))
      )
    }
  })

    #autre
    
    output$sorties=renderTable({
      return(as.data.frame(values()$res.MCA$eig))
    },rownames=TRUE)
    
    output$sorties2=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      return(as.data.frame(values()$res.MCA$var$coord))
    },rownames=TRUE)
    
    output$sorties3=renderTable({
      
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      return(as.data.frame(values()$res.MCA$var$contrib))
    },rownames=TRUE)
    
    output$sorties4=renderTable({
      
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      return(as.data.frame(values()$res.MCA$var$cos2))
    },rownames=TRUE)
    
    output$sorties22=renderDataTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      tab<-as.data.frame(values()$res.MCA$ind$coord)
      tab<-round(tab, 3)
      tab<-cbind(Names=rownames(tab),tab)
      return(tab)
    })
    
    output$sorties33=renderDataTable({
      
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      tab1<-as.data.frame(values()$res.MCA$ind$contrib)
      tab1<-round(tab1,3)
      tab1<-cbind(Names=rownames(tab1),tab1)
      return(tab1)
    })
    
    output$sorties44=renderDataTable({
      
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      tab2<-as.data.frame(values()$res.MCA$ind$cos2)
      tab2<-round(tab2,3)
      tab2<-cbind(Names=rownames(tab2),tab2)
      return(tab2)
    })
    
    output$sorties23=renderTable({
      
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      validate(
        need(length(input$supvar)!=0, gettext("No supplementary categorical variables"))
      )
      return(as.data.frame(values()$res.MCA$quali.sup$coord))
    })
    
    output$sorties232=renderTable({
      
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      validate(
        need(length(input$supvar)!=0, gettext("No supplementary categorical variables"))
      )
      return(as.data.frame(values()$res.MCA$quali.sup$cos2))
    },rownames=TRUE)
    
    output$sorties233=renderTable({
      
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      validate(
        need(length(input$supvar)!=0, gettext("No supplementary categorical variables"))
      )
      return(as.data.frame(values()$res.MCA$quali.sup$v.test))
    },rownames=TRUE)
    
    output$sorties43=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      validate(
        need(length(input$supquanti)!=0 || input$supquanti==TRUE, gettext("No supplementary quantitative variables"))
      )
      return(as.data.frame(values()$res.MCA$quanti.sup$coord))
    },rownames=TRUE)
    
    output$sortiesIsupC=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      validate(
        need(length(input$indsup)!=0,gettext("No supplementary individuals"))
      )
      return(as.data.frame(values()$res.MCA$ind.sup$coord))
    })
    
    output$sortiesIsupCos=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      validate(
        need(length(input$indsup)!=0,gettext("No supplementary individuals"))
      )
      return(as.data.frame(values()$res.MCA$ind.sup$cos2))
    },rownames=TRUE)
    
    #DIM1
    
    output$sortieDimdesc=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[1]]$category))
    },rownames=TRUE)
    
    output$sortieDimdesc2=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[1]]$quali))
    },rownames=TRUE)
    output$sortieDimdesc3=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      validate(
        need(length(input$supquanti)>0,gettext("No quantitative variable")))
      validate(
        need(length(dimdesc(values()$res.MCA)[[1]]$quanti)!=0,"")
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[1]]$quanti))
    },rownames=TRUE)
    
    #DIM2
    output$sortieDimdesc00=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[2]]$category))
    },rownames=TRUE)
    output$sortieDimdesc22=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[2]]$quali))
    },rownames=TRUE)
    output$sortieDimdesc33=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      validate(
        need(length(dimdesc(values()$res.MCA)[[2]]$quanti)!=0,"")
      )
      validate(
        need(length(input$supquanti)>0,gettext("No quantitative variable")))
      return(as.data.frame(dimdesc(values()$res.MCA)[[2]]$quanti))
    },rownames=TRUE)
    
    #DIM3
    output$sortieDimdesc000=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[3]]$category))
    },rownames=TRUE)
    output$sortieDimdesc222=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      return(as.data.frame(dimdesc(values()$res.MCA)[[3]]$quali))
    },rownames=TRUE)
    output$sortieDimdesc333=renderTable({
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variable"))
      )
      validate(
        need(length(dimdesc(values()$res.MCA)[[3]]$quanti)!=0,"")
      )
      validate(
        need(length(input$supquanti)>0,gettext("No quantitative variable")))
      return(as.data.frame(dimdesc(values()$res.MCA)[[3]]$quanti))
    },rownames=TRUE)
    
    #Le JDDONNEES
    output$JDD=renderDataTable({
      cbind(Names=rownames(newdataMCAshiny),newdataMCAshiny)},
      
      options = list(    "orderClasses" = TRUE,
                         "responsive" = TRUE,
                         "pageLength" = 10))
    ####
  observe({
    if(input$Investigatehtml!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveMCAshiny)
        if (gettext(input$choixLANG)==gettext("English")) FactoInvestigate::Investigate(values()$res.MCA, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="en")
        if (gettext(input$choixLANG)==gettext("French")) FactoInvestigate::Investigate(values()$res.MCA, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="fr")
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$Investigatedoc!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveMCAshiny)
        if (gettext(input$choixLANG)==gettext("English")) FactoInvestigate::Investigate(values()$res.MCA,document="word_document",openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="en")
        if (gettext(input$choixLANG)==gettext("French")) FactoInvestigate::Investigate(values()$res.MCA,document="word_document",openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="fr")
        setwd(path.aux)
      })
    }
  })
  

  observe({
    if(input$InvestigateRmd!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveMCAshiny)
	    if (gettext(input$choixLANG)==gettext("English")) FactoInvestigate::Investigate(values()$res.MCA, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="en")
	    if (gettext(input$choixLANG)==gettext("French")) FactoInvestigate::Investigate(values()$res.MCA, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="fr")
        setwd(path.aux)
      })
    }
  })
    
    output$downloadData0 = downloadHandler(
      filename = function() { 
        paste('graph4','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot44()
        dev.off()
      },
      contentType='image/png')
    
    output$downloadData10 = downloadHandler(
      filename = function() { 
        paste('graph4','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot44()
        dev.off()
      },
      contentType='image/jpg')
    
    output$downloadData20 = downloadHandler(
      filename = function() { 
        paste('graph4','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot44()
        dev.off()
      },
      contentType=NA)
    
    ####
    
    
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
        
        hab=Plot1()$habill
        if(!is.null(input$drawconf)&&input$drawconf==TRUE){
          plotellipses(values()$res.MCA,keepvar=hab)
        }else if (!is.null(input$drawconf)&&input$drawconf==FALSE){
          p <- Plot11()
        }
        dev.off()
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
        paste('graph2','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot2()
        dev.off()
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
        paste('graph1','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot2()
        dev.off()
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
        paste('graph1','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot2()
        dev.off()
      },
      contentType=NA)    
    
    ####AXES
    
    output$NB1=renderUI({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variables"))
      )
      if(input$selecactive==gettext("All") || length(getactive())>5){
        return(textInput("nb1", label = h6(gettext("x axis")), axe1MCAshiny,width='50%'))
      }
      else{
        baba=c(1:length(getactive()))
        return(selectInput("nb1",label=h6(gettext("x axis")), choices=baba,selected=axe1MCAshiny,width='80%'))
      }
    })
    
    
    
    output$NB2=renderUI({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variables"))
      )
      if(input$selecactive==gettext("All") || length(getactive())>5){
        return(textInput("nb2", label = h6(gettext("y axis")), axe2MCAshiny,width='50%'))
      }
      else{
        baba=c(1:length(getactive()))
        return(selectInput("nb2",label=h6(gettext("y axis")), choices=baba,selected=axe2MCAshiny,width='80%'))
      }
    })
    
    Plot11=function(){
      if(input$select==gettext("Manual")){
      selecindiv=c(input$indiv) 
    }
    else if(input$select=="cos2"){
      if(input$slider1!=1){
        selecindiv=paste("cos2",input$slider1)
      }
      else{
        selecindiv="cos2 0.999"
      }
    }
    else if(input$select=="Contrib"){
      selecindiv=paste("contrib ",input$sliderContrib) 
    }
    else if(input$select==gettext("No selection")){
      selecindiv=NULL
    }
    
    if(input$selectMod=="cos2"){
      if(input$sliderCosMod!=1){
        selecMod=paste("cos2",input$sliderCosMod)
      }
      else{
       selecMod="cos2 0.999" 
      }
    }
    else if(input$selectMod=="Contrib"){
      selecMod=paste("contrib ",input$slider4)
    }
    else if(input$selectMod==gettext("No selection")){
      selecMod=NULL
    }
    
    
    if(length(input$supvar)==0 || input$habi==FALSE){
      hab="none"
      habText<-"'none'"
      colquali="magenta"
    }
    
    if(length(input$supvar)>1){
      if(length(input$habiller)==0){
        hab="none"
        habText<-"'none'"
        colquali="magenta"
      }
      
      if (length(input$habiller)==1 & input$habi==TRUE){
        hab=as.character(input$habiller)
        habText<-paste("'",input$habiller,"'",sep="")
        colquali="blue"
      }
      
      if (length(input$habiller)==2 & input$habi==TRUE){
        hab=dim(values()$DATA)[2]
        habText<-hab
        colquali="blue"
      }
    }
    ###
    else if (length(input$supvar)==1){
      if(input$habi==TRUE){
        hab=values()$choixqual
        habText<-hab
        colquali="blue"
      }
      else{
        hab="none"
        habText<-"'non'"
        colquali="magenta"
      }
    }
    choixText="NULL"
    inv<-getinv()$inv
    invText<-getinv()$vecinv
    sel<-selecindiv
    selm<-selecMod
    colindsup<-"darkgreen"
    if(is.null(input$colindact)){
      colouract="blue"
    }else{
      colouract=input$colindact
    }
    if(is.null(input$colvaract)){
      colouract2="red"
    }else{
      colouract2=input$colvaract
    }
    if(is.null(input$colindsup)){
      coloursup="darkblue"
    }else{
      coloursup=input$colindsup
    }
    if(is.null(input$colvarsup)){
      coloursup2="darkgreen"
    }else{
      coloursup2=input$colvarsup
    }
    if(input$eachvar==TRUE){
      colouract2=rep(1:length(c(values()$res.MCA$call$quali,values()$res.MCA$call$quali.sup)),unlist(lapply(values()$res.MCA$call$X[,c(values()$res.MCA$call$quali,values()$res.MCA$call$quali.sup)],nlevels)))
    }
    plot.MCA(values()$res.MCA,choix="ind",title=as.character(input$title1MCAshiny),invisible=inv,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),selectMod=selm,selec=sel,habillage=hab,col.var=colouract2,col.ind.sup=coloursup,col.ind=colouract,col.quali.sup = coloursup2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex)}
    
    Plot44=function(){
      inv=getinv2()$inv
    if(is.null(input$colvaract1)){
      coll1="red"
    }else{
      coll1=input$colvaract1
    }
    if(is.null(input$colvarsup1)){
      coll2="darkgreen"
    }else{
      coll2=input$colvarsup1
    }
    plot.MCA(values()$res.MCA,choix="var",title=input$title2MCAshiny,invisible=inv,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),col.var=coll1,col.quali.sup = coll2,cex=input$cex2,cex.main=input$cex2,cex.axis=input$cex2)
    }
    
  }
