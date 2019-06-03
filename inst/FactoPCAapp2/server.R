function(input, output,session) {
  values <- reactive({
    if (input$selecactive==gettext("All")){
      data.selec <- newdataPCAshiny[,VariableChoicesPCAshiny]
    }
    else{
      validate(
        need(length(input$supvar)>0, gettext("Please select at least one supplementary variable"))
      )
      data.selec <- newdataPCAshiny[,c(getactive())]
    }
    
    ## pour avoir les indices des variables quanti (pb si variables quali pas dans le jeu de donnees)
    #    if (!is.null(input$supvar)) cat(paste0("c(",paste0(which(colnames(newdataPCAshiny)%in%input$supvar),collapse=","),")"))
    
    if(length(QualiChoicePCAshiny)==0){
      choixquali <- NULL
    }
    else if (length(QualiChoicePCAshiny)==1){
      if(input$supquali==FALSE){
        choixquali <- NULL
      }
      else{
        data.selec <- cbind(data.selec,newdataPCAshiny[,QualiChoicePCAshiny])
        colnames(data.selec)[ncol(data.selec)] <- QualiChoicePCAshiny
        choixquali <- length(data.selec)
      }
    }
    else{
      if(length(input$supquali)==0){
        choixquali <- NULL
      }
      else{
        data.selec <- cbind(data.selec,newdataPCAshiny[,input$supquali])
        if(length(input$supquali)==1){
          choixquali <- length(data.selec)
          colnames(data.selec)[choixquali] <- input$supquali
        }
        else{
          choixquali <- seq((dim(data.selec)[2]-length(input$supquali)+1),dim(data.selec)[2])
          colnames(data.selec)[choixquali] <- input$supquali
        }
      }
    }
    #numQualiNotChosen  <- which(colnames(newdataPCAshiny)%in%setdiff(QualiChoice,input$supquali))
    #don <- newdataPCAshiny[,-numQualiNotChosen]
    #numQualiChosen <- which(colnames(don)%in%input$supquali)
    #numQuantiChosen <- which(colnames(don)%in%input$supvar)
    #numHab  <- which(colnames(newdataPCAshiny)%in%input$habi)
    
    #resu <- paste0("res <- PCA(don",if (length(numQualiNotChosen)!=0) paste0("[,-c(",paste0(numQualiNotChosen,collapse=","),")]"), if (length(numQuantiChosen)!=0) paste0(",quanti.sup=c(",paste0(numQuantiChosen,collapse=","),")"), if (length(numQualiChosen)!=0) paste0(",quali.sup=c(",paste0(numQualiChosen,collapse=","),")"),")")
    #cat(paste(numQualiNotChosen," zz ",numQualiChosen," yy ",numQuantiChosen))
    #cat(resu)
    #cat("\naaa\n")
    #sortie <- eval(parse(text=resu))
    #cat(sortie$eig)
    #cat("\nttt\n")
    #cat(res$eig)
    
    if(length(input$supvar)==0){
      choixquanti <- NULL
    }
    else {
      data.selec <- cbind(data.selec,newdataPCAshiny[,input$supvar])
      if(length(input$supvar)==1){
        choixquanti <- length(data.selec)
        colnames(data.selec)[choixquanti]<-input$supvar
      }
      else{
        choixquanti <- seq((ncol(data.selec)-length(input$supvar)+1),ncol(data.selec))
      }
    }
    if (length(input$habiller)==2 && input$habi==TRUE){
      data.selec <- data.frame(data.selec,newCol=paste(newdataPCAshiny[,input$habiller[1]],newdataPCAshiny[,input$habiller[2]],sep="/"))
      choixquali <- c(choixquali,ncol(data.selec))
    }
    if(length(input$indsup)==0){
      suple <- NULL
    }
    else{
      suple <- which(nomPCAshiny%in%input$indsup)
    }
    if(input$hcpcparam==FALSE){
      list(res.PCA=(PCA(data.selec,quali.sup=choixquali,quanti.sup=choixquanti,scale.unit=input$nor,graph=FALSE,ncp=max(5,as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering)),ind.sup=suple,row.w=poids1PCAshiny,col.w=poids2PCAshiny)),DATA=(data.selec),choixquant=(choixquanti),choixqual=(choixquali),choixsuple=(suple))
    } else{
      list(res.PCA=(PCA(data.selec,quali.sup=choixquali,quanti.sup=choixquanti,scale.unit=input$nor,graph=FALSE,ncp=max(2,as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering)),ind.sup=suple,row.w=poids1PCAshiny,col.w=poids2PCAshiny)),DATA=(data.selec),choixquant=(choixquanti),choixqual=(choixquali),choixsuple=(suple))
    }
    list(res.PCA=(PCA(data.selec,quali.sup=choixquali,quanti.sup=choixquanti,scale.unit=input$nor,graph=FALSE,ncp=max(5,as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering)),ind.sup=suple,row.w=poids1PCAshiny,col.w=poids2PCAshiny)),DATA=(data.selec),choixquant=(choixquanti),choixqual=(choixquali),choixsuple=(suple))
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
        selecindiv <- paste("cos2 ",input$slider00)
      }
      else{
        selecindiv <- "cos2 0.999"
      }
      selecindivText <- paste("'",selecindiv,"'",sep="")
    }
    if(input$select0==gettext("No selection")){
      selecindiv <- NULL
      selecindivText <- "NULL"
    }
    if(input$select0=="contrib"){
      selecindiv <- paste("contrib ",input$slider4)
      selecindivText <- paste("'",selecindiv,"'",sep="")
    }
    if(is.null(input$colorsupvarPCAshiny)){
      colo <- "blue"
    }else{
      colo <- input$colorsupvarPCAshiny
    }
    list(PLOT=(plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="var",select=selecindiv,unselect=0,col.quanti.sup=colo,col.var=input$coloractvarPCAshiny,cex=input$cex2,cex.main=input$cex2,cex.axis=input$cex2,title=input$title2)),SELECTION=(selecindiv),selecindivText=(selecindivText))
  })
  
  output$map <- renderPlot({
    p <- Plot1()$PLOT
  })
  
  output$choixindmod <- renderUI({
    choix <- gettext("Individuals")
    bool1 <- FALSE
    if(!(is.null(input$indsup))){
      choix <- c(choix,gettext("Supplementary individuals"))
      bool1 <- TRUE
    }
    if (length(input$supquali)>0){
      if(length(QualiChoicePCAshiny)>0 & (input$supquali!=FALSE)){
        choix <- c(choix,gettext("Supplementary categories"))
        bool1 <- TRUE
      }}
    if(bool1==TRUE){
      div(align="left",checkboxGroupInput("ind_mod","", choices=choix, selected = indmodPCAshiny))
    }
  })
  
  output$pointlabel <- renderUI({
    validate(need(!is.null(input$ind_mod),""))
    choix <- list()
    reponse <- input$ind_mod
    if(sum(gettext("Individuals")==reponse)==0){
      choix <- c(choix,gettext("Individuals"))
    }
    if(sum(gettext("Supplementary individuals")==reponse)==0){
      choix <- c(choix,gettext("Supplementary individuals"))
    }
    if(sum(gettext("Supplementary categories")==reponse)==0){
      choix <- c(choix,gettext("Supplementary categories"))
    }
    div(align="center",checkboxGroupInput("indmodpoint","",choices=choix,selected=labmodPCAshiny))
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
        selecindiv <- paste("cos2 ",input$slider1)
      }
      else{
        selecindiv <- "cos2 0.999"
      }
      selecindivtext <- paste0("'",selecindiv,"'")
    }
    if(input$select==gettext("No selection")){
      selecindiv <- NULL
      selecindivtext <- "NULL"
    }
    if(input$select=="contrib"){
      selecindiv <- paste("contrib ",input$slider0)
      selecindivtext <- paste0("'",selecindiv,"'")
    }
    if(input$select==gettext("Manual")){
      selecindiv <- c(input$indiv)
    }
    if(input$supquali==FALSE || length(QualiChoicePCAshiny)==0 || length(input$supquali)==0 || input$habi==FALSE){
      hab <- "none"
    }
    else if(length(QualiChoicePCAshiny)==1 && input$supquali==TRUE){
      if(input$habi==TRUE){
        hab <- QualiChoicePCAshiny
      }
      else{
        hab <- "none"
      }
    }
    else if (length(input$supquali)==1){
      if(input$habi==TRUE){
        hab <- input$supquali
      }
      else{
        hab <- "none"
      }
    }
    if(length(input$supquali)>1){
      if(length(input$habiller)==0){
        hab <- "none"
      }
      if (length(input$habiller)==1 & input$habi==TRUE){
        hab <- as.character(input$habiller)
      }
      if (length(input$habiller)==2 & input$habi==TRUE){
        hab <- dim(values()$DATA)[2]
      }
    }
    
    if(input$select==gettext("Manual")){
      if(length(input$indiv)==0){
        selecindivtext <- "NULL"
      }
      if(length(input$indiv)>1){
        vec<- paste("'",paste(selecindiv,collapse="','"),"'",sep="")
        selecindivtext<-paste("c(",vec,")",sep="")
      }
      else if (length(input$indiv)==1){
        selecindivtext <- paste0("'",c(input$indiv),"'")
      }
    }
    if(!is.null(input$colorsup)){
      colors <- input$colorsup
    }else{
      colors <- "blue"
    }
    if(!is.null(input$colorquali)){
      colorss <- input$colorquali
    }else{
      colorss <- "magenta"
    }
    
    inv <- "none"
    if(!is.null(input$ind_mod)) inv<-getinv()$inv
    if(!is.null(input$elip)&&input$elip==TRUE){
      if(!is.null(values()$res.PCA$call$ind.sup)){
        aa <- cbind.data.frame(values()$DATA[-c(values()$res.PCA$call$ind.sup),hab],values()$res.PCA$ind$coord)
      }else{
        aa <- cbind.data.frame(values()$DATA[,hab],values()$res.PCA$ind$coord)
      }
      bb <- coord.ellipse(aa,bar=TRUE)
      formula <- plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="ind",invisible=inv,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,select=selecindiv,habillage=hab,title=input$title1,ellipse=bb,col.ind=input$coloract,col.ind.sup=colors,col.quali=colorss)
    }else{
      formula <- plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="ind",invisible=inv,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,select=selecindiv,habillage=hab,title=input$title1,col.ind=input$coloract,col.ind.sup=colors,col.quali=colorss)
    }
    colquali <- colorss
    list(PLOT=(formula),INV=(inv),SELECTION2=(selecindiv),SELECTION3=(selecindivtext),HABILLAGE=(hab),colquali=(colorss),colindsup=(colors), text="")      
  })
  
  output$map2 <- renderPlot({
    p <- Plot2()$PLOT
    
  })
  
  output$colourn2 <- renderUI({
    sup <- values()$choixsuple
    if(!is.null(sup)){
      if(!is.null(supindPCAshiny)){
        return(colourpicker::colourInput("colorsup", h6(gettext("Choose colour for supplementary individuals")), supindPCAshiny))
      }else{
        return(colourpicker::colourInput("colorsup", h6(gettext("Choose colour for supplementary individuals")), "blue")) 
      }
    }
  })
  
  output$colourn3 <- renderUI({
    sup <- values()$choixqual
    if(!is.null(sup)){
      if(!is.null(categPCAshiny)){
        return(colourpicker::colourInput("colorquali", h6(gettext("Choose colour for the categories")), categPCAshiny))
      }else{
        return(colourpicker::colourInput("colorquali", h6(gettext("Choose colour for the categories")), "magenta"))
      }
    }
  })
  
  ### Bouton pour quitter l'application
  ### Recuperation parametres
  observe({
    if(input$Quit!=0){
      isolate({
        stopApp(returnValue=valeuretour())
      })
    }
  })
  
  
  valeuretour <- function(){
    res <- list()
    res$nomDataPCAshiny <- nomDataPCAshiny
    res$data <- newdataPCAshiny
    res$a <- values()$DATA
    if (length(QualiChoicePCAshiny)==1){
      if(input$supquali==FALSE){
        qualiPCAshiny <- NULL
      }
      else{
        qualiPCAshiny <- QualiChoicePCAshiny
      }
    }
    else{
      if(length(input$supquali)==0){
        qualiPCAshiny <- NULL
      }
      else{
        qualiPCAshiny <- input$supquali
      }
    }
    res$b <- qualiPCAshiny
    res$c <- input$supvar
    res$d <- input$indsup
    res$y <- input$ind_mod
    res$e <- input$nb1
    res$f <- input$nb2
    hab <- NULL
    if(length(QualiChoicePCAshiny)==1 && input$supquali==TRUE){
      if(input$habi==TRUE){
        hab <- QualiChoicePCAshiny
      }
    }
    else if (length(input$supquali)==1){
      if(input$habi==TRUE){
        hab <- input$supquali
      }
    }
    if(length(input$supquali)>1){
      if (length(input$habiller)==1 & input$habi==TRUE){
        hab <- as.character(input$habiller)
      }
      if (length(input$habiller)==2 & input$habi==TRUE){
        hab <- input$habiller
      }
    }
    res$g <- hab
    if(input$select=="cos2"){
      selecindiv <- input$slider1
    }
    if(input$select==gettext("No selection")){
      selecindiv <- NULL
    }
    if(input$select=="contrib"){
      selecindiv <- input$slider0
    }
    if(input$select==gettext("Manual")){
      selecindiv <- input$indiv
    }
    res$h <- input$select
    res$i <- selecindiv
    selecindiv2 <- NULL
    if(input$select0=="cos2"){
      selecindiv2 <- input$slider00
    }
    if(input$select0=="contrib"){
      selecindiv2 <- input$slider4
    }
    res$j <- input$select0
    res$k <- selecindiv2
    res$l <- input$cex
    res$m <- input$cex2
    res$code1 <- code()
    res$code2 <- codeGraphVar()
    if(!is.null(input$elip)&&input$elip==TRUE){
      res$codeellipse <- codeellipses()
      phrase2 <- "bb <- coord.ellipse(aa,bary=TRUE)"
      res$codeellipse2 <- phrase2
    }
    res$code3 <- codeGraphInd()
    res$title1 <- input$title1
    res$title2 <- input$title2
    res$anafact <- values()$res.PCA
    res$ellipsesPCAshiny <- input$elip
    res$supin <- input$colorsup
    res$categPCAshiny <- input$colorquali
    res$activeindPCAshiny <- input$coloract
    res$coloractvarPCAshiny <- input$coloractvarPCAshiny
    res$colorsupvarPCAshiny <- input$colorsupvarPCAshiny
    res$normePCAshiny <- input$nor
    res$poids1PCAshiny <- values()$res.PCA$call$row.W
    res$poids2PCAshiny <- values()$res.PCA$call$col.W
    ## ADD for clustering
    res$hcpcparam <- input$hcpcparam
    res$nbdimclustPCAshiny <- input$nbDimClustering
    class(res) <- "PCAshiny"
    return(res)
  }
  
  getinv <- function(){
    
    inv<-c()
    if(sum(gettext("Individuals")==input$ind_mod)==0){
      inv<-c(inv,"ind")
    }
    
    if(!(is.null(values()$choixqual))){
      if(sum(gettext("Supplementary categories")==input$ind_mod)==0){
        inv<-c(inv,"quali")
      }
    }
    if(!(is.null(values()$choixsuple))){
      if(sum(gettext("Supplementary individuals")==input$ind_mod)==0){
        inv<-c(inv,"ind.sup")
      }
    }
    #      vecinv <- paste("'",paste(inv,collapse="','"),"'",sep="")
    #      if(length(inv)>1){
    #        vecinv<-paste("c(",vecinv,")",sep="")
    #      }
    #      else if(length(inv)==1){
    #        vecinv<-paste("'",inv,"'",sep="")
    #      }
    #      else if(length(inv)==0){
    #        vecinv<-"NULL"
    #      }
    #      list(inv=(inv),vecinv=(vecinv))
    list(inv=inv)
  }
  
  #### Fonction recuperation de code
  
  observe({
    if(input$PCAcode!=0){
      isolate({
        if (length(input$habiller)==2 & input$habi==TRUE){
          cat(paste("newCol<-paste(",nomDataPCAshiny,"[,'",input$habiller[1],"'],",nomDataPCAshiny,"[,'",input$habiller[2],"'],","sep='/')",sep=""),sep="\n")
        }
        cat(code(),sep="\n")
        cat(codeGraphVar(),sep="\n")
        if(!is.null(input$elip)&&input$elip==TRUE){
          cat(codeellipses(),sep="\n")
          phrase2 <- "bb <- coord.ellipse(aa,bary=TRUE)"
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
    indsuplPCAshiny<-values()$choixsuple
    test <- identical(newdataPCAshiny,Datasel)
    vec <-colnames(Datasel)
    vec2<-paste("'",paste(colnames(Datasel),collapse="','"),"'",sep="")
    if(test==FALSE){
      vecfinal<-paste(nomDataPCAshiny,"[,c(",vec2,")","]",sep="")
    }else{
      vecfinal <- nomDataPCAshiny
    }
    
    vec4 <- paste(vecquant,collapse=",")
    vecquant1<-paste("c(",vec4,")",sep="")
    vecquant2<-vecquant
    
    vecqual<-choixqual
    vec5 <- paste(vecqual,collapse=",")
    vecqual1<-paste("c(",vec5,")",sep="")
    vecqual2<-vecqual
    
    vecind <- paste(indsuplPCAshiny,collapse=",")
    vecind1<-paste("c(",vecind,")",sep="")
    vecind2<-indsuplPCAshiny
    vec<-vecfinal
    
    if(length(input$indsup)==0){
      indsuplPCAshiny<-"NULL"
    }
    else if(length(input$indsup)==1){
      indsuplPCAshiny<-vecind2
    }
    else if(length(input$indsup)>1){
      indsuplPCAshiny<-vecind1
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
    if(length(QualiChoicePCAshiny)==1){
      if(input$supquali==TRUE){
        vecqual<-vecqual2 
      }
      else{
        vecqual<-"NULL"  
      }
    }
    
    else if(length(QualiChoicePCAshiny)>1){
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
    else if(length(QualiChoicePCAshiny)==0){
      vecqual<-"NULL"
    }
    if(!is.null(poids1PCAshiny)){
      prow <- paste(",row.w=c(",paste(poids1PCAshiny,collapse=","),")",sep="")
    }
    if(!is.null(poids2PCAshiny)){
      pcol <- paste(",col.w=c(",paste(poids2PCAshiny,collapse=","),")",sep="")
    }
    
    Call1 <- as.name(paste0("res.PCA<-PCA(",vec,if(vecqual!="NULL") paste0(",quali.sup=",vecqual),if(vecquant!="NULL") paste0(",quanti.sup=",vecquant),if(indsuplPCAshiny!="NULL") paste0(",ind.sup=",indsuplPCAshiny),if (!is.null(poids1PCAshiny)) prow,if (!is.null(poids2PCAshiny)) pcol,if(input$nor!="TRUE") paste0(",scale.unit=",input$nor),if (!is.null(input$nbDimClustering)) paste0(",ncp=",max(as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))),if(max(5,as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))!=5) paste0(",ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))),",graph=FALSE)"))
    return(Call1)
  }
  
  
  codeGraphVar<-function(){
    
    if(length(input$slider4)==0){
      selectionPCAshiny <- "NULL"
    }
    else{
      selectionPCAshiny <- Plot1()$selecindivText
    }
    if(is.null(input$colorsupvarPCAshiny)){
      colo <- "blue"
    }else{
      colo <- input$colorsupvarPCAshiny
    }
    Call1 <- paste("plot.PCA(res.PCA,axes=c(",input$nb1,",",input$nb2,"),choix='var'",if(selectionPCAshiny!="NULL") paste0(",select=",selectionPCAshiny,",unselect=0"),if (input$cex2!=1) paste0(",cex=",input$cex2,",cex.main=",input$cex2,",cex.axis=",input$cex2),if(input$title2!="Variables factor map (PCA)") paste0(",title='",input$title2),if (colo!="blue"&colo!="#0000FF") paste0(",col.quanti.sup='",colo,"'"),if(input$coloractvarPCAshiny!="#000000") paste0(",col.var='",input$coloractvarPCAshiny,"'"),")",sep="")
    return(Call1)
  }
  
  codeellipses <- function(){
    Datasel<-values()$DATA
    indsuplPCAshiny<-values()$choixsuple
    
    vec<-NULL
    for (i in 1:length(colnames(Datasel))){
      vec<-c(vec,colnames(Datasel)[i])
    }
    vec2<-NULL
    vec2<-paste(vec2,"'",vec[1],"'",sep="")
    for (i in 2:(length(vec))){
      vec2<-paste(vec2,paste("'",vec[i],"'",sep=""),sep=",")
    }
    vecfinal<-paste(nomDataPCAshiny,"[,c(",vec2,")","]",sep="")
    vec <- vecfinal
    if(input$supquali==FALSE || length(QualiChoicePCAshiny)==0 || length(input$supquali)==0 || input$habi==FALSE){
      hab <- "none"
      colquali <- "magenta"
    }
    else if(length(QualiChoicePCAshiny)==1 && input$supquali==TRUE){
      if(input$habi==TRUE){
        hab <- QualiChoicePCAshiny
        colquali <- "blue"
      }
      else{
        hab <- "none"
        colquali <- "magenta"
      }
    }
    else if (length(input$supquali)==1){
      if(input$habi==TRUE){
        hab <- input$supquali
        colquali <- "blue"
      }
      else{
        hab <- "none"
        colquali <- "magenta"
      }
    }
    if(length(input$supquali)>1){
      if(length(input$habiller)==0){
        hab <- "none"
        colquali <- "magenta"
      }
      if (length(input$habiller)==1 & input$habi==TRUE){
        hab <- as.character(input$habiller)
        colquali <- "blue"
      }
      if (length(input$habiller)==2 & input$habi==TRUE){
        hab <- dim(values()$DATA)[2]
        colquali <- "blue"
      }
    }
    phrase1 <- paste("aa <- cbind.data.frame(",vec,"[,'",hab,"'],res.PCA$ind$coord)",sep="")
    return(phrase1)
  }
  
  codeGraphInd<-function(){
    if(is.null(input$ind_mod)){
      inv <- "none"
    }else{
      inv<-getinv()$inv
    }
    
    if (length(input$habiller)<=1 & input$habi==TRUE || input$habi==FALSE){
      hab <- paste("'",Plot2()$HABILLAGE,"'",sep="")
    }
    else if (length(input$habiller)==2 & input$habi==TRUE){
      hab <- Plot2()$HABILLAGE
    }
    Call2 <- paste0("plot.PCA(res.PCA,","axes=c(",input$nb1,",",input$nb2,")",if(!is.null(Plot2()$INV)){if (Plot2()$INV[1]!="none") paste0(",invisible=c(",paste0("'",paste(Plot2()$INV,collapse="','"),"'"),")")}, if(Plot2()$SELECTION3!="NULL"){paste0(",select=",Plot2()$SELECTION3)},if (hab!="'none'"){paste0(",habillage=",hab)},if(input$title1!="Individuals factor map (PCA)")paste0(",title='",input$title1,"'"),if(input$cex!=1)paste0(",cex=",input$cex,",cex.main=",input$cex,",cex.axis=",input$cex),if(input$coloract!="#000000")paste0(",col.ind='",input$coloract),if(Plot2()$colindsup!="blue"&Plot2()$colindsup!="#0000FF")paste0(",col.ind.sup='",Plot2()$colindsup,"'"),if(Plot2()$colquali!="magenta"&Plot2()$colquali!="#FF00FF")paste0(",col.quali='",Plot2()$colquali,"'"),if(!is.null(input$elip)&&input$elip==TRUE)",ellipse=bb",")")
    return(Call2)
  }
  
  ##### Fin de la fonction recuperation du code
  
  
  output$out22 <- renderUI({
    choix <- list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results of the variables"),gettext("Results of the individuals"))
    if(!is.null(values()$choixsuple)){
      choix <- c(choix,gettext("Results of the supplementary individuals"))
    }
    if(!is.null(values()$choixquant)){
      choix <- c(choix,gettext("Results of the supplementary variables"))
    }
    if(!is.null(values()$choixqual)){
      choix <- c(choix,gettext("Results of the categorical variables"))
    }
    radioButtons("out",gettext("Which outputs do you want?"),
                 choices=choix,selected=gettext("Summary of outputs"),inline=TRUE)
  })
  
  getactive <- function(){
    if(input$selecactive==gettext("Choose")){
      sup <- NULL
      if(length(input$supvar)==0){
        activevar <- VariableChoicesPCAshiny
      }
      else{
        sup <- which(VariableChoicesPCAshiny%in%input$supvar)
        if (length(sup)==0) sup <- NULL
        activevar <- VariableChoicesPCAshiny[-sup]
      }
      return(activevar)
    }
  }
  
  output$NB1 <- renderUI({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    if(input$selecactive==gettext("All") || length(getactive())>5){
      return(textInput("nb1", label = h6(gettext("x axis")), axe1PCAshiny,width='51px'))
    } else{
      baba <- c(1:length(getactive()))
      return(selectInput("nb1",label=h6(gettext("x axis")), choices=baba,selected=axe1PCAshiny,width='51px'))
    }
  })
  
  output$NB2=renderUI({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    if(input$selecactive==gettext("All") || length(getactive())>5){
      return(textInput("nb2", label = h6(gettext("y axis")), axe2PCAshiny,width='51px'))
    } else{
      baba <- c(1:length(getactive()))
      return(selectInput("nb2",label=h6(gettext("y axis")), choices=baba,selected=axe2PCAshiny,width='51px'))
    }
  })
  
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
      fluidRow(
        tags$head(
          tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle; } 
                     #inline .form-group { display: table-row;}")
          ),
        return(tags$div(id = "inline", numericInput(inputId = "nbDimClustering", label = gettext("Number of dimensions kept for clustering:"),value=nbdimclustPCAshiny,min=1)))
      )
    }
  })
  
  output$sorties <- renderTable({
    return(as.data.frame(values()$res.PCA$eig))
  },rownames <- TRUE)
  
  output$sorties12 <- renderTable({
    validate(
      need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
    )
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    return(as.data.frame(values()$res.PCA$quali.sup$coord))
  },rownames=TRUE)
  
  output$sorties13 <- renderTable({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    validate(
      need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
    )
    return(as.data.frame(values()$res.PCA$quali.sup$v.test))
  },rownames=TRUE)
  
  output$sorties2 <- renderTable({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    return(as.data.frame(values()$res.PCA$var$coord))
  },rownames=TRUE)
  
  output$sorties22 <- renderTable({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    return(as.data.frame(values()$res.PCA$ind$coord))
  },rownames=TRUE)
  
  output$sorties23 <- renderTable({
    validate(
      need(length(input$supvar)!=0, gettext("No supplementary quantitative variables"))
    )
    validate(
      need(length(input$getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    return(as.data.frame(values()$res.PCA$quanti.sup$coord))
  },rownames=TRUE)
  
  output$sorties32 <- renderTable({
    validate(
      need(length(input$supvar)!=0, gettext("No supplementary quantitative variables"))
    )
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    return(as.data.frame(values()$res.PCA$quanti.sup$cor))
  },rownames=TRUE)
  
  output$sorties36 <- renderTable({
    validate(
      need(length(input$indsup)!=0, gettext("No supplementary individuals"))
    )
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variables"))
    )
    return(as.data.frame(values()$res.PCA$ind.sup$coord))
  },rownames=TRUE)
  
  output$sorties37 <- renderTable({
    validate(
      need(length(input$indsup)!=0, gettext("No supplementary individuals"))
    )
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    return(as.data.frame(values()$res.PCA$ind.sup$cos2))
  },rownames=TRUE)
  
  
  output$sorties3 <- renderTable({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    return(as.data.frame(values()$res.PCA$var$contrib))
  },rownames=TRUE)
  
  output$sorties33 <- renderTable({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    return(as.data.frame(values()$res.PCA$ind$contrib))
  },rownames=TRUE)
  
  output$sorties4 <- renderTable({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    return(as.data.frame(values()$res.PCA$var$cos2))
  },rownames=TRUE)
  
  output$sorties44 <- renderTable({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    return(as.data.frame(values()$res.PCA$ind$cos2))
  },rownames=TRUE)
  
  output$sortieDimdesc3 <- renderTable({
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    validate(need(length(dimdesc(values()$res.PCA))>0,gettext("No quantitative variable describes axis 1")))
    return(as.data.frame(dimdesc(values()$res.PCA)[[1]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc4 <- renderTable({
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    validate(need(length(dimdesc(values()$res.PCA))>0,gettext("No categorical variable describes axis 1")))
    return(as.data.frame(dimdesc(values()$res.PCA)[[1]]$quali))
  },rownames=TRUE)
  
  #DIM2
  
  output$sortieDimdesc33 <- renderTable({
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    validate(need(length(dimdesc(values()$res.PCA))>1,gettext("No quantitative variable describes axis 2")))
    return(as.data.frame(dimdesc(values()$res.PCA)[[2]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc44 <- renderTable({
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    validate(need(length(dimdesc(values()$res.PCA))>1,gettext("No categorical variable describes axis 2")))
    return(as.data.frame(dimdesc(values()$res.PCA)[[2]]$quali))
  },rownames=TRUE)
  
  #DIM3
  
  output$sortieDimdesc333 <- renderTable({
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    validate(need(length(dimdesc(values()$res.PCA))>2,gettext("No quantitative variable describes axis 3")))
    return(as.data.frame(dimdesc(values()$res.PCA)[[3]]$quanti))
  },rownames=TRUE)
  
  output$sortieDimdesc444 <- renderTable({
    validate(
      need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more variables"))
    )
    validate(need(length(dimdesc(values()$res.PCA))>2,"No categorical variable describes axis 3"))
    return(as.data.frame(dimdesc(values()$res.PCA)[[3]]$quali))
  },rownames=TRUE)
  
  
  output$map3 <- renderPlot({
    return(barplot(values()$res.PCA$eig[,1],names.arg=rownames(values()$res.PCA$eig),las=2))
  })
  
  output$JDD <- renderDataTable({
    cbind(Names=rownames(newdataPCAshiny),newdataPCAshiny)},
    options = list(    "orderClasses" = TRUE,
                       "responsive" = TRUE,
                       "pageLength" = 10))
  
  output$summary <- renderPrint({
    summary(newdataPCAshiny)
  })
  
  output$summaryPCA <- renderPrint({
    validate(
      need(input$nbele!=0, gettext("Please select at least one element"))
    )
    a<-values()$res.PCA
    a$call$call<-code()
    summary.PCA(a,nbelements=input$nbele)
  })
  
  output$summary2 <- downloadHandler(filename = function() { 
    paste('summaryofPCA','.txt', sep='') 
  },
  content = function(file) {
    summary.PCA(values()$res.PCA,nbelements=input$nbele,file=file)
  },
  contentType='text/csv')
  
  
  output$slider3 <- renderUI({
    validate(
      need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary variable"))
    )
    if(input$selecactive==gettext("All")){
      maxvar=length(VariableChoicesPCAshiny)
    }
    if(input$selecactive==gettext("Choose")){
      maxvar=length(getactive())
    }
    if(selection3PCAshiny=="contrib"){
      return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables"),
                                            min=1,max=maxvar,value=selection4,step=1)))  
    }
    else{
      return(div(align="center",sliderInput("slider4",label=gettext("Number of the most contributive variables"),
                                            min=1,max=maxvar,value=maxvar,step=1)))}
  })
  
  
  output$habillage2 <- renderUI({
    if(length(QualiChoicePCAshiny)==0 || input$supquali==FALSE || length(input$supquali)==0){
      return(p(gettext("No categorical variable")))
    }
    if(length(input$supquali)>1){
      if(is.null(habillageindPCAshiny)){
        numPCAshiny <- c(1:length(input$supquali))
        return(selectInput("habiller",gettext("Select 1 or 2 variables"), choices=list(numPCAshiny=input$supquali),multiple=TRUE))
      }
      else{
        numPCAshiny <- c(1:length(input$supquali))
        return(selectInput("habiller",gettext("Select 1 or 2 variables"), choices=list(numPCAshiny=input$supquali),multiple=TRUE,selected=habillageindPCAshiny))
      }
    }
  })
  
  output$ellipsesPCAshiny <- renderUI({
    #validate(need(!is.null(input$habiller),""))
    if(length(QualiChoicePCAshiny)==0 || input$supquali==FALSE || length(input$supquali)==0){
      return(p(" "))
    }else{
      return(checkboxInput("elip",gettext("Draw the confidence ellipses around the categories"),ellipsesPCAshiny))
    }
  })
  
  output$varsu <- renderUI({
    test <- values()$choixquant
    if(!is.null(test)){
      if(!is.null(colorsupvarPCAshiny)){
        return(colourpicker::colourInput("colorsupvarPCAshiny", h6(gettext("Choose colour for supplementary variables")), colorsupvarPCAshiny))
      }else{
        return(colourpicker::colourInput("colorsupvarPCAshiny", h6(gettext("Choose colour for supplementary variables")), "blue"))
      }
    }
  })
  
  output$histo <- renderPlot({
    par(mfrow=c(1,2))
    boxplot(newdataPCAshiny[,input$bam])
    hist(newdataPCAshiny[,input$bam],main="",xlab="")
  })
  
  observe({
    if(input$Investigatehtml!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsavePCAshiny)
        if (gettext(input$choixLANG)==gettext("English")) FactoInvestigate::Investigate(values()$res.PCA, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="en")
        if (gettext(input$choixLANG)==gettext("French")) FactoInvestigate::Investigate(values()$res.PCA, openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="fr")
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$Investigatedoc!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsavePCAshiny)
        if (gettext(input$choixLANG)==gettext("English")) FactoInvestigate::Investigate(values()$res.PCA,document="word_document",openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="en")
        if (gettext(input$choixLANG)==gettext("French")) FactoInvestigate::Investigate(values()$res.PCA,document="word_document",openFile=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="fr")
        setwd(path.aux)
      })
    }
  })
  

  observe({
    if(input$InvestigateRmd!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsavePCAshiny)
	    if (gettext(input$choixLANG)==gettext("English")) FactoInvestigate::Investigate(values()$res.PCA, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="en")
	    if (gettext(input$choixLANG)==gettext("French")) FactoInvestigate::Investigate(values()$res.PCA, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, display.HCPC =input$hcpcparam, language="fr")
        setwd(path.aux)
      })
    }
  })
    
  output$downloadData  <-  downloadHandler(
    filename = function() { 
      paste('graph1','.png', sep='') 
    },
    content = function(file) {
      png(file)
      Plot11()
      dev.off()
    },
    contentType='image/png')
  
  output$downloadData1  <-  downloadHandler(
    filename = function() { 
      paste('graph1','.jpg', sep='') 
    },
    content = function(file) {
      jpeg(file)
      Plot11()
      dev.off()
    },
    contentType='image/jpg')
  
  output$downloadData2  <-  downloadHandler(
    filename = function() { 
      paste('graph1','.pdf', sep='') 
    },
    content = function(file) {
      pdf(file)
      Plot11()
      dev.off()
    },
    contentType=NA)
  
  output$downloadData3  <-  downloadHandler(
    filename = function() { 
      paste('graph2','.png', sep='') 
    },
    content = function(file) {
      png(file)
      Plot22()
      dev.off()
    },
    contentType='image/png')
  
  output$downloadData4  <-  downloadHandler(
    filename = function() { 
      paste('graph1','.jpg', sep='') 
    },
    content = function(file) {
      jpeg(file)
      Plot22()
      dev.off()
    },
    contentType='image/jpg')
  
  output$downloadData5  <-  downloadHandler(
    filename = function() { 
      paste('graph1','.pdf', sep='') 
    },
    content = function(file) {
      pdf(file)
      Plot22()
      dev.off()
    },
    contentType=NA)
  
  Plot11 <- function(){
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
    if(is.null(input$colorsupvarPCAshiny)){
      colo <- "blue"
    }else{
      colo <- input$colorsupvarPCAshiny
    }
    plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="var",select=selecindiv,unselect=0,col.quanti.sup=colo,cex=input$cex2,cex.main=input$cex2,cex.axis=input$cex2,title=input$title2,col.var=input$coloractvarPCAshiny)
  }
  Plot22 <- function(){
    if(input$select=="cos2"){
      if(input$slider1!=1){
        selecindiv=paste("cos2 ",input$slider1)
      }
      else{
        selecindiv <- "cos2 0.999"
      }
    }
    if(input$select==gettext("No selection")){
      selecindiv <- NULL
    }
    if(input$select=="contrib"){
      selecindiv <- paste("contrib ",input$slider0)
    }
    if(input$select==gettext("Manual")){
      selecindiv <- c(input$indiv)
    }
    if(input$supquali==FALSE || length(QualiChoicePCAshiny)==0 || length(input$supquali)==0 || input$habi==FALSE){
      hab <- "none"
      colquali <- "magenta"
    }
    else if(length(QualiChoicePCAshiny)==1 && input$supquali==TRUE){
      if(input$habi==TRUE){
        hab <- QualiChoicePCAshiny
        colquali <- "blue"
      }
      else{
        hab <- "none"
        colquali <- "magenta"
      }
    }
    else if (length(input$supquali)==1){
      if(input$habi==TRUE){
        hab <- input$supquali
        colquali <- "blue"
      }
      else{
        hab <- "none"
        colquali <- "magenta"
      }
    }
    if(length(input$supquali)>1){
      if(length(input$habiller)==0){
        hab <- "none"
        colquali <- "magenta"
      }
      if (length(input$habiller)==1 & input$habi==TRUE){
        hab <- as.character(input$habiller)
        colquali <- "blue"
      }
      if (length(input$habiller)==2 & input$habi==TRUE){
        hab <- dim(values()$DATA)[2]
        colquali <- "blue"
      }
    }
    if(!is.null(input$colorsup)){
      colors <- input$colorsup
    }else{
      colors <- "blue"
    }
    if(!is.null(input$colorquali)){
      colorss <- input$colorquali
    }else{
      colorss <- "magenta"
    }
    if(!is.null(input$elip)&&input$elip==TRUE){
      aa <- cbind.data.frame(values()$DATA[,hab],values()$res.PCA$ind$coord)
      bb <- coord.ellipse(aa,bar=TRUE)
      plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="ind",cex=input$cex,cex.main=input$cex,cex.axis=input$cex,select=selecindiv,habillage=hab,col.quali=colorss,col.ind.sup=colors,title=input$title1,ellipse=bb,col.ind = input$coloract)
    }else{
      plot.PCA(values()$res.PCA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),choix="ind",cex=input$cex,cex.main=input$cex,cex.axis=input$cex,select=selecindiv,habillage=hab,col.quali=colorss,col.ind.sup=colors,title=input$title1,col.ind = input$coloract)
    }
  }
}
