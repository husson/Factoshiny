# server2. AFM2

shinyServer(
  function(input, output) {
    
    ### fonctions graphiques
    Plot1 <- function(){
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )      
      if(input$choixpartial==gettext("None")){
        part=NULL
      }
      if(input$choixpartial==gettext("All")){
        part="all"
      }
      if(input$choixpartial==gettext("Choose")){
        part=input$indivpartiel
      }
      lapbar=TRUE
      if(input$choixpartial!=gettext("None") && input$partind==FALSE){
        lapbar=FALSE
      }
      habi="none"
      if(!(is.null(input$drawind))){
      if(input$choixpartial==gettext("None") && input$drawind==gettext("No selection")){
        habi="none"
      }
      else if(input$choixpartial==gettext("None") && input$drawind==gettext("individual")){
        habi="ind"
      }
      else if((input$choixpartial==gettext("All") || input$choixpartial==gettext("Choose")) && input$drawind==gettext("individual")){
        habi="ind"
      }
      else if((input$choixpartial==gettext("All") || input$choixpartial==gettext("Choose")) && input$drawind==gettext("group")){
        habi="group"
      }
      else if(input$drawind==gettext("categorical variable")){
        habi=input$habiquali
      }
      }
      invi="none"
      if(input$meanind1==FALSE){
        invi="ind" 
      }
      else if (input$qualind1==FALSE){
        invi="quali"
      }
      if (input$qualind1==FALSE && input$meanind1==FALSE){
        invi=c("ind","quali")
      }
      if(!(is.null(habi))){
      plot.MFA(anafact,choix="ind",axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),title=input$title2,partial=part,lab.ind=input$meanind, lab.par=lapbar,lab.var=input$qualind, habillage=habi,invisible=invi)
      }
    }
    
    output$map <- renderPlot({
      p <- Plot1()
    })
    
    Plot2=function(){
      if(input$colorgroup==TRUE){
        habi="group"
      }
      if(input$colorgroup==FALSE){
        habi="none"
      }
      if(input$selection==gettext("No selection")){
        selec=NULL
      }
      if(input$selection=="contrib"){
        selec=paste("contrib ",input$slider2)
      }
      if(input$selection=="cos2"){
        if(input$slider3!=1){
          selec=paste("cos2 ",input$slider3)
        }
        else{
          selec="cos2 0.999"
        }
      }
#      invi="none"
      #if (hide=="non") invi="none"
      if(is.null(input$hides)){
        invi="none"}else{
	  if (input$hides==gettext("Nothing")) invi="none"
	  if (input$hides==gettext("Active variables")) invi="quanti"
	  if (input$hides==gettext("Supplementary variables")) invi="quanti.sup"
        }
      plot.MFA(anafact,choix="var",axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),habillage=habi,title=input$title3,select=selec,invisible=invi)
    }
    
    output$map2 <- renderPlot({
      p=Plot2()
    })
    
    output$map22=renderUI({
      validate(
        need(!(is.null(anafact$quanti.var)),gettext("No quantitative group"))
      )
      plotOutput("map2", width = 500, height=500)
    })
    
    Plot5 <- function(){
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      plot.MFA(anafact,choix="group",title=input$title1,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)))
    }
    
    output$map5 <- renderPlot({
      p <- Plot5()
    })
    
    Plot4 <- function(){
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      if(input$coloraxe==TRUE){
        habi="group"
      }
      else{
        habi="none"
      }
      plot.MFA(anafact,choix="axes",axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),habillage=habi,title=input$title4)
    }
    
    output$map4 <- renderPlot({
      p <- Plot4()
    })

    output$map6 <- renderPlot({
      if(input$affichcol==TRUE){
        col=TRUE
      }
      if(input$affichcol==FALSE){
        col=FALSE
      }
      plot.MFA(anafact,choix="freq",axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),lab.col=col,title=input$title5)
    })
    
    output$map66=renderUI({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      if(is.null(anafact$freq)){
        return(p(gettext("No groups of frequencies")))
      }
      else{
        return(plotOutput("map6", width = 500, height=500))
      }
    })

    
    output$drawindiv=renderUI({
      if(input$choixpartial==gettext("None")){
        return(radioButtons("drawind",h6(gettext("Drawing by")),choices=list(gettext("No selection"),gettext("individual"),gettext("categorical variable")),selected=drawing,inline=TRUE))
      }
      else{
        return(radioButtons("drawind",h6(gettext("Drawing by")),choices=list(gettext("individual"),gettext("group"),gettext("categorical variable")),selected=drawing,inline=TRUE))
      }
    })
    
    output$habillagequali=renderUI({
        if(!(is.null(anafact$quali.var))){
          choix=quali
          if(length(choix)==1){
          return(selectInput("habiquali"," ",choices=choix))
          }
          else{
          num=c(1:length(choix))
          return(selectInput("habiquali"," ",choices=list(num=choix)))
          }
        }
      else{
        p(gettext("No groups of categorical variable"))
      }
    })
    
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
      res$ligne=ligne
      res$anafact=anafact
      res$data=newdataMFAshiny
      res$axe1=input$nb1
      res$axe2=input$nb2
      res$ind1=input$meanind1
      res$ind2=input$meanind
      res$ind3=input$qualind1
      res$ind4=input$qualind
      res$drawing=input$drawind
      res$drawing2=input$habiquali
      res$partial=input$choixpartial
      res$partial2=input$indivpartiel
      res$partial3=input$partind
      res$selectvar=input$selection
      sel=NULL
      if(input$selection=="contrib"){
        sel=input$slider2
      }
      if(input$selection=="cos2"){
       sel=input$slider3
      }
      res$selectvar2=sel
      res$hide=input$hides
      res$colorvar=input$colorgroup
      res$freq1=input$affichind
      res$freq2=input$affichcol
      res$partaxe=input$coloraxe
      res$nom=nomData
      res$code1=codeGraph1()
      res$code2=codeGraph2()
      res$code3=codeGraph3()
      res$code4=codeGraph4()
      res$code5=codeGraph5()
      res$title1=input$title1
      res$title2=input$title2
      res$title3=input$title3
      res$title4=input$title4
      res$title5=input$title5
      res$hcpcparam <- input$hcpcparam
      res$nbdimclustPCAshiny <- input$nbDimClustering
      class(res)="MFAshiny"
      return(res)
    }
    
    output$indivpartiel2=renderUI({
      if(is.null(partial2)){
        return(selectInput("indivpartiel",label=h6(gettext("Select individuals")),
                           choices=list(num=nom),multiple=TRUE))
      }
      else{
        return(selectInput("indivpartiel",label=h6(gettext("Select individuals")),
                           choices=list(num=nom),multiple=TRUE,selected=partial2))
      }
    })
    
    output$slider1=renderUI({
      if(inherits(x,"MFA")){
      maxlength=dim(anafact$quanti.var$coord)[1]
      if(input$selection=="contrib"){
        return(sliderInput("slider2",h6(gettext("Number of the most contributive variables")),min=1, max=maxlength, value=maxlength, step=1))
      }
      if(input$selection=="cos2"){
        return(sliderInput("slider3",h6(gettext("Number of variables with highest cos2")),min=0, max=1, value=1, step=0.01))
      }
      }
      if(inherits(x,"MFAshiny")){
        maxlength=dim(anafact$quanti.var$coord)[1]
        if(input$selection=="contrib"){
          if(selectvar=="contrib"){
          return(sliderInput("slider2",h6(gettext("Number of the most contributive variables")),min=1, max=maxlength, value=selectvar2, step=1))
          }
          else{
            return(sliderInput("slider2",h6(gettext("Number of the most contributive variables")),min=1, max=maxlength, value=maxlength, step=1))  
          }
          }
        if(input$selection=="cos2"){
          if(selectvar=="cos2"){
          return(sliderInput("slider3",h6(gettext("Number of variables with highest cos2")),min=0, max=1, value=selectvar2, step=0.01))
          }
          else{
            return(sliderInput("slider3",h6(gettext("Number of variables with highest cos2")),min=0, max=1, value=1, step=0.01))  
          }
          }  
      }
    })
    
    output$hide2=renderUI({
      if(!(is.null(anafact$quanti.var.sup))){
        if(!is.null(hide)){
        return(radioButtons("hides",h6(gettext("Hide:")),choices=list(gettext("Nothing"),gettext("Active variables"),gettext("Supplementary variables")),selected=hide))
        }else{
          return(radioButtons("hides",h6(gettext("Hide:")),choices=list(gettext("Nothing"),gettext("Active variables"),gettext("Supplementary variables")),selected=gettext("Nothing")))
        }
      }
    })
    
  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
      fluidRow(
        tags$head(
          tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle; } 
                     #inline .form-group { display: table-row;}")
          ),
        return(tags$div(id = "inline", numericInput(inputId = "nbDimClustering", label = gettext("Number of dimensions kept for clustering:"),value=nbdimclustMFAshiny,min=1)))
      )
    }
  })

  output$sorties=renderTable({
      return(as.data.frame(anafact$eig))
    },rownames=TRUE)
    
    output$map3=renderPlot({
      return(barplot(anafact$eig[,1],names.arg=rownames(anafact$eig),las=2))
    })
    output$JDD=renderDataTable({
      tab=cbind(Names=rownames(anafact$global.pca$call$X),anafact$global.pca$call$X)
      quanti=names(which(sapply(tab,is.numeric)))
      tab[quanti]=round(tab[quanti],5)
      tab
      },
      options = list(    "orderClasses" = TRUE,
                         "responsive" = TRUE,
                         "pageLength" = 10))
    
    output$summary=renderPrint({
      summary(anafact$global.pca$call$X)
    })
    
    output$summaryMFA=renderPrint({
      summary.MFA(anafact)
    })  
    
      
    output$histo=renderPlot({
      par(mfrow=c(1,2))
      boxplot(x[,input$bam])
      plot(density(x[,input$bam]),main="",xlab="")
    })
    
    
    output$downloadData = downloadHandler(
      filename = function() { 
        paste('graph1','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot1()
        dev.off()
      },
      contentType='image/png')
    
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
    
    output$download3=renderUI({
      if(is.null(anafact$quanti.var)){
        return()
      }
      else{
        return(downloadButton("downloadData3",gettext("Download as png")))
      }
    })
    
    output$downloadData11 = downloadHandler(
      filename = function() { 
        paste('graph3','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot5()
        dev.off()
      },
      contentType='image/png')
    
    output$downloadData12 = downloadHandler(
      filename = function() { 
        paste('graph3','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot5()
        dev.off()
      },
      contentType='image/jpg')
    
    output$downloadData13 = downloadHandler(
      filename = function() { 
        paste('graph3','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot5()
        dev.off()
      },
      contentType=NA)
    
    
    output$downloadData15 = downloadHandler(
      filename = function() { 
        paste('graph4','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot4()
        dev.off()
      },
      contentType='image/png')
    
    output$downloadData16 = downloadHandler(
      filename = function() { 
        paste('graph4','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot4()
        dev.off()
      },
      contentType='image/jpg')
    
    output$downloadData17 = downloadHandler(
      filename = function() { 
        paste('graph4','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot4()
        dev.off()
      },
      contentType=NA)
    
    
    output$downloadData19 = downloadHandler(
      filename = function() { 
        paste('graph5','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot6()
        dev.off()
      },
      contentType='image/png')
    
    output$download19=renderUI({
      if(is.null(anafact$freq)){
        return()
      }
      else{
        return(downloadButton("downloadData19",gettext("Download as png")))
      }
    })
    
    output$downloadData20 = downloadHandler(
      filename = function() { 
        paste('graph5','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot6()
        dev.off()
      },
      contentType='image/jpg')
    
    output$download20=renderUI({
      if(is.null(anafact$freq)){
        return()
      }
      else{
        return(downloadButton("downloadData20",gettext("Download as jpg")))
      }
    })
    
    output$downloadData21 = downloadHandler(
      filename = function() { 
        paste('graph5','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot6()
        dev.off()
      },
      contentType=NA)
    
    output$download21=renderUI({
      if(is.null(anafact$freq)){
        return()
      }
      else{
        return(downloadButton("downloadData21",gettext("Download as pdf")))
      }
    })
    
    output$downloadData22 = downloadHandler(
      filename = function() { 
        paste('graph5','.emf', sep='') 
      },
      content = function(file) {
        emf(file)
        Plot6()
        dev.off()
      },
      contentType=NA)
    
    output$downloadData1 = downloadHandler(
      filename = function() { 
        paste('graph1','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot1()
        dev.off()
      },
      contentType='image/jpg')
    
    output$downloadData2 = downloadHandler(
      filename = function() { 
        paste('graph1','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot1()
        dev.off()
      },
      contentType=NA)
    
    output$downloadData4 = downloadHandler(
      filename = function() { 
        paste('graph2','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot2()
        dev.off()
      },
      contentType='image/jpg')
    
    output$download4=renderUI({
      if(is.null(anafact$quanti.var)){
        return()
      }
      else{
        return(downloadButton("downloadData4",gettext("Download as jpg")))
      }
    })
    
    output$downloadData5 = downloadHandler(
      filename = function() { 
        paste('graph2','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot2()
        dev.off()
      },
      contentType=NA)
    
    output$download5=renderUI({
      if(is.null(anafact$quanti.var)){
        return()
      }
      else{
        return(downloadButton("downloadData5",gettext("Download as pdf")))
      }
    })
    
    output$downloadData6 = downloadHandler(
      filename = function() { 
        paste('graph2','.emf', sep='') 
      },
      content = function(file) {
        emf(file)
        Plot2()
        dev.off()
      },
      contentType=NA)
    
    output$downloadData7 = downloadHandler(
      filename = function() { 
        paste('graph1','.emf', sep='') 
      },
      content = function(file) {
        emf(file)
        Plot1()
        dev.off()
      },
      contentType=NA)
    
    ### Sorties
    
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
    
    
    
    ###Recup codes
    observe({
      if(input$MFAcode==0){
      }
      else {
        isolate({
          print(ligne)
          cat(codeGraph1(),sep="\n")
          cat(codeGraph2(),sep="\n")
          cat(codeGraph3(),sep="\n")
          cat(codeGraph4(),sep="\n")
          if(!is.null(anafact$freq)){
            cat(codeGraph5(),sep="\n")
          }
        })
      }
    })
    
    codeGraph1<-function(){
      if(input$choixpartial==gettext("None")){
        part="NULL"
      }
      if(input$choixpartial==gettext("All")){
        part="all"
      }
      if(input$choixpartial==gettext("Choose")){
        part1=input$indivpartiel
        if(length(input$indivpartiel)==gettext("None")){
          part=paste("'",part1,"'")
        }
        if(length(input$indivpartiel)>1){
          vec4=NULL
          vec4<-paste(vec4,"'",input$indivpartiel[1],"'",sep="")
          for (i in 2:(length(input$indivpartiel))){
            vec4<-paste(vec4,paste("'",input$indivpartiel[i],"'",sep=""),sep=",")
          }
          part=paste("c(",vec4,")",sep="")
        }
      }
      lapbar=TRUE
      if(input$choixpartial!=gettext("None") && input$partind==FALSE){
        lapbar=FALSE
      }
      habi="none"
      if(!(is.null(input$drawind))){
        if(input$choixpartial==gettext("None") && input$drawind==gettext("No selection")){
          habi="group"
        }
        else if(input$choixpartial==gettext("None") && input$drawind==gettext("individual")){
          habi="ind"
        }
        else if((input$choixpartial==gettext("All") || input$choixpartial==gettext("Choose")) && input$drawind==gettext("individual")){
          habi="ind"
        }
        else if((input$choixpartial==gettext("All") || input$choixpartial==gettext("Choose")) && input$drawind==gettext("group")){
          habi="group"
        }
        else if(input$drawind==gettext("categorical variable")){
          habi=input$habiquali
        }
      }
      invi="none"
      if(input$meanind1==FALSE){
        invi="ind" 
      }
      else if (input$qualind1==FALSE){
        invi="quali"
      }
      if (input$qualind1==FALSE && input$meanind1==FALSE){
        invi=c("ind","quali")
      }
        Call1=as.name(paste("plot.MFA(res,choix='ind',axes=c(",input$nb1,",",input$nb2,"),partial=",part,",title='",input$title2,"',lab.ind=",input$meanind,",lab.par=",lapbar,",lab.var=",input$qualind, ",habillage='",habi,"',invisible='",invi,"')",sep=""))
      return(Call1)
    }
    
    codeGraph2<-function(){
      if(input$colorgroup==TRUE){
        habi="group"
      }
      if(input$colorgroup==FALSE){
        habi="none"
      }
      if(input$selection==gettext("No selection")){
        selec="NULL"
      }
      if(input$selection=="contrib"){
        selec=paste("contrib ",input$slider2)
      }
      if(input$selection=="cos2"){
        if(input$slider3!=1){
          selec=paste("cos2 ",input$slider3)
        }
        else{
          selec="cos2 0.999"
        }
      }
#      invi="none"
      #if (hide==gettext("Nothing")) invi="none"
      if(is.null(input$hides)){
        invi="none"
      }else{
	  if (input$hides==gettext("Nothing")) invi="none"
	  if (input$hides==gettext("Active variables")) invi="quanti"
	  if (input$hides==gettext("Supplementary variables")) invi="quanti.sup"
      }
      Call2=paste("plot.MFA(res,choix='var',axes=c(",input$nb1,",",input$nb2,"),habillage='",habi,"',select=",selec,",title='",input$title3,"',invisible='",invi,"')",sep="")
      return(Call2)
    }
    
    codeGraph3<-function(){
      Call3=paste("plot.MFA(res,choix='group',title='",input$title1,"',axes=c(",input$nb1,",",input$nb2,"))",sep="")
      return(Call3)
    }
    
    codeGraph4<-function(){
      if(input$coloraxe==TRUE){
        habi="group"
      }
      else{
        habi="none"
      }
      Call4=paste("plot.MFA(res,choix='axes',title='",input$title4,"',axes=c(",input$nb1,",",input$nb2,"),habillage='",habi,"')",sep="")
      return(Call4)
    }

    codeGraph5<-function(){
      if(input$affichcol==TRUE){
        col=TRUE
      }
      if(input$affichcol==FALSE){
        col=FALSE
      }
      Call5=paste("plot.MFA(res,choix='freq',title='",input$title5,"',axes=c(",input$nb1,",",input$nb2,"),lab.col=",col,")",sep="")
      return(Call5)
}

  }
)
