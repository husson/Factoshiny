# server scipt for CA2

shinyServer(
  function(input, output) {
    values=reactive({
      if (input$selecactive==gettext("All")){
        data.selec=newdata[,VariableChoices]
      }
      else{
        validate(
          need(length(getactive()!=0), gettext("Please select at least one supplementary column"))
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
          colnames(data.selec)[dim(data.selec)[2]]=input$supvar
          choixquanti=length(data.selec)
        }
        else{
          choixquanti=seq((dim(data.selec)[2]-length(input$supvar)+1),dim(data.selec)[2])
        }
      }
      if(length(input$rowsupl)!=0){
        # indexes=c()
        # for (i in 1:length(nom)){
          # if(nom[i]%in%input$rowsupl){
            # indexes=c(indexes,i)
          # }
        # }
	  indexes=which(nom%in%input$rowsupl)
	  if (length(indexes)==0) indexes=NULL
      }
      else{
        indexes=NULL
      }
      indexes=c(indexes,rowna)
      choixquanti2=NULL
      if(length(withna)!=0){
        data.selec=cbind(data.selec,newdata[,withna])
        if(length(withna)==1){
          colnames(data.selec)[dim(data.selec)[2]]=withna
          if(is.null(choixquanti)){
            choixquanti2=length(data.selec)
          }
          else{
            choixquanti2=c(choixquanti,length(data.selec))
          }
        }
        else{
          if(is.null(choixquanti)){
            choixquanti2=seq((dim(data.selec)[2]-length(withna)+1),dim(data.selec)[2])
          }
          else{
            choixquanti2=c(choixquanti,seq((dim(data.selec)[2]-length(withna)+1),dim(data.selec)[2]))
          }
        }
      }
      else{
        choixquanti2=choixquanti
      }
      list(res.CA=(CA(data.selec,quali.sup=choixquali,col.sup=choixquanti2,row.sup=indexes,graph=FALSE,ncp=max(5,as.numeric(input$nb1),as.numeric(input$nb2)))),DATA=(data.selec),CHOIXQUALI=(choixquali),CHOIXQUANTI=(choixquanti2),INDEXES=(indexes))
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
    
    output$col1=renderUI({
      if(!is.null(values()$res.CA$row)){
        return(colourpicker::colourInput("colrow",gettext("Colour of row points"),col1))
      }
    })
    output$col2=renderUI({
      if(!is.null(values()$res.CA$col)){
        return(colourpicker::colourInput("colcol",gettext("Colour of column points"),col2))
      }
    })
    output$col3=renderUI({
      if(!is.null(values()$res.CA$row.sup)){
        return(colourpicker::colourInput("colrowsup",gettext("Colour of supplementary row points"),col3))
      }
    })
    output$col4=renderUI({
      if(!is.null(values()$res.CA$col.sup)){
        return(colourpicker::colourInput("colcolsup",gettext("Colour of supplementary column points"),col4))
      }
    })
    
    output$ellipses=renderUI({
      values1=c()
      if(!is.null(values()$res.CA$col)){
        values1=c(values1,gettext("Columns"))
      }
      if(!is.null(values()$res.CA$row)){
        values1=c(values1,gettext("Rows"))
      }
      if(length(values)!=0){
        if(is.null(ellipses)){
        return(checkboxGroupInput("ellip",h6(""),choices=values1,selected=NULL,inline=TRUE))
        }else{
          return(checkboxGroupInput("ellip",h6(""),choices=values1,selected=ellipses,inline=TRUE))
        }
      }
    })
    
    valeuretour=function(){
      res=list()
      res$data=newdata
      res$nomData=nomData
      # a : colonnes supplementaires
      res$a=input$supvar
      # b : lignes supplementaires
      res$b=input$rowsupl
      # c : colonnes quali
      choixquali=NULL
      if (length(QualiChoice)==1){
        if(input$supquali==TRUE){
          choixquali=QualiChoice
        }
      }
      else{
        if(length(input$supquali)!=0){
          choixquali=input$supquali
        }
      }
      res$c=choixquali
      # d et e : axes
      res$d=input$nb1
      res$e=input$nb2
      # f : invisible points 
      invisi=NULL
      if(length(input$invis)!=0){
        invisi=NULL
        if(sum(gettext("Rows")==input$invis)>0) invisi<-c(invisi,"row")
        if(sum(gettext("Columns")==input$invis)>0) invisi<-c(invisi,"col")
        if(sum(gettext("Supplementary rows")==input$invis)>0) invisi<-c(invisi,"row.sup")
        if(sum(gettext("Supplementary columns")==input$invis)>0) invisi<-c(invisi,"col.sup")
        if(sum(gettext("Supplementary qualitative variables")==input$invis)>0) invisi<-c(invisi,"quali.sup")
      }
      res$f=invisi
      res$type1=input$seleccol
      res$type2=input$selecrow
      res$selec1=NULL
      if(input$seleccol=="cos2"){
        res$selec1=input$slider3
      }
      if(input$seleccol=="contrib"){
        res$selec1=input$contrib1
      }
      res$selec2=NULL
      if(input$selecrow=="cos2"){
        res$selec2=input$slider4
      }
      if(input$seleccol=="contrib"){
        res$selec2=input$contrib2
      }
      res$taille=input$cex
      res$code1=Code()
      res$code2=CodeGraph()
      res$title1=input$title1
      res$anafact=values()$res.CA
      if(is.null(input$colrow)){
        col1="blue"
      }else{
        col1=input$colrow
      }
      if(is.null(input$colcol)){
        col2="red"
      }else{
        col2=input$colcol
      }
      if(is.null(input$colrowsup)){
        col3="darkblue"
      }else{
        col3=input$colrowsup
      }
      if(is.null(input$colcolsup)){
        col4="darkred"
      }else{
        col4=input$colcolsup
      }
      res$col1=col1
      res$col2=col2
      res$col3=col3
      res$col4=col4
      res$ellip=input$ellip
      class(res) <- "CAshiny"
      return(res)
    }
    
    
    observe({
      if(input$CAcode==0){
      }
      else {
        isolate({
          cat(Code(),sep="\n")
          cat(CodeGraph(),sep="\n")
        })
      }
    })
        
    
    Code=function(){
      
      vecquant<-values()$CHOIXQUANTI
      vecqual<-values()$CHOIXQUALI
      Datasel<-values()$DATA
      indexes<-values()$INDEXES
      
	  vec2 <- paste("'",paste(colnames(Datasel),collapse="','"),"'",sep="")
      vecfinal<-paste(nomData,"[,c(",vec2,")","]",sep="")
      
      vecquant1 <- paste("c(",paste(vecquant,collapse=","),")",sep="")
      vecquant2<-vecquant
      
      vecqual1 <- paste("c(",paste(vecqual,collapse=","),")",sep="")
      vecqual2<-vecqual
      
      indexes1 <- paste("c(",paste(indexes,collapse=","),")",sep="")
      indexes2<-indexes
      
      if(length(vecqual)==0){
        vecqual<-"NULL" 
      }
      else if(length(vecqual)==1){
        vecqual<-vecqual
      }
      else if(length(vecqual)>1){
        vecqual<-vecqual2
      }
      
      if(length(vecquant)==0){
        vecquant<-"NULL"  
      }
      else if(length(vecquant)==1){
        vecquant
      }
      else if(length(vecquant)>1){
        vecquant<-vecquant1
      }
      
      
      if(length(indexes)==0){
        indexes<-"NULL"  
      }
      else if(length(indexes)==1){
        indexes
      }
      else if(length(indexes)>1){
        indexes<-indexes1
      }
      Call1=as.name(paste("res.CA=CA(",vecfinal,",quali.sup=",vecqual,",col.sup=",vecquant,",row.sup=",indexes,",graph=FALSE,ncp=",max(5,as.numeric(input$nb1),as.numeric(input$nb2)),")",sep=""))
      return(Call1)
    }
    
    CodeGraph=function(){
      sel="NULL"
      if(input$seleccol=="cos2"){
        if(input$slider3!=1){
          sel=paste("cos2 ",input$slider3)
        }
        else{
          sel="cos2 0.999"
        }
      }
      if(input$seleccol=="contrib"){
        sel=paste("contrib ",input$contrib1)
      }
      sel2="NULL"
      if(input$selecrow=="cos2"){
        if(input$slider4!=1){
          sel2=paste("cos2 ",input$slider4)
        }
        else{
          sel2="cos2 0.999"
        }
      }
      if(input$selecrow=="contrib"){
        sel2=paste("contrib ",input$contrib2)
      }
      if(is.null(input$colrow)){
        col1="blue"
      }else{
        col1=input$colrow
      }
      if(is.null(input$colcol)){
        col2="red"
      }else{
        col2=input$colcol
      }
      if(is.null(input$colrowsup)){
        col3="darkblue"
      }else{
        col3=input$colrowsup
      }
      if(is.null(input$colcolsup)){
        col4="darkred"
      }else{
        col4=input$colcolsup
      }
      if(is.null(input$ellip)||length(input$ellip)==0){
        Call2=paste('plot.CA(res.CA,axes=c(',as.numeric(input$nb1),',',as.numeric(input$nb2),'),selectCol="',sel,'",selectRow="',sel2,'",unselect=0,cex=',input$cex,',title="',input$title1,'",col.row="',col1,'",col.col="',col2,'",col.row.sup="',col3,'",col.col.sup="',col4,'",invisible=',Plot1()$invisiText,')',sep='')
      }else{
        vect=c()
        if(gettext("Columns")%in%input$ellip){
          vect=c(vect,"col")
        }
        if(gettext("Rows")%in%input$ellip){
          vect=c(vect,"row")
        }
        myellip=paste(paste("'",vect,"'",sep=""),collapse=",")
        Call2=paste('ellipseCA(res.CA,ellipse=c(',myellip,'),axes=c(',as.numeric(input$nb1),',',as.numeric(input$nb2),'),selectCol="',sel,'",selectRow="',sel2,'",unselect=0,cex=',input$cex,',title="',input$title1,'",col.row="',col1,'",col.col="',col2,'",col.row.sup="',col3,'",col.col.sup="',col4,'",invisible=',Plot1()$invisiText,')',sep='')
      }
      
      return(Call2)
    }
    
    Plot1=reactive({
      validate(
        need(input$nb1 != input$nb2, gettext("Please select two different dimensions"))
      )
      validate(
        need(length(getactive())>2 || input$selecactive==gettext("All"),gettext("Please select more columns"))
      )
      if(length(input$invis)==0){
        invisi="none"
        invisiText=paste("'","none","'",sep="")
      }
      if(length(input$invis)!=0){
        invisi=NULL
        if(sum(gettext("Rows")==input$invis)>0) invisi<-c(invisi,"row")
        if(sum(gettext("Columns")==input$invis)>0) invisi<-c(invisi,"col")
        if(sum(gettext("Supplementary rows")==input$invis)>0) invisi<-c(invisi,"row.sup")
        if(sum(gettext("Supplementary columns")==input$invis)>0) invisi<-c(invisi,"col.sup")
        if(sum(gettext("Supplementary qualitative variables")==input$invis)>0) invisi<-c(invisi,"quali.sup")
        invisiText=invisi
        invisiText=paste("c(",paste(paste("'",invisi,"'",sep=""),collapse = ","),")",sep="")
      }
      sel=NULL
      if(input$seleccol=="cos2"){
        if(input$slider3!=1){
          sel=paste("cos2 ",input$slider3)
        }
        else{
          sel="cos2 0.999"
        }
      }
      if(input$seleccol=="contrib"){
        sel=paste("contrib ",input$contrib1)
      }
      sel2=NULL
      if(input$selecrow=="cos2"){
        if(input$slider4!=1){
          sel2=paste("cos2 ",input$slider4)
        }
        else{
          sel2="cos2 0.999"
        }
      }
      if(input$selecrow=="contrib"){
        sel2=paste("contrib ",input$contrib2)
      }
      values2=c()
      if(!is.null(input$ellip)){
      if(gettext("Columns")%in%input$ellip){
        values2=c(values2,"col")
      }
        if(gettext("Rows")%in%input$ellip){
          values2=c(values2,"row")
        }
      }
      if(is.null(input$colrowsup)){
        colrowsup="darkblue"
      }else{
        colrowsup=input$colrowsup
      }
      if(is.null(input$colcolsup)){
        colcolsup="darkred"
      }else{
        colcolsup=input$colcolsup
      }
      list(PLOT1=(plot.CA(values()$res.CA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),selectCol=sel,title=input$title1,selectRow=sel2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,unselect=0,invisible=invisi,col.row=input$colrow,col.col=input$colcol,col.row.sup=colrowsup,col.col.sup=colcolsup)),invisiText=(invisiText),
           PLOT2=(ellipseCA(values()$res.CA,ellipse=values2,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),selectCol=sel,title=input$title1,selectRow=sel2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,unselect=0,invisible=invisi,col.row=input$colrow,col.col=input$colcol,col.row.sup=colrowsup,col.col.sup=colcolsup)))
    })
    
    output$map <- renderPlot({
      if(is.null(input$ellip)||length(input$ellip)==0){
      p <- Plot1()$PLOT1
      }else{
      p=Plot1()$PLOT2
      }
    })
    
    
    getactive=function(){
      if(input$selecactive==gettext("Choose")){
        sup=NULL
        if(length(input$supvar)==0){
          activevar=VariableChoices
        }
        else{
	      sup=which(VariableChoices%in%input$supvar)
          activevar=VariableChoices[-sup]
        }
        return(activevar)
      }
    }
    
    output$contribcol=renderUI({
      maxx=dim(values()$res.CA$col$coord)[1]
      if(selec1=="contrib"){
        return(sliderInput("contrib1",h6(gettext("Number of the most contributive active columns")),min=1,max=maxx,value=valueselec2,step=1))
      }
      else{
        return(sliderInput("contrib1",h6(gettext("Number of the most contributive active columns")),min=1,max=maxx,value=maxx,step=1))
      }
      
    })
    
    output$contribrow=renderUI({
      maxx=dim(values()$res.CA$row$coord)[1]
      if(selec2=="contrib"){
        return(sliderInput("contrib2",h6(gettext("Number of the most contributive active rows")),min=1,max=maxx,value=valueselec2,step=1))
      }
      else{
        return(sliderInput("contrib2",h6(gettext("Number of the most contributive active rows")),min=1,max=maxx,value=maxx,step=1))
      }
    })
    
    output$out22=renderUI({
      choix=list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results for the columns"),gettext("Results for the rows"))
      if(!is.null(values()$INDEXES)){
        choix=c(choix,gettext("Results for the supplementary rows"))
      }
      if(!is.null(values()$CHOIXQUANTI)){
        choix=c(choix,gettext("Results for the supplementary columns"))
      }
      if(!is.null(values()$CHOIXQUALI)){
        choix=c(choix,gettext("Results for the categorical variables"))
      }
      radioButtons("out",gettext("Which outputs do you want?"),
                   choices=choix,selected=gettext("Summary of outputs"),inline=TRUE)
    })
    
    output$warn=renderPrint({
      if(length(withna)!=0){
        baba=paste(withna,collapse=", ")
        bibi=paste(nomrow,collapse=", ")
        a=paste0(gettext("Warning: "), baba, gettext(" have NA : they are considered as supplementary columns"))
        b=paste0(gettext("Warning: "), bibi, gettext(" have NA : they are considered as supplementary rows"))
        return(cat(a,b,sep="\n"))
      }
    })
    
    output$NB1=renderUI({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary column"))
      )
      if(input$selecactive==gettext("All") || length(getactive())>5){
        # return(selectInput("nb1", label = h6("x axis"), 
                           # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = axe1,width='80%'))
        return(textInput("nb1", label = h6(gettext("x axis")), axe1,width='50%'))
      }
      else{
        baba=c(1:length(getactive()))
        return(selectInput("nb1",label=h6("x axis"), choices=baba,selected=axe1,width='80%'))
      }
    })
    
    output$NB2=renderUI({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Please select at least one supplementary column"))
      )
      if(input$selecactive==gettext("All") || length(getactive())>5){
        # return(selectInput("nb2", label = h6("y axis"), 
                           # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = axe2,width='80%'))
        return(textInput("nb2", label = h6(gettext("y axis")), axe2,width='50%'))
      }
      else{
        baba=c(1:length(getactive()))
        return(selectInput("nb2",label=h6("y axis"), choices=baba,selected=axe2,width='80%'))
      }
    })
    
    
    output$sorties=renderTable({
      return(as.data.frame(values()$res.CA$eig))
    },rownames=TRUE)
    
    output$sorties1=renderTable({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Not enough active columns"))
      )
      return(as.data.frame(values()$res.CA$col$coord))
    },rownames=TRUE)
    
    output$sorties2=renderTable({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Not enough active columns"))
      )
      return(as.data.frame(values()$res.CA$col$cos2))
    },rownames=TRUE)
    
    output$sorties3=renderTable({
      validate(
        need(length(getactive())>1 || input$selecactive==gettext("All"),gettext("Not enough active columns"))
      )
      return(as.data.frame(values()$res.CA$col$contrib))
    },rownames=TRUE)
    
    output$sorties4=renderTable({
      return(as.data.frame(values()$res.CA$row$coord))
    },rownames=TRUE)
    
    output$sorties5=renderTable({
      return(as.data.frame(values()$res.CA$row$cos2))
    },rownames=TRUE)
    
    output$sorties6=renderTable({
      return(as.data.frame(values()$res.CA$row$contrib))
    },rownames=TRUE)
    
    output$sorties7=renderTable({
      validate(
        need((length(input$rowsupl)>0), gettext("No supplementary rows selected"))
      )
      return(as.data.frame(values()$res.CA$row.sup$coord))
    },rownames=TRUE)
    
    output$sorties8=renderTable({
      return(as.data.frame(values()$res.CA$row.sup$cos2))
    },rownames=TRUE)
    
    output$sorties9=renderTable({
      return(as.data.frame(values()$res.CA$col.sup$coord))
    },rownames=TRUE)
    
    output$sorties10=renderTable({
      return(as.data.frame(values()$res.CA$col.sup$cos2))
    },rownames=TRUE)
    
    output$sorties11=renderTable({
      validate(
        need((length(input$supquali)>0 || input$supquali==TRUE), gettext("No categorical variables selected"))
      )
      return(as.data.frame(values()$res.CA$quali.sup))
    },rownames=TRUE)
    
    
    
    output$map3=renderPlot({
      return(barplot(values()$res.CA$eig[,1],names.arg=rownames(values()$res.CA$eig),las=2))
    })
    
    ### Fonction permettant l'affichage du JDD sous la forme d'un DataTable, qui permet la recherche de donnes. 
    output$JDD=renderDataTable({
      cbind(Names=rownames(newdata),newdata)},
      options = list(    "orderClasses" = TRUE,
                         "responsive" = TRUE,
                         "pageLength" = 10))
    
    ### Fonction permettant l'affichage du summary du JDD
    output$summary=renderPrint({
      summary(newdata)
    })
    
    
    ### Fonction permettant l'affichage du summary de la fonction CA sur le JDD
    output$summaryCA=renderPrint({
      a<-values()$res.CA
      a$call$call<-Code()
      summary.CA(a,nbelements=input$nbele)
    })
    
    output$summary2=downloadHandler(filename = function() { 
      paste('summaryofCA','.txt', sep='') 
    },
    content = function(file) {
      summary.CA(values()$res.CA,nbelements=input$nbele,file=file)
    },
    contentType='text/csv')
    
    
    ## Creation des fonctions permettant l'enregistrement des graphs sous les formats : png, jpeg, pdf et emf
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
    
    Plot11=function(){
      if(length(input$invis)==0){
        invisi="none"
      }
      if(length(input$invis)!=0){
        invisi=NULL
        if(sum(gettext("Rows")==input$invis)>0) invisi<-c(invisi,"row")
        if(sum(gettext("Columns")==input$invis)>0) invisi<-c(invisi,"col")
        if(sum(gettext("Supplementary rows")==input$invis)>0) invisi<-c(invisi,"row.sup")
        if(sum(gettext("Supplementary columns")==input$invis)>0) invisi<-c(invisi,"col.sup")
        if(sum(gettext("Supplementary qualitative variables")==input$invis)>0) invisi<-c(invisi,"quali.sup")
      }
      sel=NULL
      if(input$seleccol=="cos2"){
        if(input$slider3!=1){
          sel=paste("cos2 ",input$slider3)
        }
        else{
          sel="cos2 0.999"
        }
      }
      if(input$seleccol=="contrib"){
        sel=paste("contrib ",input$contrib1)
      }
      sel2=NULL
      if(input$selecrow=="cos2"){
        if(input$slider4!=1){
          sel2=paste("cos2 ",input$slider4)
        }
        else{
          sel2="cos2 0.999"
        }
      }
      if(input$seleccol=="contrib"){
        sel2=paste("contrib ",input$contrib2)
      }
      if(is.null(input$colrowsup)){
        colrowsup="darkblue"
      }else{
        colrowsup=input$colrowsup
      }
      if(is.null(input$colcolsup)){
        colcolsup="darkred"
      }else{
        colcolsup=input$colcolsup
      }
      plot.CA(values()$res.CA,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),selectCol=sel,selectRow=sel2,cex=input$cex,cex.main=input$cex,cex.axis=input$cex,title=input$title1,unselect=0,invisible=invisi,col.row=input$colrow,col.col=input$colcol,col.row.sup=colrowsup,col.col.sup=colcolsup)
    }
    
  }
)
