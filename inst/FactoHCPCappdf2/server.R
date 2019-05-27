# server script for HCPC for dataframe
  function(input, output) {
    
    values=reactive({
        data.selec=x[,VariableChoices]
        choixquali=NULL
        if(length(qualiHCPCshiny)!=0){
          data.selec=cbind(data.selec,x[,QualiChoice])
          choixquali=seq((length(data.selec)-length(qualiHCPCshiny)),length(data.selec))
        }
        
      list(res.PCA=(PCA(data.selec,quali.sup=choixquali,scale.unit=FALSE,graph=FALSE,ncp=Inf)),DATA=(data.selec))
      })
    
    res.HCPCdef=reactive({
      (HCPC(values()$res.PCA,nb.clust=-1,graph=FALSE)$call$t$nb.clust)
    })
    
    # res.HCPC=reactive({
    # (HCPC(values()$res.PCA,nb.clust=input$clust,consol=input$consoli,graph=FALSE,metric=input$metric))
    # })
	res.HCPC=reactive({
    (if (input$metric=="Manhattan") HCPC(values()$res.PCA,nb.clust=input$clust,consol=input$consoli,graph=FALSE,metric="manhattan")
    else HCPC(values()$res.PCA,nb.clust=input$clust,consol=input$consoli,graph=FALSE,metric="euclidean"))
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
      res$nomDataHCPCshiny=nomDataHCPCshiny
      res$data=x
      res$classx<-"data.frame"
      class(res) <- c("HCPCshiny")
      res$clust=input$clust
      res$consoli=input$consoli
      res$metric=input$metric
      res$drawtree=input$drawtree 
      res$nom3D=input$nom3D
      res$center=input$center
      res$num=input$num
      res$nb1=as.numeric(input$nb1)
      res$nb2=as.numeric(input$nb2)
      res$code1=Code()
      res$code2=Plot1Code()
      res$code3=Plot2Code()
      res$code4=Plot3Code()
      res$title1HCPCshiny=input$title1HCPCshiny
      res$title2HCPCshiny=input$title2HCPCshiny
      res$title3HCPCshiny=input$title3HCPCshiny
      return(res)
    }
    
    observe({
      if(input$HCPCcode==0){
      }
      else {
        isolate({
          cat(Code(),sep="\n")
          cat(Plot1Code(),sep="\n")
          cat(Plot2Code(),sep="\n")
          cat(Plot3Code(),sep="\n")
        })
      }
    })
    
    Code <- function(){
#      Call1=as.name(paste("res.HCPC<-HCPC(",nomDataHCPCshiny,",nb.clust=",input$clust,",consol=",input$consoli,",graph=FALSE,metric='",input$metric,"')",sep="")) 
	  if (input$metric==gettext("Euclidean")) Call1<-as.name(paste("res.HCPC<-HCPC(",nomDataHCPCshiny,",nb.clust=",input$clust,",consol=",input$consoli,",graph=FALSE,metric='euclidean')",sep="")) 
      if (input$metric=="Manhattan") Call1<-as.name(paste("res.HCPC<-HCPC(",nomDataHCPCshiny,",nb.clust=",input$clust,",consol=",input$consoli,",graph=FALSE,metric='manhattan')",sep=""))
      return(Call1)
    }
    
    Plot1Code <- function(){
      Call2=paste("plot.HCPC(res.HCPC,choice='map',draw.tree=",input$drawtree,",title='",input$title2HCPCshiny,"',axes=c(",as.numeric(input$nb1),",",as.numeric(input$nb2),"))",sep="") 
      return(Call2)
    }
    
    Plot2Code <- function(){
      Call3=paste("plot.HCPC(res.HCPC,choice='3D.map',ind.names=",input$nom3D,",centers.plot=",input$center,",title='",input$title,"',angle=",input$num,",axes=c(",as.numeric(input$nb1),",",as.numeric(input$nb2),"))",sep="") 
      return(Call3)
    }
    
    Plot3Code <- function(){
      Call4=paste("plot.HCPC(res.HCPC,choice='tree',title='",input$title3HCPCshiny,"')",sep="")
      return(Call4)
    }
    
    getactive=function(){
      if(input$quantisup==TRUE){
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
        activevar=VariableChoices[-sup]
        }
        return(activevar)
      }
    }
    
    Plot1 <- function(){
      if(is.null(input$clust)){
        return()
      }
      else{
      return(plot.HCPC(res.HCPC(),choice="map",draw.tree=input$drawtree,title=input$title2HCPCshiny,axes=c(as.numeric(input$nb1),as.numeric(input$nb2))))
      }
    }
    output$map <- renderPlot({
      p <- Plot1()
    })
    
    
    Plot2 <- function(){
      if(is.null(input$clust)){
        return()
      }
      else{
      return(plot.HCPC(res.HCPC(),choice="3D.map",ind.names=input$nom3D,title=input$title1HCPCshiny,centers.plot=input$center,axes=c(as.numeric(input$nb1),as.numeric(input$nb2)),angle=input$num))
      }
    }
    
    output$map2 <- renderPlot({
      p <- Plot2()
    })
    
    Plot4=function(){
      if(is.null(input$clust)){
        return()
      }
      else{
        return(plot.HCPC(res.HCPC(),choice="tree",title=input$title3HCPCshiny))
      }
    }
    
    output$map4=renderPlot({
      p=Plot4()
    })
    
    output$sorties=renderTable({
      if(input$out=="axe"){
        return(as.data.frame(res.HCPC()$desc.axes))
      }
      if(input$out=="para"){
        return(as.data.frame(res.HCPC()$ind.desc))
      }
    },rownames=TRUE)
    

    output$clusters=renderUI({
      choix=res.HCPCdef()
      if(is.data.frame(x)==TRUE){
        if(nbindivHCPCshiny<=11){
          sliderInput("clust","Number of clusters",min=2,max=(nbindivHCPCshiny-1),value=choix,step=1)
        }
        else{
          sliderInput("clust","Number of clusters",min=2,max=10,value=choix,step=1)
        }
      }
      else{
      if(nbindivHCPCshiny<=11){
        sliderInput("clust","Number of clusters",min=2,max=(nbindivHCPCshiny-1),value=clustdfHCPCshiny,step=1)
      }
      else{
        sliderInput("clust","Number of clusters",min=2,max=10,value=clustdfHCPCshiny,step=1)
      }
      }
    })
    
    output$JDD=renderDataTable({
      cbind(Names=rownames(x),x)},
      options = list(    "orderClasses" = TRUE,
                         "responsive" = TRUE,
                         "pageLength" = 10))

    
  observe({
    if(input$Investigatehtml!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveHCPCshiny)
        if (gettext(input$choixLANG)==gettext("English")) FactoInvestigate::Investigate(values()$res.HCPC, openFile=TRUE, file = input$titleFile, language="en")
        if (gettext(input$choixLANG)==gettext("French")) FactoInvestigate::Investigate(values()$res.HCPC, openFile=TRUE, file = input$titleFile, language="fr")
        setwd(path.aux)
      })
    }
  })
  
  observe({
    if(input$Investigatedoc!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveHCPCshiny)
        if (gettext(input$choixLANG)==gettext("English")) FactoInvestigate::Investigate(values()$res.HCPC,document="word_document",openFile=TRUE, file = input$titleFile, language="en")
        if (gettext(input$choixLANG)==gettext("French")) FactoInvestigate::Investigate(values()$res.HCPC,document="word_document",openFile=TRUE, file = input$titleFile, language="fr")
        setwd(path.aux)
      })
    }
  })
  

  observe({
    if(input$InvestigateRmd!=0){
      isolate({
        path.aux <- getwd()
        setwd(pathsaveHCPCshiny)
	    if (gettext(input$choixLANG)==gettext("English")) FactoInvestigate::Investigate(values()$res.HCPC, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, language="en")
	    if (gettext(input$choixLANG)==gettext("French")) FactoInvestigate::Investigate(values()$res.HCPC, openFile=FALSE,remove.temp =FALSE, keepRmd=TRUE, file = input$titleFile, language="fr")
        setwd(path.aux)
      })
    }
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
    
    output$downloadData6 = downloadHandler(
      filename = function() { 
        paste('graph3','.png', sep='') 
      },
      content = function(file) {
        png(file)
        Plot4()
        dev.off()
      },
      contentType='image/png')
    
    output$downloadData7 = downloadHandler(
      filename = function() { 
        paste('graph3','.jpg', sep='') 
      },
      content = function(file) {
        jpeg(file)
        Plot4()
        dev.off()
      },
      contentType='image/jpg')
    
    output$downloadData8 = downloadHandler(
      filename = function() { 
        paste('graph3','.pdf', sep='') 
      },
      content = function(file) {
        pdf(file)
        Plot4()
        dev.off()
      },
      contentType=NA)
    


    
    ### Fonction permettant d'afficher la description des classes par les variables
    output$descript=renderTable({
      write.infile(X=res.HCPC()$desc.var$quanti,file=paste(getwd(),"essai.csv"),sep=";")
      baba=read.csv(paste(getwd(),"essai.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      b=which(baba[,1]=="format non affichable")
      file.remove(paste(getwd(),"essai.csv")) 
      baba[,-ncol(baba)]
    },
    rownames=FALSE)
    
    ### Fonction permettant d'afficher les parangons des classes
    output$parangons=renderTable({
      bibi=list()
      for (i in 1:input$clust){
        bibi[[i]]=rbind(colnames(res.HCPC()$desc.ind$para[[i]]),res.HCPC()$desc.ind$para[[i]])
        rownames(bibi[[i]])="Distance"
}
      write.infile(X=bibi,file=paste(getwd(),"essai3.csv"),sep=";",nb.dec=8)
      baba=read.csv(paste(getwd(),"essai3.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      file.remove(paste(getwd(),"essai3.csv"))
      baba[,-ncol(baba)]
    },
    rownames=FALSE)
    
    ### Fonction permettant d'afficher la description des classes par les axes 
    output$axes=renderTable({
      write.infile(X=res.HCPC()$desc.axes$quanti,file=paste(getwd(),"essai2.csv"),sep=";",nb.dec=8)
      baba=read.csv(paste(getwd(),"essai2.csv"),sep=";",header=FALSE)
      colnames(baba)=NULL
      file.remove(paste(getwd(),"essai2.csv"))
      baba[,-ncol(baba)]
    },
    rownames=FALSE)
    
    
  }
      