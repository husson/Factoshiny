library(shiny)
shinyServer(function(input, output, session) {
  
  resultat <- reactive({
    
    
    
    sortie <- condes(donnee = my_data,
                     num.var = which(colnames(my_data) == input$select_quanti_var),
                     weight = NULL,
                     proba = 1
    )
    return(sortie)
  })
  
  tab_quantitative <- reactive({
    resu <- resultat()

    if(!is.null(resu$quanti)){
      
      
      validate(
        need(length(input$select_quanti_var_test) > 0, gettext("Put a quantitative variable",domain="R-Factoshiny"))
      )
      
      
      
      if(length(input$select_quanti_var_test) == 1){
        
        sortie <- signif(resu$quanti[resu$quanti[,"p.value"] <= input$select_proba_plot & rownames(resu$quanti) %in% input$select_quanti_var_test,,drop = FALSE],3)
        
      } else if(length(input$select_quanti_var_test) > 1){
        
        sortie <- signif(resu$quanti[resu$quanti[,"p.value"] <= input$select_proba_plot & rownames(resu$quanti) %in% input$select_quanti_var_test,,drop = FALSE],3)
        
      } else {
        
        sortie <- matrix(rep(0,2), nrow = 1)
        
      }
      if(min(resu$quanti[rownames(resu$quanti) %in% input$select_quanti_var_test,"p.value"],50, na.rm = TRUE) < 50){
        validate(
          need(
            input$select_proba_plot >= min(resu$quanti[rownames(resu$quanti) %in% input$select_quanti_var_test,"p.value"], na.rm = TRUE), paste(gettext("The p-value should be greater than"),signif(min(resu$quanti[rownames(resu$quanti) %in% input$select_quanti_var_test,"p.value"],na.rm = TRUE),3))
          )
        )
      }
      
      
      
      
      return(sortie)
    }
    
  })
  
  
  
  tableau_quali <- reactive({
    resu <- resultat()
    
    validate(
      need(length(input$select_quali_var_test) > 0, gettext("Put a qualitative variable",domain="R-Factoshiny"))
    )
    
    if(length(input$select_quali_var_test) > 1){
      
      sortie <- as.matrix(
        signif(
          resu$quali[resu$quali[,"p.value"] <= input$select_proba_plot & rownames(resu$quali) %in% input$select_quali_var_test,,drop = FALSE],3))
      
    } else if(length(input$select_quali_var_test) == 1){
      
      sortie <- signif(as.matrix(resu$quali[resu$quali[,"p.value"] <= input$select_proba_plot & rownames(resu$quali) %in% input$select_quali_var_test ,,drop = FALSE]),3)
      
    } else {
      sortie <- NA
    }
    
    validate(
      need(input$select_proba_plot > min(resu$quali[rownames(resu$quali) %in% input$select_quali_var_test,"p.value"], na.rm = TRUE), paste(gettext("The p-value should be greater than"),signif(min(resu$quali[rownames(resu$quali) %in% input$select_quali_var_test,"p.value"],na.rm = TRUE),3)))
    )
    
    return(sortie)
    
    
    
  })
  
  tableau_category <- reactive({
    
    resu <- resultat()
    
    tabmodalite <- as.data.frame(unlist(lapply(rownames(resu$category),FUN = function(i){unlist(strsplit(x = i,split = "="))[1]})))
    colnames(tabmodalite) <- "modalite"
    
    sortie <- signif(resu$category[resu$category[,"p.value"] <= input$select_proba_plot & tabmodalite$modalite %in% input$select_quali_var_test,,drop = FALSE],3)
    validate(
      need(input$select_proba_plot > min(resu$category[tabmodalite$modalite %in% input$select_quali_var_test,"p.value"], na.rm = TRUE), paste(gettext("The p-value should be greater than"),signif(min(resu$category[tabmodalite$modalite %in% input$select_quali_var_test,"p.value"],na.rm = TRUE),3)))
    )
    
    return(sortie)
    
    
    
  })
  
  tableau_df_quanti_reactive <- reactive({
    if(!is.null(tab_quantitative())){
      if(nrow(tab_quantitative()) > 0){
        
        quant <- seq(
          min(tab_quantitative(), na.rm = TRUE), 
          max(tab_quantitative(), na.rm = TRUE), 
          length.out = 100
        )
        
        color <- grDevices::colorRampPalette(c(input$col_up,"white",input$col_low))(length(quant)+1)
        
        a <- DT::formatStyle(
          DT::datatable(tab_quantitative(),
                        extensions = c('Buttons','FixedColumns','FixedHeader'),
                        options = list(
                          pageLength = nrow(tab_quantitative()),
                          dom = 'Bfrtip',
                          buttons = c('csv'),
                          fixedColumns = TRUE,
                          fixedHeader = TRUE
                        )
          ),
          columns = 1:2,
          valueColumns = 2,
          backgroundColor = DT::styleInterval(quant, color)
        )
        return(a)
      }
    }
    
  })
  
  
  output$tableau_df_quanti <- DT::renderDataTable({
    return(tableau_df_quanti_reactive())
  })
  
  tableau_df_quali_reactive <- reactive({
    if(length(input$select_quali_var_test) > 0){
      quant <- seq(
        min(tableau_quali()[,2],1, na.rm = TRUE), 
        max(tableau_quali()[,2],1, na.rm=TRUE), 
        length.out = 100)
      color <- grDevices::colorRampPalette(c(input$col_low,"white",input$col_up))(length(quant)+1)
      
      a <- DT::formatStyle(
        DT::datatable(tableau_quali(),
                      extensions = c('Buttons','FixedColumns','FixedHeader'),
                      options = list(
                        pageLength = nrow(tableau_quali()),
                        dom = 'Bfrtip',
                        buttons = c('csv'),
                        fixedColumns = TRUE,
                        fixedHeader = TRUE
                      )
        ),
        columns = 1:2,
        valueColumns = 2,
        backgroundColor = DT::styleInterval(quant, color)
      )
      return(a)
    }
  })
  
  output$tableau_df_quali <- DT::renderDataTable({
    return(tableau_df_quali_reactive())
  })
  
  tableau_df_both_reactive <- reactive({
    if(length(input$select_quali_var_test) > 0){
      
      tab <- as.matrix(tableau_category())
      quant <- seq(min(tab[,2],1, na.rm = T), max(tab[,2],1, na.rm = T), length.out = 100)
      color <- grDevices::colorRampPalette(c(input$col_up,"white",input$col_low))(length(quant)+1)
      
      a <- DT::formatStyle(
        DT::datatable(tab,
                      extensions = c('Buttons','FixedColumns','FixedHeader'),
                      options = list(
                        pageLength = nrow(tab),
                        dom = 'Bfrtip',
                        buttons = c('csv'),
                        fixedColumns = TRUE,
                        fixedHeader = TRUE
                      )
        ),
        columns = 1:2,
        valueColumns = 2,
        backgroundColor = DT::styleInterval(quant, color)
      )
      return(a)
    }
  })
  
  output$tableau_df_both <- DT::renderDataTable({
    return(tableau_df_both_reactive())
  })
  
  
  output$resu_condes <- renderPrint({
    
    resultat()[names(resultat()) != "call"]
  })
  
  
  output$select_quali <- renderUI({
    x <- colnames(my_data)[sapply(my_data,is.factor)]
    selectInput(
      inputId = "select_quali_var_test",
      label = gettext("Qualitative variables",domain="R-Factoshiny"),
      multiple = TRUE,
      choices =  x,
      selected = choix_var_quali
    )
  })
  
  output$select_quanti <- renderUI({
    x <- colnames(my_data)[sapply(my_data,is.numeric) & colnames(my_data) != input$select_quanti_var]
    selectInput(
      inputId = "select_quanti_var_test",
      label = gettext("Quantitative variables",domain="R-Factoshiny"),
      multiple = TRUE,
      choices =  x,
      selected = choix_var_quanti
    )
  })
  
  
  
  output$quanti_quali_category <- renderUI({
    
    
    if(!is.null(input$select_quanti_var_test) | !is.null(input$select_quali_var_test)){
      old.x <- gettext("Quantitative",domain="R-Factoshiny")
      if(!is.null(input$quanti_quali_both)) old.x <- input$quanti_quali_both
      if(sum(sapply(my_data[,c(input$select_quanti_var,input$select_quanti_var_test, input$select_quali_var_test)], is.factor)) > 0 &
         sum(sapply(my_data[,c(input$select_quanti_var,input$select_quanti_var_test, input$select_quali_var_test)], is.numeric)) > 1){
        # if(!is.null(resultat()$quanti) & !is.null(resultat()$quali) & !is.null(resultat()$category))
        
        x <- c(gettext("Quantitative",domain="R-Factoshiny"),gettext("Qualitative",domain="R-Factoshiny"),gettext("Category",domain="R-Factoshiny"))
        
      }
      
      
      else if(sum(sapply(my_data[,c(input$select_quanti_var,input$select_quanti_var_test, input$select_quali_var_test)], is.factor)) == 0 &
              sum(sapply(my_data[,c(input$select_quanti_var,input$select_quanti_var_test, input$select_quali_var_test)], is.numeric)) > 1){
        
        x <- c(gettext("Quantitative",domain="R-Factoshiny"))
        
      }
      
      
      else if(sum(sapply(my_data[,c(input$select_quanti_var,input$select_quanti_var_test, input$select_quali_var_test)], is.factor)) > 0 &
              sum(sapply(my_data[,c(input$select_quanti_var,input$select_quanti_var_test, input$select_quali_var_test)], is.numeric)) == 1){
        
        x <- c(gettext("Qualitative",domain="R-Factoshiny"),gettext("Category",domain="R-Factoshiny"))
        
      }
      if(old.x %in% x){
        radioButtons(
          inputId = "quanti_quali_both",
          choices = x,
          inline = TRUE,
          label = gettext("Describe by ... variables",domain="R-Factoshiny"),
          selected = old.x
        )
      } else {
        radioButtons(
          inputId = "quanti_quali_both",
          choices = x,
          inline = TRUE,
          label = gettext("Describe by ... variables",domain="R-Factoshiny")
        )
      }
    }
  })
  
  
  observeEvent(input$Download_quanti,{
    NameFile <- tcltk::tclvalue(tcltk::tcl("tk_getSaveFile"))
    if (!(any(strsplit(NameFile,split="[.]")[[1]]=="html")) & !(any(strsplit(NameFile,split="[.]")[[1]]=="htm"))) NameFile <- paste0(NameFile,".html")
    htmlwidgets::saveWidget(widget = tableau_df_quanti_reactive(), file = NameFile)
    
  })
  
  
  observeEvent(input$Download_quali,{
    NameFile <- tcltk::tclvalue(tcltk::tcl("tk_getSaveFile"))
    if (!(any(strsplit(NameFile,split="[.]")[[1]]=="html")) & !(any(strsplit(NameFile,split="[.]")[[1]]=="htm"))) NameFile <- paste0(NameFile,".html")
    htmlwidgets::saveWidget(widget = tableau_df_quali_reactive(), file = NameFile)
    
  })
  
  
  observeEvent(input$Download_category,{
    NameFile <- tcltk::tclvalue(tcltk::tcl("tk_getSaveFile"))
    if (!(any(strsplit(NameFile,split="[.]")[[1]]=="html")) & !(any(strsplit(NameFile,split="[.]")[[1]]=="htm"))) NameFile <- paste0(NameFile,".html")
    htmlwidgets::saveWidget(widget = tableau_df_both_reactive(), file = NameFile)
  })
  
  observeEvent(input$condesMAJ,{
    output$resu_condes <- renderPrint({
      isolate(
        return(condes(donnee = my_data[,c(input$select_quanti_var,input$select_quali_var_test,input$select_quanti_var_test)],
                      num.var = 1, 
                      proba = input$select_proba_plot)[names(condes(donnee = my_data[,c(input$select_quanti_var,input$select_quali_var_test,input$select_quanti_var_test)],
                                                                    num.var = 1, 
                                                                    proba = input$select_proba_plot)) != "call"]
        )
      )
    }) 
  })
  
  liste_retourner <- reactive({
    retour <- list()
    retour$donnees = my_data
    retour$explain = input$select_quanti_var
    retour$proba = input$select_proba_plot
    retour$col_basse = input$col_low
    retour$col_haute = input$col_up
    retour$var_quanti = input$select_quanti_var_test
    retour$var_quali = input$select_quali_var_test
    class(retour) <- c("condesshiny", "list")
    
    return(retour)
  })
  
  observe({
    if(input$Quit!=0){
      isolate({
        stopApp(returnValue=liste_retourner())
      })
    }
  })
  
  
  
  
  
  
  
})
