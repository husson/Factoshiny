function(input, output, session) {
  
  resultat <- reactive({
    return(catdes(donnee = jdd,
                  num.var = which(colnames(jdd) == input$select_categorical_var),
                  proba = 1
    ))
  })
  
  output$select_quali <- renderUI({
    x <- colnames(jdd)[sapply(jdd,is.factor) & colnames(jdd) != input$select_categorical_var]
    selectInput(
      inputId = "select_quali_var_test",
      label = gettext("Qualitative variables",domain="R-Factoshiny"),
      multiple = TRUE,
      choices =  x,
      selected = x
    )
  })
  
  tableau_vtest <- reactive({
    
    if(!is.null(resultat()$quanti)){
      lvl <- length(resultat()$quanti)
      x <- resultat()$quanti[[1]][,"v.test", drop = FALSE]
      tabvtest <- x[sort(rownames(x)),,drop=FALSE]
      x <- resultat()$quanti[[1]][,"p.value", drop = FALSE]
      tabpvalue <- x[sort(rownames(x)),,drop=FALSE]
      for(i in 2:lvl){
        x <- resultat()$quanti[[i]][,"v.test", drop = FALSE]
        tabvtest <- cbind(tabvtest,as.data.frame(x[sort(rownames(x)),]))
        x <- resultat()$quanti[[i]][,"p.value", drop = FALSE]
        tabpvalue <- cbind(tabpvalue,as.data.frame(x[sort(rownames(x)),]))
      }
      colnames(tabpvalue) <- colnames(tabvtest) <- names(resultat()$quanti)
      
      validate(
        need(input$select_proba_plot > min(tabpvalue), paste(gettext("The p-value should be greater than",domain="R-Factoshiny"),signif(min(tabpvalue),3)))
      )
      sortie <- signif(t(as.matrix(tabvtest[apply(tabpvalue,1,min) <= input$select_proba_plot & rownames(tabvtest) %in% input$select_quanti_var,])),3)
      return(sortie)
    }
  })
  
  tableau_quali <- reactive({
    if(!is.null(resultat()$category) & input$select_categorical_var != input$select_quali_var_test){
      coldonnee <- colnames(jdd[input$select_quali_var_test])
      
      rows <- NULL
      for (i in 1:nrow(resultat()$category[[1]])) {
        if (strsplit(rownames(resultat()$category[[1]]),"=")[[i]][1]%in%coldonnee) rows <- c(rows,rownames(resultat()$category[[1]])[i])
      }
      lvl <- length(resultat()$category)
      x <- resultat()$category[[1]][rows,"v.test", drop = FALSE]
      tabvtest <- as.data.frame(x[sort(rownames(x)),])
      x <- resultat()$category[[1]][rows,"p.value", drop = FALSE]
      tabpvalue <- as.data.frame(x[sort(rownames(x)),])
      for(i in 2:lvl){
        x <- resultat()$category[[i]][rows,"v.test", drop = FALSE]
        tabvtest <- cbind(tabvtest,as.data.frame(x[sort(rownames(x)),]))
        x <- resultat()$category[[i]][rows,"p.value", drop = FALSE]
        tabpvalue <- cbind(tabpvalue,as.data.frame(x[sort(rownames(x)),]))
      }
      colnames(tabpvalue) <- colnames(tabvtest) <- names(resultat()$category)
      validate(
        need(input$select_proba_plot > min(tabpvalue), paste(gettext("The p-value should be greater than",domain="R-Factoshiny"),signif(min(tabpvalue),3)))
      )
      sortie <- signif(tabvtest[apply(tabpvalue,1,min) <= input$select_proba_plot,,drop = FALSE],3)
      return(sortie)
    }
  })
  
  output$barplot <-  renderPlot({
    plot.catdes(x = resultat(), 
                level = input$select_proba_plot,
                col.upper = input$col_up,
                col.lower = input$col_low,
                barplot = TRUE
    )
  })
  
  tab_quanti <- reactive({
    if(!is.null(resultat()$quanti)){
      quant <- seq(
        min(tableau_vtest(), na.rm = T), 
        max(tableau_vtest(), na.rm = T), 
        length.out = 100
      )
      color <- grDevices::colorRampPalette(c(input$col_low,"white",input$col_up))(length(quant)+1)
      
      a <- DT::formatStyle(
        DT::datatable(t(as.matrix(tableau_vtest())),
                      options = list(pageLength = ncol(tableau_vtest()))),
        rownames(tableau_vtest()),
        backgroundColor = DT::styleInterval(quant, color)
      )
      return(a)
    }
  })
  
  
  output$tableau_df_quanti <- DT::renderDataTable({
    if(!is.null(resultat()$quanti)) return(tab_quanti())
  })
  
  
  tab_quali <- reactive({
    if(!is.null(input$select_quali_var_test)){
      
      if( input$select_categorical_var != input$select_quali_var_test){
        
        quant <- seq(min(tableau_quali(),na.rm = TRUE), max(tableau_quali(), na.rm = TRUE), length.out = 100)
        color <- grDevices::colorRampPalette(c(input$col_low,"white",input$col_up))(length(quant)+1)
        
        a <- DT::formatStyle(
          DT::datatable(tableau_quali(), options = list(pageLength = nrow(tableau_quali()))),
          colnames(tableau_quali()),
          backgroundColor = DT::styleInterval(quant, color)
        )  
        return(a)
      }
    }
  })
  
  output$tableau_df_quali <- DT::renderDataTable({
    if (!is.null(tab_quali())) return(tab_quali())
  })
  
  tab_both <- reactive({
    tab <- NULL
    if(!is.null(input$select_quali_var_test) & !is.null(input$select_quanti_var)){
      tab <- rbind(tableau_quali(), t(tableau_vtest()))
    }
    
    if(!is.null(input$select_quali_var_test) & is.null(input$select_quanti_var)){
      tab <- tableau_quali()
    }
    
    if(is.null(input$select_quali_var_test) & !is.null(input$select_quanti_var)){
      tab <- t(tableau_vtest())
    }
    
    if (!is.null(tab)){
      quant <- seq(min(tab, na.rm = T), max(tab, na.rm = T), length.out = 100)
      color <- grDevices::colorRampPalette(c(input$col_low,"white",input$col_up))(length(quant)+1)
      
      a <- DT::formatStyle(
        DT::datatable(tab, options = list(pageLength = nrow(tab))),
        colnames(tab),
        backgroundColor = DT::styleInterval(quant, color)
      )
      return(a)
    }
  })
  
  output$tableau_df_both <- DT::renderDataTable({
    if (!is.null(tab_both())) return(tab_both())
  })
  
  observe({ input$catdesMAJ
      output$resu_catdes <- renderPrint({ isolate(catdes(donnee = jdd[,c(input$select_categorical_var,input$select_quali_var_test,input$select_quanti_var)],
                  num.var = 1, proba = input$select_proba_plot))
				  }) 
	})
  
  observe({
    if(input$Quit!=0){
      isolate({
        stopApp(returnValue=resultat())
      })
    }
  })
  
  tableau_link_quanti <- reactive({
    if(!is.null(input$select_quanti_var)){
      
      validate(
        need(input$select_proba_plot > min(resultat()$quanti.var[,"P-value"]), paste(gettext("The p-value should be greater than",domain="R-Factoshiny"),signif(min(resultat()$quanti.var[,"P-value"]),3)))
      )
      
      tab <- as.data.frame(resultat()$quanti.var[rownames(resultat()$quanti.var) %in% input$select_quanti_var  & resultat()$quanti.var[,"P-value"] <= input$select_proba_plot,c("Eta2","P-value"), drop = FALSE])
      return(tab)
    }
  })
  
  df_link_quanti <- reactive({
    if(!is.null(input$select_quanti_var)){
      
      quant <- seq(min(tableau_link_quanti()[,1]), max(tableau_link_quanti()[,1]), length.out = 100)
      color <- grDevices::colorRampPalette(c(input$col_low,"white",input$col_up))(length(quant)+1)
      
      a <- DT::formatStyle(
        DT::datatable(signif(tableau_link_quanti(),3),
                      options = list(pageLength = nrow(tableau_link_quanti()))),
        target = 'row',
        columns = colnames(tableau_link_quanti())[1],
        backgroundColor = DT::styleInterval(quant, color)
      )
    }
    return(a)
  })
  
  output$table_link_quanti <- DT::renderDataTable({
    if (!is.null(df_link_quanti())) return(df_link_quanti())
  })
  
  tableau_link_chisquare <- reactive({
    if(!is.null(resultat()$test.chi2)){
      
      validate(
        need(input$select_proba_plot > min(resultat()$test.chi2[,"p.value"]), paste(gettext("The p-value should be greater than",domain="R-Factoshiny"),signif(min(resultat()$test.chi2[,"p.value"]),3)))
      )
      tab <- (resultat()$test.chi2[rownames(resultat()$test.chi2) %in% input$select_quali_var_test & resultat()$test.chi2[,"p.value"] <= input$select_proba_plot,"p.value", drop = FALSE])
      return(tab)
    }
  })
  
  tab_chisquare <- reactive({
    if(!is.null(resultat()$test.chi2)){
      if(nrow(tableau_link_chisquare()) > 0){
        
        quant <- seq(min(tableau_link_chisquare()), max(tableau_link_chisquare()), length.out = 100)
        color <- grDevices::colorRampPalette(c(input$col_up,"white",input$col_low))(length(quant)+1)
        
        a <- DT::formatStyle(
          DT::datatable(signif(tableau_link_chisquare(),3),
                        options = list(pageLength = nrow(tableau_link_chisquare()))),
          target = 'row',
          columns = colnames(tableau_link_chisquare()),
          backgroundColor = DT::styleInterval(quant, color)
        )
        return(a)
      }
    }
  })
  
  output$table_link_chisquare <- DT::renderDataTable({
    if(!is.null(tab_chisquare())) return(tab_chisquare())
  })
  
  output$quanti_quali_both1 <- renderUI({
    
    old.x <- gettext("Both",domain="R-Factoshiny")
    if(!is.null(input$quanti_quali_both)) old.x <- input$quanti_quali_both
    if(!is.null(input$select_quali_var_test) & !is.null(input$select_quanti_var)){
      x <- c(gettext("Both",domain="R-Factoshiny"),gettext("Quantitative",domain="R-Factoshiny"),gettext("Qualitative",domain="R-Factoshiny"))
    }
    
    if(is.null(input$select_quali_var_test) & !is.null(input$select_quanti_var)){
      x <- c(gettext("Quantitative",domain="R-Factoshiny"))
    }
    
    if(!is.null(input$select_quali_var_test) & is.null(input$select_quanti_var)){      
      x <- c(gettext("Qualitative",domain="R-Factoshiny"))
    }
    
    if (old.x%in%x){
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
  })
    
  observe({
    if(input$download_tabquanti != 0){
      NameFile <- tcltk::tclvalue(tcltk::tcl("tk_getSaveFile"))
	  if (!(any(strsplit(NameFile,split="[.]")[[1]]=="html")) & !(any(strsplit(NameFile,split="[.]")[[1]]=="htm"))) NameFile <- paste0(NameFile,".html")
      htmlwidgets::saveWidget(widget = df_link_quanti(), file = NameFile)
    }
  })
  
  observe({
    if(input$download_tabquali != 0){
	  NameFile <- tcltk::tclvalue(tcltk::tcl("tk_getSaveFile"))
	  if (!(any(strsplit(NameFile,split="[.]")[[1]]=="html")) & !(any(strsplit(NameFile,split="[.]")[[1]]=="htm"))) NameFile <- paste0(NameFile,".html")
      htmlwidgets::saveWidget(widget =  tab_chisquare(), file = NameFile)
    }
  })
  
  observe({
    if(input$download_cate_quanti != 0){
	  NameFile <- tcltk::tclvalue(tcltk::tcl("tk_getSaveFile"))
	  if (!(any(strsplit(NameFile,split="[.]")[[1]]=="html")) & !(any(strsplit(NameFile,split="[.]")[[1]]=="htm"))) NameFile <- paste0(NameFile,".html")
      htmlwidgets::saveWidget(widget =  tab_quanti(), file = NameFile)
    }
  })
  
  observe({
    if(input$download_cate_quali != 0){
	  NameFile <- tcltk::tclvalue(tcltk::tcl("tk_getSaveFile"))
	  if (!(any(strsplit(NameFile,split="[.]")[[1]]=="html")) & !(any(strsplit(NameFile,split="[.]")[[1]]=="htm"))) NameFile <- paste0(NameFile,".html")
      htmlwidgets::saveWidget(widget =  tab_quali(), file = NameFile)
    }
  })
  
  observe({
    if(input$download_cate_both != 0){
	  NameFile <- tcltk::tclvalue(tcltk::tcl("tk_getSaveFile"))
	  if (!(any(strsplit(NameFile,split="[.]")[[1]]=="html")) & !(any(strsplit(NameFile,split="[.]")[[1]]=="htm"))) NameFile <- paste0(NameFile,".html")
      htmlwidgets::saveWidget(widget =  tab_both(), file = NameFile)
      # htmlwidgets::saveWidget(widget =  tab_both(), file = paste0(getwd(),"cate_both.html"))
    }
  })
  
}
