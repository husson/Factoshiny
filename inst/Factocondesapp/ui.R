library(shiny)
shinyUI(fluidPage(
  
  titlePanel(gettext("Description of a quantitative variable",domain="R-Factoshiny")),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "select_quanti_var",
        label = gettext("Variable to explain",domain="R-Factoshiny"),
        multiple = FALSE,
        choices = colnames(my_data)[sapply(my_data,is.numeric)],
        selected = choix_var_expliquer
      ),
      
      
      numericInput(
        inputId = "select_proba_plot",
        label = gettext("P-value",domain="R-Factoshiny"),
        min = 0,
        max = 1,
        value = valeur_proba,
        step = 0.1
      ),
      
      wellPanel(
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            colourpicker::colourInput(
              inputId = "col_low",
              label = gettext("Low color",domain="R-Factoshiny"),
              value = couleur_basse,
              returnName = TRUE
            )),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            colourpicker::colourInput(
              inputId = "col_up",
              label = gettext("High color",domain="R-Factoshiny"),
              value = couleur_haute,
              returnName = TRUE
            )),style = "text-align : center; padding : 2px"),
      
      
      uiOutput(outputId = "quanti_quali_category"),
      uiOutput(outputId = "select_quanti"),
      uiOutput(outputId = "select_quali"),
      

      
      
      
      actionButton(
        "Quit", 
        gettext("Quit the app",domain="R-Factoshiny"),
        style='padding:5px;text-align:center;white-space: normal;'
      )
      
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = gettext("Summary",domain="R-Factoshiny"),
          conditionalPanel(
            condition = paste("input.quanti_quali_both == '",gettext("Quantitative",domain="R-Factoshiny"),"'", sep = ""),
            h4(gettext("Description of a quantitative variable by quantitative variables",domain="R-Factoshiny")),
            actionButton("Download_quanti",gettext("HTML")),
            DT::dataTableOutput("tableau_df_quanti",
                                width = "80%")
          ),
          
          conditionalPanel(
            condition = paste("input.quanti_quali_both == '",gettext("Qualitative",domain="R-Factoshiny"),"'", sep = ""),
            h4(gettext("Description of a quantitative variable by qualitative variables",domain="R-Factoshiny")),
            actionButton("Download_quali",gettext("HTML",domain="R-Factoshiny")),
            DT::dataTableOutput("tableau_df_quali",width = "80%")
          ),
          
          conditionalPanel(
            condition = paste("input.quanti_quali_both == '",gettext("Category",domain="R-Factoshiny"),"'", sep = ""),
            h4(gettext("Description of a quantitative variable by categories",domain="R-Factoshiny")),
            actionButton("Download_category",gettext("HTML",domain="R-Factoshiny")),
            DT::dataTableOutput("tableau_df_both",width = "80%")
          )
          
          
          
        ),
        
        tabPanel(
          title = gettext("Condes Output"),
          actionButton("condesMAJ", gettext("Update the output",domain="R-Factoshiny")),
          verbatimTextOutput("resu_condes")
        )
      )
      
    )
  )
)
)
