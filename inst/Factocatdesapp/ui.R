fluidPage(
  
  titlePanel(gettext("Description of a variable and its categories",domain="R-Factoshiny")),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "select_categorical_var",
        label = gettext("Variable to explain",domain="R-Factoshiny"),
        multiple = FALSE,
        choices = colnames(jdd)[sapply(jdd,is.factor)],
        selected = colnames(jdd)[sapply(jdd,is.factor)][1]
      ),
      
      
      numericInput(
        inputId = "select_proba_plot",
        label = gettext("P-value",domain="R-Factoshiny"),
        min = 0.0001,
        max = 1,
        value = 1,
        step = 0.1
      ),
      
      wellPanel(
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            colourpicker::colourInput(
              inputId = "col_low",
              label = gettext("Low color",domain="R-Factoshiny"),
              value = "#03F7FF",
              returnName = TRUE
            )),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            colourpicker::colourInput(
              inputId = "col_up",
              label = gettext("High color",domain="R-Factoshiny"),
              value = "#F00000",
              returnName = TRUE
            )),style = "text-align : center; padding : 2px"),
      
      
      uiOutput("quanti_quali_both1"),
      
      selectInput(
        inputId = "select_quanti_var",
        label = gettext("Quantitative variables",domain="R-Factoshiny"),
        multiple = TRUE,
        choices = colnames(jdd)[(sapply(jdd,is.numeric))],
        selected = colnames(jdd)[(sapply(jdd,is.numeric))]
      ),
      
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
          title = gettext("Categories description",domain="R-Factoshiny"),
          conditionalPanel(
            condition=paste("input.quanti_quali_both=='",gettext("Quantitative",domain="R-Factoshiny"),"'",sep=''),
            h4(gettext("Description of each category by quantitative variables",domain="R-Factoshiny")),
            actionButton("download_cate_quanti", gettext("Download",domain="R-Factoshiny")),
            DT::dataTableOutput("tableau_df_quanti",
                                width = "80%")
          ),
          
          conditionalPanel(
            condition=paste("input.quanti_quali_both=='",gettext("Qualitative",domain="R-Factoshiny"),"'",sep=''),
            h4(gettext("Description of each category by the categories of the other variables",domain="R-Factoshiny")),
            actionButton("download_cate_quali", gettext("Download",domain="R-Factoshiny")),
            DT::dataTableOutput("tableau_df_quali",
                                width = "80%")
          ),
          
          conditionalPanel(
            condition=paste("input.quanti_quali_both=='",gettext("Both",domain="R-Factoshiny"),"'",sep=''),
            h4(gettext("Description of each category by quantitative variables and categories",domain="R-Factoshiny")),
            actionButton("download_cate_both", gettext("Download",domain="R-Factoshiny")),
            DT::dataTableOutput("tableau_df_both",
                                width = "80%")
          )
        ),
        tabPanel(
          title = gettext("Links with the variable",domain="R-Factoshiny"),
          conditionalPanel(condition = paste("input.quanti_quali_both=='",gettext("Quantitative",domain="R-Factoshiny"),"' || input.quanti_quali_both=='",gettext("Both",domain="R-Factoshiny"),"'",sep=''),
                           h4(gettext("Link with the quantitative variables",domain="R-Factoshiny")),
                           actionButton("download_tabquanti", gettext("Download",domain="R-Factoshiny")),
                           DT::dataTableOutput("table_link_quanti")
          ),

          conditionalPanel(condition = paste("input.quanti_quali_both=='",gettext("Qualitative",domain="R-Factoshiny"),"' || input.quanti_quali_both=='",gettext("Both",domain="R-Factoshiny"),"'",sep=''),
                           h4(gettext("Link with the qualitative variables",domain="R-Factoshiny")),
                           actionButton("download_tabquali", gettext("Download",domain="R-Factoshiny")),
                           DT::dataTableOutput("table_link_chisquare")
          )

        ),
        
        tabPanel(
          title = gettext("Barplot",domain="R-Factoshiny"),
          plotOutput("barplot")
        ),
        
        
        # tabPanel(
        #   title = gettext("Catdes output",domain="R-Factoshiny"),
        #   verbatimTextOutput("resu_catdes")
        # )
        
        tabPanel(
          title = gettext("Catdes output",domain="R-Factoshiny"),
          actionButton("catdesMAJ", gettext("Update the output",domain="R-Factoshiny")),
          verbatimTextOutput("resu_catdes")
        )
      )
      
    )
  )
)

