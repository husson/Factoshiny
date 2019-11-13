# ui script for FAMD2
fluidPage(
  titlePanel(div(paste(gettext("FAMD on the dataset ",domain="R-Factoshiny"),nomDatacourt),style="color:#6E6E6E",align="center"),windowTitle="FAMDshiny"),

  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #ffd77a; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
        tags$style(type='text/css', "#title2 { height: 25px; }"),
        tags$style(type='text/css', "#title3 { height: 25px; }")
      ),
      wellPanel(
      div(align="center",checkboxInput("famdparam",gettext("FAMD parameters",domain="R-Factoshiny"),FALSE)),
      conditionalPanel(
        condition="input.famdparam==true",
          selectizeInput("supvar",label=gettext("Select supplementary quantitative variables",domain="R-Factoshiny"), choices=VariableChoices, selected=quantisup,multiple=TRUE),
          selectInput("supvar1",label=gettext("Select supplementary categorical variables",domain="R-Factoshiny"),choices=QualiChoice,multiple=TRUE,selected=qualisup),
          selectizeInput("indsup",gettext("Select supplementary individuals",domain="R-Factoshiny"),choices=nom, multiple=TRUE,selected=indsupl),
            uiOutput("imputeData"),
          actionButton("submit", label = gettext("Submit",domain="R-Factoshiny"))
      ),
      style = "padding: 3px;background-color: #ffdbdb;"),
      wellPanel(
      div(align="center",checkboxInput("graph",gettext("Graphical options",domain="R-Factoshiny"),FALSE)),
      conditionalPanel(
        condition="input.graph==true",
        div(gettext("Axes:",domain="R-Factoshiny"), style="display: inline-block;padding: 5px"),
        div(uiOutput("NB1"), style="display: inline-block;"),
        div(uiOutput("NB2"), style="display: inline-block;"),
        div(align="center",selectInput("choixgraph",gettext("Which graph would you like to modify?",domain="R-Factoshiny"), choices=list(gettext("Individuals and categories",domain="R-Factoshiny"),"Variables"="var",gettext("Quantitative variables",domain="R-Factoshiny")),selected=gettext("Individuals and categories",domain="R-Factoshiny"))),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Individuals and categories",domain="R-Factoshiny"),"'",sep=''),
          textInput("title1",gettext("Title of the graph: ",domain="R-Factoshiny"), title1),
          uiOutput("choixindmod"),
          sliderInput("cex",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=size,step=0.05,ticks=FALSE),
          selectInput("select",label=gettext("Draw individuals according to:",domain="R-Factoshiny"),
                      choices=list("No selection"="NONE","cos2"="cos2","Contribution"="contrib","Manual"="Manuel"),selected=selection),
          conditionalPanel(
            condition="input.select=='cos2'",
            if(selection=="cos2"){
            div(align="center",sliderInput("slider1", label = "cos2",
                        min = 0, max = 1, value =as.numeric(selection2),step=0.05))}
            else{
              div(align="center",sliderInput("slider1", label = "cos2",
                                             min = 0, max = 1, value =0,step=0.05))
            }),
          conditionalPanel(
            condition="input.select=='contrib'",
            uiOutput("slider7")),
          conditionalPanel(
            condition="input.select=='Manuel'",
            if(selection=="Manuel"){
              selectInput("indiv",label=gettext("Select individuals",domain="R-Factoshiny"), choices=nom,multiple=TRUE,selected=selection2)
			} else{
              selectInput("indiv",label=gettext("Select individuals",domain="R-Factoshiny"), choices=nom,multiple=TRUE)
            }),
            checkboxInput("habi",gettext("Points colour depend on categorical variable",domain="R-Factoshiny"),!is.null(habillageind)),
            conditionalPanel(
              condition="input.habi==true",
              uiOutput("habillage2")
            )
        ),
        conditionalPanel(
          condition="input.choixgraph=='var'",
          textInput("title2",gettext("Title of the graph: ",domain="R-Factoshiny"), title2),
          sliderInput("cex2",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=size2,step=0.05,ticks=FALSE),
          br(),
          selectInput("select0",label=gettext("Draw variables according to:",domain="R-Factoshiny"),
                      choices=list("No selection"="NONE","cos2"="cos2","Contribution"="contrib"),selected=selection3),
          conditionalPanel(
            condition="input.select0=='contrib'",
            uiOutput("slider3")
            ),
          conditionalPanel(
            condition="input.select0=='cos2'",
            if(selection3=="cos2"){
              div(align="center",sliderInput("slider00", label = "cos2",
                                             min = 0, max = 1, value =as.numeric(selection4),step=0.05))  
            }
            else{
            div(align="center",sliderInput("slider00", label = "cos2",
                                           min = 0, max = 1, value =0,step=0.05))}
          )
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Quantitative variables",domain="R-Factoshiny"),"'",sep=''),
          textInput("title3",gettext("Title of the graph: ",domain="R-Factoshiny"), title3),
          sliderInput("cex3",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=size3,step=0.05,ticks=FALSE),
          br(),
          selectInput("selecti",label=gettext("Draw variables according to:",domain="R-Factoshiny"),
                      choices=list("No selection"="NONE","cos2"="cos2","Contribution"="contrib"),selected=selection5),
          conditionalPanel(
            condition="input.selecti=='contrib'",
            uiOutput("slider5")
          ),
          conditionalPanel(
            condition="input.selecti=='cos2'",
            if(selection3=="cos2"){
              div(align="center",sliderInput("slider000", label = "cos2",
                                             min = 0, max = 1, value =as.numeric(selection6),step=0.05))  
            }
            else{
              div(align="center",sliderInput("slider000", label = "cos2",
                                             min = 0, max = 1, value =0,step=0.05))}
          )
        )
      ),
      style = "padding: 3px;background-color: #fcefba"),
      wellPanel(
        div(align="center",checkboxInput("hcpcparam",gettext("Perform clustering after leaving FAMD app?",domain="R-Factoshiny"),hcpcparaFAMDshiny)),
        conditionalPanel(
          condition="input.hcpcparam==true",
          uiOutput("NbDimForClustering")
        ),
        align="center", style = "padding: 3px;background-color: #ecffdb"
      ),
      div(align="center",actionButton("FAMDcode", gettext("Get the FAMD code",domain="R-Factoshiny"),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;')),
      div(align="center",actionButton("Quit", gettext("Quit the app",domain="R-Factoshiny"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
      
      mainPanel(
        tabsetPanel(id = "graph_sort",
                    tabPanel(gettext("Graphs",domain="R-Factoshiny"),
 fluidRow(
                           br(),
                 column(width = 6,shinyjqui::jqui_resizable(plotOutput("map2", height="500")),
                             br(),
                             p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData4",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData3",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData5",gettext("pdf",domain="R-Factoshiny")),align="center"),
                             br(),
							 align="center"),
                 column(width = 6,shinyjqui::jqui_resizable(plotOutput("map", height="500")),
                             br(),
                             p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData1",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData2",gettext("pdf",domain="R-Factoshiny")),align="center"),
                             br(),
							 align="center")),
 fluidRow(
                           br(),
                 column(width = 6,shinyjqui::jqui_resizable(plotOutput("map4", height="500")),
                             br(),
                             p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData6",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData7",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData8",gettext("pdf",domain="R-Factoshiny")),align="center"),
                             align="center"))),

                    tabPanel(gettext("Values",domain="R-Factoshiny"),
                             br(),
                             uiOutput("out22", width = "500", height="500"),
                             br(),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Eigenvalues",domain="R-Factoshiny"),"'",sep=''),
                               shinyjqui::jqui_resizable(plotOutput("map3", height="500")),
                               div(align="center",tableOutput("sorties"))),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results of the variables",domain="R-Factoshiny"),"'",sep=''),
                               h6(gettext("Coordinates",domain="R-Factoshiny")),
                               div(align="center",tableOutput("sorties2")),
                               br(),
                               h6("Contributions"),
                               div(align="center",tableOutput("sorties3")),
                               br(),
                               h6("Cos2"),
                               div(align="center",tableOutput("sorties4"))),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results of the individuals",domain="R-Factoshiny"),"'",sep=''),
                               h6(gettext("Coordinates",domain="R-Factoshiny")),
                               div(align="center",tableOutput("sorties22")),
                               br(),
                               h6("Contributions"),
                               div(align="center",tableOutput("sorties33")),
                               br(),
                               h6("Cos2"),
                               div(align="center",tableOutput("sorties44"))),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Summary of outputs",domain="R-Factoshiny"),"'",sep=''),
                               numericInput("nbele",gettext("Number of elements to print",domain="R-Factoshiny"),value=10),
                               verbatimTextOutput("summaryFAMD"),
                               p(downloadButton("summary2",gettext("Download the summary",domain="R-Factoshiny")),align="center")),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results of the supplementary variables",domain="R-Factoshiny"),"'",sep=''),
                               h6(gettext("Coordinates",domain="R-Factoshiny")),
                               div(align="center",tableOutput("sorties23")),
                               h6("Cos2"),
                               div(align="center",tableOutput("sorties32"))),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results of the supplementary individuals",domain="R-Factoshiny"),"'",sep=''),
                               h6(gettext("Coordinates",domain="R-Factoshiny")),
                               div(align="center",tableOutput("sorties36")),
                               h6("Cos2"),
                               div(align="center",tableOutput("sorties37")))
                             ),
                  tabPanel(gettext("Automatic description of axes",domain="R-Factoshiny"),
                           br(),
                           numericInput("pvalueDimdesc",gettext("P-value",domain="R-Factoshiny"),value=pvalueDimdescInit, min=0,max=1),
                           radioButtons("Dim",label=gettext("Choose the dimensions",domain="R-Factoshiny"),choices=list("Dimension 1"="Dim1","Dimension 2"="Dim2","Dimension 3"="Dim3"),selected="Dim1"),
                           conditionalPanel(
                             condition="input.Dim=='Dim1'",
                             p("Quantitative"),
                             div(align="center",tableOutput("sortieDimdesc3")),
                             p("Qualitative"),
                             div(align="center",tableOutput("sortieDimdesc4"))
                           ),
                           br(),
                           conditionalPanel(
                             condition="input.Dim=='Dim2'",
                             p("Quantitative"),
                             div(align="center",tableOutput("sortieDimdesc33")),
                             p("Qualitative"),
                             div(align="center",tableOutput("sortieDimdesc44"))
                           ),
                           br(),
                           conditionalPanel(
                             condition="input.Dim=='Dim3'",
                             p("Quantitative"),
                             div(align="center",tableOutput("sortieDimdesc333")),
                             p("Qualitative"),
                             div(align="center",tableOutput("sortieDimdesc444"))
                           )
                  ),
                    tabPanel(gettext("Summary of dataset",domain="R-Factoshiny"),
                             br(),
                             verbatimTextOutput("summary"),
                             br(),
                             selectInput("bam",h6(gettext("Graphs for",domain="R-Factoshiny")),choices=allVariables,multiple=FALSE),
                             plotOutput("histo")),
                    
                    tabPanel(gettext("Data",domain="R-Factoshiny"),
                             br(),
                             DT::dataTableOutput("JDD")
                             )
        )
      ,width=9)
    )
)
