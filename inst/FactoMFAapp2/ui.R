# ui.R AFM2

fluidPage(
  titlePanel(div(paste(gettext("MFA on the dataset "),nomDatacourt),style="color:#6E6E6E",align="center"),windowTitle="MFAshiny"),
  sidebarLayout(
      sidebarPanel(
        tags$head(
          tags$style("body {background-color: #E1D3FB; }"),
          tags$style(type='text/css', "#title1 { height: 25px; }"),
          tags$style(type='text/css', "#title2 { height: 25px; }"),
          tags$style(type='text/css', "#title3 { height: 25px; }"),
          tags$style(type='text/css', "#title4 { height: 25px; }"),
          tags$style(type='text/css', "#title5 { height: 25px; }")
        ),
        wellPanel(
        div(align="center",checkboxInput("graph",gettext("Show graphs options"),FALSE)),
        conditionalPanel(
          condition="input.graph==true",
          div(gettext("Axes:"), style="display: inline-block;padding: 5px"),
          div(uiOutput("NB1"), style="display: inline-block;"),
          div(uiOutput("NB2"), style="display: inline-block;"),
          uiOutput("choixgraphic"),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Individuals"),"'",sep=''),
          textInput("titleInd",gettext("Title of the graph: "), titleInd),
          sliderInput("cexInd",gettext("Size of labels"),min=0.5,max=2.5,value=sizeInd,step=0.05,ticks=FALSE),
            uiOutput("choixindvar"),
            selectInput("select",label=gettext("Labels for individuals selected by:"), choices=list(gettext("No selection"),"cos2"="cos2","Contribution"="contrib",gettext("Manual")),selected=selectionMFAshiny),
            conditionalPanel(
              condition="input.select=='cos2'",
              if(selectionMFAshiny=="cos2"){
                div(align="center",sliderInput("sliderind1", label = gettext("Labels for cos2 greater than"),
                                               min = 0, max = 1, value =as.numeric(selection2MFAshiny),step=0.05))}
              else{
                div(align="center",sliderInput("sliderind1", label = gettext("Labels for cos2 greater than"),
                                               min = 0, max = 1, value =0,step=0.05))
              }),
			  
            conditionalPanel(
              condition="input.select=='contrib'",
              if(selectionMFAshiny=="contrib"){
                div(align="center",sliderInput("sliderind0", label = gettext("Number of the most contributive individuals"),
                                               min = 1, max = length(nomMFAshiny), value =as.numeric(selection2MFAshiny),step=1))}
              else{
                div(align="center",sliderInput("sliderind0", label = gettext("Number of the most contributive individuals"),
                                               min = 1, max = length(nomMFAshiny), value =length(nomMFAshiny),step=1)) 
              }),
            conditionalPanel(
              condition=paste("input.select=='",gettext("Manual"),"'",sep=''),
                selectInput("indiv",label=gettext("Select individuals:"),
                            choices=nomMFAshiny,multiple=TRUE,selected=selection2MFAshiny)
			  ),
			  uiOutput("drawindiv"),
          conditionalPanel(
            condition=paste("input.drawind=='",gettext("categorical variable"),"'",sep=''),
            uiOutput("habillagequali")
            ),
          radioButtons("choixpartial",gettext("Partial points to draw"),choices=list(gettext("None"),gettext("All"),gettext("Choose")),selected=partial,inline=TRUE),
          conditionalPanel(
            condition=paste("input.choixpartial=='",gettext("Choose"),"'",sep=''),
            uiOutput("indivpartiel2")),
          conditionalPanel(
            condition=paste("input.choixpartial!='",gettext("None"),"'",sep=''),
            checkboxInput("partind",gettext("Draw labels for the partial individuals"),partial3))
          ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Quantitative variables"),"'",sep=''),
          textInput("titleVar",gettext("Title of the graph: "), titleVar),
          sliderInput("cexVar",gettext("Size of labels"),min=0.5,max=2.5,value=sizeVar,step=0.05,ticks=FALSE),
          radioButtons("selection",gettext("Draw variables according to:"),choices=list(gettext("No selection"),"Contribution"="contrib","Cos2"="cos2"),selected=gettext("No selection")),
          uiOutput("slider1"),
          uiOutput("hide2"),
          checkboxInput("colorgroup",gettext("Color the variables by group"),colorvar) 
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Frequencies"),"'",sep=''),
          textInput("titleFreq",gettext("Title of the graph: "), titleFreq),
          sliderInput("cexFreq",gettext("Size of labels"),min=0.5,max=2.5,value=sizeFreq,step=0.05,ticks=FALSE),
          uiOutput("choixindvarfreq"),
          checkboxInput("affichind",gettext("Draw labels for the mean individuals"),freq1),
          checkboxInput("affichcol",gettext("Draw labels for the columns"),freq2)
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Partial axes"),"'",sep=''),
          textInput("titlePartial",gettext("Title of the graph: "), titlePartial),
          sliderInput("cexPartial",gettext("Size of labels"),min=0.5,max=2.5,value=sizePartial,step=0.05,ticks=FALSE)
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Groups"),"'",sep=''),
          textInput("titleGroup",gettext("Title of the graph: "), titleGroup),
          sliderInput("cexGroup",gettext("Size of labels"),min=0.5,max=2.5,value=sizeGroup,step=0.05,ticks=FALSE)
        )
       ), style = "padding: 3px;background-color: #fcefba"),
      wellPanel(
        div(align="center",checkboxInput("hcpcparam",gettext("Perform clustering after leaving MFA app?"),hcpcparaMFAshiny)),
        conditionalPanel(
          condition="input.hcpcparam==true",
          uiOutput("NbDimForClustering")
        ),
        align="center", style = "padding: 3px;background-color: #ecffdb"),
      div(align="center",actionButton("MFAcode", gettext("Get the MFA code"),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;')),
      div(align="center",actionButton("Quit", gettext("Quit the app"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
      
      mainPanel(
        tabsetPanel(id = "graph_sort",
                    tabPanel(gettext("Graphs"),
     fluidRow(
                 br(),
                          column(width = 6,shinyjqui::jqui_resizable(plotOutput("map", height="500")),
                             br(),
                             p(gettext("Download as"),downloadButton("downloadData1",gettext("jpg")),downloadButton("downloadData",gettext("png")),downloadButton("downloadData2",gettext("pdf")),align="center"),
							 align="center"),
                             div(align = "center",uiOutput("map22", height="500"))
							 ),
 fluidRow(
                             br(),
                 column(width = 6,shinyjqui::jqui_resizable(plotOutput("map5", height="500")),
                             br(),
                             p(gettext("Download as"),downloadButton("downloadData11",gettext("jpg")),downloadButton("downloadData12",gettext("png")),downloadButton("downloadData13",gettext("pdf")),align="center"),
							 align="center"),
                 column(width = 6,shinyjqui::jqui_resizable(plotOutput("map4", height="500")),
                             br(),
                             p(gettext("Download as"),downloadButton("downloadData15",gettext("jpg")),downloadButton("downloadData16",gettext("png")),downloadButton("downloadData17",gettext("pdf")),align="center"),
							 align="center")),
 fluidRow(
                             br(),
            div(align = "center",uiOutput("map66", height="500"))
							 )
							 ),

                    tabPanel(gettext("Values"),
                             br(),
                             radioButtons("out",gettext("Which outputs do you want?"),
                                          choices=list(gettext("Summary of outputs"),gettext("Eigenvalues"),gettext("Results for the individuals"),
                                                       gettext("Results for the quantitative variables"),gettext("Results for the groups"),gettext("Results for the partial axes")),inline=TRUE),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Summary of outputs"),"'",sep=''),
                               verbatimTextOutput("summaryMFA")
                               ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Eigenvalues"),"'",sep=''),
                               shinyjqui::jqui_resizable(plotOutput("map3", height="500")),
                               tableOutput("sorties")),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the individuals"),"'",sep=''),
                               radioButtons("out2",gettext("What type of results?"),choices=list(gettext("Coordinates"),gettext("Contributions"),gettext("Cos2"),gettext("Within inertia"),
                                                                                         gettext("Partial coordinates"),gettext("Within partial inertia")),selected=gettext("Coordinates"),inline=TRUE),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Coordinates"),"'",sep=''),
                                 div(align="center",tableOutput("sorties1"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Contributions"),"'",sep=''),
                                 div(align="center",tableOutput("sorties2"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Cos2"),"'",sep=''),
                                 div(align="center",tableOutput("sorties3"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Within inertia"),"'",sep=''),
                                 div(align="center",tableOutput("sorties4"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Partial coordinates"),"'",sep=''),
                                 div(align="center",tableOutput("sorties5"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Within partial inertia"),"'",sep=''),
                                 div(align="center",tableOutput("sorties6")))
                               ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the quantitative variables"),"'",sep=''),
                               radioButtons("out3","What type of results?",choices=list(gettext("Coordinates"),gettext("Contributions"),gettext("Cos2"),gettext("Correlations")),selected=gettext("Coordinates"),inline=TRUE),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Coordinates"),"'",sep=''),
                                 div(align="center",tableOutput("sorties11"))),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Contributions"),"'",sep=''),
                                 div(align="center",tableOutput("sorties22"))),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Cos2"),"'",sep=''),
                                 div(align="center",tableOutput("sorties33"))),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Correlations"),"'",sep=''),
                                 div(align="center",tableOutput("sorties44")))
                             ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the groups"),"'",sep=''),
                               div(align="center",tableOutput("sortiegroup"))
                               ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the partial axes"),"'",sep=''),
                               radioButtons("out4",gettext("What type of results?"),choices=list(gettext("Coordinates"),gettext("Correlations"),gettext("Contribution"),gettext("Correlations between")),selected=gettext("Coordinates"),inline=TRUE),
                               conditionalPanel(
                               condition=paste("input.out4=='",gettext("Coordinates"),"'",sep=''),
                                 div(align="center",tableOutput("sorties12"))),
                               conditionalPanel(
                               condition=paste("input.out4=='",gettext("Correlations"),"'",sep=''),
                                 div(align="center",tableOutput("sorties23"))),
                               conditionalPanel(
                               condition=paste("input.out4=='",gettext("Correlations between"),"'",sep=''),
                                 div(align="center",tableOutput("sorties45")))
                               )
                             
                             ),
                    tabPanel(gettext("Summary of dataset"),
                             br(),
                             verbatimTextOutput("summary")),
                    
                    tabPanel(gettext("Data"),
                             br(),
                             dataTableOutput("JDD")
                             )
        )
      ,width=9)
    )
)
