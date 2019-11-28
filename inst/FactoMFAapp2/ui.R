# ui.R AFM2

fluidPage(
  titlePanel(div(paste(gettext("MFA on the dataset ",domain="R-Factoshiny"),nomDatacourt),style="color:#6E6E6E",align="center"),windowTitle="MFAshiny"),
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
        div(align="center",checkboxInput("graph",gettext("Show graphs options",domain="R-Factoshiny"),FALSE)),
        conditionalPanel(
          condition="input.graph==true",
          div(gettext("Axes:",domain="R-Factoshiny"), style="display: inline-block;padding: 5px"),
          div(uiOutput("NB1"), style="display: inline-block;"),
          div(uiOutput("NB2"), style="display: inline-block;"),
          uiOutput("choixgraphic"),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Individuals",domain="R-Factoshiny"),"'",sep=''),
          textInput("titleInd",gettext("Title of the graph: ",domain="R-Factoshiny"), titleInd),
          sliderInput("cexInd",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=sizeInd,step=0.05,ticks=FALSE),
            uiOutput("choixindvar"),
            selectInput("select",label=gettext("Labels for individuals selected by:",domain="R-Factoshiny"), choices=list(gettext("No selection",domain="R-Factoshiny"),"cos2"="cos2","Contribution"="contrib",gettext("Manual",domain="R-Factoshiny")),selected=selectionMFAshiny),
            conditionalPanel(
              condition="input.select=='cos2'",
              if(selectionMFAshiny=="cos2"){
                div(align="center",sliderInput("sliderind1", label = gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                                               min = 0, max = 1, value =as.numeric(selection2MFAshiny),step=0.05))}
              else{
                div(align="center",sliderInput("sliderind1", label = gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                                               min = 0, max = 1, value =0,step=0.05))
              }),
			  
            conditionalPanel(
              condition="input.select=='contrib'",
              if(selectionMFAshiny=="contrib"){
                div(align="center",sliderInput("sliderind0", label = gettext("Number of the most contributive individuals",domain="R-Factoshiny"),
                                               min = 1, max = length(nomMFAshiny), value =as.numeric(selection2MFAshiny),step=1))}
              else{
                div(align="center",sliderInput("sliderind0", label = gettext("Number of the most contributive individuals",domain="R-Factoshiny"),
                                               min = 1, max = length(nomMFAshiny), value =length(nomMFAshiny),step=1)) 
              }),
            conditionalPanel(
              condition=paste("input.select=='",gettext("Manual",domain="R-Factoshiny"),"'",sep=''),
                selectInput("indiv",label=gettext("Select individuals:",domain="R-Factoshiny"),
                            choices=nomMFAshiny,multiple=TRUE,selected=selection2MFAshiny)
			  ),
			  uiOutput("drawindiv"),
          conditionalPanel(
            condition=paste("input.drawind=='",gettext("categorical variable",domain="R-Factoshiny"),"'",sep=''),
            uiOutput("habillagequali")
            ),
          radioButtons("choixpartial",gettext("Partial points to draw",domain="R-Factoshiny"),choices=list(gettext("None",domain="R-Factoshiny"),gettext("All",domain="R-Factoshiny"),gettext("Choose",domain="R-Factoshiny")),selected=partial,inline=TRUE),
          conditionalPanel(
            condition=paste("input.choixpartial=='",gettext("Choose",domain="R-Factoshiny"),"'",sep=''),
            uiOutput("indivpartiel2")),
          conditionalPanel(
            condition=paste("input.choixpartial!='",gettext("None",domain="R-Factoshiny"),"'",sep=''),
            checkboxInput("partind",gettext("Draw labels for the partial individuals",domain="R-Factoshiny"),partial3))
          ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Quantitative variables",domain="R-Factoshiny"),"'",sep=''),
          textInput("titleVar",gettext("Title of the graph: ",domain="R-Factoshiny"), titleVar),
          sliderInput("cexVar",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=sizeVar,step=0.05,ticks=FALSE),
          radioButtons("selection",gettext("Draw variables according to:",domain="R-Factoshiny"),choices=list(gettext("No selection",domain="R-Factoshiny"),"Contribution"="contrib","Cos2"="cos2"),selected=gettext("No selection",domain="R-Factoshiny")),
          uiOutput("slider1"),
          uiOutput("hide2"),
          checkboxInput("colorgroup",gettext("Color the variables by group",domain="R-Factoshiny"),colorvar) 
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Frequencies",domain="R-Factoshiny"),"'",sep=''),
          textInput("titleFreq",gettext("Title of the graph: ",domain="R-Factoshiny"), titleFreq),
          sliderInput("cexFreq",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=sizeFreq,step=0.05,ticks=FALSE),
          uiOutput("choixindvarfreq"),
          checkboxInput("affichind",gettext("Draw labels for the mean individuals",domain="R-Factoshiny"),freq1),
          checkboxInput("affichcol",gettext("Draw labels for the columns",domain="R-Factoshiny"),freq2)
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Partial axes",domain="R-Factoshiny"),"'",sep=''),
          textInput("titlePartial",gettext("Title of the graph: ",domain="R-Factoshiny"), titlePartial),
          sliderInput("cexPartial",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=sizePartial,step=0.05,ticks=FALSE)
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Groups",domain="R-Factoshiny"),"'",sep=''),
          textInput("titleGroup",gettext("Title of the graph: ",domain="R-Factoshiny"), titleGroup),
          sliderInput("cexGroup",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=sizeGroup,step=0.05,ticks=FALSE)
        )
       ), style = "padding: 3px;background-color: #fcefba"),
      wellPanel(
        div(align="center",checkboxInput("hcpcparam",gettext("Perform clustering after leaving MFA app?",domain="R-Factoshiny"),hcpcparaMFAshiny)),
        conditionalPanel(
          condition="input.hcpcparam==true",
          uiOutput("NbDimForClustering")
        ),
        align="center", style = "padding: 3px;background-color: #ecffdb"),
      wellPanel(
        div(align="center",checkboxInput("MFAcode",gettext("Get the MFA code",domain="R-Factoshiny"),FALSE)),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;'
	  ),
      # div(align="center",actionButton("MFAcode", gettext("Get the MFA code",domain="R-Factoshiny"),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;')),
      div(align="center",actionButton("Quit", gettext("Quit the app",domain="R-Factoshiny"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
      
      mainPanel(
        tabsetPanel(id = "graph_sort",
                    tabPanel(gettext("Graphs",domain="R-Factoshiny"),
                             div(verbatimTextOutput("CodePrinted")),
     fluidRow(
                 br(),
                          column(width = 6,shinyjqui::jqui_resizable(plotOutput("map", height="500")),
                             br(),
                             p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData1",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData2",gettext("pdf",domain="R-Factoshiny")),align="center"),
							 align="center"),
                             div(align = "center",uiOutput("map22", height="500"))
							 ),
 fluidRow(
                             br(),
                 column(width = 6,shinyjqui::jqui_resizable(plotOutput("map5", height="500")),
                             br(),
                             p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData11",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData12",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData13",gettext("pdf",domain="R-Factoshiny")),align="center"),
							 align="center"),
                 column(width = 6,shinyjqui::jqui_resizable(plotOutput("map4", height="500")),
                             br(),
                             p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData15",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData16",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData17",gettext("pdf",domain="R-Factoshiny")),align="center"),
							 align="center")),
 fluidRow(
                             br(),
            div(align = "center",uiOutput("map66", height="500"))
							 )
							 ),

                    tabPanel(gettext("Values",domain="R-Factoshiny"),
                             div(verbatimTextOutput("CodePrintedSummary")),
                             br(),
                             radioButtons("out",gettext("Which outputs do you want?",domain="R-Factoshiny"),
                                          choices=list(gettext("Summary of outputs",domain="R-Factoshiny"),gettext("Eigenvalues",domain="R-Factoshiny"),gettext("Results for the individuals",domain="R-Factoshiny"),
                                                       gettext("Results for the quantitative variables",domain="R-Factoshiny"),gettext("Results for the groups",domain="R-Factoshiny"),gettext("Results for the partial axes",domain="R-Factoshiny")),inline=TRUE),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Summary of outputs",domain="R-Factoshiny"),"'",sep=''),
                               numericInput("nbele",gettext("Number of elements to print",domain="R-Factoshiny"),value=10),
                               verbatimTextOutput("summaryMFA")
                               ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Eigenvalues",domain="R-Factoshiny"),"'",sep=''),
                               shinyjqui::jqui_resizable(plotOutput("map3", height="500")),
                               tableOutput("sorties")),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the individuals",domain="R-Factoshiny"),"'",sep=''),
                               radioButtons("out2",gettext("What type of results?",domain="R-Factoshiny"),choices=list(gettext("Coordinates",domain="R-Factoshiny"),gettext("Contributions",domain="R-Factoshiny"),gettext("Cos2",domain="R-Factoshiny"),gettext("Within inertia",domain="R-Factoshiny"),
                                                                                         gettext("Partial coordinates",domain="R-Factoshiny"),gettext("Within partial inertia",domain="R-Factoshiny")),selected=gettext("Coordinates",domain="R-Factoshiny"),inline=TRUE),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Coordinates",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties1"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Contributions",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties2"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Cos2",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties3"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Within inertia",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties4"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Partial coordinates",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties5"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Within partial inertia",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties6")))
                               ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the quantitative variables",domain="R-Factoshiny"),"'",sep=''),
                               radioButtons("out3","What type of results?",choices=list(gettext("Coordinates",domain="R-Factoshiny"),gettext("Contributions",domain="R-Factoshiny"),gettext("Cos2",domain="R-Factoshiny"),gettext("Correlations",domain="R-Factoshiny")),selected=gettext("Coordinates",domain="R-Factoshiny"),inline=TRUE),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Coordinates",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties11"))),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Contributions",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties22"))),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Cos2"),"'",sep=''),
                                 div(align="center",tableOutput("sorties33"))),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Correlations",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties44")))
                             ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the groups",domain="R-Factoshiny"),"'",sep=''),
                               div(align="center",tableOutput("sortiegroup"))
                               ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the partial axes",domain="R-Factoshiny"),"'",sep=''),
                               radioButtons("out4",gettext("What type of results?",domain="R-Factoshiny"),choices=list(gettext("Coordinates",domain="R-Factoshiny"),gettext("Correlations",domain="R-Factoshiny"),gettext("Contribution",domain="R-Factoshiny"),gettext("Correlations between",domain="R-Factoshiny")),selected=gettext("Coordinates",domain="R-Factoshiny"),inline=TRUE),
                               conditionalPanel(
                               condition=paste("input.out4=='",gettext("Coordinates",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties12"))),
                               conditionalPanel(
                               condition=paste("input.out4=='",gettext("Correlations",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties23"))),
                               conditionalPanel(
                               condition=paste("input.out4=='",gettext("Correlations between",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties45")))
                               )
                             
                             ),
                    tabPanel(gettext("Summary of dataset",domain="R-Factoshiny"),
                             br(),
                             verbatimTextOutput("summary")),
                    
                    tabPanel(gettext("Data",domain="R-Factoshiny"),
                             br(),
                             DT::dataTableOutput("JDD")
                             )
        )
      ,width=9)
    )
)
