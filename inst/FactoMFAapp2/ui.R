# ui.R AFM2

fluidPage(
  titlePanel(div(paste(gettext("MFA on the dataset "),nameJDD),style="color:#6E6E6E",align="center"),windowTitle="MFAshiny"),
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
        div(align="center",selectInput("choixgraph",h6(gettext("Which graph would you like to modify?")), choices=list(gettext("Groups"),gettext("Individuals"),gettext("Quantitative variables"),gettext("Frequencies"),gettext("Partial axes")),selected=gettext("Groups"))),
        hr(),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Individuals"),"'",sep=''),
#          condition="input.choixgraph=='ind'",
          textInput("title2",h6(gettext("Title of the graph: ")), title2),
          checkboxInput("meanind1",gettext("Plot points for the mean individuals"),ind1),
          checkboxInput("meanind",gettext("Draw labels for the mean individuals"),ind2),
          checkboxInput("qualind1",gettext("Plot points for the categories"),ind3),
          checkboxInput("qualind",gettext("Draw labels for the categories"),ind4),
          hr(),
          uiOutput("drawindiv"),
          conditionalPanel(
            condition=paste("input.drawind=='",gettext("categorical variable"),"'",sep=''),
#            condition="input.drawind=='c'",
            uiOutput("habillagequali")
            ),
          hr(),
          radioButtons("choixpartial",h6(gettext("Partial points to draw")),choices=list(gettext("None"),gettext("All"),gettext("Choose")),selected=partial,inline=TRUE),
          conditionalPanel(
#            condition="input.choixpartial==3",
            condition=paste("input.choixpartial=='",gettext("Choose"),"'",sep=''),
            uiOutput("indivpartiel2")),
          hr(),
          conditionalPanel(
#            condition="input.choixpartial!=1",
            condition=paste("input.choixpartial!='",gettext("None"),"'",sep=''),
            checkboxInput("partind",gettext("Draw labels for the partial individuals"),partial3))
          ),
        conditionalPanel(
          textInput("title3",h6(gettext("Title of the graph: ")), title3),
          condition=paste("input.choixgraph=='",gettext("Quantitative variables"),"'",sep=''),
#          condition="input.choixgraph=='quant'",
          radioButtons("selection",h6(gettext("Draw variables according to:")),choices=list(gettext("No selection"),"Contribution"="contrib","Cos2"="cos2"),selected=gettext("No selection")),
          uiOutput("slider1"),
          hr(),
          uiOutput("hide2"),
          checkboxInput("colorgroup",gettext("Color the variables by group"),colorvar) 
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Frequencies"),"'",sep=''),
#          condition="input.choixgraph=='freq'",
          textInput("title5",h6(gettext("Title of the graph: ")), title5),
          checkboxInput("affichind",gettext("Draw labels for the mean individuals"),freq1),
          checkboxInput("affichcol",gettext("Draw labels for the columns"),freq2)
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Partial axes"),"'",sep=''),
#          condition="input.choixgraph=='axes'",
          textInput("title4",h6(gettext("Title of the graph: ")), title4),
          checkboxInput("coloraxe",gettext("Color the partial axe by group"),partaxe)
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Groups"),"'",sep=''),
          textInput("title1",h6(gettext("Title of the graph: ")), title1)
        ),
        fluidRow(
          column(5,selectInput("nb1", label = h6(gettext("x axis")), 
                               choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = axe1,width='80%')),
          column(5,selectInput("nb2", label =h6(gettext("y axis")), 
                               choices = list("1" = 1, "2" = 2,"3" = 3,"4"= 4,"5" =5), selected = axe2,width='80%')))
        ),
		style = "padding: 3px;background-color: #fcefba"),
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
                 column(width = 6,plotOutput("map5", width = "500", height="500"),
                             br(),
                             p(gettext("Download as"),downloadButton("downloadData11",gettext("jpg")),downloadButton("downloadData12",gettext("png")),downloadButton("downloadData13",gettext("pdf")),align="center"),
							 align="center"),
                 column(width = 6,plotOutput("map", width = "500", height="500"),
                             br(),
                             p(gettext("Download as"),downloadButton("downloadData1",gettext("jpg")),downloadButton("downloadData",gettext("png")),downloadButton("downloadData2",gettext("pdf")),align="center"),
							 align="center")),
 fluidRow(
                             br(),
                 column(width = 6,uiOutput("map22", width = "500", height="500"),
                             br(),
                             p(gettext("Download as"),downloadButton("downloadData4",gettext("jpg")),downloadButton("downloadData3",gettext("png")),downloadButton("downloadData5",gettext("pdf")),align="center"),
							 align="center"),
                 column(width = 6,plotOutput("map4", width = "500", height="500"),
                             br(),
                             p(gettext("Download as"),downloadButton("downloadData15",gettext("jpg")),downloadButton("downloadData16",gettext("png")),downloadButton("downloadData17",gettext("pdf")),align="center"),
							 align="center")),
 fluidRow(
                             br(),
                 column(width = 6,plotOutput("map66", width = "500", height="500"),
                             br(),
                             p(gettext("Download as"),downloadButton("downloadData19",gettext("jpg")),downloadButton("downloadData20",gettext("png")),downloadButton("downloadData21",gettext("pdf")),align="center"),
                             align="center"))),

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
                               tableOutput("sorties"),
                               plotOutput("map3", width = "700", height="500")),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the individuals"),"'",sep=''),
#                               condition="input.out=='ind'",
                               radioButtons("out2",gettext("What type of results?"),choices=list(gettext("Coordinates"),gettext("Contributions"),gettext("Cos2"),gettext("Within inertia"),
                                                                                         gettext("Partial coordinates"),gettext("Within partial inertia")),selected=gettext("Coordinates"),inline=TRUE),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Coordinates"),"'",sep=''),
#                                 condition="input.out2=='coord'",
                                 div(align="center",tableOutput("sorties1"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Contributions"),"'",sep=''),
#                                 condition="input.out2=='contrib'",
                                 div(align="center",tableOutput("sorties2"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Cos2"),"'",sep=''),
#                                 condition="input.out2=='cos2'",
                                 div(align="center",tableOutput("sorties3"))),
                               conditionalPanel(
#                                 condition="input.out2=='witi'",
                               condition=paste("input.out2=='",gettext("Within inertia"),"'",sep=''),
                                 div(align="center",tableOutput("sorties4"))),
                               conditionalPanel(
#                                 condition="input.out2=='partco'",
                               condition=paste("input.out2=='",gettext("Partial coordinates"),"'",sep=''),
                                 div(align="center",tableOutput("sorties5"))),
                               conditionalPanel(
                               condition=paste("input.out2=='",gettext("Within partial inertia"),"'",sep=''),
#                                 condition="input.out2=='wpi'",
                                 div(align="center",tableOutput("sorties6")))
                               ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the quantitative variables"),"'",sep=''),
#                               condition="input.out=='quantvar'",
                               radioButtons("out3","What type of results?",choices=list(gettext("Coordinates"),gettext("Contributions"),gettext("Cos2"),gettext("Correlations")),selected=gettext("Coordinates"),inline=TRUE),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Coordinates"),"'",sep=''),
#                                 condition="input.out3=='coord'",
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
#                               condition="input.out=='group'",
                               div(align="center",tableOutput("sortiegroup"))
                               ),
                             conditionalPanel(
#                               condition="input.out=='partaxe'",
                               condition=paste("input.out=='",gettext("Results for the partial axes"),"'",sep=''),
                               radioButtons("out4",gettext("What type of results?"),choices=list(gettext("Coordinates"),gettext("Correlations"),gettext("Contribution"),gettext("Correlations between")),selected=gettext("Coordinates"),inline=TRUE),
                               conditionalPanel(
                               condition=paste("input.out4=='",gettext("Coordinates"),"'",sep=''),
#                                 condition="input.out4=='coord'",
                                 div(align="center",tableOutput("sorties12"))),
                               conditionalPanel(
                               condition=paste("input.out4=='",gettext("Correlations"),"'",sep=''),
                                 div(align="center",tableOutput("sorties23"))),
                               # conditionalPanel(
                               # condition=paste("input.out4=='",gettext("Contributions"),"'",sep=''),
                                 # div(align="center",tableOutput("sorties34"))),
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
