# ui script for FAMD2
shinyUI(fluidPage(
  titlePanel(div(paste(gettext("FAMD on the dataset ",nomData)),style="color:#6E6E6E",align="center"),windowTitle="FAMDshiny"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #ffd77a; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
        tags$style(type='text/css', "#title2 { height: 25px; }"),
        tags$style(type='text/css', "#title3 { height: 25px; }")
      ),
      wellPanel(
      div(align="center",checkboxInput("pcaparam",gettext("FAMD parameters"),FALSE)),
      conditionalPanel(
        condition="input.pcaparam==true",
        if(is.null(quantisup) && is.null(qualisup)){
        radioButtons("selecactive",label=h6(gettext("Choose the active variables")),
                          choices=list(gettext("All"),gettext("Choose")),selected=gettext("All"))
        }
        else{
          radioButtons("selecactive",label=h6(gettext("Choose the active variables")),
                       choices=list(gettext("All"),gettext("Choose")),selected=gettext("Choose"))
        },
        conditionalPanel(
#          condition="input.selecactive=='choix'",
          condition=paste("input.selecactive=='",gettext("Choose"),"'",sep=''),
          h6(gettext("Select the supplementary quantitative variables")),
          if(length(VariableChoices)>1){
            selectInput("supvar",label="",
                    choices=list(IdChoices=VariableChoices),
                    selected=quantisup,multiple=TRUE)}
          else{
            selectInput("supvar",label="",
                        choices=VariableChoices,
                        selected=quantisup,multiple=TRUE)
          },
          h6(gettext("Select the supplementary qualitative variables")),
          if(length(QualiChoice)>1){
          selectInput("supvar1",label="",
                      choices=list(Idqualisup=QualiChoice),
                      selected=qualisup,
                      multiple=TRUE)}
          else{
          selectInput("supvar1",label="",
                        choices=quali,selected=qualisup,
                        multiple=TRUE)  
          }
          ),
        br(),
        h6(gettext("Select the supplementary individuals")),
        if(is.null(indsupl)){
          selectInput("indsup","",choices=list(num=nom), multiple=TRUE)
        }
        else{
          selectInput("indsup","",choices=list(num=nom), multiple=TRUE,selected=indsupl)
        }
      ),
      style = "padding: 3px;background-color: #ffdbdb;"),
      wellPanel(
      div(align="center",checkboxInput("graph",gettext("Graphical options"),FALSE)),
      conditionalPanel(
        condition="input.graph==true",
        fluidRow(
          column(5,uiOutput("NB1")),
          column(5,uiOutput("NB2"))),
        hr(),
        div(align="center",selectInput("choixgraph",h6(gettext("Which graph would you like to modify?")), choices=list(gettext("Individuals and categories"),"Variables"="var",gettext("Quantitative variables")),selected=gettext("Individuals and categories"))),
        br(),
        conditionalPanel(
#          condition="input.choixgraph=='ind'",
          condition=paste("input.choixgraph=='",gettext("Individuals and categories"),"'",sep=''),
          textInput("title1",h6(gettext("Title of the graph: ")), title1),
          sliderInput("cex",h6(gettext("Size of labels")),min=0.5,max=2.5,value=size,step=0.05,ticks=FALSE),
          br(),
          checkboxInput("labels2",gettext("Draw labels of individuals"),labind),
          checkboxInput("labels",gettext("Draw labels of categories"),labvar),
          selectInput("select",label=h6(gettext("Draw individuals according to:")),
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
            selectInput("indiv",label=gettext("Select individuals"),
                        choices=list(num=nom),multiple=TRUE,selected=selection2)}
            else{
              selectInput("indiv",label=gettext("Select individuals"),
                          choices=list(num=nom),multiple=TRUE)
            }),
            if(is.null(habillageind)){
              checkboxInput("habi",gettext("Points colour depend on categorical variable"),FALSE)
            }
            else{
              checkboxInput("habi",gettext("Points colour depend on categorical variable"),TRUE)
            },
            conditionalPanel(
              condition="input.habi==true",
              uiOutput("habillage2")
            )
        ),
        conditionalPanel(
          condition="input.choixgraph=='var'",
          textInput("title2",h6(gettext("Title of the graph: ")), title2),
          sliderInput("cex2",h6(gettext("Size of labels")),min=0.5,max=2.5,value=size2,step=0.05,ticks=FALSE),
          br(),
          selectInput("select0",label=h6(gettext("Draw variables according to:")),
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
          condition=paste("input.choixgraph=='",gettext("Quantitative variables"),"'",sep=''),
#          condition="input.choixgraph=='quant'",
          textInput("title3",h6(gettext("Title of the graph: ")), title3),
          sliderInput("cex3",h6(gettext("Size of labels")),min=0.5,max=2.5,value=size3,step=0.05,ticks=FALSE),
          br(),
          selectInput("selecti",label=h6(gettext("Draw variables according to:")),
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
        div(align="center",checkboxInput("hcpcparam",gettext("Perform clustering after leaving FAMD app?"),hcpcparaFAMDshiny)),
        conditionalPanel(
          condition="input.hcpcparam==true",
          uiOutput("NbDimForClustering")
        ),
        align="center", style = "padding: 3px;background-color: #ecffdb"
      ),
      div(align="center",actionButton("FAMDcode", gettext("Get the FAMD code"),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;')),
      div(align="center",actionButton("Quit", gettext("Quit the app"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
      
      mainPanel(
        tabsetPanel(id = "graph_sort",
                    tabPanel(gettext("Graphs"),
 fluidRow(
                           br(),
                 column(width = 6,plotOutput("map2", width = "500", height="500"),
###                             div(align = "center",plotOutput("map2", width = 500, height=500)),
                             br(),
                             p(gettext("Download as"),downloadButton("downloadData4",gettext("jpg")),downloadButton("downloadData3",gettext("png")),downloadButton("downloadData5",gettext("pdf")),align="center"),
                             # conditionalPanel(
                               # condition="input.paramdown=='jpg'",
                               # p(downloadButton("downloadData4",gettext("Download as jpg")),align="center")),
                             # conditionalPanel(
                               # condition="input.paramdown=='png'",
                               # p(downloadButton("downloadData3",gettext("Download as png")),align="center")),
                             # conditionalPanel(
                               # condition="input.paramdown=='pdf'",
                               # p(downloadButton("downloadData5",gettext("Download as pdf")),align="center")),
                             br(),
							 align="center"),
                 column(width = 6,plotOutput("map", width = "500", height="500"),
###                             div(align="center",plotOutput("map", width = 500, height=500)),
                             br(),
                                    p(gettext("Download as"),downloadButton("downloadData1",gettext("jpg")),downloadButton("downloadData",gettext("png")),downloadButton("downloadData2",gettext("pdf")),align="center"),
                             # conditionalPanel(
                               # condition="input.paramdown=='jpg'",
                               # p(downloadButton("downloadData1",gettext("Download as jpg")),align="center")),
                             # conditionalPanel(
                               # condition="input.paramdown=='png'",
                               # p(downloadButton("downloadData",gettext("Download as png")),align="center")),
                             # conditionalPanel(
                               # condition="input.paramdown=='pdf'",
                               # p(downloadButton("downloadData2",gettext("Download as pdf")),align="center")),
                             br(),
							 align="center")),
 fluidRow(
                           br(),
                 column(width = 6,plotOutput("map4", width = "500", height="500"),
#                             div(align="center",plotOutput("map4", width = 500, height=500)),
                             br(),
                             p(gettext("Download as"),downloadButton("downloadData6",gettext("jpg")),downloadButton("downloadData7",gettext("png")),downloadButton("downloadData8",gettext("pdf")),align="center"),
                             # conditionalPanel(
                               # condition="input.paramdown=='jpg'",
                               # p(downloadButton("downloadData6",gettext("Download as jpg")),align="center")),
                             # conditionalPanel(
                               # condition="input.paramdown=='png'",
                               # p(downloadButton("downloadData7",gettext("Download as png")),align="center")),
                             # conditionalPanel(
                               # condition="input.paramdown=='pdf'",
                               # p(downloadButton("downloadData8",gettext("Download as pdf")),align="center")),
                             align="center"))),

                    tabPanel(gettext("Values"),
                             br(),
                             uiOutput("out22", width = "500", height="500"),
                             br(),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Eigenvalues"),"'",sep=''),
#                               condition="input.out=='eig'",
                               div(align="center",tableOutput("sorties")),
                               plotOutput("map3", width = "700", height="500")),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results of the variables"),"'",sep=''),
#                               condition="input.out=='resvar'",
                               h6(gettext("Coordinates")),
                               div(align="center",tableOutput("sorties2")),
                               br(),
                               h6("Contributions"),
                               div(align="center",tableOutput("sorties3")),
                               br(),
                               h6("Cos2"),
                               div(align="center",tableOutput("sorties4"))),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results of the individuals"),"'",sep=''),
#                               condition="input.out=='resind'",
                               h6(gettext("Coordinates")),
                               div(align="center",tableOutput("sorties22")),
                               br(),
                               h6("Contributions"),
                               div(align="center",tableOutput("sorties33")),
                               br(),
                               h6("Cos2"),
                               div(align="center",tableOutput("sorties44"))),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Summary of outputs"),"'",sep=''),
#                               condition="input.out=='ACP'",
                               numericInput("nbele",gettext("Number of elements to print"),value=10),
                               verbatimTextOutput("summaryFAMD"),
                               p(downloadButton("summary2",gettext("Download the summary")),align="center")),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results of the supplementary variables"),"'",sep=''),
#                               condition="input.out=='varsup'",
                               h6(gettext("Coordinates")),
                               div(align="center",tableOutput("sorties23")),
                               h6("Cos2"),
                               div(align="center",tableOutput("sorties32"))),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results of the supplementary individuals"),"'",sep=''),
#                               condition="input.out=='supind'",
                               h6(gettext("Coordinates")),
                               div(align="center",tableOutput("sorties36")),
                               h6("Cos2"),
                               div(align="center",tableOutput("sorties37")))
                             ),
                    tabPanel(gettext("Summary of dataset"),
                             br(),
                             verbatimTextOutput("summary"),
                             br(),
                             selectInput("bam",h6(gettext("Graphs for")),choices=list(Idall=all),multiple=FALSE),
                             plotOutput("histo", width = "1000", height="500")),
                    
                    tabPanel(gettext("Data"),
                             br(),
                             dataTableOutput("JDD")
                             )
        )
      ,width=9)
    )
))
