fluidPage(
  titlePanel(div(paste(gettext("PCA on the dataset"),nomDataPCAshiny),style="color:#6E6E6E",align="center"),windowTitle="PCAshiny"),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #EFFBFB; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
        tags$style(type='text/css', "#title2 { height: 25px; }")
      ),
      wellPanel(
        div(align="center",checkboxInput("pcaparam",gettext("PCA parameters"),FALSE)),
        conditionalPanel(
          condition="input.pcaparam==true",
          if(is.null(quantisupPCAshiny)){
            radioButtons("selecactive",label=h6(gettext("Choose the active variables")),
                         choices=list(gettext("All"),gettext("Choose")),selected=gettext("All"))
          }
          else{
            radioButtons("selecactive",label=h6(gettext("Choose the active variables")),
                         choices=list(gettext("All"),gettext("Choose")),selected=gettext("Choose"))
          },
          conditionalPanel(
            condition=paste("input.selecactive=='",gettext("Choose"),"'",sep=''),
            selectInput("supvar",label=gettext("Select the supplementary quantitative variables"),
                        choices=list(IdChoicesPCAshiny=VariableChoicesPCAshiny),
                        selected=quantisupPCAshiny,multiple=TRUE)
          ),
          br(),      
          h6(gettext("Select the supplementary categorical variables")),
          
          if(length(QualiChoicePCAshiny)>1){
            if(is.null(qualisupPCAshiny)){
              selectInput("supquali",label="",choices=list(IdqualisupPCAshiny=as.vector(QualiChoicePCAshiny)),multiple=TRUE)
            }
            else{
              selectInput("supquali",label="",choices=list(IdqualisupPCAshiny=as.vector(QualiChoicePCAshiny)),multiple=TRUE,selected=qualisupPCAshiny)  
            }
          }
          else if (length(QualiChoicePCAshiny)==1){
            if(is.null(qualisupPCAshiny)){
              checkboxInput("supquali",QualiChoicePCAshiny,FALSE)
            }
            else{
              checkboxInput("supquali",QualiChoicePCAshiny,TRUE)
            }
          }
          else if(length(QualiChoicePCAshiny)==0){
            p(gettext("No categorical variable in your dataset"))
          },
          
          br(),
          h6(gettext("Select the supplementary individuals")),
          if(is.null(indsuplPCAshiny)){
            selectInput("indsup","",choices=list(numPCAshiny=nomPCAshiny), multiple=TRUE)
          }
          else{
            selectInput("indsup","",choices=list(numPCAshiny=nomPCAshiny), multiple=TRUE,selected=indsuplPCAshiny)
          },
          checkboxInput("nor",gettext("Scale data to unit value"),normePCAshiny)
        ),
        style = "padding: 3px;"),
      wellPanel(
        div(align="center",checkboxInput("graph",gettext("Graphs options"),FALSE)),
        conditionalPanel(
          condition="input.graph==true",
          fluidRow(
            column(5,uiOutput("NB1")),
            column(5,uiOutput("NB2"))),
          hr(),
          div(align="center",radioButtons("ind_var","",
                                          choices=list(gettext("Graph of individuals"),gettext("Graph of variables")),selected=gettext("Graph of individuals"),inline=TRUE)),
          conditionalPanel(
            condition=paste0("input.ind_var=='",gettext("Graph of individuals"),"'"),
            textInput("title1",h6(gettext("Title of the graph: ")), titre1PCAshiny),
            uiOutput("choixindmod"),
            sliderInput("cex",h6(gettext("Size of labels")),min=0.5,max=2.5,value=sizePCAshiny,step=0.05,ticks=FALSE),
            selectInput("select",label=h6(gettext("Draw individuals according to:")),
                        choices=list(gettext("No selection"),"cos2"="cos2","Contribution"="contrib",gettext("Manual")),selected=selectionPCAshiny),
            conditionalPanel(
              condition="input.select=='cos2'",
              if(selectionPCAshiny=="cos2"){
                div(align="center",sliderInput("slider1", label = "cos2",
                                               min = 0, max = 1, value =as.numeric(selection2PCAshiny),step=0.05))}
              else{
                div(align="center",sliderInput("slider1", label = "cos2",
                                               min = 0, max = 1, value =0,step=0.05))
              }),
            conditionalPanel(
              condition="input.select=='contrib'",
              if(selectionPCAshiny=="contrib"){
                div(align="center",sliderInput("slider0", label = gettext("Number of the most contributive individuals"),
                                               min = 1, max = length(nomPCAshiny), value =as.numeric(selection2PCAshiny),step=1))}
              else{
                div(align="center",sliderInput("slider0", label = gettext("Number of the most contributive individuals"),
                                               min = 1, max = length(nomPCAshiny), value =length(nomPCAshiny),step=1)) 
              }),
            conditionalPanel(
              condition=paste("input.select=='",gettext("Manual"),"'",sep=''),
              if(selectionPCAshiny==gettext("Manual")){
                selectInput("indiv",label=gettext("Select individuals:"),
                            choices=list(numPCAshiny=nomPCAshiny),multiple=TRUE,selected=selection2PCAshiny)}
              else{
                selectInput("indiv",label=gettext("Select individuals:"),
                            choices=list(numPCAshiny=nomPCAshiny),multiple=TRUE)
              }),
            #colourInput("colour1","Colour of active points",value="blue"),
            colourpicker::colourInput("coloract", h6(gettext("Choose colour for active individuals")), activeindPCAshiny),
            uiOutput("colourn2"),
            uiOutput("colourn3"),
            if(is.null(habillageindPCAshiny)){
              checkboxInput("habi",gettext("Points colour depend on categorical variable"),FALSE)
            }
            else{
              checkboxInput("habi",gettext("Points colour depend on categorical variable"),TRUE)
            },
            conditionalPanel(
              condition="input.habi==true",
              uiOutput("habillage2"),
              uiOutput("ellipsesPCAshiny")
            )
          ),
          conditionalPanel(
            condition=paste("input.ind_var=='",gettext("Graph of variables"),"'",sep=''),
            textInput("title2",h6(gettext("Title of the graph:")), titre2PCAshiny),
            sliderInput("cex2",h6(gettext("Size of labels")),min=0.5,max=2.5,value=size2PCAshiny,step=0.05,ticks=FALSE),
            selectInput("select0",label=h6(gettext("Draw variables according to:")),
                        choices=list(gettext("No selection"),"cos2"="cos2","Contribution"="contrib"),selected=selection3PCAshiny),
            conditionalPanel(
              condition="input.select0=='contrib'",
              uiOutput("slider3")
            ),
            conditionalPanel(
              condition="input.select0=='cos2'",
              if(selection3PCAshiny=="cos2"){
                div(align="center",sliderInput("slider00", label = "cos2",
                                               min = 0, max = 1, value =as.numeric(selection4PCAshiny),step=0.05))  
              }
              else{
                div(align="center",sliderInput("slider00", label = "cos2",
                                               min = 0, max = 1, value =0,step=0.05))}
            ),
            colourpicker::colourInput("coloractvarPCAshiny", h6(gettext("Choose colour for active variables")), coloractvarPCAshiny),
            uiOutput("varsu")
          )
        ),
        style = "padding: 3px;"
      ),
      wellPanel(
        div(align="center",checkboxInput("hcpcparam",gettext("Perform clustering after leaving PCA app?"),hcpcparaPCAshiny)),
        conditionalPanel(
          condition="input.hcpcparam==true",
          uiOutput("NbDimForClustering")
        ),
        align="center", style = "padding: 3px;"
      ),
      wellPanel(
        div(align="center",checkboxInput("reportparam",gettext("Automatic report"),FALSE)),
        conditionalPanel(
          condition="input.reportparam==true",
		  div(gettext("File name (without extension):")),
          textInput("titleFile",NULL, gettext("Report"),width=200),
          radioButtons("choixLANG",gettext("Language"), choices=c(gettext("English"),gettext("French")), selected = gettext("English"), inline=TRUE),
          div(actionButton("InvestigateRmd", "Rmd"), actionButton("Investigatehtml", "html"), actionButton("Investigatedoc", "doc")),
          paste(gettext("The file will be saved in the directory"),pathsavePCAshiny)
        ),
        align="center", style = "padding: 3px;"
      ),
      div(align="center",actionButton("PCAcode", gettext("Get the PCA code"))),
      div(align="center",actionButton("Quit", gettext("Quit the app")))
      ,width=3),
    
    mainPanel(
      tabsetPanel(id = "graph_sort",
                  tabPanel(gettext("Graphs"),
                           fluidRow(
                             br(),
                             # column(width = 6,plotOutput("map2", width = "500", height="500"),
                             #                            br(),
                             #                            conditionalPanel(
                             #                              condition="input.paramdown=='jpg'",
                             #                              p(downloadButton("downloadData4",gettext("Download as jpg")),align="center")),
                             #                            conditionalPanel(
                             #                              condition="input.paramdown=='png'",
                             #                              p(downloadButton("downloadData3",gettext("Download as png")),align="center")),
                             #                            conditionalPanel(
                             #                              condition="input.paramdown=='pdf'",
                             #                              p(downloadButton("downloadData5",gettext("Download as pdf")),align="center")),
                             #                            br(),align="center"),
                             column(width = 6,plotOutput("map2", width = "500", height="500"),
                                    br(),
                                    p(gettext("Download as"),downloadButton("downloadData4",gettext("jpg")),downloadButton("downloadData3",gettext("png")),downloadButton("downloadData5",gettext("pdf")),align="center"),
                                    br(),align="center"),
                             column(width = 6,plotOutput("map", width = "500",height="500"),
                                    br(),
                                    p(gettext("Download as"),downloadButton("downloadData1",gettext("jpg")),downloadButton("downloadData",gettext("png")),downloadButton("downloadData2",gettext("pdf")),align="center"),
                                    br(),align="center"))),
                  # column(width = 6,plotOutput("map", width = "500",height="500"),
                  #                             br(),
                  #                             conditionalPanel(
                  #                               condition="input.paramdown=='jpg'",
                  #                               p(downloadButton("downloadData1",gettext("Download as jpg")),align="center")),
                  #                             conditionalPanel(
                  #                               condition="input.paramdown=='png'",
                  #                               p(downloadButton("downloadData",gettext("Download as png")),align="center")),
                  #                             conditionalPanel(
                  #                               condition="input.paramdown=='pdf'",
                  #                               p(downloadButton("downloadData2",gettext("Download as pdf")),align="center"))
                  #                             ,align="center"))),
                  
                  tabPanel(gettext("Values"),
                           br(),
                           uiOutput("out22"),
                           br(),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Eigenvalues"),"'",sep=''),
                             div(align="center",tableOutput("sorties")),
                             plotOutput("map3", width = "700", height="500")
                           ),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Results of the variables"),"'",sep=''),
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
                             numericInput("nbele",gettext("Number of elements to print"),value=10),
                             verbatimTextOutput("summaryPCA"),
                             p(downloadButton("summary2",gettext("Download the summary")),align="center")
                           ),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Results of the supplementary variables"),"'",sep=''),
                             h6(gettext("Coordinates")),
                             div(align="center",tableOutput("sorties23")),
                             h6("Correlations"),
                             div(align="center",tableOutput("sorties32"))
                           ),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Results of the categorical variables"),"'",sep=''),
                             h6(gettext("Coordinates")),
                             div(align="center",tableOutput("sorties12")),
                             h6("V-test"),
                             div(align="center",tableOutput("sorties13"))
                           ),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Results of the supplementary individuals"),"'",sep=''),
                             h6(gettext("Coordinates")),
                             div(align="center",tableOutput("sorties36")),
                             h6("Cos2"),
                             div(align="center",tableOutput("sorties37"))
                           )
                  ),
                  tabPanel(gettext("Automatic description of axes"),
                           br(),
                           radioButtons("Dim",label=gettext("Choose the dimensions"),choices=list("Dimension 1"="Dim1","Dimension 2"="Dim2","Dimension 3"="Dim3"),selected="Dim1"),
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
                  tabPanel(gettext("Summary of dataset"),
                           br(),
                           verbatimTextOutput("summary"),
                           br(),
                           selectInput("bam",h6(gettext("Graphs for")),choices=list(IdChoicesPCAshiny=VariableChoicesPCAshiny),multiple=FALSE),
                           plotOutput("histo")),
                  
                  tabPanel(gettext("Data"),
                           br(),
                           dataTableOutput("JDD")
                  )
      )
      ,width=9)
  )
)