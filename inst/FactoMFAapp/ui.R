# ui2.R AFM

fluidPage(
  titlePanel(div(paste(gettext("MFA on the dataset "),nomData),style="color:#6E6E6E",align="center"),windowTitle="MFAshiny"),
  
  sidebarLayout(
      sidebarPanel(
        tags$head(
          tags$style("body {background-color: #E1D3FB; }"),
          tags$style(type='text/css', "#nameG1 { max-width: 100px; }"),
          tags$style(type='text/css', "#listvarG1 { max-width: 170px; }"),
          tags$style(type='text/css', "#nameG2 { max-width: 100px; }"),
          tags$style(type='text/css', "#listvarG2 { max-width: 170px; }"),
          tags$style(type='text/css', "#nameG3 { max-width: 100px; }"),
          tags$style(type='text/css', "#listvarG3 { max-width: 170px; }"),
          tags$style(type='text/css', "#nameG4 { max-width: 100px; }"),
          tags$style(type='text/css', "#listvarG4 { max-width: 170px; }"),
          tags$style(type='text/css', "#nameG5 { max-width: 100px; }"),
          tags$style(type='text/css', "#listvarG5 { max-width: 170px; }"),
          tags$style(type='text/css', "#nameG6 { max-width: 100px; }"),
          tags$style(type='text/css', "#listvarG6 { max-width: 170px; }"),
          tags$style(type='text/css', "#nameG7 { max-width: 100px; }"),
          tags$style(type='text/css', "#listvarG7 { max-width: 170px; }"),
          tags$style(type='text/css', "#nameG8 { max-width: 100px; }"),
          tags$style(type='text/css', "#listvarG8 { max-width: 170px; }"),
          tags$style(type='text/css', "#nameG9 { max-width: 100px; }"),
          tags$style(type='text/css', "#listvarG9 { max-width: 170px; }"),
          tags$style(type='text/css', "#nameG10 { max-width: 100px; }"),
          tags$style(type='text/css', "#nb1 { height:50px; }"),
          tags$style(type='text/css', "#nb2 { height:50px; }"),
          tags$style(type='text/css', "#listvarG10 { max-width: 170px; }"),
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
          checkboxInput("affichind",gettext("Draw labels for the mean individuals"),freq1),
          checkboxInput("affichcol",gettext("Draw labels for the columns"),freq2)
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Partial axes"),"'",sep=''),
          textInput("titlePartial",gettext("Title of the graph: "), titlePartial),
          sliderInput("cexPartial",gettext("Size of labels"),min=0.5,max=2.5,value=sizePartial,step=0.05,ticks=FALSE)
          # checkboxInput("coloraxe",gettext("Color the partial axe by group"),partaxe)
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
                    tabPanel(gettext("Creation of groups"),
                             br(),
                             checkboxInput("activemodif",gettext("Create the groups"),FALSE),
                             conditionalPanel(
                               condition="input.activemodif==true",
                               h6("Group 1"),
                              fluidRow(
                              column(3,
                                     radioButtons("typeG1"," ",choices=list(gettext("Quantitative"),gettext("Qualitative"),gettext("Frequencies")),selected=gettext("Quantitative"))
                              ),
                              column(3,
                                     uiOutput("listvarG1")),
                              column(3,
                                     textInput("nameG1", label = " ", value = "Gr 1"),
                                     conditionalPanel(
#                                       condition="input.typeG1=='quant'",
                                       condition=paste("input.typeG1=='",gettext("Quantitative"),"'",sep=''),
                                       radioButtons("scale1","",choices=list(gettext("Scaled"),gettext("Unscaled")),selected=gettext("Scaled"),inline=TRUE))),
                              column(2,
                                     radioButtons("typeG12","",choices=list(gettext("Active"),gettext("Supplementary")),selected=gettext("Active")))),
                              br(),
                              checkboxInput("activeG2",h6(paste(gettext("Create Group"),2)),FALSE),
                              conditionalPanel(
                                condition="input.activeG2==true",
                                fluidRow(
                                column(3,
                                       radioButtons("typeG2"," ",choices=list(gettext("Quantitative"),gettext("Qualitative"),gettext("Frequencies")),selected=gettext("Quantitative"))
                                ),
                                column(3,
                                       uiOutput("listvarG2")),
                                column(3,
                                       textInput("nameG2", label = " ", value = "Gr 2"),
                                       conditionalPanel(
#                                         condition="input.typeG2=='quant'",
                                       condition=paste("input.typeG2=='",gettext("Quantitative"),"'",sep=''),
                                         radioButtons("scale2","",choices=list(gettext("Scaled"),gettext("Unscaled")),selected=gettext("Scaled"),inline=TRUE))),
                                column(2,
                                       radioButtons("typeG22","",choices=list(gettext("Active"),gettext("Supplementary")),selected=gettext("Active")))),
                                br(),
                                checkboxInput("activeG3",h6(paste(gettext("Create Group"),3)),FALSE),
                                conditionalPanel(
                                  condition="input.activeG3==true",
                                  fluidRow(
                                  column(3,
                                         radioButtons("typeG3"," ",choices=list(gettext("Quantitative"),gettext("Qualitative"),gettext("Frequencies")),selected=gettext("Quantitative"))
                                  ),
                                  column(3,
                                         uiOutput("listvarG3")),
                                  column(3,
                                         textInput("nameG3", label = " ", value = "Gr 3"),
                                         conditionalPanel(
                                            condition=paste("input.typeG3=='",gettext("Quantitative"),"'",sep=''),
#                                           condition="input.typeG3=='quant'",
                                           radioButtons("scale3","",choices=list(gettext("Scaled"),gettext("Unscaled")),selected=gettext("Scaled"),inline=TRUE))),
                                  column(3,
                                         radioButtons("typeG32","",choices=list(gettext("Active"),gettext("Supplementary")),selected=gettext("Active")))),
                                  br(),
                                  checkboxInput("activeG4",h6(paste(gettext("Create Group"),4)),FALSE),
                                  conditionalPanel(
                                    condition="input.activeG4==true",
                                    fluidRow(
                                    column(3,
                                           radioButtons("typeG4"," ",choices=list(gettext("Quantitative"),gettext("Qualitative"),gettext("Frequencies")),selected=gettext("Quantitative"))
                                    ),
                                    column(3,
                                           uiOutput("listvarG4")),
                                    column(3,
                                           textInput("nameG4", label = " ", value = "Gr 4"),
                                           conditionalPanel(
                                             condition=paste("input.typeG4=='",gettext("Quantitative"),"'",sep=''),
#                                             condition="input.typeG4=='quant'",
                                             br(),
                                             radioButtons("scale4","",choices=list(gettext("Scaled"),gettext("Unscaled")),selected=gettext("Scaled"),inline=TRUE))),
                                    column(3,
                                           radioButtons("typeG42","",choices=list(gettext("Active"),gettext("Supplementary")),selected=gettext("Active")))),
                                    br(),
                                    checkboxInput("activeG5",h6(paste(gettext("Create Group"),5)),FALSE),
                                    conditionalPanel(
                                      condition="input.activeG5==true",
                                      fluidRow(
                                      column(3,
                                             radioButtons("typeG5"," ",choices=list(gettext("Quantitative"),gettext("Qualitative"),gettext("Frequencies")),selected=gettext("Quantitative"))
                                      ),
                                      column(3,
                                             uiOutput("listvarG5")),
                                      column(3,
                                             textInput("nameG5", label = " ", value = "Gr 5"),
                                             conditionalPanel(
                                               condition=paste("input.typeG5=='",gettext("Quantitative"),"'",sep=''),
#                                               condition="input.typeG5=='quant'",
                                               br(),
                                               radioButtons("scale5","",choices=list(gettext("Scaled"),gettext("Unscaled")),selected=gettext("Scaled"),inline=TRUE))),
                                      column(3,
                                             radioButtons("typeG52","",choices=list(gettext("Active"),gettext("Supplementary")),selected=gettext("Active")))),
                                      br(),
                                      checkboxInput("activeG6",h6(paste(gettext("Create Group"),6)),FALSE),
                                      conditionalPanel(
                                        condition="input.activeG6==true",
                                        fluidRow(
                                        column(3,
                                               radioButtons("typeG6"," ",choices=list(gettext("Quantitative"),gettext("Qualitative"),gettext("Frequencies")),selected=gettext("Quantitative"))
                                        ),
                                        column(3,
                                               uiOutput("listvarG6")),
                                        column(3,
                                               textInput("nameG6", label = " ", value = "Gr 6"),
                                               conditionalPanel(
                                             condition=paste("input.typeG6=='",gettext("Quantitative"),"'",sep=''),
#                                                 condition="input.typeG6=='quant'",
                                                 br(),
                                                 radioButtons("scale6","",choices=list(gettext("Scaled"),gettext("Unscaled")),selected=gettext("Scaled"),inline=TRUE))),
                                        column(3,
                                               radioButtons("typeG62","",choices=list(gettext("Active"),gettext("Supplementary")),selected=gettext("Active")))),
                                        br(),
                                        checkboxInput("activeG7",h6(paste(gettext("Create Group"),7)),FALSE),
                                        conditionalPanel(
                                          condition="input.activeG7==true",
                                          fluidRow(
                                          column(3,
                                                 radioButtons("typeG7"," ",choices=list(gettext("Quantitative"),gettext("Qualitative"),gettext("Frequencies")),selected=gettext("Quantitative"))
                                          ),
                                          column(3,
                                                 uiOutput("listvarG7")),
                                          column(3,
                                                 textInput("nameG7", label = " ", value = "Gr 7"),
                                                 conditionalPanel(
                                                   condition=paste("input.typeG7=='",gettext("Quantitative"),"'",sep=''),
#                                                   condition="input.typeG7=='quant'",
                                                   br(),
                                                   radioButtons("scale7","",choices=list(gettext("Scaled"),gettext("Unscaled")),selected=gettext("Scaled"),inline=TRUE))),
                                          column(3,
                                                 radioButtons("typeG72","",choices=list(gettext("Active"),gettext("Supplementary")),selected=gettext("Active")))),
                                          br(),
                                          checkboxInput("activeG8",h6(paste(gettext("Create Group"),8)),FALSE),
                                          conditionalPanel(
                                            condition="input.activeG8==true",
                                            fluidRow(
                                            column(3,
                                                   radioButtons("typeG8"," ",choices=list(gettext("Quantitative"),gettext("Qualitative"),gettext("Frequencies")),selected=gettext("Quantitative"))
                                            ),
                                            column(3,
                                                   uiOutput("listvarG8")),
                                            column(3,
                                                   textInput("nameG8", label = " ", value = "Gr 8"),
                                                   conditionalPanel(
                                             condition=paste("input.typeG8=='",gettext("Quantitative"),"'",sep=''),
#                                                     condition="input.typeG8=='quant'",
                                                     br(),
                                                     radioButtons("scale8","",choices=list(gettext("Scaled"),gettext("Unscaled")),selected=gettext("Scaled"),inline=TRUE))),
                                            column(3,
                                                   radioButtons("typeG82","",choices=list(gettext("Active"),gettext("Supplementary")),selected=gettext("Active")))),
                                            br(),
                                            checkboxInput("activeG9",h6(paste(gettext("Create Group"),9)),FALSE),
                                            conditionalPanel(
                                              condition="input.activeG9==true",
                                              fluidRow(
                                              column(3,
                                                     radioButtons("typeG9"," ",choices=list(gettext("Quantitative"),gettext("Qualitative"),gettext("Frequencies")),selected=gettext("Quantitative"))
                                              ),
                                              column(3,
                                                     uiOutput("listvarG9")),
                                              column(3,
                                                     textInput("nameG9", label = " ", value = "Gr 9"),
                                                     conditionalPanel(
                                             condition=paste("input.typeG9=='",gettext("Quantitative"),"'",sep=''),
#                                                       condition="input.typeG9=='quant'",
                                                       br(),
                                                       radioButtons("scale9","",choices=list(gettext("Scaled"),gettext("Unscaled")),selected=gettext("Scaled"),inline=TRUE))),
                                              column(3,
                                                     radioButtons("typeG92","",choices=list(gettext("Active"),gettext("Supplementary")),selected=gettext("Active")))),
                                              br(),
                                              checkboxInput("activeG10",h6(paste(gettext("Create Group"),10)),FALSE),
                                              conditionalPanel(
                                                condition="input.activeG10==true",
                                                fluidRow(
                                                column(3,
                                                       radioButtons("typeG10"," ",choices=list(gettext("Quantitative"),gettext("Qualitative"),gettext("Frequencies")),selected=gettext("Quantitative"))
                                                ),
                                                column(3,
                                                       uiOutput("listvarG10")),
                                                column(3,
                                                       textInput("nameG10", label = " ", value = "Gr 10"),
                                                       conditionalPanel(
                                             condition=paste("input.typeG10=='",gettext("Quantitative"),"'",sep=''),
#                                                         condition="input.typeG10=='quant'",
                                                         br(),
                                                         radioButtons("scale10","",choices=list(gettext("Scaled"),gettext("Unscaled")),selected=gettext("Scaled"),inline=TRUE))),
                                                column(3,
                                                       radioButtons("typeG102","",choices=list(gettext("Active"),gettext("Supplementary")),selected=gettext("Active"))))
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                  )
                                )
                             )),
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
