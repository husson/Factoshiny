# ui2.R AFM

shinyUI(fluidPage(
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
        div(align="center",checkboxInput("graph",gettext("Graphical options"),FALSE)),
        conditionalPanel(
          condition="input.graph==true",
        div(align="center",selectInput("choixgraph",h6(gettext("Which graph would you like to modify?")), choices=list(gettext("Groups"),gettext("Individuals"),gettext("Quantitative variables"),gettext("Frequencies"),gettext("Partial axes")),selected=gettext("Groups"))),
        hr(),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Individuals"),"'",sep=''),
#          condition="input.choixgraph=='ind'",
          textInput("title2",h6(gettext("Title of the graph :")), title2),
          checkboxInput("meanind1",gettext("Plot points for the mean individuals"),TRUE),
          checkboxInput("meanind",gettext("Draw labels for the mean individuals"),TRUE),
          checkboxInput("qualind1",gettext("Plot points for the categories"),TRUE),
          checkboxInput("qualind",gettext("Draw labels for the categories"),TRUE),
          hr(),
          uiOutput("drawindiv"),
          conditionalPanel(
            condition=paste("input.drawind=='",gettext("categorical variable"),"'",sep=''),
#            condition="input.drawind=='c'",
            uiOutput("habillagequali")
            ),
          hr(),
#          radioButtons("choixpartial",gettext("Partial points are drawn"),choices=list("None"=1,"All"=2,"Choose"=3),selected=1,inline=TRUE),
          radioButtons("choixpartial",gettext("Partial points to draw"),choices=list(gettext("None"),gettext("All"),gettext("Choose")),selected=gettext("None"),inline=TRUE),
          conditionalPanel(
#            condition="input.choixpartial==3",
            condition=paste("input.choixpartial=='",gettext("Choose"),"'",sep=''),
            selectInput("indivpartiel",label=gettext("Select individuals"),
                      choices=list(num=nom),multiple=TRUE)),
          hr(),
          conditionalPanel(
#            condition="input.choixpartial!=1",
            condition=paste("input.choixpartial!='",gettext("None"),"'",sep=''),
            checkboxInput("partind",gettext("Draw labels for the partial individuals"),TRUE))
          ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Quantitative variables"),"'",sep=''),
#          condition="input.choixgraph=='quant'",
          textInput("title3",h6(gettext("Title of the graph: ")), title3),
          radioButtons("selection",gettext("Draw variables according to:"),choices=list(gettext("No selection"),"Contribution"="contrib","Cos2"="cos2"),selected=gettext("No selection")),
          uiOutput("slider1"),
          hr(),
          uiOutput("hide2"),
          checkboxInput("colorgroup",gettext("Color the variables by group"),TRUE) 
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Frequencies"),"'",sep=''),
#          condition="input.choixgraph=='freq'",
          textInput("title5",h6(gettext("Title of the graph: ")), title5),
          checkboxInput("affichind",gettext("Draw labels for the mean individuals"),TRUE),
          checkboxInput("affichcol",gettext("Draw labels for the columns"),TRUE)
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Partial axes"),"'",sep=''),
#          condition="input.choixgraph=='axes'",
          textInput("title4",h6(gettext("Title of the graph: ")), title4),
          checkboxInput("coloraxe",gettext("Color the partial axe by group"),TRUE)
        ),
        conditionalPanel(
          condition=paste("input.choixgraph=='",gettext("Groups"),"'",sep=''),
#          condition="input.choixgraph=='group'",
          textInput("title1",h6(gettext("Title of the graph: ")), title1)
        ),
        fluidRow(
          # column(5,selectInput("nb1", label = h6(gettext("x axis")), 
                               # choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = 1,width='80%')),
        column(5,textInput("nb1", label = h6(gettext("x axis")), 1,width='50%')),

		
          # column(5,selectInput("nb2", label =h6(gettext("y axis")), 
                               # choices = list("1" = 1, "2" = 2,"3" = 3,"4"= 4,"5" =5), selected = 2,width='80%')))
        column(5,textInput("nb2", label = h6(gettext("y axis")), 2,width='50%')))
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
                                    checkboxInput("activeG5",h6(paste(gettext("Create Group"),4)),FALSE),
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
                               plotOutput("map3", width = "500", height="500")),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the individuals"),"'",sep=''),
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
                             verbatimTextOutput("summary"),
                             selectInput("bam",gettext("Graphs for"),choices=list(IdChoices=VariableChoices),multiple=FALSE),
                             plotOutput("histo")),
                    
                    tabPanel(gettext("Data"),
                             br(),
                             dataTableOutput("JDD")
                             )
        )
      ,width=9)
    )
))
