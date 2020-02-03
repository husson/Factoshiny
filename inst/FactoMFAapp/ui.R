# ui2.R AFM

fluidPage(
  titlePanel(div(paste(gettext("MFA on the dataset ",domain="R-Factoshiny"),nomData),style="color:#6E6E6E",align="center"),windowTitle="MFAshiny"),
  
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
      div(align="center",checkboxInput("mfaparam",gettext("Other MFA parameters",domain="R-Factoshiny"),FALSE)),
      conditionalPanel(
        condition="input.mfaparam==true",
          selectizeInput("indsup",gettext("Select supplementary individuals",domain="R-Factoshiny"),choices=nomMFAshiny, multiple=TRUE,selected=indsupl),
          uiOutput("imputeData"),
          actionButton("submit", label = gettext("Submit",domain="R-Factoshiny"))
      ),
      style = "padding: 3px;background-color: #ffdbdb;"),
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
                selectInput("indiv",label=gettext("Select individuals",domain="R-Factoshiny"),
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
          sliderInput("cexPartial",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=sizePartial,step=0.05,ticks=FALSE),
          textInput("nbDimPartialAxes",gettext("Number of dim to draw",domain="R-Factoshiny"), nbDimPartialAxes)
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
      div(align="center",actionButton("Quit", gettext("Quit the app",domain="R-Factoshiny"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
      
      mainPanel(
        tabsetPanel(id = "graph_sort",
                    tabPanel(gettext("Creation of groups",domain="R-Factoshiny"),
                             br(),
                            # radioButtons("activemodif",gettext("Create the groups",domain="R-Factoshiny"), choices=c(gettext("Create the groups with lines of code",domain="R-Factoshiny"),gettext("Create the groups one by one",domain="R-Factoshiny"),gettext("Validate the groups",domain="R-Factoshiny")), selected = gettext("Create the groups with lines of code",domain="R-Factoshiny")),
                            radioButtons("activemodif",gettext("Create the groups",domain="R-Factoshiny"), choices=c(gettext("Create the groups one by one (max 10 groups)",domain="R-Factoshiny"),gettext("Create the groups with lines of code",domain="R-Factoshiny")), selected = gettext("Create the groups one by one (max 10 groups)",domain="R-Factoshiny"), width="100%"),
							 actionButton("ValidateGroup", label = gettext("Validate the groups",domain="R-Factoshiny"),style='padding:5px; background-color: #F39081;text-align:center;white-space: normal;'),
                             br(),
                             conditionalPanel(
                               condition=paste0("input.activemodif=='",gettext("Create the groups with lines of code",domain="R-Factoshiny"),"'"),
                                textInput("DefGroup",gettext("Number of variables in each group",domain="R-Factoshiny"), width="100%",NULL,placeholder ="3 3 4   or   3,3,4  or  c(rep(3,2),4)"),
                                textInput("DefType",gettext("Type of each group (s for scale, c for continuous, f for frequency, n for nominal)",domain="R-Factoshiny"), width="100%", NULL,placeholder ="s s n   or   s,s,n   or   c('s','s','n')"),
                                textInput("DefNameGroup",gettext("Names of the groups (NULL by default)",domain="R-Factoshiny"), width="100%", NULL,placeholder ="c('Group1', 'G2', 'Gr quali')"),
                                textInput("DefNumGroupSup",gettext("Supplementary groups (if NULL all groups are active)",domain="R-Factoshiny"), width="100%", NULL)
		                     ),
                             conditionalPanel(
                               condition=paste0("input.activemodif=='",gettext("Create the groups one by one (max 10 groups)",domain="R-Factoshiny"),"'"),
                               h6("Group 1"),
                              fluidRow(
                              column(3,
                                     radioButtons("typeG1"," ",choices=listeType,selected=listeType[1])
                              ),
                              column(3,
                                     uiOutput("listvarG1")),
                              column(3,
                                     textInput("nameG1", label = " ", value = "Gr 1"),
                                     conditionalPanel(
                                       condition=paste("input.typeG1=='",gettext("Quantitative",domain="R-Factoshiny"),"'",sep=''),
                                       radioButtons("scale1","",choices=list(gettext("Scaled",domain="R-Factoshiny"),gettext("Unscaled",domain="R-Factoshiny")),selected=gettext("Scaled",domain="R-Factoshiny"),inline=TRUE))),
                              column(2,
                                     radioButtons("typeG12","",choices=list(gettext("Active",domain="R-Factoshiny"),gettext("Supplementary",domain="R-Factoshiny")),selected=gettext("Active",domain="R-Factoshiny")))),
                              br(),
                              checkboxInput("activeG2",h6(paste(gettext("Create Group",domain="R-Factoshiny"),2)),FALSE),
                              conditionalPanel(
                                condition="input.activeG2==true",
                                fluidRow(
                                column(3,
                                       radioButtons("typeG2"," ",choices=listeType,selected=listeType[1])
                                ),
                                column(3,
                                       uiOutput("listvarG2")),
                                column(3,
                                       textInput("nameG2", label = " ", value = "Gr 2"),
                                       conditionalPanel(
                                       condition=paste("input.typeG2=='",gettext("Quantitative",domain="R-Factoshiny"),"'",sep=''),
                                         radioButtons("scale2","",choices=list(gettext("Scaled",domain="R-Factoshiny"),gettext("Unscaled",domain="R-Factoshiny")),selected=gettext("Scaled",domain="R-Factoshiny"),inline=TRUE))),
                                column(2,
                                       radioButtons("typeG22","",choices=list(gettext("Active",domain="R-Factoshiny"),gettext("Supplementary",domain="R-Factoshiny")),selected=gettext("Active",domain="R-Factoshiny")))),
                                br(),
                                checkboxInput("activeG3",h6(paste(gettext("Create Group",domain="R-Factoshiny"),3)),FALSE),
                                conditionalPanel(
                                  condition="input.activeG3==true",
                                  fluidRow(
                                  column(3,
                                         radioButtons("typeG3"," ",choices=listeType,selected=listeType[1])
                                  ),
                                  column(3,
                                         uiOutput("listvarG3")),
                                  column(3,
                                         textInput("nameG3", label = " ", value = "Gr 3"),
                                         conditionalPanel(
                                            condition=paste("input.typeG3=='",gettext("Quantitative",domain="R-Factoshiny"),"'",sep=''),
                                           radioButtons("scale3","",choices=list(gettext("Scaled",domain="R-Factoshiny"),gettext("Unscaled",domain="R-Factoshiny")),selected=gettext("Scaled",domain="R-Factoshiny"),inline=TRUE))),
                                  column(3,
                                         radioButtons("typeG32","",choices=list(gettext("Active",domain="R-Factoshiny"),gettext("Supplementary",domain="R-Factoshiny")),selected=gettext("Active",domain="R-Factoshiny")))),
                                  br(),
                                  checkboxInput("activeG4",h6(paste(gettext("Create Group",domain="R-Factoshiny"),4)),FALSE),
                                  conditionalPanel(
                                    condition="input.activeG4==true",
                                    fluidRow(
                                    column(3,
                                           radioButtons("typeG4"," ",choices=listeType,selected=listeType[1])
                                    ),
                                    column(3,
                                           uiOutput("listvarG4")),
                                    column(3,
                                           textInput("nameG4", label = " ", value = "Gr 4"),
                                           conditionalPanel(
                                             condition=paste("input.typeG4=='",gettext("Quantitative"),"'",sep=''),
                                             br(),
                                             radioButtons("scale4","",choices=list(gettext("Scaled",domain="R-Factoshiny"),gettext("Unscaled",domain="R-Factoshiny")),selected=gettext("Scaled",domain="R-Factoshiny"),inline=TRUE))),
                                    column(3,
                                           radioButtons("typeG42","",choices=list(gettext("Active",domain="R-Factoshiny"),gettext("Supplementary",domain="R-Factoshiny")),selected=gettext("Active",domain="R-Factoshiny")))),
                                    br(),
                                    checkboxInput("activeG5",h6(paste(gettext("Create Group",domain="R-Factoshiny"),5)),FALSE),
                                    conditionalPanel(
                                      condition="input.activeG5==true",
                                      fluidRow(
                                      column(3,
                                             radioButtons("typeG5"," ",choices=listeType,selected=listeType[1])
                                      ),
                                      column(3,
                                             uiOutput("listvarG5")),
                                      column(3,
                                             textInput("nameG5", label = " ", value = "Gr 5"),
                                             conditionalPanel(
                                               condition=paste("input.typeG5=='",gettext("Quantitative",domain="R-Factoshiny"),"'",sep=''),
                                               br(),
                                               radioButtons("scale5","",choices=list(gettext("Scaled",domain="R-Factoshiny"),gettext("Unscaled",domain="R-Factoshiny")),selected=gettext("Scaled",domain="R-Factoshiny"),inline=TRUE))),
                                      column(3,
                                             radioButtons("typeG52","",choices=list(gettext("Active",domain="R-Factoshiny"),gettext("Supplementary",domain="R-Factoshiny")),selected=gettext("Active",domain="R-Factoshiny")))),
                                      br(),
                                      checkboxInput("activeG6",h6(paste(gettext("Create Group",domain="R-Factoshiny"),6)),FALSE),
                                      conditionalPanel(
                                        condition="input.activeG6==true",
                                        fluidRow(
                                        column(3,
                                               radioButtons("typeG6"," ",choices=listeType,selected=listeType[1])
                                        ),
                                        column(3,
                                               uiOutput("listvarG6")),
                                        column(3,
                                               textInput("nameG6", label = " ", value = "Gr 6"),
                                               conditionalPanel(
                                             condition=paste("input.typeG6=='",gettext("Quantitative",domain="R-Factoshiny"),"'",sep=''),
                                                 br(),
                                                 radioButtons("scale6","",choices=list(gettext("Scaled",domain="R-Factoshiny"),gettext("Unscaled",domain="R-Factoshiny")),selected=gettext("Scaled",domain="R-Factoshiny"),inline=TRUE))),
                                        column(3,
                                               radioButtons("typeG62","",choices=list(gettext("Active",domain="R-Factoshiny"),gettext("Supplementary",domain="R-Factoshiny")),selected=gettext("Active",domain="R-Factoshiny")))),
                                        br(),
                                        checkboxInput("activeG7",h6(paste(gettext("Create Group",domain="R-Factoshiny"),7)),FALSE),
                                        conditionalPanel(
                                          condition="input.activeG7==true",
                                          fluidRow(
                                          column(3,
                                                 radioButtons("typeG7"," ",choices=listeType,selected=listeType[1])
                                          ),
                                          column(3,
                                                 uiOutput("listvarG7")),
                                          column(3,
                                                 textInput("nameG7", label = " ", value = "Gr 7"),
                                                 conditionalPanel(
                                                   condition=paste("input.typeG7=='",gettext("Quantitative",domain="R-Factoshiny"),"'",sep=''),
                                                   br(),
                                                   radioButtons("scale7","",choices=list(gettext("Scaled",domain="R-Factoshiny"),gettext("Unscaled",domain="R-Factoshiny")),selected=gettext("Scaled",domain="R-Factoshiny"),inline=TRUE))),
                                          column(3,
                                                 radioButtons("typeG72","",choices=list(gettext("Active",domain="R-Factoshiny"),gettext("Supplementary",domain="R-Factoshiny")),selected=gettext("Active",domain="R-Factoshiny")))),
                                          br(),
                                          checkboxInput("activeG8",h6(paste(gettext("Create Group",domain="R-Factoshiny"),8)),FALSE),
                                          conditionalPanel(
                                            condition="input.activeG8==true",
                                            fluidRow(
                                            column(3,
                                                   radioButtons("typeG8"," ",choices=listeType,selected=listeType[1])
                                            ),
                                            column(3,
                                                   uiOutput("listvarG8")),
                                            column(3,
                                                   textInput("nameG8", label = " ", value = "Gr 8"),
                                                   conditionalPanel(
                                             condition=paste("input.typeG8=='",gettext("Quantitative",domain="R-Factoshiny"),"'",sep=''),
                                                     br(),
                                                     radioButtons("scale8","",choices=list(gettext("Scaled",domain="R-Factoshiny"),gettext("Unscaled",domain="R-Factoshiny")),selected=gettext("Scaled",domain="R-Factoshiny"),inline=TRUE))),
                                            column(3,
                                                   radioButtons("typeG82","",choices=list(gettext("Active",domain="R-Factoshiny"),gettext("Supplementary",domain="R-Factoshiny")),selected=gettext("Active",domain="R-Factoshiny")))),
                                            br(),
                                            checkboxInput("activeG9",h6(paste(gettext("Create Group",domain="R-Factoshiny"),9)),FALSE),
                                            conditionalPanel(
                                              condition="input.activeG9==true",
                                              fluidRow(
                                              column(3,
                                                     radioButtons("typeG9"," ",choices=listeType,selected=listeType[1])
                                              ),
                                              column(3,
                                                     uiOutput("listvarG9")),
                                              column(3,
                                                     textInput("nameG9", label = " ", value = "Gr 9"),
                                                     conditionalPanel(
                                             condition=paste("input.typeG9=='",gettext("Quantitative",domain="R-Factoshiny"),"'",sep=''),
                                                       br(),
                                                       radioButtons("scale9","",choices=list(gettext("Scaled",domain="R-Factoshiny"),gettext("Unscaled",domain="R-Factoshiny")),selected=gettext("Scaled",domain="R-Factoshiny"),inline=TRUE))),
                                              column(3,
                                                     radioButtons("typeG92","",choices=list(gettext("Active",domain="R-Factoshiny"),gettext("Supplementary",domain="R-Factoshiny")),selected=gettext("Active",domain="R-Factoshiny")))),
                                              br(),
                                              checkboxInput("activeG10",h6(paste(gettext("Create Group",domain="R-Factoshiny"),10)),FALSE),
                                              conditionalPanel(
                                                condition="input.activeG10==true",
                                                fluidRow(
                                                column(3,
                                                       radioButtons("typeG10"," ",choices=listeType,selected=listeType[1])
                                                ),
                                                column(3,
                                                       uiOutput("listvarG10")),
                                                column(3,
                                                       textInput("nameG10", label = " ", value = "Gr 10"),
                                                       conditionalPanel(
                                             condition=paste("input.typeG10=='",gettext("Quantitative",domain="R-Factoshiny"),"'",sep=''),
                                                         br(),
                                                         radioButtons("scale10","",choices=list(gettext("Scaled",domain="R-Factoshiny"),gettext("Unscaled",domain="R-Factoshiny")),selected=gettext("Scaled",domain="R-Factoshiny"),inline=TRUE))),
                                                column(3,
                                                       radioButtons("typeG102","",choices=list(gettext("Active",domain="R-Factoshiny"),gettext("Supplementary",domain="R-Factoshiny")),selected=gettext("Active",domain="R-Factoshiny"))))
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
                               radioButtons("out3",gettext("What type of results?",domain="R-Factoshiny"),choices=list(gettext("Coordinates",domain="R-Factoshiny"),gettext("Contributions",domain="R-Factoshiny"),gettext("Cos2",domain="R-Factoshiny"),gettext("Correlations",domain="R-Factoshiny")),selected=gettext("Coordinates",domain="R-Factoshiny"),inline=TRUE),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Coordinates",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties11"))),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Contributions",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties22"))),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Cos2",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties33"))),
                               conditionalPanel(
                               condition=paste("input.out3=='",gettext("Correlations",domain="R-Factoshiny"),"'",sep=''),
                                 div(align="center",tableOutput("sorties44")))
                             ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the groups",domain="R-Factoshiny"),"'",sep=''),
                                h6(gettext("Lg coefficients",domain="R-Factoshiny")),
                                div(align="center",tableOutput("sortiegroupLg")),
                                h6(gettext("RV coefficients",domain="R-Factoshiny")),
                                div(align="center",tableOutput("sortiegroupRV")),
                                h6(gettext("Group coordinates",domain="R-Factoshiny")),
                                div(align="center",tableOutput("sortiegroupcoord")),
                                h6(gettext("Group contribution",domain="R-Factoshiny")),
                                div(align="center",tableOutput("sortiegroupcontrib")),
                                h6(gettext("Group cos2",domain="R-Factoshiny")),
                                div(align="center",tableOutput("sortiegroupcos2")),
                                h6(gettext("Group correlation",domain="R-Factoshiny")),
                                div(align="center",tableOutput("sortiegroupcorrelation"))
                                # h6(gettext("Group dist2",domain="R-Factoshiny")),
                                # div(align="center",tableOutput("sortiegroupdist2")),
                                # h6(gettext("Supplementary group coordinates",domain="R-Factoshiny")),
                                # div(align="center",tableOutput("sortiegroupcoordsup")),
                                # h6(gettext("Supplementary group cos2",domain="R-Factoshiny")),
                                # div(align="center",tableOutput("sortiegroupcos2sup")),
                                # h6(gettext("Supplementary group dist2",domain="R-Factoshiny")),
                                # div(align="center",tableOutput("sortiegroupdist2sup"))
                                # tableOutput("sortiegroupOther")
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
                  tabPanel(gettext("Automatic description of axes",domain="R-Factoshiny"),
                             div(verbatimTextOutput("CodePrintedDimdesc")),
                           br(),
                           numericInput("pvalueDimdesc",gettext("P-value",domain="R-Factoshiny"),value=pvalueDimdescInit, min=0,max=1),
                           radioButtons("Dim",label=gettext("Choose the dimensions",domain="R-Factoshiny"),choices=list("Dimension 1"="Dim1","Dimension 2"="Dim2","Dimension 3"="Dim3"),selected="Dim1"),
                           conditionalPanel(
                             condition="input.Dim=='Dim1'",
                             p(gettext("Categorical variables",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sortieDimdesc2")),
                             br(),
                             p(gettext("Categories",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sortieDimdesc")),
                             br(),
                             p(gettext("Quantitative variables",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sortieDimdesc3"))
                           ),
                           br(), 
                           conditionalPanel(
                             condition="input.Dim=='Dim2'",
                             p(gettext("Categorical variables",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sortieDimdesc22")),
                             br(),
                             p(gettext("Categories",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sortieDimdesc00")),
                             br(),
                             p(gettext("Quantitative variables",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sortieDimdesc33"))
                           ),
                           br(),
                           conditionalPanel(
                             condition="input.Dim=='Dim3'",
                             p(gettext("Categorical variables",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sortieDimdesc222")),
                             br(),
                             p(gettext("Categories",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sortieDimdesc000")),
                             br(),
                             p(gettext("Quantitative variables",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sortieDimdesc333"))
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
