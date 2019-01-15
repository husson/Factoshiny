# ui script for HCPC for dataframe2

shinyUI(fluidPage(
  titlePanel(div(paste(gettext("HCPC on the dataset "),nomData),style="color:#0A2A12",align="center"),windowTitle="HCPCshiny"),
#  titlePanel(div(paste("HCPC on the ",unlist(strsplit(nomData, split='[', fixed=TRUE))[1]," dataset"),style="color:#0A2A12",align="center")),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #D2FAE5; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
        tags$style(type='text/css', "#title2 { height: 25px; }"),
        tags$style(type='text/css', "#title3 { height: 25px; }")
      ),
      wellPanel(
      div(align="center",checkboxInput("hcpcparam",gettext("Show HCPC parameters"),FALSE)),
      conditionalPanel(
        condition="input.hcpcparam==true",
      uiOutput("clusters"),
      hr(),
      checkboxInput("consoli","Consolidation",consolidf),
      hr(),
      radioButtons("metric",gettext("Which metric would you like to use?"),choices=list(gettext("Euclidean"),"Manhattan"),inline=TRUE,select=metricdf)
      )),
      wellPanel(
      div(align="center",checkboxInput("graph",gettext("Show graphs options"),FALSE)),
      conditionalPanel(
        condition="input.graph==true",
      fluidRow(
        column(5,selectInput("nb1", label = h6(gettext("x axis")), 
                             choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = nb1df,width='80%')),
        column(5,selectInput("nb2", label =h6(gettext("y axis")), 
                             choices = list("1" = 1, "2" = 2,"3" = 3,"4"= 4,"5" =5), selected = nb2df,width='80%'))),
      hr(),
      radioButtons("HCPCgraph",h6(gettext("Which graph do you want to modify?")),
                   choices=list(gettext("Hierarchical tree"),gettext("Factorial map"),gettext("3D plot")),inline=TRUE),
      hr(),
      conditionalPanel(
#        condition="input.HCPCgraph=='3D'",
        condition=paste("input.HCPCgraph=='",gettext("3D plot"),"'",sep=''),
        textInput("title1",h6(gettext("Title of the graph: ")), title1),
        checkboxInput("nom3D",gettext("Names on 3D plot"),df),
        checkboxInput("center",gettext("Draw centers of clusters"),centerdf),
        hr(),
        sliderInput("num",gettext("Angle (in degrees)"),value=numdf,min=0,max=360,step=1)
        ),
      conditionalPanel(
#        condition="input.HCPCgraph=='ind'",
        condition=paste("input.HCPCgraph=='",gettext("Factorial map"),"'",sep=''),
        textInput("title2",h6(gettext("Title of the graph: ")), title2),
        checkboxInput("drawtree",gettext("Draw tree"),drawdf)
        ),
      conditionalPanel(
#        condition="input.HCPCgraph=='tree'",
         condition=paste("input.HCPCgraph=='",gettext("Hierarchical tree"),"'",sep=''),
        textInput("title3",h6(gettext("Title of the graph: ")), title3))
      )),
      wellPanel(
        h5(gettext("Save graphs as:"),align="center"),
        radioButtons("paramdown","",
                     choices=list("PNG"="png","JPG"="jpg","PDF"="pdf"),selected="png"),
        br(),
        div(align="center",actionButton("HCPCcode", gettext("Get the HCPC code")))),
      div(align="center",actionButton("Quit", gettext("Quit the app")))
      ,width=3),
      
      mainPanel(
        tags$style(type = "text/css", "a{color: #0B6121;}"),
        tabsetPanel(id = "graph_sort",
                    tabPanel(gettext("Graphs"),
 fluidRow(
                           br(),
                 column(width = 6,plotOutput("map4", width = "500", height="500"),
#                             div(align="center",plotOutput("map4",width = 650, height=500)),
                             br(),
                             conditionalPanel(
                               condition="input.paramdown=='jpg'",
                               p(downloadButton("downloadData6",gettext("Download as jpg")),align="center")),
                             conditionalPanel(
                               condition="input.paramdown=='png'",
                               p(downloadButton("downloadData7",gettext("Download as png")),align="center")),
                             conditionalPanel(
                               condition="input.paramdown=='pdf'",
                               p(downloadButton("downloadData8",gettext("Download as pdf")),align="center")),
                             br(),
							 align="center"),
                 column(width = 6,plotOutput("map", width = "500", height="500"),
#                             div(align="center",plotOutput("map",width = 500, height=500)),
                             br(),
                             conditionalPanel(
                               condition="input.paramdown=='jpg'",
                               p(downloadButton("downloadData1",gettext("Download as jpg")),align="center")),
                             conditionalPanel(
                               condition="input.paramdown=='png'",
                               p(downloadButton("downloadData",gettext("Download as png")),align="center")),
                             conditionalPanel(
                               condition="input.paramdown=='pdf'",
                               p(downloadButton("downloadData2",gettext("Download as pdf")),align="center")),
                             br(),
							 align="center")),
                             div(align="center",plotOutput("map2",width=750,height=500)),
                             br(),
                             conditionalPanel(
                               condition="input.paramdown=='jpg'",
                               p(downloadButton("downloadData4",gettext("Download as jpg")),align="center")),
                             conditionalPanel(
                               condition="input.paramdown=='png'",
                               p(downloadButton("downloadData3",gettext("Download as png")),align="center")),
                             conditionalPanel(
                               condition="input.paramdown=='pdf'",
                               p(downloadButton("downloadData5",gettext("Download as pdf")),align="center")),
                             br()
                             ),

                    tabPanel(gettext("Values"),
                             br(),
                             radioButtons("out",gettext("Which outputs do you want?"),
                                  choices=list(gettext("Description of classes by variables"),gettext("Description of classes by axes"),gettext("Parangons")),selected=gettext("Description of classes by variables"),inline=TRUE),
                             conditionalPanel(
#                               condition="input.out=='var'",
                               condition=paste("input.out=='",gettext("Description of classes by variables"),"'",sep=''),
                               div(align="center",tableOutput("descript"))
                               ),
                             conditionalPanel(
#                               condition="input.out=='para'",
                               condition=paste("input.out=='",gettext("Parangons"),"'",sep=''),
                               div(align="center",tableOutput("parangons"))),
                             conditionalPanel(
#                               condition="input.out=='axe'",
                               condition=paste("input.out=='",gettext("Description of classes by axes"),"'",sep=''),
                               div(align="center",tableOutput("axes")))
                             ),
                    
                    tabPanel(gettext("Data"),
                             dataTableOutput("JDD")
                             )
        )
      ,width=9)
    )
))
