# ui script for HCPC2

fluidPage(
  titlePanel(div(paste(gettext("HCPC on the dataset "),nomDataHCPCshiny),style="color:#0A2A12",align="center"),windowTitle="HCPCshiny"),

  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #D2FAE5; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
        tags$style(type='text/css', "#title2 { height: 25px; }"),
        tags$style(type='text/css', "#title3 { height: 25px; }"),
		tags$style(type="text/css", "#loadmessage { padding: 5px 0px 5px 0px; text-align: center; font-weight: bold; font-size: 100%; color: #000000; background-color: #ff8533; z-index: 105; }")
      ),
      wellPanel(
        div(align="center",checkboxInput("hcpcparam",gettext("Show HCPC parameters"),FALSE)),
      conditionalPanel(
        condition="input.hcpcparam==true",
      uiOutput("clusters"),
      hr(),
      checkboxInput("consoli","Consolidation",consolidfHCPCshiny),
      hr(),
      radioButtons("metric",gettext("Which metric would you like to use?"),choices=list(gettext("Euclidean"),"Manhattan"),inline=TRUE,select=metricdfHCPCshiny)
      ),
      style = "padding: 3px;background-color: #ffdbdb;"),
      wellPanel(
      div(align="center",checkboxInput("graph",gettext("Graphical options"),FALSE)),
      conditionalPanel(
        condition="input.graph==true",
      fluidRow(
        column(5,selectInput("nb1", label = h6(gettext("x axis")),
                             choices = list("1" = 1, "2" = 2, "3" = 3,"4"= 4,"5" =5), selected = nb1dfHCPCshiny,width='80%')),
        column(5,selectInput("nb2", label =h6(gettext("y axis")), 
                             choices = list("1" = 1, "2" = 2,"3" = 3,"4"= 4,"5" =5), selected = nb2dfHCPCshiny,width='80%'))),
      hr(),
      radioButtons("HCPCgraph",h6(gettext("Which graph do you want to modify?")),
                   choices=list(gettext("Hierarchical tree"),gettext("Factorial map"),gettext("3D plot")),inline=TRUE),
      hr(),
      conditionalPanel(
        condition=paste("input.HCPCgraph=='",gettext("3D plot"),"'",sep=''),
        textInput("title1HCPCshiny",h6(gettext("Title of the graph: ")), title1HCPCshiny),
        checkboxInput("nom3D",gettext("Names on 3D plot"),dfHCPCshiny),
        checkboxInput("center",gettext("Draw centers of clusters"),centerdfHCPCshiny),
        hr(),
        sliderInput("num",gettext("Angle (in degrees)"),value=numdfHCPCshiny,min=0,max=360,step=1)
        ),
      conditionalPanel(
        condition=paste("input.HCPCgraph=='",gettext("Factorial map"),"'",sep=''),
        textInput("title2HCPCshiny",h6(gettext("Title of the graph:")), title2HCPCshiny),
        checkboxInput("drawtree",gettext("Draw tree"),drawdfHCPCshiny)
        ),
      conditionalPanel(
         condition=paste("input.HCPCgraph=='",gettext("Hierarchical tree"),"'",sep=''),
       textInput("title3HCPCshiny",h6(gettext("Title of the graph:")), title3HCPCshiny))
      ),
      style = "padding: 3px;background-color: #fcefba"),
      wellPanel(
        div(align="center",checkboxInput("reportparam",gettext("Automatic report"),FALSE)),
        conditionalPanel(
          condition="input.reportparam==true",
		  div(gettext("File name (without extension):")),
          textInput("titleFile",NULL, paste0(gettext("Report"),"_",Sys.Date()),width=200),
          if (strsplit(Sys.getlocale("LC_COLLATE"),"_")[[1]][1]!="French"){ radioButtons("choixLANG",gettext("Language"), choices=c(gettext("English"),gettext("French")), selected = gettext("English"), inline=TRUE)} else {radioButtons("choixLANG",gettext("Language"), choices=c(gettext("English"),gettext("French")), selected = gettext("French"), inline=TRUE)},
          # div(actionButton("InvestigateRmd", "Rmd"), actionButton("Investigatehtml", "html"), actionButton("Investigatedoc", "doc")),
		  div(downloadButton("downloadInvestigateRmd", "Rmd",style = "padding: 3px;"),actionButton("Investigatehtml", "html"), actionButton("Investigatedoc", "doc")),
		  conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div(gettext("Ongoing reporting process..."),id="loadmessage"))
        ),
        align="center", style = "padding: 3px;background-color: #dbe6ff"
      ),
      div(align="center",actionButton("HCPCcode", gettext("Get the HCPC code"),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;')),
      div(align="center",actionButton("Quit", gettext("Quit the app"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
      
      mainPanel(
        tags$style(type = "text/css", "a{color: #0B6121;}"),
        tabsetPanel(id = "graph_sort",
                    tabPanel(gettext("Graphs"),
fluidRow(
                           br(),
                 column(width = 6,plotOutput("map4", width = "500", height="500"),
                             br(),
                        p(gettext("Download as"),downloadButton("downloadData6","jpg"),downloadButton("downloadData7","png"),downloadButton("downloadData8","pdf"),align="center"),
                             br(),
							 align="center"),
                 column(width = 6,plotOutput("map", width = "500", height="500"),
                             br(),
                        p(gettext("Download as"),downloadButton("downloadData1","jpg"),downloadButton("downloadData","png"),downloadButton("downloadData2","pdf"),align="center"),
 							 align="center")),
                            br(),
                             div(align="center",plotOutput("map2",width=750,height=500)),
                             br(),
                p(gettext("Download as"),downloadButton("downloadData4","jpg"),downloadButton("downloadData3","png"),downloadButton("downloadData5","pdf"),align="center"),
                             br()
                             ),

                    tabPanel(gettext("Values"),
                             br(),
                             radioButtons("out",gettext("Which outputs do you want?"),
                                          choices=list(gettext("Description of classes by variables"),gettext("Description of classes by axes"),gettext("Parangons")),selected=gettext("Description of classes by variables"),inline=TRUE),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Description of classes by variables"),"'",sep=''),
                               div(align="center",tableOutput("descript"))
                               ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Parangons"),"'",sep=''),
                               div(align="center",tableOutput("parangons"))),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Description of classes by axes"),"'",sep=''),
                               div(align="center",tableOutput("axes")))
                             )
        )
      ,width=9)
    )
)
