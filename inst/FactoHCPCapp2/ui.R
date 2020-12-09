# ui script for HCPC2

fluidPage(
  titlePanel(div(paste(gettext("HCPC on the dataset",domain="R-Factoshiny"),nomDataHCPCshiny),style="color:#0A2A12",align="center"),windowTitle="HCPCshiny"),

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
        div(align="center",checkboxInput("hcpcparam",gettext("Show HCPC parameters",domain="R-Factoshiny"),FALSE)),
      conditionalPanel(
        condition="input.hcpcparam==true",
      uiOutput("clusters"),
      uiOutput("clusterCA"),
	  div(checkboxInput("kkparam",gettext("Kmeans preprocessing before clustering (useful if many elements)",domain="R-Factoshiny"),kkparamInit)),
      conditionalPanel(
        condition="input.kkparam==true",
        uiOutput("kkInt")),
      checkboxInput("consoli","Consolidation",consolidfHCPCshiny),
      radioButtons("metric",gettext("Which metric would you like to use?",domain="R-Factoshiny"),choices=list(gettext("Euclidean",domain="R-Factoshiny"),"Manhattan"),inline=TRUE,select=metricdfHCPCshiny)
      ),
      style = "padding: 3px;background-color: #ffdbdb;"),
      wellPanel(
      div(align="center",checkboxInput("graph",gettext("Graphical options",domain="R-Factoshiny"),FALSE)),
      conditionalPanel(
        condition="input.graph==true",
          div(gettext("Axes:",domain="R-Factoshiny"), style="display: inline-block;padding: 5px"),
          div(uiOutput("NB1"), style="display: inline-block;"),
          div(uiOutput("NB2"), style="display: inline-block;"),
      radioButtons("HCPCgraph",gettext("Which graph do you want to modify?",domain="R-Factoshiny"),
                   choices=list(gettext("Hierarchical tree",domain="R-Factoshiny"),gettext("Factorial map",domain="R-Factoshiny"),gettext("3D plot",domain="R-Factoshiny")),inline=TRUE),
      conditionalPanel(
        condition=paste("input.HCPCgraph=='",gettext("3D plot",domain="R-Factoshiny"),"'",sep=''),
        textInput("title1HCPCshiny",gettext("Title of the graph: ",domain="R-Factoshiny"), title1HCPCshiny),
        checkboxInput("nom3D",gettext("Names on 3D plot",domain="R-Factoshiny"),dfHCPCshiny),
        checkboxInput("center",gettext("Draw centers of clusters",domain="R-Factoshiny"),centerdfHCPCshiny),
        hr(),
        sliderInput("num",gettext("Angle (in degrees)",domain="R-Factoshiny"),value=numdfHCPCshiny,min=0,max=360,step=1)
        ),
      conditionalPanel(
        condition=paste("input.HCPCgraph=='",gettext("Factorial map",domain="R-Factoshiny"),"'",sep=''),
        textInput("title2HCPCshiny",gettext("Title of the graph:",domain="R-Factoshiny"), title2HCPCshiny),
        checkboxInput("drawtree",gettext("Draw tree",domain="R-Factoshiny"),drawdfHCPCshiny)
        ),
      conditionalPanel(
        condition=paste("input.HCPCgraph=='",gettext("Hierarchical tree",domain="R-Factoshiny"),"'",sep=''),
        textInput("title3HCPCshiny",gettext("Title of the graph:",domain="R-Factoshiny"), title3HCPCshiny))
      ),
      style = "padding: 3px;background-color: #fcefba"),
      wellPanel(
        div(align="center",checkboxInput("reportparam",gettext("Automatic report",domain="R-Factoshiny"),FALSE)),
        conditionalPanel(
          condition="input.reportparam==true",
		  div(gettext("File name (without extension):",domain="R-Factoshiny")),
          textInput("titleFile",NULL, paste0(gettext("Report",domain="R-Factoshiny"),"_",Sys.Date()),width=200),
          if (strsplit(Sys.getlocale("LC_COLLATE"),"_")[[1]][1]!="French"){ radioButtons("choixLANG",gettext("Language",domain="R-Factoshiny"), choices=c(gettext("English",domain="R-Factoshiny"),gettext("French",domain="R-Factoshiny")), selected = gettext("English",domain="R-Factoshiny"), inline=TRUE)} else {radioButtons("choixLANG",gettext("Language",domain="R-Factoshiny"), choices=c(gettext("English",domain="R-Factoshiny"),gettext("French",domain="R-Factoshiny")), selected = gettext("French",domain="R-Factoshiny"), inline=TRUE)},
		  div(downloadButton("downloadInvestigateRmd", "Rmd",style = "padding: 3px;"),actionButton("Investigatehtml", "html"), actionButton("Investigatedoc", "doc")),
		  conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div(gettext("Ongoing reporting process...",domain="R-Factoshiny"),id="loadmessage"))
        ),
        align="center", style = "padding: 3px;background-color: #dbe6ff"
      ),
      wellPanel(
        div(align="center",checkboxInput("HCPCcode",gettext("Get the HCPC code",domain="R-Factoshiny"),FALSE)),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;'
	  ),
      div(align="center",actionButton("Quit", gettext("Quit the app",domain="R-Factoshiny"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
      
      mainPanel(
        tags$style(type = "text/css", "a{color: #0B6121;}"),
        tabsetPanel(id = "graph_sort",
                    tabPanel(gettext("Graphs",domain="R-Factoshiny"),
                             div(verbatimTextOutput("CodePrinted")),
fluidRow(
                           br(),
                 column(width = 6,shinyjqui::jqui_resizable(plotOutput("mapTree", height="500")),
                             br(),
                        p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData6","jpg"),downloadButton("downloadData7","png"),downloadButton("downloadData8","pdf"),align="center"),
                             br(),
							 align="center"),
                 column(width = 6,shinyjqui::jqui_resizable(plotOutput("map2D", height="500")),
                             br(),
                        p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData1","jpg"),downloadButton("downloadData","png"),downloadButton("downloadData2","pdf"),align="center"),
 							 align="center")),
                            br(),
                              div(align="center",shinyjqui::jqui_resizable(plotOutput("map3D", height="500"))),
                             br(),
                p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData4","jpg"),downloadButton("downloadData3","png"),downloadButton("downloadData5","pdf"),align="center"),
                             br()
                             ),

                    tabPanel(gettext("Clusters' characterization",domain="R-Factoshiny"),
                             # div(verbatimTextOutput("CodePrintedCatdes")),
                             br(),
                             numericInput(inputId = "select_proba_plot", label = gettext("P-value",domain="R-Factoshiny"), min = 0, max = 1, value = 0.05, step = 0.1),
                             h4(gettext("Description of each cluster by quantitative variables and categories",domain="R-Factoshiny")),
                             # actionButton("download_cate_both", gettext("HTML",domain="R-Factoshiny")),
                             DT::dataTableOutput("tableau_df_both", width = "80%")
                             ),
                   tabPanel(
                        title = gettext("Links with the partition",domain="R-Factoshiny"),
                           h4(gettext("Link with the quantitative variables",domain="R-Factoshiny")),
                           # actionButton("download_tabquanti", gettext("HTML",domain="R-Factoshiny")),
                           DT::dataTableOutput("table_link_quanti"),
                           h4(gettext("Link with the qualitative variables",domain="R-Factoshiny")),
                           # actionButton("download_tabquali", gettext("HTML",domain="R-Factoshiny")),
                           DT::dataTableOutput("table_link_chisquare")
                     ),
                    tabPanel(gettext("Values",domain="R-Factoshiny"),
                             div(verbatimTextOutput("CodePrintedSummary")),
                             br(),
                             numericInput("pvalueDimdesc",gettext("P-value",domain="R-Factoshiny"),value=pvalueDimdescInit, min=0,max=1),
                             radioButtons("out",gettext("Which outputs do you want?",domain="R-Factoshiny"),
                                          choices=list(gettext("Description of partition and classes by the variables",domain="R-Factoshiny"),gettext("Description of classes by axes",domain="R-Factoshiny"),gettext("Parangons",domain="R-Factoshiny"),gettext("Specificity",domain="R-Factoshiny")),selected=gettext("Description of partition and classes by the variables",domain="R-Factoshiny"),inline=TRUE),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Description of partition and classes by the variables",domain="R-Factoshiny"),"'",sep=''),
                                verbatimTextOutput("printDescVar"),
                               ),
                             # conditionalPanel(
                               # condition=paste("input.out=='",gettext("Description of classes by quantitative variables",domain="R-Factoshiny"),"'",sep=''),
                               # div(align="center",tableOutput("descriptquanti"))
                               # ),
                             # conditionalPanel(
                               # condition=paste("input.out=='",gettext("Description of partition by qualitative variables",domain="R-Factoshiny"),"'",sep=''),
                               # div(align="center",tableOutput("descriptqualivar"))
                               # ),
                             # conditionalPanel(
                               # condition=paste("input.out=='",gettext("Description of classes by qualitative variables",domain="R-Factoshiny"),"'",sep=''),
                               # div(align="center",tableOutput("descriptquali"))
                               # ),
                             # conditionalPanel(
                               # condition=paste("input.out=='",gettext("Parangons",domain="R-Factoshiny"),"'",sep=''),
                               # div(align="center",tableOutput("parangons"))),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Parangons",domain="R-Factoshiny"),"'",sep=''),
                               verbatimTextOutput("printDescPara")),
                             # conditionalPanel(
                               # condition=paste("input.out=='",gettext("Specificity",domain="R-Factoshiny"),"'",sep=''),
                               # div(align="center",tableOutput("distind"))),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Specificity",domain="R-Factoshiny"),"'",sep=''),
                               verbatimTextOutput("printDescDist")),
                             # conditionalPanel(
                               # condition=paste("input.out=='",gettext("Description of classes by axes",domain="R-Factoshiny"),"'",sep=''),
                               # div(align="center",tableOutput("axes")))
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Description of classes by axes",domain="R-Factoshiny"),"'",sep=''),
                               verbatimTextOutput("printDescAxes"))
                             )
        )
      ,width=9)
    )
)
