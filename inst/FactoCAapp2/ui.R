# ui script for CA2

fluidPage(
  titlePanel(div(paste(gettext("CA on the dataset",domain="R-Factoshiny"),nomDataCAshinycourt),style="color:#2A0A29",align="center"),windowTitle="CAshiny"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #F0E6E6; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
		tags$style(type="text/css", "#loadmessage { padding: 5px 0px 5px 0px; text-align: center; font-weight: bold; font-size: 100%; color: #000000; background-color: #ff8533; z-index: 105; }")
      ),
      wellPanel(
        div(align="center",checkboxInput("caparam",gettext("Show CA parameters",domain="R-Factoshiny"),FALSE)),
        conditionalPanel(
          condition="input.caparam==true",
            selectizeInput("rowsupl",gettext("Select supplementary rows",domain="R-Factoshiny"),choices=nomCAshiny, multiple=TRUE,selected=lignesupCAshiny),
            selectizeInput("supvar",label=gettext("Select supplementary columns",domain="R-Factoshiny"), choices=VariableChoicesCAshiny, selected=colonnesupCAshiny,multiple=TRUE),
            if(length(QualiChoiceCAshiny)>=1) selectInput("supquali",label=gettext("Select supplementary categorical variables",domain="R-Factoshiny"),choices=QualiChoiceCAshiny,multiple=TRUE,selected=catsupCAshiny),
            selectizeInput("quantisupvar",label=gettext("Select supplementary quantitative variables",domain="R-Factoshiny"), choices=VariableChoicesCAshiny, selected=quantisupCAshiny,multiple=TRUE),
            uiOutput("imputeData"),
            actionButton("submit", label = gettext("Submit",domain="R-Factoshiny"))
        ),
      style = "padding: 3px;background-color: #ffdbdb;"),
      wellPanel(
        div(align="center",checkboxInput("graph",gettext("Graphical options",domain="R-Factoshiny"),FALSE)),
        conditionalPanel(
          condition="input.graph==true",
          div(gettext("Axes:",domain="R-Factoshiny"), style="display: inline-block;padding: 5px"),
          div(uiOutput("NB1"), style="display: inline-block;"),
          div(uiOutput("NB2"), style="display: inline-block;"),
          textInput("title1CAshiny",gettext("Title of the graph:",domain="R-Factoshiny"),title1CAshiny),
		  uiOutput("Titre2"),
          # textInput("title2CAshiny",gettext("Title of the graph for quantitative variables:",domain="R-Factoshiny"),title2CAshiny),
          sliderInput("cex",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=3.5,value=sizeCAshiny,step=0.05),
          uiOutput("choixinvis"),
          uiOutput("ellipsesCAshiny"),
          selectInput("color_point",label=gettext("Colour points according to:",domain="R-Factoshiny"),
                                          choices=listeChoixColourPoint,selected=color_pointInit),
            conditionalPanel(
              condition=paste0("input.color_point=='",gettext("row/column",domain="R-Factoshiny"),"'"),
                uiOutput("col1CAshiny"),
                uiOutput("col2CAshiny"),
                uiOutput("col3CAshiny"),
                uiOutput("col4CAshiny"),
                uiOutput("col5CAshiny")
		    ),
        uiOutput("habillage2"),
        selectInput("selecrow",gettext("Labels for rows selected by:",domain="R-Factoshiny"), choices=list(gettext("No selection",domain="R-Factoshiny"),"Cos2"="cos2","Contribution"="contrib"),selected=selec2CAshiny),
        conditionalPanel(
          condition="input.selecrow=='cos2'",
          if(selec2CAshiny=="cos2"){sliderInput("slider4",label=gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                                         min=0,max=1,value=valueselec2CAshiny,step=0.05)}
          else{sliderInput("slider4",label=gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                           min=0,max=1,value=0,step=0.05)}),
        conditionalPanel(
          condition="input.selecrow=='contrib'",
          uiOutput("contribrow")),
          selectInput("seleccol",gettext("Labels for columns selected by:",domain="R-Factoshiny"), choices=list(gettext("No selection",domain="R-Factoshiny"),"Cos2"="cos2","Contribution"="contrib"),selected=selec1CAshiny),
          conditionalPanel(
            condition="input.seleccol=='cos2'",
            if(selec1CAshiny=="cos2"){
              sliderInput("slider3",label=gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                          min=0,max=1,value=valueselec1CAshiny,step=0.05)
            }
            else{
              sliderInput("slider3",label=gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                          min=0,max=1,value=0,step=0.05) 
          }),
        conditionalPanel(
          condition="input.seleccol=='contrib'",
          uiOutput("contribcol"))
		  ),
      style = "padding: 3px;background-color: #fcefba"),
      wellPanel(
        div(align="center",checkboxInput("hcpcparam",gettext("Perform clustering after leaving CA app?",domain="R-Factoshiny"),hcpcparaCAshiny)),
        conditionalPanel(
          condition="input.hcpcparam==true",
		  # radioButtons("ellipseCA",gettext("Clustering on the",domain="R-Factoshiny"), choices=c(gettext("Rows",domain="R-Factoshiny"),gettext("Columns",domain="R-Factoshiny")), selected = gettext("Rows",domain="R-Factoshiny"), inline=TRUE),
          uiOutput("NbDimForClustering")
        ),
        align="center", style = "padding: 3px;background-color: #ecffdb"
      ),
      wellPanel(
        div(align="center",checkboxInput("reportparam",gettext("Automatic report",domain="R-Factoshiny"),FALSE)),
        conditionalPanel(
           condition="input.reportparam==true",
		  div(gettext("File name (without extension):",domain="R-Factoshiny")),
          textInput("titleFile",NULL, paste0(gettext("Report",domain="R-Factoshiny"),"_",Sys.Date()),width=200),
          if (strsplit(Sys.getlocale("LC_COLLATE"),"_")[[1]][1]!="French"){ radioButtons("choixLANG",gettext("Language",domain="R-Factoshiny"), choices=c(gettext("English",domain="R-Factoshiny"),gettext("French",domain="R-Factoshiny")), selected = gettext("English",domain="R-Factoshiny"), inline=TRUE)} else {radioButtons("choixLANG",gettext("Language",domain="R-Factoshiny"), choices=c(gettext("English",domain="R-Factoshiny"),gettext("French",domain="R-Factoshiny")), selected = gettext("French",domain="R-Factoshiny"), inline=TRUE)},
          radioButtons("choixGRAPH",gettext("Which graphs to use?",domain="R-Factoshiny"), choices=c(gettext("Suggested graphs",domain="R-Factoshiny"),gettext("Graphs done",domain="R-Factoshiny")), selected = gettext("Suggested graphs",domain="R-Factoshiny"), inline=TRUE),
		  div(actionButton("InvestigateRmd", "Rmd"),actionButton("Investigatehtml", "html"), actionButton("Investigatedoc", "doc")),
		  conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div(gettext("Ongoing reporting process...",domain="R-Factoshiny"),id="loadmessage"))
        ),
        align="center", style = "padding: 3px;background-color: #dbe6ff"
      ),
      wellPanel(
        div(align="center",checkboxInput("CAcode",gettext("Get the CA code",domain="R-Factoshiny"),FALSE)),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;'
	  ),
      # div(align="center",actionButton("CAcode", gettext("Get the CA code",domain="R-Factoshiny"),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;')),
      div(align="center",actionButton("Quit", gettext("Quit the app",domain="R-Factoshiny"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
      mainPanel(
        tags$style(type = "text/css", "a{color: #2F0B3A;}"),
        tabsetPanel(id = "graph_sort",
                    tabPanel(gettext("Graph",domain="R-Factoshiny"),
                             div(verbatimTextOutput("warn")),
                             div(verbatimTextOutput("CodePrinted")),
          fluidRow(
                 br(),
                 column(width = 7,shinyjqui::jqui_resizable(plotOutput("map",height=550)),
                             br(),
                             p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData1",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData2",gettext("pdf",domain="R-Factoshiny")),align="center"),
                             br()),
                 uiOutput("map22"),
							 br(),align="center")),
                    tabPanel(gettext("Values",domain="R-Factoshiny"),
                             div(verbatimTextOutput("CodePrintedSummary")),
                             br(),
                             uiOutput("out22"),
                             br(),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Eigenvalues",domain="R-Factoshiny"),"'",sep=''),
                               div(align="center",shinyjqui::jqui_resizable(plotOutput("map3", height="300"))),
                               div(align="center",tableOutput("sorties"))),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Summary of outputs",domain="R-Factoshiny"),"'",sep=''),
                               numericInput("nbele",h6(gettext("Number of elements to print",domain="R-Factoshiny")),value=10),
                               verbatimTextOutput("summaryCA"),
                               p(downloadButton("summary2",gettext("Download the summary",domain="R-Factoshiny")),align="center")
                               ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the columns",domain="R-Factoshiny"),"'",sep=''),
                               h6(gettext("Coordinates",domain="R-Factoshiny")),
                               div(align="center",tableOutput("sorties1")),
                               h6("cos2"),
                               div(align="center",tableOutput("sorties2")),
                               h6("Contributions"),
                               div(align="center",tableOutput("sorties3"))
                               ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the rows",domain="R-Factoshiny"),"'",sep=''),
                               h6(gettext("Coordinates",domain="R-Factoshiny")),
                               div(align="center",tableOutput("sorties4")),
                               h6("cos2"),
                               div(align="center",tableOutput("sorties5")),
                               h6("Contributions"),
                               div(align="center",tableOutput("sorties6"))
                             ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the supplementary rows",domain="R-Factoshiny"),"'",sep=''),
                               h6(gettext("Coordinates",domain="R-Factoshiny")),
                               div(align="center",tableOutput("sorties7")),
                               h6("cos2"),
                               div(align="center",tableOutput("sorties8"))
                             ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the supplementary columns",domain="R-Factoshiny"),"'",sep=''),
                               h6(gettext("Coordinates",domain="R-Factoshiny")),
                               div(align="center",tableOutput("sorties9")),
                               h6("cos2"),
                               div(align="center",tableOutput("sorties10"))
                             ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the categorical variables",domain="R-Factoshiny"),"'",sep=''),
                               div(align="center",tableOutput("sorties11"))
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
