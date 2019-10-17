# ui script for CA2

fluidPage(
  titlePanel(div(paste(gettext("CA on the dataset"),nomDataCAshinycourt),style="color:#2A0A29",align="center"),windowTitle="CAshiny"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #F0E6E6; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
		tags$style(type="text/css", "#loadmessage { padding: 5px 0px 5px 0px; text-align: center; font-weight: bold; font-size: 100%; color: #000000; background-color: #ff8533; z-index: 105; }")
      ),
      wellPanel(
        div(align="center",checkboxInput("caparam",gettext("Show CA parameters"),FALSE)),
        conditionalPanel(
          condition="input.caparam==true",
            selectizeInput("rowsupl",gettext("Select supplementary rows"),choices=nomCAshiny, multiple=TRUE,selected=lignesupCAshiny),
            selectizeInput("supvar",label=gettext("Select supplementary columns"), choices=VariableChoicesCAshiny, selected=colonnesupCAshiny,multiple=TRUE),
            if(length(QualiChoiceCAshiny)>=1) selectInput("supquali",label=gettext("Select supplementary categorical variables"),choices=QualiChoiceCAshiny,multiple=TRUE,selected=catsupCAshiny),
            selectizeInput("quantisupvar",label=gettext("Select supplementary quantitative variables"), choices=VariableChoicesCAshiny, selected=quantisupCAshiny,multiple=TRUE)
        ),
      style = "padding: 3px;background-color: #ffdbdb;"),
      wellPanel(
        div(align="center",checkboxInput("graph",gettext("Graphical options"),FALSE)),
        conditionalPanel(
          condition="input.graph==true",
          div(gettext("Axes:"), style="display: inline-block;padding: 5px"),
          div(uiOutput("NB1"), style="display: inline-block;"),
          div(uiOutput("NB2"), style="display: inline-block;"),
          textInput("title1CAshiny",gettext("Title of the graph:"),title1CAshiny),
          sliderInput("cex",gettext("Size of labels"),min=0.5,max=3.5,value=sizeCAshiny,step=0.05),
          uiOutput("choixinvis"),
          uiOutput("ellipsesCAshiny"),
          selectInput("color_point",label=gettext("Colour points according to:"),
                                          choices=listeChoixColourPoint,selected=color_pointInit),
            conditionalPanel(
              condition=paste0("input.color_point=='",gettext("row/column"),"'"),
                uiOutput("col1CAshiny"),
                uiOutput("col2CAshiny"),
                uiOutput("col3CAshiny"),
                uiOutput("col4CAshiny"),
                uiOutput("col5CAshiny")
		    ),
        selectInput("selecrow",gettext("Labels for rows selected by:"), choices=list(gettext("No selection"),"Cos2"="cos2","Contribution"="contrib"),selected=selec2CAshiny),
        conditionalPanel(
          condition="input.selecrow=='cos2'",
          if(selec2CAshiny=="cos2"){sliderInput("slider4",label=gettext("Labels for cos2 greater than:"),
                                         min=0,max=1,value=valueselec2CAshiny,step=0.05)}
          else{sliderInput("slider4",label=gettext("Labels for cos2 greater than:"),
                           min=0,max=1,value=0,step=0.05)}),
        conditionalPanel(
          condition="input.selecrow=='contrib'",
          uiOutput("contribrow")),
          selectInput("seleccol",gettext("Labels for columns selected by:"), choices=list(gettext("No selection"),"Cos2"="cos2","Contribution"="contrib"),selected=selec1CAshiny),
          conditionalPanel(
            condition="input.seleccol=='cos2'",
            if(selec1CAshiny=="cos2"){
              sliderInput("slider3",label=gettext("Labels for cos2 greater than:"),
                          min=0,max=1,value=valueselec1CAshiny,step=0.05)
            }
            else{
              sliderInput("slider3",label=gettext("Labels for cos2 greater than:"),
                          min=0,max=1,value=0,step=0.05) 
          }),
        conditionalPanel(
          condition="input.seleccol=='contrib'",
          uiOutput("contribcol"))
		  ),
      style = "padding: 3px;background-color: #fcefba"),
      wellPanel(
        div(align="center",checkboxInput("hcpcparam",gettext("Perform clustering after leaving CA app?"),hcpcparaCAshiny)),
        conditionalPanel(
          condition="input.hcpcparam==true",
          uiOutput("NbDimForClustering")
        ),
        align="center", style = "padding: 3px;background-color: #ecffdb"
      ),
      wellPanel(
        div(align="center",checkboxInput("reportparam",gettext("Automatic report"),FALSE)),
        conditionalPanel(
           condition="input.reportparam==true",
		  div(gettext("File name (without extension):")),
          textInput("titleFile",NULL, paste0(gettext("Report"),"_",Sys.Date()),width=200),
          if (strsplit(Sys.getlocale("LC_COLLATE"),"_")[[1]][1]!="French"){ radioButtons("choixLANG",gettext("Language"), choices=c(gettext("English"),gettext("French")), selected = gettext("English"), inline=TRUE)} else {radioButtons("choixLANG",gettext("Language"), choices=c(gettext("English"),gettext("French")), selected = gettext("French"), inline=TRUE)},
          radioButtons("choixGRAPH",gettext("Which graphs to use?"), choices=c(gettext("Suggested graphs"),gettext("Graphs done")), selected = gettext("Suggested graphs"), inline=TRUE),
		  div(actionButton("InvestigateRmd", "Rmd"),actionButton("Investigatehtml", "html"), actionButton("Investigatedoc", "doc")),
		  conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div(gettext("Ongoing reporting process..."),id="loadmessage"))
        ),
        align="center", style = "padding: 3px;background-color: #dbe6ff"
      ),
      div(align="center",actionButton("CAcode", gettext("Get the CA code"),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;')),
      div(align="center",actionButton("Quit", gettext("Quit the app"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
      mainPanel(
        tags$style(type = "text/css", "a{color: #2F0B3A;}"),
        tabsetPanel(id = "graph_sort",
                    tabPanel(gettext("Graph"),
                             br(),
                             div(verbatimTextOutput("warn")),
                             br(),
          fluidRow(
                 br(),
                 column(width = 7,shinyjqui::jqui_resizable(plotOutput("map",height=550)),
                             br(),
                             p(gettext("Download as"),downloadButton("downloadData1",gettext("jpg")),downloadButton("downloadData",gettext("png")),downloadButton("downloadData2",gettext("pdf")),align="center"),
                             br()),
                 uiOutput("map22"),
							 br(),align="center")),
                    tabPanel(gettext("Values"),
                             br(),
                             uiOutput("out22"),
                             br(),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Eigenvalues"),"'",sep=''),
                               div(align="center",shinyjqui::jqui_resizable(plotOutput("map3", height="500"))),
                               div(align="center",tableOutput("sorties"))),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Summary of outputs"),"'",sep=''),
                               numericInput("nbele",h6(gettext("Number of elements to print")),value=10),
                               verbatimTextOutput("summaryCA"),
                               p(downloadButton("summary2",gettext("Download the summary")),align="center")
                               ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the columns"),"'",sep=''),
                               h6(gettext("Coordinates")),
                               div(align="center",tableOutput("sorties1")),
                               h6("cos2"),
                               div(align="center",tableOutput("sorties2")),
                               h6("Contributions"),
                               div(align="center",tableOutput("sorties3"))
                               ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the rows"),"'",sep=''),
                               h6(gettext("Coordinates")),
                               div(align="center",tableOutput("sorties4")),
                               h6("cos2"),
                               div(align="center",tableOutput("sorties5")),
                               h6("Contributions"),
                               div(align="center",tableOutput("sorties6"))
                             ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the supplementary rows"),"'",sep=''),
                               h6(gettext("Coordinates")),
                               div(align="center",tableOutput("sorties7")),
                               h6("cos2"),
                               div(align="center",tableOutput("sorties8"))
                             ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the supplementary columns"),"'",sep=''),
                               h6(gettext("Coordinates")),
                               div(align="center",tableOutput("sorties9")),
                               h6("cos2"),
                               div(align="center",tableOutput("sorties10"))
                             ),
                             conditionalPanel(
                               condition=paste("input.out=='",gettext("Results for the categorical variables"),"'",sep=''),
                               div(align="center",tableOutput("sorties11"))
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
