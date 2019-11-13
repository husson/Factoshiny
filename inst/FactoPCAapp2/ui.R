fluidPage(
  titlePanel(div(paste(gettext("PCA on the dataset",domain="R-Factoshiny"),nomDataPCAshinycourt),style="color:#6E6E6E",align="center"),windowTitle="PCAshiny"),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #EFFBFB; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
        tags$style(type='text/css', "#title2 { height: 25px; }"),
    	tags$style(type="text/css", "#loadmessage { padding: 5px 0px 5px 0px; text-align: center; font-weight: bold; font-size: 100%; color: #000000; background-color: #ff8533; z-index: 105; }")
      ),
      wellPanel(
        div(align="center",checkboxInput("pcaparam",gettext("PCA parameters",domain="R-Factoshiny"),FALSE)),
        conditionalPanel(
          condition="input.pcaparam==true",
            selectizeInput("supvar",label=gettext("Select supplementary quantitative variables",domain="R-Factoshiny"), choices=VariableChoicesPCAshiny, selected=quantisupPCAshiny,multiple=TRUE),
            if(length(QualiChoicePCAshiny)>=1) selectInput("supquali",label=gettext("Select supplementary categorical variables",domain="R-Factoshiny"),choices=QualiChoicePCAshiny,multiple=TRUE,selected=qualisupPCAshiny),
            selectizeInput("indsup",gettext("Select supplementary individuals",domain="R-Factoshiny"),choices=nomPCAshiny, multiple=TRUE,selected=indsuplPCAshiny),
            checkboxInput("nor",gettext("Scale data to unit value",domain="R-Factoshiny"),normePCAshiny),
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
		  br(),
          div(radioButtons("ind_var",label=gettext("Modify graph of",domain="R-Factoshiny"),
                                          choices=list(gettext("individuals",domain="R-Factoshiny"),gettext("variables",domain="R-Factoshiny")),selected=gettext("individuals",domain="R-Factoshiny"),inline=TRUE), style="display: inline-block;"),
          conditionalPanel(
            condition=paste0("input.ind_var=='",gettext("individuals",domain="R-Factoshiny"),"'"),
            textInput("title1",gettext("Graph title: ",domain="R-Factoshiny"), titre1PCAshiny),
            uiOutput("choixindmod"),
            uiOutput("pointlabel"),
            sliderInput("cex",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=sizePCAshiny,step=0.05,ticks=FALSE),
            selectInput("select",label=gettext("Labels for individuals selected by:",domain="R-Factoshiny"), choices=list(gettext("No selection",domain="R-Factoshiny"),"cos2"="cos2","Contribution"="contrib",gettext("Manual",domain="R-Factoshiny")),selected=selectionPCAshiny),
            conditionalPanel(
              condition="input.select=='cos2'",
              if(selectionPCAshiny=="cos2"){
                div(align="center",sliderInput("slider1", label = gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                                               min = 0, max = 1, value =as.numeric(selection2PCAshiny),step=0.05))}
              else{
                div(align="center",sliderInput("slider1", label = gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                                               min = 0, max = 1, value =0,step=0.05))
              }),
			  
            conditionalPanel(
              condition="input.select=='contrib'",
              if(selectionPCAshiny=="contrib"){
                div(align="center",sliderInput("slider0", label = gettext("Number of the most contributive individuals",domain="R-Factoshiny"),
                                               min = 1, max = length(nomPCAshiny), value =as.numeric(selection2PCAshiny),step=1))}
              else{
                div(align="center",sliderInput("slider0", label = gettext("Number of the most contributive individuals",domain="R-Factoshiny"),
                                               min = 1, max = length(nomPCAshiny), value =length(nomPCAshiny),step=1)) 
              }),
            conditionalPanel(
              condition=paste("input.select=='",gettext("Manual",domain="R-Factoshiny"),"'",sep=''),
                selectInput("indiv",label=gettext("Select individuals:",domain="R-Factoshiny"),
                            choices=nomPCAshiny,multiple=TRUE,selected=selection2PCAshiny)
			  ),
          selectInput("color_point",label=gettext("Colour points according to:",domain="R-Factoshiny"),
                                          choices=list(gettext("active/supplementary",domain="R-Factoshiny"),"cos2"="cos2","contribution"="contribution",gettext("quantitative variable",domain="R-Factoshiny"),gettext("qualitative variable",domain="R-Factoshiny"),gettext("2 qualitative variables",domain="R-Factoshiny")),selected=color_pointInit),
            conditionalPanel(
              condition=paste0("input.color_point=='",gettext("active/supplementary",domain="R-Factoshiny"),"'"),
            div(colourpicker::colourInput("coloract", label=NULL, activeindPCAshiny,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
            div(gettext("active individuals",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"),
            uiOutput("colourn2"),
            uiOutput("colourn3")
            ),
            conditionalPanel(
              condition=paste0("input.color_point!='",gettext("active/supplementary",domain="R-Factoshiny"),"'"),
              uiOutput("habillage2"),
              uiOutput("ellipsesPCAshiny")
            ), style = "padding: 3px;background-color: #fdf4ce"
          ),
          conditionalPanel(
            condition=paste("input.ind_var=='",gettext("variables",domain="R-Factoshiny"),"'",sep=''),
            div(gettext("Graph title:",domain="R-Factoshiny"), style="display: inline-block;padding: 3px"),
            div(textInput("title2",label=NULL,titre2PCAshiny), style="display: inline-block;"),
            sliderInput("cex2",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=size2PCAshiny,step=0.05,ticks=FALSE),
            selectInput("select0",label=gettext("Labels for variables selected by:",domain="R-Factoshiny"),
                        choices=list(gettext("No selection",domain="R-Factoshiny"),"cos2"="cos2","Contribution"="contrib"),selected=selection3PCAshiny),
            conditionalPanel(
              condition="input.select0=='contrib'",
              uiOutput("slider3")
            ),
            conditionalPanel(
              condition="input.select0=='cos2'",
                div(align="center",sliderInput("slider00", label = gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                                               min = 0, max = 1, value =as.numeric(selection4PCAshiny),step=0.05))
			),
		  selectInput("color_arrow",label=gettext("Colour variables according to:",domain="R-Factoshiny"),
                                          choices=list(gettext("active/supplementary",domain="R-Factoshiny"),"cos2"="cos2","contribution"="contribution"),selected=color_arrowInit),
            conditionalPanel(
              condition=paste0("input.color_arrow=='",gettext("active/supplementary",domain="R-Factoshiny"),"'"),
              div(colourpicker::colourInput("coloractvarPCAshiny", label=NULL, coloractvarPCAshiny,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
              div(gettext("active variables",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"),
              uiOutput("colourv2")
            ), style = "padding: 3px;background-color: #fbe89d"
          )			
        ),
        style = "padding: 3px;background-color: #fcefba"
	  ),
      wellPanel(
        div(align="center",checkboxInput("hcpcparam",gettext("Perform clustering after leaving PCA app?",domain="R-Factoshiny"),hcpcparaPCAshiny)),
        conditionalPanel(
          condition="input.hcpcparam==true",
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
      div(align="center",actionButton("PCAcode", gettext("Get the PCA code",domain="R-Factoshiny"),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;')),
      div(align="center",actionButton("Quit", gettext("Quit the app",domain="R-Factoshiny"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
    
    mainPanel(
      tabsetPanel(id = "graph_sort",
                  tabPanel(gettext("Graphs",domain="R-Factoshiny"),
                           fluidRow(
                             br(),
##                             column(width = 6,plotOutput("map2", width = "500", brush = brushOpts(id = "plot_brush_ind")),
                             # column(width = 7,plotOutput("map2", height = "500"),
                             column(width = 7,shinyjqui::jqui_resizable(plotOutput("map2", height = "500")),
                                    br(),
                                    p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData4",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData3",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData5",gettext("pdf",domain="R-Factoshiny")),align="center"),
                                    align="center"),
                             column(width = 5,shinyjqui::jqui_resizable(plotOutput("map", height = "500")),
                                    br(),
                                    p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData1",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData2",gettext("pdf",domain="R-Factoshiny")),align="center"),
                                    align="center"))),
                  tabPanel(gettext("Values",domain="R-Factoshiny"),
                           br(),
                           uiOutput("out22"),
                           br(),
                           conditionalPanel(
                             condition=paste0("input.out=='",gettext("Summary of outputs",domain="R-Factoshiny"),"'"),
                             numericInput("nbele",gettext("Number of elements to print",domain="R-Factoshiny"),value=10),
                             verbatimTextOutput("summaryPCA"),
                             p(downloadButton("summary2",gettext("Download the summary",domain="R-Factoshiny")),align="center")
                           ),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Eigenvalues",domain="R-Factoshiny"),"'",sep=''),
                             shinyjqui::jqui_resizable(plotOutput("map3", height="500")),
                             div(align="center",tableOutput("sorties"))
                           ),
                           conditionalPanel(
                             condition=paste0("input.out=='",gettext("Results of the variables",domain="R-Factoshiny"),"'"),
                             h6(gettext("Coordinates",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sorties2")),
                             br(),
                             h6("Contributions"),
                             div(align="center",tableOutput("sorties3")),
                             br(),
                             h6("Cos2"),
                             div(align="center",tableOutput("sorties4"))),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Results of the individuals",domain="R-Factoshiny"),"'",sep=''),
                             h6(gettext("Coordinates",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sorties22")),
                             br(),
                             h6("Contributions"),
                             div(align="center",tableOutput("sorties33")),
                             br(),
                             h6("Cos2"),
                             div(align="center",tableOutput("sorties44"))),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Results of the supplementary variables",domain="R-Factoshiny"),"'",sep=''),
                             h6(gettext("Coordinates",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sorties23")),
                             h6("Correlations"),
                             div(align="center",tableOutput("sorties32"))
                           ),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Results of the categorical variables",domain="R-Factoshiny"),"'",sep=''),
                             h6(gettext("Coordinates",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sorties12")),
                             h6("V-test"),
                             div(align="center",tableOutput("sorties13"))
                           ),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Results of the supplementary individuals",domain="R-Factoshiny"),"'",sep=''),
                             h6(gettext("Coordinates",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sorties36")),
                             h6("Cos2"),
                             div(align="center",tableOutput("sorties37"))
                           )
                  ),
                  tabPanel(gettext("Automatic description of axes",domain="R-Factoshiny"),
                           br(),
                           numericInput("pvalueDimdesc",gettext("P-value",domain="R-Factoshiny"),value=pvalueDimdescInit, min=0,max=1),
                           radioButtons("Dim",label=gettext("Choose the dimensions",domain="R-Factoshiny"),choices=list("Dimension 1"="Dim1","Dimension 2"="Dim2","Dimension 3"="Dim3"),selected="Dim1"),
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
                  tabPanel(gettext("Summary of dataset",domain="R-Factoshiny"),
                           br(),
                           verbatimTextOutput("summary"),
                           br(),
                           selectizeInput("bam",gettext("Graphs for",domain="R-Factoshiny"),choices=c(VariableChoicesPCAshiny),multiple=FALSE),
                           plotOutput("histo")),
                  
                  tabPanel(gettext("Data",domain="R-Factoshiny"),
                           br(),
                           DT::dataTableOutput("JDD")
                  )
      )
      ,width=9)
  )
)