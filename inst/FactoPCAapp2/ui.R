fluidPage(
  titlePanel(div(paste(gettext("PCA on the dataset"),nomDataPCAshinycourt),style="color:#6E6E6E",align="center"),windowTitle="PCAshiny"),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #EFFBFB; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
        tags$style(type='text/css', "#title2 { height: 25px; }"),
    	tags$style(type="text/css", "#loadmessage { padding: 5px 0px 5px 0px; text-align: center; font-weight: bold; font-size: 100%; color: #000000; background-color: #ff8533; z-index: 105; }")
      ),
      wellPanel(
        div(align="center",checkboxInput("pcaparam",gettext("PCA parameters"),FALSE)),
        conditionalPanel(
          condition="input.pcaparam==true",
            selectizeInput("supvar",label=gettext("Select supplementary quantitative variables"), choices=VariableChoicesPCAshiny, selected=quantisupPCAshiny,multiple=TRUE),
            if(length(QualiChoicePCAshiny)>=1) selectInput("supquali",label=gettext("Select supplementary categorical variables"),choices=QualiChoicePCAshiny,multiple=TRUE,selected=qualisupPCAshiny),
            selectizeInput("indsup",gettext("Select supplementary individuals"),choices=nomPCAshiny, multiple=TRUE,selected=indsuplPCAshiny),
            checkboxInput("nor",gettext("Scale data to unit value"),normePCAshiny),
            uiOutput("imputeData"),
            actionButton("submit", label = gettext("Submit"))
          ),
        style = "padding: 3px;background-color: #ffdbdb;"),
      wellPanel(
        div(align="center",checkboxInput("graph",gettext("Graphical options"),FALSE)),
        conditionalPanel(
          condition="input.graph==true",
          # div(align="center",radioButtons("graph_type",label=NULL,
                                          # choices=list(gettext("ggplot"),gettext("classic")),inline=TRUE)),
          div(gettext("Axes:"), style="display: inline-block;padding: 5px"),
          div(uiOutput("NB1"), style="display: inline-block;"),
          div(uiOutput("NB2"), style="display: inline-block;"),
		  br(),
          div(radioButtons("ind_var",label=gettext("Modify graph of"),
                                          choices=list(gettext("individuals"),gettext("variables")),selected=gettext("individuals"),inline=TRUE), style="display: inline-block;"),
          conditionalPanel(
            condition=paste0("input.ind_var=='",gettext("individuals"),"'"),
            textInput("title1",gettext("Graph title: "), titre1PCAshiny),
            uiOutput("choixindmod"),
            uiOutput("pointlabel"),
            sliderInput("cex",gettext("Size of labels"),min=0.5,max=2.5,value=sizePCAshiny,step=0.05,ticks=FALSE),
            selectInput("select",label=gettext("Labels for individuals selected by:"), choices=list(gettext("No selection"),"cos2"="cos2","Contribution"="contrib",gettext("Manual")),selected=selectionPCAshiny),
            conditionalPanel(
              condition="input.select=='cos2'",
              if(selectionPCAshiny=="cos2"){
                div(align="center",sliderInput("slider1", label = gettext("Labels for cos2 greater than"),
                                               min = 0, max = 1, value =as.numeric(selection2PCAshiny),step=0.05))}
              else{
                div(align="center",sliderInput("slider1", label = gettext("Labels for cos2 greater than"),
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
                selectInput("indiv",label=gettext("Select individuals:"),
                            choices=nomPCAshiny,multiple=TRUE,selected=selection2PCAshiny)
			  ),
          selectInput("color_point",label=gettext("Colour points according to:"),
                                          choices=list(gettext("active/supplementary"),"cos2"="cos2","contribution"="contribution",gettext("quantitative variable"),gettext("qualitative variable"),gettext("2 qualitative variables")),selected=color_pointInit),
            conditionalPanel(
              condition=paste0("input.color_point=='",gettext("active/supplementary"),"'"),
            div(colourpicker::colourInput("coloract", label=NULL, activeindPCAshiny,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
            div(gettext("active individuals"), style="display: inline-block;padding: 0px 0px 0px 10px"),
            uiOutput("colourn2"),
            uiOutput("colourn3")
            ),
            conditionalPanel(
              condition=paste0("input.color_point!='",gettext("active/supplementary"),"'"),
              uiOutput("habillage2"),
              uiOutput("ellipsesPCAshiny")
            ), style = "padding: 3px;background-color: #fdf4ce"
          ),
          conditionalPanel(
            condition=paste("input.ind_var=='",gettext("variables"),"'",sep=''),
            div(gettext("Graph title:"), style="display: inline-block;padding: 3px"),
            div(textInput("title2",label=NULL,titre2PCAshiny), style="display: inline-block;"),
            sliderInput("cex2",gettext("Size of labels"),min=0.5,max=2.5,value=size2PCAshiny,step=0.05,ticks=FALSE),
            selectInput("select0",label=gettext("Labels for variables selected by:"),
                        choices=list(gettext("No selection"),"cos2"="cos2","Contribution"="contrib"),selected=selection3PCAshiny),
            conditionalPanel(
              condition="input.select0=='contrib'",
              uiOutput("slider3")
            ),
            conditionalPanel(
              condition="input.select0=='cos2'",
                div(align="center",sliderInput("slider00", label = gettext("Labels for cos2 greater than"),
                                               min = 0, max = 1, value =as.numeric(selection4PCAshiny),step=0.05))
			),
		  selectInput("color_arrow",label=gettext("Colour variables according to:"),
                                          choices=list(gettext("active/supplementary"),"cos2"="cos2","contribution"="contribution"),selected=color_arrowInit),
            conditionalPanel(
              condition=paste0("input.color_arrow=='",gettext("active/supplementary"),"'"),
              div(colourpicker::colourInput("coloractvarPCAshiny", label=NULL, coloractvarPCAshiny,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
              div(gettext("active variables"), style="display: inline-block;padding: 0px 0px 0px 10px"),
              uiOutput("colourv2")
            ), style = "padding: 3px;background-color: #fbe89d"
          )			
        ),
        style = "padding: 3px;background-color: #fcefba"
	  ),
      wellPanel(
        div(align="center",checkboxInput("hcpcparam",gettext("Perform clustering after leaving PCA app?"),hcpcparaPCAshiny)),
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
      div(align="center",actionButton("PCAcode", gettext("Get the PCA code"),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;')),
      div(align="center",actionButton("Quit", gettext("Quit the app"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
    
    mainPanel(
      tabsetPanel(id = "graph_sort",
                  tabPanel(gettext("Graphs"),
                           fluidRow(
                             br(),
##                             column(width = 6,plotOutput("map2", width = "500", brush = brushOpts(id = "plot_brush_ind")),
                             # column(width = 7,plotOutput("map2", height = "500"),
                             column(width = 7,shinyjqui::jqui_resizable(plotOutput("map2", height = "500")),
                                    br(),
                                    p(gettext("Download as"),downloadButton("downloadData4",gettext("jpg")),downloadButton("downloadData3",gettext("png")),downloadButton("downloadData5",gettext("pdf")),align="center"),
                                    align="center"),
                             column(width = 5,shinyjqui::jqui_resizable(plotOutput("map", height = "500")),
                                    br(),
                                    p(gettext("Download as"),downloadButton("downloadData1",gettext("jpg")),downloadButton("downloadData",gettext("png")),downloadButton("downloadData2",gettext("pdf")),align="center"),
                                    align="center"))),
                  tabPanel(gettext("Values"),
                           br(),
                           uiOutput("out22"),
                           br(),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Eigenvalues"),"'",sep=''),
                             shinyjqui::jqui_resizable(plotOutput("map3", height="500")),
                             div(align="center",tableOutput("sorties"))
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
                           numericInput("pvalueDimdesc",gettext("P-value"),value=pvalueDimdescInit, min=0,max=1),
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
                           selectizeInput("bam",gettext("Graphs for"),choices=c(VariableChoicesPCAshiny),multiple=FALSE),
                           plotOutput("histo")),
                  
                  tabPanel(gettext("Data"),
                           br(),
                           dataTableOutput("JDD")
                  )
      )
      ,width=9)
  )
)