# ui script for MCA2

fluidPage(
  titlePanel(div(paste(gettext("MCA on the dataset",domain="R-Factoshiny"),nomDataMCAshinycourt),style="color:#6E6E6E",align="center"),windowTitle="MCAshiny"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #FBEFEF; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
        tags$style(type='text/css', "#title2 { height: 25px; }"),
        tags$style(type='text/css', "#title3 { height: 25px; }"),
		tags$style(type="text/css", "#loadmessage { padding: 5px 0px 5px 0px; text-align: center; font-weight: bold; font-size: 100%; color: #000000; background-color: #ff8533; z-index: 105; }")
      ),
      wellPanel(
        div(align="center",checkboxInput("mcaparam",gettext("Show MCA parameters",domain="R-Factoshiny"),FALSE)),
        conditionalPanel(
          condition="input.mcaparam==true",
            selectizeInput("supvar",label=gettext("Select supplementary categorical variables",domain="R-Factoshiny"), choices=VariableChoicesMCAshiny, selected=supqualiMCAshiny,multiple=TRUE),
            if(length(QuantiChoiceMCAshiny)>=1) selectInput("supquanti",label=gettext("Select supplementary quantitative variables",domain="R-Factoshiny"),choices=QuantiChoiceMCAshiny,multiple=TRUE,selected=quantiMCAshiny),
            selectizeInput("indsup",gettext("Select supplementary individuals",domain="R-Factoshiny"),choices=nomMCAshiny, multiple=TRUE,selected=indsuplMCAshiny),
            uiOutput("imputeData"),
            actionButton("submit", label = gettext("Submit",domain="R-Factoshiny"))

          # if(is.null(supqualiMCAshiny)){
            # radioButtons("selecactive",label=h6(gettext("Choose the active qualitative variables",domain="R-Factoshiny")),
                         # choices=list(gettext("All"),gettext("Choose")),selected=gettext("All"))
          # }
          # else{
            # radioButtons("selecactive",label=h6(gettext("Choose the active qualitative variables")),
                         # choices=list(gettext("All"),gettext("Choose")),selected=gettext("Choose"))
          # },
          # conditionalPanel(
            # condition=paste("input.selecactive=='",gettext("Choose"),"'",sep=''),
            # selectInput("supvar",label=h6(gettext("Select the supplementary qualitative variables")),
                        # choices=list(IdChoices=VariableChoicesMCAshiny),
                        # selected=supqualiMCAshiny,multiple=TRUE)
          # ),
          
          # h6(gettext("Select the supplementary quantitative variables")),
          # if(length(QuantiChoiceMCAshiny)>1){
            # if(is.null(quantiSMCAshiny)){
            # selectInput("supquanti",label="",choices=list(Idquantisup=as.vector(QuantiChoiceMCAshiny)),multiple=TRUE)
          # }
          # else {
            # selectInput("supquanti",label="",choices=list(Idquantisup=as.vector(QuantiChoiceMCAshiny)),multiple=TRUE,selected=quantiSMCAshiny)
            
          # }
          # }
          # else if (length(QuantiChoiceMCAshiny)==1){
            # if(is.null(quantiSMCAshiny)){
            # checkboxInput("supquanti",QuantiChoiceMCAshiny,FALSE)
          # }
            # else{
            # checkboxInput("supquanti",QuantiChoiceMCAshiny,TRUE)  
            # }
          # }
          # else if(length(QuantiChoiceMCAshiny)==0){
            # p(gettext("No quantitative variable in your dataset"))
          # },
          
          
          # h6(gettext("Supplementary individuals")),
          # if(is.null(indsuplMCAshiny)){
            # selectInput("indsup","",choices=list(num=nomMCAshiny), multiple=TRUE)
          # }
          # else{
            # selectInput("indsup","",choices=list(num=nomMCAshiny), multiple=TRUE,selected=indsuplMCAshiny)
          # }
        ),
      style = "padding: 3px;background-color: #ffdbdb;"),
      
      #Prametres graphiques
      wellPanel(
        div(align="center",checkboxInput("graph",gettext("Graphical options",domain="R-Factoshiny"),FALSE)),
        conditionalPanel(
          condition="input.graph==true",
          div(gettext("Axes:",domain="R-Factoshiny"), style="display: inline-block;padding: 5px"),
          div(uiOutput("NB1"), style="display: inline-block;"),
          div(uiOutput("NB2"), style="display: inline-block;"),
          uiOutput("choixchange"),
          conditionalPanel(
            condition=paste("input.MCAgraph=='",gettext("Individuals and categories",domain="R-Factoshiny"),"'",sep=''),
            textInput("title1MCAshiny",gettext("Title of the graph: ",domain="R-Factoshiny"), title1MCAshiny),
            uiOutput("choixindvar"),
            uiOutput("pointlabel"),
            sliderInput("cex",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=1,step=0.05,ticks=FALSE),
              selectInput("select",label=gettext("Draw individuals according to:",domain="R-Factoshiny"),
                          choices=list(gettext("No selection",domain="R-Factoshiny"),"cos2"="cos2",gettext("Manual",domain="R-Factoshiny"),"Contribution"="Contrib"),selected=selectionMCAshiny),
             conditionalPanel(
              condition="input.select=='cos2'",
              if(selectionMCAshiny=="cos2"){
                div(align="center",sliderInput("slider1", label = "cos2", min = 0, max = 1, value =selection2MCAshiny,step=0.05))
              } else{
                div(align="center",sliderInput("slider1", label = "cos2", min = 0, max = 1, value =0,step=0.05))
              }),
              
            conditionalPanel(
              condition=paste("input.select=='",gettext("Manual",domain="R-Factoshiny"),"'",sep=''),
              if(selectionMCAshiny==gettext("Manual",domain="R-Factoshiny")){
                selectInput("indiv",label=gettext("Select individuals:",domain="R-Factoshiny"),  choices=nomMCAshiny,multiple=TRUE,selected=selection2MCAshiny) 
              } else{
                selectInput("indiv",label=gettext("Select individuals:",domain="R-Factoshiny"), choices=nomMCAshiny,multiple=TRUE)  
              }),
            conditionalPanel(
              condition="input.select=='Contrib'",
              if(selectionMCAshiny=="Contrib"){
                sliderInput("sliderContrib",label=gettext("Number of the most contributive individuals",domain="R-Factoshiny"),
                            min=1,max=length(nomMCAshiny),value=selection2MCAshiny,step=1)  
              } else{
                sliderInput("sliderContrib",label=gettext("Number of the most contributive individuals",domain="R-Factoshiny"),
                            min=1,max=length(nomMCAshiny),value=length(nomMCAshiny),step=1)
              }),
              selectInput("selectMod",label=gettext("Draw categories according to",domain="R-Factoshiny"),
                          choices=list(gettext("No selection",domain="R-Factoshiny"),"cos2"="cos2","Contribution"="Contrib"),selected=selection3MCAshiny),
            conditionalPanel(
              condition="input.selectMod=='cos2'",
              if(selection3MCAshiny=="cos2"){
                div(align="center",sliderInput("sliderCosMod", label = "cos2",
                                               min = 0, max = 1, value =as.numeric(selection4MCAshiny),step=0.05))}  
              else{
                div(align="center",sliderInput("sliderCosMod", label = "cos2",
                                               min = 0, max = 1, value=0,step=0.05))  
              }),
            conditionalPanel(
              condition="input.selectMod=='Contrib'",
              uiOutput("slider3")
            ),
			
          selectInput("color_point",label=gettext("Colour points according to:",domain="R-Factoshiny"),
                                          choices=list(gettext("active/supplementary",domain="R-Factoshiny"),"cos2"="cos2","contribution"="contribution",gettext("1 qualitative variable",domain="R-Factoshiny"),gettext("2 qualitative variables",domain="R-Factoshiny")),selected=color_pointInit),
            conditionalPanel(
              condition=paste0("input.color_point=='",gettext("active/supplementary",domain="R-Factoshiny"),"'"),
              div(colourpicker::colourInput("colindact", label=NULL, color1MCAshiny,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
              div(gettext("active individuals",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"),
              uiOutput("col1"),
              checkboxInput("eachvar",gettext("Colour each variable with different colour",domain="R-Factoshiny"),valdefMCAshiny)
			),
            conditionalPanel(
              condition=paste0("input.color_point!='",gettext("active/supplementary",domain="R-Factoshiny"),"'"),
              uiOutput("habillage2"),
			  uiOutput("ellips")
            )
		  ),
          conditionalPanel(
            condition="input.MCAgraph=='var'",
            textInput("title2MCAshiny",gettext("Title of the graph: ",domain="R-Factoshiny"), title2MCAshiny),
            sliderInput("cex2",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=1,step=0.05,ticks=FALSE),
            div(align="left",checkboxGroupInput("var_sup",gettext("Points to draw",domain="R-Factoshiny"),choices=list(gettext("Active qualitative variables",domain="R-Factoshiny"),gettext("Supplementary qualitative variables",domain="R-Factoshiny"),gettext("Supplementary quantitative variables",domain="R-Factoshiny")),selected=varsupMCAshiny)),
              div(colourpicker::colourInput("colvaract1", label=NULL, color5MCAshiny ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
		      div(gettext("active categories",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"),
            uiOutput("col3"),
            uiOutput("colquantib")
          ),
          conditionalPanel(
            condition=paste("input.MCAgraph=='",gettext("Quantitative variables",domain="R-Factoshiny"),"'",sep=''),
            textInput("title3MCAshiny",gettext("Title of the graph: ",domain="R-Factoshiny"), title3MCAshiny),
            sliderInput("cex3",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=2.5,value=1,step=0.05,ticks=FALSE),
            uiOutput("colquanti12"))
		), style = "padding: 3px;background-color: #fcefba"),
      wellPanel(
        div(align="center",checkboxInput("hcpcparam",gettext("Perform clustering after leaving MCA app?",domain="R-Factoshiny"),hcpcparaMCAshiny)),
        conditionalPanel(
          condition="input.hcpcparam==true",
          uiOutput("NbDimForClustering")
        ), align="center", style = "padding: 3px;background-color: #ecffdb"
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
        ), align="center", style = "padding: 3px;background-color: #dbe6ff"
      ),
      
      wellPanel(
        div(align="center",checkboxInput("MCAcode",gettext("Get the MCA code",domain="R-Factoshiny"),FALSE)),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;'
	  ),
      # div(align="center",actionButton("MCAcode", gettext("Get the MCA code",domain="R-Factoshiny"),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;')),
      div(align="center",actionButton("Quit", gettext("Quit the app",domain="R-Factoshiny"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
    mainPanel(
      tags$style(type = "text/css", "a{color: #B53977;}"),
      tabsetPanel(id = "graph_sort",
        tabPanel(gettext("Graphs",domain="R-Factoshiny"),
          div(verbatimTextOutput("CodePrinted")),
          fluidRow(
                 br(),
                 column(width = 6,shinyjqui::jqui_resizable(plotOutput("map", height="500")),
                 br(),
                 p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData1",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData2",gettext("pdf",domain="R-Factoshiny")),align="center"),
                 br(),align="center"),
                 column(width = 6,shinyjqui::jqui_resizable(plotOutput("map4", height="500")),
                 br(),
                 p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData10",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData0",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData20",gettext("pdf",domain="R-Factoshiny"))),
                 align="center")
			),
            br(),
            div(align = "center",uiOutput("map22", height="500"))
        ),
                  
                  tabPanel(gettext("Values",domain="R-Factoshiny"),
                             div(verbatimTextOutput("CodePrintedSummary")),
                           br(),
                           uiOutput("out22"),
                           br(), 
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Eigenvalues",domain="R-Factoshiny"),"'",sep=''),
                             div(align="center",shinyjqui::jqui_resizable(plotOutput("map3", height=300))),
                             div(align="center",tableOutput("sorties"))),
                           
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Results of the variables",domain="R-Factoshiny"),"'",sep=''),
                             h6(gettext("Coordinates",domain="R-Factoshiny")),
                             
                             div(align="center",tableOutput("sorties2")),
                             br(),
                             h6("Contributions"),
                             div(align="center",tableOutput("sorties3")),
                             br(),
                             h6("Cos2"),
                             div(align="center",tableOutput("sorties4"))),
                           #Mettre un if pour le nombre d'individus
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
                               condition=paste("input.out=='",gettext("Results of the supplementary individuals",domain="R-Factoshiny"),"'",sep=''),
                             h6(gettext("Coordinates",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sorties22s")),
                             br(),
                             h6("Cos2"),
                             div(align="center",tableOutput("sorties44s"))),

                           conditionalPanel(
                               condition=paste("input.out=='",gettext("Summary of outputs",domain="R-Factoshiny"),"'",sep=''),
                             numericInput("nbele",gettext("Number of elements to print",domain="R-Factoshiny"),value=10),
                             br(),
                             verbatimTextOutput("summaryMCA"),
                             p(downloadButton("summary2",gettext("Download the summary",domain="R-Factoshiny")),align="center")
                           ),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Results of the supplementary categorical variables",domain="R-Factoshiny"),"'",sep=''),
                             h6(gettext("Coordinates",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sorties23")),
                             br(),
                             h6("cos2"),
                             div(align="center",tableOutput("sorties232")),
                             br(),
                             h6("v.test"),
                             div(align="center",tableOutput("sorties233"))),
                           
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Results of the supplementary quantitative variables",domain="R-Factoshiny"),"'",sep=''),
                             h6(gettext("Coordinates",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sorties43"))),
                           
                           conditionalPanel(
                             condition="input.out=='Isup'",
                             h6(gettext("Coordinates",domain="R-Factoshiny")),
                             div(align="center",tableOutput("sortiesIsupC")),
                             br(),
                             h6("Cos2"),
                             div(align="center",tableOutput("sortiesIsupCos")))
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
                           verbatimTextOutput("summary"),
                           selectInput("bam",gettext("Graphs for ",domain="R-Factoshiny"),choices=list(IdChoices=VariableChoicesMCAshiny),multiple=FALSE),
                           
                           div(align = "center",plotOutput("histo", width = 500, height=500))),
                  tabPanel(gettext("Data",domain="R-Factoshiny"),
                           br(),
                           DT::dataTableOutput("JDD")
                  )                       
      )
    ,width=9)
  )
)
