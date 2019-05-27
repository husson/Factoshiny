# ui script for CA2

fluidPage(
  titlePanel(div(paste(gettext("CA on the dataset"),nomDataCAshiny),style="color:#2A0A29",align="center"),windowTitle="CAshiny"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #F0E6E6; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }")
      ),
      wellPanel(
        div(align="center",checkboxInput("caparam",gettext("Show CA parameters"),FALSE)),
        conditionalPanel(
          condition="input.caparam==true",
          br(),
          if(is.null(colonnesupCAshiny)){
            radioButtons("selecactive",label=h6(gettext("Choose the active columns")),
                         choices=list(gettext("All"),gettext("Choose")),selected=gettext("All"))}
          else{
            radioButtons("selecactive",label=h6(gettext("Choose the active columns")),
                         choices=list(gettext("All"),gettext("Choose")),selected=gettext("Choose")) 
          },
          conditionalPanel(
            #        condition="input.selecactive=='choix'",
            condition=paste("input.selecactive=='",gettext("Choose"),"'",sep=''),
            if(is.null(colonnesupCAshiny)){
              selectInput("supvar",label=h6(gettext("Select the supplementary columns")),
                          choices=list(IdChoicesCAshiny=VariableChoicesCAshiny),
                          multiple=TRUE)}
            else{
              selectInput("supvar",label=h6(gettext("Select the supplementary columns")),
                          choices=list(IdChoicesCAshiny=VariableChoicesCAshiny),
                          multiple=TRUE,selected=colonnesupCAshiny)  
            }
          ),
          if(is.null(catsupCAshiny)){
            if(length(QualiChoiceCAshiny)>1){
              selectInput("supquali",label=h6(gettext("Select the supplementary categorical variables")),choices=list(IdqualisupCAshiny=as.vector(QualiChoiceCAshiny)),multiple=TRUE)
            }
            if (length(QualiChoiceCAshiny)==1){
              h6(gettext("Select the supplementary categorical variables"))
              checkboxInput("supquali",QualiChoiceCAshiny,FALSE)
            }}
          else{
            if(length(QualiChoiceCAshiny)>1){
              selectInput("supquali",label=h6(gettext("Select the supplementary categorical variables")),choices=list(IdqualisupCAshiny=as.vector(QualiChoiceCAshiny)),multiple=TRUE,selected=catsupCAshiny)
            }
            if (length(QualiChoiceCAshiny)==1){
              h6(gettext("Select the supplementary categorical variables"))
              checkboxInput("supquali",QualiChoiceCAshiny,TRUE)
            } 
          },
          br(),
          if(is.null(lignesupCAshiny)){
            selectInput("rowsupl",label=h6(gettext("Select the supplementary rows")),choices=list(numCAshiny=nomCAshiny),multiple=TRUE)}
          else{
            selectInput("rowsupl",label=h6(gettext("Select the supplementary rows")),choices=list(numCAshiny=nomCAshiny),multiple=TRUE,selected=lignesupCAshiny)
          }
        ),
      style = "padding: 3px;"),
      wellPanel(
        div(align="center",checkboxInput("graph",gettext("Show graphs options"),FALSE)),
        conditionalPanel(
          condition="input.graph==true",
          fluidRow(
            column(5,uiOutput("NB1")),
            column(5,uiOutput("NB2"))),
          hr(),
          textInput("title1CAshiny",h6(gettext("Title of the graph: ")),title1CAshiny),
          if(is.null(InvisibleCAshiny)){
            #          selectInput("invis",h6(gettext("Invisible elements")),choices=list("Rows"="row","Columns"="col","Supplementary rows"="row.sup","Supplementary columns"="col.sup","Supplementary qualitative variable"="quali.sup"),multiple=TRUE)}
            selectInput("invis",h6(gettext("Invisible elements")),choices=list(gettext("Rows"),gettext("Columns"),gettext("Supplementary rows"),gettext("Supplementary columns"),gettext("Supplementary qualitative variables")),multiple=TRUE)}
          else{
            #          selectInput("invis",h6(gettext("Invisible elements")),choices=list("Rows"="row","Columns"="col","Supplementary rows"="row.sup","Supplementary columns"="col.sup","Supplementary qualitative variable"="quali.sup"),multiple=TRUE,selected=InvisibleCAshiny)
            selectInput("invis",h6(gettext("Invisible elements")),choices=list(gettext("Rows"),gettext("Columns"),gettext("Supplementary rows"),gettext("Supplementary columns"),gettext("Supplementary qualitative variables")),multiple=TRUE,selected=InvisibleCAshiny)
          },
          br(),
          sliderInput("cex",h6(gettext("Size of labels")),min=0.5,max=1.5,value=sizeCAshiny,step=0.05),
          br(),
          uiOutput("col1CAshiny"),
          uiOutput("col2CAshiny"),
          uiOutput("col3CAshiny"),
          uiOutput("col4CAshiny"),
          br(),
          if(is.null(ellipsesCAshiny)){
            div(align="left",checkboxInput("el",h6(gettext("Draw confidence ellipses")),FALSE))
          }else{
          div(align="left",checkboxInput("el",h6(gettext("Draw confidence ellipses")),TRUE))},
          conditionalPanel(
            condition='input.el==true',
            uiOutput("ellipsesCAshiny")
          ),
          hr(),
          radioButtons("seleccol",h6(gettext("Draw columns according to:")), choices=list(gettext("No selection"),"Cos2"="cos2","Contribution"="contrib"),selected=selec1CAshiny,inline=TRUE),
          conditionalPanel(
            condition="input.seleccol=='cos2'",
            if(selec1CAshiny=="cos2"){
              sliderInput("slider3",label=h6(gettext("Draw columns that have a cos2 greater than:")),
                          min=0,max=1,value=valueselec1CAshiny,step=0.05)
            }
            else{
              sliderInput("slider3",label=h6(gettext("Draw columns that have a cos2 greater than:")),
                          min=0,max=1,value=0,step=0.05) 
          }),
        conditionalPanel(
          condition="input.seleccol=='contrib'",
          uiOutput("contribcol")),
        br(),
        radioButtons("selecrow",h6(gettext("Draw rows according to:")), choices=list(gettext("No selection"),"Cos2"="cos2","Contribution"="contrib"),selected=selec2CAshiny,inline=TRUE),
        conditionalPanel(
          condition="input.selecrow=='cos2'",
          if(selec2CAshiny=="cos2"){sliderInput("slider4",label=h6(gettext("Draw rows that have a cos2 greater than:")),
                                         min=0,max=1,value=valueselec2CAshiny,step=0.05)}
          else{sliderInput("slider4",label=h6(gettext("Draw rows that have a cos2 greater than:")),
                           min=0,max=1,value=0,step=0.05)}),
        conditionalPanel(
          condition="input.selecrow=='contrib'",
          uiOutput("contribrow"))
      ),
      style = "padding: 3px;"),
      wellPanel(
        div(align="center",checkboxInput("hcpcparam",gettext("Perform clustering after leaving CA app?"),hcpcparaCAshiny)),
        conditionalPanel(
          condition="input.hcpcparam==true",
          uiOutput("NbDimForClustering")
        ),
        align="center", style = "padding: 3px;"
      ),
      wellPanel(
        div(align="center",checkboxInput("reportparam",gettext("Automatic report"),FALSE)),
        conditionalPanel(
          condition="input.reportparam==true",
          textInput("titleFile",h6(gettext("File name (without extension):")), gettext("Report")),
          radioButtons("choixLANG",gettext("Language"), choices=c(gettext("English"),gettext("French")), selected = gettext("English"), inline=TRUE),
          div(actionButton("InvestigateRmd", "Rmd"), actionButton("Investigatehtml", "html"), actionButton("Investigatedoc", "doc")),
          paste(gettext("The file will be saved in the directory"),pathsaveCAshiny)
        ),
        align="center", style = "padding: 3px;"
      ),
    div(align="center",actionButton("CAcode", gettext("Get the CA code"))),
    div(align="center",actionButton("Quit", gettext("Quit the app")))
    ,width=3),  
      mainPanel(
        tags$style(type = "text/css", "a{color: #2F0B3A;}"),
        tabsetPanel(id = "graph_sort",
                    tabPanel(gettext("Graphs"),
                             br(),
                             div(verbatimTextOutput("warn")),
                             br(),
                             div(align="center",plotOutput("map",width=550,height=550)),
                             br(),
                             p(gettext("Download as"),downloadButton("downloadData1",gettext("jpg")),downloadButton("downloadData",gettext("png")),downloadButton("downloadData2",gettext("pdf")),align="center"),
                             br(),align="center"),
                             # conditionalPanel(
                               # condition="input.paramdown=='jpg'",
                               # p(downloadButton("downloadData1",gettext("Download as jpg")),align="center")),
                             # conditionalPanel(
                               # condition="input.paramdown=='png'",
                               # p(downloadButton("downloadData",gettext("Download as png")),align="center")),
                             # conditionalPanel(
                               # condition="input.paramdown=='pdf'",
                               # p(downloadButton("downloadData2",gettext("Download as pdf")),align="center"))),
                    tabPanel(gettext("Values"),
                             br(),
                             uiOutput("out22"),
                             br(),
                             conditionalPanel(
#                               condition="input.out=='eig'",
                               condition=paste("input.out=='",gettext("Eigenvalues"),"'",sep=''),
                               div(align="center",tableOutput("sorties")),
                               div(align="center",plotOutput("map3", width = "700", height="500"))),
                             conditionalPanel(
#                               condition="input.out=='CA'",
                               condition=paste("input.out=='",gettext("Summary of outputs"),"'",sep=''),
                               numericInput("nbele",h6(gettext("Number of elements to print")),value=10),
                               verbatimTextOutput("summaryCA"),
                               p(downloadButton("summary2",gettext("Download the summary")),align="center")
                               ),
                             conditionalPanel(
#                               condition="input.out=='var'",
                               condition=paste("input.out=='",gettext("Results for the columns"),"'",sep=''),
                               h6(gettext("Coordinates")),
                               div(align="center",tableOutput("sorties1")),
                               h6("cos2"),
                               div(align="center",tableOutput("sorties2")),
                               h6("Contributions"),
                               div(align="center",tableOutput("sorties3"))
                               ),
                             conditionalPanel(
#                               condition="input.out=='ind'",
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
#                               condition="input.out=='suprow'",
                               h6(gettext("Coordinates")),
                               div(align="center",tableOutput("sorties7")),
                               h6("cos2"),
                               div(align="center",tableOutput("sorties8"))
                             ),
                             conditionalPanel(
#                               condition="input.out=='supcol'",
                               condition=paste("input.out=='",gettext("Results for the supplementary columns"),"'",sep=''),
                               h6(gettext("Coordinates")),
                               div(align="center",tableOutput("sorties9")),
                               h6("cos2"),
                               div(align="center",tableOutput("sorties10"))
                             ),
                             conditionalPanel(
#                               condition="input.out=='qualico'",
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
