# ui script for MCA2

shinyUI(fluidPage(
  titlePanel(div(paste(gettext("MCA on the dataset"),nomData),style="color:#6E6E6E",align="center"),windowTitle="MCAshiny"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #FBEFEF; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
        tags$style(type='text/css', "#title2 { height: 25px; }"),
        tags$style(type='text/css', "#title3 { height: 25px; }")
      ),
      wellPanel(
        div(align="center",checkboxInput("mcaparam",gettext("Show MCA parameters"),FALSE)),
        conditionalPanel(
          condition="input.mcaparam==true",
          if(is.null(supquali)){
            radioButtons("selecactive",label=h6(gettext("Choose the active qualitative variables")),
                         choices=list(gettext("All"),gettext("Choose")),selected=gettext("All"))
          }
          else{
            radioButtons("selecactive",label=h6(gettext("Choose the active qualitative variables")),
                         choices=list(gettext("All"),gettext("Choose")),selected=gettext("Choose"))
          },
          conditionalPanel(
#            condition="input.selecactive=='choix'",
            condition=paste("input.selecactive=='",gettext("Choose"),"'",sep=''),
            selectInput("supvar",label=h6(gettext("Select the supplementary qualitative variables")),
                        choices=list(IdChoices=VariableChoices),
                        selected=supquali,multiple=TRUE)
          ),
          
          #Selection des variables quantitatives supplementaires
          h6(gettext("Select the supplementary quantitative variables")),
          if(length(QuantiChoice)>1){
            if(is.null(quantiS)){
            selectInput("supquanti",label="",choices=list(Idquantisup=as.vector(QuantiChoice)),multiple=TRUE)
          }
          else {
            selectInput("supquanti",label="",choices=list(Idquantisup=as.vector(QuantiChoice)),multiple=TRUE,selected=quantiS)
            
          }
          }
          else if (length(QuantiChoice)==1){
            if(is.null(quantiS)){
            checkboxInput("supquanti",QuantiChoice,FALSE)
          }
            else{
            checkboxInput("supquanti",QuantiChoice,TRUE)  
            }
          }
          else if(length(QuantiChoice)==0){
            p(gettext("No quantitative variable in your dataset"))
          },
          
          
          h6(gettext("Supplementary individuals")),
          if(is.null(indsupl)){
            selectInput("indsup","",choices=list(num=nom), multiple=TRUE)
          }
          else{
            selectInput("indsup","",choices=list(num=nom), multiple=TRUE,selected=indsupl)
          }
        )
      ),
      
      #Prametres graphiques
      wellPanel(
        div(align="center",checkboxInput("graph",gettext("Show graphs options"),FALSE)),
        conditionalPanel(
          condition="input.graph==true",
          
          #
          fluidRow(
            column(5,uiOutput("NB1")),
            column(5,uiOutput("NB2"))),
          hr(),
          uiOutput("choixchange"),
          hr(),
          conditionalPanel(
            condition=paste("input.MCAgraph=='",gettext("Individuals and categories"),"'",sep=''),
#            condition="input.MCAgraph=='ind'",
            p(gettext("Graph of individuals and categories"),align="center"),
            uiOutput("choixindvar"),
            br(),
           # p("Draw labels for :",align="center"),
          #  uiOutput("pointlabel"),
            textInput("title1",h6(gettext("Title of the graph: ")), title1),
            sliderInput("cex",h6(gettext("Size of labels")),min=0.5,max=2.5,value=1,step=0.05,ticks=FALSE),
            div(align="center",radioButtons("modind",h6(gettext("Select elements to modify"),align="center"),choices=list(gettext("Individuals"),gettext("Categories")),selected=gettext("Categories"),inline=TRUE)),
            br(),
            conditionalPanel(
             condition=paste("input.modind=='",gettext("Individuals"),"'",sep=''),
#            condition="input.modind=='Ind'",
            if(selection==gettext("No selection")){
              selectInput("select",label=h6(gettext("Select individuals from:")),
                          choices=list(gettext("No selection"),"cos2"="cos2",gettext("Manual"),"Contribution"="Contrib"),selected=gettext("No selection"))
            }
            else{
              selectInput("select",label=h6(gettext("Select individuals from:")),
                          choices=list(gettext("No selection"),"cos2"="cos2",gettext("Manual"),"Contribution"="Contrib"),selected=selection)
              },
             conditionalPanel(
              condition="input.select=='cos2'",
              if(selection=="cos2"){
                div(align="center",sliderInput("slider1", label = "cos2",
                                               min = 0, max = 1, value =selection2,step=0.05))
              }
              else{
                div(align="center",sliderInput("slider1", label = "cos2",
                                               min = 0, max = 1, value =0,step=0.05))
              }),
              
            conditionalPanel(
#              condition="input.select=='Manuel'",
              condition=paste("input.select=='",gettext("Manual"),"'",sep=''),
              if(selection==gettext("Manual")){
                selectInput("indiv",label=gettext("Select individuals:"),
                            choices=list(num=nom),multiple=TRUE,selected=selection2) 
              }
              else{
                selectInput("indiv",label=gettext("Select individuals:"),
                            choices=list(num=nom),multiple=TRUE)  
              }),
            conditionalPanel(
              condition="input.select=='Contrib'",
              if(selection=="Contrib"){
                sliderInput("sliderContrib",label=gettext("Number of the most contributive individuals"),
                            min=1,max=length(nom),value=selection2,step=1)  
              }
              else{
                sliderInput("sliderContrib",label=gettext("Number of the most contributive individuals"),
                            min=1,max=length(nom),value=length(nom),step=1)
              }),
            colourpicker::colourInput("colindact",gettext("Colour of active individuals"),color1),
            uiOutput("col1"),

if(is.null(habillageind)){
  checkboxInput("habi",gettext("Points colour depend on categorical variable"),FALSE)
}
else{
  checkboxInput("habi",gettext("Points colour depend on categorical variable"),TRUE)
},
            
            conditionalPanel(
              condition="input.habi==true",
              uiOutput("habillage2")
              #uiOutput("ellips")
            )
          ),
          
          conditionalPanel(
             condition=paste("input.modind=='",gettext("Categories"),"'",sep=''),
#            condition="input.modind=='Mod'",
            if(selection3==gettext("No selection")){
              selectInput("selectMod",label=h6(gettext("Draw categories according to")),
                          choices=list(gettext("No selection"),"cos2"="cos2","Contribution"="Contrib"),selected=gettext("No selection")) 
            }
            else{
              selectInput("selectMod",label=h6(gettext("Draw categories according to")),
                          choices=list(gettext("No selection"),"cos2"="cos2","Contribution"="Contrib"),selected=selection3)
              },
            conditionalPanel(
              condition="input.selectMod=='cos2'",
              if(selection3=="cos2"){
                div(align="center",sliderInput("sliderCosMod", label = "cos2",
                                               min = 0, max = 1, value =as.numeric(selection4),step=0.05))}  
              else{
                div(align="center",sliderInput("sliderCosMod", label = "cos2",
                                               min = 0, max = 1, value=0,step=0.05))  
              }),
            conditionalPanel(
              condition="input.selectMod=='Contrib'",
              uiOutput("slider3")
            ),
        checkboxInput("eachvar",gettext("Colour each variable with different colour"),valdef),
        colourpicker::colourInput("colvaract",gettext("Colour of active categories"),color3),
        uiOutput("col2")
          )),
          conditionalPanel(
            condition="input.MCAgraph=='var'",
            p(gettext("Graph of variables"),align="center"),
            textInput("title2",h6(gettext("Title of the graph: ")), title2),
            sliderInput("cex2",h6(gettext("Size of labels")),min=0.5,max=2.5,value=1,step=0.05,ticks=FALSE),
#          div(align="center",checkboxGroupInput("var_sup",h6(""),choices=list("Supplementary qualitative variables"="suplquali","Supplementary quantitative variables"="suplquanti","Active qualitative variables"="act"),selected=varsup))
          div(align="center",checkboxGroupInput("var_sup",h6(""),choices=list(gettext("Active qualitative variables"),gettext("Supplementary qualitative variables"),gettext("Supplementary quantitative variables")),selected=varsup)),
          colourpicker::colourInput("colvaract1",gettext("Colour of active categories"),color5),
          uiOutput("col3"),
          uiOutput("colquantib")
        ),
        conditionalPanel(
#          condition="input.MCAgraph=='quant'",
          condition=paste("input.MCAgraph=='",gettext("Quantitative variables"),"'",sep=''),
          p(gettext("Graph of the supplementary quantitative variables"),align="center"),
          textInput("title3",h6(gettext("Title of the graph: ")), title3),
          sliderInput("cex3",h6(gettext("Size of labels")),min=0.5,max=2.5,value=1,step=0.05,ticks=FALSE),
          uiOutput("colquanti12")))
      ),
      
      
      wellPanel(
        h5(gettext("Save graphs as"),align="center"),
        radioButtons("paramdown","",
                     choices=list("PNG"="png","JPG"="jpg","PDF"="pdf"),selected="png"),
      br(),
      div(align="center",actionButton("MCAcode", gettext("Get the MCA code")))),

    br(),
    div(align="center",actionButton("Quit", gettext("Quit the app")))
    ,width=3),
    mainPanel(
      tags$style(type = "text/css", "a{color: #B53977;}"),
      tabsetPanel(id = "graph_sort",
                  tabPanel(gettext("Graphs"),
 fluidRow(
                           br(),
                 column(width = 6,plotOutput("map", width = "500", height="500"),
#                           div(align = "center",plotOutput("map", width = 500, height=500)),
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
                           br(),align="center"),
 column(width = 6,plotOutput("map4", width = "500",height="500"),
#                           div(align="center",plotOutput("map4", width = 500, height=500)),
                           br(),
                           conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData10",gettext("Download as jpg")),align="center")),
                           conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData0",gettext("Download as png")),align="center")),
                           conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData20",gettext("Download as pdf")),align="center")),
                           align="center")),
                           br(),
                           div(align = "center",uiOutput("map22")),
                           br(),
                           conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             div(uiOutput("download4"),align="center")),
                           conditionalPanel(
                             condition="input.paramdown=='png'",
                             div(uiOutput("download3"),align="center")),
                           conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             div(uiOutput("download5"),align="center"))
                  ),
                  
                  ####
                  
                  tabPanel(gettext("Values"),
                           br(),
                           uiOutput("out22"),
                           br(), 
                           conditionalPanel(
#                             condition="input.out=='eig'",
                               condition=paste("input.out=='",gettext("Eigenvalues"),"'",sep=''),
                             div(align="center",tableOutput("sorties")),
                             div(align="center",plotOutput("map3",width = 500, height=300))),
                           
                           conditionalPanel(
#                             condition="input.out=='resvar'",
                               condition=paste("input.out=='",gettext("Results of the variables"),"'",sep=''),
                             h6(gettext("Coordinates")),
                             
                             div(align="center",tableOutput("sorties2")),
                             br(),
                             h6("Contributions"),
                             div(align="center",tableOutput("sorties3")),
                             br(),
                             h6("Cos2"),
                             div(align="center",tableOutput("sorties4"))),
                           #Mettre un if pour le nombre d'individus
                           conditionalPanel(
#                             condition="input.out=='resind'",
                               condition=paste("input.out=='",gettext("Results of the individuals"),"'",sep=''),
                             h6(gettext("Coordinates")),
                             div(align="center",dataTableOutput("sorties22")),
                             br(),
                             h6("Contributions"),
                             div(align="center",dataTableOutput("sorties33")),
                             br(),
                             h6("Cos2"),
                             div(align="center",dataTableOutput("sorties44"))),
                           
                           conditionalPanel(
#                             condition="input.out=='MCA'",
                               condition=paste("input.out=='",gettext("Summary of outputs"),"'",sep=''),
                             numericInput("nbele",h6(gettext("Number of elements to print")),value=10),
                             br(),
                             verbatimTextOutput("summaryMCA"),
                             p(downloadButton("summary2",gettext("Download the summary")),align="center")
                           ),
                           conditionalPanel(
#                             condition="input.out=='varsup'",
                             condition=paste("input.out=='",gettext("Results of the supplementary categorical variables"),"'",sep=''),
                             h6(gettext("Coordinates")),
                             div(align="center",tableOutput("sorties23")),
                             br(),
                             h6("cos2"),
                             div(align="center",tableOutput("sorties232")),
                             br(),
                             h6("v.test"),
                             div(align="center",tableOutput("sorties233"))),
                           
                           conditionalPanel(
#                             condition="input.out=='quantico'",
                             condition=paste("input.out=='",gettext("Results of the supplementary quantitative variables"),"'",sep=''),
                             h6(gettext("Coordinates")),
                             div(align="center",tableOutput("sorties43"))),
                           
                           conditionalPanel(
                             condition="input.out=='Isup'",
                             h6(gettext("Coordinates")),
                             div(align="center",tableOutput("sortiesIsupC")),
                             br(),
                             h6("Cos2"),
                             div(align="center",tableOutput("sortiesIsupCos")))
                  ),
                  
                  tabPanel(gettext("Automatic description of axes"),
                           br(),
                           radioButtons("Dim",label=gettext("Choose the dimensions"),choices=list("Dimension 1"="Dim1","Dimension 2"="Dim2","Dimension 3"="Dim3"),selected="Dim1"),
                           conditionalPanel(
                             condition="input.Dim=='Dim1'",
                             p(gettext("Categorical variables")),
                             div(align="center",tableOutput("sortieDimdesc2")),
                             br(),
                             p(gettext("Categories")),
                             div(align="center",tableOutput("sortieDimdesc")),
                             br(),
                             p(gettext("Quantitative variables")),
                             div(align="center",tableOutput("sortieDimdesc3"))
                           ),
                           br(), 
                           conditionalPanel(
                             condition="input.Dim=='Dim2'",
                             p(gettext("Categorical variables")),
                             div(align="center",tableOutput("sortieDimdesc22")),
                             br(),
                             p(gettext("Categories")),
                             div(align="center",tableOutput("sortieDimdesc00")),
                             br(),
                             p(gettext("Quantitative variables")),
                             div(align="center",tableOutput("sortieDimdesc33"))
                           ),
                           br(),
                           conditionalPanel(
                             condition="input.Dim=='Dim3'",
                             p(gettext("Categorical variables")),
                             div(align="center",tableOutput("sortieDimdesc222")),
                             br(),
                             p(gettext("Categories")),
                             div(align="center",tableOutput("sortieDimdesc000")),
                             br(),
                             p(gettext("Quantitative variables")),
                             div(align="center",tableOutput("sortieDimdesc333"))
                           )
                  ),
                  tabPanel(gettext("Summary of dataset"),
                           br(),
                           verbatimTextOutput("summary"),
                           selectInput("bam",gettext("Graphs for "),choices=list(IdChoices=VariableChoices),multiple=FALSE),
                           
                           div(align = "center",plotOutput("histo", width = 500, height=500))),
                  tabPanel(gettext("Data"),
                           br(),
                           dataTableOutput("JDD")
                  )                       
      )
    ,width=9)
  )
))
