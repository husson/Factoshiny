page_Facto <- 
  shinydashboard::tabItem(tabName = "Facto",
          h1(gettext("Select an analysis",domain="R-Factoshiny")),
          h2(" "),
          fluidRow(
            div(verbatimTextOutput("warn"),style='padding:0px;text-align:center;background-color:light-blue;color:red;white-space:normal;'),
            column(width = 4,
                   wellPanel(
                     div(align="center",gettext("Characterizing a qualitative variable",domain="R-Factoshiny"), style='font-size:14pt; padding:5px;text-align:center;background-color: light-blue;white-space: normal;'),
                     div(align="center",style="display:inline-block",actionButton("catdesmethod", icon=icon("caret-square-right"), paste(gettext("Run",domain="R-Factoshiny")),style='padding:5px;text-align:center;background-color: yellow;white-space: normal;'),actionButton("action_catdes", icon=icon("question-circle"), label = gettext("Help",domain="R-Factoshiny"))),
                     uiOutput("def_catdes"),
                     align="center", style='padding:5px; background-color: #FFF8EA;text-align:justify; white-space: normal;'
                   ),
                   wellPanel(
                     div(align="center",gettext("Characterizing a quantitative variable",domain="R-Factoshiny"), style='font-size:14pt; padding:5px;text-align:center;background-color: light-blue;white-space: normal;'),
                     actionButton("condesmethod", icon=icon("caret-square-right"), gettext("Run",domain="R-Factoshiny"),style='padding:5px;text-align:center;background-color: yellow;white-space: normal;'),
                     div(style="display:inline-block",actionButton("action_condes", icon=icon("question-circle"), label = gettext("Help",domain="R-Factoshiny"))),
                     uiOutput("def_condes"),
                     align="center", style='padding:5px; background-color: #FFF8EA;text-align:justify; white-space: normal;'
                   ),

                   wellPanel(
                     div(align="center",gettext("Clustering",domain="R-Factoshiny"), style='font-size:14pt; padding:5px;text-align:center;background-color: light-blue;white-space: normal;'),
                     actionButton("HCPCmethod", icon=icon("caret-square-right"), gettext("Run",domain="R-Factoshiny"), style='padding:5px;text-align:center;background-color: yellow; white-space: normal;'),
                     div(style="display:inline-block",actionButton("action_hcpc", icon=icon("question-circle"), label = gettext("Help",domain="R-Factoshiny"))),
                     uiOutput("def_hcpc"), 
                     align="center", style='padding:5px; background-color: #FFF8EA;text-align:justify; white-space: normal;'
                   )
            ),
            column(width = 4,
                   wellPanel(
                     div(align="center",gettext("Principal Component Analysis",domain="R-Factoshiny"), style='font-size:14pt; padding:5px;text-align:center;background-color: light-blue;white-space: normal;'),
                     actionButton("PCAmethod", icon=icon("caret-square-right"), gettext("Run",domain="R-Factoshiny"),style='padding:5px;text-align:center;background-color: yellow;white-space: normal;'),
                     div(style="display:inline-block",actionButton("action_pca", icon=icon("question-circle"), label = gettext("Help",domain="R-Factoshiny"))),
                     uiOutput("def_pca"),
                     align="center", style='padding:5px; background-color: #FFF8EA;text-align:justify; white-space: normal;'
                   ),
                   wellPanel(
                     div(align="center",gettext("Correspondence Analysis",domain="R-Factoshiny"), style='font-size:14pt; padding:5px;text-align:center;background-color: light-blue;white-space: normal;'),
                     actionButton("CAmethod", icon=icon("caret-square-right"), gettext("Run",domain="R-Factoshiny"), style='padding:5px;text-align:center;background-color: yellow; white-space: normal;'),
                     div(align="center",style="display:inline-block",actionButton("action_ca", icon=icon("question-circle"), label = gettext("Help",domain="R-Factoshiny"))),
                     uiOutput("def_ca"), 
                     align="center", style='padding:5px; background-color: #FFF8EA;text-align:justify; white-space: normal;'
                   ),
                   wellPanel(
                     div(align="center",gettext("Multiple Factor Analysis",domain="R-Factoshiny"), style='font-size:14pt; padding:5px;text-align:center;background-color: light-blue;white-space: normal;'),
                     actionButton("MFAmethod", icon=icon("caret-square-right"), gettext("Run",domain="R-Factoshiny"), style='padding:5px;text-align:center;background-color: yellow; white-space: normal;'),
                     div(style="display:inline-block",actionButton("action_mfa", icon=icon("question-circle"), label = gettext("Help",domain="R-Factoshiny"))),
                     uiOutput("def_mfa"), 
                     align="center", style='padding:5px; background-color: #FFF8EA;text-align:justify; white-space: normal;'
                   )
            ),
            column(width = 4,
                   wellPanel(
                     div(align="center",gettext("Multiple Correspondence Analysis",domain="R-Factoshiny"), style='font-size:14pt; padding:5px;text-align:center;background-color: light-blue;white-space: normal;'),
                     actionButton("MCAmethod", icon=icon("caret-square-right"), gettext("Run",domain="R-Factoshiny"), style='padding:5px;text-align:center;background-color: yellow; white-space: normal;'),
                     div(style="display:inline-block",actionButton("action_mca", icon=icon("question-circle"), label = gettext("Help",domain="R-Factoshiny"))),
                     uiOutput("def_mca"), 
                     align="center", style='padding:5px; background-color: #FFF8EA;text-align:justify; white-space: normal;'
                   ),
                   wellPanel(
                     div(align="center",gettext("Factor Analysis on Mixed Data",domain="R-Factoshiny"), style='font-size:14pt; padding:5px;text-align:center;background-color: light-blue;white-space: normal;'),
                     actionButton("FAMDmethod", icon=icon("caret-square-right"), gettext("Run",domain="R-Factoshiny"), style='padding:5px;text-align:center;background-color: yellow; white-space: normal;'),
                     div(style="display:inline-block",actionButton("action_famd", icon=icon("question-circle"), label = gettext("Help",domain="R-Factoshiny"))),
                     uiOutput("def_famd"),
                     align="center", style='padding:5px; background-color: #FFF8EA;text-align:justify; white-space: normal;'
                     )
                   )
            )
          
  )


ui <- shinydashboard::dashboardPage(skin = "purple", 
                    shinydashboard::dashboardHeader(title = gettext("Factoshiny interface",domain="R-Factoshiny")),
                    shinydashboard::dashboardSidebar(width=250,
                      tags$head(
                       tags$style(HTML(".content-wrapper {
                        background-color: light-blue !important;
                       }
                       .main-sidebar {
                        background-color: #4d5c75 !important;
                        padding : 10px; font-size: 16px;     }
                      "))
                    ),
                    HTML(paste("<br><br><h3>",gettext("Dataset description",domain="R-Factoshiny"),"</h3>")),
                    HTML(paste(gettext("The dataset",domain="R-Factoshiny")," <em>",nomDatashiny,"</em> ",gettext("has",domain="R-Factoshiny"),nrow(x), gettext("individuals, and",domain="R-Factoshiny"),ncol(x),"variables:")),
                    HTML(if(sum(!sapply(x,is.numeric))==0){gettext("All the variables are quantitative.",domain="R-Factoshiny")}),
                    HTML(if(sum(sapply(x,is.numeric))==0){gettext("All the variables are qualitative.",domain="R-Factoshiny")}),
                    HTML(if(sum(sapply(x,is.numeric))==1){paste(sum(sapply(x,is.numeric)),gettext("variable is quantitative and",domain="R-Factoshiny"),sum(!sapply(x,is.numeric)), gettext("are qualitative.",domain="R-Factoshiny"))}),
                    HTML(if(sum(!sapply(x,is.numeric))==1){paste(sum(sapply(x,is.numeric)),gettext("variables are quantitative and",domain="R-Factoshiny"),sum(!sapply(x,is.numeric)), gettext("is qualitative.",domain="R-Factoshiny"))}),
                    HTML(if((sum(sapply(x,is.numeric))>1) & (sum(!sapply(x,is.numeric))>1)){paste(sum(sapply(x,is.numeric)),gettext("variables are quantitative and",domain="R-Factoshiny"),sum(!sapply(x,is.numeric)), gettext("are qualitative.",domain="R-Factoshiny"))}),
                    HTML("<br><h3>",gettext("Select an analysis",domain="R-Factoshiny"),"</h3>",gettext("You can use one of the following methods: ",domain="R-Factoshiny")),
                    if(sum(!sapply(x,is.numeric))==0){ if(Sys.getenv("LANG")=="fr") {HTML("ACP, AFC, AFM ou classification.")} else {HTML("PCA, CA, MFA or clustering.")}},
                    if(sum(sapply(x,is.numeric))==0){ if(Sys.getenv("LANG")=="fr") {HTML("ACM ou AFM si vous avez des groupes de variables.")} else {HTML("MCA or MFA if you have groups of variables.")}},
                    if(sum(sapply(x,is.numeric))==1){ if(Sys.getenv("LANG")=="fr") {HTML("ACM, AFDM ou AFM.")} else {HTML("MCA, FAMD or MFA.")}},
                    if(sum(!sapply(x,is.numeric))==1){ if(Sys.getenv("LANG")=="fr") {HTML("ACP, AFC, AFDM ou AFM.")} else {HTML("PCA, CA, FAMD or MFA.")}},
                    if((sum(sapply(x,is.numeric))>1) & (sum(!sapply(x,is.numeric))>1)){ if(Sys.getenv("LANG")=="fr") {HTML("ACP, AFC, ACM, AFDM ou AFM.")} else {HTML("PCA, CA, MCA, FAMD or MFA.")}},
                    HTML("<br>"),
                    if(Sys.getenv("LANG")=="fr") {HTML(gettext("Pour avoir de l'aide sur le choix de la méthode, voir cette <a href='https://www.youtube.com/watch?v=UrS00sOpeec'>vidéo</a>."))} else {HTML(gettext("If you need any guidance on the choice of the method, see this <a href='https://youtu.be/666OzkmvYbs'>video</a>."))},
                    HTML("<br><h3>"),
                    HTML(gettext("Useful links",domain="R-Factoshiny")),
                    HTML("</h3>"),
                    shinydashboard::sidebarMenu(
                      shinydashboard::menuItem(gettext("Factoshiny website",domain="R-Factoshiny"), icon = icon("info-circle"), href='http://factominer.free.fr/graphs/factoshiny.html',
                               startExpanded=T
                      ),                       
                      shinydashboard::menuItem(gettext("FactoMineR website",domain="R-Factoshiny"), icon = icon("laptop-code"), href='http://factominer.free.fr/',
                               startExpanded=T
                      ), 
                      shinydashboard::menuItem("F. Husson", icon = icon("user"), href='https://husson.github.io/',
                          startExpanded=T
                      ), 
                      tabName = "Facto"
                      ),
                    div(align="center",actionButton("Quit", gettext("Quit the app",domain="R-Factoshiny"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
                    ),

          shinydashboard::dashboardBody(page_Facto)
)
