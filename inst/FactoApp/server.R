shinyServer(function(input, output) {
  

  valeuretour <- function(){
    if(input$condesmethod %% 2) param <- "condes"
    if(input$catdesmethod %% 2) param <- "catdes"
    if(input$PCAmethod %% 2) param <- "PCA"
    if(input$CAmethod %% 2) param <- "CA"
    if(input$MCAmethod %% 2) param <- "MCA"
    if(input$MFAmethod %% 2) param <- "MFA"
    if(input$FAMDmethod %% 2) param <- "FAMD"
    if(input$HCPCmethod %% 2) param <- "HCPC"
    return(param)
  }  
  
  observe({
    if((input$catdesmethod %% 2) |(input$condesmethod %% 2) |(input$PCAmethod %% 2) |(input$CAmethod %% 2) |(input$MCAmethod %% 2) |(input$MFAmethod %% 2) | (input$FAMDmethod %% 2) |(input$HCPCmethod %% 20) ){
      sumInput <- input$catdesmethod + input$condesmethod + input$PCAmethod + input$CAmethod + input$MCAmethod + input$MFAmethod + input$FAMDmethod + input$HCPCmethod
	  if(input$condesmethod %% 2) validate(need(any(sapply(x,is.numeric)) || (input$condesmethod + input$PCAmethod + input$CAmethod)!=sumInput," "))
	  if(input$PCAmethod %% 2) validate(need(sum(sapply(x,is.numeric))>2 || (input$condesmethod + input$PCAmethod + input$CAmethod)!=sumInput," "))
	  if(input$CAmethod %% 2) validate(need(sum(sapply(x,is.numeric))>2 || (input$condesmethod + input$PCAmethod + input$CAmethod)!=sumInput," "))
	  if(input$catdesmethod %% 2) validate(need(!all(sapply(x,is.numeric)) || (input$catdesmethod + input$MCAmethod + input$HCPCmethod)!=sumInput," "))
	  if(input$MCAmethod %% 2) validate(need(sum(!sapply(x,is.numeric))>2 || (input$catdesmethod + input$MCAmethod + input$HCPCmethod)!=sumInput," "))
	  if(input$FAMDmethod %% 2) validate(need((!all(sapply(x,is.numeric)) & any(sapply(x,is.numeric))) || input$FAMDmethod!=sumInput," "))
	  isolate({
        stopApp(returnValue=valeuretour())
      })
    }
  })
    output$warn <- renderPrint({
	  warna <- NULL
      if(input$condesmethod %% 2 && !any(sapply(x,is.numeric))) warna <- gettext("You need at least 1 quantitative variable",domain="R-Factoshiny")
      if(input$PCAmethod %% 2 && sum(sapply(x,is.numeric))<4) warna <- gettext("You need at least 3 quantitative variables",domain="R-Factoshiny")
      if(input$CAmethod %% 2 && sum(sapply(x,is.numeric))<4) warna <- gettext("You need at least 3 quantitative columns",domain="R-Factoshiny")
      if(input$MCAmethod %% 2 && sum(!sapply(x,is.numeric))<4) warna <- gettext("You need at least 3 qualitative variables",domain="R-Factoshiny")
      if(input$catdesmethod %% 2 && all(sapply(x,is.numeric))) warna <- gettext("You need at least 1 qualitative variable",domain="R-Factoshiny")
      if(input$FAMDmethod %% 2 && ((all(!sapply(x,is.numeric)) || all(sapply(x,is.numeric))))) warna <- gettext("Your dataset is not mixed",domain="R-Factoshiny")
      if (!is.null(warna)) return(cat(gettext("This method is not available",domain="R-Factoshiny"),warna,sep="\n"))
    })
  
  observe({
    if(input$Quit!=0){
      isolate({
        stopApp(returnValue = NULL)
      })
    }
  })

  output$def_catdes = renderUI({
    if (input$action_catdes %% 2){
      helpText(    if(Sys.getenv("LANG")=="fr"){
        HTML("Cette méthode caractérise une variable qualitative en fonction de toutes les autres variables du jeu de données. Elle trie les variables
             en fonction de l'intensité de leur liaison avec cette variable qualitative d'intérêt. De même chaque modalité de la variable d'intérêt est décrite par les autres variables.<br>
             Voici une <a href='https://youtu.be/N7GkrUYP1LM' target='_blank'>vidéo</a> qui décrit la méthode et comment interpréter les résultats. 
			 Et une <a href='https://youtu.be/JAsvTf-8cXo' target='_blank'>vidéo</a> qui décrit comment mettre en oeuvre la méthode avec Factoshiny.")
      } else {
        HTML("This method characterizes a qualitative variable by all the others variables in the dataset. It sorts the variables
             depending on the intensity of their relationship with this quaitative variable. Similarly, each modality of the variable of interest is described by the other variables.")
      }
  )
    } else {
      return()
    }
  })


  output$def_condes <- renderUI({
    if (input$action_condes %% 2){
      helpText( if(Sys.getenv("LANG")=="fr"){
      HTML("Cette méthode caractérise une variable d'intérêt du jeu de données en fonction de toutes les autres. Elle trie les variables
 en fonction de l'intensité de leur liaison avec cette variable d'intérêt. De même chaque modalité de la variable d'intérêt est décrite par les autres variables.")
    } else {
      HTML("This method characterizes a variable of interest in the dataset as a function of all the others. It sorts the variables
 depending on the intensity of their relationship with this variable of interest. Similarly, each modality of the variable of interest is described by the other variables.")
    }
      )
    } else {
      return()
    }
  })
  
    
    output$def_pca <- renderUI({
      if (input$action_pca %% 2){
        helpText( if(Sys.getenv("LANG")=="fr"){
        HTML("L'analyse en composantes principales (ACP) permet de décrire et visualiser des tableaux de données
            où les individus sont décrits par des variables quantitatives. Des variables qualitatives peuvent être considérées comme variables
            supplémentaires.<br>
            Voici quelques vidéos qui décrivent la méthode et comment interpréter les résultats:
           <ul>
           <li><a href='https://www.youtube.com/watch?v=KrNbyM925wI' target='_blank'>Données - problématiques</a></li>
           <li><a href='https://youtu.be/twuP1Na0COE' target='_blank'>Etude des individus et des variables</a></li>
           <li><a href='https://youtu.be/D0fiLfSDY38' target='_blank'>Aides à l'interprétation</a></li>
           <li><a href='https://youtu.be/i-mEyUa9U5k' target='_blank'>Mise en oeuvre avec Factoshiny</a></li>
           <li><a href='https://www.youtube.com/watch?v=bdD9P3fGb70' target='_blank'>Gestion des données manquantes en ACP</a></li>
           </ul>")
      } else {
          HTML("Principal Component Analysis (PCA) allows you to explore and 
               visualize dataset where individuals are described by quantitative variables. Qualitative variables can be used as supplementary variables.<br>
           Some videos that decribe the method:
           <ul>
           <li><a href='https://www.youtube.com/watch?v=IuSbb3nq4aI' target='_blank'>Data - practicalities</a></li>
           <li><a href='https://www.youtube.com/watch?v=msGKYXVdMxc' target='_blank'>Studying individuals and variables</a></li>
           <li><a href='https://www.youtube.com/watch?v=BOanMaBiN2w' target='_blank'>Interpretation aids</a></li>
         <li><a href='https://youtu.be/pks8m2ka7Pk' target='_blank'>Implementation with FactoMineR</a></li>
           <li><a href='https://www.youtube.com/watch?v=OOM8_FH6_8o' target='_blank'>Handling missing values in PCA</a></li>
           </ul>")
          }
        )
      } else {
        return()
      }
    })
  
  output$def_ca <- renderUI({
    if (input$action_ca %% 2){
      helpText(if(Sys.getenv("LANG")=="fr"){
      HTML("L'analyse des correspondences (AFC) est une méthode qui permet de décrire et visualiser des tableaux de contingence 
           et donc d'étudier les relations entre les modalités de 2 variables qualitatives. La méthode est classiquement utilisée en analyse textuelle.
           Des variables quantitatives et qualitatives peuvent être ajoutées comme variables
            supplémentaires.<br>
            Voici quelques vidéos qui décrivent la méthode et comment interpréter les résultats:
           <ul>
           <li><a href='https://youtu.be/KsSmAwiNKnw' target='_blank'>Données - problématiques</a></li>
           <li><a href='https://youtu.be/B2mAeRRBunk' target='_blank'>Visualisation des nuages de lignes et de colonnes</a></li>
           <li><a href='https://youtu.be/QgASpH8zVo4' target='_blank'>Inertie et pourcetnage d'inertie</a></li>
           <li><a href='https://youtu.be/EL88lwm-Kkc' target='_blank'>Représentation simultanée</a></li>
           <li><a href='https://youtu.be/clXIk0bE8dc' target='_blank'>Aides à l'interprétation </a></li>
           <li><a href='https://youtu.be/Ucg3x9vCtZU' target='_blank'>Mise en oeuvre avec Factoshiny</a></li>
           <li><a href='https://youtu.be/W1my-DEoPZ0' target='_blank'>Etude de cas : analyse textuelle </a></li>
           </ul>")
    } else {
      HTML("Correspondence analysis (CA) is a method that explore and 
               visualize contingency table. Qualitative and quantitative variables can be used as supplementary variables.<br>
           Some videos that decribe the method:
         <ul>
         <li><a href='https://www.youtube.com/watch?v=Z5Lo1hvZ9fA' target='_blank'>Introduction</a></li>
         <li><a href='https://www.youtube.com/watch?v=VVYPQKG7jfw' target='_blank'>Visualizing the row and column clouds</a></li>
         <li><a href='https://www.youtube.com/watch?v=ffZb9wAiElk' target='_blank'>Inertia and percentage of inertia</a></li>
         <li><a href='https://www.youtube.com/watch?v=3-zxmzZZTIQ' target='_blank'>Simultaneous representation</a></li>
         <li><a href='https://www.youtube.com/watch?v=Jg9IXaAi98w' target='_blank'>Interpretation aids</a></li>
         <li><a href='https://youtu.be/vP4korRby0Q' target='_blank'>Implementation with FactoMineR</a></li>
         <li><a href='https://www.youtube.com/watch?v=62N0JMf5hOs' target='_blank'>Example on textual data</a></li>
         </ul>")
    }
      )
    } else {
      return()
    }
  })

  output$def_mca <- renderUI({
    if (input$action_mca %% 2){
      helpText(if(Sys.getenv("LANG")=="fr"){
      HTML("L'analyse des correspondences multiples (ACM) est une méthode qui permet de décrire et visualiser des tableaux où les individus sont décrits par des variables qualitatives.
       Classiquement, cette méthode est utilisée pour analyser des quationnaitres. 
           Des variables quantitatives et qualitatives peuvent être ajoutées comme variables
            supplémentaires.<br>
            Voici quelques vidéos qui décrivent la méthode et comment interpréter les résultats:
           <ul>
      <li><a href='https://www.youtube.com/watch?v=C_JhUZtrPsc' target='_blank'>Données - problématiques</a></li>
      <li><a href='https://www.youtube.com/watch?v=S7ojPthc0bE' target='_blank'>Etude des individus</a></li>
      <li><a href='https://www.youtube.com/watch?v=liWN3V7RkZc' target='_blank'>Etude des variables</a></li>
      <li><a href='https://www.youtube.com/watch?v=lt9F2Iumzxs' target='_blank'>Aides à l'interprétation</a></li>
           <li><a href='https://youtu.be/mUKz4L2ZsuY' target='_blank'>Mise en oeuvre avec Factoshiny</a></li>
      <li><a href='https://www.youtube.com/watch?v=4F2C11hcvMM' target='_blank'>Gestion des données manquantes en ACM</a></li>
           </ul>")
    } else {
      HTML("Multiple Correspondence analysis (MCA) is a method that explore and 
               visualize dataset where individuals are described by qualitative variables. Quantitative variables can be used as supplementary variables.<br>
           Some videos that decribe the method:
         <ul>
         <li><a href='https://www.youtube.com/watch?v=gZ_7WWEVlTg' target='_blank'>Data - issues</a></li>
         <li><a href='https://www.youtube.com/watch?v=b4kRAt4mkB8' target='_blank'>Visualizing the point cloud of individuals</a></li>
         <li><a href='https://www.youtube.com/watch?v=OZW1KrKO5Ac' target='_blank'>Visualizing the cloud of categories</a></li>
         <li><a href='https://www.youtube.com/watch?v=wTCuThGiy4w' target='_blank'>Interpretation aids</a></li>
         <li><a href='https://youtu.be/Sl1-UYD6iac' target='_blank'>Implementation with FactoMineR</a></li>
         <li><a href='https://www.youtube.com/watch?v=uyIH1CtrfsU' target='_blank'>Handling missing values in MCA</a></li>
         </ul>")
    }
      )
    } else {
      return()
    }
  })
  
  output$def_mfa <- renderUI({
    if (input$action_mfa %% 2){
      helpText(if(Sys.getenv("LANG")=="fr"){
      HTML("L'analyse Factorielle Multiple (AFM) permet de décrire et visualiser des tableaux où les individus sont décrits par des variables structurées en groupes.
           Les variables d'un groupe peuvent être quantitatives ou qualitatives, ou encore peuvent être un tableau de fréquences.<br>
           Voici quelques vidéos qui décrivent la méthode et comment interpréter les résultats:
           <ul>
           <li><a href='https://www.youtube.com/watch?v=1U-s8u1rcpo' target='_blank'>Données - problématiques</a></li>
           <li><a href='https://www.youtube.com/watch?v=u8SA8d8h6MU' target='_blank'>Equilibre et ACP globale</a></li>
           <li><a href='https://www.youtube.com/watch?v=abQllkEfIeM' target='_blank'>Etude des groupes de variables</a></li>
           <li><a href='https://www.youtube.com/watch?v=g8aOwlsgk2U' target='_blank'>Compléments (données qualitatives, fréquences, aides à l'interprétation)</a></li>
           <li><a href='https://youtu.be/g0lM2qQ4lvs' target='_blank'>Mise en oeuvre avec Factoshiny</a></li>
           </ul>")
    } else {
      HTML("Multiple Factor Analysis (MFA) is a method that explore and 
         visualize dataset where individuals are described by variables that are structured by groups.<br> You can compare the information brings by each group of variables.<br>
           Some videos that decribe the method:
         <ul>
         <li><a href='https://youtu.be/MOl0Aw1TTFE' target='_blank'>Introduction</a></li>
         <li><a href='https://youtu.be/kGSjHD3yG84' target='_blank'>Weighting and global PCA</a></li>
         <li><a href='https://youtu.be/OVtNX6Or1FI' target='_blank'>Study of the groups of variables</a></li>
         <li><a href='https://youtu.be/Vumu7OoFHdA' target='_blank'>Complements: qualitative groups, frenquency tables</a></li>
         <li><a href='https://youtu.be/7HHuKsl2_fk' target='_blank'>Implementation with FactoMineR</a></li>
         </ul>")
    }
      )
    } else {
      return()
    }
  })
  
  output$def_famd <- renderUI({
    if (input$action_famd %% 2){
      helpText(if(Sys.getenv("LANG")=="fr"){
      HTML("L'Analye factorielle de données mixtes est un méthode qui permet de prendre en compte simultanément des variables quantitatives et qualitatives
comme variables actives, i.e. comme variables pour calculer les distances entre individus.")
    } else {
      HTML("Factor Analysis on Mixed Data (FAMD) is a method that explore and 
         visualize dataset where individuals are described by both quantitative and qualitative variables.")
      }
      )
    } else {
      return()
    }
  })
  
  output$def_hcpc <- renderUI({
    if (input$action_hcpc %% 2){
      helpText(if(Sys.getenv("LANG")=="fr"){
      HTML("La classification permet de construire un arbre hiérarchique sur les individus à partir de variables quantitatives. 
          Une partition des individus en groupes est ensuite construite et les groupes sont décrits par les variables quantitatives ou qualitatives.
           Si vous avez des variables qualitatives, faire une analyse des correspondances multiples, puis une classification.<br>
Voici des vidéos décrivant la classification et la description des classes:
           <ul>
           <li><a href='https://www.youtube.com/watch?v=SE_4dLh5vXY' target='_blank'>Classification ascendante hiérarchique</a></li>
           <li><a href='https://www.youtube.com/watch?v=3XVoGXVG1mo' target='_blank'>Exemple et choix du nombre de classes</a></li>
           <li><a href='Méthode de partitionnement et complémentarité avec la CAH' target='_blank'>Méthode de partitionnement et complémentarité avec la CAH</a></li>
           <li><a href='https://www.youtube.com/watch?v=S-vfGS-VTDI' target='_blank'>Caractérisation des classes d'individus</a></li>
           <li><a href='https://youtu.be/6AqPc_kZQ0g' target='_blank'>Mise en oeuvre avec Factoshiny</a></li>
           </ul>")
    } else {
      HTML("Clustering constructs a hierarchical tree on individuals based on quantitative variables. 
          If you have qualitative variables, perfom a multiple correspondence analysis and then a clustering.<br>
           Some videos that decribe the method:
         <ul>
         <li><a href='https://www.youtube.com/watch?v=J-IddCBb-ZQ' target='_blank'>Introduction</a></li>
         <li><a href='https://www.youtube.com/watch?v=Emi1eMvGEJQ' target='_blank'>Example and how to choose the number of clusters</a></li>
         <li><a href='https://www.youtube.com/watch?v=qaE9D_kjhLk' target='_blank'>The partitioning method K-means</a></li>
         <li><a href='https://www.youtube.com/watch?v=w-EGV6xExWw' target='_blank'>Characterizing clusters</a></li>
         </ul>")
    }
      )
    } else {
      return()
    }
  })
  
})