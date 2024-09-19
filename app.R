#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(renv)
library(lubridate)
library(readxl)
library(shiny)
library(DT)
library(plotly)
library(leaflet)
library(rAmCharts)
library(highcharter)

#################### Function ######################################### 

# Utiliser plot_ly avec les données et spécifier la couleur des tranches


sprint_Spe <- function(time,distance,Temps) {
  if(time == 0){
    return(0)
  }
  if (distance %in% c("Hauteur","Poids","Longueur","Triple_Jump","Perche","Disque","Marteau","Javelot","Heptathlon","Decathlon")){
    max<-max(Temps[, distance],na.rm=TRUE)
    min<-min(Temps[, distance],na.rm=TRUE)
    if (time > max){
      return(1400)
    }
    if (time < min){
      return(0)
    }
    d<-Temps[,distance] - time
    
    if (any(0 == d & !is.na(d)) == TRUE ){
      ind<-which(d == 0)
      return(Temps$Points[ind])
    } else {
      n<-1
      ind<-which(d < 0)[n]
      while (is.na(ind) ==  TRUE){
        n<-n+1
        
        ind<-which(d < 0)[n]
      }
      return(Temps$Points[ind])
    }
  }else{
    max<-min(Temps[,distance],na.rm=TRUE)
    min<-max(Temps[,distance],na.rm=TRUE)
    if (time < max){
      return(1400)
    }
    if (time > min){
      return(0)
    }
    d<-Temps[,distance] - time
    
    if (any(0 == d & !is.na(d)) == TRUE ){
      ind<-which(d == 0)
      return(Temps$Points[ind])
    } else {
      n<-1
      ind<-which(d > 0)[n]
      
      while (is.na(ind) ==  TRUE){
        n<-n+1
        ind<-which(d > 0)[n]
      }
      return(Temps$Points[ind])
    }
  }
  
} # Points pour catégorie en sec ou metre

diff_minute<-function(time,distance,Temps){
  if (time == "0:0.0"){
    return(0)
  }
  
  min<-ms(Temps[1,distance])
  max<-ms(Temps[1400,distance])
  
  time1<-ms(time)
  
  if (time1 < min){
    return(1400)
  }
  if (time1 > max){
    return(0)
  }
  
  ind<-0
  for (i in 1:nrow(Temps)){
    ind<-ind+1
    t<-ms(Temps[i,distance]) - time1
    
    if (any(t >= 0 & !is.na(t)) == TRUE ){
      return(Temps$Points[ind])
    } 
  }
  
} # points pour catégorie en minute

Test_cat<-function(categorie){
  if (categorie %in% c("Hauteur","Poids","Longueur","Triple_Jump","Perche","Disque","Marteau","Javelot",'Decathlon','100m','200m','300m','110mH','100mH')){
    return(TRUE)
  }
  return(FALSE)
} # verif cat

V_sexe<-function(sexe){
  if (sexe == 'H'){
    return(Temps_H)} else{
      return(Temps_F)
    }
} # verif sexe

update_don<-function(data){
  data<-as.data.frame(data)
  # update points estimée
  
  ind<-which(data[,"temps_est"] >=0)
  for (num in ind){
    Temps <- V_sexe(data[num, 'Sexe']) # cherche la bonne base de temps
    dist <- data$Categorie[num]
    
    # Appliquer les modifications aux données
    if (Test_cat(dist) == TRUE) {
      temps<-data[num, "temps_est"]
      temps<-as.numeric(temps)
      
      data[num, "temps_est"]<-as.character(round(temps,2))
      data[num, "points_estimee"] <- sprint_Spe(temps, dist, Temps)}
    
    else {
      temps<-data[num, "temps_est"]
      data[num, "points_estimee"] <- diff_minute(temps, dist, Temps)}
  }
  
  # update points reel
  
  ind<-which(data[,"temps_reel"] >=0)
  for (num in ind){
    Temps <- V_sexe(data[num, 'Sexe']) # cherche la bonne base de temps
    dist <- data$Categorie[num]
    
    # Appliquer les modifications aux données
    if (Test_cat(dist) == TRUE) {
      temps<-data[num, "temps_reel"]
      temps<-as.numeric(temps)
      
      data[num, "temps_reel"]<-as.character(round(temps,2))
      
      data[num, "points_reel"] <- sprint_Spe(temps, dist, Temps)
      
    }
    
    else {
      
      temps<-data[num, "temps_reel"]
      
      data[num, "points_reel"] <- diff_minute(temps, dist, Temps)}
  }
  print(data)
  return(data)
} # Update don

#######################################################
################# Importation #########################
#######################################################


Temps_H<-read_xlsx("data/don_temps.xlsx",sheet = "H")

# Données des Licenciés par clubs
Temps_F<-read_xlsx("data/don_temps.xlsx",sheet = "F")
df <- read_xlsx("data/Don_35.xlsx")

#######################################################
################# Application #########################
#######################################################


####################################################
# Define UI for application that draws a histogram #
####################################################
ui <- fluidPage(
  
  navbarPage(
    # Application title
    title = "Visualisation Atlétisme",
    
    
    tabPanel(title = "Table",
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(fileInput(inputId = "file", label = "Importation Base", accept = c(".csv",".xlsx")),
                            selectInput(inputId = "type",label = "type",c("temps_est","temps_reel")),
                            textInput(inputId = "Nom",label = "Nom :"),
                            numericInput(inputId = "idMinute", label = "minutes",
                                         value = 0, min = 0, max = 60, step = 1),
                            numericInput(inputId = "idSeconde", label = "secondes/m . cent / cm",
                                         value = 0, min = 0, max = 60, step = 0.1),
                            actionButton("ajouter", "Save"),width=2
                            
               ),
               
               # Show a plot of the generated distribution
               mainPanel(verbatimTextOutput("message"),
                         verbatimTextOutput("message2"),
                         dataTableOutput(outputId = "myDataTable"),
                         actionButton("Verification", "Verification Base")
               )
             )),
    navbarMenu("Visualisation des points",
               tabPanel("Points Estimés",
                        h3("Visualisation des points pour les données estimées"),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "Sexe", label = "Choix du sexe :", c("Tout", "F", "H")),
                            selectInput(inputId = "type_cat", label = "Choix du type :", c("Sprint", "Demi fond", "Saut", "Lancer", "Marche")),
                            width=2),
                          mainPanel(
                            fluidRow(
                              
                              column(width = 6, plotlyOutput("graphe_sexe_E"),highchartOutput("graphe_cat_E")),
                              column(width = 6,offset = 0.5, highchartOutput("graphe_type_E"))
                            )
                          )
                        )
               )
               
               
               
               
               ,
               tabPanel("Points Réel",
                        h3("Visualisation des points pour les données réelles"),
                        tabsetPanel(
                          tabPanel("Estimation des points",
                                   tags$style(".tab-content {margin-top: 20px;}"),
                                   plotlyOutput("Graphe_Total_2")
                          ),
                          tabPanel("Distributions des points",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput(inputId = "Sexe_2", label = "Choix du sexe :", c("Tout", "F", "H")),
                                       selectInput(inputId = "type_cat_2", label = "Choix du type :", c("Sprint", "Demi fond", "Saut", "Lancer", "Marche")),
                                       width=3),
                                     mainPanel(
                                       fluidRow(
                                         
                                         column(width = 6, plotlyOutput("graphe_sexe_E_2"),highchartOutput("graphe_cat_E_2")),
                                         column(width = 6, highchartOutput("graphe_type_E_2"))
                                       )
                                     )
                                   )
                          )
                          
                        )
                        
                        
                        
               )
    ),
    # Onglet "Visualisation des licenciés"
    navbarMenu(
      title = "Visualisation des licenciés",
      icon = icon("map"),
      
      
      tabPanel(
        "Carte des clubs d'athlétismes en Bretagne",
        h3("Visualisation des clubs en Bretagne"),
        fluidRow(
          
          column(width = 6, leafletOutput("carte")),
          column(width = 6, plotlyOutput("circulaire_sexe"))
        ),
        textOutput("dynamic_text")
        
      ),
      
      tabPanel(
        "Histogramme des licenciés",
        fluidRow(
          
          sidebarPanel(
            sliderInput(inputId = "bins",
                        label = "Nombre de bins pour l'histogramme:",
                        min = 1,
                        max = 40,
                        value =  20),
            
            # Sélection des clubs qui participent aux tournois ou pas
            radioButtons("choix", "Participation du club aux tournois :",
                         choices = list("Tout" = 0, "Classés" = 1, "Non-classés" = 2),
                         selected = 0),
            
            # Sélection du département souhaité
            radioButtons("choix_dep", "Zone d'annalyse souhaité :",
                         choices = list("Bretagne" = 0, "Côtes-d'Armor" = 22, "Finistère" = 29,
                                        "Ille-et-Vilaine" = 35, "Morbihan" = 56),
                         selected = 0),
            
            # Bouton valider
            actionButton("bouton_mise_a_jour", "Afficher", icon = icon("refresh")) 
          ),
          
          
          
          mainPanel(
            tabsetPanel(
              tabPanel(
                "Histogramme des licenciés",
                fluidRow(
                  plotlyOutput("histo_licencie")
                )
              ),
              tabPanel(
                "Boîte à moustaches",
                fluidRow(
                  amChartsOutput("boxplot_licencie", height = "400px")
                )
              )
            )
          )
        ),
        textOutput("dynamic_texte_points")
      )
      
    )
  )
)



#################################################
################# SERVER ########################
#################################################


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ### Pour la table local ###
  
  # Créer un conteneur pour stocker les données
  data <- reactiveVal(NULL)
  
  
  observeEvent(input$myDataTable_rows_selected, {
    num <- input$myDataTable_rows_selected
    
    data1<-as.data.frame(data())
    
    if (Test_cat(data1[num,"Categorie"])){
      
      updateNumericInput(session, "idMinute", value = 0)
      updateNumericInput(session, "idSeconde", value = data1[num,input$type])
      updateTextInput(session,"Nom",value = data1[num,"Athlete"])
    } else{
      nombre<-strsplit(data1[num,input$type], ":")
      
      updateNumericInput(session, "idMinute", value = nombre[[1]][1])
      updateNumericInput(session, "idSeconde", value = nombre[[1]][2])
      updateTextInput(session,"Nom",value = data1[num,"Athlete"])
    }
    
    
  })
  
  
  # Observer l'événement du bouton "ajouter"
  observeEvent(input$ajouter, {
    
    # Vérifier si les données existent
    if (!is.null(data())) {
      num <- input$myDataTable_rows_selected
      
      # Vérifier si le numéro de ligne est valide
      if (!is.null(num) && length(num) > 0) {
        min <- input$idMinute
        sec <- input$idSeconde
        
        # Vérifier si les valeurs d'entrée sont valides
        if (min >= 0 & sec >= 0 ) {
          # Récupérer les données actuelles
          current_data <- as.data.frame(data())
          
          # Récupérer du sexe et de la distance
          Temps <- V_sexe(current_data[num, 'Sexe'])
          dist <- current_data[num, "Categorie"]
          
          # Appliquer les modifications aux données
          if (Test_cat(dist) == TRUE) {
            
            if(input$type == "temps_est"){
              current_data[num, "temps_est"] <- sec
              current_data[num, "points_estimee"] <- sprint_Spe(sec, dist, Temps)}
            else{
              
              current_data[num, "temps_reel"] <- sec
              current_data[num, "points_reel"] <- sprint_Spe(sec, dist, Temps)
            }
            
            
          } else {
            
            temps <- as.character(paste0(min, ":", sec))
            
            if(input$type == "temps_est"){
              current_data[num, "temps_est"] <- temps
              current_data[num, "points_estimee"] <- diff_minute(temps, dist, Temps)}
            else{
              current_data[num, "temps_reel"] <- temps
              current_data[num, "points_reel"] <- diff_minute(temps, dist, Temps)
            }
          }
          
          if (nchar(input$Nom) > 0){
            current_data[num, "Athlete"]<-input$Nom
          } 
          
          # Mettre à jour les données
          data(current_data)
          
          # Réinitialiser les champs d'entrée
          updateNumericInput(session, "idMinute", value = 0)
          updateNumericInput(session, "idSeconde", value = 0)
          updateTextInput(session, "Nom", value = NaN)
          
          
        } else {
          # Afficher un message d'erreur si les valeurs d'entrée sont invalides
          showModal(modalDialog(
            title = "Erreur",
            "Veuillez saisir des valeurs de temps valides."
          ))
        }
      } else {
        # Afficher un message d'erreur si le numéro de ligne est invalide
        showModal(modalDialog(
          title = "Erreur",
          "Numéro de ligne invalide."
        ))
      }
    } else {
      # Afficher un message d'erreur si les données ne sont pas disponibles
      showModal(modalDialog(
        title = "Erreur",
        "Les données ne sont pas disponibles."
      ))
    }
  })
  
  # Verification Base
  
  observeEvent(input$Verification,{
    if (!is.null(data())) {
      data(update_don(data()))
    }else {
      # Afficher un message d'erreur si les données ne sont pas disponibles
      showModal(modalDialog(
        title = "Erreur",
        "Les données ne sont pas disponibles."
      ))
    }
  })
  
  # Rendre le DataTable
  output$myDataTable <- renderDT({
    datatable(data(), extensions = c("Buttons"),selection = 'single',  # Activer l'extension Buttons pour les boutons d'exportation
              options = list(
                dom = 'Bfrtip',  # Placement des boutons
                buttons = c('copy','excel', 'pdf', 'print'), 
                # Boutons disponibles
                lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'Tout')),
                pageLength = -1
                
              ))
  })
  
  # Imporation du fichier
  observeEvent(input$file, {
    if (!is.null(input$file)) {
      if (endsWith(input$file$datapath,".csv") == TRUE){
        data(read.csv(input$file$datapath))
      } else {
        don<-read_xlsx(input$file$datapath,skip = 1)
        data(don[,-1])
      }
    }
  })
  
  # Message pour nombre de Points
  output$message <- renderText({
    total_points <- sum(data()$points_estimee, na.rm = TRUE)
    paste("Points Estimés :",total_points)
  })
  output$message2 <- renderText({
    total_points <- sum(data()$points_reel, na.rm = TRUE)
    paste("Points Actuel :",total_points)
  })
  
  
  
  
  # Graphique
  
  
  output$graphe_type_E <- renderHighchart({
    if (input$Sexe =="Tout"){
      data <- as.data.frame(data())
      don <- aggregate(points_estimee ~ type, data = data, sum)
      # Création du graphique interactif avec rAmCharts
      don %>%
        hchart("treemap", 
               hcaes(x = type, value = points_estimee, color = points_estimee)) %>%
        hc_title(text="Répartition des points selon le type de catégorie")
    } else{
      data <- as.data.frame(data())
      data<-data[data$Sexe==input$Sexe,]
      don <- aggregate(points_estimee ~ type, data = data, sum)
      # Création du graphique interactif avec rAmCharts
      don %>%
        hchart("treemap", 
               hcaes(x = type, value = points_estimee, color = points_estimee)) %>%
        hc_title(text="Répartition des points selon le type de catégorie")
    }
    
    
  })
  
  output$graphe_cat_E<-renderHighchart({
    if (input$Sexe =="Tout"){
      data <- as.data.frame(data())
      data<-data[data$type==input$type_cat,]
      don <- aggregate(points_estimee ~ Categorie, data = data, sum)
      # Création du graphique interactif avec rAmCharts
      don %>%
        hchart("treemap", 
               hcaes(x = Categorie, value = points_estimee, color = points_estimee)) %>%
        hc_title(text=ifelse(input$type_cat=="Marche",paste("Points selon la discipline pour la",input$type_cat),
                             paste("Nombre de points selon la discipline pour le",input$type_cat)
        ))
    } else{
      data <- as.data.frame(data())
      data<-data[data$Sexe==input$Sexe & data$type==input$type_cat,]
      
      don <- aggregate(points_estimee ~ Categorie, data = data, sum)
      # Création du graphique interactif avec rAmCharts
      don %>%
        hchart("treemap", 
               hcaes(x = Categorie, value = points_estimee, color = points_estimee)) %>%
        hc_title(text=ifelse(input$type_cat=="Marche",paste("Points selon la discipline pour la",input$type_cat),
                             paste("Nombre de points selon la discipline pour le",input$type_cat)
        ))
    }
    
    
  })
  
  

  output$graphe_sexe_E <- renderPlotly({
    data <- as.tibble(data())  
    couleurs_sexe <- c("F" = "#e377c2", "H" = "#17becf")
    
    # Utiliser plot_ly avec les données et spécifier la couleur des tranches
    plot_ly(data, labels = ~Sexe, values = ~points_estimee, type = 'pie', 
            marker = list(colors = couleurs_sexe[as.character(data$Sexe)], 
                          line = list(color = '#FFFFFF', width = 1))) %>% 
      layout(title = "Points selon le sexe") %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  output$graphe_type_E_2 <- renderHighchart({
    if (input$Sexe_2 =="Tout"){
      data <- as.data.frame(data())
      don <- aggregate(points_reel ~ type, data = data, sum)
      # Création du graphique interactif avec rAmCharts
      don %>%
        hchart("treemap", 
               hcaes(x = type, value = points_reel, color = points_reel)) %>%
        hc_title(text="Répartition des points selon le type de catégorie")
    } else{
      data <- as.data.frame(data())
      data<-data[data$Sexe==input$Sexe_2,]
      don <- aggregate(points_reel ~ type, data = data, sum)
      # Création du graphique interactif avec rAmCharts
      don %>%
        hchart("treemap", 
               hcaes(x = type, value = points_reel, color = points_reel)) %>%
        hc_title(text="Répartition des points selon le type de catégorie")
    }
    
    
  })
  
  output$graphe_cat_E_2<-renderHighchart({
    if (input$Sexe_2 =="Tout"){
      data <- as.data.frame(data())
      data<-data[data$type==input$type_cat_2,]
      don <- aggregate(points_reel ~ Categorie, data = data, sum)
      # Création du graphique interactif avec rAmCharts
      don %>%
        hchart("treemap", 
               hcaes(x = Categorie, value = points_reel, color = points_reel)) %>%
        hc_title(text=ifelse(input$type_cat_2=="Marche",paste("Points selon la discipline pour la",input$type_cat_2),
                             paste("Nombre de points selon la discipline pour le",input$type_cat_2)
        ))
    } else{
      data <- as.data.frame(data())
      data<-data[data$Sexe==input$Sexe_2 & data$type==input$type_cat_2,]
      
      don <- aggregate(points_reel ~ Categorie, data = data, sum)
      # Création du graphique interactif avec rAmCharts
      don %>%
        hchart("treemap", 
               hcaes(x = Categorie, value = points_reel, color = points_reel)) %>%
        hc_title(text=ifelse(input$type_cat_2=="Marche",paste("Points selon la discipline pour la",input$type_cat_2),
                             paste("Nombre de points selon la discipline pour le",input$type_cat_2)
        ))
    }
    
    
  })
  
  
  output$graphe_sexe_E_2 <- renderPlotly({
    data <- as.tibble(data())  
    
    couleurs_sexe <- c("F" = "#e377c2", "H" = "#17becf")
    
    # Utiliser plot_ly avec les données et spécifier la couleur des tranches
    plot_ly(data, labels = ~Sexe, values = ~points_reel, type = 'pie', 
            marker = list(colors = couleurs_sexe[as.character(data$Sexe)], 
                          line = list(color = '#FFFFFF', width = 1))) %>% 
      layout(title = "Points selon le sexe") %>%
      config(displayModeBar = FALSE)
  })
  
  output$Graphe_Total_2 <- renderPlotly({
    
    data <- as.tibble(data())
    ind<-which(data$points_reel > 0)
    estim<-data[ind,]$points_reel
    estim<-c(estim,data[-ind,]$points_estimee)
    
    cumulative_values<-cumsum(data$points_reel[ind])
    
    plot_ly(x = seq(1:length(cumulative_values)), type = 'scatter', mode = 'lines', name = 'Actual Points', 
            y = cumulative_values )  %>% 
      add_trace(x = seq(1:nrow(data)),y=sum(estim,na.rm = TRUE), 
                mode = 'lines', name = 'Projections Points', 
                line = list(color = 'red'),hoverinfo = 'y') %>% 
      add_trace(x = seq(1:nrow(data)),y=sum(data$points_estimee,na.rm = TRUE), 
                mode = 'lines', name = 'Points Estimés', 
                line = list(color = 'black',dash = 'dash'),hoverinfo = 'y') %>% 
      layout(title = "Evolution des points") 
    
    
  })
  
  
  
  ######################################
  # Onglet visualisation des licenciés #
  ######################################
  
  # Transformation en numérique
  df$long <- as.numeric(df$long)
  df$lat <- as.numeric(df$lat)
  df$Points <- as.numeric(df$Points)
  df$Licencies <- as.numeric(df$Licencies)
  
  # Ajout d'une colonne pour le classement des clubs aux tournois inter-clubs
  df <- df[order(df$Points, decreasing = TRUE), ]
  df$Classement <- seq_len(nrow(df))
  
  # Trouver les valeurs minimales et maximales de Licencies
  min_licencies <- min(df$Licencies, na.rm = TRUE)
  max_licencies <- max(df$Licencies, na.rm = TRUE)
  
  # Créer la palette de couleurs avec le nouveau domaine
  ColorPal2 <- colorNumeric(scales::seq_gradient_pal(low = "#d0d9ec", high = "#2c56ab", space = "Lab"), domain = c(min_licencies, max_licencies))
  
  
  # Initialisation de l'objet output$circulaire_sexe en dehors de observe
  output$circulaire_sexe <- renderPlotly({
    ggplot() + theme_void()  # Vous pouvez personnaliser cela en fonction de vos besoins
  })
  
  
  
  # Enregistrement du nom du club lorsque l'on clique dessus
  observe({
    click <- input$carte_marker_click
    if (!is.null(click)) {
      club<-click$id
      l<-which(df$Nom==club)
      
      data_ligne<-df[l,]
      
      data_licencie<-data.frame(Sexe=c('H',"F"),Val=c(data_ligne$Homme,data_ligne$Femme))
      
      couleurs_sexe <- c("F" = "#e377c2", "H" = "#17becf")
      
      g<-plot_ly(data_licencie, labels = ~Sexe, values = ~Val, type = 'pie',
                 marker = list(colors = couleurs_sexe[as.character(data_licencie$Sexe)], 
                               line = list(color = '#FFFFFF', width = 1))) %>% 
        layout(title = "Licenciés dans le club en fonction du sexes") 
      
      # Création d'un texte qui donne le nombre de points du club et son classement 
      if (!is.na(data_ligne$Points)){
        dynamic_text <- paste("Le club de ", as.character(data_ligne$Nom), " à un score de ", as.character(data_ligne$Points),
                              " points, il est classé ", as.character(data_ligne$Classement), "ème.")
      } else {
        dynamic_text <- paste("Le club de ", as.character(data_ligne$Nom), " n'a pas encore de score, il ne participe pas encore aux compétitions inter-club.")
      }
      
      # Affichage du graphique circulaire
      output$circulaire_sexe <- renderPlotly({
        g
      })
      
      # Affichage des points et du classement du club
      output$dynamic_text <- renderText({
        dynamic_text
      })
      
    }
  })
  
  
  # Créer la carte Leaflet
  output$carte <- renderLeaflet({
    leaflet(data = df) %>% 
      addTiles() %>%
      addCircleMarkers(
        ~ long, ~ lat,
        stroke = FALSE, fillOpacity = 0.7,
        color = ~ColorPal2(Licencies),
        popup = ~ paste("Le club de", as.character(Nom), "compte", as.character(Licencies), "licenciés", sep = " "),
        layerId = ~Nom
      ) %>%
      setView(lng = -2.795212, lat = 48.21251, zoom = 7)
  })
  
  
  
  # Histogramme et boxplot des licenciés en fonction des départements
  # Affichage du choix sélectionné  output$histo_licencie <- renderPlotly({
  
  observeEvent(input$bouton_mise_a_jour,{
    # Mise à jour des données souhaitées pour le boxplot et l'histogramme
    if (input$choix_dep == 0) {
      df_dep <- df
      titre <- "Bretagne"
    } else {
      df_dep <- df[df$Dpt. == input$choix_dep, ]
      if (input$choix_dep == 35) {
        titre <- "Ille-et-Vilaine"
      } else if (input$choix_dep == 22) {
        titre <- "Côtes-d'Armor"
      } else if (input$choix_dep == 29) {
        titre <- "Finistère"
      } else if (input$choix_dep == 56) {
        titre <- "Morbihan"
      }
    }
    
    # Mise à jour des données souhaitées pour faciliter l'analyse du boxplot
    if (input$choix == 0) {
      df_dep_seuil <- df_dep
    } else if (input$choix == 1) {
      df_dep_seuil <- df_dep[!is.na(df_dep$Points), ]
    } else if (input$choix == 2) {
      df_dep_seuil <- df_dep[is.na(df_dep$Points),]
    }
    
    
    # Histogramme des licenciés
    output$histo_licencie <- renderPlotly({
      x <- df_dep_seuil$Licencies
      
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      ggplot(df_dep_seuil, aes(x)) +
        geom_histogram(fill = "#d0d9ec", color = "#2c56ab", breaks = bins) +
        theme_minimal() +
        ggtitle(paste("Histogramme des Licenciés en", titre)) +
        xlab("Licenciés") +
        ylab("Fréquence") 
    })
    
    # Boxplot des licenciés en fonction des départements
    output$boxplot_licencie <- renderAmCharts({
      amBoxplot(Licencies ~ Dpt.,
                data = df_dep_seuil,
                main = paste("Boxplot des licenciés en ", titre),
                xlab = "Département",
                ylab = "Licenciés",
                col = "#d0d9ec",
                border = "#2c56ab")
      
      
    })
  })
}











# Run the application 
shinyApp(ui = ui, server = server)





















