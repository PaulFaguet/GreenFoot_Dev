library(readxl) #lire les fichiers xlsx
library(sf) #création des coordonnées gps
library(tidyverse) 
library(dplyr)
library(shiny)
library(shinydashboard) #interface utilisateur
library(leaflet) #création de la map et des filtres
 
######### INITIALISATION DES DONNÉES ######### 
european_stadiums <- read_excel("/Users/paulfaguet/Desktop/Stade_europeen_final.xlsx") #Stades européens (Fr, All, Ang, It, Esp)
european_cities <- read_excel("/Users/paulfaguet/Desktop/cities_coords_final_version.xlsx")


#Jointure pour ne garder que les villes qui contiennent un stade et les stades qui sont dans une ville
cities_joined_stadiums <- inner_join(european_stadiums, european_cities, by = c('ville' = 'ville')) 
cities_joined_stadiums <- cities_joined_stadiums %>%
  select(nom, ville, club, capacite, pays.x, date_de_renovation, date_de_realisation, latitude, longitude, coords)
cities_joined_stadiums <- cities_joined_stadiums %>%
  filter(!is.na(cities_joined_stadiums$capacite)) #Suppression des valeurs NA pour la colonne "capacité"

data_cities_stadiums_coords <- cities_joined_stadiums %>% #Création des coordonnées (colonne "geometry")
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326)

data_cities_stadiums_coords <- data_cities_stadiums_coords %>% #Création de la popup d'information pour chaque stade
  mutate(popup_information = paste(
                            '<b>', 'Ville :', '</b>', data_cities_stadiums_coords$ville, '<br/>',
                            '<b>', 'Club :', '</b>', data_cities_stadiums_coords$club, '<br/>',
                            '<b>', 'Nom du stade :', '</b>', data_cities_stadiums_coords$nom,'<br/>',
                            '<b>', 'Capacité :', '</b>', data_cities_stadiums_coords$capacite, '<br/>',
                            '<b>', 'Date de construction :', '</b>', data_cities_stadiums_coords$date_de_realisation, '<br/>',
                            '<b>', 'Date de rénovation :', '</b>', data_cities_stadiums_coords$date_de_renovation, '<br/>')
         )


#Création du vecteur qui recense tous les pays présents dans la base de données
countries <- c()
for (i in 1:length(data_cities_stadiums_coords$pays.x)) {
  countries <- c(countries, data_cities_stadiums_coords$pays.x[i])
}
countries <- levels(factor(countries))
################################ 


#football_icon <- makeIcon(
  #iconUrl = '/Users/paulfaguet/Desktop/football.svg',
  #iconUrl = '/Users/paulfaguet/Desktop/unicons-line-6563ff/map-marker-info.svg',
  #iconWidth = 30, 
  #iconHeight = 30
#)


ui <- dashboardPage(
  
  dashboardHeader(title = 'GREENFOOT'), #En-tête de la page
  dashboardSidebar( #Barre du côté (partie noire)
    #Slider pour filtrer la capaicté des stades
    sliderInput('range', 'Capacité des stades (en milliers)', min = min(cities_joined_stadiums$capacite), max = max(cities_joined_stadiums$capacite), value = c(min(cities_joined_stadiums$capacite), 80000), step = 1000),
    #Choix des pays à visualiser
    checkboxGroupInput('countries', 'Choisissez des pays à visualiser', c('Allemagne', 'France', 'Italie'), selected = 'France'),
    #Slider pour le choix de la construction des stades
    sliderInput('construction', 'La date de construction du stade', 1906, 2022, value = 1910, step = 1)
  ),
  dashboardBody( #Corps (partie blanche) de l'interface
    leafletOutput('map', width = '100%', height = 650)
  )
  
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({ #Fonction d'intéractivité de la map
    data_cities_stadiums_coords %>%
      filter((capacite >= input$range[1] & capacite <= input$range[2]) & pays.x %in% input$countries & date_de_realisation >= input$construction)
  })
 
    observe({
    leafletProxy('map', data = filtered_data()) %>%
      addProviderTiles(providers$OpenStreetMap.France) %>% #Style du fond de carte
      addMiniMap(tiles = providers$OpenStreetMap.France, toggleDisplay = TRUE) %>% #Ajoute une mini map  
      clearMarkers() %>% #Efface les précédents markers pour remettre la map vierge
      addCircleMarkers( #Type de markers
        #icon = football_icon,
        radius = 7, #Rayon des markers
        color = "blue", #Couleur des markers
        stroke = FALSE, #Bordure des markers
        opacity = 0.8,
        fillOpacity = 0.4, #Opacité de la couleur
        popup = ~popup_information #Informations pour la popup selon le stade choisi
      )
  })
  
  output$map <- renderLeaflet({
    map <- leaflet() %>% setView(lng = 8.227512, lat = 46.818188, zoom = 5) #Coordonnéess de l'Europe
  })
}

shinyApp(ui, server)
