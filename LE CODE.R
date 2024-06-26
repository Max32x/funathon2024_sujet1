# Charger le package yaml
library(yaml)
library(httr)
library(dplyr)
library(jsonlite)

rm(list = ls())


get_travel_time_api_response <- function(endpoint, request_body) {
  secrets <- yaml.load_file("secrets.yaml")
  X_API_ID <- secrets$travelTime$X_API_ID
  X_API_KEY <- secrets$travelTime$X_API_KEY
  
  
  headers <- add_headers(
    "X-Application-Id" = X_API_ID,
    "X-Api-Key" = X_API_KEY,
    "Content-Type" = "application/json"
  )
  
  
  response <- POST(endpoint, headers, body = request_body)
  
  print(status_code(response))
  
  return((response))
}


# Tester la fonction avec la même requête que précédemment
endpoint <- "https://api.traveltimeapp.com/v4/routes"
request_body <- '
{
  "locations": [
    {
      "id": "point-from",
      "coords": {
        "lat": 51.5119637,
        "lng": -0.1279543
      }
    },
    {
      "id": "point-to-1",
      "coords": {
        "lat": 51.5156177,
        "lng": -0.0919983
      }
    }
  ],
  "departure_searches": [
    {
      "id": "departure-search",
      "transportation": {
        "type": "public_transport"
      },
      "departure_location_id": "point-from",
      "arrival_location_ids": [
        "point-to-1"
      ],
      "departure_time": "2024-06-25T07:00:00.000Z",
      "properties": [
        "travel_time",
        "route"
      ],
      "range": {
        "enabled": true,
        "max_results": 5,
        "width": 900
      }
    }
  ]
}'
  
  response <- get_travel_time_api_response(endpoint, request_body)
  
  print(content(response, "parsed"))
  
  # Affecter la liste des descriptions des itinéraires trouvés à une variable list_itinerary
  list_itinerary <- content(response)$results[[1]]$locations[[1]]$properties
  
  
  print(list_itinerary)
  
  
  ### PARTIE 2
  
  # data_gares <- read.csv2("https://static.data.gouv.fr/41/363ec0e431633eea7e187053a2e3220c2fc9f3900fa50e7179f6ca58c607c2.csv")
  
  get_coordinates <- function(gare_name) {
    data_gares <- read.csv2(
      "https://ressources.data.sncf.com/api/explore/v2.1/catalog/datasets/liste-des-gares/exports/csv"
    )
    
    if (gare_name == "Strasbourg-Ville") {
      return(c(48.584488, 7.735626))
    } else {
      gare <- data_gares %>%
        filter(libelle == gare_name)
      
      gare <- gare[1, ]
      
      # Vérifier si la gare existe
      if (nrow(gare) == 0) {
        return(paste("La gare", gare_name, "n'a pas été trouvée."))
      }
      
      return(c(as.numeric(gare$y_wgs84), as.numeric(gare$x_wgs84)))
      
    }
  }
  
  # Tester la fonction avec la gare de Toulouse-Matabiau
  coordinates <- get_coordinates("Dunkerque")
  print(coordinates)
  
  
  
  # Définir la fonction pour générer le corps de la requête JSON
  generate_routing_request <- function(coords1, coords2) {
    request_body <- sprintf(
      '{
    "locations": [
      {
        "id": "point-from",
        "coords": {
          "lat": %f,
          "lng": %f
        }
      },
      {
        "id": "point-to-1",
        "coords": {
          "lat": %f,
          "lng": %f
        }
      }
    ],
    "departure_searches": [
      {
        "id": "departure-search",
        "transportation": {
          "type": "public_transport",
          "walking_time": 900,
          "cycling_time_to_station": 100,
          "parking_time": 0,
          "boarding_time": 0,
          "driving_time_to_station": 1800,
          "pt_change_delay": 0,
          "disable_border_crossing": false
        },
        "departure_location_id": "point-from",
        "arrival_location_ids": [
          "point-to-1"
        ],
        "departure_time": "2024-06-26T18:00:00.000Z",
        "properties": [
          "travel_time",
          "route"
        ],
        "range": {
          "enabled": true,
          "max_results": 5,
          "width": 43200
        }
      }
    ]
  }',
      coords1[1],
      coords1[2],
      coords2[1],
      coords2[2]
    )
    return(request_body)
  }
  
  
  #
  # # Tester la fonction avec des coordonnées d'exemple
  # coords1 <- c(51.5119637, -0.1279543) # Lieu de départ
  # coords2 <- c(51.5156177, -0.0919983) # Lieu d'arrivée
  # json_request <- generate_routing_request(coords1, coords2)
  #
  
  
  #FONCTION 3
  
  get_travel_time <- function(gare_name1, gare_name2) {
    # Obtenir les coordonnées des gares
    coords1 <- get_coordinates(gare_name1)
    coords2 <- get_coordinates(gare_name2)
    
    
    if (gare_name1 == gare_name2) {
      return (NA)
    }
    
    
    # Générer le corps de la requête JSON
    json_request <- generate_routing_request(coords1, coords2)
    
    
    endpoint <- "https://api.traveltimeapp.com/v4/routes"
    print("envoi requete")
    response <- get_travel_time_api_response(endpoint, json_request)
    
    
    if (status_code(response) == 429) {
      cat("Trop de requêtes, attente d'une minute...\n")
      Sys.sleep(60)
      return(get_travel_time(gare_name1, gare_name2))
    }
    
    print(response)
    
    list_itinerary <- content(response)$results[[1]]$locations[[1]]$properties
    
    if (length(list_itinerary) == 0) {
      return(Inf)
    }
    
    travel_times <- sapply(list_itinerary, function(item)
      item$travel_time)
    travel_time <- min(travel_times) / 3600
    
    
    return(travel_time)
  }
  
  
  # get_travel_time("Lyon-Perrache",  "Strasbourg-Ville")
  
  
  library(RCurl)
  getCurlOptionsConstants()[["connecttimeout"]]
  myOpts <- curlOptions(connecttimeout = 200)
  
  STATIONS <- c(
    "Paris-Nord",
    "Lyon-Perrache",
    "Marseille-St-Charles",
    "Toulouse-Matabiau",
    "Lille-Flandres",
    "Bordeaux-St-Jean",
    "Nice-Ville",
    "Nantes",
    "Strasbourg-Ville",
    "Montpellier-St-Roch"
  )
  
  travel_times_matrix <- matrix(NA, nrow = 10, ncol = 10)
  
  # Boucles pour calculer les temps de trajet entre chaque paire de stations
  for (i in 1:10) {
    print("i----")
    print(i)
    for (j in 1:10) {
      print("j")
      print(j)
      if (i < j) {
        travel_time <- get_travel_time(STATIONS[i], STATIONS[j])
        travel_times_matrix[i, j] <- travel_time
        travel_times_matrix[j, i] <- travel_time
      }
    }
  }
  
  
  # Affichage de la matrice des temps de trajet
  print(travel_times_matrix)
  
  
  
  
  THRESHOLD <- 4.5
  # On garde seulement la matrice triangulaire inférieur (car on la supposé symmétrique)
  lower_tri_matrix <- lower.tri(travel_times_matrix)
  
  # On extrait les indices où la condition n'est pas respectée
  under_threshold_indices <- which(travel_times_matrix < THRESHOLD &
                                     lower_tri_matrix, arr.ind = TRUE)
  
  # On crée une liste de paires qui remplissent les conditions
  under_threshold_routes <- mapply(
    function(i, j)
      c(STATIONS[i], STATIONS[j]),
    i = under_threshold_indices[, 1],
    j = under_threshold_indices[, 2],
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  
  
  
  
  get_air_traffic_between_cities <- function(city1, city2) {
    #### 2022
    traffic_aerien_raw <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/af8950bc-e90a-4b7e-bb81-70c79d4c3846")
    
    #### 2019
    # traffic_aerien_raw <- read.csv2(
    #   "https://www.data.gouv.fr/fr/datasets/r/0c0a451e-983b-4f06-9627-b5ff1bccd2fc"
    # )
    
    total_traffic <- traffic_aerien_raw %>%
      filter((
        grepl(tolower(city1), tolower(LSN_DEP_NOM), ignore.case = TRUE) &
          grepl(tolower(city2), tolower(LSN_ARR_NOM), ignore.case = TRUE)
      ) |
        (
          grepl(tolower(city2), tolower(LSN_DEP_NOM), ignore.case = TRUE) &
            grepl(tolower(city1), tolower(LSN_ARR_NOM), ignore.case = TRUE)
        )) %>%
      summarise(PKT = sum(as.numeric(LSN_DIST) * as.numeric(LSN_PAX_loc), na.rm = TRUE))
    
    
    return(as.numeric(total_traffic$PKT))
  }
  
  
  
  # get_air_traffic_between_cities("Nantes", "Bordeaux")
  
  total_air_traffic <- 0
  
  # Boucle pour parcourir chaque couple de gares
  for (couple in under_threshold_routes) {
    gare1 <- strsplit(couple[1], "-")[[1]]
    city1 <- gare1[1]
    
    gare2 <- strsplit(couple[2], "-")[[1]]
    city2 <- gare2[1]
    
    
    # Calculer le trafic aérien pour chaque paire
    air_traffic <- get_air_traffic_between_cities(city1, city2)
    
    total_air_traffic <- total_air_traffic + air_traffic
    
  }
  
  total_air_traffic
  
  # On estime les émissions de CO2 en tCO2éq
  cat(
    sprintf(
      "En 2019, environ %.2f tCO2 aurait pu être évités",
      total_air_traffic *  80 / 1000000
    )
  )
  
  
  
  
  
  
  
  #### PARTIE 3 VISUalISATION
  
  
  
  data_carte <- data.frame(
    Ville_depart = character(),
    lat_dep = numeric(),
    long_dep = numeric(),
    Ville_arrive = character(),
    lat_arv = numeric(),
    long_arv = numeric(),
    co2_emis = numeric()
  )
  
  
  # Boucle pour parcourir chaque couple de gares
  for (couple in under_threshold_routes) {
    coordinates_dep <- get_coordinates(couple[1])
    coordinates_ariv <- get_coordinates(couple[2])
    
    
    gare1 <- strsplit(couple[1], "-")[[1]]
    city1 <- gare1[1]
    
    gare2 <- strsplit(couple[2], "-")[[1]]
    city2 <- gare2[1]
    
    
    # Calculer le trafic aérien pour chaque paire
    air_traffic <- get_air_traffic_between_cities(city1, city2)
    
    
    data_carte <- rbind(
      data_carte,
      data.frame(
        Ville_depart = city1,
        lat_dep = coordinates_dep[1],
        long_dep = coordinates_dep[2],
        Ville_arrive = city2,
        lat_arv = coordinates_ariv[1],
        long_arv = coordinates_ariv[2],
        co2_emis = air_traffic * 80 / 1000000
      )
    )
  }
  
  data_carte
  
  
  
  secrets <- yaml.load_file("secrets.yaml")
  STADIA_API_KEY <- secrets$stadiaMaps$API_KEY
  TILES_URL <- sprintf("https://tiles.stadiamaps.com/tiles/outdoors/{z}/{x}/{y}{r}.png?api_key=%s", STADIA_API_KEY)
  TILES_URL
  
  library(leaflet)
  
  map <- leaflet() %>% 
    addTiles(urlTemplate = TILES_URL)
  
  map 
  
  # Ajouter les marqueurs de cercle pour chaque ville
  for (i in 1:nrow(data_carte)) {
    # Marqueurs de cercle pour les villes de départ
    map <- map %>%
      addCircleMarkers(
        lng = data_carte$long_dep[i],
        lat = data_carte$lat_dep[i],
        radius = 5,  # Taille du marqueur
        color = "blue",  # Couleur du marqueur
        fillOpacity = 0.8,  # Opacité de remplissage
        popup = data_carte$Ville_depart[i]  # Popup au survol du marqueur
      ) %>%
      # Marqueurs de cercle pour les villes d'arrivée
      addCircleMarkers(
        lng = data_carte$long_arv[i],
        lat = data_carte$lat_arv[i],
        radius = 5,
        color = "blue",
        fillOpacity = 0.8,
        popup = data_carte$Ville_arrive[i]
      )
  }
  
  # Boucle pour ajouter les lignes repérées avec addPolylines
  for (i in 1:nrow(data_carte)) {
    popup_text <- paste("CO2 émis en 2019 : ", data_carte$co2_emis[i], "tonnes")
    
    if (data_carte$co2_emis[i] > 0) {
      # Lignes avec émissions non-nulles (rouge, épaisseur proportionnelle)
      map <- map %>%
        addPolylines(
          lng = c(data_carte$long_dep[i], data_carte$long_arv[i]),
          lat = c(data_carte$lat_dep[i], data_carte$lat_arv[i]),
          color = "red",
          weight = sqrt(data_carte$co2_emis[i]) / 10,  # Épaisseur proportionnelle aux émissions
          popup = popup_text  # Popup affichant le CO2 émis
        )
    } else {
      # Lignes avec émissions nulles (noir, trait fin)
      map <- map %>%
        addPolylines(
          lng = c(data_carte$long_dep[i], data_carte$long_arv[i]),
          lat = c(data_carte$lat_dep[i], data_carte$lat_arv[i]),
          color = "black",
          weight = 1,  # Trait fin pour les émissions nulles
          popup = popup_text  # Popup affichant le CO2 émis
        )
    }
  }
  
  # Afficher la carte
  map
  
  
  