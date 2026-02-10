################## test pour récuperer les données avec l' api_key ###############################


library(httr)
library(jsonlite)


readRenviron(".Renviron")
Sys.getenv("TMDB_API_KEY")

api_key <- Sys.getenv("TMDB_API_KEY")

get_tmdb_id <- function(imdb_id, api_key) {
  url <- paste0(
    "https://api.themoviedb.org/3/find/",
    imdb_id,
    "?api_key=", api_key,
    "&external_source=imdb_id"
  )
  res <- fromJSON(content(GET(url), "text"))
  if (length(res$movie_results) > 0) {
    res$movie_results$id[1]
  } else {
    NA
  }
}
get_movie_details <- function(tmdb_id, api_key) {
  url <- paste0(
    "https://api.themoviedb.org/3/movie/",
    tmdb_id,
    "?api_key=", api_key,
    "&language=fr-FR"
  )
  fromJSON(content(GET(url), "text"))
}

get_providers <- function(tmdb_id, api_key, country = "FR") {
  url <- paste0(
    "https://api.themoviedb.org/3/movie/",
    tmdb_id,
    "/watch/providers?api_key=", api_key
  )
  res <- fromJSON(content(GET(url), "text"))
  
  if (!is.null(res$results[[country]]$flatrate)) {
    paste(res$results[[country]]$flatrate$provider_name, collapse = ", ")
  } else {
    "Non disponible en streaming"
  }
}

#####test sur la console pour voir si cela fonctionne ####### 
imdb_id <- "tt0133093"

tmdb_id <- get_tmdb_id(imdb_id, api_key)

if (!is.na(tmdb_id)) {
  details <- get_movie_details(tmdb_id, api_key)
  providers <- get_providers(tmdb_id, api_key)
  
  list(
    titre = details$title,
    resume = details$overview,
    plateformes = providers
  )
} else {
  "Film introuvable sur TMDb"
}






########################## Script a utiliser sur shiny #####################

############# ATTENTION: il faudra ajouter dans je scrit de la page 2: une partie où l'utilisateur sélectionne un film. Par exemple, avec un actionButton ou un observeEvent sur le clic d'une ligne de tableau.
# de plus il faut que dans la page 2 quand on selectionne la ligne ca face bien la recherche avec le code "imdb_id"

library(shiny)
library(httr)
library(jsonlite)
library(shinycssloaders)

#CHARGEMENT DE LA CLÉ API
readRenviron(".Renviron")
api_key <- Sys.getenv("TMDB_API_KEY")

# FONCTIONS API TMDB
get_tmdb_id <- function(imdb_id, api_key) {
  url <- paste0(
    "https://api.themoviedb.org/3/find/",
    imdb_id,
    "?api_key=", api_key,
    "&external_source=imdb_id"
  )
  res <- fromJSON(content(GET(url), "text"))
  if (length(res$movie_results) > 0) {
    res$movie_results$id[1]
  } else {
    NA
  }
}

get_movie_details <- function(tmdb_id, api_key) {
  url <- paste0(
    "https://api.themoviedb.org/3/movie/",
    tmdb_id,
    "?api_key=", api_key,
    "&language=fr-FR"
  )
  fromJSON(content(GET(url), "text"))
}

get_providers <- function(tmdb_id, api_key, country = "FR") {
  url <- paste0(
    "https://api.themoviedb.org/3/movie/",
    tmdb_id,
    "/watch/providers?api_key=", api_key
  )
  res <- fromJSON(content(GET(url), "text"))
  
  if (!is.null(res$results[[country]]$flatrate)) {
    paste(res$results[[country]]$flatrate$provider_name, collapse = ", ")
  } else {
    "Non disponible en streaming"
  }
}

# UI - PAGE 3
ui_page3 <- fluidPage(
  tags$head(
    tags$style(HTML("
      .movie-card {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-radius: 15px;
        padding: 30px;
        margin: 20px auto;
        max-width: 1200px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.3);
        color: white;
      }
      
      .movie-header {
        display: flex;
        gap: 30px;
        margin-bottom: 30px;
      }
      
      .movie-poster {
        flex-shrink: 0;
      }
      
      .movie-poster img {
        width: 300px;
        border-radius: 10px;
        box-shadow: 0 5px 15px rgba(0,0,0,0.4);
      }
      
      .movie-info {
        flex-grow: 1;
      }
      
      .movie-title {
        font-size: 2.5em;
        font-weight: bold;
        margin-bottom: 10px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
      }
      
      .movie-meta {
        font-size: 1.1em;
        margin-bottom: 15px;
        opacity: 0.9;
      }
      
      .movie-rating {
        display: inline-block;
        background: rgba(255,255,255,0.2);
        padding: 8px 15px;
        border-radius: 20px;
        font-weight: bold;
        margin-right: 10px;
      }
      
      .movie-section {
        margin-top: 25px;
      }
      
      .section-title {
        font-size: 1.5em;
        font-weight: bold;
        margin-bottom: 10px;
        border-bottom: 2px solid rgba(255,255,255,0.3);
        padding-bottom: 5px;
      }
      
      .section-content {
        font-size: 1.1em;
        line-height: 1.6;
        text-align: justify;
      }
      
      .genre-tag {
        display: inline-block;
        background: rgba(255,255,255,0.2);
        padding: 5px 12px;
        border-radius: 15px;
        margin: 5px;
        font-size: 0.9em;
      }
      
      .provider-info {
        background: rgba(255,255,255,0.15);
        padding: 15px;
        border-radius: 10px;
        margin-top: 10px;
      }
      
      .back-button {
        margin: 20px;
      }
      
      .loading-spinner {
        text-align: center;
        padding: 50px;
        font-size: 1.5em;
      }
    "))
  ),
  
  # Bouton retour
  div(class = "back-button",
      actionButton("back_to_results", 
                   "← Retour aux résultats", 
                   icon = icon("arrow-left"),
                   class = "btn btn-primary btn-lg")
  ),
  
  # Contenu du film
  uiOutput("movie_details_ui") %>% withSpinner(color = "#667eea")
)

#  SERVER - PAGE 3 
server_page3 <- function(input, output, session, selected_movie) {
  
  # Reactive pour récupérer les détails du film
  movie_data <- reactive({
    req(selected_movie())
    
    imdb_id <- selected_movie()$tconst
    
    # Récupérer l'ID TMDB
    tmdb_id <- get_tmdb_id(imdb_id, api_key)
    
    if (is.na(tmdb_id)) {
      return(list(error = "Film non trouvé dans TMDB"))
    }
    
    # Récupérer les détails
    details <- get_movie_details(tmdb_id, api_key)
    
    # Récupérer les plateformes
    providers <- get_providers(tmdb_id, api_key, "FR")
    
    list(
      tmdb_id = tmdb_id,
      details = details,
      providers = providers,
      imdb_data = selected_movie()
    )
  })
  
  # Affichage de la fiche film
  output$movie_details_ui <- renderUI({
    data <- movie_data()
    
    if (!is.null(data$error)) {
      return(
        div(class = "movie-card",
            h3("Erreur"),
            p(data$error),
            p("Informations disponibles :"),
            p(strong("Titre : "), data$imdb_data$title),
            p(strong("Année : "), data$imdb_data$year),
            p(strong("Genres : "), data$imdb_data$genres_list)
        )
      )
    }
    
    details <- data$details
    imdb_data <- data$imdb_data
    
    # URL de l'affiche
    poster_url <- if (!is.null(details$poster_path)) {
      paste0("https://image.tmdb.org/t/p/w500", details$poster_path)
    } else {
      "https://via.placeholder.com/300x450?text=Pas+d%27affiche"
    }
    
    # Genres sous forme de tags
    genres_tags <- if (!is.null(details$genres)) {
      lapply(details$genres$name, function(g) {
        span(class = "genre-tag", g)
      })
    } else {
      span(class = "genre-tag", imdb_data$genres_list)
    }
    
    # Note sur 10
    rating <- if (!is.null(details$vote_average)) {
      round(details$vote_average, 1)
    } else {
      imdb_data$rating
    }
    
    # Construction de la carte
    div(class = "movie-card",
        # Header avec affiche et infos principales
        div(class = "movie-header",
            div(class = "movie-poster",
                img(src = poster_url, alt = details$title)
            ),
            div(class = "movie-info",
                div(class = "movie-title", details$title),
                div(class = "movie-meta",
                    span(class = "movie-rating", 
                         icon("star"), " ", rating, "/10"),
                    span(imdb_data$year),
                    span(" • "),
                    span(imdb_data$runtime, " min")
                ),
                div(class = "movie-section",
                    div(class = "section-title", "Genres"),
                    div(genres_tags)
                ),
                div(class = "movie-section",
                    div(class = "section-title", "Disponibilité"),
                    div(class = "provider-info",
                        icon("tv"), " ", data$providers
                    )
                )
            )
        ),
        
        # Synopsis
        div(class = "movie-section",
            div(class = "section-title", "Synopsis"),
            div(class = "section-content",
                if (!is.null(details$overview) && details$overview != "") {
                  details$overview
                } else {
                  "Aucun synopsis disponible."
                }
            )
        ),
        
        # Informations supplémentaires
        if (!is.null(details$tagline) && details$tagline != "") {
          div(class = "movie-section",
              div(class = "section-title", "Phrase d'accroche"),
              div(class = "section-content", em(details$tagline))
          )
        }
    )
  })
  
  return(list(
    back_button = reactive(input$back_to_results)
  ))
}

# ===== EXEMPLE D'INTÉGRATION DANS L'APP COMPLÈTE =====
# (À adapter selon votre structure existante)

ui <- navbarPage(
  "Whatch ?",
  
  # Page 1 - Accueil
  tabPanel("Accueil",
           # ... votre code page 1
  ),
  
  # Page 2 - Sélection
  tabPanel("Recommandations",
           # ... votre code page 2
  ),
  
  # Page 3 - Détails (conditionnelle)
  conditionalPanel(
    condition = "output.show_details",
    ui_page3
  )
)

server <- function(input, output, session) {
  
  # Variable réactive pour stocker le film sélectionné
  selected_movie <- reactiveVal(NULL)
  
  # Indicateur pour afficher/masquer la page 3
  output$show_details <- reactive({
    !is.null(selected_movie())
  })
  outputOptions(output, "show_details", suspendWhenHidden = FALSE)
  
  # Quand l'utilisateur clique sur un film dans la page 2
  observeEvent(input$select_movie, {
    # Récupérer la ligne du film depuis votre tableau de données
    film <- your_movies_data[input$select_movie, ]
    selected_movie(film)
  })
  
  # Server de la page 3
  page3_outputs <- server_page3(input, output, session, selected_movie)
  
  # Retour aux résultats
  observeEvent(page3_outputs$back_button(), {
    selected_movie(NULL)
  })
}

shinyApp(ui, server)