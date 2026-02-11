

library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(httr)
library(jsonlite)
library(shinycssloaders)

# Config API

ensure_imdb_data <- function(
  processed_path = "data/processed/movies_final.rds",
  raw_dir = "data/raw",
  processed_dir = "data/processed",
  min_votes = 100,
  timeout_sec = 1200
) {
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

  # 1) Si déjà prêt, on ne fait rien
  if (file.exists(processed_path)) return(invisible(TRUE))

  options(timeout = timeout_sec)

  url_basics  <- "https://datasets.imdbws.com/title.basics.tsv.gz"
  url_ratings <- "https://datasets.imdbws.com/title.ratings.tsv.gz"

  path_basics  <- file.path(raw_dir, "title.basics.tsv.gz")
  path_ratings <- file.path(raw_dir, "title.ratings.tsv.gz")
  path_movies_temp <- file.path(raw_dir, "movies_only.tsv")

  # 2) Téléchargements (si manquants)
  if (!file.exists(path_basics))  download.file(url_basics,  path_basics,  mode = "wb")
  if (!file.exists(path_ratings)) download.file(url_ratings, path_ratings, mode = "wb")

  # 3) Filtrage streaming "movie" AVANT chargement complet
  if (!file.exists(path_movies_temp)) {
    con_in  <- gzfile(path_basics, "r")
    con_out <- file(path_movies_temp, "w")

    header <- readLines(con_in, n = 1)
    writeLines(header, con_out)

    chunk_size <- 10000
    repeat {
      lines <- readLines(con_in, n = chunk_size)
      if (length(lines) == 0) break
      movie_lines <- grep("\tmovie\t", lines, value = TRUE)
      if (length(movie_lines) > 0) writeLines(movie_lines, con_out)
    }

    close(con_in)
    close(con_out)
  }

  # 4) Lecture + nettoyage + fusion
  suppressPackageStartupMessages({
    library(data.table)
    library(dplyr)
    library(stringr)
  })

  basics <- data.table::fread(path_movies_temp, na.strings = "\\N", quote = "")
  ratings <- data.table::fread(path_ratings, na.strings = "\\N")

  movies <- basics %>%
    transmute(
      tconst = tconst,
      title  = primaryTitle,
      year   = as.integer(startYear),
      runtime = as.integer(runtimeMinutes),
      genres = genres
    ) %>%
    filter(
      !is.na(year),
      !is.na(runtime),
      year >= 1900,
      year <= as.integer(format(Sys.Date(), "%Y")),
      runtime >= 40,
      runtime <= 300,
      !is.na(genres)
    ) %>%
    inner_join(
      ratings %>% transmute(
        tconst = tconst,
        rating = as.numeric(averageRating),
        votes  = as.integer(numVotes)
      ),
      by = "tconst"
    ) %>%
    filter(votes >= min_votes) %>%
    mutate(
      genres_list = str_split(genres, ",")
    )

  # 5) Scores attendus par ton app (normalisation simple 0-1)
  # - popularity_score : log(votes) normalisé
  # - quality_score    : rating normalisée (sur 10)
  # (Tu peux raffiner ensuite, mais au moins ton app tourne.)
  movies <- movies %>%
    mutate(
      quality_score = pmin(pmax(rating / 10, 0), 1),
      popularity_score = {
        lv <- log1p(votes)
        (lv - min(lv, na.rm = TRUE)) / (max(lv, na.rm = TRUE) - min(lv, na.rm = TRUE) + 1e-9)
      }
    )

  saveRDS(movies, processed_path)
  invisible(TRUE)
}


readRenviron(".Renviron")
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

#Config "émotions"

EMOTION_CONFIG <- list(
  "rire" = list(
    label = "Rire", description = "Comédies légères et humoristiques",
    genres = c("Comedy"), exclude_genres = c("Drama", "War", "Horror"),
    alpha_preference = 0.6, rating_min = 6.5, duration_preference = "moyen",
    category = "positive"
  ),
  "detente" = list(
    label = "Détente", description = "Films feel-good sans prise de tête",
    genres = c("Comedy", "Romance", "Family", "Animation"),
    alpha_preference = 0.7, rating_min = 6.8, duration_preference = "moyen",
    exclude_genres = c("Horror", "Thriller", "War", "Crime"),
    category = "positive"
  ),
  "romance" = list(
    label = "Romance", description = "Histoires d'amour et relations sentimentales",
    genres = c("Romance"), exclude_genres = c("Horror", "War"),
    alpha_preference = 0.5, rating_min = 6.5, duration_preference = "moyen",
    category = "positive"
  ),
  "voyage" = list(
    label = "Voyager", description = "Aventures et découvertes dans des lieux exotiques",
    genres = c("Adventure"), exclude_genres = c("Horror", "War"),
    alpha_preference = 0.6, rating_min = 7.0, duration_preference = "long",
    category = "positive"
  ),
  "famille" = list(
    label = "En famille", description = "Films adaptés à tous les âges",
    genres = c("Family", "Animation"),
    alpha_preference = 0.8, rating_min = 7.0, duration_preference = "moyen",
    exclude_genres = c("Horror", "Thriller", "War", "Crime"),
    category = "positive"
  ),
  
  "action" = list(
    label = "Action", description = "Films d'action avec scènes spectaculaires",
    genres = c("Action"), exclude_genres = c("Romance", "Documentary"),
    alpha_preference = 0.7, rating_min = 6.5, duration_preference = "moyen",
    category = "intense"
  ),
  "peur" = list(
    label = "Peur", description = "Films d'horreur pour avoir peur",
    genres = c("Horror"), exclude_genres = c("Comedy", "Family", "Animation"),
    alpha_preference = 0.4, rating_min = 6.0, duration_preference = "moyen",
    category = "intense"
  ),
  "angoisse" = list(
    label = "Angoisse", description = "Thrillers psychologiques et suspense intense",
    genres = c("Thriller"), exclude_genres = c("Comedy", "Family", "Animation", "Romance"),
    alpha_preference = 0.3, rating_min = 7.0, duration_preference = "moyen",
    category = "intense"
  ),
  "epique" = list(
    label = "Épique", description = "Grandes fresques historiques et batailles",
    genres = c("War", "History"), exclude_genres = c("Comedy", "Romance", "Horror"),
    alpha_preference = 0.6, rating_min = 7.5, duration_preference = "long",
    category = "intense"
  ),
  
  "pleurer" = list(
    label = "Pleurer", description = "Drames émotionnels et histoires touchantes",
    genres = c("Drama"), exclude_genres = c("Horror", "Comedy", "Action", "Thriller", "Crime"),
    alpha_preference = 0.2, rating_min = 7.5, duration_preference = "long",
    category = "profonde"
  ),
  "reflechir" = list(
    label = "Réfléchir", description = "Documentaires et films biographiques instructifs",
    genres = c("Documentary", "Biography"), exclude_genres = c("Horror", "Comedy", "Action"),
    alpha_preference = 0.2, rating_min = 7.5, duration_preference = "any",
    category = "profonde"
  ),
  "mystere" = list(
    label = "Mystère", description = "Énigmes et enquêtes policières",
    genres = c("Mystery", "Crime"), exclude_genres = c("Horror", "Comedy"),
    alpha_preference = 0.3, rating_min = 7.0, duration_preference = "moyen",
    category = "profonde"
  ),
  "science_fiction" = list(
    label = "Science-Fiction", description = "Exploration de futurs technologiques et mondes imaginaires",
    genres = c("Sci-Fi"), exclude_genres = c("Horror", "Romance"),
    alpha_preference = 0.5, rating_min = 6.8, duration_preference = "moyen",
    category = "profonde"
  )
)

#Choix de durée

DURATION_RANGES <- list(
  "court" = c(40, 90),    # Films courts
  "moyen" = c(90, 130),   # Durée standard
  "long"  = c(130, 300),  # Films longs
  "any"   = c(40, 300)    # Toutes durées
)

# Fonctions de recommandation

compute_recommendation_score <- function(
    movies, 
    emotion_key, 
    year_range = NULL, 
    duration_pref = NULL, 
    selected_genres = NULL
) {
  config <- EMOTION_CONFIG[[emotion_key]]
  
  # 1) Filtrage de base par genres recherchés
  movies <- movies %>%
    filter(
      map_lgl(genres_list, ~any(.x %in% config$genres)),
      rating >= config$rating_min
    )
  
  # 2) Filtrage des genres à exclure
  if (!is.null(config$exclude_genres) && length(config$exclude_genres) > 0) {
    movies <- movies %>%
      filter(!map_lgl(genres_list, ~any(.x %in% config$exclude_genres)))
  }
  
  # 3) Filtrage par année
  if (!is.null(year_range)) {
    movies <- movies %>%
      filter(year >= year_range[1], year <= year_range[2])
  }
  
  # 4) Filtrage par durée
  duration_key <- duration_pref %||% config$duration_preference
  duration_bounds <- DURATION_RANGES[[duration_key]]
  if (!is.null(duration_bounds)) {
    movies <- movies %>%
      filter(runtime >= duration_bounds[1], runtime <= duration_bounds[2])
  }
  
  # 5) Filtrage par genres sélectionnés (facultatif)
  if (!is.null(selected_genres) && length(selected_genres) > 0) {
    movies <- movies %>%
      filter(map_lgl(genres_list, ~any(.x %in% selected_genres)))
  }
  
  # 6) Calcul du score de recommandation
  alpha <- config$alpha_preference
  movies <- movies %>%
    mutate(
      recommendation_score = alpha * quality_score + (1 - alpha) * popularity_score
    )
  
  return(movies)
}

recommend_random_movie <- function(
    movies, 
    emotion_key, 
    year_range = NULL, 
    duration_pref = NULL,
    selected_genres = NULL
) {
  scored_movies <- compute_recommendation_score(
    movies, emotion_key, year_range, duration_pref, selected_genres
  )
  
  if (nrow(scored_movies) == 0) return(NULL)
  
  # Sélection probabiliste pondérée par le score
  probs <- scored_movies$recommendation_score / sum(scored_movies$recommendation_score)
  idx <- sample(seq_len(nrow(scored_movies)), size = 1, prob = probs)
  
  scored_movies[idx, ]
}


# UI

ui <- navbarPage(
  title = div(
    img(src = "image.png", height = "30px", style = "margin-right: 10px; vertical-align: middle;"),
    "What'ch ?"
  ),
  theme = shinytheme("cyborg"),
  id = "navbar",
  
  tags$head(
    tags$style(HTML("
      body {
        background-image: linear-gradient(rgba(0, 0, 0, 0.7), rgba(0, 0, 0, 0.7)), url('back.jpg');
        background-size: cover;
        background-attachment: fixed;
        background-position: center;
      }
      
      .page-accueil {
        min-height: 80vh;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        text-align: center;
        color: white;
      }
      
      .titre { font-size: 80px; font-weight: bold; text-shadow: 2px 2px 10px rgba(0,0,0,1); }
      .sous-titre { font-size: 24px; margin-bottom: 30px; }
      
      .btn-start {
        font-size: 24px;
        padding: 15px 40px;
        border-radius: 50px;
        background-color: #ff6b6b;
        color: white;
        border: none;
        box-shadow: 0px 4px 15px rgba(255, 107, 107, 0.4);
      }
      
      .emotion-card {
        cursor: pointer;
        transition: all 0.3s ease;
        border: 2px solid transparent;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 15px;
        background: linear-gradient(145deg, rgba(255,255,255,0.05) 0%, rgba(255,255,255,0.02) 100%);
      }
      .emotion-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 20px rgba(0,0,0,0.3);
        border-color: #667eea;
      }
      .emotion-card.selected {
        border-color: #667eea;
        background: linear-gradient(145deg, rgba(102,126,234,0.2) 0%, rgba(118,75,162,0.2) 100%);
      }
      
      .emotion-icon {
        font-size: 48px;
        margin-bottom: 15px;
      }
      .emotion-title {
        font-size: 22px;
        font-weight: bold;
        margin-bottom: 8px;
        color: #667eea;
      }
      .emotion-desc {
        font-size: 14px;
        color: #999;
      }
      
      .category-header {
        font-size: 28px;
        font-weight: bold;
        margin: 30px 0 20px 0;
        padding-bottom: 10px;
        border-bottom: 3px solid;
        text-transform: uppercase;
      }
      .category-positive { border-color: #4CAF50; color: #4CAF50; }
      .category-intense { border-color: #FF9800; color: #FF9800; }
      .category-profonde { border-color: #9C27B0; color: #9C27B0; }
      
      .search-container {
        background: linear-gradient(145deg, rgba(102,126,234,0.1) 0%, rgba(118,75,162,0.1) 100%);
        padding: 30px;
        border-radius: 15px;
        margin-top: 30px;
      }
    "))
  ),
  
  # Page d'accueil
  tabPanel("Accueil", value = "accueil",
           div(class = "page-accueil",
               div(class = "titre", "🎬 What'ch ?"),
               div(class = "sous-titre", "Abracadabra, on choisit pour toi !"),
               actionButton("demarrer", "Commencer", class = "btn-start")
           )
  ),
  
  # Page de sélection (l'application principale)
  tabPanel("Sélection", value = "selection",
           conditionalPanel(
             condition = "!output.show_details",
             
             fluidRow(
               column(12,
                      div(style = "text-align: center; margin: 30px 0;",
                          h1(style = "color: #667eea; font-size: 3em; text-shadow: 2px 2px 4px rgba(0,0,0,0.3);",
                             icon("film"), " What'ch ?"),
                          p(style = "font-size: 1.3em; color: #999;",
                            "Laissez-nous choisir le film parfait pour votre humeur")
                      )
               )
             ),
             
             fluidRow(
               column(3,
                      div(style = "background: rgba(0,0,0,0.3); padding: 25px; border-radius: 15px; 
                           box-shadow: 0 4px 15px rgba(0,0,0,0.2);",
                          
                          h3(style = "color: #667eea; border-bottom: 2px solid #667eea; padding-bottom: 10px; 
                              margin-bottom: 20px;",
                             icon("smile"), " Votre humeur"),
                          
                          div(id = "emotion-selector",
                              div(class = "category-header category-positive", "😊 Émotions Positives"),
                              
                              div(class = "emotion-card", `data-emotion` = "rire", onclick = "selectEmotion('rire')",
                                  div(class = "emotion-icon", "😂"),
                                  div(class = "emotion-title", "Rire"),
                                  div(class = "emotion-desc", "Comédies légères")
                              ),
                              div(class = "emotion-card", `data-emotion` = "detente", onclick = "selectEmotion('detente')",
                                  div(class = "emotion-icon", "😌"),
                                  div(class = "emotion-title", "Détente"),
                                  div(class = "emotion-desc", "Films feel-good")
                              ),
                              div(class = "emotion-card", `data-emotion` = "romance", onclick = "selectEmotion('romance')",
                                  div(class = "emotion-icon", "💕"),
                                  div(class = "emotion-title", "Romance"),
                                  div(class = "emotion-desc", "Histoires d'amour")
                              ),
                              div(class = "emotion-card", `data-emotion` = "voyage", onclick = "selectEmotion('voyage')",
                                  div(class = "emotion-icon", "🌍"),
                                  div(class = "emotion-title", "Voyager"),
                                  div(class = "emotion-desc", "Aventures exotiques")
                              ),
                              div(class = "emotion-card", `data-emotion` = "famille", onclick = "selectEmotion('famille')",
                                  div(class = "emotion-icon", "👨‍👩‍👧‍👦"),
                                  div(class = "emotion-title", "En famille"),
                                  div(class = "emotion-desc", "Pour tous les âges")
                              ),
                              
                              div(class = "category-header category-intense", "🔥 Émotions Intenses"),
                              
                              div(class = "emotion-card", `data-emotion` = "action", onclick = "selectEmotion('action')",
                                  div(class = "emotion-icon", "💥"),
                                  div(class = "emotion-title", "Action"),
                                  div(class = "emotion-desc", "Scènes spectaculaires")
                              ),
                              div(class = "emotion-card", `data-emotion` = "peur", onclick = "selectEmotion('peur')",
                                  div(class = "emotion-icon", "😱"),
                                  div(class = "emotion-title", "Peur"),
                                  div(class = "emotion-desc", "Films d'horreur")
                              ),
                              div(class = "emotion-card", `data-emotion` = "angoisse", onclick = "selectEmotion('angoisse')",
                                  div(class = "emotion-icon", "😰"),
                                  div(class = "emotion-title", "Angoisse"),
                                  div(class = "emotion-desc", "Suspense intense")
                              ),
                              div(class = "emotion-card", `data-emotion` = "epique", onclick = "selectEmotion('epique')",
                                  div(class = "emotion-icon", "⚔️"),
                                  div(class = "emotion-title", "Épique"),
                                  div(class = "emotion-desc", "Grandes fresques")
                              ),
                              
                              div(class = "category-header category-profonde", "🧠 Émotions Profondes"),
                              
                              div(class = "emotion-card", `data-emotion` = "pleurer", onclick = "selectEmotion('pleurer')",
                                  div(class = "emotion-icon", "😢"),
                                  div(class = "emotion-title", "Pleurer"),
                                  div(class = "emotion-desc", "Drames émotionnels")
                              ),
                              div(class = "emotion-card", `data-emotion` = "reflechir", onclick = "selectEmotion('reflechir')",
                                  div(class = "emotion-icon", "🤔"),
                                  div(class = "emotion-title", "Réfléchir"),
                                  div(class = "emotion-desc", "Documentaires")
                              ),
                              div(class = "emotion-card", `data-emotion` = "mystere", onclick = "selectEmotion('mystere')",
                                  div(class = "emotion-icon", "🔍"),
                                  div(class = "emotion-title", "Mystère"),
                                  div(class = "emotion-desc", "Énigmes policières")
                              ),
                              div(class = "emotion-card", `data-emotion` = "science_fiction", onclick = "selectEmotion('science_fiction')",
                                  div(class = "emotion-icon", "🚀"),
                                  div(class = "emotion-title", "Science-Fiction"),
                                  div(class = "emotion-desc", "Futurs imaginaires")
                              )
                          ),
                          
                          textInput("emotion_selected", NULL, value = ""),
                          
                          tags$script(HTML("
                            function selectEmotion(emotion) {
                              $('.emotion-card').removeClass('selected');
                              $('.emotion-card[data-emotion=\"' + emotion + '\"]').addClass('selected');
                              $('#emotion_selected').val(emotion).trigger('change');
                            }
                          "))
                      )
               ),
               
               column(9,
                      div(style = "background: rgba(0,0,0,0.3); padding: 25px; border-radius: 15px; 
                           box-shadow: 0 4px 15px rgba(0,0,0,0.2);",
                          
                          fluidRow(
                            column(6,
                                   h4(style = "color: #667eea;", icon("calendar-alt"), " Période"),
                                   sliderInput("year_range", NULL,
                                               min = 1900, max = 2025, value = c(1980, 2025),
                                               sep = "", step = 1)
                            ),
                            column(6,
                                   h4(style = "color: #667eea;", icon("clock"), " Durée"),
                                   selectInput("duration", NULL,
                                               choices = c(
                                                 "Peu importe" = "any",
                                                 "Film court (40-90 min)" = "court",
                                                 "Durée standard (90-130 min)" = "moyen",
                                                 "Film long (130+ min)" = "long"
                                               ),
                                               selected = "any")
                            )
                          ),
                          
                          fluidRow(
                            column(12,
                                   h4(style = "color: #667eea; margin-top: 20px;", 
                                      icon("tags"), " Genres (optionnel)"),
                                   checkboxGroupInput("selected_genres", NULL,
                                                      choices = c(
                                                        "Action", "Adventure", "Animation", "Biography",
                                                        "Comedy", "Crime", "Documentary", "Drama",
                                                        "Family", "Fantasy", "History", "Horror",
                                                        "Music", "Mystery", "Romance", "Sci-Fi",
                                                        "Sport", "Thriller", "War", "Western"
                                                      ),
                                                      inline = TRUE)
                            )
                          ),
                          
                          div(style = "text-align: center; margin-top: 30px;",
                              actionButton("random", 
                                           label = div(icon("magic"), " Surprends-moi !"),
                                           class = "btn btn-primary btn-lg",
                                           style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                                                  border: none; padding: 15px 40px; font-size: 1.2em;
                                                  border-radius: 30px; box-shadow: 0 5px 15px rgba(102,126,234,0.4);")
                          ),
                          
                          conditionalPanel(
                            condition = "input.random > 0 && input.emotion_selected == ''",
                            div(style = "margin-top: 30px; padding: 20px; background: rgba(102,126,234,0.1); 
                                 border-radius: 10px; border-left: 4px solid #667eea;",
                                withSpinner(uiOutput("random_result"), type = 6, color = "#667eea")
                            )
                          ),
                          
                          div(class = "search-container",
                              h3(style = "color: #667eea; margin-top: 0;", 
                                 icon("search"), " Recherche manuelle"),
                              textInput("search", "Rechercher un film :",
                                        placeholder = "Titre, genre, année...",
                                        width = "100%"),
                              conditionalPanel(
                                condition = "input.search != ''",
                                div(style = "margin-top: 20px;",
                                    withSpinner(DTOutput("results_table"), type = 6, color = "#667eea")
                                )
                              )
                          )
                      )
               )
             )
           ),
           
           conditionalPanel(
             condition = "output.show_details",
             div(style = "background: white; min-height: 100vh; padding: 30px;",
                 div(style = "max-width: 1200px; margin: 0 auto;",
                     actionButton("close_details", 
                                  label = div(icon("arrow-left"), " Retour"),
                                  class = "btn btn-secondary",
                                  style = "margin-bottom: 20px;"),
                     withSpinner(uiOutput("movie_details_ui"), type = 6, color = "#667eea")
                 )
             )
           )
  )
)

# SERVER

server <- function(input, output, session) {
  
  # Navigation depuis la page d'accueil
  observeEvent(input$demarrer, {
    updateNavbarPage(session, "navbar", selected = "selection")
  })
  
  # Chargement données
  
  ensure_imdb_data()
  movies_data <- reactive({
    readRDS("data/processed/movies_final.rds")
  })
  
  # États pour la page de détails
  
  selected_movie <- reactiveVal(NULL)
  show_details_page <- reactiveVal(FALSE)
  random_movie <- reactiveVal(NULL)
  
  # Recommandation aléatoire
  
  observeEvent(input$random, {
    req(input$emotion_selected != "")
    
    year_range <- input$year_range
    duration_pref <- if (input$duration == "any") NULL else input$duration
    selected_genres <- if (length(input$selected_genres) > 0) input$selected_genres else NULL
    
    movie <- recommend_random_movie(
      movies_data(),
      input$emotion_selected,
      year_range,
      duration_pref,
      selected_genres
    )
    
    random_movie(movie)
  })
  
  output$random_result <- renderUI({
    req(random_movie())
    
    film <- random_movie()
    
    div(style = "background: white; padding: 25px; border-radius: 12px; 
         box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
        div(style = "display: flex; justify-content: space-between; align-items: start;",
            div(
              h3(style = "color: #667eea; margin-top: 0;", film$title),
              p(style = "color: #666; font-size: 1.1em;",
                icon("calendar"), " ", film$year, " | ",
                icon("clock"), " ", film$runtime, " min | ",
                icon("star"), " ", film$rating, "/10"),
              p(style = "color: #999;", 
                icon("tags"), " ", film$genres),
              p(style = "font-size: 1.2em; color: #764ba2; font-weight: bold;",
                "Score de recommandation : ", 
                round(film$recommendation_score * 100, 1), "%")
            ),
            actionButton("view_details", 
                         label = div(icon("info-circle"), " Voir les détails"),
                         class = "btn btn-primary",
                         style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                                border: none; padding: 10px 25px;")
        )
    )
  })
  
  # Recherche manuelle
  
  filtered_movies <- reactive({
    req(input$search)
    
    search_term <- tolower(input$search)
    
    movies_data() %>%
      filter(
        str_detect(tolower(title), fixed(search_term)) |
          str_detect(tolower(genres), fixed(search_term)) |
          str_detect(as.character(year), fixed(search_term))
      ) %>%
      arrange(desc(rating)) %>%
      head(100)
  })
  
  output$results_table <- renderDT({
    req(input$search)
    
    display_data <- filtered_movies() %>%
      transmute(
        Titre = title,
        Année = year,
        Durée = paste(runtime, "min"),
        Genres = genres,
        Note = rating,
        Score = round(recommendation_score * 100, 1)
      )
    
    datatable(
      display_data,
      selection = list(mode = 'single', target = 'row'),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50),
        dom = "lftip",
        ordering = TRUE,
        order = list(list(0, "desc")),
        language = list(
          search = "Rechercher :",
          lengthMenu = "Afficher _MENU_ films",
          info = "Films _START_ à _END_ sur _TOTAL_",
          paginate = list(previous = "Précédent", `next` = "Suivant"),
          zeroRecords = "Aucun film trouvé"
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Note',
        background = styleColorBar(range(filtered_movies()$rating, na.rm = TRUE), '#90EE90'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Score',
        fontWeight = 'bold',
        color = styleInterval(
          cuts = c(0.7, 0.8, 0.9),
          values = c("#666", "#1976D2", "#0D47A1", "#01579B")
        )
      )
  })
  
  #Clic pour détails de film
  
  observeEvent(input$view_details, {
    req(input$view_details)
    
    # Déterminer quelle source de données utiliser
    if (input$random > 0 && (is.null(input$emotion_selected) || input$emotion_selected == "")) {
      film <- random_movie()
    } else {
      req(input$search)
      films_data <- filtered_movies() %>% head(100)
      row_index <- input$view_details
      film <- films_data[row_index, ]
    }
    
    selected_movie(film)
    show_details_page(TRUE)
  })
  
  #Fermer la page de détails (2)
  
  observeEvent(input$close_details, {
    show_details_page(FALSE)
    selected_movie(NULL)
  })
  
  output$show_details <- reactive({
    show_details_page()
  })
  outputOptions(output, "show_details", suspendWhenHidden = FALSE)
  
  #Afficher les détails
  
  output$movie_details_ui <- renderUI({
    req(selected_movie())
    
    film <- selected_movie()
    imdb_id <- film$tconst
    
    withProgress(message = 'Chargement des détails...', value = 0, {
      
      incProgress(0.3, detail = "Connexion à TMDB...")
      
      tmdb_id <- tryCatch({
        get_tmdb_id(imdb_id, api_key)
      }, error = function(e) {
        NA
      })
      
      if (is.na(tmdb_id)) {
        return(
          div(style = "background: #f5f5f5; border-radius: 10px; padding: 30px; text-align: center;",
              h3(icon("exclamation-triangle"), " Informations limitées", 
                 style = "color: #f39c12;"),
              p("Ce film n'a pas été trouvé dans la base TMDB."),
              hr(),
              div(style = "text-align: left; background: white; padding: 20px; border-radius: 8px;",
                  h4(style = "color: #667eea;", film$title),
                  p(strong("Année : "), film$year),
                  p(strong("Durée : "), film$runtime, " min"),
                  p(strong("Genres : "), film$genres),
                  p(strong("Note IMDb : "), film$rating, "/10")
              )
          )
        )
      }
      
      incProgress(0.5, detail = "Récupération des détails...")
      
      details <- tryCatch({
        get_movie_details(tmdb_id, api_key)
      }, error = function(e) {
        NULL
      })
      
      incProgress(0.7, detail = "Récupération des plateformes...")
      
      providers <- tryCatch({
        get_providers(tmdb_id, api_key, "FR")
      }, error = function(e) {
        "Non disponible"
      })
      
      incProgress(1, detail = "Terminé !")
      
      if (is.null(details)) {
        return(
          div(style = "background: #f5f5f5; border-radius: 10px; padding: 30px; text-align: center;",
              h3(icon("times-circle"), " Erreur", style = "color: #e74c3c;"),
              p("Impossible de récupérer les détails du film depuis TMDB.")
          )
        )
      }
      
      # URL de l'affiche
      poster_url <- if (!is.null(details$poster_path)) {
        paste0("https://image.tmdb.org/t/p/w500", details$poster_path)
      } else {
        "https://via.placeholder.com/300x450?text=Pas+d%27affiche"
      }
      
      #Genres
      genres_tags <- if (!is.null(details$genres) && nrow(details$genres) > 0) {
        lapply(details$genres$name, function(g) {
          span(style = "display: inline-block; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                        color: white; padding: 6px 14px; border-radius: 20px; margin: 5px;
                        font-size: 0.9em; box-shadow: 0 2px 5px rgba(0,0,0,0.2);", g)
        })
      } else {
        span(style = "display: inline-block; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                      color: white; padding: 6px 14px; border-radius: 20px; margin: 5px;
                      font-size: 0.9em;", film$genres)
      }
      
      # Rating
      rating <- if (!is.null(details$vote_average)) {
        round(details$vote_average, 1)
      } else {
        film$rating
      }
      
      # Fiches détaillées
      div(
        div(style = "display: flex; gap: 30px; margin-bottom: 30px; flex-wrap: wrap;",
            div(style = "flex-shrink: 0;",
                img(src = poster_url, 
                    style = "width: 300px; max-width: 100%; border-radius: 12px; 
                         box-shadow: 0 8px 20px rgba(0,0,0,0.3);")
            ),
            div(style = "flex-grow: 1; min-width: 300px;",
                h2(style = "margin-top: 0; color: #667eea; font-size: 2em;", 
                   details$title),
                
                div(style = "font-size: 1.1em; color: #666; margin-bottom: 20px;",
                    span(style = "display: inline-block; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                           color: white; padding: 8px 16px; border-radius: 25px; 
                           margin-right: 10px; font-weight: bold;
                           box-shadow: 0 3px 10px rgba(102,126,234,0.4);",
                         icon("star"), " ", rating, "/10"),
                    span(style = "margin-right: 10px;", icon("calendar"), " ", film$year),
                    span(icon("clock"), " ", film$runtime, " min")
                ),
                
                div(style = "margin-top: 20px;",
                    h4(style = "color: #667eea; margin-bottom: 10px;", 
                       icon("tags"), " Genres"),
                    div(genres_tags)
                ),
                
                div(style = "margin-top: 20px; background: linear-gradient(135deg, rgba(102,126,234,0.1) 0%, rgba(118,75,162,0.1) 100%);
                         padding: 15px; border-radius: 10px; border-left: 4px solid #667eea;",
                    h4(style = "color: #667eea; margin-top: 0;", 
                       icon("tv"), " Disponibilité"),
                    p(style = "margin: 0; font-size: 1.05em;", providers)
                )
            )
        ),
        
        if (!is.null(details$overview) && details$overview != "") {
          div(style = "margin-top: 30px;",
              h3(style = "color: #667eea; border-bottom: 3px solid #667eea; 
                        padding-bottom: 10px; display: flex; align-items: center;",
                 icon("book-open"), 
                 span(style = "margin-left: 10px;", "Synopsis")),
              p(style = "font-size: 1.1em; line-height: 1.8; text-align: justify; 
                       color: #333; margin-top: 15px;",
                details$overview)
          )
        },
        
        if (!is.null(details$tagline) && details$tagline != "") {
          div(style = "margin-top: 20px; padding: 15px; background: #f9f9f9; 
                       border-left: 4px solid #764ba2; border-radius: 5px;",
              p(style = "margin: 0; font-style: italic; color: #666; font-size: 1.05em;",
                icon("quote-left"), " ", details$tagline, " ", icon("quote-right"))
          )
        },
        
        if (!is.null(details$budget) && details$budget > 0) {
          div(style = "margin-top: 20px; display: flex; gap: 20px;",
              div(style = "flex: 1; background: #e8f5e9; padding: 15px; border-radius: 8px;",
                  strong("Budget : "), 
                  format(details$budget, big.mark = " "), " $"
              ),
              if (!is.null(details$revenue) && details$revenue > 0) {
                div(style = "flex: 1; background: #e3f2fd; padding: 15px; border-radius: 8px;",
                    strong("Revenus : "), 
                    format(details$revenue, big.mark = " "), " $"
                )
              }
          )
        }
      )
    })
  })
}


shinyApp(ui = ui, server = server)
