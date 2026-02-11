# =============================================================================
# Whatch? - Application complète de recommandation de films
# Version finale avec gestion automatique des données et landing page
# =============================================================================

library(shiny)
library(tidyverse)
library(DT)
library(httr)
library(jsonlite)
library(shinycssloaders)
library(data.table)
library(R.utils)

# =============================================================================
# FONCTION DE GESTION AUTOMATIQUE DES DONNÉES IMDB
# =============================================================================

ensure_imdb_data <- function(
  processed_path = "data/processed/movies_final.rds",
  raw_dir = "data/raw",
  processed_dir = "data/processed"
) {
  
  # Créer les dossiers s'ils n'existent pas
  dir.create("data", showWarnings = FALSE)
  dir.create(raw_dir, showWarnings = FALSE)
  dir.create(processed_dir, showWarnings = FALSE)
  
  # Si le fichier final existe déjà, sortir immédiatement
  if (file.exists(processed_path)) {
    cat("✅ Données IMDb déjà disponibles\n")
    return(invisible(TRUE))
  }
  
  cat("📥 Données IMDb non trouvées. Démarrage du téléchargement et traitement...\n\n")
  
  # URLs et chemins
  url_basics <- "https://datasets.imdbws.com/title.basics.tsv.gz"
  url_ratings <- "https://datasets.imdbws.com/title.ratings.tsv.gz"
  path_basics <- file.path(raw_dir, "title.basics.tsv.gz")
  path_ratings <- file.path(raw_dir, "title.ratings.tsv.gz")
  path_movies_temp <- file.path(raw_dir, "movies_only.tsv")
  
  # --- ÉTAPE 1: Téléchargement ---
  cat("📥 Téléchargement des fichiers IMDb...\n")
  
  if (!file.exists(path_basics)) {
    cat("  - Téléchargement de title.basics.tsv.gz (~207 MB)...\n")
    download.file(url_basics, path_basics, mode = "wb")
  } else {
    cat("  - title.basics.tsv.gz déjà présent\n")
  }
  
  if (!file.exists(path_ratings)) {
    cat("  - Téléchargement de title.ratings.tsv.gz...\n")
    download.file(url_ratings, path_ratings, mode = "wb")
  } else {
    cat("  - title.ratings.tsv.gz déjà présent\n")
  }
  
  cat("✅ Téléchargement terminé\n\n")
  
  # --- ÉTAPE 2: Filtrage avant chargement (optimisation mémoire) ---
  cat("🔧 Extraction des films uniquement...\n")
  
  if (!file.exists(path_movies_temp)) {
    cat("  - Filtrage en cours (2-3 minutes)...\n")
    
    con_in <- gzfile(path_basics, "r")
    con_out <- file(path_movies_temp, "w")
    
    # En-tête
    header <- readLines(con_in, n = 1)
    writeLines(header, con_out)
    
    # Traitement par blocs
    chunk_size <- 10000
    n_movies <- 0
    n_total <- 0
    
    repeat {
      lines <- readLines(con_in, n = chunk_size)
      if (length(lines) == 0) break
      
      movie_lines <- grep("\tmovie\t", lines, value = TRUE)
      
      if (length(movie_lines) > 0) {
        writeLines(movie_lines, con_out)
        n_movies <- n_movies + length(movie_lines)
      }
      
      n_total <- n_total + length(lines)
      
      if (n_total %% 100000 == 0) {
        cat(sprintf("    Progression : %s lignes, %s films trouvés\r", 
                    format(n_total, big.mark = " "), 
                    format(n_movies, big.mark = " ")))
      }
    }
    
    close(con_in)
    close(con_out)
    
    cat(sprintf("\n  ✅ Films extraits : %s\n", format(n_movies, big.mark = " ")))
  } else {
    cat("  - Fichier temporaire existant, réutilisation\n")
  }
  
  # --- ÉTAPE 3: Chargement des données ---
  cat("\n📊 Chargement des données...\n")
  
  basics <- fread(path_movies_temp, na.strings = "\\N", quote = "")
  ratings <- fread(path_ratings, na.strings = "\\N")
  
  cat(sprintf("  - Films chargés : %s\n", format(nrow(basics), big.mark = " ")))
  cat(sprintf("  - Ratings chargés : %s\n", format(nrow(ratings), big.mark = " ")))
  
  # --- ÉTAPE 4: Nettoyage et transformation ---
  cat("\n🔧 Nettoyage et transformation...\n")
  
  movies_basics <- basics %>%
    select(tconst, primaryTitle, startYear, runtimeMinutes, genres) %>%
    mutate(
      startYear = as.integer(startYear),
      runtimeMinutes = as.integer(runtimeMinutes)
    ) %>%
    filter(
      !is.na(startYear),
      !is.na(runtimeMinutes),
      startYear >= 1900,
      startYear <= year(Sys.Date()),
      runtimeMinutes >= 40,
      runtimeMinutes <= 300,
      genres != "\\N"
    )
  
  rm(basics)
  gc()
  
  # --- ÉTAPE 5: Fusion avec ratings ---
  cat("\n🔗 Fusion avec les ratings...\n")
  
  movies_full <- movies_basics %>%
    inner_join(ratings, by = "tconst") %>%
    filter(numVotes >= 100)
  
  rm(movies_basics, ratings)
  gc()
  
  # --- ÉTAPE 6: Renommer les colonnes pour correspondre à l'application ---
  cat("\n🎬 Préparation finale des données...\n")
  
  movies_final <- movies_full %>%
    rename(
      title = primaryTitle,
      year = startYear,
      runtime = runtimeMinutes,
      rating = averageRating,
      votes = numVotes
    ) %>%
    mutate(
      # Créer la liste de genres
      genres_list = str_split(genres, ","),
      
      # Calculer quality_score (normalisation de rating sur [0-1])
      quality_score = (rating - min(rating, na.rm = TRUE)) / 
                      (max(rating, na.rm = TRUE) - min(rating, na.rm = TRUE)),
      
      # Calculer popularity_score (normalisation de log(votes) sur [0-1])
      log_votes = log10(votes + 1),
      popularity_score = (log_votes - min(log_votes, na.rm = TRUE)) / 
                        (max(log_votes, na.rm = TRUE) - min(log_votes, na.rm = TRUE))
    ) %>%
    select(-log_votes)
  
  # --- ÉTAPE 7: Sauvegarde ---
  cat("\n💾 Sauvegarde des données finales...\n")
  
  saveRDS(movies_final, processed_path)
  
  cat(sprintf("✅ Données sauvegardées : %s films prêts à l'emploi\n", 
              format(nrow(movies_final), big.mark = " ")))
  cat(sprintf("  - Période : %d - %d\n", min(movies_final$year), max(movies_final$year)))
  cat(sprintf("  - Note moyenne : %.2f/10\n", mean(movies_final$rating)))
  cat("\n✨ Préparation des données terminée avec succès!\n\n")
  
  return(invisible(TRUE))
}

# =============================================================================
# INITIALISATION DES DONNÉES AU DÉMARRAGE
# =============================================================================

cat("🎬 Démarrage de l'application Whatch?...\n\n")

# Garantir que les données existent
ensure_imdb_data()

# Charger les données une seule fois (objet global)
cat("📂 Chargement des données en mémoire...\n")
MOVIES_DATA <- readRDS("data/processed/movies_final.rds")
cat(sprintf("✅ %s films chargés et prêts\n\n", format(nrow(MOVIES_DATA), big.mark = " ")))

# =============================================================================
# CONFIGURATION API TMDB
# =============================================================================

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

# =============================================================================
# CONFIGURATION DES ÉMOTIONS
# =============================================================================

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
  "interroger" = list(
    label = "M'interroger", description = "Science-fiction conceptuelle et philosophique",
    genres = c("Sci-Fi"), exclude_genres = c("Horror", "Comedy", "Romance"),
    alpha_preference = 0.3, rating_min = 7.2, duration_preference = "long",
    category = "profonde"
  ),
  "contempler" = list(
    label = "Contempler", description = "Films contemplatifs à rythme lent",
    genres = c("Drama"), exclude_genres = c("Action", "Horror", "Thriller", "Comedy", "Crime", "War"),
    alpha_preference = 0.1, rating_min = 7.8, duration_preference = "any",
    category = "profonde"
  ),
  
  "nostalgie" = list(
    label = "Nostalgie", description = "Classiques des années 70-90",
    genres = NULL, alpha_preference = 0.5, rating_min = 7.5, duration_preference = "any",
    year_range = c(1970, 1999), category = "specifique"
  ),
  "surprise" = list(
    label = "Surprise", description = "Films atypiques et originaux peu connus",
    genres = c("Mystery", "Sci-Fi"), exclude_genres = c("Romance", "Comedy"),
    alpha_preference = 0.1, rating_min = 7.5, duration_preference = "any",
    boost_low_votes = TRUE, category = "specifique"
  ),
  "mystere" = list(
    label = "Mystère", description = "Enquêtes policières et énigmes",
    genres = c("Mystery", "Crime"), exclude_genres = c("Horror", "Comedy", "Romance"),
    alpha_preference = 0.4, rating_min = 7.0, duration_preference = "moyen",
    category = "specifique"
  ),
  "fantastique" = list(
    label = "Fantastique", description = "Mondes imaginaires et magie",
    genres = c("Fantasy"), exclude_genres = c("Horror", "War"),
    alpha_preference = 0.6, rating_min = 7.0, duration_preference = "long",
    category = "specifique"
  ),
  "western" = list(
    label = "Western", description = "Far West et duels de cowboys",
    genres = c("Western"), alpha_preference = 0.4, rating_min = 7.0, duration_preference = "moyen",
    category = "specifique"
  )
)

# =============================================================================
# FONCTIONS DE FILTRAGE
# =============================================================================

apply_emotion_filter <- function(data, emotion_key, custom_alpha = NULL,
                                custom_year = NULL, custom_duration = NULL) {
  
  config <- EMOTION_CONFIG[[emotion_key]]
  
  if (!is.null(config$genres)) {
    data <- data %>% filter(map_lgl(genres_list, ~ any(.x %in% config$genres)))
  }
  
  if (!is.null(config$exclude_genres)) {
    data <- data %>% filter(map_lgl(genres_list, ~ !any(.x %in% config$exclude_genres)))
  }
  
  data <- data %>% filter(rating >= config$rating_min)
  
  if (!is.null(custom_duration)) {
    data <- data %>% filter(runtime <= custom_duration)
  } else if (!is.null(config$duration_preference) && config$duration_preference != "any") {
    if (config$duration_preference == "court") {
      data <- data %>% filter(runtime <= 100)
    } else if (config$duration_preference == "moyen") {
      data <- data %>% filter(runtime >= 90, runtime <= 130)
    } else if (config$duration_preference == "long") {
      data <- data %>% filter(runtime >= 120)
    }
  }
  
  if (!is.null(custom_year)) {
    data <- data %>% filter(year >= custom_year[1], year <= custom_year[2])
  } else if (!is.null(config$year_range)) {
    data <- data %>% filter(year >= config$year_range[1], year <= config$year_range[2])
  }
  
  alpha_to_use <- if (!is.null(custom_alpha)) custom_alpha else config$alpha_preference
  
  data <- data %>%
    mutate(composite_score = (1 - alpha_to_use) * quality_score + alpha_to_use * popularity_score)
  
  if (!is.null(config$boost_low_votes) && isTRUE(config$boost_low_votes)) {
    data <- data %>%
      mutate(composite_score = composite_score + (1 - popularity_score) * 0.2)
  }
  
  data %>% arrange(desc(composite_score))
}

# =============================================================================
# INTERFACE UTILISATEUR
# =============================================================================

ui <- navbarPage(
  title = div(
    img(src = "image.png", height = "30px", style = "margin-right: 10px; vertical-align: middle;"),
    "What'ch?"
  ),
  id = "navbar",
  
  # CSS personnalisé
  tags$head(
    tags$style(HTML("
      body {
        background-image: linear-gradient(rgba(0, 0, 0, 0.7), rgba(0, 0, 0, 0.7)), url('back.jpg');
        background-size: cover;
        background-attachment: fixed;
        background-position: center;
      }
      
      /* Page d'accueil */
      .page-accueil {
        min-height: 85vh;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        text-align: center;
        color: white;
        padding: 20px;
      }
      
      .logo-container {
        margin-bottom: 40px;
      }
      
      .logo-container img {
        max-width: 150px;
        filter: drop-shadow(0 0 20px rgba(255, 255, 255, 0.3));
        animation: float 3s ease-in-out infinite;
      }
      
      @keyframes float {
        0%, 100% { transform: translateY(0px); }
        50% { transform: translateY(-20px); }
      }
      
      .titre { 
        font-size: 80px; 
        font-weight: bold; 
        text-shadow: 3px 3px 15px rgba(0,0,0,0.8);
        margin-bottom: 20px;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }
      
      .sous-titre { 
        font-size: 28px; 
        margin-bottom: 50px;
        text-shadow: 2px 2px 8px rgba(0,0,0,0.8);
        color: #f0f0f0;
      }
      
      .btn-start {
        font-size: 24px;
        padding: 18px 50px;
        border-radius: 50px;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border: none;
        box-shadow: 0px 8px 25px rgba(102, 126, 234, 0.5);
        transition: all 0.3s ease;
        cursor: pointer;
      }
      
      .btn-start:hover {
        transform: translateY(-3px);
        box-shadow: 0px 12px 35px rgba(102, 126, 234, 0.7);
      }
      
      /* Page principale */
      .navbar-default {
        background: linear-gradient(135deg, #00c6ff 0%, #ff2d95 100%);
        border: none;
      }
      .dataTables_wrapper a {
        color: #667eea;
        font-weight: bold;
        text-decoration: none;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      .btn-deselect {
        background: #e0e0e0;
        border: none;
        color: #666;
        font-size: 14px;
        padding: 8px;
        border-radius: 6px;
        margin-top: 10px;
      }
      
      .btn-deselect:hover { background: #d0d0d0; }
      .dataTables_wrapper a:hover {
        color: #764ba2;
        text-decoration: underline;
      }
      .emotion-card {
        background: white;
        border-radius: 15px;
        padding: 20px;
        margin: 10px 0;
        cursor: pointer;
        transition: all 0.3s ease;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      
      .emotion-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 15px rgba(102, 126, 234, 0.3);
      }
      
      .emotion-card.selected {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        box-shadow: 0 8px 20px rgba(102, 126, 234, 0.5);
      }
      
      .section-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 15px;
        border-radius: 10px;
        margin: 20px 0 15px 0;
        box-shadow: 0 4px 10px rgba(0,0,0,0.2);
      }
      .dataTables_wrapper a {
        color: #667eea;
        font-weight: bold;
        text-decoration: none;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      
      .dataTables_wrapper a:hover {
        color: #764ba2;
        text-decoration: underline;
      }
      .movie-details-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0,0,0,0.85);
        z-index: 9998;
        display: flex;
        align-items: center;
        justify-content: center;
        animation: fadeIn 0.3s ease-out;
      }
      
      @keyframes fadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
      }
      .movie-details-container {
        width: 90%;
        max-width: 1200px;
        max-height: 90vh;
        overflow-y: auto;
        background: white;
        border-radius: 15px;
        box-shadow: 0 20px 60px rgba(0,0,0,0.5);
        animation: slideIn 0.3s ease-out;
      }
      
      @keyframes slideIn {
        from {
          opacity: 0;
          transform: translateY(-50px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
    "))
  ),
  tags$script(HTML("
      function toggleCategory(category) {
        var element = document.getElementById('category-' + category);
        var header = element.previousElementSibling;
        if (element.classList.contains('collapsed')) {
          element.classList.remove('collapsed');
          header.classList.remove('collapsed');
        } else {
          element.classList.add('collapsed');
          header.classList.add('collapsed');
        }
      }

      Shiny.addCustomMessageHandler('deselect_emotion', function(message) {
        $('.emotion-btn').removeClass('active');
        Shiny.setInputValue('emotion_selected', '', {priority: 'event'});
      });
    "))
)
  
  # =============================================================================
  # PAGE D'ACCUEIL (LANDING PAGE)
  # =============================================================================
  
  tabPanel(
    "Accueil",
    value = "accueil",
    div(class = "page-accueil",
        div(class = "logo-container",
            img(src = "image.png", alt = "Cinéma")
        ),
        div(class = "titre", "🎬 What'ch?"),
        div(class = "sous-titre", "Abracadabra, on choisit pour toi !"),
        actionButton("demarrer", "🍿 Commencer", class = "btn-start")
    )
  )
  
  # =============================================================================
  # PAGE DE RECOMMANDATION
  # =============================================================================
  
  tabPanel(
    "Recommandation", 
    value = "selection",
    
    conditionalPanel(
      condition = "!output.show_details",
      
      fluidRow(
        column(12,
               div(class = "section-header",
                   h3(icon("heart"), " Comment te sens-tu ce soir ?",
                      style = "margin: 0;")
               )
        )
      ),
      
      fluidRow(
        column(3,
               h4("😊 Émotions positives", style = "color: white; text-align: center;"),
               uiOutput("emotions_positive")
        ),
        column(3,
               h4("⚡ Émotions intenses", style = "color: white; text-align: center;"),
               uiOutput("emotions_intense")
        ),
        column(3,
               h4("🧠 Émotions profondes", style = "color: white; text-align: center;"),
               uiOutput("emotions_profonde")
        ),
        column(3,
               h4("✨ Spécifiques", style = "color: white; text-align: center;"),
               uiOutput("emotions_specifique")
        )
      ),
      
      fluidRow(
        column(12,
               div(class = "section-header",
                   h3(icon("sliders-h"), " Affine ta recherche",
                      style = "margin: 0;")
               )
        )
      ),
      
      fluidRow(
        column(4,
               div(style = "background: white; padding: 20px; border-radius: 10px; margin: 10px 0;",
                   sliderInput("year_range", "Période :",
                               min = 1900, max = year(Sys.Date()),
                               value = c(1980, year(Sys.Date())),
                               step = 1, sep = ""),
                   sliderInput("duration_max", "Durée maximale (min) :",
                               min = 60, max = 240, value = 180, step = 10),
                   sliderInput("alpha_slider", "Type de films :",
                               min = 0, max = 1, value = 0.5, step = 0.1),
                   div(style = "font-size: 0.9em; color: #666; margin-top: -10px;",
                       "← Qualité (critiques) | Popularité (audience) →")
               )
        ),
        
        column(8,
               fluidRow(
                 column(6,
                        actionButton("search", "🔍 Lancer la recherche",
                                   class = "btn btn-primary btn-lg btn-block",
                                   style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                                            border: none; margin-top: 10px; padding: 15px;
                                            box-shadow: 0 4px 10px rgba(102,126,234,0.4);")
                 ),
                 column(6,
                        actionButton("random", "🎲 Film au hasard",
                                   class = "btn btn-warning btn-lg btn-block",
                                   style = "margin-top: 10px; padding: 15px;
                                            box-shadow: 0 4px 10px rgba(0,0,0,0.2);")
                 )
               ),
               actionButton("deselect", "✕ Désélectionner", class = "btn-deselect btn-block"),
               
               br(),
               
               conditionalPanel(
                 condition = "input.search > 0 || input.random > 0",
                 div(style = "background: white; padding: 20px; border-radius: 10px;",
                     withSpinner(
                       DTOutput("movies_table"),
                       type = 6,
                       color = "#667eea"
                     )
                 )
               )
        )
      )
    ),
    
    # Page de détails du film
    conditionalPanel(
      condition = "output.show_details",
      div(style = "background: white; border-radius: 15px; padding: 30px; 
                   margin: 20px; box-shadow: 0 8px 20px rgba(0,0,0,0.2);",
          actionButton("close_details", 
                      "← Retour aux résultats",
                      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                               color: white; border: none; padding: 10px 20px;
                               border-radius: 25px; margin-bottom: 20px;
                               box-shadow: 0 3px 10px rgba(102,126,234,0.4);"),
          withSpinner(
            uiOutput("movie_details_ui"),
            type = 6,
            color = "#667eea"
          )
      )
    )
  )
)

# =============================================================================
# SERVEUR
# =============================================================================

server <- function(input, output, session) {
  
  # Navigation : landing page → app principale
  observeEvent(input$demarrer, {
    updateNavbarPage(session, "navbar", selected = "selection")
  })
  
  # Variables réactives
  selected_emotion <- reactiveVal("")
  show_details_page <- reactiveVal(FALSE)
  selected_movie <- reactiveVal(NULL)
  random_movie <- reactiveVal(NULL)
  
  # Rendu des cartes d'émotions par catégorie
  render_emotion_cards <- function(category) {
    emotions <- EMOTION_CONFIG[sapply(EMOTION_CONFIG, function(x) x$category == category)]
    
    lapply(names(emotions), function(key) {
      config <- emotions[[key]]
      div(
        class = paste0("emotion-card", 
                      if(selected_emotion() == key) " selected" else ""),
        onclick = sprintf("Shiny.setInputValue('emotion_click', '%s', {priority: 'event'})", key),
        h4(config$label, style = "margin-top: 0;"),
        p(config$description, style = "font-size: 0.9em; margin-bottom: 0;")
      )
    })
  }
  
  output$emotions_positive <- renderUI({ render_emotion_cards("positive") })
  output$emotions_intense <- renderUI({ render_emotion_cards("intense") })
  output$emotions_profonde <- renderUI({ render_emotion_cards("profonde") })
  output$emotions_specifique <- renderUI({ render_emotion_cards("specifique") })
  
  observeEvent(input$deselect, {
    session$sendCustomMessage(type = "deselect_emotion", message = list())
  })
  
  # Gestion du clic sur une émotion
  observeEvent(input$emotion_click, {
    if (selected_emotion() == input$emotion_click) {
      selected_emotion("")
    } else {
      selected_emotion(input$emotion_click)
    }
  })
  
  # Filtrage des films
  filtered_movies <- eventReactive(input$search, {
    req(selected_emotion())
    
    withProgress(message = 'Recherche en cours...', value = 0, {
      incProgress(0.3, detail = "Filtrage des films...")
      
      result <- apply_emotion_filter(
        MOVIES_DATA,
        selected_emotion(),
        custom_alpha = input$alpha_slider,
        custom_year = input$year_range,
        custom_duration = input$duration_max
      )
      
      incProgress(0.7, detail = "Calcul des scores...")
      
      result <- result %>%
        head(100) %>%
        select(title, year, runtime, genres, rating, votes, composite_score)
      
      incProgress(1, detail = "Terminé !")
      
      result
    })
  })
  
  # Film aléatoire
  observeEvent(input$random, {
    all_movies <- MOVIES_DATA %>%
      filter(rating >= 7.0, votes >= 1000) %>%
      sample_n(1)
    
    random_movie(all_movies)
    show_details_page(TRUE)
    random_movie(NULL)
  })
  
  # Affichage du tableau
  output$movies_table <- renderDT({
    req(filtered_movies())
    
    data_display <- filtered_movies() %>%
      mutate(
        Score = round(composite_score, 3),
        Note = rating,
        Titre = title,
        Année = year,
        Durée = paste0(runtime, " min"),
        Genres = genres,
        Votes = format(votes, big.mark = " ")
      ) %>%
      select(Titre, Année, Durée, Genres, Note, Votes, Score)
    
    datatable(
      data_display,
      selection = list(mode = 'single', target = 'row', selected = NULL),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        dom = "lftip",
        ordering = TRUE,
        order = list(list(6, "desc")),
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
        background = styleColorBar(range(data_display$Note, na.rm = TRUE), '#90EE90'),
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
  
  # Clic sur un film dans le tableau
  observeEvent(input$movies_table_rows_selected, {
    req(input$movies_table_rows_selected)
    
    row_index <- input$movies_table_rows_selected
    films_data <- filtered_movies()
    
    # Récupérer les données complètes du film
    film_titre <- films_data$title[row_index]
    film_complet <- MOVIES_DATA %>% 
      filter(title == film_titre) %>%
      slice(1)
    
    selected_movie(film_complet)
    show_details_page(TRUE)
  })
  
  # Fermer la page de détails
  observeEvent(input$close_details, {
    show_details_page(FALSE)
    selected_movie(NULL)
  })
  
  output$show_details <- reactive({
    show_details_page()
  })
  outputOptions(output, "show_details", suspendWhenHidden = FALSE)
  
  # Affichage des détails du film
  output$movie_details_ui <- renderUI({
    
    # Déterminer quel film afficher
    if (input$random > 0 && !is.null(random_movie())) {
      film <- random_movie()
    } else if (!is.null(selected_movie())) {
      film <- selected_movie()
    } else {
      return(NULL)
    }
    
    req(film)
    
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
      
      # Genres
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
      
      # Affichage détaillé
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
        }
      )
    })
  })
}

# =============================================================================
# LANCEMENT DE L'APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)
