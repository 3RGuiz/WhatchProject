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
    head(res$movie_results$id[1],5)
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

# Fonction pour récupérer les acteurs
get_movie_credits <- function(tmdb_id, api_key, language = "fr-FR") {
  url <- paste0(
    "https://api.themoviedb.org/3/movie/",
    tmdb_id,
    "/credits?api_key=", api_key,
    "&language=", language
  )
  r <- GET(url)
  stop_for_status(r)
  fromJSON(content(r, "text", encoding = "UTF-8"))
}

#Config "émotions"

EMOTION_CONFIG <- list(
  "rire" = list(
    label = "Rire", description = "Comédies légères et humoristiques",
    genres = c("Comedy"), exclude_genres = c("War", "Horror"),
    alpha_preference = 0.6, rating_min = 6.5, duration_preference = "any",
    category = "positive"
  ),
  "detente" = list(
    label = "Détente", description = "Films feel-good sans prise de tête",
    genres = c("Comedy", "Romance", "Family", "Animation"),
    alpha_preference = 0.7, rating_min = 6.8, duration_preference = "any",
    exclude_genres = c("Horror", "War"),
    category = "positive"
  ),
  "romance" = list(
    label = "Romance", description = "Histoires d'amour et relations sentimentales",
    genres = c("Romance"), exclude_genres = c("Horror", "War"),
    alpha_preference = 0.5, rating_min = 6.5, duration_preference = "any",
    category = "positive"
  ),
  "voyage" = list(
    label = "Voyager", description = "Aventures et découvertes dans des lieux exotiques",
    genres = c("Adventure"), exclude_genres = c("Horror"),
    alpha_preference = 0.6, rating_min = 6.8, duration_preference = "any",
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
    genres = c("Action"), exclude_genres = c("Documentary"),
    alpha_preference = 0.7, rating_min = 6.5, duration_preference = "any",
    category = "intense"
  ),
  "peur" = list(
    label = "Peur", description = "Films d'horreur pour avoir peur",
    genres = c("Horror"), exclude_genres = c("Comedy", "Family"),
    alpha_preference = 0.4, rating_min = 6.0, duration_preference = "any",
    category = "intense"
  ),
  "angoisse" = list(
    label = "Angoisse", description = "Thrillers psychologiques et suspense intense",
    genres = c("Thriller"), exclude_genres = c("Comedy", "Family", "Romance"),
    alpha_preference = 0.3, rating_min = 7.0, duration_preference = "any",
    category = "intense"
  ),
  "epique" = list(
    label = "Épique", description = "Grandes fresques historiques et batailles",
    genres = c("War", "History"), exclude_genres = c("Comedy", "Romance", "Horror"),
    alpha_preference = 0.6, rating_min = 7.0, duration_preference = "any",
    category = "intense"
  ),
  
  "pleurer" = list(
    label = "Pleurer", description = "Drames émotionnels et histoires touchantes",
    genres = c("Drama"), exclude_genres = c("Horror", "Action"),
    alpha_preference = 0.2, rating_min = 7.0, duration_preference = "any",
    category = "profonde"
  ),
  "reflechir" = list(
    label = "M'instruire", description = "Documentaires et films biographiques instructifs",
    genres = c("Documentary", "Biography"), exclude_genres = c("Horror", "Comedy", "Action"),
    alpha_preference = 0.2, rating_min = 7.0, duration_preference = "any",
    category = "profonde"
  ),
  "mystere" = list(
    label = "Mystère", description = "Enquêtes policières et énigmes",
    genres = c("Mystery", "Crime"), exclude_genres = c("Horror", "Comedy", "Romance"),
    alpha_preference = 0.4, rating_min = 7.0, duration_preference = "any",
    category = "profonde"
  ),
  "interroger" = list(
    label = "S-F", description = "Science-fiction conceptuelle et philosophique",
    genres = c("Sci-Fi"), exclude_genres = c("Comedy", "Romance"),
    alpha_preference = 0.3, rating_min = 7.0, duration_preference = "long",
    category = "specifique"
  ),
  "contempler" = list(
    label = "Contempler", description = "Films contemplatifs à rythme lent",
    genres = c("Drama", "Adventure"), exclude_genres = c("Action", "Horror", "Thriller", "Crime", "War"),
    alpha_preference = 0.1, rating_min = 7.0, duration_preference = "any",
    category = "profonde"
  ),
  
  "nostalgie" = list(
    label = "Nostalgie", description = "Classiques des années 70-90",
    genres = NULL, alpha_preference = 0.5, rating_min = 7.0, duration_preference = "any",
    year_range = c(1970, 1999), category = "specifique"
  ),
  "surprise" = list(
    label = "Surprise", description = "Films atypiques et originaux peu connus",
    genres = c("Mystery", "Sci-Fi"), exclude_genres = c("Romance", "Comedy"),
    alpha_preference = 0.1, rating_min = 7.0, duration_preference = "any",
    boost_low_votes = TRUE, category = "specifique"
  ),
  "fantastique" = list(
    label = "Fantastique", description = "Mondes imaginaires et magie",
    genres = c("Fantasy"), exclude_genres = c("Horror", "War"),
    alpha_preference = 0.6, rating_min = 7.0, duration_preference = "any",
    category = "specifique"
  ),
  "western" = list(
    label = "Western", description = "Far West et duels de cowboys",
    genres = c("Western"), alpha_preference = 0.4, rating_min = 7.0, duration_preference = "any",
    category = "specifique"
  )
)

#Fonctions de filtrage des films

apply_emotion_filter <- function(data, emotion_key, custom_alpha = NULL,
                                 custom_year = NULL, custom_duration = NULL,
                                 custom_min_rating = NULL) {
  
  config <- EMOTION_CONFIG[[emotion_key]]
  
  if (!is.null(config$genres)) {
    data <- data %>% filter(map_lgl(genres_list, ~ any(.x %in% config$genres)))
  }
  
  if (!is.null(config$exclude_genres)) {
    data <- data %>% filter(map_lgl(genres_list, ~ !any(.x %in% config$exclude_genres)))
  }
  
  # Note minimale : utiliser custom_min_rating si fourni, sinon config
  min_rating_to_use <- if (!is.null(custom_min_rating)) {
    max(custom_min_rating, config$rating_min)
  } else {
    config$rating_min
  }
  data <- data %>% filter(rating >= min_rating_to_use)
  
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

# UI

ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: top; font-size: 36px; font-weight: bold;",
    "What'ch ?"),
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
      
      .main-container {
        background: back.jpg;
        border-radius: 15px;
        padding: 30px;
        margin: 20px auto;
        max-width: 1600px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.3);
      }
      
      .page-title {
        text-align: center;
        color: white;
        font-size: 80px;
        font-weight: bold;
        margin-bottom: 10px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.1);
      }
      
      .subtitle {
        text-align: center;
        font-size: 18px;
        color: #666;
        margin-bottom: 30px;
      }
      
      /* Boutons émotions */
      .emotion-btn {
        width: 100%;
        margin: 5px 0;
        padding: 12px;
        font-size: 15px;
        border-radius: 8px;
        border: 2px solid #e0e0e0;
        background: black;
        transition: all 0.3s;
        text-align: left;
        position: relative;
        z-index: 1;
      }
      
      .emotion-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        border-color: #2196F3;
        z-index: 99999
      }
      
      .emotion-btn:hover .tooltip-text { 
      visibility: visible; opacity: 1;
      z-index: 100000
      }
      
      .emotion-btn.active {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-color: #667eea;
        font-weight: bold;
      }
      
      .emotion-btn .tooltip-text {
        visibility: hidden;
        width: 250px;
        background-color: #333;
        color: #fff;
        text-align: center;
        border-radius: 6px;
        padding: 8px;
        position: absolute;
        z-index: 1000;
        left: 105%;
        top: 50%;
        transform: translateY(-50%);
        opacity: 0;
        transition: opacity 0.3s;
        font-size: 12px;
        font-weight: normal;
      }
      
      .category-header {
        font-size: 15px;
        font-weight: bold;
        color: white;
        margin: 15px 0 10px 0;
        padding: 10px;
        background: black;
        border: 4px solid #667eea;
        cursor: pointer;
        transition: all 0.3s;
        user-select: none;
      }
      
      .category-header:hover { background: #e3f2fd66; }
      
      .category-header .toggle-icon {
        float: right;
        transition: transform 0.3s;
      }
      
      .category-header.collapsed .toggle-icon { transform: rotate(-90deg); }
      
      .category-emotions { max-height: 500px; transition: max-height 0.3s ease-out; }
      .category-emotions.collapsed { max-height: 0; overflow: hidden; }
      
      .stats-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin: 10px 0;
        text-align: center;
      }
      
      .stats-number { font-size: 28px; font-weight: bold; margin: 0; }
      .stats-label { font-size: 13px; opacity: 0.9; margin: 5px 0 0 0; }
      
      .btn-search {
        font-size: 18px;
      padding: 18px 35px;
      border-radius: 12px;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      border: none;
      box-shadow: 0px 6px 20px rgba(110, 101, 198, 0.5);
      font-weight: bold;
      }
      
      .btn-search:hover { opacity: 0.9; transform: translateY(-2px); }
      
      .btn-random {
      font-size: 14px;
      padding: 14px 20px;
      border-radius: 12px;
      background-color: #ff6b6b;
      color: white;
      border: none;
      box-shadow: 0px 4px 15px rgba(255, 107, 107, 0.4);
      font-weight: bold;
      }

      .btn-random:hover {background-color: #ff5555; opacity: 0.9; transform: translateY(-2px); }
      
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
      
      /* Style pour les titres cliquables */
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
      
      /* Overlay pour la page détails */
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
      
      .movie-details-header {
        padding: 20px 30px;
        border-bottom: 2px solid #667eea;
        display: flex;
        justify-content: space-between;
        align-items: center;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-radius: 15px 15px 0 0;
      }
      
      .movie-details-content {
        padding: 30px;
      }
      
      .close-btn {
        background: rgba(255,255,255,0.2);
        border: 2px solid white;
        color: white;
        padding: 10px 25px;
        border-radius: 25px;
        cursor: pointer;
        font-weight: bold;
        transition: all 0.3s;
        font-size: 16px;
      }
      
      .close-btn:hover {
        background: white;
        color: #667eea;
        transform: scale(1.05);
      }
      
      /* Styles pour l'onglet FAQ */
      .faq-container {
        background: rgba(255, 255, 255, 0.95);
        border-radius: 15px;
        padding: 40px;
        margin: 20px auto;
        max-width: 1000px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.3);
      }
      
      .faq-container h1 {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  color: transparent;
        font-weight: 800;
        letter-spacing: -0.5px;
        margin-bottom: 20px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.1);
      }
      
      .faq-container h2 {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  color: transparent;
        font-weight: 700;
        margin-top: 40px;
        margin-bottom: 20px;
      }
      
      .faq-container h3, 
      .faq-container h4 {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  color: transparent;
        font-weight: 600;
      }
      
      .faq-container p {
        font-size: 16px;
        line-height: 1.7;
        color: #2C2C54;
      }
      
      .highlight {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  color: transparent;
  font-weight: 700;
      }
      
      .scenario-block {
        background: linear-gradient(135deg, #F1F0FF, #E6E4FF);
        border-radius: 18px;
        padding: 28px;
        height: 100%;
        box-shadow: 0 10px 22px rgba(102, 126, 234, 0.18);
        transition: transform 0.25s ease;
        margin-bottom: 20px;
      }
      
      .scenario-block:hover {
        transform: translateY(-6px);
      }
      
      .scenario-block h4 {
        font-weight: 700;
        margin-bottom: 14px;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  color: transparent;
      }
      
      .drawer-btn {
  background-color: transparent;
  border: none;
  font-weight: 600;
  font-size: 15px;
  padding: 10px 0;
  cursor: pointer;
  transition: all 0.2s ease;

  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  color: transparent;
}
      
      .drawer-btn:hover {
  text-decoration: underline;
  filter: brightness(1.15);
}
      
      .creators {
        width: 100%;
        display: flex;
        justify-content: space-around;
        align-items: center;
        margin-top: 30px;
        padding: 20px 0;
        font-family: 'Inter', 'Segoe UI', Arial, sans-serif;
        font-weight: 700;
        font-size: 18px;
        color: #667eea;
        letter-spacing: 1px;
        border-top: 2px solid #E6E4FF;
      }
      
      .creators span {
        transition: all 0.3s ease;
      }
      
      .creators span:hover {
        color: #764ba2;
        transform: scale(1.1);
      }
    ")),
    
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
  ),
  
  # Page d'accueil
  tabPanel(
    "Accueil",
    value = "accueil",
    div(class = "page-accueil",
        div(class = "logo-container",
            img(src = "bobine.png", alt = "Cinéma")
        ),
        div(class = "titre", "What'ch?"),
        div(class = "sous-titre", "Abracadabra, on choisit pour toi !"),
        actionButton("demarrer", "🍿 Commencer", class = "btn-start")
    )
  ),
  
  # Page de sélection
  tabPanel("Sélection", value = "selection",
           
           # Page 1
           div(class = "main-container",
               h1(class = "page-title", "🎬 What'ch?"),
               p(class = "subtitle", "Abracadabra, on choisit pour toi!"),
               
               fluidRow(
                 # Colonne gauche = sélection
                 column(
                   3,
                   actionButton("search", "🔍 Trouver mon film", class = "btn-search btn-block btn-lg"),
                   br(),
                   textInput("search_title", 
                             label = NULL,
                             placeholder = "🔍 Ou rechercher un titre directement..."),
                   br(),
                   actionButton("random", "🎲 Au hasard !", class = "btn btn-random btn-block"),
                   br(),
                   
                   h3("J'ai envie de...", style = "color: white;"),
                   
                   div(class = "category-header", onclick = "toggleCategory('positive')",
                       "Légèreté", tags$span(class = "toggle-icon", "▼")),
                   div(id = "category-positive", class = "category-emotions collapsed", uiOutput("emotions_positive")),
                   
                   div(class = "category-header", onclick = "toggleCategory('intense')",
                       "Intensivité", tags$span(class = "toggle-icon", "▼")),
                   div(id = "category-intense", class = "category-emotions collapsed", uiOutput("emotions_intense")),
                   
                   div(class = "category-header", onclick = "toggleCategory('profonde')",
                       "Profondeur", tags$span(class = "toggle-icon", "▼")),
                   div(id = "category-profonde", class = "category-emotions collapsed", uiOutput("emotions_profonde")),
                   
                   div(class = "category-header", onclick = "toggleCategory('specifique')",
                       "Spécificité", tags$span(class = "toggle-icon", "▼")),
                   div(id = "category-specifique", class = "category-emotions collapsed", uiOutput("emotions_specifique")),
                   
                   actionButton("deselect", "✕ Désélectionner", class = "btn-deselect btn-block"),
                   br(),
                   
                   checkboxInput("show_advanced", "⚙️ Options avancées", value = FALSE),
                   conditionalPanel(
                     condition = "input.show_advanced == true",
                     sliderInput("alpha_override", "Découverte ↔ Populaire",
                                 min = 0, max = 1, value = 0.5, step = 0.05),
                     sliderInput("min_rating",
                                 label = div(icon("star"), " Note minimale IMDb"),
                                 min = 0,
                                 max = 10,
                                 value = 0,
                                 step = 0.5,
                                 post = "/10"),
                     helpText(style = "font-size: 11px; color: #666;",
                              "← Pépites méconnues | Films populaires →"),
                     sliderInput("year_range", "Période",
                                 min = 1950, max = 2026, value = c(1990, 2026), step = 1, sep = ""),
                     sliderInput("duration_max", "Durée max (min)",
                                 min = 60, max = 240, value = 180, step = 10)
                   ),
                   
                   br(),
                   helpText(style = "font-size: 12px; color: #999; text-align: center;",
                            "💡 Le bouton 'Au hasard' propose un film aléatoire bien noté")
                 ),
                 
                 # Colonne droite = résultats
                 column(
                   9,
                   uiOutput("stats_panel"),
                   br(),
                   
                   conditionalPanel(
                     condition = "(input.emotion_selected == null || input.emotion_selected == '') && (!input.random || input.random == 0)",
                     div(style = "text-align: center; padding: 80px 20px; color: #999;",
                         h2("👈 Choisissez votre humeur pour commencer", style = "color: #999;"),
                         p("Sélectionnez une émotion ou cliquez sur 'Au hasard'", style = "font-size: 16px;")
                     )
                   ),
                   
                   conditionalPanel(
                     condition = "(input.emotion_selected != null && input.emotion_selected != '' && input.search > 0) || (input.random && input.random > 0) || (input.search_title != null && input.search_title != '')",
                     h3("🎯 Films recommandés"),
                     p(style = "text-align: center; color: #666; margin-bottom: 20px;",
                       "Cliquez sur un titre pour voir ses détails complets"),
                     DTOutput("results_table")
                   )
                 )
               )
           ),
           
           #Page 2 = détails
           
           conditionalPanel(
             condition = "output.show_details",
             
             div(class = "movie-details-overlay",
                 onclick = "if (event.target === this) { Shiny.setInputValue('close_details', Math.random(), {priority: 'event'}); }",
                 
                 div(class = "movie-details-container",
                     # Header
                     div(class = "movie-details-header",
                         h3(style = "margin: 0; color: white;", 
                            icon("film"), " Détails du film"),
                         tags$button(
                           class = "close-btn",
                           onclick = "Shiny.setInputValue('close_details', Math.random(), {priority: 'event'});",
                           "✕ Fermer"
                         )
                     ),
                     
                     # Contenu
                     div(class = "movie-details-content",
                         uiOutput("movie_details_ui")
                     )
                 )
             )
           )
  ),
  
  # Onglet À propos
  tabPanel(
    "À propos",
    value = "apropos",
    
    fluidPage(
      div(class = "faq-container",
          # Titre
          h1("🎬 À propos de What'ch"),
          p("Une application pensée pour vous aider à choisir ",
            span("le film idéal", class = "highlight"),
            ", sans hésitation selon votre humeur et vos envies."),
          
          ## Partie : Le décor
          h2("Le décor"),
          p("Ne vous est-il jamais arrivé de passer de longues minutes à chercher un film ? ",
            "Aujourd'hui, le choix ne manque pas. ",
            span("Films, séries, plateformes de streaming", class = "highlight"),
            " : l'offre est immense, ce qui rend la décision plus difficile."),
          p(
            "Le véritable problème n'est plus de trouver un film, mais de savoir ",
            span("lequel regarder", class = "highlight"),
            "."),
          p(
            "C'est la raison pour laquelle nous avons créé ",
            strong("What'ch"),
            " : une application pensée pour faciliter le choix de votre prochain film ",
            "et vous permettre de passer moins de temps à chercher, et plus de temps à regarder."),
          
          # Partie : Le scénario 
          h2("Le scénario"),
          p("Une expérience pensée pour être simple et rapide."),
          
          fluidRow(
            column(4,
                   div(
                     class = "scenario-block",
                     h4("🎭 Choisissez vos envies"),
                     
                     p("Humeur du jour, temps disponible, envie de découverte ou besoin de légèreté : ",
                       span("vous sélectionnez en quelques clics ce qui vous correspond sur le moment.", 
                            class = "highlight")
                     )
                   )
            ),
            column(4,
                   div(
                     class = "scenario-block",
                     h4("🎬 Le film idéal en un clic"),
                     p(
                       "Une fois vos critères définis, il vous suffit de valider. ",
                       "Grâce à des correspondances entre les préférences de l'utilisateur ",
                       "et les caractéristiques des films, ",
                       span("What'ch vous propose des recommandations pertinentes.", 
                            class = "highlight")
                     )
                   )
            ),
            column(4,
                   div(
                     class = "scenario-block",
                     h4("🎞️ Explorez et choisissez"),
                     p("Les films recommandés s'affichent par ordre de pertinence ",
                       span("(score What'ch). ", class = "highlight"),
                       "Cliquez sur une suggestion pour accéder à sa fiche descriptive."
                     )
                   )
            )
          ),
          
          br(),
          
          # Bouton pour la Partie 'Comment ça marche'
          actionButton(
            "toggle_algo",
            "🔍 Comment fonctionne la recommandation ?",
            class = "drawer-btn"),
          
          conditionalPanel(
            condition = "input.toggle_algo % 2 == 1",  #clic impair : ouverture/ fermeture
            br(),
            div(style = "background: linear-gradient(135deg, rgba(102,126,234,0.1) 0%, rgba(118,75,162,0.1) 100%);
                       padding: 20px; border-radius: 10px; border-left: 4px solid #667eea; margin-top: 15px;",
                p("Le système de recommandation repose sur une analyse des préférences sélectionnées ",
                  "par l'utilisateur et sur des correspondances avec les caractéristiques des films."),
                
                p("Plusieurs critères sont pris en compte (genres, durée, note, année) afin de ",
                  "calculer un score de pertinence, garantissant des recommandations cohérentes et fiables.")
            )
          ),
          
          ## Partie : La vision
          h2("La vision"),
          p("Notre objectif était de concevoir une application ",
            span("utile au quotidien", class = "highlight"),
            ", pensée avant tout pour l'utilisateur."),
          
          p("Pas de fonctionnalités inutiles, pas de complexité : l'idée est d'aller à l'essentiel."),
          p(
            "L'interface a été conçue pour être agréable à parcourir, permettant à l'utilisateur ",
            "de comprendre rapidement le fonctionnement de l'application et de trouver un film ",
            "sans effort, en quelques instants seulement."),
          
          ## Partie : Le casting
          h2("Créateurs"),
          p("Cette application a été développée par une équipe d'étudiants composée de :"),
          
          div(class = "creators",
              span("3RGuiz"),
              span("Alex6s7"),
              span("Valentin.mass"),
              span("Emilezolv")
          )
      )
    )
  )
)

# SERVER