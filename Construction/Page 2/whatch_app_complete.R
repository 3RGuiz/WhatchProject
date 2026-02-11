# =============================================================================
# Whatch? - Application complète
# =============================================================================
# Application Shiny d'aide au choix de films basée sur les émotions
# Tout-en-un : chargement données + configuration + UI + Server
# =============================================================================

library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)  # Pour de beaux boutons

# =============================================================================
# PARTIE 1 : CONFIGURATION DES ÉMOTIONS
# =============================================================================

EMOTION_CONFIG <- list(
  
  # =========================================================================
  # CATÉGORIE : ÉMOTIONS POSITIVES / LÉGÈRES
  # =========================================================================
  
  "rire" = list(
    emoji = "😂",
    label = "Rire",
    description = "Comédie légère, humour, bonne humeur",
    genres = c("Comedy"),
    alpha_preference = 0.6,
    rating_min = 6.5,
    duration_preference = "moyen",
    category = "positive"
  ),
  
  "detente" = list(
    emoji = "😌",
    label = "Détente",
    description = "Films feel-good, sans prise de tête",
    genres = c("Comedy", "Romance", "Family", "Animation"),
    alpha_preference = 0.7,
    rating_min = 6.8,
    duration_preference = "moyen",
    exclude_genres = c("Horror", "Thriller", "War"),
    category = "positive"
  ),
  
  "romance" = list(
    emoji = "💕",
    label = "Romance",
    description = "Histoires d'amour, émotions sentimentales",
    genres = c("Romance"),
    alpha_preference = 0.5,
    rating_min = 6.5,
    duration_preference = "moyen",
    category = "positive"
  ),
  
  "voyage" = list(
    emoji = "✈️",
    label = "Voyager",
    description = "Dépaysement, aventure, découverte",
    genres = c("Adventure", "Fantasy"),
    alpha_preference = 0.6,
    rating_min = 7.0,
    duration_preference = "long",
    category = "positive"
  ),
  
  "famille" = list(
    emoji = "👨‍👩‍👧‍👦",
    label = "En famille",
    description = "Films pour tous les âges",
    genres = c("Family", "Animation"),
    alpha_preference = 0.8,
    rating_min = 7.0,
    duration_preference = "moyen",
    exclude_genres = c("Horror", "Thriller", "War"),
    category = "positive"
  ),
  
  "spectacle" = list(
    emoji = "🎪",
    label = "Spectacle",
    description = "Musicaux, performances, show",
    genres = c("Musical", "Music"),
    alpha_preference = 0.6,
    rating_min = 7.0,
    duration_preference = "moyen",
    category = "positive"
  ),
  
  # =========================================================================
  # CATÉGORIE : ÉMOTIONS INTENSES
  # =========================================================================
  
  "action" = list(
    emoji = "💥",
    label = "Action",
    description = "Scènes d'action, adrénaline, rythme soutenu",
    genres = c("Action"),
    alpha_preference = 0.7,
    rating_min = 6.5,
    duration_preference = "moyen",
    category = "intense"
  ),
  
  "peur" = list(
    emoji = "😱",
    label = "Peur",
    description = "Films d'horreur, jump scares",
    genres = c("Horror"),
    alpha_preference = 0.4,
    rating_min = 6.0,
    duration_preference = "moyen",
    category = "intense"
  ),
  
  "angoisse" = list(
    emoji = "😰",
    label = "Angoisse",
    description = "Thriller psychologique, suspense, tension",
    genres = c("Thriller", "Mystery"),
    alpha_preference = 0.3,
    rating_min = 7.0,
    duration_preference = "moyen",
    category = "intense"
  ),
  
  "frissons" = list(
    emoji = "🥶",
    label = "Frissons",
    description = "Horreur psychologique, ambiance glauque",
    genres = c("Horror", "Thriller"),
    alpha_preference = 0.3,
    rating_min = 6.8,
    duration_preference = "moyen",
    category = "intense"
  ),
  
  "epique" = list(
    emoji = "⚔️",
    label = "Épique",
    description = "Grandes fresques, batailles, héroïsme",
    genres = c("War", "History", "Adventure"),
    alpha_preference = 0.6,
    rating_min = 7.5,
    duration_preference = "long",
    category = "intense"
  ),
  
  "passion" = list(
    emoji = "🔥",
    label = "Passion",
    description = "Drames passionnels, intensité émotionnelle",
    genres = c("Drama", "Romance"),
    alpha_preference = 0.3,
    rating_min = 7.0,
    duration_preference = "moyen",
    category = "intense"
  ),
  
  # =========================================================================
  # CATÉGORIE : ÉMOTIONS PROFONDES / RÉFLEXIVES
  # =========================================================================
  
  "pleurer" = list(
    emoji = "😢",
    label = "Pleurer",
    description = "Drames émotionnels, histoires touchantes",
    genres = c("Drama"),
    alpha_preference = 0.2,
    rating_min = 7.5,
    duration_preference = "long",
    exclude_genres = c("Horror", "Comedy"),
    category = "profonde"
  ),
  
  "reflechir" = list(
    emoji = "🤔",
    label = "Réfléchir",
    description = "Films intellectuels, documentaires, biopics",
    genres = c("Documentary", "Biography", "History"),
    alpha_preference = 0.2,
    rating_min = 7.5,
    duration_preference = "any",
    category = "profonde"
  ),
  
  "interroger" = list(
    emoji = "❓",
    label = "M'interroger",
    description = "Science-fiction conceptuelle, philosophie",
    genres = c("Sci-Fi", "Mystery"),
    alpha_preference = 0.3,
    rating_min = 7.2,
    duration_preference = "long",
    category = "profonde"
  ),
  
  "meditation" = list(
    emoji = "🧘",
    label = "Méditer",
    description = "Slow cinema, films contemplatifs, poétiques",
    genres = c("Drama"),
    alpha_preference = 0.1,
    rating_min = 7.5,
    duration_preference = "any",
    category = "profonde"
  ),
  
  "culture" = list(
    emoji = "🎓",
    label = "Culture",
    description = "Classiques incontournables, patrimoine",
    genres = NULL,
    alpha_preference = 0.3,
    rating_min = 8.0,
    duration_preference = "any",
    year_range = c(1920, 2000),
    category = "profonde"
  ),
  
  # =========================================================================
  # CATÉGORIE : ÉMOTIONS SPÉCIFIQUES
  # =========================================================================
  
  "nostalgie" = list(
    emoji = "🕰️",
    label = "Nostalgie",
    description = "Films rétro, années 70-90",
    genres = NULL,
    alpha_preference = 0.5,
    rating_min = 7.5,
    duration_preference = "any",
    year_range = c(1970, 1999),
    category = "specifique"
  ),
  
  "surprise" = list(
    emoji = "🎲",
    label = "Surprise",
    description = "Films atypiques, twists, originalité",
    genres = c("Mystery", "Thriller", "Sci-Fi"),
    alpha_preference = 0.1,
    rating_min = 7.5,
    duration_preference = "any",
    boost_low_votes = TRUE,
    category = "specifique"
  ),
  
  "mystere" = list(
    emoji = "🔍",
    label = "Mystère",
    description = "Enquêtes, puzzles, révélations",
    genres = c("Mystery", "Crime"),
    alpha_preference = 0.4,
    rating_min = 7.0,
    duration_preference = "moyen",
    category = "specifique"
  ),
  
  "esthetique" = list(
    emoji = "🎨",
    label = "Esthétique",
    description = "Films visuellement marquants, photographie",
    genres = c("Drama", "Fantasy", "Sci-Fi"),
    alpha_preference = 0.2,
    rating_min = 7.5,
    duration_preference = "any",
    category = "specifique"
  ),
  
  "nocturne" = list(
    emoji = "🌙",
    label = "Nocturne",
    description = "Film noir, néo-noir, ambiance sombre",
    genres = c("Crime", "Thriller", "Mystery"),
    alpha_preference = 0.3,
    rating_min = 7.2,
    duration_preference = "moyen",
    category = "specifique"
  ),
  
  "rapide" = list(
    emoji = "⚡",
    label = "Rapide",
    description = "Films courts et efficaces < 90 min",
    genres = NULL,
    alpha_preference = 0.5,
    rating_min = 7.0,
    duration_preference = "court",
    duration_max_override = 90,
    category = "specifique"
  ),
  
  "monde" = list(
    emoji = "🌍",
    label = "Monde",
    description = "Cinéma international, diversité culturelle",
    genres = NULL,
    alpha_preference = 0.2,
    rating_min = 7.5,
    duration_preference = "any",
    category = "specifique"
  )
)

# =============================================================================
# PARTIE 2 : FONCTIONS UTILITAIRES
# =============================================================================

# Fonction d'application des filtres émotionnels
apply_emotion_filter <- function(data, emotion_key, custom_alpha = NULL, 
                                  custom_year = NULL, custom_duration = NULL) {
  
  config <- EMOTION_CONFIG[[emotion_key]]
  
  # Filtrer par genres
  if (!is.null(config$genres)) {
    data <- data %>%
      filter(map_lgl(genres_list, ~ any(.x %in% config$genres)))
  }
  
  # Exclure certains genres
  if (!is.null(config$exclude_genres)) {
    data <- data %>%
      filter(map_lgl(genres_list, ~ !any(.x %in% config$exclude_genres)))
  }
  
  # Filtrer par note minimale
  data <- data %>% filter(rating >= config$rating_min)
  
  # Filtrer par durée
  if (!is.null(custom_duration)) {
    data <- data %>% filter(runtime <= custom_duration)
  } else if (!is.null(config$duration_max_override)) {
    data <- data %>% filter(runtime <= config$duration_max_override)
  } else if (!is.null(config$duration_preference) && config$duration_preference != "any") {
    if (config$duration_preference == "court") {
      data <- data %>% filter(runtime <= 100)
    } else if (config$duration_preference == "moyen") {
      data <- data %>% filter(runtime >= 90, runtime <= 130)
    } else if (config$duration_preference == "long") {
      data <- data %>% filter(runtime >= 120)
    }
  }
  
  # Filtrer par période
  if (!is.null(custom_year)) {
    data <- data %>% filter(year >= custom_year[1], year <= custom_year[2])
  } else if (!is.null(config$year_range)) {
    data <- data %>% filter(year >= config$year_range[1], year <= config$year_range[2])
  }
  
  # Calculer le score
  alpha_to_use <- if (!is.null(custom_alpha)) custom_alpha else config$alpha_preference
  
  data <- data %>%
    mutate(composite_score = (1 - alpha_to_use) * quality_score + alpha_to_use * popularity_score)
  
  # Boost spécial pour "surprise"
  if (!is.null(config$boost_low_votes) && config$boost_low_votes) {
    data <- data %>%
      mutate(composite_score = composite_score + (1 - popularity_score) * 0.2)
  }
  
  # Trier
  data <- data %>% arrange(desc(composite_score))
  
  return(data)
}

# =============================================================================
# PARTIE 3 : INTERFACE UTILISATEUR
# =============================================================================

ui <- fluidPage(
  
  # CSS personnalisé
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        min-height: 100vh;
      }
      .main-container {
        background: white;
        border-radius: 15px;
        padding: 30px;
        margin: 20px auto;
        max-width: 1400px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.3);
      }
      .emotion-btn {
        width: 100%;
        margin: 5px 0;
        padding: 12px;
        font-size: 15px;
        border-radius: 8px;
        border: 2px solid #e0e0e0;
        background: white;
        transition: all 0.3s;
        text-align: left;
      }
      .emotion-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        border-color: #2196F3;
      }
      .emotion-btn.active {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-color: #667eea;
        font-weight: bold;
      }
      .category-header {
        font-size: 16px;
        font-weight: bold;
        color: #666;
        margin: 15px 0 10px 0;
        padding: 8px;
        background: #f5f5f5;
        border-left: 4px solid #2196F3;
      }
      .stats-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin: 10px 0;
        text-align: center;
      }
      .stats-number {
        font-size: 28px;
        font-weight: bold;
        margin: 0;
      }
      .stats-label {
        font-size: 13px;
        opacity: 0.9;
        margin: 5px 0 0 0;
      }
      h1, h2, h3 {
        color: #333;
      }
      .title-section {
        text-align: center;
        margin-bottom: 30px;
        padding-bottom: 20px;
        border-bottom: 3px solid #667eea;
      }
      .btn-search {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border: none;
        color: white;
        font-size: 16px;
        padding: 12px;
        border-radius: 8px;
        font-weight: bold;
      }
      .btn-search:hover {
        opacity: 0.9;
        transform: translateY(-2px);
      }
    "))
  ),
  
  div(class = "main-container",
    
    # En-tête
    div(class = "title-section",
      h1("🎬 Whatch?", style = "margin: 0; font-size: 48px; color: #667eea;"),
      p("Trouvez le film parfait pour votre humeur", 
        style = "margin: 10px 0 0 0; font-size: 18px; color: #666;")
    ),
    
    fluidRow(
      
      # =====================================================================
      # COLONNE GAUCHE : SÉLECTION ÉMOTIONS
      # =====================================================================
      
      column(4,
        
        h3("💭 J'ai envie de...", style = "color: #667eea;"),
        
        # Émotions positives
        div(class = "category-header", "😊 Émotions légères"),
        uiOutput("emotions_positive"),
        
        # Émotions intenses
        div(class = "category-header", "💥 Émotions intenses"),
        uiOutput("emotions_intense"),
        
        # Émotions profondes
        div(class = "category-header", "🧠 Émotions profondes"),
        uiOutput("emotions_profonde"),
        
        # Émotions spécifiques
        div(class = "category-header", "🎯 Situations spécifiques"),
        uiOutput("emotions_specifique"),
        
        br(),
        
        # Options avancées
        checkboxInput("show_advanced", "⚙️ Options avancées", value = FALSE),
        
        conditionalPanel(
          condition = "input.show_advanced == true",
          
          sliderInput("alpha_override", "Découverte ↔ Populaire",
                      min = 0, max = 1, value = 0.5, step = 0.05),
          
          sliderInput("year_range", "Période",
                      min = 1920, max = 2026, value = c(1990, 2026), 
                      step = 1, sep = ""),
          
          sliderInput("duration_max", "Durée max (min)",
                      min = 60, max = 240, value = 180, step = 10)
        ),
        
        br(),
        
        # Boutons d'action
        actionButton("search", "🔍 Trouver mon film", 
                     class = "btn-search btn-block btn-lg"),
        
        br(),
        
        actionButton("random", "🎲 Film au hasard", 
                     class = "btn btn-warning btn-block")
      ),
      
      # =====================================================================
      # COLONNE DROITE : RÉSULTATS
      # =====================================================================
      
      column(8,
        
        # Statistiques
        uiOutput("stats_panel"),
        
        br(),
        
        # Message si aucune sélection
        conditionalPanel(
          condition = "input.emotion_selected == null || input.emotion_selected == ''",
          div(style = "text-align: center; padding: 80px 20px; color: #999;",
            h2("👈 Choisissez votre humeur", style = "color: #999; border: none;"),
            p("Sélectionnez une émotion dans la liste de gauche", 
              style = "font-size: 16px;")
          )
        ),
        
        # Résultats
        conditionalPanel(
          condition = "input.emotion_selected != null && input.emotion_selected != ''",
          
          h3("🎯 Films recommandés"),
          DTOutput("results_table")
        )
      )
    )
  )
)

# =============================================================================
# PARTIE 4 : LOGIQUE SERVEUR
# =============================================================================

server <- function(input, output, session) {
  
  # ===========================================================================
  # CHARGEMENT DES DONNÉES
  # ===========================================================================
  
  movies <- reactive({
    req(file.exists("data/processed/movies_final.rds"))
    
    data <- readRDS("data/processed/movies_final.rds")
    
    data %>%
      mutate(
        year = as.integer(year),
        runtime = as.integer(runtime),
        rating = as.numeric(rating),
        votes = as.integer(votes),
        quality_score = as.numeric(quality_score),
        popularity_score = as.numeric(popularity_score)
      )
  })
  
  # ===========================================================================
  # GÉNÉRATION DES BOUTONS ÉMOTIONS PAR CATÉGORIE
  # ===========================================================================
  
  # Variable réactive pour l'émotion sélectionnée
  emotion_selected <- reactiveVal(NULL)
  
  create_emotion_buttons <- function(category) {
    emotions <- Filter(function(x) x$category == category, EMOTION_CONFIG)
    
    lapply(names(emotions), function(key) {
      config <- emotions[[key]]
      
      actionButton(
        inputId = paste0("emotion_", key),
        label = HTML(paste0(config$emoji, " ", config$label)),
        class = "emotion-btn",
        onclick = sprintf("Shiny.setInputValue('emotion_selected', '%s', {priority: 'event'})", key)
      )
    })
  }
  
  output$emotions_positive <- renderUI({ create_emotion_buttons("positive") })
  output$emotions_intense <- renderUI({ create_emotion_buttons("intense") })
  output$emotions_profonde <- renderUI({ create_emotion_buttons("profonde") })
  output$emotions_specifique <- renderUI({ create_emotion_buttons("specifique") })
  
  # ===========================================================================
  # FILTRAGE DES DONNÉES
  # ===========================================================================
  
  filtered_movies <- eventReactive(input$search, {
    
    req(input$emotion_selected)
    req(movies())
    
    alpha <- if (input$show_advanced) input$alpha_override else NULL
    year <- if (input$show_advanced) input$year_range else NULL
    duration <- if (input$show_advanced) input$duration_max else NULL
    
    apply_emotion_filter(movies(), input$emotion_selected, alpha, year, duration)
  })
  
  # Film aléatoire
  random_movie <- eventReactive(input$random, {
    req(movies())
    movies() %>%
      filter(rating >= 7.5, votes >= 5000) %>%
      sample_n(1)
  })
  
  # ===========================================================================
  # OUTPUTS
  # ===========================================================================
  
  # Statistiques
  output$stats_panel <- renderUI({
    
    req(input$search)
    req(nrow(filtered_movies()) > 0)
    
    data <- filtered_movies()
    
    fluidRow(
      column(3, 
        div(class = "stats-box",
          h3(format(nrow(data), big.mark = " "), class = "stats-number"),
          p("films trouvés", class = "stats-label")
        )
      ),
      column(3,
        div(class = "stats-box",
          h3(sprintf("%.1f/10", mean(data$rating)), class = "stats-number"),
          p("note moyenne", class = "stats-label")
        )
      ),
      column(3,
        div(class = "stats-box",
          h3(sprintf("%.0f min", median(data$runtime)), class = "stats-number"),
          p("durée médiane", class = "stats-label")
        )
      ),
      column(3,
        div(class = "stats-box",
          h3(sprintf("%d-%d", min(data$year), max(data$year)), 
             class = "stats-number", style = "font-size: 20px;"),
          p("période", class = "stats-label")
        )
      )
    )
  })
  
  # Tableau des résultats
  output$results_table <- renderDT({
    
    # Film aléatoire
    if (input$random > 0 && (is.null(input$emotion_selected) || input$emotion_selected == "")) {
      
      data_to_show <- random_movie() %>%
        select(Titre = title, Année = year, Durée = runtime, 
               Genres = genres, Note = rating, Votes = votes) %>%
        mutate(Votes = format(Votes, big.mark = " "))
      
      return(
        datatable(
          data_to_show,
          options = list(pageLength = 1, dom = 't', ordering = FALSE),
          rownames = FALSE
        ) %>%
          formatStyle(columns = 1:6, backgroundColor = '#fff9c4', fontWeight = 'bold')
      )
    }
    
    # Résultats normaux
    req(input$search)
    req(nrow(filtered_movies()) > 0)
    
    filtered_movies() %>%
      head(100) %>%
      select(Score = composite_score, Titre = title, Année = year, 
             Durée = runtime, Genres = genres, Note = rating, Votes = votes) %>%
      mutate(
        Score = round(Score, 3),
        Votes = format(Votes, big.mark = " "),
        Durée = paste0(Durée, " min")
      ) %>%
      datatable(
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          dom = 'lftip',
          ordering = TRUE,
          order = list(list(0, 'desc'))
        ),
        rownames = FALSE,
        class = "display nowrap compact"
      ) %>%
        formatStyle(
          'Note',
          background = styleColorBar(range(filtered_movies()$rating), '#90EE90'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'Score',
          fontWeight = 'bold',
          color = styleInterval(
            cuts = c(0.7, 0.8, 0.9),
            values = c('#666', '#1976D2', '#0D47A1', '#01579B')
          )
        )
  })
  
  # ===========================================================================
  # MISE À JOUR VISUELLE DES BOUTONS
  # ===========================================================================
  
  # Ajouter/retirer classe 'active' avec JavaScript
  observeEvent(input$emotion_selected, {
    session$sendCustomMessage(
      type = 'emotion_selected',
      message = list(emotion = input$emotion_selected)
    )
  })
}

# =============================================================================
# PARTIE 5 : LANCEMENT DE L'APPLICATION
# =============================================================================

# JavaScript pour gérer la sélection visuelle
js_code <- "
Shiny.addCustomMessageHandler('emotion_selected', function(message) {
  $('.emotion-btn').removeClass('active');
  $('#emotion_' + message.emotion).addClass('active');
});
"

ui <- tagList(
  tags$head(tags$script(HTML(js_code))),
  ui
)

shinyApp(ui = ui, server = server)
