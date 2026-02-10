# =============================================================================
# Whatch? - Application complète (Version corrigée)
# =============================================================================
# Application Shiny d'aide au choix de films basée sur les émotions
# =============================================================================

library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)

# =============================================================================
# PARTIE 1 : CONFIGURATION DES ÉMOTIONS
# =============================================================================

EMOTION_CONFIG <- list(
  
  # =========================================================================
  # CATÉGORIE : ÉMOTIONS POSITIVES / LÉGÈRES
  # =========================================================================
  
  "rire" = list(
    label = "Rire",
    description = "Comédies légères et humoristiques",
    genres = c("Comedy"),
    exclude_genres = c("Drama", "War", "Horror"),
    alpha_preference = 0.6,
    rating_min = 6.5,
    duration_preference = "moyen",
    category = "positive"
  ),
  
  "detente" = list(
    label = "Détente",
    description = "Films feel-good sans prise de tête",
    genres = c("Comedy", "Romance", "Family", "Animation"),
    alpha_preference = 0.7,
    rating_min = 6.8,
    duration_preference = "moyen",
    exclude_genres = c("Horror", "Thriller", "War", "Crime"),
    category = "positive"
  ),
  
  "romance" = list(
    label = "Romance",
    description = "Histoires d'amour et relations sentimentales",
    genres = c("Romance"),
    exclude_genres = c("Horror", "War"),
    alpha_preference = 0.5,
    rating_min = 6.5,
    duration_preference = "moyen",
    category = "positive"
  ),
  
  "voyage" = list(
    label = "Voyager",
    description = "Aventures et découvertes dans des lieux exotiques",
    genres = c("Adventure"),
    exclude_genres = c("Horror", "War"),
    alpha_preference = 0.6,
    rating_min = 7.0,
    duration_preference = "long",
    category = "positive"
  ),
  
  "famille" = list(
    label = "En famille",
    description = "Films adaptés à tous les âges",
    genres = c("Family", "Animation"),
    alpha_preference = 0.8,
    rating_min = 7.0,
    duration_preference = "moyen",
    exclude_genres = c("Horror", "Thriller", "War", "Crime"),
    category = "positive"
  ),
  
  # =========================================================================
  # CATÉGORIE : ÉMOTIONS INTENSES
  # =========================================================================
  
  "action" = list(
    label = "Action",
    description = "Films d'action avec scènes spectaculaires",
    genres = c("Action"),
    exclude_genres = c("Romance", "Documentary"),
    alpha_preference = 0.7,
    rating_min = 6.5,
    duration_preference = "moyen",
    category = "intense"
  ),
  
  "peur" = list(
    label = "Peur",
    description = "Films d'horreur pour avoir peur",
    genres = c("Horror"),
    exclude_genres = c("Comedy", "Family", "Animation"),
    alpha_preference = 0.4,
    rating_min = 6.0,
    duration_preference = "moyen",
    category = "intense"
  ),
  
  "angoisse" = list(
    label = "Angoisse",
    description = "Thrillers psychologiques et suspense intense",
    genres = c("Thriller"),
    exclude_genres = c("Comedy", "Family", "Animation", "Romance"),
    alpha_preference = 0.3,
    rating_min = 7.0,
    duration_preference = "moyen",
    category = "intense"
  ),
  
  "epique" = list(
    label = "Épique",
    description = "Grandes fresques historiques et batailles",
    genres = c("War", "History"),
    exclude_genres = c("Comedy", "Romance", "Horror"),
    alpha_preference = 0.6,
    rating_min = 7.5,
    duration_preference = "long",
    category = "intense"
  ),
  
  # =========================================================================
  # CATÉGORIE : ÉMOTIONS PROFONDES / RÉFLEXIVES
  # =========================================================================
  
  "pleurer" = list(
    label = "Pleurer",
    description = "Drames émotionnels et histoires touchantes",
    genres = c("Drama"),
    exclude_genres = c("Horror", "Comedy", "Action", "Thriller", "Crime"),
    alpha_preference = 0.2,
    rating_min = 7.5,
    duration_preference = "long",
    category = "profonde"
  ),
  
  "reflechir" = list(
    label = "Réfléchir",
    description = "Documentaires et films biographiques instructifs",
    genres = c("Documentary", "Biography"),
    exclude_genres = c("Horror", "Comedy", "Action"),
    alpha_preference = 0.2,
    rating_min = 7.5,
    duration_preference = "any",
    category = "profonde"
  ),
  
  "interroger" = list(
    label = "M'interroger",
    description = "Science-fiction conceptuelle et philosophique",
    genres = c("Sci-Fi"),
    exclude_genres = c("Horror", "Comedy", "Romance"),
    alpha_preference = 0.3,
    rating_min = 7.2,
    duration_preference = "long",
    category = "profonde"
  ),
  
  "contempler" = list(
    label = "Contempler",
    description = "Films contemplatifs à rythme lent",
    genres = c("Drama"),
    exclude_genres = c("Action", "Horror", "Thriller", "Comedy", "Crime", "War"),
    alpha_preference = 0.1,
    rating_min = 7.8,
    duration_preference = "any",
    category = "profonde"
  ),
  
  # =========================================================================
  # CATÉGORIE : ÉMOTIONS SPÉCIFIQUES
  # =========================================================================
  
  "nostalgie" = list(
    label = "Nostalgie",
    description = "Classiques des années 70-90",
    genres = NULL,
    alpha_preference = 0.5,
    rating_min = 7.5,
    duration_preference = "any",
    year_range = c(1970, 1999),
    category = "specifique"
  ),
  
  "surprise" = list(
    label = "Surprise",
    description = "Films atypiques et originaux peu connus",
    genres = c("Mystery", "Sci-Fi"),
    exclude_genres = c("Romance", "Comedy"),
    alpha_preference = 0.1,
    rating_min = 7.5,
    duration_preference = "any",
    boost_low_votes = TRUE,
    category = "specifique"
  ),
  
  "mystere" = list(
    label = "Mystère",
    description = "Enquêtes policières et énigmes",
    genres = c("Mystery", "Crime"),
    exclude_genres = c("Horror", "Comedy", "Romance"),
    alpha_preference = 0.4,
    rating_min = 7.0,
    duration_preference = "moyen",
    category = "specifique"
  ),
  
  "fantastique" = list(
    label = "Fantastique",
    description = "Mondes imaginaires et magie",
    genres = c("Fantasy"),
    exclude_genres = c("Horror", "War"),
    alpha_preference = 0.6,
    rating_min = 7.0,
    duration_preference = "long",
    category = "specifique"
  ),
  
  "western" = list(
    label = "Western",
    description = "Far West et duels de cowboys",
    genres = c("Western"),
    alpha_preference = 0.4,
    rating_min = 7.0,
    duration_preference = "moyen",
    category = "specifique"
  )
)

# =============================================================================
# PARTIE 2 : FONCTIONS UTILITAIRES
# =============================================================================

apply_emotion_filter <- function(data, emotion_key, custom_alpha = NULL, 
                                  custom_year = NULL, custom_duration = NULL) {
  
  config <- EMOTION_CONFIG[[emotion_key]]
  
  # Filtrer par genres (AND logic - tous les genres doivent être présents)
  if (!is.null(config$genres)) {
    data <- data %>%
      filter(map_lgl(genres_list, ~ any(.x %in% config$genres)))
  }
  
  # Exclure certains genres (CRITICAL)
  if (!is.null(config$exclude_genres)) {
    data <- data %>%
      filter(map_lgl(genres_list, ~ !any(.x %in% config$exclude_genres)))
  }
  
  # Filtrer par note minimale
  data <- data %>% filter(rating >= config$rating_min)
  
  # Filtrer par durée
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
        max-width: 1600px;
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
        position: relative;
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
      .emotion-btn:hover .tooltip-text {
        visibility: visible;
        opacity: 1;
      }
      .category-header {
        font-size: 15px;
        font-weight: bold;
        color: #667eea;
        margin: 15px 0 10px 0;
        padding: 10px;
        background: #f5f5f5;
        border-left: 4px solid #667eea;
        cursor: pointer;
        transition: all 0.3s;
        user-select: none;
      }
      .category-header:hover {
        background: #e3f2fd;
      }
      .category-header .toggle-icon {
        float: right;
        transition: transform 0.3s;
      }
      .category-header.collapsed .toggle-icon {
        transform: rotate(-90deg);
      }
      .category-emotions {
        max-height: 500px;
        overflow: hidden;
        transition: max-height 0.3s ease-out;
      }
      .category-emotions.collapsed {
        max-height: 0;
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
      .btn-deselect {
        background: #e0e0e0;
        border: none;
        color: #666;
        font-size: 14px;
        padding: 8px;
        border-radius: 6px;
        margin-top: 10px;
      }
      .btn-deselect:hover {
        background: #d0d0d0;
      }
      .dataTables_wrapper {
        width: 100% !important;
      }
    "))
  ),
  
  div(class = "main-container",
    
    # En-tête
    div(class = "title-section",
      h1("🎬 Whatch?", style = "margin: 0; font-size: 48px; color: #667eea;"),
      p("Abracadabra, on choisit pour toi!", 
        style = "margin: 10px 0 0 0; font-size: 18px; color: #666;")
    ),
    
    fluidRow(
      
      # =====================================================================
      # COLONNE GAUCHE : SÉLECTION ÉMOTIONS
      # =====================================================================
      
      column(3,
        
        h3("💭 J'ai envie de...", style = "color: #667eea;"),
        
        # Émotions positives
        div(
          class = "category-header",
          onclick = "toggleCategory('positive')",
          "😊 Émotions légères",
          tags$span(class = "toggle-icon", "▼")
        ),
        div(id = "category-positive", class = "category-emotions",
          uiOutput("emotions_positive")
        ),
        
        # Émotions intenses
        div(
          class = "category-header",
          onclick = "toggleCategory('intense')",
          "💥 Émotions intenses",
          tags$span(class = "toggle-icon", "▼")
        ),
        div(id = "category-intense", class = "category-emotions collapsed",
          uiOutput("emotions_intense")
        ),
        
        # Émotions profondes
        div(
          class = "category-header",
          onclick = "toggleCategory('profonde')",
          "🧠 Émotions profondes",
          tags$span(class = "toggle-icon", "▼")
        ),
        div(id = "category-profonde", class = "category-emotions collapsed",
          uiOutput("emotions_profonde")
        ),
        
        # Émotions spécifiques
        div(
          class = "category-header",
          onclick = "toggleCategory('specifique')",
          "🎯 Situations spécifiques",
          tags$span(class = "toggle-icon", "▼")
        ),
        div(id = "category-specifique", class = "category-emotions collapsed",
          uiOutput("emotions_specifique")
        ),
        
        # Bouton désélectionner
        actionButton("deselect", "✕ Désélectionner l'émotion", 
                     class = "btn-deselect btn-block"),
        
        br(),
        
        # Options avancées
        checkboxInput("show_advanced", "⚙️ Options avancées", value = FALSE),
        
        conditionalPanel(
          condition = "input.show_advanced == true",
          
          sliderInput("alpha_override", "Découverte ↔ Populaire",
                      min = 0, max = 1, value = 0.5, step = 0.05),
          
          helpText(style = "font-size: 11px; color: #666;",
                   "← Pépites méconnues | Films populaires →"),
          
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
        
        actionButton("random", "🎲 Au hasard !", 
                     class = "btn btn-warning btn-block"),
        
        br(),
        
        helpText(style = "font-size: 12px; color: #999; text-align: center;",
                 "💡 Le bouton 'Au hasard' propose un film aléatoire bien noté")
      ),
      
      # =====================================================================
      # COLONNE DROITE : RÉSULTATS
      # =====================================================================
      
      column(9,
        
        # Statistiques
        uiOutput("stats_panel"),
        
        br(),
        
        # Message si aucune sélection
        conditionalPanel(
          condition = "input.emotion_selected == null || input.emotion_selected == ''",
          div(style = "text-align: center; padding: 80px 20px; color: #999;",
            h2("👈 Choisissez votre humeur pour commencer", style = "color: #999; border: none;"),
            p("Sélectionnez une émotion ou cliquez sur 'Au hasard'", style = "font-size: 16px;")
          )
        ),
        
        # Résultats
        conditionalPanel(
          condition = "input.emotion_selected != null && input.emotion_selected != ''",
          h3("🎯 Films recommandés"),
          DTOutput("results_table", width = "100%")
        )
      )
    )
  ),
  
  # JavaScript pour toggle des catégories
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
  "))
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
  # GÉNÉRATION DES BOUTONS ÉMOTIONS
  # ===========================================================================
  
  emotion_selected <- reactiveVal(NULL)
  
  create_emotion_buttons <- function(category) {
    emotions <- Filter(function(x) x$category == category, EMOTION_CONFIG)
    
    lapply(names(emotions), function(key) {
      config <- emotions[[key]]
      
      actionButton(
        inputId = paste0("emotion_", key),
        label = HTML(paste0(
          config$label,
          '<span class="tooltip-text">', config$description, '</span>'
        )),
        class = "emotion-btn",
        onclick = sprintf("
          $('.emotion-btn').removeClass('active');
          $(this).addClass('active');
          Shiny.setInputValue('emotion_selected', '%s', {priority: 'event'});
        ", key)
      )
    })
  }
  
  output$emotions_positive <- renderUI({ create_emotion_buttons("positive") })
  output$emotions_intense <- renderUI({ create_emotion_buttons("intense") })
  output$emotions_profonde <- renderUI({ create_emotion_buttons("profonde") })
  output$emotions_specifique <- renderUI({ create_emotion_buttons("specifique") })
  
  # Bouton désélectionner
  observeEvent(input$deselect, {
    session$sendCustomMessage(type = 'deselect_emotion', message = list())
    updateTextInput(session, "emotion_selected", value = "")
  })
  
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
  
  output$results_table <- renderDT({
    
    if (input$random > 0 && (is.null(input$emotion_selected) || input$emotion_selected == "")) {
      
      data_to_show <- random_movie() %>%
        select(Titre = title, Année = year, Durée = runtime, 
               Genres = genres, Note = rating, Votes = votes) %>%
        mutate(Votes = format(Votes, big.mark = " "))
      
      return(
        datatable(
          data_to_show,
          options = list(pageLength = 1, dom = 't', ordering = FALSE, scrollX = TRUE),
          rownames = FALSE
        ) %>%
          formatStyle(columns = 1:6, backgroundColor = '#fff9c4', fontWeight = 'bold')
      )
    }
    
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
          order = list(list(0, 'desc')),
          scrollX = TRUE,
          autoWidth = TRUE
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
}

# JavaScript pour désélectionner
ui <- tagList(
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler('deselect_emotion', function(message) {
      $('.emotion-btn').removeClass('active');
      Shiny.setInputValue('emotion_selected', '', {priority: 'event'});
    });
  "))),
  ui
)

# =============================================================================
# LANCEMENT
# =============================================================================

shinyApp(ui = ui, server = server)
