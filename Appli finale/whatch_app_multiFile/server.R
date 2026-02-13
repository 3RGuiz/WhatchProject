# SERVER

server <- function(input, output, session) {
  
  # Navigation depuis la page d'accueil
  observeEvent(input$demarrer, {
    updateNavbarPage(session, "navbar", selected = "selection")
  })
  
  # Variables réactives
  selected_movie <- reactiveVal(NULL)
  show_details_page <- reactiveVal(FALSE)
  
  #Charger les données
  
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
  
  # Boutons "émotions"
  
  create_emotion_buttons <- function(category) {
    emotions <- Filter(function(x) x$category == category, EMOTION_CONFIG)
    
    lapply(names(emotions), function(key) {
      config <- emotions[[key]]
      
      actionButton(
        inputId = paste0("emotion_", key),
        label = tagList(
          span(config$label),
          tags$span(class = "tooltip-text", config$description)
        ),
        class = "emotion-btn",
        onclick = sprintf("
          $('.emotion-btn').removeClass('active');
          $(this).addClass('active');
          Shiny.setInputValue('emotion_selected', '%s', {priority: 'event'});
        ", key)
      )
    })
  }
  
  output$emotions_positive   <- renderUI({ create_emotion_buttons("positive") })
  output$emotions_intense    <- renderUI({ create_emotion_buttons("intense") })
  output$emotions_profonde   <- renderUI({ create_emotion_buttons("profonde") })
  output$emotions_specifique <- renderUI({ create_emotion_buttons("specifique") })
  
  observeEvent(input$deselect, {
    session$sendCustomMessage(type = "deselect_emotion", message = list())
  })
  
  #Filtrage des données
  
  #Filtrage des données
  
  # Créer un reactive qui se met à jour automatiquement
  filtered_movies <- reactive({
    req(movies())
    
    # PRIORITÉ À LA RECHERCHE PAR TITRE
    if (!is.null(input$search_title) && nzchar(trimws(input$search_title))) {
      search_term <- tolower(trimws(input$search_title))
      result <- movies() %>%
        filter(str_detect(tolower(title), fixed(search_term)))
      
      # Appliquer le filtre de note minimale si options avancées activées
      if (isTRUE(input$show_advanced) && !is.null(input$min_rating)) {
        result <- result %>% filter(rating >= input$min_rating)
      }
      
      return(result %>% arrange(desc(rating)))
    }
    
    # SINON : filtrage par émotion (seulement si bouton cliqué)
    req(input$search > 0)  # Nécessite qu'on ait cliqué sur "Trouver mon film"
    req(input$emotion_selected)
    
    alpha       <- if (isTRUE(input$show_advanced)) input$alpha_override else NULL
    year        <- if (isTRUE(input$show_advanced)) input$year_range else NULL
    duration    <- if (isTRUE(input$show_advanced)) input$duration_max else NULL
    min_rating  <- if (isTRUE(input$show_advanced)) input$min_rating else NULL
    
    apply_emotion_filter(movies(), input$emotion_selected, alpha, year, duration, min_rating)
  })
  
  #Stats
  
  output$stats_panel <- renderUI({
    req(input$emotion_selected)
    
    config <- EMOTION_CONFIG[[input$emotion_selected]]
    
    fluidRow(
      column(4, div(class = "stats-box",
                    p(class = "stats-number", config$label),
                    p(class = "stats-label", "ÉMOTION SÉLECTIONNÉE"))),
      column(4, div(class = "stats-box",
                    p(class = "stats-number", round(config$alpha_preference * 100), "%"),
                    p(class = "stats-label", "POIDS POPULARITÉ"))),
      column(4, div(class = "stats-box",
                    p(class = "stats-number", config$rating_min, "/10"),
                    p(class = "stats-label", "NOTE MINIMALE")))
    )
  })
  
  #Film aléatoire
  
  random_movie <- eventReactive(input$random, {
    req(movies())
    
    top_rated <- movies() %>%
      filter(rating >= 7.5, votes >= 1000) %>%
      arrange(desc(rating)) %>%
      head(500)
    
    if (nrow(top_rated) > 0) {
      sample_n(top_rated, 1)
    } else {
      NULL
    }
  })
  
  #Résultats
  
  output$results_table <- renderDT({
    # Mode aléatoire
    if (input$random > 0 && (is.null(input$emotion_selected) || input$emotion_selected == "") && 
        (is.null(input$search_title) || input$search_title == "")) {
      data_to_show <- random_movie() %>%
        mutate(composite_score = rating / 10) %>%
        head(1)
    } else {
      # Mode recherche ou émotion
      data_to_show <- tryCatch({
        filtered_movies() %>% head(100)
      }, error = function(e) {
        return(NULL)
      })
    }
    
    req(!is.null(data_to_show))
    req(nrow(data_to_show) > 0)
    
    # Vérifier si on est en mode recherche par titre
    is_search_mode <- !is.null(input$search_title) && nzchar(trimws(input$search_title))
    
    if (is_search_mode) {
      # Affichage simplifié sans score pour la recherche
      display <- data_to_show %>%
        transmute(
          Titre = sprintf('<a href="#" onclick="Shiny.setInputValue(\'movie_click\', \'%s\', {priority: \'event\'}); return false;">%s</a>',
                          tconst, title),
          Année = year,
          Durée = paste(runtime, "min"),
          Genres = genres,
          Note = rating
        )
    } else {
      # Affichage normal avec score pour les émotions
      display <- data_to_show %>%
        transmute(
          Titre = sprintf('<a href="#" onclick="Shiny.setInputValue(\'movie_click\', \'%s\', {priority: \'event\'}); return false;">%s</a>',
                          tconst, title),
          Année = year,
          Durée = paste(runtime, "min"),
          Genres = genres,
          Note = rating,
          Score = round(composite_score * 100, 1)
        )
    }
    
    # Ordre par défaut différent selon le mode
    default_order <- if (is_search_mode) {
      list(list(4, "desc"))  # Tri par Note (colonne 5 devient 4 sans Score)
    } else {
      list(list(5, "desc"))  # Tri par Score
    }
    
    dt <- datatable(
      display,
      escape = FALSE,
      selection = 'none',
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50),
        dom = "lftip",
        ordering = TRUE,
        order = default_order,
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
        background = styleColorBar(range(data_to_show$rating, na.rm = TRUE), '#90EE90'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    
    # Appliquer le formatStyle pour 'Score' SEULEMENT si pas en mode recherche
    if (!is_search_mode) {
      dt <- dt %>%
        formatStyle(
          'Score',
          fontWeight = 'bold',
          color = styleInterval(
            cuts = c(70, 80, 90),
            values = c("#666", "#1976D2", "#0D47A1", "#01579B")
          )
        )
    }
    
    dt
  })
  
  #Clic sur un film
  
  observeEvent(input$movie_click, {
    req(input$movie_click)
    
    data_to_use <- if (input$random > 0 && (is.null(input$emotion_selected) || input$emotion_selected == "")) {
      random_movie()
    } else {
      filtered_movies() %>% head(100)
    }
    
    film <- data_to_use %>% filter(tconst == input$movie_click)
    
    if (nrow(film) > 0) {
      selected_movie(film)
      show_details_page(TRUE)
    }
  })
  
  #Fermer les détails
  
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
      
      incProgress(0.6, detail = "Récupération des acteurs...")
      
      # Récupérer les acteurs
      credits <- tryCatch({
        get_movie_credits(tmdb_id, api_key)
      }, error = function(e) NULL)
      
      # Extraire le cast et limiter aux 10 premiers acteurs
      cast <- if (!is.null(credits) && !is.null(credits$cast) && nrow(credits$cast) > 0) {
        head(credits$cast, 10)
      } else {
        NULL
      }
      
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
        
        # Section Acteurs principaux
        if (!is.null(cast) && nrow(cast) > 0) {
          div(style = "margin-top: 30px;",
              h3(style = "color: #667eea; border-bottom: 3px solid #667eea; 
                        padding-bottom: 10px; display: flex; align-items: center;",
                 icon("users"), 
                 span(style = "margin-left: 10px;", "Acteurs principaux")),
              div(style = "display: flex; flex-wrap: wrap; gap: 15px; margin-top: 20px;",
                  lapply(1:nrow(cast), function(i) {
                    actor <- cast[i, ]
                    
                    # URL de la photo de l'acteur
                    actor_photo <- if (!is.null(actor$profile_path) && !is.na(actor$profile_path)) {
                      paste0("https://image.tmdb.org/t/p/w185", actor$profile_path)
                    } else {
                      "https://via.placeholder.com/120x180?text=Pas+de+photo"
                    }
                    
                    # Carte acteur
                    div(
                      style = "width: 120px; text-align: center; background: #f9f9f9; 
                           border-radius: 10px; padding: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                           transition: all 0.3s;",
                      onmouseover = "this.style.transform='translateY(-5px)'; this.style.boxShadow='0 5px 15px rgba(0,0,0,0.2)';",
                      onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 2px 8px rgba(0,0,0,0.1)';",
                      
                      img(src = actor_photo, 
                          style = "width: 100px; height: 150px; object-fit: cover; 
                               border-radius: 8px; margin-bottom: 8px;"),
                      
                      div(style = "font-weight: bold; font-size: 0.85em; color: #333; margin-bottom: 3px;",
                          actor$name),
                      
                      div(style = "font-size: 0.75em; color: #666; font-style: italic;",
                          actor$character)
                    )
                  })
              )
          )
        },
        
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