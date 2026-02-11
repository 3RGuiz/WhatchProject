# =============================================================================
# Whatch? - Prototype d'interface Shiny
# =============================================================================
# Ceci est un exemple minimal d'application Shiny utilisant le dataset
# et la formule de pondération. À adapter selon vos besoins.
# =============================================================================

library(shiny)
library(tidyverse)
library(dplyr)
library(DT)

# Charger le dataset
movies <- readRDS("data/processed/movies_final.rds")

movies <- movies %>%
  dplyr::mutate(
    year = as.integer(year),
    runtime = as.integer(runtime),
    rating = as.numeric(rating),
    votes = as.integer(votes),
    quality_score = as.numeric(quality_score),
    popularity_score = as.numeric(popularity_score))

# Fonction de calcul du score composite
score_composite <- function(quality, popularity, alpha) {
  (1 - alpha) * quality + alpha * popularity}

# Extraire la liste unique des genres
all_genres <- unique(unlist(movies$genres_list))
all_genres <- sort(all_genres[all_genres != "N/A"])

# =============================================================================
# INTERFACE UTILISATEUR
# =============================================================================

ui <- fluidPage(
  
  titlePanel("🎬 Whatch? - Aide au choix de films"),
  
  sidebarLayout(
    
    # Panneau de contrôle
    sidebarPanel(
      width = 3,
      
      h4("Paramètres de recherche"),
      
      # Curseur Découverte ↔ Mainstream
      sliderInput(
        "alpha",
        "Curseur Découverte ↔ Mainstream",
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.05
      ),
      
      helpText(
        "← Découverte : privilégie les pépites méconnues",
        br(),
        "→ Mainstream : privilégie les films populaires"
      ),
      
      hr(),
      
      # Filtres
      h4("Filtres"),
      
      selectInput(
        "genres",
        "Genres (sélection multiple)",
        choices = all_genres,
        multiple = TRUE,
        selected = NULL
      ),
      
      sliderInput(
        "year_range",
        "Période",
        min = min(movies$year),
        max = max(movies$year),
        value = c(1990, max(movies$year)),
        step = 1,
        sep = ""
      ),
      
      sliderInput(
        "duration_max",
        "Durée maximale (minutes)",
        min = 40,
        max = 300,
        value = 180,
        step = 10
      ),
      
      sliderInput(
        "rating_min",
        "Note minimale IMDb",
        min = 0,
        max = 10,
        value = 6,
        step = 0.5
      ),
      
      sliderInput(
        "votes_min",
        "Nombre minimum de votes",
        min = 100,
        max = 10000,
        value = 1000,
        step = 100
      ),
      
      hr(),
      
      # Bouton de mise à jour
      actionButton(
        "update",
        "🔄 Mettre à jour",
        class = "btn-primary btn-block"
      ),
      
      br(),
      
      helpText(
        "💡 Astuce : modifiez les filtres puis cliquez sur 'Mettre à jour' pour recalculer."
      )
    ),
    
    # Panneau principal
    mainPanel(
      width = 9,
      
      tabsetPanel(
        type = "tabs",
        
        # Onglet : Résultats
        tabPanel(
          "📊 Résultats",
          
          br(),
          
          # Statistiques
          fluidRow(
            column(3, wellPanel(
              h4(textOutput("n_results"), style = "margin: 0;"),
              p("films trouvés", style = "margin: 0; color: #666;")
            )),
            column(3, wellPanel(
              h4(textOutput("avg_rating"), style = "margin: 0;"),
              p("note moyenne", style = "margin: 0; color: #666;")
            )),
            column(3, wellPanel(
              h4(textOutput("alpha_display"), style = "margin: 0;"),
              p("position du curseur", style = "margin: 0; color: #666;")
            )),
            column(3, wellPanel(
              h4(textOutput("score_range"), style = "margin: 0;"),
              p("étendue des scores", style = "margin: 0; color: #666;")
            ))
          ),
          
          br(),
          
          # Tableau des résultats
          h4("Top films recommandés"),
          DTOutput("results_table")
        ),
        
        # Onglet : Visualisation
        tabPanel(
          "📈 Visualisation",
          
          br(),
          
          h4("Distribution Qualité vs Popularité"),
          plotOutput("scatter_plot", height = "500px"),
          
          br(),
          
          h4("Distribution des scores composites"),
          plotOutput("score_distribution", height = "300px")
        ),
        
        # Onglet : Documentation
        tabPanel(
          "📚 Documentation",
          
          br(),
          
          h3("Comment fonctionne Whatch? ?"),
          
          p("Whatch? utilise une formule de scoring transparente pour classer les films selon deux dimensions :"),
          
          tags$ul(
            tags$li(strong("Qualité perçue :"), "note moyenne IMDb (0-10)"),
            tags$li(strong("Notoriété :"), "nombre de votes IMDb (transformé logarithmiquement)")
          ),
          
          h4("Formule mathématique"),
          
          tags$code("score(α) = (1 - α) × quality_score + α × popularity_score"),
          
          br(), br(),
          
          p("Le curseur", strong("Découverte ↔ Mainstream"), "permet de contrôler l'arbitrage :"),
          
          tags$ul(
            tags$li(strong("α = 0 :"), "100% qualité (pépites méconnues)"),
            tags$li(strong("α = 0.5 :"), "équilibre 50/50"),
            tags$li(strong("α = 1 :"), "100% popularité (blockbusters)")
          ),
          
          h4("Pourquoi une transformation logarithmique ?"),
          
          p("Le nombre de votes suit une distribution très asymétrique. Sans transformation, 
            les blockbusters (millions de votes) écraseraient complètement les films indépendants."),
          
          p("La transformation log₁₀ compresse l'échelle :"),
          
          tags$ul(
            tags$li("100 votes → log₁₀(100) ≈ 2"),
            tags$li("10 000 votes → log₁₀(10 000) = 4"),
            tags$li("1 000 000 votes → log₁₀(1 000 000) = 6")
          ),
          
          h4("Limitations assumées"),
          
          tags$ul(
            tags$li("Biais culturels d'IMDb (surreprésentation du cinéma anglophone)"),
            tags$li("Popularité ≠ qualité intrinsèque"),
            tags$li("Pas de personnalisation fine sur l'historique utilisateur"),
            tags$li("Catégorisation simplifiée des genres")
          ),
          
          p(em("Ces limites font partie du raisonnement et ne sont pas cachées."))
        )
      )
    )
  )
)

# =============================================================================
# LOGIQUE SERVEUR
# =============================================================================

server <- function(input, output, session) {
  
  # Données filtrées (reactive)
  filtered_data <- eventReactive(input$update, {
    
    data <- movies
    
    # Filtre genres
    if (!is.null(input$genres) && length(input$genres) > 0) {
      data <- data %>%
        filter(map_lgl(genres_list, ~ any(.x %in% input$genres)))
    }
    
    # Filtre période
    data <- data %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])
    
    # Filtre durée
    data <- data %>%
      filter(runtime <= input$duration_max)
    
    # Filtre note minimale
    data <- data %>%
      filter(rating >= input$rating_min)
    
    # Filtre votes minimaux
    data <- data %>%
      filter(votes >= input$votes_min)
    
    # Calculer le score composite
    data <- data %>%
      mutate(
        composite_score = calculate_score(quality_score, popularity_score, input$alpha)
      ) %>%
      arrange(desc(composite_score))
    
    return(data)
    
  }, ignoreNULL = FALSE)
  
  # Statistiques
  output$n_results <- renderText({
    format(nrow(filtered_data()), big.mark = " ")
  })
  
  output$avg_rating <- renderText({
    sprintf("%.2f/10", mean(filtered_data()$rating))
  })
  
  output$alpha_display <- renderText({
    sprintf("%.0f%%", input$alpha * 100)
  })
  
  output$score_range <- renderText({
    sprintf("%.2f - %.2f", min(filtered_data()$composite_score), max(filtered_data()$composite_score))
  })
  
  # Tableau des résultats
  output$results_table <- renderDT({
    
    filtered_data() %>%
      head(50) %>%
      select(
        Titre = title,
        Année = year,
        Durée = runtime,
        Genres = genres,
        Note = rating,
        Votes = votes,
        Score = composite_score
      ) %>%
      mutate(
        Votes = format(Votes, big.mark = " "),
        Score = round(Score, 3)
      ) %>%
      datatable(
        options = list(
          pageLength = 20,
          dom = 'tp',
          ordering = FALSE
        ),
        rownames = FALSE
      )
  })
  
  # Graphique scatter
  output$scatter_plot <- renderPlot({
    
    data <- filtered_data() %>%
      head(500)  # Limiter pour la performance
    
    ggplot(data, aes(x = popularity_score, y = quality_score, 
                     size = composite_score, color = composite_score)) +
      geom_point(alpha = 0.6) +
      scale_color_gradient(low = "steelblue", high = "coral") +
      scale_size_continuous(range = c(1, 8)) +
      labs(
        title = "Qualité vs Popularité",
        subtitle = sprintf("Top 500 films • Curseur : %.0f%%", input$alpha * 100),
        x = "Popularité (log₁₀ de votes, normalisé)",
        y = "Qualité (note IMDb normalisée)",
        size = "Score composite",
        color = "Score composite"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right")
  })
  
  # Distribution des scores
  output$score_distribution <- renderPlot({
    
    ggplot(filtered_data(), aes(x = composite_score)) +
      geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
      geom_vline(xintercept = median(filtered_data()$composite_score), 
                 linetype = "dashed", color = "red", linewidth = 1) +
      labs(
        title = "Distribution des scores composites",
        subtitle = sprintf("%s films • Médiane : %.3f", 
                          format(nrow(filtered_data()), big.mark = " "),
                          median(filtered_data()$composite_score)),
        x = "Score composite",
        y = "Nombre de films"
      ) +
      theme_minimal(base_size = 14)
  })
}

# =============================================================================
# LANCER L'APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)
