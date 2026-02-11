library(shiny)

ui <- navbarPage(
  
  title = "What'ch",
  
#Intégration du style CSS 

  tags$head(
    tags$style(HTML("
      
      body {
        font-family: 'Inter', 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
        background-color: #FFFFFF;
        color: #2C2C54;
        line-height: 1.7;}

      h1 {
        color: #5A4FCF;
        font-weight: 800;
        letter-spacing: -0.5px;}

      h2 {
        color: #5A4FCF;
        font-weight: 700;
        margin-top: 40px;}

      h3, h4 {
        color: #4B47B8;
        font-weight: 600;}

      p {
        font-size: 16px;
        max-width: 900px;}

      .highlight {
        color: #5A4FCF;
        font-weight: 600;}

      .scenario-block {
        background: linear-gradient(135deg, #F1F0FF, #E6E4FF);
        border-radius: 18px;
        padding: 28px;
        height: 100%;
        box-shadow: 0 10px 22px rgba(90, 79, 207, 0.18);
        transition: transform 0.25s ease;
      }

      .scenario-block:hover {
        transform: translateY(-6px);
      }

      .scenario-block h4 {
        font-weight: 700;
        margin-bottom: 14px;
      }


      .drawer-btn {
        background-color: transparent;
        border: none;
        color: #5A4FCF;
        font-weight: 600;
        font-size: 15px;
        padding: 0;
        cursor: pointer;
      }

      .drawer-btn:hover {
        text-decoration: underline;
      }

.team-member {
  text-align: center;
}

.team-member img {
  width: 140px;
  height: 140px;
  border-radius: 50%;
  object-fit: cover;
  margin-bottom: 15px;
  border: 4px solid #E6E4FF;
  transition: all 0.3s ease;
}

.team-member:hover img {
  border-color: #5A4FCF;
  transform: scale(1.08);
}

.team-member strong {
  display: block;
  font-size: 1.1rem;
  color: #2C2C54;
  margin-bottom: 5px;
}

.team-member p {
  color: #5A4FCF;
  font-size: 0.95rem;
  margin: 0;
}
    "))
  ),
  
#Onglet à propos : 

  tabPanel(
    "À propos",
    
    fluidPage(
      
# Titre
      h1("🎬 À propos de What'ch"),
      p("Une application pensée pour vous aider à choisir ",
        span("le film idéal", class = "highlight"),
        ", sans hésitation selon votre humeur et vos envies."),
      
## Partie : Le décor

      h2("🎞️ Le décor"),
      p("Ne vous est-il jamais arrivé de passer de longues minutes à chercher un film ? ",
        "Aujourd’hui, le choix ne manque pas. ",
        span("Films, séries, plateformes de streaming", class = "highlight"),
        " : l’offre est immense, ce qui rend la décision plus difficile."),
      p(
        "Le véritable problème n’est plus de trouver un film, mais de savoir ",
        span("lequel regarder", class = "highlight"),
        "."),
      p(
        "C’est la raison pour laquelle nous avons créé ",
        strong("What'ch"),
        " : une application pensée pour faciliter le choix de votre prochain film ",
        "et vous permettre de passer moins de temps à chercher, et plus de temps à regarder."),
      
#Partie : Le scénario 

      h2("🎥 Le scénario"),
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
              "Grâce à des correspondances entre les préférences de l’utilisateur ",
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
            p(
              "Les films recommandés s’affichent par ordre de pertinence ",
              span("(score What'ch). ", class = "highlight"),
              "Cliquez sur une suggestion pour accéder à sa fiche descriptive."
            )
          )
        )
      ),
      
      br(),
      
#Bouton pour la Partie 'Comment ça marche'
      actionButton(
        "toggle_algo",
        "🔍 Comment fonctionne la recommandation ?",
        class = "drawer-btn"),
      
      conditionalPanel(
        condition = "input.toggle_algo % 2 == 1",  #clic impair : ouverture/ fermeture
        br(),
        p("Le système de recommandation repose sur une analyse des préférences sélectionnées ",
          "par l’utilisateur et sur des correspondances avec les caractéristiques des films."),
        
        p("Plusieurs critères sont pris en compte (genres, durée, note, année) afin de ",
          "calculer un score de pertinence, garantissant des recommandations cohérentes et fiables.")),
      
##Partie : La vision

      h2("🎯 La vision"),
      p("Notre objectif était de concevoir une application ",
        span("utile au quotidien", class = "highlight"),
        ", pensée avant tout pour l’utilisateur."),

      p("Pas de fonctionnalités inutiles, pas de complexité : l’idée est d’aller à l’essentiel."),
      p(
        "L’interface a été conçue pour être agréable à parcourir, permettant à l’utilisateur ",
        "de comprendre rapidement le fonctionnement de l’application et de trouver un film ",
        "sans effort, en quelques instants seulement."),
      
##Partie : Le casting

      div(class = "section-header",
        h2("🎭 Le casting")),
      
      p(style = "font-size: 1.1rem; margin-bottom: 30px;",
        "Derrière cette application se trouve une équipe de développeurs réunis ",
        "autour d'un même objectif : proposer une expérience simple et efficace."),
      
      fluidRow(
        column(3,
          div(
            class = "team-member",
            img(src = "image.jpg", onerror = "this.src='https://via.placeholder.com/140/5A4FCF/FFFFFF?text=M1'"),
            strong("Gui"),
            p("Développement & interface")
          )
        ),
        column(3,
          div(
            class = "team-member",
            img(src = "image.jpg", onerror = "this.src='https://via.placeholder.com/140/5A4FCF/FFFFFF?text=M2'"),
            strong("Val"),
            p("Traitement des données")
          )
        ),
        column(3,
          div(
            class = "team-member",
            img(src = "image.jpg", onerror = "this.src='https://via.placeholder.com/140/5A4FCF/FFFFFF?text=M3'"),
            strong("Alex"),
            p("Expérience utilisateur")
          )
        ),
        column(3,
          div(
            class = "team-member",
            img(src = "image.jpg", onerror = "this.src='https://via.placeholder.com/140/5A4FCF/FFFFFF?text=M4'"),
            strong("Emi"),
            p("Coordination & tests")
          )
        )
      )
      )
    )
  )


server <- function(input, output, session) {}

shinyApp(ui, server)
