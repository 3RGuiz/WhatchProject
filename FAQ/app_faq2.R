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

  .creators {
  width: 100%;
    display: flex;
    justify-content: space-around;
    align-items: center;
    margin-top: 20px;
    padding: 15px 0;
    font-family: 'Oswald', 'Inter', 'Segoe UI', Arial, sans-serif;
    font-weight: 700;
    font-size: 18px;
    color: #5A4FCF;
    letter-spacing: 1px;}
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
            p("Les films recommandés s’affichent par ordre de pertinence ",
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
    h2("🎭 Créateurs")),

p("Cette application a été développée par une équipe d’étudiants composée de :"),

div( class= "creators",
  
  span("3RGuiz"),
  span("Alex6s7"),
  span("Valentin.mass"),
  span("Emilezolv"))
      )
    )
  )


server <- function(input, output, session) {}

shinyApp(ui, server)
