library(shiny)
library(shinythemes)
#j'ai téléchargé le package shinytheme en amont. # j'ai vu sur github que les themes de ce package provenaient de https://bootswatch.com/ 

ui <- navbarPage(
  title = div(
    img(src = "image.jpg", height = "30px", style = "margin-right: 10px; vertical-align: middle;"),
    "What'ch ?"
  ),   #image libre de droit téléchargée dans le même dossier que le projet dans un dossier appelé www car seul moyen pour shiny de la lire
  theme = shinytheme("cyborg"), #j'ai choisi ce thème cyborg qui va bien pour un thème cinéma, plutôt sombre et simple
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
    "))
  ), # lignes générées par une IA car trop complexe, pour pouvoir avoir une image en fond mais discrète qu'on voit quand même nos éléments
  
  tabPanel("Accueil", value = "accueil",
           div(class = "page-accueil",
               div(class = "titre", "🎬 Whatch ?"),
               div(class = "sous-titre", "Abracadabra, on choisit pour toi !"),
               actionButton("demarrer", "Commencer", class = "btn-start")
           )
  )
)