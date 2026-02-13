# 🎬 What'ch ? - Application de recommandation de films

**Auteurs :** CAUBARRERE Guillaume, PERRAUD Emile, MASSON Valentin, BARBERET Alexis  
**Date :** Février 2026  
**Projet :** Application Shiny de recommandation de films basée sur les émotions

---

##  Table des matières

- [Introduction](#-introduction)
- [Prérequis](#-prérequis-et-installation)
- [Données utilisées](#-données-utilisées)
- [Architecture de l'application](#-architecture-de-lapplication)
- [Les 17 émotions](#-les-17-émotions-disponibles)
- [Algorithme de recommandation](#-algorithme-de-recommandation)
- [Page de détails TMDB](#-page-de-détails-tmdb)
- [Utilisation](#-utilisation-de-lapplication)
- [Difficultés rencontrées](#-difficultés-rencontrées-et-solutions)
- [Améliorations futures](#-améliorations-futures)
- [Références](#-références)

---

##  Introduction

### Problématique

Avec la multiplication des plateformes de streaming et l'abondance de contenus audiovisuels disponibles aujourd'hui, choisir un film est devenu une tâche complexe. Face à des catalogues toujours plus vastes, les utilisateurs passent souvent plus de temps à chercher un programme qu'à réellement le regarder.

### Solution : What'ch ?

**What'ch ?** (jeu de mots entre "Watch" et "What") est une application interactive développée avec **Shiny sous R Studio** qui facilite la découverte de films en fonction de **l'humeur et des émotions** de l'utilisateur.

### Particularité

L'application se distingue par :
17 émotions organisées en 4 catégories, Recommandations basées sur des critères subjectifs (humeur, intensité émotionnelle)
Algorithme d'équilibre qualité/popularité personnalisable
Intégration complète avec l'API TMDB (affiches, synopsis, acteurs, streaming)
Interface cinématographique avec thème Cyborga, Base de ~600 000 films issus d'IMDb

---

## Prérequis et Installation

### Packages R nécessaires

```r
# Packages principaux
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)

# API et web
library(httr)
library(jsonlite)

# Optimisation et données
library(data.table)
library(R.utils)

# Interface utilisateur
library(shinycssloaders)
```

### Installation

```r
# Installer tous les packages nécessaires
install.packages(c(
  "shiny", "shinythemes", "tidyverse", "DT", 
  "httr", "jsonlite", "data.table", "R.utils", 
  "shinycssloaders"
))
```

### Configuration API TMDB

L'application nécessite une clé API TMDB (gratuite) :

1. Créer un compte sur [themoviedb.org](https://www.themoviedb.org/)
2. Obtenir une clé API dans les paramètres
3. Créer un fichier `.Renviron` à la racine du projet :

```
TMDB_API_KEY=votre_clé_api_ici
```

---

## Données utilisées

### Source : Datasets IMDb

L'application utilise les **datasets publics d'IMDb** :

- **title.basics.tsv.gz** (~207 MB) : Informations sur les films
- **title.ratings.tsv.gz** : Notes et votes

### Gestion automatique

La fonction `ensure_imdb_data()` :
1. **Télécharge** automatiquement les fichiers IMDb
2. **Filtre** uniquement les films (`titleType = "movie"`)
3. **Nettoie** les données (années 1900-2026, durée 40-300 min, min 100 votes)
4. **Calcule** les scores normalisés :
   - `quality_score` : Note IMDb normalisée sur [0-1]
   - `popularity_score` : Log des votes normalisé sur [0-1]
5. **Sauvegarde** dans `data/processed/movies_final.rds`

### Résultat

Environ **600 000 films** prêts à être recommandés !

---

## Architecture de l'application

### Structure en 3 pages

1. **Page d'accueil** (Landing page)
2. **Page de sélection** (Émotions + Résultats)
3. **Page de détails** (Modal TMDB)

---

## Page 1 : Accueil (Landing Page)

### Objectif
Créer une **première impression engageante** et cinématographique.

### Éléments visuels

```r
tabPanel("Accueil", value = "accueil",
  div(class = "page-accueil",
    div(class = "titre", "🎬 What'ch ?"),
    div(class = "sous-titre", "Abracadabra, on choisit pour toi !"),
    actionButton("demarrer", "Commencer", class = "btn-start")
  )
)
```

**Design** :
- Titre géant (80px) avec ombre portée
- Slogan accrocheur
- Image de fond cinématographique (back.jpg) avec overlay sombre
- Bouton rouge vif avec effet hover
- Logo animé avec effet "float"

**CSS** :
```css
body {
  background-image: linear-gradient(rgba(0,0,0,0.7), rgba(0,0,0,0.7)), 
                    url('back.jpg');
  background-size: cover;
  background-attachment: fixed;
}
```

---

## Page 2 : Sélection et Recommandations

### Layout : 2 colonnes

#### **Colonne gauche** : Sélection des émotions

**4 catégories repliables** :
 Émotions légères (5 émotions)
 Émotions intenses (4 émotions)
 Émotions profondes (5 émotions)
 Situations spécifiques (3 émotions)

**Boutons d'émotions** :
- Design noir avec bordure violette
- Hover : Élévation + ombre
- Actif : Dégradé violet (#667eea → #764ba2)
- Tooltips au survol avec description

**Options avancées** (optionnel) :
```r
checkboxInput("show_advanced", "⚙️ Options avancées")

# Si activé :
sliderInput("alpha_override", "Découverte ↔ Populaire", 
            min = 0, max = 1, value = 0.5, step = 0.05)
sliderInput("year_range", "Période", 
            min = 1920, max = 2026, value = c(1990, 2026))
sliderInput("duration_max", "Durée max (min)", 
            min = 60, max = 240, value = 180)
sliderInput("min_rating", "Note minimale", 
            min = 5.0, max = 9.0, value = 6.0, step = 0.1)
```

Boutons d'action :
- " Trouver mon film" : Recherche selon critères
- " Au hasard !" : Film aléatoire bien noté (≥7.5, ≥5000 votes)
- " Désélectionner" : Réinitialiser la sélection

#### Colonne droite : Résultats

**Statistiques en temps réel** :
```
┌──────────────┬──────────────┬──────────────┬──────────────┐
│  2,543 films │   7.8/10     │   118 min    │  1995-2024   │
│   trouvés    │ note moyenne │ durée médiane│   période    │
└──────────────┴──────────────┴──────────────┴──────────────┘
```

**Tableau interactif (DT)** :

| Score | Titre | Année | Durée | Genres | Note | Votes |
|-------|-------|-------|-------|--------|------|-------|
| 0.923 | [The Shawshank Redemption](#) | 1994 | 142 min | Drama | 9.3 | 2,896,914 |

**Fonctionnalités** :
- Tri par Score décroissant
- Pagination (25 films/page)
- Barre de recherche
- Titres cliquables → Ouvre la page de détails
- Barre de couleur verte pour la note
- Couleur du score selon valeur (gris → bleu foncé)

---

##  Les 17 émotions disponibles

### Émotions légères / positives (5)

| Émotion | Description | Genres | Alpha | Note min | Durée |
|---------|-------------|--------|-------|----------|-------|
| **Rire** | Comédies légères et humoristiques | Comedy | 0.6 | 6.5 | any |
| **Détente** | Films feel-good sans prise de tête | Comedy, Romance, Family, Animation | 0.7 | 6.8 | any |
| **Romance** | Histoires d'amour et relations sentimentales | Romance | 0.5 | 6.5 | any |
| **Voyager** | Aventures et découvertes exotiques | Adventure | 0.6 | 6.8 | any |
| **En famille** | Films adaptés à tous les âges | Family, Animation | 0.8 | 7.0 | moyen |

Genres exclus : Horror, War, Thriller, Crime

---

### Émotions intenses (4)

| Émotion | Description | Genres | Alpha | Note min | Durée |
|---------|-------------|--------|-------|----------|-------|
| **Action** | Films d'action avec scènes spectaculaires | Action | 0.7 | 6.5 | any |
| **Peur** | Films d'horreur pour avoir peur | Horror | 0.4 | 6.0 | any |
| **Angoisse** | Thrillers psychologiques et suspense | Thriller | 0.3 | 7.0 | any |
| **Épique** | Grandes fresques historiques | War, History | 0.6 | 7.0 | any |

---

###  Émotions profondes (5)

| Émotion | Description | Genres | Alpha | Note min | Durée |
|---------|-------------|--------|-------|----------|-------|
| **Pleurer** | Drames émotionnels touchants | Drama | 0.2 | 7.0 | any |
| **M'instruire** | Documentaires et biographies | Documentary, Biography | 0.2 | 7.0 | any |
| **Mystère** | Enquêtes policières et énigmes | Mystery, Crime | 0.4 | 7.0 | any |
| **Contempler** | Films contemplatifs à rythme lent | Drama, Adventure | 0.1 | 7.0 | any |

---

###  Situations spécifiques (3)

| Émotion | Description | Genres | Alpha | Note min | Particularité |
|---------|-------------|--------|-------|----------|---------------|
| **Nostalgie** | Classiques des années 70-90 | Tous | 0.5 | 7.0 | Période 1970-1999 |
| **Surprise** | Films atypiques peu connus | Mystery, Sci-Fi | 0.1 | 7.0 | Boost films méconnus |
| **Fantastique** | Mondes imaginaires et magie | Fantasy | 0.6 | 7.0 | - |
| **Western** | Far West et duels de cowboys | Western | 0.4 | 7.0 | - |
| **S-F** | Science-fiction conceptuelle | Sci-Fi | 0.3 | 7.0 | Durée longue |

---

##  Algorithme de recommandation

### Principe : Le paramètre Alpha

L'algorithme repose sur un équilibre qualité/popularité :

```r
composite_score = (1 - alpha) × quality_score + alpha × popularity_score
```

- `alpha` ∈ [0, 1] : paramètre d'équilibre
- `quality_score` ∈ [0, 1] : note IMDb normalisée
- `popularity_score` ∈ [0, 1] : popularité (log des votes normalisé)

### Interprétation d'Alpha

| Alpha | Signification | Exemples d'émotions |
|-------|---------------|---------------------|
| **0.1** | 90% qualité, 10% popularité | Contempler, Surprise |
| **0.2-0.3** | Priorité qualité | Pleurer, Angoisse, S-F |
| **0.5** | Équilibre 50/50 | Romance, Nostalgie |
| **0.6-0.7** | Priorité popularité | Rire, Action, Détente |
| **0.8** | 80% popularité | Famille |

### Calcul des scores de base

#### 1. Quality Score

```r
quality_score = rating / 10
```

Normalisation simple de la note IMDb (sur 10).

#### 2. Popularity Score

```r
log_votes = log1p(votes)
popularity_score = (log_votes - min) / (max - min)
```


Sans log, un film avec 1M de votes écraserait un film avec 10k votes.

Avec log :
- 100 votes → log ≈ 2.0
- 1,000 votes → log ≈ 3.0
- 10,000 votes → log ≈ 4.0
- 100,000 votes → log ≈ 5.0

L'échelle devient proportionnelle au lieu de linéaire.

### Filtres appliqués

#### Par genres

```r
# Inclusion (au moins un genre doit correspondre)
filter(map_lgl(genres_list, ~ any(.x %in% config$genres)))

# Exclusion (aucun genre exclu ne doit être présent)
filter(map_lgl(genres_list, ~ !any(.x %in% config$exclude_genres)))
```

Exemple "Pleurer" :
- Genres recherchés : Drama
- Genres exclus : Horror, Action
- Résultat : Drames purs uniquement

#### Par note minimale

```r
data %>% filter(rating >= max(config$rating_min, custom_min_rating))
```

Chaque émotion impose une note minimale (6.0 à 7.0).

#### Par durée

```r
if (duration_preference == "court") {
  filter(runtime <= 100)
} else if (duration_preference == "moyen") {
  filter(runtime >= 90, runtime <= 130)
} else if (duration_preference == "long") {
  filter(runtime >= 120)
}
```

#### Par période (Nostalgie uniquement)

```r
filter(year >= 1970, year <= 1999)
```

### Boost spécial : "Surprise"

Pour favoriser les films méconnus :

```r
if (boost_low_votes == TRUE) {
  composite_score = composite_score + (1 - popularity_score) × 0.2
}
```

**Effet** :
- Film populaire (popularity = 0.9) → boost +2%
- Film méconnu (popularity = 0.2) → boost +16%

---

##  Page de détails TMDB

### Déclenchement

La page de détails s'ouvre lorsque l'utilisateur **clique sur un titre** dans le tableau.

### Design : Modal Overlay

```css
.movie-details-overlay {
  position: fixed;
  background: rgba(0,0,0,0.85);
  z-index: 9998;
  animation: fadeIn 0.3s;
}
```

- Fond noir transparent (85%)
- Container blanc centré
- Animation d'apparition fluide
- Scrollable si contenu long

### Informations affichées

#### 1. **Affiche du film**
```r
poster_url <- paste0("https://image.tmdb.org/t/p/w500", details$poster_path)
```
- Haute résolution (500px de large)
- Coins arrondis, ombre portée
- Placeholder si indisponible

#### 2. **Métadonnées**
- Titre en français
- Note TMDB + Note IMDb
- Année de sortie
- Durée en minutes
- Genres (badges violets)

#### 3. **Synopsis complet**
```r
details$overview  # En français (language=fr-FR)
```

#### 4. **Acteurs principaux** (Top 10)
```r
credits <- get_movie_credits(tmdb_id, api_key)
cast <- head(credits$cast, 10)
```

**Affichage** :
- Photos des acteurs (185px)
- Nom de l'acteur
- Nom du personnage
- Cartes avec effet hover (élévation)

#### 5. **Disponibilité streaming**
```r
providers <- get_providers(tmdb_id, api_key, country = "FR")
```

Plateformes en France : Netflix, Prime Video, Disney+, etc.

#### 6. **Budget et Revenus**
```r
if (details$budget > 0) {
  # Affichage avec formatage
  format(details$budget, big.mark = " ")
}
```

#### 7. **Tagline**
```r
details$tagline  # Phrase d'accroche du film
```

### Bouton de fermeture

```r
actionButton("close_details", "← Retour aux résultats", 
             class = "btn btn-secondary")
```

---

## Utilisation de l'application

### Lancement

```r
# Méthode 1 : Depuis RStudio
source("app.R")

# Méthode 2 : Avec runApp
shiny::runApp("app.R")
```


#### Scénario 1 : Recherche par émotion

1. Page d'accueil → Cliquer sur "Commencer"
2. Sélectionner une émotion (ex: "Rire")
3. Le bouton devient violet (actif)
4. *(Optionnel)* Ajuster les options avancées
5. Cliquer sur " Trouver mon film"
6. Explorer les résultats dans le tableau
7. Cliquer sur un titre pour voir les détails
8. Modal avec affiche, synopsis, acteurs, streaming
9. "← Retour" pour revenir aux résultats

#### Scénario 2 : Film au hasard

1. Cliquer directement sur "Au hasard !"
2. Un film bien noté (≥7.5, ≥5000 votes) s'affiche
3. Fond jaune pour le distinguer
4. Cliquer sur le titre pour les détails

### Exemples concrets

#### "Je veux pleurer devant un drame"

Configuration :
- Émotion : Pleurer (alpha=0.2, note min 7.0)
- Genres : Drama uniquement
- Exclusions : Horror, Action

Résultat :
- Priorité qualité (80%)
- Drames purs, émouvants
- Ex: "Schindler's List", "The Shawshank Redemption"

#### Film d'action récent et populaire

Configuration :
- Émotion : Action (alpha=0.7)
- Options : Période 2015-2026

Résultat :
- Priorité popularité (70%)
- Blockbusters récents
- Ex: "John Wick 4", "Top Gun: Maverick"

#### "Pépite méconnue de science-fiction"

Configuration :
- Émotion : Surprise (alpha=0.1, boost +20%)
- Genres : Mystery, Sci-Fi

Résultat :
- Films peu connus mais excellents
- Ex: "Coherence", "Primer", "Moon"

---

```r
genres_list <- str_split(genres, ",")
filter(map_lgl(genres_list, ~ any(.x %in% config$genres)))
```

---

##  Répartition des tâches

| Membre | Responsabilités |
|--------|-----------------|
| Guillaume CAUBARRERE | Architecture globale, algorithme de scoring, gestion IMDb |
| Emile PERRAUD | Interface utilisateur (UI), design CSS, expérience utilisateur, readme |
| Valentin MASSON | Intégration API TMDB, configuration des émotions, tests |
| Alexis BARBERET | Documentation, README, système de filtrage, gestion genres |

Travail collectif : Réflexion sur les 17 émotions, calibrage alpha, choix des exclusions de genres, tests utilisateurs

---

##  Références

### Données

- IMDb Datasets : https://datasets.imdbws.com/
  - Documentation : https://www.imdb.com/interfaces/
  

- TMDB API : https://www.themoviedb.org/documentation/api
  - Clé gratuite : https://www.themoviedb.org/settings/api

### Technologies

- **R** (4.3+) : https://www.r-project.org/
- **Shiny** : https://shiny.rstudio.com/
- **tidyverse** : https://www.tidyverse.org/
- **DT (DataTables)** : https://rstudio.github.io/DT/
- **shinythemes** : https://rstudio.github.io/shinythemes/



## Équipe de développement:
- Guillaume CAUBARRERE
- Emile PERRAUD
- Valentin MASSON
- Alexis BARBERET

---

## Conclusion
Whatch propose une approche innovante de la recommandation de films en privilégiant l'approche émotionnelle plutôt que les genres traditionnels. 

Avec 17 émotions soigneusement calibrées, un algorithme simple mais efficace basé sur le paramètre alpha, et une intégration complète avec TMDB, l'application offre une expérience de découverte cinématographique personnalisée et surprenante.

**Base de données: ~600 000 films  
**Période couverte : 1900-2026  


---

"Abracadabra, on choisit pour toi !"* 

