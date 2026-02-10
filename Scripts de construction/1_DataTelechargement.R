# =============================================================================
# Whatch? - Téléchargement et préparation des données IMDb (VERSION OPTIMISÉE)
# =============================================================================
# Cette version évite les crashs mémoire en filtrant AVANT de charger
# =============================================================================

library(tidyverse)
library(data.table)
library(R.utils)

# Augmenter la mémoire disponible
options(datatable.verbose = FALSE)

# Créer les répertoires nécessaires
dir.create("data", showWarnings = FALSE)
dir.create("data/raw", showWarnings = FALSE)
dir.create("data/processed", showWarnings = FALSE)

# URLs des fichiers IMDb (non-commercial datasets)
url_basics <- "https://datasets.imdbws.com/title.basics.tsv.gz"
url_ratings <- "https://datasets.imdbws.com/title.ratings.tsv.gz"

# Chemins locaux
path_basics <- "data/raw/title.basics.tsv.gz"
path_ratings <- "data/raw/title.ratings.tsv.gz"
path_movies_temp <- "data/raw/movies_only.tsv"

# =============================================================================
# 1. TÉLÉCHARGEMENT DES DONNÉES
# =============================================================================

cat("📥 Téléchargement des données IMDb...\n")

if (!file.exists(path_basics)) {
  cat("  - Téléchargement de title.basics.tsv.gz (207 MB)...\n")
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

# =============================================================================
# 2. FILTRAGE AVANT CHARGEMENT (ÉVITE LE CRASH)
# =============================================================================

cat("🔧 Extraction des films uniquement (optimisation mémoire)...\n")

# Vérifier si le fichier temporaire existe déjà
if (!file.exists(path_movies_temp)) {
  
  cat("  - Décompression et filtrage en cours (2-3 minutes)...\n")
  
  # Méthode compatible Windows/Mac/Linux
  # On lit ligne par ligne et on garde seulement les films
  
  con_in <- gzfile(path_basics, "r")
  con_out <- file(path_movies_temp, "w")
  
  # Lire et écrire la ligne d'en-tête
  header <- readLines(con_in, n = 1)
  writeLines(header, con_out)
  
  # Traiter le fichier par blocs
  chunk_size <- 10000
  n_movies <- 0
  n_total <- 0
  
  repeat {
    lines <- readLines(con_in, n = chunk_size)
    if (length(lines) == 0) break
    
    # Filtrer les lignes contenant "\tmovie\t"
    movie_lines <- grep("\tmovie\t", lines, value = TRUE)
    
    if (length(movie_lines) > 0) {
      writeLines(movie_lines, con_out)
      n_movies <- n_movies + length(movie_lines)
    }
    
    n_total <- n_total + length(lines)
    
    # Afficher la progression tous les 100k lignes
    if (n_total %% 100000 == 0) {
      cat(sprintf("    Traité : %s lignes, Films trouvés : %s\r", 
                  format(n_total, big.mark = " "), 
                  format(n_movies, big.mark = " ")))
    }
  }
  
  close(con_in)
  close(con_out)
  
  cat(sprintf("\n  ✅ Films extraits : %s sur %s lignes totales\n", 
              format(n_movies, big.mark = " "), 
              format(n_total, big.mark = " ")))
  
} else {
  cat("  - Fichier temporaire déjà existant, réutilisation\n")
}

# =============================================================================
# 3. CHARGEMENT DES DONNÉES FILTRÉES
# =============================================================================

cat("\n📊 Chargement des données filtrées...\n")

# Maintenant on charge seulement les films (beaucoup plus petit)
basics <- fread(path_movies_temp, na.strings = "\\N", quote = "")

cat(sprintf("  - Films chargés : %s lignes\n", format(nrow(basics), big.mark = " ")))

# Ratings (fichier plus petit, pas de problème)
ratings <- fread(path_ratings, na.strings = "\\N")

cat(sprintf("  - Ratings chargés : %s lignes\n", format(nrow(ratings), big.mark = " ")))

# =============================================================================
# 4. NETTOYAGE DES DONNÉES
# =============================================================================

cat("\n🔧 Nettoyage des données...\n")

# On a déjà filtré titleType == "movie", donc on garde tout
movies_basics <- basics %>%
  select(tconst, primaryTitle, startYear, runtimeMinutes, genres)

rm(basics)  # Libérer la mémoire
gc()

# Nettoyer les types de données
movies_basics <- movies_basics %>%
  mutate(
    startYear = as.integer(startYear),
    runtimeMinutes = as.integer(runtimeMinutes)
  ) %>%
  # Filtrer les valeurs aberrantes
  filter(
    !is.na(startYear),
    !is.na(runtimeMinutes),
    startYear >= 1900,
    startYear <= year(Sys.Date()),
    runtimeMinutes >= 40,
    runtimeMinutes <= 300,
    genres != "\\N"
  )

cat(sprintf("  - Films après nettoyage : %s\n", format(nrow(movies_basics), big.mark = " ")))

# =============================================================================
# 5. FUSION AVEC LES RATINGS
# =============================================================================

cat("\n🔗 Fusion avec les ratings...\n")

movies_full <- movies_basics %>%
  inner_join(ratings, by = "tconst") %>%
  filter(numVotes >= 100)

rm(movies_basics, ratings)  # Libérer la mémoire
gc()

cat(sprintf("  - Films avec ratings (≥100 votes) : %s\n", format(nrow(movies_full), big.mark = " ")))

# =============================================================================
# 6. TRANSFORMATION DES GENRES
# =============================================================================

cat("\n🎭 Traitement des genres...\n")

movies_full <- movies_full %>%
  mutate(
    genres_list = str_split(genres, ",")
  )

# Statistiques sur les genres
all_genres <- movies_full %>%
  unnest(genres_list) %>%
  count(genres_list, sort = TRUE)

cat("  - Genres les plus fréquents :\n")
print(head(all_genres, 10))

# =============================================================================
# 7. SAUVEGARDE
# =============================================================================

cat("\n💾 Sauvegarde des données...\n")

saveRDS(movies_full, "data/processed/movies_cleaned.rds")
write_csv(all_genres, "data/processed/genres_stats.csv")

# Nettoyer le fichier temporaire si souhaité (décommenter pour supprimer)
# file.remove(path_movies_temp)

cat("✅ Données sauvegardées\n")

# =============================================================================
# 8. STATISTIQUES DESCRIPTIVES
# =============================================================================

cat("\n📈 Statistiques descriptives :\n")
cat(sprintf("  - Nombre total de films : %s\n", format(nrow(movies_full), big.mark = " ")))
cat(sprintf("  - Période : %d - %d\n", min(movies_full$startYear), max(movies_full$startYear)))
cat(sprintf("  - Durée moyenne : %.1f minutes\n", mean(movies_full$runtimeMinutes)))
cat(sprintf("  - Note moyenne : %.2f/10\n", mean(movies_full$averageRating)))
cat(sprintf("  - Nombre médian de votes : %s\n", format(median(movies_full$numVotes), big.mark = " ")))
cat(sprintf("  - Nombre de genres uniques : %d\n", nrow(all_genres)))

cat("\n✨ Script terminé avec succès!\n")
cat("\n💡 Astuce : vous pouvez supprimer data/raw/movies_only.tsv pour libérer de l'espace\n")
