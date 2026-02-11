#!/usr/bin/env Rscript

# =============================================================================
# Whatch? - Pipeline complète de préparation des données
# =============================================================================
# Ce script exécute séquentiellement tous les scripts de préparation
# =============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                        ║\n")
cat("║                  Whatch? - Construction de la BDD                     ║\n")
cat("║                                                                        ║\n")
cat("║          Application R Shiny d'aide au choix de films                 ║\n")
cat("║                                                                        ║\n")
cat("╚════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Vérifier les packages nécessaires
required_packages <- c("tidyverse", "data.table", "scales", "knitr", "R.oo", "R.utils")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("❌ Packages manquants :", paste(missing_packages, collapse = ", "), "\n")
  cat("\nInstallez-les avec :\n")
  cat("install.packages(c('", paste(missing_packages, collapse = "', '"), "'))\n", sep = "")
  quit(status = 1)
}

cat("✅ Tous les packages requis sont installés\n\n")

library(tidyverse)
library(data.table)
library(scales)
library(knitr)
library(R.utils)

# ---- Force le working directory au dossier de run_all.R ----
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])

if (length(script_path) == 1 && file.exists(script_path)) {
  setwd(dirname(normalizePath(script_path)))
} else if (interactive()) {
  # Cas RStudio / Run : on essaye de se caler sur le fichier actif
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) setwd(dirname(p))
  }
}

cat("📁 Working directory :", getwd(), "\n")


# Timer global
start_time <- Sys.time()

# =============================================================================
# ÉTAPE 1 : Téléchargement et nettoyage des données IMDb
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("ÉTAPE 1/3 : Téléchargement et nettoyage des données IMDb\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

step1_start <- Sys.time()

tryCatch({
  if (!file.exists("01_download_imdb_data.R")) {
    stop("01_download_imdb_data.R introuvable dans : ", getwd())
  }
  source("01_download_imdb_data.R")
  step1_duration <- as.numeric(difftime(Sys.time(), step1_start, units = "secs"))
  cat(sprintf("\n✅ Étape 1 terminée en %.1f secondes\n\n", step1_duration))
}, error = function(e) {
  cat("\n❌ ERREUR à l'étape 1 :\n")
  cat(conditionMessage(e), "\n")
  quit(status = 1)
})

# =============================================================================
# ÉTAPE 2 : Construction de la formule de pondération
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("ÉTAPE 2/3 : Construction de la formule de pondération\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

step2_start <- Sys.time()

tryCatch({
  source("02_build_scoring_formula.R")
  step2_duration <- as.numeric(difftime(Sys.time(), step2_start, units = "secs"))
  cat(sprintf("\n✅ Étape 2 terminée en %.1f secondes\n\n", step2_duration))
}, error = function(e) {
  cat("\n❌ ERREUR à l'étape 2 :\n")
  cat(conditionMessage(e), "\n")
  quit(status = 1)
})

# =============================================================================
# ÉTAPE 3 : Validation et documentation
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("ÉTAPE 3/3 : Validation et documentation\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

step3_start <- Sys.time()

tryCatch({
  source("03_validate_formula.R")
  step3_duration <- as.numeric(difftime(Sys.time(), step3_start, units = "secs"))
  cat(sprintf("\n✅ Étape 3 terminée en %.1f secondes\n\n", step3_duration))
}, error = function(e) {
  cat("\n❌ ERREUR à l'étape 3 :\n")
  cat(conditionMessage(e), "\n")
  quit(status = 1)
})

# =============================================================================
# RÉSUMÉ FINAL
# =============================================================================

total_duration <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                        ║\n")
cat("║                      ✨ PIPELINE TERMINÉE ✨                          ║\n")
cat("║                                                                        ║\n")
cat("╚════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat(sprintf("⏱️  Durée totale : %.1f minutes\n\n", total_duration))

cat("📁 Fichiers générés :\n")
cat("  ├─ data/raw/\n")
cat("  │  ├─ title.basics.tsv.gz\n")
cat("  │  └─ title.ratings.tsv.gz\n")
cat("  │\n")
cat("  └─ data/processed/\n")
cat("     ├─ movies_cleaned.rds\n")
cat("     ├─ movies_final.rds ⭐ (dataset pour Shiny)\n")
cat("     ├─ FORMULA_DOCUMENTATION.md\n")
cat("     ├─ formula_documentation.rds\n")
cat("     ├─ genres_stats.csv\n")
cat("     ├─ popularity_transformation.pdf\n")
cat("     └─ scoring_formula_test.pdf\n")
cat("\n")

# Charger le dataset final pour les stats finales
movies_final <- readRDS("data/processed/movies_final.rds")

cat("📊 Dataset final :\n")
cat(sprintf("  • Nombre de films : %s\n", format(nrow(movies_final), big.mark = " ")))
cat(sprintf("  • Période : %d - %d\n", min(movies_final$year), max(movies_final$year)))
cat(sprintf("  • Note moyenne : %.2f/10\n", mean(movies_final$rating)))
cat(sprintf("  • Médiane de votes : %s\n", format(median(movies_final$votes), big.mark = " ")))
cat(sprintf("  • Nombre de genres : %d\n", length(unique(unlist(movies_final$genres_list)))))

cat("\n🎯 Formule de pondération :\n")
cat("  score(α) = (1 - α) × quality_score + α × popularity_score\n")
cat("  \n")
cat("  Où :\n")
cat("  • α ∈ [0, 1] : curseur Découverte ↔ Mainstream\n")
cat("  • quality_score : note IMDb normalisée [0, 1]\n")
cat("  • popularity_score : log₁₀(votes) normalisé [0, 1]\n")

cat("\n🚀 Prochaines étapes :\n")
cat("  1. Développer l'interface Shiny\n")
cat("  2. Implémenter les filtres (genres, durée, année)\n")
cat("  3. Créer les visualisations\n")
cat("  4. Tests utilisateurs\n")

cat("\n📚 Documentation :\n")
cat("  • README.md : instructions complètes\n")
cat("  • data/processed/FORMULA_DOCUMENTATION.md : documentation technique\n")

cat("\n✅ Vous pouvez maintenant utiliser le dataset 'movies_final.rds' dans votre application Shiny!\n\n")
