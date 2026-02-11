# =============================================================================
# Whatch? - Formule de pondération multi-critère
# =============================================================================
# Ce script développe et teste la formule de scoring qui combine :
# - Qualité perçue (note IMDb)
# - Notoriété/popularité (nombre de votes, transformé)
# - Le curseur Découverte ↔ Mainstream
# =============================================================================

library(tidyverse)
library(scales)

# --- Fix affichage NA (évite "invalid 'na.print' specification") ---
options(na.print = "NA")
options(tibble.na.print = "NA")

# Charger les données nettoyées
movies <- readRDS("data/processed/movies_cleaned.rds")

# =============================================================================
# PARTIE 1 : NORMALISATION DE LA POPULARITÉ
# =============================================================================

# Le nombre de votes suit une distribution log-normale très asymétrique.
# Problème : quelques blockbusters ont des millions de votes, la majorité < 1000
# Solution : transformation logarithmique + normalisation [0, 1]

cat("📊 Distribution du nombre de votes (avant transformation) :\n")
summary(movies$numVotes)

# Transformation logarithmique (log10 pour lisibilité)
movies <- movies %>%
  mutate(
    log_votes = log10(numVotes + 1),  # +1 pour éviter log(0)
    # Normalisation min-max sur la distribution log
    popularity_score = (log_votes - min(log_votes)) / (max(log_votes) - min(log_votes))
  )

cat("\n📊 Distribution de popularity_score (après transformation) :\n")
summary(movies$popularity_score)

# Visualisation de la transformation
pdf("data/processed/popularity_transformation.pdf", width = 12, height = 5)
par(mfrow = c(1, 3))

hist(movies$numVotes, breaks = 100, main = "Distribution brute des votes",
     xlab = "Nombre de votes", col = "steelblue")

hist(movies$log_votes, breaks = 100, main = "Distribution log10(votes)",
     xlab = "log10(nombre de votes)", col = "coral")

hist(movies$popularity_score, breaks = 100, main = "Score de popularité normalisé",
     xlab = "Popularity score [0, 1]", col = "forestgreen")

dev.off()
cat("📈 Graphiques sauvegardés dans data/processed/popularity_transformation.pdf\n")

# =============================================================================
# PARTIE 2 : NORMALISATION DE LA QUALITÉ
# =============================================================================

# La note IMDb est déjà sur une échelle [0, 10]
# On normalise sur [0, 1] pour cohérence avec popularity_score

movies <- movies %>%
  mutate(
    quality_score = averageRating / 10
  )

cat("\n📊 Distribution de quality_score :\n")
summary(movies$quality_score)

# =============================================================================
# PARTIE 3 : FORMULE DE SCORING COMPOSITE
# =============================================================================

# Le curseur "Découverte ↔ Mainstream" est un paramètre alpha ∈ [0, 1]
# - alpha = 0 : 100% qualité, 0% popularité (privilégie les pépites méconnues)
# - alpha = 0.5 : équilibre 50/50
# - alpha = 1 : 100% popularité, 0% qualité (privilégie les blockbusters)

# Formule de base :
# score(alpha) = (1 - alpha) × quality_score + alpha × popularity_score

# Variante : on peut ajouter un terme d'interaction pour valoriser
# les films qui sont à la fois bien notés ET populaires
# score_enhanced(alpha) = score(alpha) + beta × quality_score × popularity_score

# Fonction de calcul du score
calculate_score <- function(quality, popularity, alpha, beta = 0) {
  base_score <- (1 - alpha) * quality + alpha * popularity
  interaction_term <- beta * quality * popularity
  return(base_score + interaction_term)
}

# =============================================================================
# PARTIE 4 : TESTS ET VALIDATION
# =============================================================================

cat("\n🧪 Tests de la formule de scoring...\n\n")

# Test sur différents profils de films
test_cases <- tribble(
  ~film, ~quality, ~popularity, ~description,
  "Pépite indé", 0.95, 0.1, "Excellent film méconnu",
  "Blockbuster moyen", 0.6, 0.95, "Film populaire mais critiqué",
  "Chef-d'œuvre populaire", 0.95, 0.95, "Le Parrain, Shawshank...",
  "Film obscur moyen", 0.5, 0.05, "Film quelconque et inconnu"
)

# Calculer les scores pour différentes valeurs de alpha
alphas <- seq(0, 1, by = 0.25)

results <- expand_grid(test_cases, alpha = alphas) %>%
  mutate(
    score_base = calculate_score(quality, popularity, alpha, beta = 0),
    score_enhanced = calculate_score(quality, popularity, alpha, beta = 0.2)
  )

cat("Scores selon le curseur Découverte ↔ Mainstream (formule de base) :\n")
results %>%
  select(film, alpha, score_base) %>%
  pivot_wider(names_from = alpha, values_from = score_base, names_prefix = "alpha_") %>%
  print()

# Visualisation
pdf("data/processed/scoring_formula_test.pdf", width = 10, height = 6)
ggplot(results, aes(x = alpha, y = score_base, color = film, group = film)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = seq(0, 1, 0.25),
                     labels = c("100% Découverte", "75%", "50%", "25%", "100% Mainstream")) +
  labs(
    title = "Évolution du score composite selon le curseur Découverte ↔ Mainstream",
    subtitle = "score = (1 - α) × qualité + α × popularité",
    x = "Position du curseur (α)",
    y = "Score composite",
    color = "Profil de film"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
dev.off()
cat("\n📈 Graphique sauvegardé dans data/processed/scoring_formula_test.pdf\n")

# =============================================================================
# PARTIE 5 : APPLICATION SUR LE DATASET RÉEL
# =============================================================================

cat("\n🎬 Application de la formule sur le dataset complet...\n")

# Créer une fonction wrapper pour faciliter l'usage dans Shiny
add_composite_score <- function(data, alpha = 0.5, beta = 0) {
  data %>%
    mutate(
      composite_score = calculate_score(quality_score, popularity_score, alpha, beta)
    )
}

# Exemple : top 20 films avec alpha = 0.5 (équilibre)
top_balanced <- movies %>%
  add_composite_score(alpha = 0.5) %>%
  arrange(desc(composite_score)) %>%
  dplyr::select(primaryTitle, startYear, averageRating, numVotes,
                quality_score, popularity_score, composite_score) %>%
  head(20)

cat("\nTop 20 films (curseur à 50% - équilibre qualité/popularité) :\n")
print(as.data.frame(top_balanced), row.names = FALSE, na.print = "NA")

# Exemple : top 20 en mode "Découverte" (alpha = 0.1)
top_discovery <- movies %>%
  add_composite_score(alpha = 0.1) %>%
  arrange(desc(composite_score)) %>%
  select(primaryTitle, startYear, averageRating, numVotes,
         quality_score, popularity_score, composite_score) %>%
  head(20)

cat("\nTop 20 films (curseur à 10% - mode Découverte) :\n")
print(as.data.frame(top_discovery), row.names = FALSE, na.print = "NA")

# Exemple : top 20 en mode "Mainstream" (alpha = 0.9)
top_mainstream <- movies %>%
  add_composite_score(alpha = 0.9) %>%
  arrange(desc(composite_score)) %>%
  select(primaryTitle, startYear, averageRating, numVotes,
         quality_score, popularity_score, composite_score) %>%
  head(20)

cat("\nTop 20 films (curseur à 90% - mode Mainstream) :\n")
print(as.data.frame(top_mainstream), row.names = FALSE, na.print = "NA")

# =============================================================================
# PARTIE 6 : AJOUT DES SCORES AU DATASET ET SAUVEGARDE
# =============================================================================

cat("\n💾 Préparation du dataset final avec scores normalisés...\n")

# Ajouter les scores normalisés au dataset
movies_final <- movies %>%
  dplyr::select(
    tconst,
    title = primaryTitle,
    year = startYear,
    runtime = runtimeMinutes,
    genres,
    genres_list,
    rating = averageRating,
    votes = numVotes,
    quality_score,
    popularity_score
  ) %>%
  mutate(
    dataset_version = "1.0",
    dataset_date = Sys.Date()
  )


# Sauvegarder le dataset final
saveRDS(movies_final, "data/processed/movies_final.rds")

cat(sprintf("✅ Dataset final sauvegardé : %s films\n", format(nrow(movies_final), big.mark = " ")))

# =============================================================================
# PARTIE 7 : DOCUMENTATION DE LA FORMULE
# =============================================================================

documentation <- list(
  formula = "score(α) = (1 - α) × quality_score + α × popularity_score",
  parameters = list(
    alpha = "Curseur Découverte ↔ Mainstream, ∈ [0, 1]",
    quality_score = "Note IMDb normalisée sur [0, 1]",
    popularity_score = "log10(numVotes) normalisé sur [0, 1]"
  ),
  interpretation = list(
    alpha_0 = "100% qualité - privilégie les films excellents même méconnus",
    alpha_0.5 = "Équilibre - compromis entre qualité et notoriété",
    alpha_1 = "100% popularité - privilégie les films très connus"
  ),
  justification = c(
    "Transparence : chaque composante est explicable et contrôlable",
    "Absence de biais algorithmique : pas de recommandation prédictive",
    "Contextualisation : l'utilisateur choisit l'arbitrage qualité/popularité",
    "Reproductibilité : formule déterministe, résultats stables"
  ),
  limitations = c(
    "Biais culturel d'IMDb (surreprésentation du cinéma anglophone)",
    "Nombre de votes comme proxy imparfait de notoriété réelle",
    "Pas de prise en compte des préférences individuelles fines",
    "Catégorisation simplifiée des genres"
  )
)

saveRDS(documentation, "data/processed/formula_documentation.rds")

cat("\n📚 Documentation de la formule sauvegardée\n")
cat("\n✨ Script terminé avec succès!\n")
cat("\n📁 Fichiers créés :\n")
cat("  - data/processed/movies_final.rds (dataset pour Shiny)\n")
cat("  - data/processed/formula_documentation.rds (documentation)\n")
cat("  - data/processed/popularity_transformation.pdf (graphiques)\n")
cat("  - data/processed/scoring_formula_test.pdf (validation)\n")
