## Lire rapidement les gros fichiers
library("data.table")

## Manipuler les tableaux
library("dplyr")

## Lecture des données 
loan <- fread("loan.csv")

## ---- 3. Définir bon / mauvais client ----
loan_clean <- loan %>%
  filter(loan_status %in% c("Fully Paid", "Charged Off", "Default")) %>%
  mutate(defaut = ifelse(loan_status == "Fully Paid", 0, 1))

# ---- 4. Vérifications ----
dim(loan_clean)
table(loan_clean$defaut)
prop.table(table(loan_clean$defaut))

# ---- 5. Sélection des variables ----
variables <- c("defaut", "loan_amnt", "term", "emp_length", "home_ownership",
               "annual_inc", "verification_status", "purpose", "dti",
               "delinq_2yrs", "inq_last_6mths", "open_acc", "pub_rec",
               "revol_bal", "revol_util", "total_acc")

base <- loan_clean %>%
  select(all_of(variables))

# ---- 6. Valeurs manquantes ----
dim(base)
colSums(is.na(base))
summary(base)

# ---- 7. Valeurs manquantes : affichage clair ----
na_par_colonne <- colSums(is.na(base))
na_par_colonne[na_par_colonne > 0]

# ---- 8. Colonnes de texte ----
table(base$emp_length)
table(base$home_ownership)
table(base$term)

# ---- 9. Nettoyage ----
base_propre <- base %>%
  filter(!is.na(revol_util)) %>%
  mutate(
    term = trimws(term),
    emp_length = ifelse(emp_length == "n/a", "Inconnu", emp_length),
    home_ownership = ifelse(home_ownership %in% c("ANY", "NONE", "OTHER"),
                            "OTHER", home_ownership)
  ) %>%
  mutate(across(c(term, emp_length, home_ownership,
                  verification_status, purpose), as.factor))

dim(base_propre)
colSums(is.na(base_propre))
table(base_propre$home_ownership)
table(base_propre$emp_length)
str(base_propre)

# ---- 10. Valeurs aberrantes ----
boxplot(base_propre$revol_bal, main = "Solde de crédit (revol_bal)")

quantile(base_propre$revol_bal, probs = c(0.5, 0.9, 0.99, 0.999, 1))

# ---- 11. Plafonner les valeurs extrêmes ----
plafonner <- function(x) {
  seuil <- quantile(x, probs = 0.99, na.rm = TRUE)
  pmin(x, seuil)
}

base_propre <- base_propre %>%
  mutate(across(c(annual_inc, revol_bal, revol_util, dti), plafonner))

summary(base_propre[, c("annual_inc", "revol_bal", "revol_util", "dti")])
boxplot(base_propre$revol_bal, main = "Solde de crédit après plafonnement")

# ---- 12. Exploration ----
taux_par_duree <- base_propre %>%
  group_by(term) %>%
  summarise(taux_defaut = mean(defaut), nb_clients = n())

taux_par_duree

install.packages("ggplot2")
library(ggplot2)

ggplot(taux_par_duree, aes(x = term, y = taux_defaut)) +
geom_col(fill = "steelblue") +
labs(title = "Taux de défaut selon la durée du prêt",
       x = "Durée", y = "Taux de défaut")

taux_par_objet <- base_propre %>%
  group_by(purpose) %>%
  summarise(taux_defaut = mean(defaut), nb_clients = n())

taux_par_objet

ggplot(taux_par_objet, aes(x = reorder(purpose, taux_defaut), y = taux_defaut)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Taux de défaut selon l'objet du prêt",
       x = "Objet du prêt", y = "Taux de défaut")

# ---- 13. Variables numériques ----
base_propre %>%
  group_by(defaut) %>%
  summarise(revenu_moyen = mean(annual_inc),
            dti_moyen = mean(dti),
            montant_moyen = mean(loan_amnt),
            utilisation_moyenne = mean(revol_util))
ggplot(base_propre, aes(x = factor(defaut), y = dti)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Endettement (dti) selon le statut du client",
       x = "Défaut (0 = a remboursé, 1 = défaut)", y = "dti")

saveRDS(base_propre, "base_propre.rds")

# ---- 14. Séparation entraînement / test ----
set.seed(123)
n <- nrow(base_propre)
indices_train <- sample(1:n, size = 0.7 * n)

train <- base_propre[indices_train, ]
test  <- base_propre[-indices_train, ]

dim(train)
dim(test)

# ---- 15. Modèle de régression logistique ----
modele <- glm(defaut ~ ., data = train, family = binomial)

summary(modele)

# ---- 16. Rapports de cotes ----
exp(coef(modele))

# ---- 17. Prédictions sur le jeu de test ----
test$proba <- predict(modele, newdata = test, type = "response")
head(test$proba)

library(pROC)

roc_obj <- roc(test$defaut, test$proba)
auc(roc_obj)
plot(roc_obj, main = "Courbe ROC")

# ---- 18. Décision et matrice de confusion ----
test$prediction <- ifelse(test$proba > 0.20, 1, 0)

matrice <- table(Reel = test$defaut, Prevu = test$prediction)
matrice

# Part de mauvais clients détectés
matrice["1", "1"] / sum(matrice["1", ])
# Part de bons clients refusés à tort
matrice["0", "1"] / sum(matrice["0", ])

saveRDS(modele, "modele_scoring.rds")

# ---- 19. Note de score et classes de risque ----
test$score <- round(300 + 550 * (1 - test$proba))

test$classe_risque <- cut(test$score,
                          breaks = c(0, 600, 700, 800, 1000),
                          labels = c("Très élevé", "Élevé", "Moyen", "Faible"))

summary(test$score)
table(test$classe_risque)

test %>%
  group_by(classe_risque) %>%
  summarise(taux_defaut_reel = mean(defaut), nb_clients = n())


