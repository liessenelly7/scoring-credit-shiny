# Scoring de crédit : application Shiny

Application d'aide à la décision pour l'octroi de crédit, développée en R.
Un conseiller saisit le profil d'un client et obtient une note de score
(300 à 850), une probabilité de défaut, une classe de risque et une décision.

## Données
Prêts de la plateforme Lending Club (jeu de données public disponible sur Kaggle,
fichier `loan.csv`) : 887 379 prêts, dont 254 190 avec une issue connue
(remboursé ou défaut). Les données ne sont pas incluses dans ce dépôt.

## Méthode
- Nettoyage : valeurs manquantes, regroupement de catégories rares,
  plafonnement des valeurs extrêmes
- Variables connues avant l'octroi uniquement, pour éviter toute fuite d'information
- Régression logistique, découpage 70 % entraînement / 30 % test
- Performance : AUC = 0,68 sur le jeu de test
- Note de score = 300 + 550 × (1 − probabilité de défaut)

## Contenu du dépôt
- `02_preparation.R` : préparation des données, exploration et modélisation
- `app.R` : application Shiny
- `modele_scoring_leger.rds` : modèle de régression logistique allégé

## Lancer l'application
Dans R, installer les packages puis lancer l'application :

install.packages(c("shiny", "ggplot2"))
shiny::runApp()

## Droits
© 2026 [Iradukunda]. Projet présenté à titre de démonstration, tous droits réservés.

