# Scoring de crédit : application Shiny

Application d'aide à la décision pour l'octroi de crédit.

## Données
Prêts Lending Club (887 379 prêts, dont 254 190 avec une issue connue).

## Méthode
- Nettoyage : valeurs manquantes, plafonnement des valeurs extrêmes
- Variables connues avant l'octroi uniquement (pas de fuite d'information)
- Régression logistique, découpage 70 % entraînement / 30 % test
- Performance : AUC = 0,68 sur le jeu de test

## Application
Formulaire client, note de score (300 à 850), classe de risque,
jauge de position et seuil de décision réglable.

## Lancer l'application
shiny::runApp("scoring_app")




