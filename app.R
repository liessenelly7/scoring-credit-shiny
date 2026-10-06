library(shiny)
library(ggplot2)

# ---- Chargement du modèle ----
modele <- readRDS("modele_scoring_leger.rds")
niveaux <- modele$xlevels

# ---- Interface ----
ui <- fluidPage(
  titlePanel("Scoring de crédit : évaluation d'un client"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Informations sur le client"),
      actionButton("calculer", "Calculer le score", class = "btn-primary"),
      br(), br(),
      
      numericInput("loan_amnt", "Montant du prêt :", value = 10000, min = 500, max = 40000, step = 500),
      selectInput("term", "Durée du prêt :", choices = niveaux$term),
      selectInput("emp_length", "Ancienneté dans l'emploi :",
                  choices = c("< 1 year", "1 year", "2 years", "3 years", "4 years", "5 years",
                              "6 years", "7 years", "8 years", "9 years", "10+ years", "Inconnu"),
                  selected = "5 years"),
      selectInput("home_ownership", "Logement :", choices = niveaux$home_ownership),
      numericInput("annual_inc", "Revenu annuel :", value = 60000, min = 3000, max = 236000, step = 1000),
      selectInput("verification_status", "Revenu vérifié :", choices = niveaux$verification_status),
      selectInput("purpose", "Objet du prêt :", choices = niveaux$purpose),
      numericInput("dti", "Endettement (dti) :", value = 15, min = 0, max = 35),
      numericInput("delinq_2yrs", "Retards de paiement (2 ans) :", value = 0, min = 0, max = 20),
      numericInput("inq_last_6mths", "Demandes de crédit (6 mois) :", value = 0, min = 0, max = 10),
      numericInput("open_acc", "Crédits ouverts :", value = 10, min = 0, max = 50),
      numericInput("pub_rec", "Incidents publics négatifs :", value = 0, min = 0, max = 10),
      numericInput("revol_bal", "Solde de crédit renouvelable :", value = 10000, min = 0, max = 78000, step = 500),
      numericInput("revol_util", "Utilisation du crédit (%) :", value = 50, min = 0, max = 100),
      numericInput("total_acc", "Nombre total de comptes :", value = 20, min = 1, max = 100),
      hr(),
      sliderInput("seuil", "Seuil de refus (probabilité de défaut) :",
                  min = 0.05, max = 0.50, value = 0.20, step = 0.01)
    ),
    
    mainPanel(
      h3("Résultat"),
      uiOutput("carte"),
      plotOutput("jauge", height = "120px")
    )
  )
)

# ---- Serveur ----
server <- function(input, output) {
  
  resultat <- eventReactive(input$calculer, {
    client <- data.frame(
      loan_amnt = input$loan_amnt,
      term = factor(input$term, levels = niveaux$term),
      emp_length = factor(input$emp_length, levels = niveaux$emp_length),
      home_ownership = factor(input$home_ownership, levels = niveaux$home_ownership),
      annual_inc = input$annual_inc,
      verification_status = factor(input$verification_status, levels = niveaux$verification_status),
      purpose = factor(input$purpose, levels = niveaux$purpose),
      dti = input$dti,
      delinq_2yrs = input$delinq_2yrs,
      inq_last_6mths = input$inq_last_6mths,
      open_acc = input$open_acc,
      pub_rec = input$pub_rec,
      revol_bal = input$revol_bal,
      revol_util = input$revol_util,
      total_acc = input$total_acc
    )
    
    proba <- predict(modele, newdata = client, type = "response")
    score <- round(300 + 550 * (1 - proba))
    classe <- cut(score, breaks = c(0, 600, 700, 800, 1000),
                  labels = c("Très élevé", "Élevé", "Moyen", "Faible"))
    decision <- ifelse(proba > input$seuil, "À refuser ou à étudier de près", "À accepter")
    
    list(proba = proba, score = score, classe = classe, decision = decision)
  })
  
  output$carte <- renderUI({
    r <- resultat()
    
    couleur <- switch(as.character(r$classe),
                      "Faible" = "#2e7d32",
                      "Moyen" = "#f9a825",
                      "Élevé" = "#ef6c00",
                      "Très élevé" = "#c62828")
    
    div(style = paste0("background:", couleur,
                       "; color:white; padding:25px; border-radius:10px;"),
        h4("Note de score (300 à 850)"),
        h1(r$score),
        p(paste0("Probabilité de défaut : ", round(r$proba * 100, 1), " %")),
        p(paste0("Classe de risque : ", r$classe)),
        h3(r$decision)
    )
  })
  
  output$jauge <- renderPlot({
    r <- resultat()
    
    zones <- data.frame(
      debut  = c(300, 600, 700, 800),
      fin    = c(600, 700, 800, 850),
      classe = factor(c("Très élevé", "Élevé", "Moyen", "Faible"),
                      levels = c("Très élevé", "Élevé", "Moyen", "Faible"))
    )
    
    ggplot() +
      geom_rect(data = zones,
                aes(xmin = debut, xmax = fin, ymin = 0, ymax = 1, fill = classe)) +
      geom_vline(xintercept = r$score, linewidth = 2) +
      scale_fill_manual(values = c("Très élevé" = "#c62828", "Élevé" = "#ef6c00",
                                   "Moyen" = "#f9a825", "Faible" = "#2e7d32")) +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(fill = "Zone de risque")
  })
}

shinyApp(ui, server)