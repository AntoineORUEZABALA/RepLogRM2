library(shiny)
library(readr)
library(readxl)
library(shinyWidgets)
library(DT)
library(dplyr)
library(shinythemes)
library(nnet) # Pour multinom

get_dataset_description <- function(dataset_name) {
  descriptions <- list(
    "iris" = "Le jeu de données iris contient des mesures de longueur et de largeur des sépales et pétales pour trois espèces de fleurs : Setosa, Versicolor, et Virginica.",
    "Titanic" = "Le jeu de données Titanic fournit des informations agrégées sur les passagers du Titanic, comprenant la classe, le sexe, l'âge et la survie.",
    "esoph" = "Le jeu de données esoph contient des données épidémiologiques sur la relation entre la consommation d'alcool et de tabac et le cancer de l'œsophage.",
    "UCBAdmissions" = "Le jeu de données UCBAdmissions contient des données sur l'admission des étudiants à six départements de l'Université de Californie à Berkeley, selon le sexe et le statut d'admission.",
    "warpbreaks" = "Le jeu de données warpbreaks examine le nombre de ruptures de fils lors du tissage, en fonction de la tension appliquée et du type de laine utilisé."
  )
  return(descriptions[[dataset_name]])
}

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  tags$head(
    tags$style(HTML("
    
  /* Style général pour le pickerInput */
  .bootstrap-select .dropdown-toggle {
    height: 34px !important;
    padding: 6px 12px !important;
    line-height: 1.5 !important;
    border-radius: 4px !important;
    background-color: white !important;
    border: 1px solid #cecece !important;
  }
  
  .bootstrap-select .dropdown-toggle[title='Sélectionner...'] .filter-option {
    color: #6c757d !important;
  }

  .bootstrap-select .dropdown-toggle:not([title='Sélectionner...']) .filter-option {
    color: #212529 !important;
    font-style: normal;
  }

  .bootstrap-select .dropdown-toggle:focus {
    border-color: #66afe9 !important;
    box-shadow: 0 0 8px rgba(102, 175, 233, 0.6) !important;
    outline: none !important;
    border-radius: 4px !important;
  }

  .bootstrap-select .dropdown-menu {
    border-radius: 4px !important;
  }

  .bootstrap-select .dropdown-menu .active {
    background-color: #66afe9 !important;
    color: white !important;
  }
  
  
"))
  ),
  titlePanel("Régression Logistique"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Chargement des données"),
      
      radioButtons("data_source", "Source des données:",
                   choices = c("Fichier CSV/Excel", "Dataset intégré"),
                   selected = "Dataset intégré"),
      
      conditionalPanel(
        condition = "input.data_source == 'Fichier CSV/Excel'",
        fileInput("file_input", "Importer un fichier",
                  accept = c(".csv", ".xlsx")),
        radioButtons("file_sep", "Séparateur:",
                     choices = c("Virgule" = ",",
                                 "Tabulation" = "\t",
                                 "Point-virgule" = ";"),
                     selected = ",")
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'Dataset intégré'",
        selectInput("dataset_choice", "Choisir un dataset:",
                    choices = c("iris", "Titanic", "esoph", "UCBAdmissions", "warpbreaks"))
      ),
      
      h3("Choix des variables"),
      selectInput("target_var", "Variable cible:", choices = NULL),
      pickerInput(
        inputId = "expl_vars",
        label = "Variables explicatives:",
        choices = NULL,
        multiple = TRUE,
        options = list(
          'actions-box' = TRUE,
          'none-selected-text' = "Sélectionner...",
          'style' = "btn-default"
        )
      ),
      
      h3("Type de problème"),
      radioButtons("problem_type", "Choisir le type de problème:",
                   choices = c("Binaire" = "binary", "Multinomial" = "multinomial"),
                   selected = "binary")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Aperçu des données", 
                 uiOutput("dataset_info_ui"),
                 h3("Tableau interactif"),
                 DTOutput("data_table")),
        tabPanel("Résumé statistique",
                 h3("Résumé des variables"),
                 verbatimTextOutput("data_summary")),
        tabPanel("Régression Logistique avec GLM/Multinom",
                 h3("Résultats de la régression"),
                 br(),
                 p("Dans cet onglet, les fonctions ", 
                   strong("glm"), " et ", strong("multinom"), 
                   " de R sont utilisées pour effectuer la régression logistique.",
                   style = "color: #555; font-size: 14px;"),
                 br(),
                 actionButton("run_model", "Exécuter la régression"),
                 br(), br(),  # Ajouter des sauts de ligne ici
                 uiOutput("warning_message"),
                 br(),
                 verbatimTextOutput("model_summary")),
        
        tabPanel("Régression logistique avec MIMOSA",
                 h3("Résultats de la régression avec MIMOSA"),
                 br(),
                 p("Dans cet onglet, la régression logistique est réalisée à l'aide du package ",
                   strong("MIMOSA"), 
                   ". Ce package propose une implémentation alternative des modèles de régression logistique, développée pour offrir une meilleure flexibilité et des fonctionnalités supplémentaires.",
                   style = "color: #555; font-size: 14px;"),
                 br(),
                 actionButton("run_mimosa", "Exécuter la régression avec MIMOSA"),
                 br(), br(),  # Ajouter des sauts de ligne ici  
                 verbatimTextOutput("mimosa_summary")) # Résultats pour MIMOSA
      )
    )
  )
)

server <- function(input, output, session) {
  # Charger les données selon la source sélectionnée
  dataset <- reactive({
    if (input$data_source == "Fichier CSV/Excel") {
      req(input$file_input)
      tryCatch({
        data <- if (grepl("\\.csv$", input$file_input$name)) {
          read_delim(input$file_input$datapath, delim = input$file_sep, show_col_types = FALSE)
        } else if (grepl("\\.xlsx$", input$file_input$name)) {
          read_excel(input$file_input$datapath)
        } else {
          stop("Format de fichier non supporté.")
        }
        data <- data %>% 
          mutate(across(where(is.character), as.factor)) 
        return(data)
      }, error = function(e) {
        showNotification(paste("Erreur lors du chargement du fichier:", e$message),
                         type = "error")
        return(NULL)
      })
    } else if (input$data_source == "Dataset intégré") {
      switch(input$dataset_choice,
             "iris" = iris,
             "Titanic" = as.data.frame(Titanic),  # Convertir Titanic en dataframe
             "esoph" = esoph,
             "UCBAdmissions" = as.data.frame(UCBAdmissions),  # Convertir UCBAdmissions
             "warpbreaks" = warpbreaks)
    }
  })
  
  
  
  # Effacer le message d'avertissement à chaque changement de sélection
  observe({
    input$problem_type
    input$target_var
    input$expl_vars
    output$warning_message <- renderUI({ NULL })
  })
  
  
  # Mettre à jour les choix pour les variables dès qu'un dataset est chargé
  observe({
    data <- dataset()
    if (!is.null(data)) {
      updateSelectInput(session, "target_var", 
                        choices = c("Sélectionner..." = "", names(data)),
                        selected = NULL)
      updatePickerInput(session, "expl_vars", 
                        choices = names(data), 
                        selected = NULL)
    }
  })
  
  # Exclure la variable cible des variables explicatives
  observeEvent(input$target_var, {
    data <- dataset()
    if (!is.null(data) && !is.null(input$target_var)) {
      expl_choices <- setdiff(names(data), input$target_var)
      updatePickerInput(session, "expl_vars", 
                        choices = expl_choices, 
                        selected = intersect(input$expl_vars, expl_choices))
    }
  })
  
  # Mettre à jour la description du dataset sélectionné
  output$dataset_description <- renderText({
    req(input$data_source == "Dataset intégré")  # Assure que c'est un dataset intégré
    req(input$dataset_choice)  # Vérifie que le dataset est sélectionné
    get_dataset_description(input$dataset_choice)  # Affiche la description du dataset
  })
  
  # Bloc dynamique pour les informations du dataset (Titre + Description)
  output$dataset_info_ui <- renderUI({
    if (input$data_source == "Dataset intégré" && input$dataset_choice != "") {
      tagList(
        h3("Informations sur le dataset :"),
        textOutput("dataset_description"),
        br()
      )
    } else {
      NULL  # Rien n'est affiché si aucun dataset intégré n'est sélectionné
    }
  })
  
  # Table interactive
  output$data_table <- renderDT({
    data <- dataset()
    req(data)
    datatable(data, options = list(
      pageLength = 10,
      lengthMenu = c(10, 25, 50),
      autoWidth = TRUE
    ))
  })
  
  # Résumé statistique
  output$data_summary <- renderPrint({
    data <- dataset()
    req(data)
    summary(data)
  })
  
  # Modèle binaire ou multinomial
  model <- eventReactive(input$run_model, {
    data <- dataset()
    target <- input$target_var
    expl_vars <- input$expl_vars
    problem_type <- input$problem_type
    
    req(data, target, expl_vars, problem_type)
    
    if (!is.factor(data[[target]])) {
      data[[target]] <- as.factor(data[[target]])
    }
    
    formula <- as.formula(paste(target, "~", paste(expl_vars, collapse = "+")))
    
    if (problem_type == "binary") {
      # Vérifier que le problème est binaire
      if (length(unique(data[[target]])) != 2) {
        stop("La variable cible doit contenir exactement 2 classes pour une régression binaire.")
      }
      
      # Ajuster le modèle binaire avec glm
      tryCatch({
        glm(formula, data = data, family = binomial())
      }, warning = function(w) {
        showNotification(paste("Avertissement:", w$message), type = "warning")
        NULL
      }, error = function(e) {
        showNotification(paste("Erreur:", e$message), type = "error")
        NULL
      })
      
    } else if (problem_type == "multinomial") {
      # Vérifier que le problème est multinomial
      if (length(unique(data[[target]])) <= 2) {
        output$warning_message <- renderUI({
          div(style = "color: orange;", "Votre problème semble binaire. Vous pourriez utiliser une régression logistique binaire.")
        })
      }
      
      # Ajuster le modèle multinomial avec multinom
      tryCatch({
        multinom(formula, data = data)
      }, warning = function(w) {
        showNotification(paste("Avertissement:", w$message), type = "warning")
        NULL
      }, error = function(e) {
        showNotification(paste("Erreur:", e$message), type = "error")
        NULL
      })
    }
  })
  
  # Résumé des résultats du modèle
  output$model_summary <- renderPrint({
    req(model())
    summary(model())
  })
  
  # Modèle MIMOSA (placeholder)
  mimosa_model <- eventReactive(input$run_mimosa, {
    # Placeholder pour le modèle MIMOSA
    data <- dataset()
    req(data)
    "Régression MIMOSA non implémentée encore."
  })
  
  output$mimosa_summary <- renderPrint({
    mimosa_model()
  })
}

shinyApp(ui, server)
