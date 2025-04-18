


# Charger les librairies
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(readxl)
library(haven)
library(dplyr)
library(shinyWidgets)
library(plotly)
library(scales)

# Fonction pour lire les fichiers
lire_fichier_generique <- function(path, ext) {
  switch(tolower(ext),
         "csv"  = read.csv(path, stringsAsFactors = FALSE),
         "xlsx" = read_excel(path),
         "xls"  = read_excel(path),
         "dta"  = {
           data <- read_dta(path)
           data <- data %>% mutate(across(where(is.labelled), ~ as_factor(.)))  # Transformation des variables étiquetées
           return(data)
         },
         "sav"  = read_sav(path),
         stop("Format de fichier non pris en charge"))
}


# Fonction pour obtenir le mode
get_mode <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA)
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Fonction pour détecter les valeurs aberrantes (méthode IQR)
detecter_aberrantes <- function(x) {
  if (!is.numeric(x) || all(is.na(x))) return(rep(FALSE, length(x)))
  Q1   <- quantile(x, 0.25, na.rm = TRUE)
  Q3   <- quantile(x, 0.75, na.rm = TRUE)
  IQRv <- IQR(x, na.rm = TRUE)
  out  <- x < (Q1 - 1.5 * IQRv) | x > (Q3 + 1.5 * IQRv)
  out[is.na(out)] <- FALSE
  out
}


## Augmenter la taille maximale des requêtes (ici 500 Mo)
options(shiny.maxRequestSize = 500 * 1024^2)  # 500 Mo

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://cdn-icons-png.flaticon.com/512/2103/2103652.png", height = "30px"),
      "DataCleaner Pro"
    ),
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Chargement des données", tabName = "chargement", icon = icon("upload")),
      menuItem("Traitement des données", tabName = "traitement", icon = icon("edit"),
               menuSubItem("Visualisation",       tabName = "visualisation", icon = icon("table")),
               menuSubItem("Doublons",            tabName = "doublons",      icon = icon("copy")),
               menuSubItem("Valeurs Manquantes",  tabName = "manquantes",    icon = icon("question-circle")),
               menuSubItem("Valeurs Aberrantes",  tabName = "aberrantes",    icon = icon("exclamation-triangle")),
               menuSubItem("Enregistrer la base",  tabName = "enregistrer",    icon = icon("table"))
               
      ),
      menuItem("Statistiques Descriptives", tabName = "stat_desc", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo { background-color: #3c8dbc; font-weight: bold; font-size: 20px; }
        .skin-blue .main-header .navbar { background-color: #3c8dbc; }
        .box { border-top: 3px solid #3c8dbc; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        .content-wrapper { background-color: #f8f9fa; }
        .btn-primary { background-color: #3c8dbc; border-color: #367fa9; }
        .btn-primary:hover { background-color: #367fa9; border-color: #204d74; }
        .sidebar-menu .treeview-menu > li > a { padding-left: 35px; }
        .box-header { color: #3c8dbc; }
      "))
    ),
    
    tabItems(
      # Accueil
      tabItem("accueil",
              fluidRow(
                box(width = 12, status = "info", title = "Bienvenue dans DataCleaner Pro", solidHeader = TRUE,
                    div(style = "text-align:center; padding:20px;",
                        icon("database", "fa-5x", style = "color:#3c8dbc;"),
                        h3("Votre solution complète pour le traitement de données"),
                        p("Cette application vous permet de charger, nettoyer et analyser vos données avec facilité.")
                    )
                )
              ),
              fluidRow(
                valueBox("Chargement", "Importez vos données",        icon = icon("upload"),       color = "blue",   width = 4),
                valueBox("Nettoyage",  "Doublons, NA, aberrants",    icon = icon("broom"),        color = "green",  width = 4),
                valueBox("Analyse",    "Statistiques & graphes",     icon = icon("chart-bar"),    color = "purple", width = 4)
              )
      ),
      
      # Chargement
      tabItem("chargement",
              fluidRow(
                box(width = 4, status = "primary", title = "Importer un fichier",
                    fileInput("fichier", "Choisir un fichier", accept = c(".csv",".dta",".sav",".xlsx",".xls")),
                    actionBttn("charger", "Charger le fichier", style = "gradient", color = "primary", icon = icon("check")),
                    hr(), verbatimTextOutput("message")
                ),
                box(width = 8, status = "info", title = "Informations sur les données", DTOutput("tableau_resume"))
              ),
              fluidRow(
                box(width = 12, status = "primary", title = "Variables et leurs caractéristiques", DTOutput("structure_variables"))
              )
      ),
      
      # Visualisation
      tabItem("visualisation",
              box(width = 12, status = "primary", title = "Aperçu des données", DTOutput("table_base"))
      ),
      
      # Doublons
      tabItem("doublons",
              fluidRow(
                box(width = 4, status = "warning", title = "Gestion des doublons",
                    actionBttn("detecter_doublons", "Détecter",   style = "gradient", color = "warning", icon = icon("search")),
                    hr(),
                    actionBttn("supprimer_doublons","Supprimer", style = "gradient", color = "danger",  icon = icon("trash")),
                    hr(),
                    actionBttn("annuler_doublons",  "Restaurer", style = "gradient", color = "default", icon = icon("undo")),
                    hr(), verbatimTextOutput("message_doublons")
                ),
                box(width = 8, status = "primary", title = "Doublons détectés", DTOutput("table_doublons"))
              )
      ),
      
      # Valeurs manquantes
      tabItem("manquantes",
              fluidRow(
                box(width = 4, status = "warning", title = "Traitement des NA",
                    uiOutput("var_manquantes_select"), hr(),
                    uiOutput("methode_imputation_ui"), hr(),
                    conditionalPanel("input.methode_imputation=='conditionnee'",
                                     uiOutput("var_condition_select"),
                                     uiOutput("nb_conditions_ui")
                    ),
                    hr(), actionBttn("imputer", "Imputer", style = "gradient", color = "primary", icon = icon("magic")),
                    hr(), verbatimTextOutput("message_imputation")
                ),
                tabBox(width = 8, title = "Analyse des NA",
                       tabPanel("Résumé",      DTOutput("resume_manquantes")),
                       tabPanel("Visualisation", plotOutput("plot_manquantes"))
                )
              )
      ),
      
      # Valeurs aberrantes
      tabItem("aberrantes",
              fluidRow(
                box(width = 4, status = "danger", title = "Traitement des outliers",
                    uiOutput("var_aberrantes_select"), hr(),
                    uiOutput("methode_traitement_aberrantes_ui"), hr(),
                    conditionalPanel("input.methode_traitement_aberrantes=='conditionnee'",
                                     uiOutput("var_condition_aberrantes_select"),
                                     uiOutput("nb_conditions_aberrantes_ui")
                    ),
                    hr(), actionBttn("traiter_aber", "Traiter", style = "gradient", color = "danger", icon = icon("filter")),
                    hr(), verbatimTextOutput("message_aber")
                ),
                tabBox(width = 8, title = "Analyse des outliers",
                       tabPanel("Résumé",      DTOutput("resume_aberrantes")),
                       tabPanel("Visualisation", plotOutput("plot_aberrantes"))
                )
              )
      ),
      # Ajout du bouton "Enregistrer la base"
      tabItem("traitement",
              fluidRow(
                box(width = 12, status = "success", title = "Enregistrer la base traitée",
                    downloadButton(
                      outputId = "download_data",
                      label    = "Télécharger la base traitée",
                      class    = "btn btn-success"
                    )
                ),
                
                verbatimTextOutput("message_enregistrement")
              )
      ),    
      # Statistiques descriptives
      tabItem("stat_desc",
              fluidRow(
                box(width = 3, status = "primary", title = "Sélection",
                    uiOutput("var_stat_select"), hr(),
                    conditionalPanel("output.var_type=='numeric'",
                                     selectInput("graph_type_num","Graphique numérique",
                                                 choices = c("Histogramme"="hist","Boxplot"="boxplot","Densité"="density"))
                    ),
                    conditionalPanel("output.var_type=='factor'",
                                     selectInput("graph_type_cat","Graphique factoriel",
                                                 choices = c("Barres"="bar","Camembert"="pie"))
                    ),
                    hr(), actionBttn("generer_stat", "Générer", style = "gradient", color = "primary", icon = icon("chart-bar"))
                ),
                tabBox(width = 9, title = "Résultats",
                       tabPanel("Graphique", plotlyOutput("graphique_stat", height = "500px")),
                       tabPanel("Tableau",   DTOutput("table_stat"))
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  data_originale <- reactiveVal(NULL)
  data_actuelle <- reactiveVal(NULL)
  base_existe    <- reactiveVal(FALSE)
  
  # Détection du type pour stats
  output$var_type <- reactive({
    req(data_actuelle(), input$var_stat)
    if (is.numeric(data_actuelle()[[input$var_stat]])) "numeric" else "factor"
  })
  outputOptions(output, "var_type", suspendWhenHidden = FALSE)
  
  # --- Chargement des données ---
  observeEvent(input$charger, {
    req(input$fichier)
    if (base_existe()) {
      showModal(modalDialog(
        title = "Attention", "Une base existe déjà. Remplacez‑la ?",
        footer = tagList(modalButton("Annuler"),
                         actionButton("confirmer_remplacement","Remplacer"))
      ))
    } else {
      charger_donnees()
    }
  })
  observeEvent(input$confirmer_remplacement, { charger_donnees(); removeModal() })
  
  charger_donnees <- function() {
    ext <- tools::file_ext(input$fichier$name)
    tryCatch({
      d <- lire_fichier_generique(input$fichier$datapath, ext) %>%
        mutate(across(where(is.character), as.factor))
      data_originale(d); data_actuelle(d); base_existe(TRUE)
      output$message <- renderText(paste("Chargé :", input$fichier$name,
                                         "\nObs :", nrow(d), "  Var :", ncol(d)))
    }, error = function(e) {
      output$message <- renderText(paste("Erreur :", e$message))
    })
  }
  
  # --- Résumé et structure ---
  output$tableau_resume <- renderDT({
    req(data_actuelle())
    df <- data.frame(Fichier = input$fichier$name,
                     Observations = nrow(data_actuelle()),
                     Variables    = ncol(data_actuelle()))
    datatable(df, options = list(dom = 't'))
  })
  output$structure_variables <- renderDT({
    req(data_actuelle())
    df <- data_actuelle()
    info <- data.frame(
      Variable = names(df),
      Type     = sapply(df, function(x) class(x)[1]),
      NA_count = sapply(df, function(x) sum(is.na(x))),
      NA_pct   = round(sapply(df, function(x) mean(is.na(x))*100),2),
      Unique   = sapply(df, function(x) length(unique(x)))
    )
    datatable(info, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$table_base <- renderDT({
    req(data_actuelle())
    datatable(data_actuelle(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # --- Doublons ---
  observeEvent(input$detecter_doublons, {
    req(data_actuelle())
    dupl <- duplicated(data_actuelle()) | duplicated(data_actuelle(), fromLast = TRUE)
    df   <- data_actuelle()[dupl, ]
    output$table_doublons <- renderDT(
      if (nrow(df) > 0) datatable(df) else datatable(data.frame(MSG = "Aucun doublon"))
    )
    output$message_doublons <- renderText(
      if (nrow(df) > 0) paste(sum(duplicated(data_actuelle())), "doublons") else "Aucun doublon"
    )
  })
  observeEvent(input$supprimer_doublons, {
    req(data_actuelle())
    n0 <- nrow(data_actuelle())
    da <- distinct(data_actuelle()); data_actuelle(da)
    output$message_doublons <- renderText(paste(n0 - nrow(da), "doublons supprimés"))
    output$table_doublons <- renderDT(datatable(data.frame(MSG = "Doublons supprimés")))
  })
  observeEvent(input$annuler_doublons, {
    req(data_originale())
    data_actuelle(data_originale())
    output$message_doublons <- renderText("Données restaurées")
  })
  
  # --- Valeurs manquantes ---
  output$var_manquantes_select <- renderUI({
    req(data_actuelle())
    na_vars <- names(data_actuelle())[sapply(data_actuelle(), anyNA)]
    if (length(na_vars) == 0) return(tags$p("Aucune NA détectée"))
    selectInput("var_manquante", "Variable :", choices = na_vars)
  })
  output$methode_imputation_ui <- renderUI({
    req(input$var_manquante, data_actuelle())
    v <- data_actuelle()[[input$var_manquante]]
    if (is.numeric(v)) {
      tagList(
        radioButtons("methode_imputation", "Méthode", choices = c("Simple" = "simple", "Conditionnée" = "conditionnee")),
        conditionalPanel("input.methode_imputation=='simple'",
                         radioButtons("type_imputation_num", "Type", choices = c("Moyenne" = "moyenne", "Médiane" = "mediane")))
      )
    } else {
      tagList(
        radioButtons("methode_imputation", "Méthode", choices = c("Simple" = "simple", "Conditionnée" = "conditionnee")),
        conditionalPanel("input.methode_imputation=='simple'",
                         radioButtons("type_imputation_cat", "Type", choices = c("Mode" = "mode")))
      )
    }
  })
  output$var_condition_select <- renderUI({
    req(input$var_manquante, data_actuelle())
    selectInput("var_condition", "Condition sur :", setdiff(names(data_actuelle()), input$var_manquante), multiple = TRUE)
  })
  output$nb_conditions_ui <- renderUI({
    req(input$var_condition)
    numericInput("nb_conditions", "Nb variables", min = 1, max = length(input$var_condition), value = min(3, length(input$var_condition)))
  })
  output$resume_manquantes <- renderDT({
    req(data_actuelle())
    df <- data.frame(
      Variable = names(data_actuelle()),
      NA_count = sapply(data_actuelle(), function(x) sum(is.na(x))),
      NA_pct   = round(sapply(data_actuelle(), function(x) mean(is.na(x))*100),2)
    )
    df <- df[df$NA_count > 0, ]
    if (nrow(df) == 0) return(datatable(data.frame(MSG = "Aucune NA")))
    datatable(df)
  })
  output$plot_manquantes <- renderPlot({
    req(data_actuelle())
    df <- data.frame(
      Var = names(data_actuelle()),
      pct = sapply(data_actuelle(), function(x) mean(is.na(x))*100)
    ) %>% filter(pct > 0) %>% arrange(desc(pct))
    if (nrow(df) == 0) {
      ggplot() + annotate("text", x = 1, y = 1, label = "Aucune NA") + theme_void()
    } else {
      ggplot(df, aes(reorder(Var, pct), pct)) +
        geom_col() + coord_flip() +
        labs(x = "", y = "% NA", title = "Valeurs manquantes")
    }
  })
  observeEvent(input$imputer, {
    req(data_actuelle(), input$var_manquante)
    d0 <- data_actuelle(); v0 <- d0[[input$var_manquante]]
    if (!any(is.na(v0))) {
      output$message_imputation <- renderText("Pas de NA à traiter"); return()
    }
    d1 <- d0
    if (input$methode_imputation == "simple") {
      if (is.numeric(v0)) {
        val <- if (input$type_imputation_num == "moyenne") mean(v0, na.rm = TRUE) else median(v0, na.rm = TRUE)
        d1[[input$var_manquante]][is.na(d1[[input$var_manquante]])] <- val
        msg <- paste("Imputation simple par", if (input$type_imputation_num == "moyenne") "moyenne" else "médiane", round(val, 2))
      } else {
        val <- get_mode(v0)
        d1[[input$var_manquante]][is.na(d1[[input$var_manquante]])] <- val
        msg <- paste("Imputation simple par mode =", val)
      }
    } else {
      req(input$var_condition, input$nb_conditions)
      conds <- input$var_condition[1:input$nb_conditions]
      if (is.numeric(v0)) {
        d1 <- d1 %>% group_by(across(all_of(conds))) %>%
          mutate("{input$var_manquante}" := ifelse(is.na(.data[[input$var_manquante]]),
                                                   median(.data[[input$var_manquante]], na.rm = TRUE),
                                                   .data[[input$var_manquante]])) %>% ungroup()
      } else {
        d1 <- d1 %>% group_by(across(all_of(conds))) %>%
          mutate("{input$var_manquante}" := ifelse(is.na(.data[[input$var_manquante]]),
                                                   get_mode(.data[[input$var_manquante]]),
                                                   .data[[input$var_manquante]])) %>% ungroup()
      }
      msg <- paste("Imputation conditionnée sur", length(conds), "variables")
    }
    data_actuelle(d1)
    na0 <- sum(is.na(v0)); na1 <- sum(is.na(d1[[input$var_manquante]]))
    output$message_imputation <- renderText(paste(msg, "\nTraitées:", na0 - na1))
  })
  
  # --- Valeurs aberrantes ---
  output$var_aberrantes_select <- renderUI({
    req(data_actuelle())
    ab_vars <- names(data_actuelle())[sapply(data_actuelle(), function(x) is.numeric(x) && any(detecter_aberrantes(x)))]
    if (length(ab_vars) == 0) return(tags$p("Aucun outlier détecté"))
    selectInput("var_aberrante", "Variable :", choices = ab_vars)
  })
  output$methode_traitement_aberrantes_ui <- renderUI({
    req(input$var_aberrante, data_actuelle())
    tagList(
      radioButtons("methode_traitement_aberrantes", "Méthode",
                   choices = c("Simple" = "simple", "Conditionnée" = "conditionnee")),
      conditionalPanel("input.methode_traitement_aberrantes=='simple'",
                       radioButtons("type_traitement_aber_num", "Type",
                                    choices = c("Médiane" = "mediane", "Écrêtage" = "ecretage")))
    )
  })
  output$var_condition_aberrantes_select <- renderUI({
    req(input$var_aberrante, data_actuelle())
    selectInput("var_condition_aber", "Condition sur :",
                setdiff(names(data_actuelle()), input$var_aberrante), multiple = TRUE)
  })
  output$nb_conditions_aberrantes_ui <- renderUI({
    req(input$var_condition_aber)
    numericInput("nb_conditions_aber", "Nb variables", min = 1,
                 max = length(input$var_condition_aber), value = min(3, length(input$var_condition_aber)))
  })
  output$resume_aberrantes <- renderDT({
    req(data_actuelle())
    tmp <- lapply(data_actuelle(), function(x) {
      if (is.numeric(x)) {
        ab <- detecter_aberrantes(x)
        c(sum(ab), round(mean(ab)*100, 2))
      } else c(0, 0)
    })
    df <- do.call(rbind, tmp) %>% as.data.frame()
    names(df) <- c("Nb_outliers", "Pct_outliers")
    df$Variable <- rownames(df)
    df <- df[df$Nb_outliers > 0, ]
    if (nrow(df) == 0) datatable(data.frame(MSG = "Aucun outlier"))
    else datatable(df)
  })
  output$plot_aberrantes <- renderPlot({
    req(input$var_aberrante, data_actuelle())
    v  <- data_actuelle()[[input$var_aberrante]]
    ab <- detecter_aberrantes(v)
    df <- data.frame(idx = seq_along(v), value = v, out = ab)
    ggplot(df, aes(idx, value, color = out)) +
      geom_point() +
      scale_color_manual(values = c("FALSE" = "#3c8dbc", "TRUE" = "#f39c12")) +
      labs(title = paste("Outliers de", input$var_aberrante), color = "Aberrant") +
      theme_minimal()
  })
  observeEvent(input$traiter_aber, {
    req(input$var_aberrante, data_actuelle())
    d0  <- data_actuelle(); v0 <- d0[[input$var_aberrante]]
    ab0 <- detecter_aberrantes(v0)
    if (!any(ab0)) {
      output$message_aber <- renderText("Pas d'outliers"); return()
    }
    d1 <- d0
    if (input$methode_traitement_aberrantes == "simple") {
      if (input$type_traitement_aber_num == "mediane") {
        val <- median(v0[!ab0], na.rm = TRUE)
        d1[[input$var_aberrante]][ab0] <- val
        msg <- paste("Remplacé par médiane:", round(val, 2))
      } else {
        Q1   <- quantile(v0, 0.25, na.rm = TRUE)
        Q3   <- quantile(v0, 0.75, na.rm = TRUE)
        IQRv <- IQR(v0, na.rm = TRUE)
        lo   <- Q1 - 1.5 * IQRv
        hi   <- Q3 + 1.5 * IQRv
        d1[[input$var_aberrante]][v0 < lo] <- lo
        d1[[input$var_aberrante]][v0 > hi] <- hi
        msg <- paste("Écrêtage aux bornes:", round(lo, 2), "et", round(hi, 2))
      }
    } else {
      req(input$var_condition_aber, input$nb_conditions_aber)
      conds <- input$var_condition_aber[1:input$nb_conditions_aber]
      dc    <- d0; dc$aberrant_flag <- ab0
      for (gr in split(dc, dc[conds])) {
        key      <- do.call(paste, c(gr[1, conds, drop = FALSE], sep = "_"))
        idx0     <- which(do.call(paste, c(d0[conds], sep = "_")) == key)
        ab_idx   <- idx0[gr$aberrant_flag]
        ok_vals  <- v0[idx0][!gr$aberrant_flag]
        if (length(ab_idx) > 0 && length(ok_vals) > 0) {
          med <- median(ok_vals, na.rm = TRUE)
          d1[[input$var_aberrante]][ab_idx] <- med
        }
      }
      # si restants
      ab1 <- detecter_aberrantes(d1[[input$var_aberrante]])
      if (any(ab1)) {
        medg <- median(v0[!ab0], na.rm = TRUE)
        d1[[input$var_aberrante]][ab1] <- medg
      }
      msg <- paste("Conditionné sur", length(conds), "variables")
    }
    data_actuelle(d1)
    out0 <- sum(ab0); out1 <- sum(detecter_aberrantes(d1[[input$var_aberrante]]))
    output$message_aber <- renderText(paste(msg, "\nTraités:", out0 - out1))
  })
  
  # --- Enregistrer la base traitée ---
  
  
  # --- Statistiques descriptives ---
  output$var_stat_select <- renderUI({
    req(data_actuelle()); selectInput("var_stat", "Variable", names(data_actuelle()))
  })
  observeEvent(input$generer_stat, {
    req(data_actuelle(), input$var_stat)
    v <- data_actuelle()[[input$var_stat]]
    output$graphique_stat <- renderPlotly({
      if (is.numeric(v)) {
        if (input$graph_type_num == "hist") {
          p <- ggplot(data_actuelle(), aes(.data[[input$var_stat]])) +
            geom_histogram(bins = 30) + labs(title = "Histogramme")
        } else if (input$graph_type_num == "boxplot") {
          p <- ggplot(data_actuelle(), aes(y = .data[[input$var_stat]])) +
            geom_boxplot() + labs(title = "Boxplot")
        } else {
          p <- ggplot(data_actuelle(), aes(.data[[input$var_stat]])) +
            geom_density() + labs(title = "Densité")
        }
        ggplotly(p)
      } else {
        freq <- as.data.frame(table(v))
        names(freq) <- c("Catégorie", "Fréquence")
        freq$Pourcent <- round(freq$Fréquence / sum(freq$Fréquence) * 100, 2)
        if (input$graph_type_cat == "bar") {
          p <- ggplot(freq, aes(x = Catégorie, y = Fréquence)) +
            geom_col() + labs(title = "Barres")
          ggplotly(p)
        } else {
          plot_ly(freq, labels = ~Catégorie, values = ~Fréquence, type = "pie") %>%
            layout(title = "Camembert")
        }
      }
    })
    output$table_stat <- renderDT({
      if (is.numeric(v)) {
        df <- data.frame(
          Stat       = c("Moyenne","Médiane","Mode","Écart‑type","Min","Max","Q1","Q3","NA","Total"),
          Valeur = c(mean(v, na.rm = TRUE), median(v, na.rm = TRUE), get_mode(v),
                     sd(v, na.rm = TRUE), min(v, na.rm = TRUE), max(v, na.rm = TRUE),
                     quantile(v, 0.25, na.rm = TRUE), quantile(v, 0.75, na.rm = TRUE),
                     sum(is.na(v)), length(v))
        )
        datatable(df) %>% formatRound("Valeur", 2)
      } else {
        freq <- as.data.frame(table(v))
        names(freq) <- c("Catégorie","Fréquence")
        freq$Pourcent <- round(freq$Fréquence / sum(freq$Fréquence) * 100, 2)
        datatable(freq) %>% formatRound("Pourcent", 2)
      }
    })
  })
  # downloadHandler multisupport (CSV, Excel, Stata, SPSS)
output$download_data <- downloadHandler(
  filename = function() {
    orig      <- input$fichier$name
    ext       <- tolower(tools::file_ext(orig))
    base_name <- tools::file_path_sans_ext(orig)
    paste0(base_name, "_traitee_", Sys.Date(), ".", ext)
  },
  content = function(file) {
    df  <- data_actuelle()
    ext <- tolower(tools::file_ext(input$fichier$name))
    switch(ext,
      "csv"  = write.csv(df,   file, row.names = FALSE),
      "xlsx" = {
        if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
        writexl::write_xlsx(df, file)
      },
      "xls"  = {
        if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
        writexl::write_xlsx(df, file)
      },
      "dta"  = haven::write_dta(df, file),
      "sav"  = haven::write_sav(df, file),
      stop("Format non supporté : ", ext)
    )
  }
)

}



# Lancer l’application
shinyApp(ui, server)

# (FIN DU FICHIER – SAUT DE LIGNE CI‑DESSOUS)
