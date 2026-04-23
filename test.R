library(shiny)
library(shinydashboard)
library(ggplot2)
library(lmtest)
library(plotly)
library(FactoMineR)
library(factoextra)
library(shinythemes)
library(DT)
library(dplyr)
library(rstatix)
library(ggpubr)

# Interface utilisateur (UI)
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      tags$img(src = "logo.jpg", height = 30, width = 30, style = "margin-right: 200px;"), # Logo
      "DataVision" # Titre
    ),
    titleWidth = 300,
    tags$li(class = "dropdown", style = "padding: 8px;",
            tags$style(".main-header .logo { font-family: 'Georgia', serif; font-size: 24px; }")
    )
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Statistiques Descriptives", tabName = "descriptive_stats", icon = icon("chart-bar")),
      menuItem("Régression", tabName = "regression", icon = icon("chart-line")),
      menuItem("ANOVA", tabName = "anova", icon = icon("chart-bar")),
      menuItem("ACP", tabName = "acp", icon = icon("chart-pie"),
               menuSubItem("Résultats ACP", tabName = "acp_results"),
               menuSubItem("Graphiques ACP", tabName = "acp_graphs")),
      menuItem("Clustering", tabName = "clustering", icon = icon("object-group"))
    ),
    tags$style(HTML("
      .sidebar {
        background-color: #34495E !important;
        color: white !important;
      }
      .sidebar-menu li a {
        color: white !important;
        font-size: 16px;
      }
      .sidebar-menu li a:hover {
        background-color: #1ABC9C !important;
      }
    "))
  ),
  
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      # Page d'accueil
      tabItem(
        tabName = "accueil",
        fluidPage(
          id='accueil',
          div(
            style = "text-align: center; margin-top: 50px;",
            h1(
              style = "font-size: 50px; color: white; font-family: 'Georgia', serif;",
              "DataVision"
            ),
            h1(
              style = "font-size: 36px; color: white; font-family: 'Georgia', serif;",
              "Bienvenue dans l'application d'analyse de données"
            ),
            div(style = " margin-top: 200px;"),
            br(),
            
          ),
          
          # Section sur les fonctionnalités
          fluidRow(
            column(
              width = 4,
              div(
                style = "text-align: center; background-color: white",
                icon("chart-bar", class = "fa-3x", style = "color: #E74C3C;"),
                h4("Analyses Statistiques"),
                p("Effectuez des tests statistiques avancés comme l'ANOVA, les tests post-hoc et bien plus.")
              )
            ),
            column(
              width = 4,
              div(
                style = "text-align: center;background-color: white;",
                icon("table", class = "fa-3x", style = "color: #27AE60;"),
                h4("Visualisation de Données"),
                p("Générez des graphiques interactifs pour mieux comprendre vos données.")
              )
            ),
            column(
              width = 4,
              div(
                style = "text-align: center;background-color: white;",
                icon("cogs", class = "fa-3x", style = "color: #F39C12;"),
                h4("Outils d'Exploration"),
                p("Utilisez des techniques d’ACP et de clustering pour explorer vos données.")
              )
            )
          ),
          
          br(),
          
          # Pied de page
          div(
            style = "background-color: #34495E !important; color: white; text-align: center; padding: 10px; margin-top: 50px; font-size: 14px;",
            hr(style = "border-color: rgba(255, 255, 255, 0.2); width: 80%;"),
            p("© ", format(Sys.Date(), "%Y"), " FST Béni Mellal | Intelligence Artificielle & Informatique Digitale"),
            p(
              "Contact : ",
              a("LinkedIn", href = "https://www.linkedin.com/in/nada-hjiouaj", target = "_blank", style = "color: #1ABC9C; text-decoration: none;"),
              " | ",
              a("Email", href = "mailto:nada.hjiouaj@example.com", style = "color: #1ABC9C; text-decoration: none;")
            )
          )
        )
      ),
      
      # Page Statistiques Descriptives
      tabItem(
        tabName = "descriptive_stats",
        fluidPage(
          h2("Statistiques Descriptives"),
          uiOutput("var_select_descriptive"),
          actionButton("run_descriptive", "Calculer les Statistiques", class = "btn-primary"),
          br(), br(),
          
          # Section pour les variables quantitatives
          conditionalPanel(
            condition = "output.is_quantitative",
            fluidRow(
              column(width = 6,
                     h4("Résultats Numériques (Quantitatives)"),
                     verbatimTextOutput("descriptive_summary")
              ),
              column(width = 6,
                     h4("Histogramme (Quantitatives)"),
                     plotlyOutput("descriptive_histogram")
              )
            ),
            fluidRow(
              column(width = 6,
                     h4("Box Plot (Quantitatives)"),
                     plotlyOutput("descriptive_boxplot")
              ),
              column(width = 6,
                     h4("Quartiles et Extrêmes (Quantitatives)"),
                     verbatimTextOutput("descriptive_quartiles")
              )
            )
          ),
          
          # Section pour les variables qualitatives
          conditionalPanel(
            condition = "output.is_qualitative",
            fluidRow(
              column(width = 6,
                     h4("Tableau d'Effectifs et Fréquences (Qualitatives)"),
                     DTOutput("qualitative_table")
              ),
              column(width = 6,
                     h4("Diagramme en Barres (Qualitatives)"),
                     plotlyOutput("qualitative_barplot")
              )
            ),
            fluidRow(
              column(width = 6,
                     h4("Diagramme Circulaire (Qualitatives)"),
                     plotlyOutput("qualitative_piechart")
              ),
              column(width = 6,
                     h4("Courbe Cumulative (Ordinale)"),
                     plotlyOutput("qualitative_cumulative_curve")
              )
            )
          )
        )
      ),
      
      # Page Régression
      tabItem(tabName = "regression",
              fluidPage(
                h2("Analyse de Régression"),
                uiOutput("var_select_reg"),
                actionButton("btn_graphiques", "Afficher les Graphiques", class = "btn-primary"),
                actionButton("btn_tests", "Afficher les Tests", class = "btn-primary"),
                uiOutput("dynamic_content_reg")
              )
      ),
      
      # Page ANOVA
      tabItem(tabName = "anova",
              fluidPage(
                h2("Analyse ANOVA"),
                radioButtons("anova_type", "Type d'ANOVA", 
                             choices = list("ANOVA à un facteur" = "anova1", "ANOVA à deux facteurs" = "anova2"),
                             inline = TRUE),
                uiOutput("var_select_anova"),  # Interface dynamique pour sélectionner les variables
                actionButton("run_anova", "Exécuter l'ANOVA", icon = icon("play")),
                box(title = "Résultats de l'ANOVA", width = 12, solidHeader = TRUE, status = "primary",
                    verbatimTextOutput("anova_result")),
                box(title = "Graphique ANOVA", width = 12, solidHeader = TRUE, status = "info",
                    plotOutput("anova_plot"))
              )
      )
      ,
      # Page ACP
      tabItem(tabName = "acp_results",
              fluidPage(
                h2("Analyse en Composantes Principales"),
                uiOutput("var_select_acp"),
                actionButton("run_acp", "Lancer l'ACP"),
                DTOutput("data_preview"),
                h3("Aperçu des données"),
                DTOutput("data_preview"),
                h3("Test d'adéquation à l'ACP"),
                verbatimTextOutput("kmo_test"),
                verbatimTextOutput("bartlett_test"),
                h3("Résumé ACP"),
                DTOutput("summary_acp"),
                h3("Matrice des Composantes Principales"),
                DTOutput("cp_matrix"),
                h3("Qualité de représentation (cos²)"),
                DTOutput("cos2_table"),
                h3("Contribution des variables"),
                DTOutput("var_contrib"),
                h3("Contribution des individus"),
                DTOutput("ind_contrib"),
                h3("Matrice de covariance"),
                DTOutput("cov_matrix")
              )
      ),
      tabItem(tabName = "acp_graphs",
              fluidPage(
                h2("Graphiques ACP"),
                plotlyOutput("acp_plot"),
                plotlyOutput("acp_circle"),
                plotlyOutput("scree_plot"),
                plotlyOutput("biplot")
              )
      ),
      
      # Page Clustering
      tabItem(tabName = "clustering",
              fluidPage(
                h2("Clustering K-means"),
                numericInput("k", "Nombre de clusters (K)", value = 3, min = 2, max = 10),
                div(class = "plot-container",
                    plotlyOutput("clustering_plot")
                ),
                div(style = "overflow-x: auto; max-height: 400px;",
                    uiOutput("cluster_info_ui"),
                    tableOutput("age_summary"),
                    plotOutput("age_distribution"),
                    plotOutput("severity_plot"),
                    
                    
                )
              )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Réactive globale pour charger les données
  global_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Bouton de téléchargement personnalisé avec icône
  output$file_input <- renderUI({
    tags$div(
      style = "padding: 10px;",
      tags$label(
        class = "btn btn-primary",
        style = "background-color: #1ABC9C; border-color: #1ABC9C;",
        tags$span(icon("download"), "Choisir un fichier CSV"),
        tags$input(
          id = "file",
          type = "file",
          accept = ".csv",
          style = "display: none;"
        )
      ),
      tags$script(HTML("
        $(document).on('change', '#file', function() {
          var file = this.files[0];
          if (file) {
            $('#file_name').text(file.name);
          } else {
            $('#file_name').text('Aucun fichier sélectionné');
          }
        });
      ")),
      tags$div(id = "file_name", style = "margin-top: 20px; color: #34495E; font-weight: bold;", "Aucun fichier sélectionné")
    )
  })
  
  # Afficher le fileInput dans la barre latérale
  insertUI(
    selector = ".sidebar",
    where = "beforeEnd",
    ui = uiOutput("file_input")
  )
  
  # Statistiques Descriptives
  # Sélection des variables pour les statistiques descriptives
  output$var_select_descriptive <- renderUI({
    req(global_data())
    df <- global_data()
    all_vars <- names(df)
    selectInput("selected_var", "Choisissez une variable:", choices = all_vars)
  })
  
  # Déterminer si la variable sélectionnée est quantitative ou qualitative
  output$is_quantitative <- reactive({
    req(global_data(), input$selected_var)
    df <- global_data()
    is.numeric(df[[input$selected_var]])
  })
  
  output$is_qualitative <- reactive({
    req(global_data(), input$selected_var)
    df <- global_data()
    is.factor(df[[input$selected_var]]) || is.character(df[[input$selected_var]])
  })
  
  outputOptions(output, "is_quantitative", suspendWhenHidden = FALSE)
  outputOptions(output, "is_qualitative", suspendWhenHidden = FALSE)
  
  # Statistiques descriptives pour les variables quantitatives
  observeEvent(input$run_descriptive, {
    req(global_data(), input$selected_var)
    df <- global_data()
    selected_var <- input$selected_var
    
    if (is.numeric(df[[selected_var]])) {
      # Calcul des statistiques pour les variables quantitatives
      mean_val <- mean(df[[selected_var]], na.rm = TRUE)
      sd_val <- sd(df[[selected_var]], na.rm = TRUE)
      var_val <- var(df[[selected_var]], na.rm = TRUE)
      min_val <- min(df[[selected_var]], na.rm = TRUE)
      max_val <- max(df[[selected_var]], na.rm = TRUE)
      quartiles <- quantile(df[[selected_var]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      
      # Affichage des résultats
      output$descriptive_summary <- renderPrint({
        cat("Moyenne:", mean_val, "\n")
        cat("Écart-type:", sd_val, "\n")
        cat("Variance:", var_val, "\n")
        cat("Minimum:", min_val, "\n")
        cat("Maximum:", max_val, "\n")
      })
      
      output$descriptive_quartiles <- renderPrint({
        cat("Premier Quartile (Q1):", quartiles[1], "\n")
        cat("Médiane (Q2):", quartiles[2], "\n")
        cat("Troisième Quartile (Q3):", quartiles[3], "\n")
      })
      
      # Histogramme
      output$descriptive_histogram <- renderPlotly({
        p <- ggplot(df, aes_string(x = selected_var)) +
          geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
          labs(title = "Histogramme de la variable sélectionnée")
        ggplotly(p)
      })
      
      # Box Plot
      output$descriptive_boxplot <- renderPlotly({
        p <- ggplot(df, aes_string(y = selected_var)) +
          geom_boxplot(fill = "lightgreen", color = "black") +
          labs(title = "Box Plot de la variable sélectionnée")
        ggplotly(p)
      })
    }
  })
  
  # Statistiques descriptives pour les variables qualitatives
  observeEvent(input$run_descriptive, {
    req(global_data(), input$selected_var)
    df <- global_data()
    selected_var <- input$selected_var
    
    if (is.factor(df[[selected_var]]) || is.character(df[[selected_var]])) {
      # Tableau d'effectifs et de fréquences
      freq_table <- df %>%
        group_by(!!sym(selected_var)) %>%
        summarise(Effectif = n()) %>%
        mutate(Fréquence = Effectif / sum(Effectif))
      
      output$qualitative_table <- renderDT({
        datatable(freq_table)
      })
      
      # Diagramme en barres
      output$qualitative_barplot <- renderPlotly({
        p <- ggplot(freq_table, aes_string(x = selected_var, y = "Effectif")) +
          geom_bar(stat = "identity", fill = "lightblue") +
          labs(title = "Diagramme en Barres", x = selected_var, y = "Effectif")
        ggplotly(p)
      })
      # Diagramme en Bâtons pour les variables quantitatives discrètes
      output$discrete_barplot <- renderPlotly({
        req(global_data(), input$selected_var)
        df <- global_data()
        selected_var <- input$selected_var
        
        # Vérifier si la variable est discrète
        if (is.numeric(df[[selected_var]]) && length(unique(df[[selected_var]])) <= 20) {
          # Créer un diagramme en bâtons
          freq_table <- df %>%
            group_by(!!sym(selected_var)) %>%
            summarise(Effectif = n())
          
          p <- ggplot(freq_table, aes_string(x = selected_var, y = "Effectif")) +
            geom_bar(stat = "identity", fill = "lightblue", color = "black") +
            labs(title = "Diagramme en Bâtons", x = selected_var, y = "Effectif")
          ggplotly(p)
        } else {
          return(NULL) # Ne rien afficher si la variable n'est pas discrète
        }
      })
      # Diagramme circulaire
      output$qualitative_piechart <- renderPlotly({
        req(global_data(), input$selected_var)
        df <- global_data()
        selected_var <- input$selected_var
        
        # Create a frequency table for the selected qualitative variable
        freq_table <- df %>%
          group_by(!!sym(selected_var)) %>%
          summarise(Effectif = n()) %>%
          mutate(Fréquence = Effectif / sum(Effectif))
        
        # Use plot_ly to create the pie chart
        plot_ly(freq_table, labels = ~get(selected_var), values = ~Effectif, type = 'pie') %>%
          layout(title = "Diagramme Circulaire")
      })
      
      # Courbe cumulative (pour les variables ordinales)
      if (is.ordered(df[[selected_var]])) {
        output$qualitative_cumulative_curve <- renderPlotly({
          p <- ggplot(df, aes_string(x = selected_var)) +
            stat_ecdf(geom = "step", color = "blue") +
            labs(title = "Courbe Cumulative", x = selected_var, y = "Fréquence Cumulée")
          ggplotly(p)
        })
      } else {
        output$qualitative_cumulative_curve <- renderPlotly(NULL)
      }
    }
  })
  
  # Régression
  observeEvent(global_data(), {
    req(global_data())
    df <- global_data()
    all_vars <- names(df)
    
    output$var_select_reg <- renderUI({
      tagList(
        selectInput("ind_var_reg", "Variable indépendante:", choices = all_vars, selected = all_vars[1]),
        selectInput("dep_var_reg", "Variable dépendante:", choices = all_vars, selected = all_vars[2])
      )
    })
  })
  
  # Variables d'affichage pour activer les graphiques/tests pour la régression
  display_state_reg <- reactiveValues(
    graphiques = FALSE,
    tests = FALSE
  )
  
  observeEvent(input$btn_graphiques, {
    display_state_reg$graphiques <- TRUE
    display_state_reg$tests <- FALSE
  })
  
  observeEvent(input$btn_tests, {
    display_state_reg$tests <- TRUE
    display_state_reg$graphiques <- FALSE
  })
  
  # Contenu dynamique pour la régression
  output$dynamic_content_reg <- renderUI({
    if (display_state_reg$graphiques) {
      tagList(
        h3("Représentations Graphiques"),
        plotlyOutput("regression_plot"),
        plotlyOutput("residuals_plot"),
        plotlyOutput("residuals_histogram")
      )
    } else if (display_state_reg$tests) {
      tagList(
        h3("Tests Statistiques"),
        verbatimTextOutput("shapiro_test"),
        verbatimTextOutput("breusch_pagan_test"),
        verbatimTextOutput("durbin_watson_test"),
        verbatimTextOutput("interpretation_tests")
      )
    }
  })
  
  # Modèle de régression réactif
  model_reg <- reactive({
    req(global_data(), input$ind_var_reg, input$dep_var_reg)
    df <- global_data()
    formula <- as.formula(paste(input$dep_var_reg, "~", input$ind_var_reg))
    lm(formula, data = df)
  })
  
  # Graphiques et tests pour la régression
  observe({
    req(model_reg())
    
    output$regression_plot <- renderPlotly({
      df <- global_data()
      p <- ggplot(df, aes_string(x = input$ind_var_reg, y = input$dep_var_reg)) +
        geom_point(color = "blue") +
        geom_smooth(method = "lm", color = "red") +
        labs(title = "Graphique de Régression")
      ggplotly(p)
    })
    
    output$residuals_plot <- renderPlotly({
      model <- model_reg()
      p <- ggplot(data.frame(fitted = fitted(model), residuals = resid(model)), aes(x = fitted, y = residuals)) +
        geom_point(color = "blue") +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Résidus vs Valeurs prédites")
      ggplotly(p)
    })
    
    output$residuals_histogram <- renderPlotly({
      model <- model_reg()
      p <- ggplot(data.frame(residuals = resid(model)), aes(x = residuals)) +
        geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
        labs(title = "Histogramme des Résidus")
      ggplotly(p)
    })
  })
  
  observe({
    req(model_reg())
    model <- model_reg()
    
    shapiro_result <- shapiro.test(resid(model))
    breusch_pagan_result <- bptest(model)
    durbin_watson_result <- dwtest(model)
    
    output$shapiro_test <- renderPrint({ shapiro_result })
    output$breusch_pagan_test <- renderPrint({ breusch_pagan_result })
    output$durbin_watson_test <- renderPrint({ durbin_watson_result })
    
    output$interpretation_tests <- renderPrint({
      cat("Interprétation des tests:\n")
      cat("- Test de Shapiro-Wilk: p-value =", shapiro_result$p.value, "\n")
      if (shapiro_result$p.value < 0.05) {
        cat("  -> Les résidus ne suivent pas une distribution normale.\n")
      } else {
        cat("  -> Les résidus suivent une distribution normale.\n")
      }
      cat("- Test de Breusch-Pagan: p-value =", breusch_pagan_result$p.value, "\n")
      if (breusch_pagan_result$p.value < 0.05) {
        cat("  -> Présence d'hétéroscédasticité.\n")
      } else {
        cat("  -> Pas d'hétéroscédasticité détectée.\n")
      }
      cat("- Test de Durbin-Watson: p-value =", durbin_watson_result$p.value, "\n")
      if (durbin_watson_result$p.value < 0.05) {
        cat("  -> Présence d'autocorrélation des erreurs.\n")
      } else {
        cat("  -> Pas d'autocorrélation détectée.\n")
      }
    })
  })
  
  # ANOVA
  # ANOVA
  observeEvent(global_data(), {
    req(global_data())
    df <- global_data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    factor_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    output$var_select_anova <- renderUI({
      tagList(
        selectInput("dependent_var", "Variable dépendante", choices = numeric_vars),
        selectInput("factor1", "Facteur 1", choices = factor_vars),
        conditionalPanel(
          condition = "input.anova_type == 'anova2'",
          selectInput("factor2", "Facteur 2", choices = factor_vars)
        )
      )
    })
  })
  
  observeEvent(input$run_anova, {
    req(global_data(), input$dependent_var, input$factor1)
    
    df <- global_data()
    df[[input$factor1]] <- as.factor(df[[input$factor1]])
    
    if (input$anova_type == "anova1") {
      data <- df %>% select(all_of(c(input$dependent_var, input$factor1)))
      colnames(data) <- c("score", "group")
      data$group <- as.factor(data$group)
      
      # Tests statistiques pour ANOVA à un facteur
      output$anova_result <- renderPrint({
        # 1. Statistiques descriptives
        cat("=== Statistiques descriptives ===\n")
        print(data %>% group_by(group) %>% get_summary_stats(score, type = "mean_sd"))
        
        # 2. Test de normalité (Shapiro-Wilk)
        cat("\n=== Test de normalité (Shapiro-Wilk) ===\n")
        print(data %>% group_by(group) %>% shapiro_test(score))
        
        # 3. Test d'homogénéité des variances (Bartlett et Levene)
        cat("\n=== Test d'homogénéité des variances ===\n")
        bartlett_res <- bartlett.test(score ~ group, data = data)
        levene_res <- data %>% levene_test(score ~ group)
        print(list("Bartlett Test" = bartlett_res, "Levene Test" = levene_res))
        
        # 4. ANOVA
        cat("\n=== Résultat de l'ANOVA ===\n")
        anova_res <- aov(score ~ group, data = data)
        print(summary(anova_res))
        
        # 5. Test post-hoc de Tukey
        cat("\n=== Test post-hoc de Tukey ===\n")
        print(data %>% tukey_hsd(score ~ group))
        
        # 6. Normalité des résidus
        cat("\n=== Normalité des résidus ===\n")
        mdl <- lm(score ~ group, data = data)
        shapiro_res <- shapiro_test(residuals(mdl))
        print(shapiro_res)
        
        # 7. Tests non paramétriques (si les résidus ne sont pas normaux)
        if (shapiro_res$p.value < 0.05) {
          cat("\n=== Les résidus ne sont pas normaux. Utilisation de tests non paramétriques ===\n")
          cat("\n=== Test de Kruskal-Wallis ===\n")
          print(data %>% kruskal_test(score ~ group))
          cat("\n=== Taille de l'effet (Kruskal-Wallis) ===\n")
          print(data %>% kruskal_effsize(score ~ group))
          cat("\n=== Test post-hoc de Dunn ===\n")
          print(data %>% dunn_test(score ~ group, p.adjust.method = "bonferroni"))
        }
        
        # 8. Tests alternatifs si les variances ne sont pas homogènes
        if (bartlett_res$p.value < 0.05) {
          cat("\n=== Les variances ne sont pas homogènes. Utilisation de tests alternatifs ===\n")
          cat("\n=== Test de Games-Howell ===\n")
          print(data %>% games_howell_test(score ~ group))
        }
      })
      
      # Graphique ANOVA
      output$anova_plot <- renderPlot({
        ggboxplot(data, x = "group", y = "score") +
          stat_pvalue_manual(data %>% tukey_hsd(score ~ group) %>% add_xy_position(x = "group"), hide.ns = TRUE)
      })
      
    } else if (input$anova_type == "anova2") {
      req(input$factor2)
      df[[input$factor2]] <- as.factor(df[[input$factor2]])
      
      data <- df %>% select(all_of(c(input$dependent_var, input$factor1, input$factor2)))
      colnames(data) <- c("score", "factor1", "factor2")
      data <- data %>% mutate(interaction = interaction(factor1, factor2))  # Créer la colonne interaction
      
      # Tests statistiques pour ANOVA à deux facteurs
      output$anova_result <- renderPrint({
        # 1. Statistiques descriptives
        cat("=== Statistiques descriptives ===\n")
        print(data %>% group_by(factor1, factor2) %>% get_summary_stats(score, type = "mean_sd"))
        
        # 2. Test de normalité (Shapiro-Wilk)
        cat("\n=== Test de normalité (Shapiro-Wilk) ===\n")
        print(data %>% group_by(factor1, factor2) %>% shapiro_test(score))
        
        # 3. Test d'homogénéité des variances (Bartlett et Levene)
        cat("\n=== Test d'homogénéité des variances ===\n")
        bartlett_res <- bartlett.test(score ~ interaction(factor1, factor2), data = data)
        levene_res <- data %>% levene_test(score ~ interaction(factor1, factor2))
        print(list("Bartlett Test" = bartlett_res, "Levene Test" = levene_res))
        
        # 4. ANOVA à deux facteurs
        cat("\n=== Résultat de l'ANOVA à deux facteurs ===\n")
        anova_res <- aov(score ~ factor1 * factor2, data = data)
        print(summary(anova_res))
        
        # 5. Test post-hoc de Tukey
        cat("\n=== Test post-hoc de Tukey ===\n")
        print(data %>% tukey_hsd(score ~ interaction(factor1, factor2)))
        
        # 6. Normalité des résidus
        cat("\n=== Normalité des résidus ===\n")
        mdl <- lm(score ~ factor1 * factor2, data = data)
        shapiro_res <- shapiro_test(residuals(mdl))
        print(shapiro_res)
        
        # 7. Tests non paramétriques (si les résidus ne sont pas normaux)
        if (shapiro_res$p.value < 0.05) {
          cat("\n=== Les résidus ne sont pas normaux. Utilisation de tests non paramétriques ===\n")
          cat("\n=== Test de Kruskal-Wallis ===\n")
          print(data %>% kruskal_test(score ~ interaction(factor1, factor2)))
          cat("\n=== Taille de l'effet (Kruskal-Wallis) ===\n")
          print(data %>% kruskal_effsize(score ~ interaction(factor1, factor2)))
          cat("\n=== Test post-hoc de Dunn ===\n")
          print(data %>% dunn_test(score ~ interaction(factor1, factor2), p.adjust.method = "bonferroni"))
        }
        
        # 8. Tests alternatifs si les variances ne sont pas homogènes
        if (bartlett_res$p.value < 0.05) {
          cat("\n=== Les variances ne sont pas homogènes. Utilisation de tests alternatifs ===\n")
          cat("\n=== Test de Games-Howell ===\n")
          print(data %>% games_howell_test(score ~ interaction(factor1, factor2)))
        }
      })
      
      # Graphique ANOVA à deux facteurs
      output$anova_plot <- renderPlot({
        ggboxplot(data, x = "factor1", y = "score", color = "factor2") +
          stat_pvalue_manual(data %>% tukey_hsd(score ~ interaction) %>% add_xy_position(x = "factor1"), hide.ns = TRUE)
      })
    }
  })
  
  # ACP
  output$data_preview <- renderDT({
    req(global_data())
    datatable(global_data())
  })
  output$var_select_acp <- renderUI({
    req(global_data())
    checkboxGroupInput("selected_vars_acp", "Choisissez les variables pour l'ACP:", 
                       choices = names(global_data()), selected = names(global_data()))
  })
  pca_result <- eventReactive(input$run_acp, {
    req(input$selected_vars_acp)
    df_selected <- global_data()[, input$selected_vars_acp, drop = FALSE]
    df_numeric <- df_selected[, sapply(df_selected, is.numeric)]
    PCA(df_numeric, scale.unit = TRUE, graph = FALSE)
  })
  # Tests d'adéquation à l'ACP
  output$kmo_test <- renderPrint({
    req(input$selected_vars_acp)
    df_selected <- global_data()[, input$selected_vars_acp, drop = FALSE]
    df_numeric <- df_selected[, sapply(df_selected, is.numeric)]
    kmo_result <- KMO(df_numeric)
    cat("Indice KMO :", kmo_result$MSA, "\n")
    if (kmo_result$MSA >= 0.6) {
      cat("L'ACP est adaptée aux données.\n")
    } else {
      cat("L'ACP peut ne pas être appropriée (KMO < 0.6).\n")
    }
  })
  
  output$bartlett_test <- renderPrint({
    req(input$selected_vars_acp)
    df_selected <- global_data()[, input$selected_vars_acp, drop = FALSE]
    df_numeric <- df_selected[, sapply(df_selected, is.numeric)]
    bartlett_result <- cortest.bartlett(cor(df_numeric), n = nrow(df_numeric))
    cat("Test de Bartlett : p-value =", bartlett_result$p.value, "\n")
    if (bartlett_result$p.value < 0.05) {
      cat("L'ACP est pertinente.\n")
    } else {
      cat("Les variables ne sont pas suffisamment corrélées.\n")
    }
  })
  
  output$summary_acp <- renderDT({
    req(pca_result())
    datatable(as.data.frame(summary(pca_result())))
  })
  
  output$cp_matrix <- renderDT({
    req(pca_result())
    datatable(as.data.frame(pca_result()$ind$coord))
  })
  
  output$cos2_table <- renderDT({
    req(pca_result())
    datatable(as.data.frame(pca_result()$ind$cos2))
  })
  
  output$var_contrib <- renderDT({
    req(pca_result())
    datatable(as.data.frame(pca_result()$var$contrib))
  })
  
  output$ind_contrib <- renderDT({
    req(pca_result())
    datatable(as.data.frame(pca_result()$ind$contrib))
  })
  
  # Matrice de covariance
  output$cov_matrix <- renderDT({
    req(input$selected_vars_acp)
    df_selected <- global_data()[, input$selected_vars_acp, drop = FALSE]
    df_numeric <- df_selected[, sapply(df_selected, is.numeric)]
    cov_matrix <- cov(df_numeric)
    datatable(as.data.frame(cov_matrix), options = list(pageLength = 5))
  })
  
  output$acp_plot <- renderPlotly({
    req(pca_result())
    ggplotly(fviz_pca_ind(pca_result(), geom = "point", col.ind = "cos2"))
  })
  output$acp_circle <- renderPlotly({
    req(pca_result())
    ggplotly(fviz_pca_var(pca_result(), col.var = "contrib", 
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")))
  })
  output$scree_plot <- renderPlotly({
    req(pca_result())
    ggplotly(fviz_eig(pca_result(), addlabels = TRUE, ylim = c(0, 100)))
  })
  output$biplot <- renderPlotly({
    req(pca_result())
    ggplotly(fviz_pca_biplot(pca_result(), repel = TRUE))
  })
  
  # Clustering
  output$clustering_plot <- renderPlotly({
    req(global_data(), input$k)
    df <- global_data()
    
    # Sélectionner uniquement les colonnes numériques
    df_numeric <- df %>% select(where(is.numeric))
    
    # Vérifier qu'il y a suffisamment de colonnes numériques
    if (ncol(df_numeric) < 2) {
      showNotification("Le fichier doit contenir au moins 2 colonnes numériques pour le clustering.", type = "error")
      return(NULL)
    }
    
    # Appliquer le clustering K-means
    set.seed(123)  # Pour la reproductibilité
    kmeans_result <- kmeans(df_numeric, centers = input$k)
    
    # Ajouter les clusters aux données
    df$Cluster <- as.factor(kmeans_result$cluster)
    
    # Générer le graphique de clustering
    p <- fviz_cluster(kmeans_result, data = df_numeric, geom = "point", ellipse.type = "norm")
    
    # Créer un tableau réactif avec les informations détaillées des clusters
    output$cluster_info <- renderTable({
      cluster_summary <- df %>% 
        group_by(Cluster) %>% 
        summarise(
          Count = n(),
          across(where(is.numeric), list(Mean = mean, SD = sd), na.rm = TRUE)
        )
      cluster_summary
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    # Afficher les statistiques détaillées des clusters
    output$cluster_details <- renderPrint({
      list(
        "Centres des clusters" = kmeans_result$centers,
        "Taille des clusters" = kmeans_result$size,
        "Totale Within-cluster sum of squares" = kmeans_result$tot.withinss
      )
    })
    
    ggplotly(p)
  })
  
  # Ajouter une sortie pour afficher les informations détaillées des clusters
  output$cluster_info_ui <- renderUI({
    tagList(
      h3("Informations sur les clusters"),
      tableOutput("cluster_info"),
      verbatimTextOutput("cluster_details")
    )
  })
  #influence de l'age
  output$age_distribution <- renderPlot({
    df <- global_data()
    req(df, input$k)
    
    # Vérifier que la colonne "Age" existe
    if (!"Age" %in% colnames(df)) {
      showNotification("La colonne Age est absente.", type = "error")
      return(NULL)
    }
    
    df_numeric <- df %>% select(Age, Temps_Reaction_ms, Score_Memoire, Score_Fatigue)
    df_scaled <- scale(df_numeric)
    
    set.seed(123)
    kmeans_result <- kmeans(df_scaled, centers = input$k)
    df$Cluster <- as.factor(kmeans_result$cluster)
    
    ggplot(df, aes(x = Cluster, y = Age, fill = Cluster)) +
      geom_boxplot() +
      labs(title = "Distribution de l'Âge par Cluster", y = "Âge", x = "Cluster") +
      theme_minimal()
  })
  
  output$age_summary <- renderTable({
    df <- global_data()
    req(df, input$k)
    
    if (!"Age" %in% colnames(df)) {
      showNotification("La colonne Age est absente.", type = "error")
      return(NULL)
    }
    
    df_numeric <- df %>% select(Age, Temps_Reaction_ms, Score_Memoire, Score_Fatigue)
    df_scaled <- scale(df_numeric)
    
    set.seed(123)
    kmeans_result <- kmeans(df_scaled, centers = input$k)
    df$Cluster <- as.factor(kmeans_result$cluster)
    
    df %>%
      group_by(Cluster) %>%
      summarise(Age_moyen = round(mean(Age, na.rm = TRUE), 2))
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  #impact de la sévérité
  output$severity_plot <- renderPlot({
    df <- global_data()
    req(df, input$k)
    
    if (!"Severite_COVID" %in% colnames(df)) {
      showNotification("La colonne Sévérité_COVID est absente.", type = "error")
      return(NULL)
    }
    
    df_numeric <- df %>% select(Age, Temps_Reaction_ms, Score_Memoire, Score_Fatigue)
    df_scaled <- scale(df_numeric)
    
    set.seed(123)
    kmeans_result <- kmeans(df_scaled, centers = input$k)
    df$Cluster <- as.factor(kmeans_result$cluster)
    
    ggplot(df, aes(x = Cluster, fill = Severite_COVID)) +
      geom_bar(position = "fill") +
      labs(title = "Répartition de la Sévérité du COVID par Cluster", y = "Proportion", x = "Cluster") +
      theme_minimal()
  })
  
}

# Lancer l'application Shiny
shinyApp(ui, server)