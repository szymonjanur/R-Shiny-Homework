library(shiny)
library(ggplot2)
library(rpart)
library(pROC)
library(ROCR)
library(readr)
library(rsconnect)

# Wczytanie danych
d <- read.csv("friance.csv", sep=',')
d$EstimatedSalary <- as.numeric(gsub("\\s", "", as.character(d$EstimatedSalary)))

# Lista zmiennych do wykluczenia
exclude_vars_roc <- c("EstimatedSalary", "Exited", "Surname_tfidf_0", "Surname_tfidf_1", "Surname_tfidf_2", "Surname_tfidf_3", "Surname_tfidf_4", "Cred_Bal_Sal", "Bal_sal", "Tenure_Age", "Age_Tenure_product")
exclude_vars_hist_density <- c("Surname_tfidf_0", "Surname_tfidf_1", "Surname_tfidf_2", "Surname_tfidf_3", "Surname_tfidf_4", "Cred_Bal_Sal", "Bal_sal", "Tenure_Age", "Age_Tenure_product")

# Losowe kolory
random_colors <- sample(colors(), length(exclude_vars_hist_density))

# Definicja UI
ui <- fluidPage(
  titlePanel("Estymacja ryzyka kredytowego"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stat_method", "Wybierz metodę statystyczną:", 
                  choices = c("Histogram", "Density Plot", "Boxplot", "Krzywa ROC", "Scatter Plot")),
      conditionalPanel(
        condition = "input.stat_method != 'Krzywa ROC' && input.stat_method != 'Density Plot'&& input.stat_method != 'Histogram'&& input.stat_method != 'Scatter Plot'",
        radioButtons("kolumna", "Wybierz kolumnę", choices = setdiff(names(d), exclude_vars_roc))
      ),
      conditionalPanel(
        condition = "input.stat_method == 'Histogram'",
        sliderInput("ilosc_przedzialow", "Ilość przedziałów", min = 1, max = 200, value = 10),
        radioButtons("kolumna_hist", "Wybierz kolumnę", choices = setdiff(names(d), exclude_vars_hist_density))
      ),
      conditionalPanel(
        condition = "input.stat_method == 'Density Plot'",
        checkboxGroupInput("kolumny_density", "Wybierz kolumny do Density Plot", choices = setdiff(names(d), exclude_vars_hist_density))
      ),
      conditionalPanel(
        condition = "input.stat_method == 'Krzywa ROC'",
        radioButtons("metoda_roc", "Wybierz metodę:",
                     choices = c("glm", "tree")),
        checkboxGroupInput("kolumny_roc_tree", "Wybierz kolumny dla Krzywej ROC i modelu drzewa", choices = setdiff(names(d), exclude_vars_roc)),
        sliderInput("sample_size", "Wielkość próby:", min = 10, max = 10000, value = 1000),
        sliderInput("test_train_ratio", "Stosunek testowych do treningowych:", min = 1, max = 99, value = 50)
      ),
      conditionalPanel(
        condition = "input.stat_method == 'Scatter Plot'",
        radioButtons("kolumna_x", "Wybierz zmienną X:", choices = setdiff(names(d), exclude_vars_roc)),
        radioButtons("kolumna_y", "Wybierz zmienną Y:", choices = setdiff(names(d), exclude_vars_roc)),
        radioButtons("kolumna_color", "Wybierz kolor:", choices = c("Exited", "France", "Germany", "Spain", "Female", "Male")),
        sliderInput("sample_size_scatter", "Liczba punktów do wyświetlenia:", min = 10, max = 1000, value = 100)
      ),
      actionButton("generuj_wykres", "Generuj Wykres")
    ),
    mainPanel(
      plotOutput("wykres"),
      plotOutput("krzywa_roc"),
      plotOutput("scatter_plot")
    )
  )
)

# Definicja serwera
server <- function(input, output, session) {
  
  dane_subset <- reactive({
    d
  })
  
  output$wykres <- renderPlot({
    if (input$generuj_wykres > 0) {
      switch(input$stat_method,
             "Histogram" = ggplot(dane_subset(), aes(x = get(input$kolumna_hist))) +
               geom_histogram(binwidth = (max(d[[input$kolumna_hist]]) - min(d[[input$kolumna_hist]])) / input$ilosc_przedzialow, fill = "blue", color = "black", alpha = 0.7) +
               labs(title = paste("Histogram dla kolumny", input$kolumna_hist),
                    x = input$kolumna_hist,
                    y = "Liczba obserwacji"),
             "Density Plot" = {
               plots <- lapply(seq_along(input$kolumny_density), function(i) {
                 ggplot(dane_subset(), aes(x = get(input$kolumny_density[i]))) +
                   geom_density(fill = random_colors[i], alpha = 0.7) +
                   labs(title = paste("Density Plot dla kolumny", input$kolumny_density[i]),
                        x = input$kolumny_density[i],
                        y = "Gęstość")
               })
               do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
             },
             "Boxplot" = ggplot(dane_subset(), aes(x = as.factor(Exited), y = get(input$kolumna))) +
               geom_boxplot() +
               labs(title = paste("Boxplot dla kolumny", input$kolumna),
                    x = "Exited",
                    y = input$kolumna)
      )
    }
  })
  
  output$krzywa_roc <- renderPlot({
    if (input$stat_method == 'Krzywa ROC') {
      selected_vars <- input$kolumny_roc_tree
      sample_size <- input$sample_size
      test_train_ratio <- input$test_train_ratio / 100  # Divide by 100 to convert percentage to a decimal
      test_size <- round(sample_size * test_train_ratio)
      train_size <- sample_size - test_size
      
      # Generate random indices for the entire dataset
      all_indices <- sample(1:nrow(dane_subset()), size = nrow(dane_subset()))
      
      # Split the indices into training and testing sets
      train_indices <- all_indices[1:train_size]
      test_indices <- all_indices[(train_size + 1):(train_size + test_size)]
      
      # Extract the corresponding rows for training and testing sets
      train_data <- dane_subset()[train_indices, ]
      test_data <- dane_subset()[test_indices, ]
      
      if (input$metoda_roc == "glm") {
        glm_formula <- as.formula(paste("Exited ~", paste(selected_vars, collapse = " + ")))
        glm_model <- glm(glm_formula, data = train_data, family = 'binomial')
        predicted_prob <- predict(glm_model, test_data, type = "response")
      } else if (input$metoda_roc == "tree") {
        tree_formula <- as.formula(paste("Exited ~", paste(selected_vars, collapse = " + ")))
        tree_model <- rpart(tree_formula, data = train_data, control = rpart.control(cp = 0.01, minsplit = 10, minbucket = 5))
        predicted_prob <- predict(tree_model, test_data, type = "vector")
      }
      
      pred <- prediction(predicted_prob, test_data$Exited)
      perf <- performance(pred, "tpr", "fpr")
      
      plot_output <- plot(perf, col = "blue", main = "Krzywa ROC", lwd = 2, cex.lab = 1.2, cex.main = 1.4)
      abline(a = 0, b = 1, lty = 2, col = "red")
      legend("bottomright", legend = c("Model", "Losowy"), col = c("blue", "red"), lty = 1:2, cex = 0.8)
      
      return(plot_output)
    }
  })
  
  
  output$scatter_plot <- renderPlot({
    if (input$stat_method == 'Scatter Plot') {
      n <- input$sample_size_scatter
      n <- min(n, nrow(dane_subset()))  # Wybieramy minimum z liczby wybranych punktów i maksymalnej liczby rekordów
      sample_indices <- sample(1:nrow(dane_subset()), size = n)  # Losowo wybieramy indeksy n punktów
      
      ggplot(dane_subset()[sample_indices, ], aes_string(x = input$kolumna_x, y = input$kolumna_y, color = input$kolumna_color)) +
        geom_point(size = 3) +
        scale_color_gradient2(low = "red", mid = "white", high = "green", midpoint = 0.5) +
        labs(title = "Scatter Plot", x = input$kolumna_x, y = input$kolumna_y, color = input$kolumna_color) +
        xlim(0, max(d[[input$kolumna_x]])) + ylim(0, max(d[[input$kolumna_y]]))
    }
  })
  
}

# Uruchomienie aplikacji Shiny
shinyApp(ui, server)

