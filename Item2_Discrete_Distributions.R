# ============================================================
# ITEM 2: UNIVARIATE AND BIVARIATE DISCRETE DISTRIBUTIONS
# Shiny Front-End Application
# ============================================================

library(shiny)
library(ggplot2)

# ------------------------------------------------------------
# HELPER FUNCTIONS
# ------------------------------------------------------------

parse_numbers <- function(text_input) {
  if (is.null(text_input) ||
      length(text_input) == 0 ||
      !nzchar(trimws(text_input))) {
    stop("Please enter numeric values.")
  }
  
  values <- suppressWarnings(
    as.numeric(
      strsplit(trimws(text_input), "[,;[:space:]]+")[[1]]
    )
  )
  
  if (any(!is.finite(values))) {
    stop("Enter valid numeric values separated by commas.")
  }
  
  values
}

validate_distribution <- function(values, probabilities) {
  
  if (length(values) == 0 ||
      length(values) != length(probabilities)) {
    stop("Each value must have a corresponding probability.")
  }
  
  if (any(!is.finite(values)) ||
      any(!is.finite(probabilities))) {
    stop("All values and probabilities must be finite.")
  }
  
  if (any(probabilities < 0 | probabilities > 1)) {
    stop("Each probability must be between 0 and 1.")
  }
  
  if (abs(sum(probabilities) - 1) > 1e-8) {
    stop("The probabilities must sum to 1.")
  }
  
  if (anyDuplicated(values)) {
    stop("Values must be unique. Combine probabilities for repeated values.")
  }
  
  invisible(TRUE)
}

calculate_mean <- function(values, probabilities) {
  sum(values * probabilities)
}

calculate_variance <- function(values, probabilities) {
  mu <- calculate_mean(values, probabilities)
  sum((values - mu)^2 * probabilities)
}

distribution_table <- function(values, probabilities) {
  order_index <- order(values)
  
  values <- values[order_index]
  probabilities <- probabilities[order_index]
  
  data.frame(
    Value = values,
    Probability = probabilities,
    Cumulative_Probability = cumsum(probabilities)
  )
}

make_distribution_plot <- function(values, probabilities,
                                   plot_type, plot_title) {
  
  table_data <- distribution_table(values, probabilities)
  
  if (plot_type == "PDF") {
    ggplot(table_data,
           aes(x = Value, y = Probability)) +
      geom_col() +
      geom_point(size = 2) +
      labs(
        title = paste(plot_title, "- PDF"),
        x = "Value",
        y = "Probability"
      ) +
      theme_minimal()
    
  } else {
    ggplot(table_data,
           aes(x = Value, y = Cumulative_Probability)) +
      geom_step(direction = "hv") +
      geom_point(size = 2) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(
        title = paste(plot_title, "- CDF"),
        x = "Value",
        y = "Cumulative Probability"
      ) +
      theme_minimal()
  }
}

# ------------------------------------------------------------
# USER INTERFACE
# ------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Discrete Probability Distribution Calculator"),
  
  p("Enter the values and probabilities of discrete random variables.
    All probabilities must be between 0 and 1 and sum to 1."),
  
  tabsetPanel(
    
    # ========================================================
    # UNIVARIATE DISTRIBUTION
    # ========================================================
    
    tabPanel(
      "Univariate",
      
      h3("Univariate Discrete Random Variable"),
      
      textInput(
        "u_values",
        "Values of X (separate with commas)",
        value = "0,1,2,3"
      ),
      
      textInput(
        "u_probabilities",
        "Probabilities P(X=x) (separate with commas)",
        value = "0.1,0.2,0.4,0.3"
      ),
      
      actionButton(
        "calculate_u",
        "Calculate Univariate Distribution"
      ),
      
      h4("Probability Distribution"),
      tableOutput("u_table"),
      
      h4("Mean and Variance"),
      verbatimTextOutput("u_statistics"),
      
      fluidRow(
        column(6, plotOutput("u_pdf")),
        column(6, plotOutput("u_cdf"))
      )
    ),
    
    # ========================================================
    # BIVARIATE DISTRIBUTION
    # ========================================================
    
    tabPanel(
      "Bivariate",
      
      h3("Bivariate Discrete Random Variable"),
      
      p("Enter the values of X and Y. The joint probability
        matrix must contain one row for each Y value and one
        column for each X value."),
      
      textInput(
        "b_x",
        "Values of X (separate with commas)",
        value = "0,1"
      ),
      
      textInput(
        "b_y",
        "Values of Y (separate with commas)",
        value = "0,1"
      ),
      
      textAreaInput(
        "b_matrix",
        "Joint probability matrix (one row per Y value)",
        value = "0.1,0.2\n0.3,0.4",
        rows = 4
      ),
      
      actionButton(
        "calculate_b",
        "Calculate Bivariate Distribution"
      ),
      
      h4("Joint Probability Distribution"),
      tableOutput("joint_table"),
      
      h4("Marginal Distribution of X"),
      tableOutput("marginal_x_table"),
      verbatimTextOutput("marginal_x_stats"),
      
      fluidRow(
        column(6, plotOutput("marginal_x_pdf")),
        column(6, plotOutput("marginal_x_cdf"))
      ),
      
      h4("Marginal Distribution of Y"),
      tableOutput("marginal_y_table"),
      verbatimTextOutput("marginal_y_stats"),
      
      fluidRow(
        column(6, plotOutput("marginal_y_pdf")),
        column(6, plotOutput("marginal_y_cdf"))
      ),
      
      h4("Conditional Distribution of X Given Y"),
      tableOutput("conditional_x_table"),
      verbatimTextOutput("conditional_x_stats"),
      
      h4("Conditional Distribution of Y Given X"),
      tableOutput("conditional_y_table"),
      verbatimTextOutput("conditional_y_stats")
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ==========================================================
  # UNIVARIATE CALCULATIONS
  # ==========================================================
  
  univariate_data <- eventReactive(input$calculate_u, {
    
    values <- parse_numbers(input$u_values)
    probabilities <- parse_numbers(input$u_probabilities)
    
    validate_distribution(values, probabilities)
    
    list(
      values = values,
      probabilities = probabilities
    )
  })
  
  output$u_table <- renderTable({
    data <- univariate_data()
    
    distribution_table(
      data$values,
      data$probabilities
    )
  }, digits = 6)
  
  output$u_statistics <- renderPrint({
    data <- univariate_data()
    
    mu <- calculate_mean(
      data$values,
      data$probabilities
    )
    
    variance <- calculate_variance(
      data$values,
      data$probabilities
    )
    
    cat("Mean E(X):", round(mu, 6), "\n")
    cat("Variance Var(X):", round(variance, 6), "\n\n")
    
    cat("Interpretation: The mean is the probability-weighted",
        "average of the values. The variance measures the",
        "spread of the distribution around the mean.")
  })
  
  output$u_pdf <- renderPlot({
    data <- univariate_data()
    
    make_distribution_plot(
      data$values,
      data$probabilities,
      "PDF",
      "Univariate Distribution"
    )
  })
  
  output$u_cdf <- renderPlot({
    data <- univariate_data()
    
    make_distribution_plot(
      data$values,
      data$probabilities,
      "CDF",
      "Univariate Distribution"
    )
  })
  
  # ==========================================================
  # BIVARIATE INPUT AND VALIDATION
  # ==========================================================
  
  bivariate_data <- eventReactive(input$calculate_b, {
    
    x <- parse_numbers(input$b_x)
    y <- parse_numbers(input$b_y)
    
    if (length(x) == 0 || length(y) == 0) {
      stop("Enter at least one value for both X and Y.")
    }
    
    if (anyDuplicated(x) || anyDuplicated(y)) {
      stop("The X values and Y values must each be unique.")
    }
    
    lines <- strsplit(
      input$b_matrix,
      "\n",
      fixed = TRUE
    )[[1]]
    
    lines <- lines[nzchar(trimws(lines))]
    
    matrix_rows <- lapply(lines, parse_numbers)
    
    if (length(matrix_rows) != length(y)) {
      stop("The number of matrix rows must equal the number of Y values.")
    }
    
    if (any(lengths(matrix_rows) != length(x))) {
      stop("Each matrix row must contain one probability per X value.")
    }
    
    joint <- do.call(rbind, matrix_rows)
    
    if (any(!is.finite(joint)) ||
        any(joint < 0 | joint > 1)) {
      stop("Each joint probability must be between 0 and 1.")
    }
    
    if (abs(sum(joint) - 1) > 1e-8) {
      stop("All joint probabilities must sum to 1.")
    }
    
    list(
      x = x,
      y = y,
      joint = joint
    )
  })
  
  # ==========================================================
  # JOINT DISTRIBUTION
  # ==========================================================
  
  output$joint_table <- renderTable({
    data <- bivariate_data()
    
    result <- as.data.frame(data$joint)
    names(result) <- paste0("X=", data$x)
    
    result <- cbind(
      Y = data$y,
      result
    )
    
    result
  }, digits = 6)
  
  # ==========================================================
  # MARGINAL DISTRIBUTIONS
  # ==========================================================
  
  marginal_x <- reactive({
    data <- bivariate_data()
    
    list(
      values = data$x,
      probabilities = colSums(data$joint)
    )
  })
  
  marginal_y <- reactive({
    data <- bivariate_data()
    
    list(
      values = data$y,
      probabilities = rowSums(data$joint)
    )
  })
  
  output$marginal_x_table <- renderTable({
    data <- marginal_x()
    
    distribution_table(
      data$values,
      data$probabilities
    )
  }, digits = 6)
  
  output$marginal_y_table <- renderTable({
    data <- marginal_y()
    
    distribution_table(
      data$values,
      data$probabilities
    )
  }, digits = 6)
  
  output$marginal_x_stats <- renderPrint({
    data <- marginal_x()
    
    cat("Mean of X:",
        round(calculate_mean(
          data$values,
          data$probabilities
        ), 6), "\n")
    
    cat("Variance of X:",
        round(calculate_variance(
          data$values,
          data$probabilities
        ), 6), "\n")
  })
  
  output$marginal_y_stats <- renderPrint({
    data <- marginal_y()
    
    cat("Mean of Y:",
        round(calculate_mean(
          data$values,
          data$probabilities
        ), 6), "\n")
    
    cat("Variance of Y:",
        round(calculate_variance(
          data$values,
          data$probabilities
        ), 6), "\n")
  })
  
  output$marginal_x_pdf <- renderPlot({
    data <- marginal_x()
    
    make_distribution_plot(
      data$values,
      data$probabilities,
      "PDF",
      "Marginal Distribution of X"
    )
  })
  
  output$marginal_x_cdf <- renderPlot({
    data <- marginal_x()
    
    make_distribution_plot(
      data$values,
      data$probabilities,
      "CDF",
      "Marginal Distribution of X"
    )
  })
  
  output$marginal_y_pdf <- renderPlot({
    data <- marginal_y()
    
    make_distribution_plot(
      data$values,
      data$probabilities,
      "PDF",
      "Marginal Distribution of Y"
    )
  })
  
  output$marginal_y_cdf <- renderPlot({
    data <- marginal_y()
    
    make_distribution_plot(
      data$values,
      data$probabilities,
      "CDF",
      "Marginal Distribution of Y"
    )
  })
  
  # ==========================================================
  # CONDITIONAL DISTRIBUTION: X GIVEN Y
  # ==========================================================
  
  output$conditional_x_table <- renderTable({
    data <- bivariate_data()
    
    result <- do.call(rbind, lapply(seq_along(data$y), function(j) {
      
      total <- sum(data$joint[j, ])
      
      probabilities <- if (total > 0) {
        data$joint[j, ] / total
      } else {
        rep(NA_real_, length(data$x))
      }
      
      data.frame(
        Condition = paste0("Y=", data$y[j]),
        X = data$x,
        Conditional_Probability = probabilities
      )
    }))
    
    result
  }, digits = 6)
  
  output$conditional_x_stats <- renderPrint({
    data <- bivariate_data()
    
    cat("Conditional mean and variance of X given each Y:\n\n")
    
    for (j in seq_along(data$y)) {
      
      total <- sum(data$joint[j, ])
      
      cat("Condition: Y =", data$y[j], "\n")
      
      if (total == 0) {
        cat("Undefined because P(Y=y) = 0.\n\n")
      } else {
        
        probabilities <- data$joint[j, ] / total
        
        cat("Mean:",
            round(calculate_mean(
              data$x, probabilities
            ), 6), "\n")
        
        cat("Variance:",
            round(calculate_variance(
              data$x, probabilities
            ), 6), "\n\n")
      }
    }
  })
  
  # ==========================================================
  # CONDITIONAL DISTRIBUTION: Y GIVEN X
  # ==========================================================
  
  output$conditional_y_table <- renderTable({
    data <- bivariate_data()
    
    result <- do.call(rbind, lapply(seq_along(data$x), function(i) {
      
      total <- sum(data$joint[, i])
      
      probabilities <- if (total > 0) {
        data$joint[, i] / total
      } else {
        rep(NA_real_, length(data$y))
      }
      
      data.frame(
        Condition = paste0("X=", data$x[i]),
        Y = data$y,
        Conditional_Probability = probabilities
      )
    }))
    
    result
  }, digits = 6)
  
  output$conditional_y_stats <- renderPrint({
    data <- bivariate_data()
    
    cat("Conditional mean and variance of Y given each X:\n\n")
    
    for (i in seq_along(data$x)) {
      
      total <- sum(data$joint[, i])
      
      cat("Condition: X =", data$x[i], "\n")
      
      if (total == 0) {
        cat("Undefined because P(X=x) = 0.\n\n")
      } else {
        
        probabilities <- data$joint[, i] / total
        
        cat("Mean:",
            round(calculate_mean(
              data$y, probabilities
            ), 6), "\n")
        
        cat("Variance:",
            round(calculate_variance(
              data$y, probabilities
            ), 6), "\n\n")
      }
    }
  })
}

# ------------------------------------------------------------
# RUN THE SHINY APPLICATION
# ------------------------------------------------------------

shinyApp(ui = ui, server = server)