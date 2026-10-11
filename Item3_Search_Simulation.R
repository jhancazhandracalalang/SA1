# ============================================================
# ITEM 3: GEOMETRIC SEARCH ENGINE SIMULATION
# ============================================================

cat("============================================\n")
cat("SEARCH ENGINE SIMULATION\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# STEP 1: INPUT VALIDATION
# ------------------------------------------------------------

get_probability <- function() {
  
  repeat {
    
    p_input <- readline(
      "Enter the probability p that a site contains the phrase (0 < p <= 1): "
    )
    
    p <- suppressWarnings(as.numeric(p_input))
    
    if (length(p) == 1 &&
        is.finite(p) &&
        p > 0 &&
        p <= 1) {
      return(p)
    }
    
    cat("Invalid input. Enter a probability greater than 0 and at most 1.\n")
  }
}

get_simulation_count <- function() {
  
  repeat {
    
    n_input <- readline(
      "Enter the number of simulations (default 10000): "
    )
    
    if (n_input == "") {
      return(10000L)
    }
    
    n <- suppressWarnings(as.numeric(n_input))
    
    if (length(n) == 1 &&
        is.finite(n) &&
        n >= 100 &&
        n <= 1000000 &&
        n == floor(n)) {
      return(as.integer(n))
    }
    
    cat("Enter a whole number from 100 to 1,000,000.\n")
  }
}

p <- get_probability()
n <- get_simulation_count()

# ------------------------------------------------------------
# STEP 2: GENERATE THE SIMULATION
# ------------------------------------------------------------

set.seed(123)

# rgeom counts failures before the first success.
# Add 1 so X represents the total number of searches.
X <- rgeom(n, prob = p) + 1

cat("\nSimulation completed.\n")
cat("Number of simulations:", n, "\n")
cat("Probability of success:", p, "\n")

# ------------------------------------------------------------
# STEP 3: SIMULATED PDF AND CDF
# ------------------------------------------------------------

maximum_searches <- max(X)

search_values <- seq_len(maximum_searches)

search_counts <- tabulate(
  X,
  nbins = maximum_searches
)

simulated_pdf <- search_counts / n
simulated_cdf <- cumsum(simulated_pdf)

distribution_results <- data.frame(
  Searches = search_values,
  Count = search_counts,
  Simulated_PDF = simulated_pdf,
  Simulated_CDF = simulated_cdf
)

cat("\n============================================\n")
cat("SIMULATED DISTRIBUTION\n")
cat("============================================\n")

print(distribution_results, row.names = FALSE, digits = 6)

# ------------------------------------------------------------
# STEP 4: MEAN AND VARIANCE
# ------------------------------------------------------------

simulated_mean <- mean(X)
simulated_variance <- var(X)

theoretical_mean <- 1 / p
theoretical_variance <- (1 - p) / p^2

statistics_results <- data.frame(
  Statistic = c("Mean", "Variance"),
  Simulated = c(simulated_mean, simulated_variance),
  Theoretical = c(theoretical_mean, theoretical_variance)
)

cat("\n============================================\n")
cat("MEAN AND VARIANCE\n")
cat("============================================\n")

print(statistics_results, row.names = FALSE, digits = 6)

cat("\nINTERPRETATION:\n")
cat("The simulated mean and variance should be reasonably close\n")
cat("to the theoretical geometric distribution values. Small\n")
cat("differences are expected because of random sampling.\n")

# ------------------------------------------------------------
# STEP 5: PLOT THE SIMULATED PDF
# ------------------------------------------------------------

pdf_plot <- ggplot(
  distribution_results,
  aes(x = Searches, y = Simulated_PDF)
) +
  geom_col() +
  labs(
    title = "Simulated PDF of Searches Until First Success",
    x = "Number of Searches",
    y = "Simulated Probability"
  ) +
  theme_minimal()

print(pdf_plot)

ggsave(
  filename = "outputs/Item3_Simulated_PDF.png",
  plot = pdf_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# STEP 6: PLOT THE SIMULATED CDF
# ------------------------------------------------------------

cdf_plot <- ggplot(
  distribution_results,
  aes(x = Searches, y = Simulated_CDF)
) +
  geom_step(direction = "hv") +
  geom_point(size = 1.5) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Simulated CDF of Searches Until First Success",
    x = "Number of Searches",
    y = "Cumulative Probability"
  ) +
  theme_minimal()

print(cdf_plot)

ggsave(
  filename = "outputs/Item3_Simulated_CDF.png",
  plot = cdf_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# STEP 7: CONDITIONAL DISTRIBUTION AFTER THREE FAILURES
# ------------------------------------------------------------

# X > 3 means the first three searches were unsuccessful.
# X - 3 counts the additional searches until the first success.

X_after_three_failures <- X[X > 3] - 3

if (length(X_after_three_failures) == 0) {
  
  cat("\nNo simulations had more than three searches.\n")
  cat("The conditional distribution cannot be estimated.\n")
  
} else {
  
  conditional_n <- length(X_after_three_failures)
  
  conditional_max <- max(X_after_three_failures)
  
  conditional_values <- seq_len(conditional_max)
  
  conditional_counts <- tabulate(
    X_after_three_failures,
    nbins = conditional_max
  )
  
  conditional_pdf <- conditional_counts / conditional_n
  conditional_cdf <- cumsum(conditional_pdf)
  
  conditional_results <- data.frame(
    Additional_Searches = conditional_values,
    Count = conditional_counts,
    Conditional_PDF = conditional_pdf,
    Conditional_CDF = conditional_cdf
  )
  
  cat("\n============================================\n")
  cat("CONDITIONAL DISTRIBUTION AFTER THREE FAILURES\n")
  cat("============================================\n")
  
  print(conditional_results, row.names = FALSE, digits = 6)
  
  conditional_mean <- mean(X_after_three_failures)
  conditional_variance <- var(X_after_three_failures)
  
  conditional_statistics <- data.frame(
    Statistic = c("Mean", "Variance"),
    Conditional = c(
      conditional_mean,
      conditional_variance
    ),
    Original_Simulated = c(
      simulated_mean,
      simulated_variance
    ),
    Theoretical = c(
      theoretical_mean,
      theoretical_variance
    )
  )
  
  cat("\nConditional versus original statistics:\n")
  
  print(
    conditional_statistics,
    row.names = FALSE,
    digits = 6
  )
  
  cat("\nINTERPRETATION:\n")
  cat("The conditional distribution of the number of additional\n")
  cat("searches should approximately match the original geometric\n")
  cat("distribution. Its mean and variance should be close to the\n")
  cat("original simulated and theoretical values. Differences are\n")
  cat("expected because the conditional sample is smaller.\n")
  
  conditional_plot <- ggplot(
    conditional_results,
    aes(x = Additional_Searches, y = Conditional_PDF)
  ) +
    geom_col() +
    labs(
      title = "Conditional PDF After Three Unsuccessful Searches",
      x = "Additional Searches Until First Success",
      y = "Conditional Probability"
    ) +
    theme_minimal()
  
  print(conditional_plot)
  
  ggsave(
    filename = "outputs/Item3_Conditional_PDF.png",
    plot = conditional_plot,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# ------------------------------------------------------------
# STEP 8: TEST THE MEMORYLESS PROPERTY
# ------------------------------------------------------------

# Estimate probabilities from the simulated sample.

p_x1 <- mean(X == 1)
p_x2 <- mean(X == 2)

number_after_three <- sum(X > 3)

p_x4_given_x_gt3 <- if (number_after_three > 0) {
  sum(X == 4) / number_after_three
} else {
  NA_real_
}

p_x5_given_x_gt3 <- if (number_after_three > 0) {
  sum(X == 5) / number_after_three
} else {
  NA_real_
}

memoryless_results <- data.frame(
  Comparison = c(
    "P(X=4 | X>3) versus P(X=1)",
    "P(X=5 | X>3) versus P(X=2)"
  ),
  Conditional_Estimate = c(
    p_x4_given_x_gt3,
    p_x5_given_x_gt3
  ),
  Original_Estimate = c(
    p_x1,
    p_x2
  ),
  Theoretical_Conditional = c(
    p,
    (1 - p) * p
  ),
  Theoretical_Original = c(
    p,
    (1 - p) * p
  )
)

cat("\n============================================\n")
cat("MEMORYLESS PROPERTY TEST\n")
cat("============================================\n")

print(memoryless_results, row.names = FALSE, digits = 6)

cat("\nTHEORETICAL COMPARISONS:\n")
cat("P(X=4 | X>3) = P(X=1) = p =", p, "\n")
cat("P(X=5 | X>3) = P(X=2) = (1-p)*p =",
    (1 - p) * p, "\n")

cat("\nINTERPRETATION:\n")
cat("The conditional probability estimates should be close to\n")
cat("their corresponding original probabilities. This supports\n")
cat("the memoryless property of the geometric distribution.\n")
cat("The estimates may differ due to random sampling variation.\n")

# ------------------------------------------------------------
# STEP 9: SAVE OUTPUT TABLES
# ------------------------------------------------------------

write.csv(
  distribution_results,
  "outputs/Item3_Simulated_Distribution.csv",
  row.names = FALSE
)

write.csv(
  statistics_results,
  "outputs/Item3_Statistics.csv",
  row.names = FALSE
)

write.csv(
  memoryless_results,
  "outputs/Item3_Memoryless_Results.csv",
  row.names = FALSE
)

if (exists("conditional_results")) {
  write.csv(
    conditional_results,
    "outputs/Item3_Conditional_Distribution.csv",
    row.names = FALSE
  )
  
  write.csv(
    conditional_statistics,
    "outputs/Item3_Conditional_Statistics.csv",
    row.names = FALSE
  )
}

cat("\n============================================\n")
cat("OUTPUT FILES SAVED\n")
cat("============================================\n")
cat("Check the outputs folder for plots and CSV files.\n")