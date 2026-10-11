# ============================================================
# ITEM 1: PROBABILITY OF A DEFECTIVE PRODUCT
# ============================================================

cat("============================================\n")
cat("FACTORY DEFECTIVE PROBABILITY CALCULATOR\n")
cat("============================================\n\n")

# Function for validated numeric input
get_valid_number <- function(prompt, lower, upper) {
  repeat {
    answer <- readline(prompt)
    
    value <- suppressWarnings(as.numeric(answer))
    
    if (length(value) == 1 &&
        is.finite(value) &&
        value >= lower &&
        value <= upper) {
      return(value)
    }
    
    cat("Invalid input. Enter a number from",
        lower, "to", upper, ".\n")
  }
}

# Function to input three values with a required total
get_three_values <- function(label, lower, upper, required_sum) {
  
  repeat {
    cat("\nEnter", label, "for each factory.\n")
    cat("Allowed range:", lower, "to", upper, "\n")
    
    values <- numeric(3)
    
    for (i in 1:3) {
      values[i] <- get_valid_number(
        paste0("Factory ", i, ": "),
        lower,
        upper
      )
    }
    
    cat("\nSum of entered values:", sum(values), "\n")
    
    if (abs(sum(values) - required_sum) < 1e-8) {
      return(values)
    }
    
    cat("Invalid total. The three values must sum to",
        required_sum, ". Please enter them again.\n")
  }
}

# Input production proportions
cat("PRODUCTION PROPORTIONS\n")
cat("Each value must be from 0.10 to 0.40.\n")
cat("The sum must equal 1.00.\n")

x <- get_three_values(
  label = "production proportions (x)",
  lower = 0.10,
  upper = 0.40,
  required_sum = 1.00
)

# Input defective rates
cat("\nDEFECTIVE RATES\n")
cat("Each value must be from 0.01 to 0.05.\n")
cat("The sum must equal 0.12.\n")

y <- get_three_values(
  label = "defective rates (y)",
  lower = 0.01,
  upper = 0.05,
  required_sum = 0.12
)

# Calculate each factory's contribution
contributions <- x * y

# Calculate total defective probability
probability_defective <- sum(contributions)

# Display results
results <- data.frame(
  Factory = paste("Factory", 1:3),
  Production_Proportion = x,
  Defective_Rate = y,
  Defective_Contribution = contributions
)

cat("\n============================================\n")
cat("RESULTS\n")
cat("============================================\n\n")

print(results, row.names = FALSE, digits = 6)

cat("\nSum of production proportions:", sum(x), "\n")
cat("Sum of defective rates:", sum(y), "\n")

cat("\nProbability of a defective product:",
    round(probability_defective, 6), "\n")

cat("Percentage of defective products:",
    round(probability_defective * 100, 4), "%\n")

cat("\nINTERPRETATION:\n")
cat("The probability that a randomly selected product is defective\n")
cat("is the sum of each factory's production proportion multiplied\n")
cat("by its corresponding defective rate.\n")