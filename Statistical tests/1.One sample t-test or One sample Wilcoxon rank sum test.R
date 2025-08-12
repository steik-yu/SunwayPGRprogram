### ----- PERFORM one sample t test manually -----

# Research Question: The average tumor size in colorectal cancer patients is suspected to be above 35 mm.
# Test this hypothesis.
# • H0: μ = 35 mm
# • H1: μ > 35 mm
# i. Perform a one-sample t-test.
# ii. Check normality assumptions using Shapiro-Wilk test and/or Q-Q plot.
# iii. Interpret the results.


# If Tumor_Size_mm normally distributed
t.test(data$Tumor_Size_mm, mu = 35, alternative = "greater")

# If Tumor_Size_mm NOT normally distributed
wilcox.test(data$Tumor_Size_mm, mu = 35, alternative = "greater", conf.int = TRUE)

# enter ?t.test or ?wilcox.test for more information.


### ----- create one sample t test function -----

perform_one_sample_test <- function(data) {
  # Load required packages
  suppressPackageStartupMessages({
    if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
    if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
    if (!requireNamespace("ggpubr", quietly = TRUE)) install.packages("ggpubr")
    library(dplyr)
    library(ggplot2)
    library(ggpubr)
  })
  
  # Show column mapping
  cat("Column Number Mapping:\n")
  col_names <- names(data)
  for (i in seq_along(col_names)) {
    cat(sprintf("%s: %d,\n", col_names[i], i))
  }
  
  # Get target column
  cat("\nEnter column number for the test: ")
  target_index <- as.integer(readline())
  
  # Validate target
  if (!target_index %in% seq_along(col_names)) {
    stop("Invalid column number")
  }
  target_col <- col_names[target_index]
  
  if (!is.numeric(data[[target_col]])) {
    stop("Selected column must be numeric")
  }
  
  # Get population mean
  cat("\nEnter population mean (μ) to test against: ")
  mu <- as.numeric(readline())
  
  # Get alternative hypothesis
  cat("\nSelect alternative hypothesis:\n",
      "1. Two-sided (μ ≠ ", mu, ")\n",
      "2. Greater than (μ > ", mu, ")\n",
      "3. Less than (μ < ", mu, ")\n", sep = "")
  alt_choice <- as.integer(readline("Your choice (1-3): "))
  
  if (alt_choice == 1) {
    alternative <- "two.sided"
    alt_text <- "not equal to"
  } else if (alt_choice == 2) {
    alternative <- "greater"
    alt_text <- "greater than"
  } else if (alt_choice == 3) {
    alternative <- "less"
    alt_text <- "less than"
  } else {
    stop("Invalid choice")
  }
  
  # Check normality
  cat("\nChecking normality for automatic test selection...\n")
  clean_data <- na.omit(data[[target_col]])
  n <- length(clean_data)
  
  if (n < 3) {
    stop("Insufficient data (n < 3) for normality test")
  }
  
  test_result <- shapiro.test(clean_data)
  is_normal <- test_result$p.value > 0.05
  cat(sprintf(
    "Normality test: %s (p = %.4f)\n", 
    ifelse(is_normal, "Normal", "Not Normal"), 
    test_result$p.value
  ))
  
  # Test selection
  cat("\nSelect test method:\n",
      "1. Automatic (recommended)\n",
      "2. One-sample t-test\n",
      "3. Wilcoxon signed-rank test\n")
  choice <- as.integer(readline("Your choice (1-3): "))
  
  if (choice == 1) {
    test_type <- ifelse(is_normal, "t-test", "wilcoxon")
    cat(sprintf("\nUsing %s based on normality assessment\n", 
                ifelse(is_normal, "t-test", "Wilcoxon test")))
  } else if (choice == 2) {
    test_type <- "t-test"
  } else if (choice == 3) {
    test_type <- "wilcoxon"
  } else {
    stop("Invalid choice")
  }
  
  # Perform test with selected alternative
  if (test_type == "t-test") {
    result <- t.test(clean_data, mu = mu, alternative = alternative)
    test_name <- "One-Sample t-test"
  } else {
    result <- wilcox.test(clean_data, mu = mu, conf.int = TRUE, alternative = alternative)
    test_name <- "Wilcoxon Signed-Rank Test"
  }
  
  # Print results
  cat("\n\n=== TEST RESULTS ===\n")
  cat("Variable:", target_col, "\n")
  cat("Test:", test_name, "\n")
  cat("Population mean (μ):", mu, "\n")
  cat("Alternative hypothesis: μ is", alt_text, mu, "\n")
  cat("Sample size (n):", n, "\n")
  cat("Sample mean:", mean(clean_data), "\n")
  
  if (test_type == "t-test") {
    cat("t-statistic:", result$statistic, "\n")
    cat("df:", result$parameter, "\n")
    cat("p-value:", result$p.value, "\n")
    if (alternative == "two.sided") {
      cat("95% Confidence Interval:", 
          paste(round(result$conf.int, 4), collapse = " to "), "\n")
    }
  } else {
    cat("V-statistic:", result$statistic, "\n")
    cat("p-value:", result$p.value, "\n")
    if (alternative == "two.sided") {
      cat("95% Confidence Interval for location:", 
          paste(round(result$conf.int, 4), collapse = " to "), "\n")
    }
  }
  
  # Interpretation
  cat("\nInterpretation:\n")
  if (result$p.value < 0.05) {
    cat("-> Significant evidence that μ is", alt_text, mu, "(p =", 
        round(result$p.value, 4), ")\n")
    
    if (alternative != "two.sided") {
      direction <- ifelse(mean(clean_data) > mu, "greater than", "less than")
      cat(sprintf("-> Sample mean (%.2f) is %s μ (%.2f)\n", 
                  mean(clean_data), direction, mu))
    }
  } else {
    cat("-> Insufficient evidence that μ is", alt_text, mu, "(p =", 
        round(result$p.value, 4), ")\n")
  }
  
  # Create diagnostic plots
  plot_data <- data.frame(Value = clean_data)
  
  # Histogram with density
  p1 <- ggplot(plot_data, aes(x = Value)) +
    geom_histogram(aes(y = ..density..), bins = 30, 
                   fill = "skyblue", color = "black") +
    geom_density(color = "red", linewidth = 1) +
    geom_vline(xintercept = mu, color = "blue", linewidth = 1, linetype = "dashed") +
    annotate("text", x = mu, y = Inf, label = paste("μ =", mu), 
             vjust = 2, hjust = 1.2, color = "blue") +
    labs(title = paste("Distribution of", target_col),
         subtitle = paste(test_name, "| p =", signif(result$p.value, 3)),
         x = target_col, y = "Density")
  
  # Boxplot with reference line
  p2 <- ggplot(plot_data, aes(y = Value)) +
    geom_boxplot(fill = "lightgreen", width = 0.2) +
    geom_hline(yintercept = mu, color = "blue", linewidth = 1, linetype = "dashed") +
    annotate("text", y = mu, x = Inf, label = paste("μ =", mu), 
             vjust = -1, hjust = 1, color = "blue") +
    coord_flip() +
    labs(title = "Comparison to Reference Value",
         subtitle = paste("Alternative: μ", alt_text, mu),
         y = target_col, x = "") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  # QQ-plot
  p3 <- ggplot(plot_data, aes(sample = Value)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "Normality Check (QQ-Plot)",
         subtitle = paste("Shapiro-Wilk p =", signif(test_result$p.value, 3)),
         x = "Theoretical Quantiles", y = "Sample Quantiles")
  
  # Combine plots
  combined_plot <- ggarrange(
    ggarrange(p1, p2, ncol = 2, widths = c(2, 1)),
    p3, nrow = 2
  )
  
  # Show plots
  cat("\nGenerating diagnostic plots...\n")
  print(combined_plot)
  
  return(invisible(list(
    test_type = test_type,
    result = result,
    normality_test = test_result,
    plot = combined_plot
  )))
}


### ----- PERFORM one sample t test with function -----

perform_one_sample_test(data)

