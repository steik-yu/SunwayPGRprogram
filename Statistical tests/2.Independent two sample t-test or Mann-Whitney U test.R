### ----- PERFORM two sample t test manually -----

# Research Question: Do patients from high-income and low-income countries have different tumor sizes?
# • H0: No difference in mean tumor size.
# • H1: Significant difference in tumor size.
# i. Perform an independent samples t-test.
# ii. Check normality; if violated, use Wilcoxon rank sum test instead.


##   Economic_Classification      n mean_size median_size    sd   IQR
##   <chr>                    <int>     <dbl>       <dbl> <dbl> <dbl>
## 1 Developed               100367      42.0          42  21.7    38
## 2 Developing               67130      42.0          42  21.7    38


# If Tumor_Size_mm normally distributed
t.test(Tumor_Size_mm ~ Economic_Classification, data = data)

# If Tumor_Size_mm NOT normally distributed
wilcox.test(Tumor_Size_mm ~ Economic_Classification, data = data)


### ----- create two sample t test function -----

perform_two_sample_test <- function(data) {
  # Load required packages
  suppressPackageStartupMessages({
    if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
    if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
    if (!requireNamespace("ggpubr", quietly = TRUE)) install.packages("ggpubr")
    if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
    library(dplyr)
    library(ggplot2)
    library(ggpubr)
    library(car)
  })
  
  # Show column mapping
  cat("Column Number Mapping:\n")
  col_names <- names(data)
  for (i in seq_along(col_names)) {
    cat(sprintf("%s: %d,\n", col_names[i], i))
  }
  
  # Get target column
  cat("\nEnter column number for the numeric variable to test: ")
  target_index <- as.integer(readline())
  
  # Validate target
  if (!target_index %in% seq_along(col_names)) {
    stop("Invalid column number")
  }
  target_col <- col_names[target_index]
  
  if (!is.numeric(data[[target_col]])) {
    stop("Selected target column must be numeric")
  }
  
  # Get grouping column
  cat("\nEnter column number for the grouping variable: ")
  group_index <- as.integer(readline())
  
  # Validate group
  if (!group_index %in% seq_along(col_names)) {
    stop("Invalid column number")
  }
  group_col <- col_names[group_index]
  
  # Convert to factor if needed
  if (!is.factor(data[[group_col]])) {
    data[[group_col]] <- as.factor(data[[group_col]])
  }
  
  # Check number of groups
  groups <- levels(data[[group_col]])
  if (length(groups) != 2) {
    stop("Grouping variable must have exactly 2 levels")
  }
  
  # Get alternative hypothesis
  cat("\nSelect alternative hypothesis:\n",
      "1. Two-sided (", groups[1], " ≠ ", groups[2], ")\n",
      "2. Greater than (", groups[1], " > ", groups[2], ")\n",
      "3. Less than (", groups[1], " < ", groups[2], ")\n", sep = "")
  alt_choice <- as.integer(readline("Your choice (1-3): "))
  
  if (alt_choice == 1) {
    alternative <- "two.sided"
    alt_text <- "different from"
  } else if (alt_choice == 2) {
    alternative <- "greater"
    alt_text <- "greater than"
  } else if (alt_choice == 3) {
    alternative <- "less"
    alt_text <- "less than"
  } else {
    stop("Invalid choice")
  }
  
  # Check assumptions
  cat("\nChecking assumptions...\n")
  
  # Split data
  group_data <- split(data[[target_col]], data[[group_col]])
  group1 <- na.omit(group_data[[groups[1]]])
  group2 <- na.omit(group_data[[groups[2]]])
  
  # Normality checks
  norm1 <- if (length(group1) >= 3) shapiro.test(group1)$p.value > 0.05 else NA
  norm2 <- if (length(group2) >= 3) shapiro.test(group2)$p.value > 0.05 else NA
  
  # Equal variance check
  var_equal <- leveneTest(data[[target_col]] ~ data[[group_col]])$`Pr(>F)`[1] > 0.05
  
  # Report assumptions
  cat("Group 1 (", groups[1], ") normality: ",
      if (is.na(norm1)) "n<3" else if (norm1) "Normal" else "Not Normal", "\n")
  cat("Group 2 (", groups[2], ") normality: ",
      if (is.na(norm2)) "n<3" else if (norm2) "Normal" else "Not Normal", "\n")
  cat("Equal variances: ", if (var_equal) "Yes" else "No", "\n")
  
  # Test selection
  cat("\nSelect test method:\n",
      "1. Automatic (recommended)\n",
      "2. Student's t-test (assumes equal variance)\n",
      "3. Welch's t-test (does not assume equal variance)\n",
      "4. Mann-Whitney U test\n")
  choice <- as.integer(readline("Your choice (1-4): "))
  
  if (choice == 1) {
    if (isTRUE(norm1) && isTRUE(norm2)) {
      test_type <- if (var_equal) "student" else "welch"
      cat("\nUsing ", if (var_equal) "Student's t-test" else "Welch's t-test", 
          " based on normality assessment\n")
    } else {
      test_type <- "mannwhitney"
      cat("\nUsing Mann-Whitney U test based on normality assessment\n")
    }
  } else if (choice == 2) {
    test_type <- "student"
  } else if (choice == 3) {
    test_type <- "welch"
  } else if (choice == 4) {
    test_type <- "mannwhitney"
  } else {
    stop("Invalid choice")
  }
  
  # Perform test
  formula <- as.formula(paste(target_col, "~", group_col))
  
  if (test_type == "student") {
    result <- t.test(formula, data = data, var.equal = TRUE, alternative = alternative)
    test_name <- "Student's t-test (Equal Variance)"
  } else if (test_type == "welch") {
    result <- t.test(formula, data = data, var.equal = FALSE, alternative = alternative)
    test_name <- "Welch's t-test (Unequal Variance)"
  } else {
    result <- wilcox.test(formula, data = data, alternative = alternative, conf.int = TRUE)
    test_name <- "Mann-Whitney U Test"
  }
  
  # Calculate group statistics
  group_stats <- data %>%
    group_by(!!sym(group_col)) %>%
    summarise(
      n = sum(!is.na(!!sym(target_col))),
      mean = mean(!!sym(target_col), na.rm = TRUE),
      sd = sd(!!sym(target_col), na.rm = TRUE),
      median = median(!!sym(target_col), na.rm = TRUE),
      iqr = IQR(!!sym(target_col), na.rm = TRUE)
    )
  
  # Print results
  cat("\n\n=== TEST RESULTS ===\n")
  cat("Test:", test_name, "\n")
  cat("Target Variable:", target_col, "\n")
  cat("Grouping Variable:", group_col, "\n")
  cat("Group 1:", groups[1], "\n")
  cat("Group 2:", groups[2], "\n")
  cat("Alternative hypothesis: Group 1 is", alt_text, "Group 2\n\n")
  
  # Print group statistics
  cat("Group Statistics:\n")
  print(as.data.frame(group_stats), row.names = FALSE)
  cat("\n")
  
  # Print test statistics
  if (test_type %in% c("student", "welch")) {
    cat("t-statistic:", result$statistic, "\n")
    cat("df:", result$parameter, "\n")
    cat("p-value:", result$p.value, "\n")
    if (alternative == "two.sided") {
      cat("95% Confidence Interval:", 
          paste(round(result$conf.int, 4), collapse = " to "), "\n")
    }
  } else {
    cat("W-statistic:", result$statistic, "\n")
    cat("p-value:", result$p.value, "\n")
    if (alternative == "two.sided") {
      cat("95% Confidence Interval for location difference:", 
          paste(round(result$conf.int, 4), collapse = " to "), "\n")
    }
  }
  
  # Interpretation
  cat("\nInterpretation:\n")
  if (result$p.value < 0.05) {
    cat("-> Significant difference between groups (p =", 
        round(result$p.value, 4), ")\n")
    
    # Directional interpretation
    if (alternative == "two.sided") {
      direction <- ifelse(group_stats$mean[1] > group_stats$mean[2],
                          "greater than", "less than")
      cat(sprintf("-> %s (%.2f) is %s %s (%.2f)\n",
                  groups[1], group_stats$mean[1],
                  direction,
                  groups[2], group_stats$mean[2]))
    } else if (alternative == "greater") {
      cat(sprintf("-> %s is significantly greater than %s\n", groups[1], groups[2]))
    } else {
      cat(sprintf("-> %s is significantly less than %s\n", groups[1], groups[2]))
    }
  } else {
    cat("-> No significant difference between groups (p =", 
        round(result$p.value, 4), ")\n")
  }
  
  # Create plots
  plot_data <- data %>%
    filter(!is.na(!!sym(target_col)) & !is.na(!!sym(group_col)))
  
  # Boxplot
  p1 <- ggplot(plot_data, aes(x = !!sym(group_col), y = !!sym(target_col), 
                              fill = !!sym(group_col))) +
    geom_boxplot(alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
    labs(title = paste("Comparison of", target_col, "by", group_col),
         subtitle = paste(test_name, "| p =", signif(result$p.value, 3)),
         x = group_col, y = target_col) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  # Density plot
  p2 <- ggplot(plot_data, aes(x = !!sym(target_col), fill = !!sym(group_col))) +
    geom_density(alpha = 0.5) +
    labs(title = "Distribution Comparison",
         subtitle = "Density plots by group",
         x = target_col, y = "Density") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1") +
    theme(legend.position = "top")
  
  # QQ-plots
  p3 <- ggplot(plot_data, aes(sample = !!sym(target_col), color = !!sym(group_col))) +
    stat_qq() + 
    stat_qq_line() +
    facet_wrap(as.formula(paste("~", group_col)), scales = "free") +
    labs(title = "Normality Check by Group",
         subtitle = "QQ-plots showing normality assumption",
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
  
  # Combine plots
  combined_plot <- ggarrange(
    p1,
    ggarrange(p2, p3, ncol = 2),
    nrow = 2, heights = c(1, 1.2)
  )
  
  # Show plots
  cat("\nGenerating diagnostic plots...\n")
  print(combined_plot)
  
  return(invisible(list(
    test_type = test_type,
    result = result,
    group_stats = group_stats,
    plot = combined_plot
  )))
}


### ----- PERFORM two sample t test with function -----

perform_two_sample_test(data) # type 6 and 25 for example