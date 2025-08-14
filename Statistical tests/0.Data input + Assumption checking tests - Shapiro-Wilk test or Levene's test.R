### ----- CHECK and LOAD packages -----

packages_needed <- c("tidyverse", "readxl", "car")

# Function to check/install packages
check_and_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing missing package: ", pkg)
    install.packages(pkg)
  } else {
    message("Package already installed: ", pkg)
  }
}

invisible(sapply(packages_needed, check_and_install))
invisible(lapply(packages_needed, library, character.only = TRUE))

### ----- create IMPORT data function -----

ImportData <- function() {
  file_path <- file.choose()
  cat("You selected:", file_path, "\n")
  
  # Extract file extension (lowercase for robustness)
  file_ext <- tolower(tools::file_ext(file_path))
  
  # Read file based on extension
  if (file_ext == "csv") {
    Rawdata <- read.csv(file_path)
  } else if (file_ext == "xlsx") {
    # Check/install 'readxl' if needed
    if (!requireNamespace("readxl", quietly = TRUE)) {
      message("Installing 'readxl' package...")
      install.packages("readxl")
    }
    
    # Ask user about sheet selection
    cat("\n If you need the first sheet, press enter.\n",
        "If you need a specific sheet, enter `2`.\n")
    sheet_choice <- readline(prompt = "Your choice (Enter or 2): ")
    
    # Validate input and read sheet
    if (sheet_choice == "") {
      Rawdata <- readxl::read_excel(file_path)
    } else if (sheet_choice == "2") {
      sheet_name <- readline(prompt = "Enter the sheet number: ")
      Rawdata <- readxl::read_excel(file_path, sheet = as.numeric(sheet_name))
    } else {
      stop("Invalid choice. Please run again and press enter or enter `2`.")
    }
  } else {
    stop("Unsupported file type. Please use .csv or .xlsx.")
  }
  
  return(Rawdata)
}


### ----- create CHECK data normality function -----

perform_normality_tests <- function(data) {
  # Load required packages
  suppressPackageStartupMessages({
    if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
    if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
    if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
    if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(gridExtra)
  })
  
  # Show column mapping
  cat("Column Number Mapping:\n")
  data <- as.data.frame(data)
  col_names <- names(data)
  for (i in seq_along(col_names)) {
    cat(sprintf("%s: %d,\n", col_names[i], i))
  }
  
  # Get target columns
  cat("\nEnter column numbers for normality testing (comma separated): ")
  target_input <- readline()
  target_indices <- as.integer(unlist(strsplit(target_input, ",\\s*")))
  
  # Validate targets
  invalid_targets <- target_indices[!target_indices %in% seq_along(col_names)]
  if (length(invalid_targets) > 0) {
    stop("Invalid target column numbers: ", paste(invalid_targets, collapse = ", "))
  }
  target_cols <- col_names[target_indices]
  
  # Get grouping option
  cat("\nEnter grouping column numbers (comma separated, 0 for no grouping): ")
  group_input <- readline()
  group_indices <- as.integer(unlist(strsplit(group_input, ",\\s*")))
  
  # Handle ungrouped analysis
  if (length(group_indices) == 1 && group_indices == 0) {
    results <- list()
    plots <- list()
    cat("\nPerforming overall normality tests:\n")
    
    for (col in target_cols) {
      if (!is.numeric(data[[col]])) {
        results[[col]] <- list(status = "Non-numeric (skipped)", p_value = NA)
        next
      }
      
      clean_data <- na.omit(data[[col]])
      n <- length(clean_data)
      uniq <- length(unique(clean_data))
      
      # Default
      status <- NULL
      p_val <- NA
      
      if (n < 3) {
        status <- "Insufficient data (n<3)"
      } else if (n > 5000) {
        status <- "Too many observations (n>5000) - skipped Shapiro"
      } else if (uniq < 2) {
        status <- "Identical values - skipped Shapiro"
      } else {
        test_result <- shapiro.test(clean_data)
        p_val <- test_result$p.value
        status <- ifelse(p_val > 0.05, "Normal", "Not normal")
      }
      
      results[[col]] <- list(status = status, p_value = p_val)
      
      # Always make plots for numeric columns
      p1 <- ggplot(data.frame(x = clean_data), aes(sample = x)) +
        stat_qq() + 
        stat_qq_line() +
        labs(title = paste("QQ-Plot:", col),
             subtitle = ifelse(is.na(p_val), status, paste("Shapiro-Wilk p =", round(p_val, 4))),
             x = "Theoretical Quantiles", y = "Sample Quantiles")
      
      p2 <- ggplot(data.frame(x = clean_data), aes(x)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
        geom_density(color = "red", linewidth = 1) +
        labs(title = paste("Distribution:", col),
             x = col, y = "Density")
      
      plots[[col]] <- list(QQ = p1, Histogram = p2)
    }
    
    # Print results
    cat("\nNormality Test Results (Overall):\n")
    for (col in target_cols) {
      res <- results[[col]]
      if (is.na(res$p_value)) {
        cat(sprintf("%s: %s\n", col, res$status))
      } else {
        cat(sprintf("%s: %s (p = %.4f)\n", col, res$status, res$p_value))
      }
    }
    
    # Show plots
    cat("\nWould you like to view diagnostic plots? (y/n): ")
    if (tolower(readline()) == "y") {
      for (col in names(plots)) {
        grid.arrange(plots[[col]]$QQ, plots[[col]]$Histogram, ncol = 2)
        cat("Press [enter] to see next plot")
        readline()
      }
    }
    
    return(invisible(list(results = results, plots = plots)))
  }
  
  # Grouped analysis
  invalid_groups <- group_indices[!group_indices %in% seq_along(col_names)]
  if (length(invalid_groups) > 0) {
    stop("Invalid grouping column numbers: ", paste(invalid_groups, collapse = ", "))
  }
  group_cols <- col_names[group_indices]
  
  group_var_name <- paste(group_cols, collapse = "_")
  data <- data %>%
    tidyr::unite(!!group_var_name, all_of(group_cols), sep = " | ", remove = FALSE)
  
  all_results <- list()
  all_plots <- list()
  
  for (target_col in target_cols) {
    if (!is.numeric(data[[target_col]])) {
      all_results[[target_col]] <- data.frame(
        Group = "All Groups",
        Status = "Non-numeric (skipped)",
        p_Value = NA_real_
      )
      next
    }
    
    plot_data <- data %>%
      drop_na(!!sym(group_var_name), !!sym(target_col))
    
    group_results <- plot_data %>%
      dplyr::group_by(!!sym(group_var_name)) %>%
      dplyr::summarize(
        n = sum(!is.na(!!sym(target_col))),
        uniq = length(unique(na.omit(!!sym(target_col)))),
        p_value = ifelse(
          n < 3 | n > 5000 | uniq < 2,
          NA_real_,
          shapiro.test(!!sym(target_col))$p.value
        ),
        status = dplyr::case_when(
          n < 3 ~ "Insufficient data (n<3)",
          n > 5000 ~ "Too many observations (n>5000) - skipped Shapiro",
          uniq < 2 ~ "Identical values - skipped Shapiro",
          is.na(p_value) ~ "Test failed",
          p_value > 0.05 ~ "Normal",
          TRUE ~ "Not normal"
        ),
        .groups = "drop"
      ) %>%
      dplyr::select(Group = !!group_var_name, Status = status, p_Value = p_value) %>%
      as.data.frame()
    
    all_results[[target_col]] <- group_results
    
    # Always make plots
    if (nrow(plot_data) > 0) {
      qq_plot <- ggplot(plot_data, aes(sample = !!sym(target_col))) +
        stat_qq() + 
        stat_qq_line() +
        facet_wrap(as.formula(paste("~", group_var_name)), scales = "free") +
        labs(title = paste("QQ-Plot by Group:", target_col))
      
      box_plot <- ggplot(plot_data, aes(x = !!sym(group_var_name), y = !!sym(target_col))) +
        geom_boxplot(fill = "lightgreen") +
        labs(title = paste("Distribution by Group:", target_col),
             x = group_var_name, y = target_col) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      all_plots[[target_col]] <- list(QQ = qq_plot, Boxplot = box_plot)
    }
  }
  
  # Print grouped results
  cat("\n\n=== GROUP-WISE NORMALITY RESULTS ===\n")
  for (target in target_cols) {
    cat("\nVariable:", target, "\n")
    df <- all_results[[target]]
    df$Display <- ifelse(is.na(df$p_Value),
                         df$Status,
                         sprintf("%s (p = %.4f)", df$Status, df$p_Value))
    print(df[c("Group", "Display")], row.names = FALSE, col.names = c("Group", "Result"))
    cat("\n")
  }
  
  # Show plots
  cat("\nWould you like to view diagnostic plots? (y/n): ")
  if (tolower(readline()) == "y") {
    for (target in target_cols) {
      if (target %in% names(all_plots)) {
        grid.arrange(all_plots[[target]]$QQ, all_plots[[target]]$Boxplot, ncol = 1)
        cat("Press [enter] to see next plot")
        readline()
      }
    }
  }
  
  return(invisible(list(results = all_results, plots = all_plots)))
}


### ----- create CHECK data variance (for ANOVA) function -----

perform_levene_test <- function(data) {
  # Load required packages
  suppressPackageStartupMessages({
    if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
    if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
    if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
    library(dplyr)
    library(car)
    library(tidyr)
  })
  
  # Show column mapping
  cat("Column Number Mapping:\n")
  col_names <- names(data)
  for (i in seq_along(col_names)) {
    cat(sprintf("%s: %d,\n", col_names[i], i))
  }
  
  # Get target column
  cat("\nEnter column number for variance homogeneity test: ")
  target_index <- as.integer(readline())
  
  # Validate target
  if (!target_index %in% seq_along(col_names)) {
    stop("Invalid column number")
  }
  target_col <- col_names[target_index]
  
  if (!is.numeric(data[[target_col]])) {
    stop("Selected column must be numeric")
  }
  
  # Get grouping columns
  cat("\nEnter grouping column numbers (comma separated): ")
  group_input <- readline()
  group_indices <- as.integer(unlist(strsplit(group_input, ",\\s*")))
  
  # Validate group indices
  invalid_groups <- group_indices[!group_indices %in% seq_along(col_names)]
  if (length(invalid_groups) > 0) {
    stop("Invalid grouping column numbers: ", paste(invalid_groups, collapse = ", "))
  }
  group_cols <- col_names[group_indices]
  
  # Create combined grouping variable using tidyr::unite
  if (length(group_cols) > 1) {
    group_var_name <- paste(group_cols, collapse = "_")
    data <- data %>%
      tidyr::unite(!!group_var_name, all_of(group_cols), sep = " | ", remove = FALSE)
  } else {
    group_var_name <- group_cols
  }
  
  # Perform Levene's test
  formula <- as.formula(paste(target_col, "~", group_var_name))
  levene_result <- car::leveneTest(formula, data = data)
  
  # Print results
  cat("\n\n=== LEVENE'S TEST RESULTS ===\n")
  cat("Target Variable:", target_col, "\n")
  cat("Grouping Variable(s):", paste(group_cols, collapse = ", "), "\n")
  if (length(group_cols) > 1) {
    cat("Combined Groups:", group_var_name, "\n")
  }
  cat("\n")
  
  print(levene_result)
  
  # Interpret results
  p_value <- levene_result[1, "Pr(>F)"]
  cat("\nInterpretation:\n")
  if (p_value > 0.05) {
    cat("-> Variances are homogeneous (p =", round(p_value, 4), ")\n")
    cat("-> Assumption for ANOVA for equal variance is satisfied\n")
  } else {
    cat("-> Variances are NOT homogeneous (p =", round(p_value, 4), ")\n")
    cat("-> Consider using:\n")
    cat("   - Kruskal-Wallis test (oneway.test) for one-way designs\n")
    cat("   - Robust regression methods\n")
    cat("   - Data transformations\n")
    cat("   - Non-parametric tests\n")
  }
  
  # Visual check
  cat("\nGenerating variance diagnostic plots...\n")
  par(mfrow = c(1, 2))
  
  # Boxplot
  boxplot(formula, data = data, main = "Variance Comparison", 
          xlab = if (length(group_cols) > 1) group_var_name else group_cols,
          ylab = target_col,
          col = rainbow(length(unique(data[[group_var_name]]))))
  
  # Residuals vs fitted
  lm_model <- lm(formula, data = data)
  plot(lm_model, which = 3, main = "Scale-Location Plot")
  
  par(mfrow = c(1, 1))
  
  return(invisible(levene_result))
}


### ----- Import your data & Check normality -----

data <- ImportData() 
# run multiple times if you have different datasheet
# data2 <- ImportData()
# data3 <- ImportData(), for example

perform_normality_tests(data)

perform_levene_test(data)












