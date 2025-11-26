# ============================================================================
# BAYESIAN ADAPTIVE SP SURVEY GENERATOR - ATHENS PILOT (R VERSION)
# ============================================================================
# Generates 8 choice sets based on configurator preferences
# Uses idefix package for Bayesian D-efficient designs

# Install required packages
if (!require("idefix")) install.packages("idefix")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

library(idefix)
library(dplyr)
library(tidyr)

# ============================================================================
# 1. DEFINE ATTRIBUTES AND PRIORS
# ============================================================================

# Define attribute levels
attributes <- list(
  mode = c("Home", "Locker", "Depot"),
  cost = c(3, 5, 7, 9),
  time_window = c(1, 2, 4, 24),
  speed = c("Same", "Next", "2-3days"),
  flexibility = c("None", "2h", "Anytime"),
  environmental = c("Standard", "CO2neutral")
)

# Prior means (population level from literature)
prior_means <- c(
  cost = -0.15,
  mode.Locker = -0.3,
  mode.Depot = -0.5,
  time.1h = 0.4,
  time.2h = 0.2,
  time.4h = 0,
  speed.Same = 0.6,
  speed.Next = 0.3,
  flex.2h = 0.3,
  flex.Anytime = 0.5,
  env.CO2 = 0.3
)

# Prior standard deviations
prior_sds <- c(
  cost = 0.05,
  mode.Locker = 0.15,
  mode.Depot = 0.20,
  time.1h = 0.15,
  time.2h = 0.12,
  time.4h = 0.10,
  speed.Same = 0.20,
  speed.Next = 0.15,
  flex.2h = 0.15,
  flex.Anytime = 0.15,
  env.CO2 = 0.20
)

# ============================================================================
# 2. UPDATE PRIORS BASED ON CONFIGURATOR
# ============================================================================

update_priors_from_configurator <- function(configurator_choice, 
                                           prior_means, 
                                           prior_sds) {
  #' Update Bayesian priors based on participant's configurator choices
  #' 
  #' @param configurator_choice List with participant's choices
  #' @param prior_means Named vector of prior means
  #' @param prior_sds Named vector of prior standard deviations
  #' @return List with updated means and covariance matrix
  
  updated_means <- prior_means
  
  # Update based on mode choice
  if (configurator_choice$mode == "Locker") {
    updated_means["mode.Locker"] <- updated_means["mode.Locker"] + 0.3
    updated_means["mode.Depot"] <- updated_means["mode.Depot"] - 0.1
  } else if (configurator_choice$mode == "Depot") {
    updated_means["mode.Depot"] <- updated_means["mode.Depot"] + 0.3
    updated_means["mode.Locker"] <- updated_means["mode.Locker"] - 0.1
  }
  
  # Update based on cost sensitivity
  if (configurator_choice$cost >= 7) {
    updated_means["cost"] <- updated_means["cost"] * 0.7  # Less sensitive
  } else if (configurator_choice$cost <= 4) {
    updated_means["cost"] <- updated_means["cost"] * 1.3  # More sensitive
  }
  
  # Update based on time window preference
  if (configurator_choice$time_window == 1) {
    updated_means["time.1h"] <- updated_means["time.1h"] + 0.3
    updated_means["time.2h"] <- updated_means["time.2h"] + 0.15
  } else if (configurator_choice$time_window == 24) {
    updated_means["time.1h"] <- updated_means["time.1h"] - 0.2
    updated_means["time.2h"] <- updated_means["time.2h"] - 0.1
  }
  
  # Update based on speed preference
  if (configurator_choice$speed == "Same") {
    updated_means["speed.Same"] <- updated_means["speed.Same"] + 0.3
    updated_means["speed.Next"] <- updated_means["speed.Next"] + 0.1
  }
  
  # Update based on flexibility preference
  if (configurator_choice$flexibility == "Anytime") {
    updated_means["flex.Anytime"] <- updated_means["flex.Anytime"] + 0.3
    updated_means["flex.2h"] <- updated_means["flex.2h"] + 0.15
  }
  
  # Update based on environmental preference
  if (configurator_choice$environmental == "CO2neutral") {
    updated_means["env.CO2"] <- updated_means["env.CO2"] + 0.3
  }
  
  # Create covariance matrix
  cov_matrix <- diag(prior_sds^2)
  
  list(
    means = updated_means,
    cov = cov_matrix
  )
}

# ============================================================================
# 3. GENERATE BAYESIAN D-EFFICIENT DESIGN
# ============================================================================

generate_adaptive_design <- function(configurator_choice,
                                    n_choice_sets = 8,
                                    n_alts = 3,
                                    n_draws = 100,
                                    seed = 42) {
  #' Generate Bayesian D-efficient design based on configurator
  #' 
  #' @param configurator_choice List with participant's configurator choices
  #' @param n_choice_sets Number of choice sets (default: 8)
  #' @param n_alts Number of alternatives per set (default: 3)
  #' @param n_draws Number of Bayesian draws (default: 100)
  #' @param seed Random seed
  #' @return Data frame with choice sets
  
  set.seed(seed)
  
  cat("="=rep(70), "\n")
  cat("GENERATING BAYESIAN ADAPTIVE DESIGN\n")
  cat("="=rep(70), "\n\n")
  
  cat("Configurator choice:\n")
  print(configurator_choice)
  cat("\n")
  
  # Update priors
  priors <- update_priors_from_configurator(configurator_choice, 
                                           prior_means, 
                                           prior_sds)
  
  cat("Updated prior means:\n")
  print(round(priors$means, 3))
  cat("\n")
  
  # Create coding scheme for idefix
  # Effects coding for categorical, continuous for cost
  coding <- c("E", "C", "E", "E", "E", "E")
  names(coding) <- names(attributes)
  
  # Generate full factorial candidate set
  cs <- Profiles(lvls = attributes, coding = coding)
  
  # Generate Bayesian D-efficient design
  cat("Generating design using Bayesian optimization...\n")
  
  design <- Modfed(
    cand.set = cs,
    n.sets = n_choice_sets,
    n.alts = n_alts,
    par.draws = list(priors$means, priors$cov),
    n.draws = n_draws,
    parallel = FALSE
  )
  
  cat(sprintf("✓ Generated %d choice sets with %d alternatives each\n", 
              n_choice_sets, n_alts))
  
  # Format design
  design_matrix <- design$design
  
  # Convert back to attribute levels
  design_df <- decode_design(design_matrix, attributes, coding)
  
  return(list(
    design = design,
    design_df = design_df,
    d_error = design$error
  ))
}

# ============================================================================
# 4. DECODE DESIGN TO ATTRIBUTE LEVELS
# ============================================================================

decode_design <- function(design_matrix, attributes, coding) {
  #' Convert design matrix back to attribute levels
  #' 
  #' @param design_matrix Numeric design matrix from idefix
  #' @param attributes List of attribute levels
  #' @param coding Coding scheme
  #' @return Data frame with decoded attributes
  
  n_rows <- nrow(design_matrix)
  
  # Initialize result data frame
  result <- data.frame(
    ChoiceSet = rep(1:(n_rows/3), each = 3),
    Alternative = rep(c("A", "B", "C"), n_rows/3)
  )
  
  # Decode each attribute
  # Note: This is simplified - actual decoding depends on idefix coding
  col_idx <- 1
  
  for (attr_name in names(attributes)) {
    attr_levels <- attributes[[attr_name]]
    
    if (coding[attr_name] == "C") {
      # Continuous - direct values
      result[[attr_name]] <- design_matrix[, col_idx]
      col_idx <- col_idx + 1
    } else {
      # Effects coded - decode back to levels
      n_levels <- length(attr_levels)
      attr_cols <- design_matrix[, col_idx:(col_idx + n_levels - 2)]
      
      # Find which level (simplified logic)
      result[[attr_name]] <- attr_levels[apply(attr_cols, 1, 
                                               function(x) which.max(c(x, -sum(x))))]
      
      col_idx <- col_idx + n_levels - 1
    }
  }
  
  # Format for display
  result <- result %>%
    mutate(
      Cost = paste0("€", Cost),
      TimeWindow = paste0(time_window, ifelse(time_window < 24, "h", "h access")),
      Mode = mode,
      Speed = speed,
      Flexibility = flexibility,
      Environmental = environmental
    ) %>%
    select(ChoiceSet, Alternative, Mode, Cost, TimeWindow, Speed, 
           Flexibility, Environmental)
  
  return(result)
}

# ============================================================================
# 5. EXAMPLE USAGE
# ============================================================================

# Example: Participant's configurator choice
configurator_choice <- list(
  mode = "Home",
  cost = 7,
  time_window = 2,
  speed = "Next",
  flexibility = "2h",
  environmental = "CO2neutral"
)

# Generate adaptive design
result <- generate_adaptive_design(
  configurator_choice = configurator_choice,
  n_choice_sets = 8,
  n_alts = 3,
  n_draws = 100,
  seed = 42
)

# Display results
cat("\n")
cat("="=rep(70), "\n")
cat("DESIGN GENERATED SUCCESSFULLY\n")
cat("="=rep(70), "\n\n")

cat(sprintf("D-error: %.4f\n\n", result$d_error))

cat("First 3 choice sets:\n")
cat("-"=rep(70), "\n")
print(result$design_df %>% filter(ChoiceSet <= 3))

# Save design
write.csv(result$design_df, "athens_sp_design.csv", row.names = FALSE)
cat("\n✓ Design saved to: athens_sp_design.csv\n")

# ============================================================================
# 6. DESIGN DIAGNOSTICS
# ============================================================================

design_diagnostics <- function(design_df) {
  #' Print design diagnostics and attribute balance
  #' 
  #' @param design_df Data frame with design
  
  cat("\n")
  cat("="=rep(70), "\n")
  cat("DESIGN DIAGNOSTICS\n")
  cat("="=rep(70), "\n\n")
  
  cat("Attribute level frequencies:\n\n")
  
  for (attr in c("Mode", "Speed", "Flexibility", "Environmental")) {
    cat(sprintf("%s:\n", attr))
    print(table(design_df[[attr]]))
    cat("\n")
  }
  
  cat("Cost distribution:\n")
  print(summary(as.numeric(gsub("€", "", design_df$Cost))))
}

# Run diagnostics
design_diagnostics(result$design_df)

# ============================================================================
# 7. ALTERNATIVE: SIMPLIFIED APPROACH WITHOUT IDEFIX
# ============================================================================

generate_simple_adaptive_design <- function(configurator_choice,
                                           n_choice_sets = 8,
                                           seed = 42) {
  #' Simplified adaptive design without idefix package
  #' Uses heuristic approach with configurator preferences
  #' 
  #' @param configurator_choice Participant's configurator choices
  #' @param n_choice_sets Number of choice sets
  #' @param seed Random seed
  #' @return Data frame with choice sets
  
  set.seed(seed)
  
  # Update preferences based on configurator
  priors <- update_priors_from_configurator(configurator_choice,
                                           prior_means,
                                           prior_sds)
  
  # Generate choice sets with strategic variation
  design_list <- list()
  
  for (i in 1:n_choice_sets) {
    # Create 3 alternatives with variation
    choice_set <- data.frame(
      ChoiceSet = i,
      Alternative = c("A", "B", "C"),
      
      # Vary mode around configurator preference
      Mode = sample(c(configurator_choice$mode, 
                     setdiff(attributes$mode, configurator_choice$mode)), 3),
      
      # Vary cost with range
      Cost = paste0("€", sample(attributes$cost, 3)),
      
      # Include configurator time window in one alternative
      TimeWindow = paste0(sample(c(configurator_choice$time_window, 
                                  sample(setdiff(attributes$time_window, 
                                                configurator_choice$time_window), 2)), 
                                3), "h"),
      
      # Mix speeds
      Speed = sample(attributes$speed, 3),
      
      # Include preferred flexibility
      Flexibility = sample(c(configurator_choice$flexibility,
                           sample(attributes$flexibility, 2, replace = TRUE)), 3),
      
      # Mix environmental options
      Environmental = sample(attributes$environmental, 3, replace = TRUE)
    )
    
    design_list[[i]] <- choice_set
  }
  
  bind_rows(design_list)
}

# Test simplified version
cat("\n\n")
cat("="=rep(70), "\n")
cat("SIMPLIFIED ADAPTIVE DESIGN (NO IDEFIX)\n")
cat("="=rep(70), "\n\n")

simple_design <- generate_simple_adaptive_design(configurator_choice, 
                                                 n_choice_sets = 8,
                                                 seed = 42)

print(simple_design %>% filter(ChoiceSet <= 3))
write.csv(simple_design, "athens_sp_design_simple.csv", row.names = FALSE)
