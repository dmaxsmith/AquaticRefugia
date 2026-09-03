################################################################################
# CLIMATE REFUGIA COMPOSITE INDEX: Optimization and Sensitivity Analysis
#
# This script performs:
# 1. Prep Data
# 2. Weight optimization for composite index using LOPT
# 3. Define Composite index function for calculation and standardization
# 4. Leave-One-Out (LOO) sensitivity analysis to compare variable influence
# 5. Comparison of 3 types of weighting options: PCA, inverse variance, even distribution (sensitivity analysis) (added 3.23.36).
# 6. Visualization for #2-5
# 7. Report of Results
# 8. Tables of Results
# 9. Text write up of Results
#
# Author: M Friggens
# Finalized: 2026-03-25


library(nloptr)
library(tidyverse)
#Define dataset with final variables already standardized (Z scores)
macrZ<-AmphRep_Z_clim
hydrZ<-AmphRep_Z_hydro

macrZ<-ColdWater_Z_clim
hydrZ<-ColdWater_Z_hydro

macrZ<-EphAmp_Z_clim
hydrZ<-EphAmp_Z_hydro

macrZ<-PerStr_Z_clim
hydrZ<-PerStr_Z_hydro

macrZ<-springs_Z_clim
hydrZ<-springs_Z_hydro

################################################################################
# WEIGHT OPTIMIZATION FUNCTION
################################################################################
## original code ran for first draft of paper. 
#This defined function and then the min_ and max_weights can be changed in actual run if needed
 
#define function

optimize_composite_index <- function(data, 
                                     min_weight = 0.01,
                                     max_weight = NULL,
                                     seed = 123,
                                     verbose = TRUE) {
  # Set random seed for reproducibility
  set.seed(seed)
  
  # Number of variables
  n_vars <- ncol(data)
  
  # If max_weight not specified, calculate dynamically but ensure it's reasonable
  if (is.null(max_weight)) {
    max_weight <- min(0.5, max(0.2, 1 / (n_vars * 1.5)))
  }
  
  if (verbose) {
    cat("Optimization Parameters:\n")
    cat("Number of variables:", n_vars, "\n")
    cat("Minimum weight:", min_weight, "\n")
    cat("Maximum weight:", max_weight, "\n")
  }
  
  # Calculate variable characteristics
  var_characteristics <- data.frame(
    variance = apply(data, 2, var),
    mean_correlation = apply(cor(data), 2, function(x) mean(abs(x[-which(x == 1)]))),
    cv = apply(data, 2, function(x) sd(x)/mean(abs(x)))
  )
  
  # Normalize characteristics to 0-1 scale
  var_characteristics <- apply(var_characteristics, 2, function(x) {
    (x - min(x)) / (max(x) - min(x))
  })
  
  # Generate valid initial weights that respect bounds
  initial_weights <- rowMeans(var_characteristics)
  # Ensure initial weights are within bounds
  initial_weights <- pmin(max_weight, pmax(min_weight, initial_weights))
  # Normalize to sum to 1
  initial_weights <- initial_weights / sum(initial_weights)
  
  if (verbose) {
    cat("\nInitial weights check:\n")
    cat("Min weight:", min(initial_weights), "\n")
    cat("Max weight:", max(initial_weights), "\n")
    cat("Sum of weights:", sum(initial_weights), "\n")
  }
  
  # Objective function
  objective_function <- function(weights) {
    # Normalize weights to sum to 1
    weights <- weights / sum(weights)
    
    # Characteristic-based penalty
    char_penalty <- sum((weights - rowMeans(var_characteristics))^2)
    
    # Correlation penalty
    cor_matrix <- cor(data)
    cor_penalty <- sum(weights %*% t(weights) * abs(cor_matrix))
    
    return(char_penalty + 0.5 * cor_penalty)
  }
  
  # Gradient function
  objective_gradient <- function(weights) {
    grad <- numeric(length(weights))
    h <- 1e-8
    
    for (i in 1:length(weights)) {
      perturbed <- weights
      perturbed[i] <- perturbed[i] + h
      perturbed <- perturbed / sum(perturbed)
      
      grad[i] <- (objective_function(perturbed) - 
                    objective_function(weights)) / h
    }
    
    return(grad)
  }
  #equality contraint function (sum to 1)
  equality_contraint = function(weights){
    return(sum(weights)-1)
  }
  #gradient of equality contraint
  #for sum(wights)=1, the gradient is a vector of ones
  equality_contraint_gradient = 
    function(weights){
      return(rep(1,length(weights)))
    }
  # Ensure bounds are properly set
  lb <- rep(min_weight, n_vars)
  ub <- rep(max_weight, n_vars)
  
  # Optimization with proper bounds
  result <- tryCatch({
    nloptr::nloptr(
      x0 = initial_weights,
      eval_f = objective_function,
      eval_grad_f = objective_gradient,
      lb = lb,
      ub = ub,
      # eval_g_eq = function(x) sum(x) - 1,  # Sum to 1 constraint
      eval_g_eq = equality_contraint,
      eval_jac_g_eq = equality_contraint_gradient, #added gradient for equality contraint
      opts = list(
        "algorithm" = "NLOPT_LD_SLSQP",
        "xtol_rel" = 1e-10,
        "maxeval" = 5000
      )
    )
  }, error = function(e) {
    if (verbose) {
      cat("\nOptimization error:", e$message, "\n")
      cat("Trying alternative optimization...\n")
    }
    # Fallback to simpler optimization if needed
    nloptr::nloptr(
      x0 = rep(1/n_vars, n_vars),  # Start with equal weights
      eval_f = objective_function,
      lb = lb,
      ub = ub,
      eval_g_eq = function(x) sum(x) - 1,
      opts = list(
        "algorithm" = "NLOPT_LN_COBYLA",
        "xtol_rel" = 1e-10,
        "maxeval" = 5000
      )
    )
  })
  
  # Process results
  optimized_weights <- result$solution
  optimized_weights <- pmax(min_weight, pmin(max_weight, optimized_weights))
  optimized_weights <- optimized_weights / sum(optimized_weights)
  
  # Calculate composite index
  composite_index <- as.matrix(data) %*% optimized_weights
  normalized_index <- (composite_index - min(composite_index)) / 
    (max(composite_index) - min(composite_index))
  
  # Create output
  output <- list(
    weights = optimized_weights,
    weight_details = data.frame(
      variable = colnames(data),
      weight = optimized_weights,
      variance = var_characteristics[, "variance"],
      mean_correlation = var_characteristics[, "mean_correlation"],
      cv = var_characteristics[, "cv"],
      stringsAsFactors = FALSE
    ),
    composite_index = as.vector(normalized_index),
    optimization_details = list(
      objective_value = result$objective,
      iterations = result$iterations,
      status = result$status
    )
  )
  
  if (verbose) {
    cat("\nOptimized Weights and Variable Characteristics:\n")
    print(output$weight_details)
    
    cat("\nWeight Statistics:\n")
    print(summary(optimized_weights))
  }
  
  return(output)
}



#############Run optimization
######### Run OPTIMIZATION (produces weights, function optimize_composite_index also calculates and standardizes composite index)

macrodata =subset(macrZ, select=-c(1)) #removing huc12 designations
hydrodata =subset(hydrZ, select=-c(1)) #removing huc12 designations
## using original min and max here.
macroresult <- optimize_composite_index(macrodata, 
                                   min_weight = 0.05,  # Minimum 0.5 % weight
                                   max_weight = 0.3,   # Maximum 30% weight
                                   verbose = TRUE)

hydroresult <- optimize_composite_index(hydrodata, 
                                        min_weight = 0.05,  # Minimum 0.5 % weight
                                        max_weight = 0.3,   # Maximum 30% weight
                                        verbose = TRUE)

################################################################################
#  DEFINE BASELINE WEIGHTS to provide output needed by LOO code
################################################################################


## define indicator_names based on dataset
macroindicators <- macrZ[, -c(1)] #removing huc12 designations
macroindicator_names <- colnames(macroindicators)

hydroindicators <- hydrZ[, -c(1)] #removing huc12 designations
hydroindicator_names <- colnames(hydroindicators)

#define new designations for results produced in optimization
macrobaseline_weights <- macroresult$weights
names(macrobaseline_weights) <- macroindicator_names
macrobaseline_composite <- macroresult$composite_index #already normalized 0-1

hydrobaseline_weights <- hydroresult$weights
names(hydrobaseline_weights) <- hydroindicator_names
hydrobaseline_composite <- hydroresult$composite_index #already normalized 0-1
################################################################################
# CREATE COMPOSITE INDEX FUNCTION
###############################################################################
## This creates index in format that can be used by LOO. Renames things to keep analysis outputs clear

create_composite_index <- function(data, weights) {
  # Calculate weighted sum
  composite <- as.matrix(data) %*% weights
  
  # Standardize to 0-1 range
  normalized <- (composite - min(composite)) / (max(composite) - min(composite))
  
  return(as.vector(normalized))
}
# Calculate baseline composite index
macrobaseline_composite <- create_composite_index(macroindicators, macrobaseline_weights)

hydrobaseline_composite <- create_composite_index(hydroindicators, hydrobaseline_weights)
################################################################################
# LEAVE-ONE-OUT SENSITIVITY ANALYSIS
################################################################################

# define function. 

leave_one_out_analysis <- function(indicators, indicator_names,
                                   baseline_weights, baseline_composite,
                                   refugia_percentile = 80) {
  
  n_indicators <- ncol(indicators)
  
  # Define baseline refugia areas
  baseline_refugia <- baseline_composite >= quantile(baseline_composite,
                                                     refugia_percentile/100)
  
  results <- list()
  loo_composites <- list()
  
  # Loop through each indicator
  for (i in 1:n_indicators) {
    
    # Remove indicator i
    indicators_reduced <- indicators[, -i, drop = FALSE]
    
    # Re-optimize weights for reduced set
    weights_reduced <- optimize_composite_index(indicators_reduced,
                                                min_weight = 0.05,
                                                max_weight = 0.5)$weights
#NOTE: due to low number of indicators, max_weight has to be increased to 0.5 to succeed for amphibians. Change above line to = 0.3 if not an issue    
    
    # Create composite from reduced set
    composite_reduced <- create_composite_index(indicators_reduced, weights_reduced)
    
    # Define refugia with same percentile threshold
    # refugia_percentile is defined as = 80 in function so this is designating top 20% as "refugia_reduced"
    refugia_reduced <- composite_reduced >= quantile(composite_reduced,
                                                     refugia_percentile/100)
    
    # Calculate correlation metrics
    # Correlations for LOO are comparing baseline (all variables, optimized) to each subsequent set (variables - 1, optimized)
    # High correlations indicate similar outcomes across that iteration and the baseline
    pearson_r <- cor(baseline_composite, composite_reduced, method = "pearson")
    #Pearson correlation measures linear relationship between variables (actual CI values) assumed to have normal distribution
    #based on covariance and stdev
    
    spearman_r <- cor(baseline_composite, composite_reduced, method = "spearman")
    #Spearman correlations measure monotonic relationships. Best when variables are not normal or are ordinal
    #Ranks data and calculates difference in ranks
    
    # Percent change in mean composite value
    # Different than percent change in area. It is simply calculating difference in value
    # I don't think this is meaningful for our analysis
    pct_change_mean <- 100 * (mean(composite_reduced) - mean(baseline_composite)) /
      mean(baseline_composite)
   
    # Spatial overlap metrics
    # Calculating overlap in top 20% (i.e.) "refugia"; overlap= sum of huc designated as refugia by both baseline and reduced 
    overlap <- sum(baseline_refugia & refugia_reduced)
    baseline_area <- sum(baseline_refugia)
    reduced_area <- sum(refugia_reduced)
    
    #Jaccard is producing index similar to percent change. It compares the number of HUC that intersect to the number of HUC in a union of HUC that do not interset. 
    #0 means no HUC in common, 1 indicates each analysis has identical HUC
    #baseline and reduced_area are calculated from refugia variable (i.e. top 20%)
    #So Jaccard refers to the degree to which high ranking HUC are the same.
    jaccard <- overlap / (baseline_area + reduced_area - overlap)
    pct_overlap <- 100 * overlap / baseline_area
    pct_area_change <- 100 * (reduced_area - baseline_area) / baseline_area
    
    # Store results
    results[[i]] <- data.frame(
      Removed_Indicator = indicator_names[i],
      Pearson_r = pearson_r,
      Spearman_r = spearman_r,
      Pct_Change_Mean = pct_change_mean,
      Jaccard_Index = jaccard,
      Pct_Spatial_Overlap = pct_overlap,
      Pct_Area_Change = pct_area_change,
      Baseline_Refugia_Pixels = baseline_area,
      Reduced_Refugia_Pixels = reduced_area,
      stringsAsFactors = FALSE
    )
    
    loo_composites[[indicator_names[i]]] <- composite_reduced
  }
  
  results_df <- do.call(rbind, results)
  
  return(list(
    results = results_df,
    composites = loo_composites
  ))
}

################# Run LOO analysis
macroloo_analysis <- leave_one_out_analysis(
  indicators = macroindicators,
  indicator_names = macroindicator_names,
  baseline_weights = macrobaseline_weights,
  baseline_composite = macrobaseline_composite,
  refugia_percentile = 80
)

hydroloo_analysis <- leave_one_out_analysis(
  indicators = hydroindicators,
  indicator_names = hydroindicator_names,
  baseline_weights = hydrobaseline_weights,
  baseline_composite = hydrobaseline_composite,
  refugia_percentile = 80
)

#Note: like max and min weights, refugia_percentile is called in run and could be changed here. Changes here will override defaults in original function
############### print results

macroloo_results <- macroloo_analysis$results
macroloo_composites <- macroloo_analysis$composites

print(macroloo_results, row.names = FALSE, digits = 3)

hydroloo_results <- hydroloo_analysis$results
hydroloo_composites <- hydroloo_analysis$composites

print(hydroloo_results, row.names = FALSE, digits = 3)
# Save results

write.csv(macroloo_results, "AmpRepMacro_loo_results.csv", row.names = FALSE)
write.csv(hydroloo_results, "AmpRepHydro_loo_results.csv", row.names = FALSE)

write.csv(macroloo_results, "ColdwaterMacro_loo_results.csv", row.names = FALSE)
write.csv(hydroloo_results, "ColdwaterHydro_loo_results.csv", row.names = FALSE)

write.csv(macroloo_results, "EphCatMacro_loo_results.csv", row.names = FALSE)
write.csv(hydroloo_results, "EphCatHydro_loo_results.csv", row.names = FALSE)

write.csv(macroloo_results, "PerStreamsMacro_loo_results.csv", row.names = FALSE)
write.csv(hydroloo_results, "PerStreamsHydro_loo_results.csv", row.names = FALSE)

write.csv(macroloo_results, "SpringsMacro_loo_results.csv", row.names = FALSE)
write.csv(hydroloo_results, "SpringsHydro_loo_results.csv", row.names = FALSE)
##########################################################################
# ALTERNATIVE WEIGHTING SCHEMES COMPARISON
##########################################################################

#### define function

compare_weighting_schemes <- function(indicators, indicator_names,
                                      baseline_weights = NULL,
                                      refugia_percentile = 80) {
  
  n_indicators <- ncol(indicators)
  
  # 1. Optimized weights (passed in or computed) (names correspond to function call made for LOO analysis. e.g. "baseline_weights")
  if (is.null(baseline_weights)) {
    baseline_weights <- optimize_composite_index(indicators,
                                                 min_weight = 0.05,
                                                 max_weight = 0.3,
                                                 verbose    = FALSE)$weights
  }
  #Naming things for plots and comparisons
  #NOTE: ran this with original max_weight value.
  names(baseline_weights) <- indicator_names
  
  # 2. Equal arithmetic weights
  equal_weights        <- rep(1 / n_indicators, n_indicators)
  names(equal_weights) <- indicator_names
  
  # 3. PCA-derived weights (first PC loadings)
  pca_result          <- prcomp(indicators, center = TRUE, scale. = TRUE)
  pca_weights         <- abs(pca_result$rotation[, 1])
  pca_weights         <- pca_weights / sum(pca_weights)
  names(pca_weights)  <- indicator_names
  
  # 4. Inverse variance weights. Can disregard! Not relevant if you have already standardized indicators (e.g. using Z scores)
  variances           <- apply(indicators, 2, var)
  inv_var_weights     <- 1 / variances
  inv_var_weights     <- inv_var_weights / sum(inv_var_weights)
  names(inv_var_weights) <- indicator_names
  
  schemes <- list(
    Optimized_Equal_Contribution = baseline_weights,
    Equal_Arithmetic             = equal_weights,
    PCA_First_Component          = pca_weights,
    Inverse_Variance             = inv_var_weights
  )
  
#Creating composite indices for each weighting scheme
  composites    <- list()
  refugia_masks <- list()
  
  for (name in names(schemes)) {
    comp                 <- create_composite_index(indicators, schemes[[name]])
    composites[[name]]   <- comp
    refugia_masks[[name]] <- comp >= quantile(comp, refugia_percentile / 100)
    cat("  ", name, ": Created\n")
  }
  
  baseline_name    <- "Optimized_Equal_Contribution"
  baseline_comp    <- composites[[baseline_name]]
  baseline_refugia <- refugia_masks[[baseline_name]] #calculating top 20%
  baseline_area    <- sum(baseline_refugia) #counting number of huc = "refugia"
  
  results <- list()
  
  for (name in names(schemes)) {
    if (name == baseline_name) next
    
    comp         <- composites[[name]]
    refugia      <- refugia_masks[[name]]
    refugia_area <- sum(refugia)
    
    pearson_r  <- cor(baseline_comp, comp, method = "pearson")
    spearman_r <- cor(baseline_comp, comp, method = "spearman")
    
    overlap        <- sum(baseline_refugia & refugia)
    pct_agreement  <- 100 * overlap / baseline_area #How much of refugia defined in first method is also in 2nd method
    jaccard        <- overlap / (baseline_area + refugia_area - overlap) #same meaning as before. How much refugia HUC intersect
    pct_area_diff  <- 100 * (refugia_area - baseline_area) / baseline_area #similar to pct_change in ppt. How different from baseline? 30% loss, 50% increase, etc
    
    results[[name]] <- data.frame(
      Weighting_Scheme     = name,
      Pearson_r            = pearson_r,
      Spearman_r           = spearman_r,
      Pct_Spatial_Agreement = pct_agreement,
      Jaccard_Index        = jaccard,
      Refugia_Area_Pixels  = refugia_area,
      Pct_Area_Difference  = pct_area_diff,
      stringsAsFactors     = FALSE
    )
  }
  
  comparison_df <- do.call(rbind, results)
  
  # Weights long-format table
  weights_data <- list()
  for (name in names(schemes)) {
    for (i in 1:n_indicators) {
      weights_data[[length(weights_data) + 1]] <- data.frame(
        Weighting_Scheme = name,
        Indicator        = indicator_names[i],
        Weight           = schemes[[name]][i],
        stringsAsFactors = FALSE
      )
    }
  }
  weights_df <- do.call(rbind, weights_data)
  
  variance_explained <- summary(pca_result)$importance[2, ]
  
  return(list(
    comparison   = comparison_df,
    composites   = composites,
    weights      = weights_df,
    pca_variance = variance_explained
  ))
}

macroscheme_analysis    <- compare_weighting_schemes(
  indicators         = macroindicators,
  indicator_names    = macroindicator_names,
  baseline_weights   = macrobaseline_weights
)

hydroscheme_analysis    <- compare_weighting_schemes(
  indicators         = hydroindicators,
  indicator_names    = hydroindicator_names,
  baseline_weights   = hydrobaseline_weights
)

macrocomparison_results <- macroscheme_analysis$comparison
macroscheme_composites  <- macroscheme_analysis$composites
macroweights_comparison <- macroscheme_analysis$weights
macropca_variance       <- macroscheme_analysis$pca_variance

hydrocomparison_results <- hydroscheme_analysis$comparison
hydroscheme_composites  <- hydroscheme_analysis$composites
hydroweights_comparison <- hydroscheme_analysis$weights
hydropca_variance       <- hydroscheme_analysis$pca_variance

write.csv(macrocomparison_results, "AmpRepMacro_weighting_schemes_comparison.csv", row.names = FALSE)
write.csv(macroweights_comparison,  "AmpRepMacro_weights_by_scheme.csv",           row.names = FALSE)
write.csv(hydrocomparison_results, "AmpRepHydro_weighting_schemes_comparison.csv", row.names = FALSE)
write.csv(hydroweights_comparison,  "AmpRepHydro_weights_by_scheme.csv",           row.names = FALSE)

write.csv(macrocomparison_results, "ColdwaterMacro_weighting_schemes_comparison.csv", row.names = FALSE)
write.csv(macroweights_comparison,  "ColdwaterMacro_weights_by_scheme.csv",           row.names = FALSE)
write.csv(hydrocomparison_results, "ColdwaterHydro_weighting_schemes_comparison.csv", row.names = FALSE)
write.csv(hydroweights_comparison,  "ColdwaterHydro_weights_by_scheme.csv",           row.names = FALSE)

write.csv(macrocomparison_results, "EphCatMacro_weighting_schemes_comparison.csv", row.names = FALSE)
write.csv(macroweights_comparison,  "EphCatMacro_weights_by_scheme.csv",           row.names = FALSE)
write.csv(hydrocomparison_results, "EphCatHydro_weighting_schemes_comparison.csv", row.names = FALSE)
write.csv(hydroweights_comparison,  "EphCatHydro_weights_by_scheme.csv",           row.names = FALSE)

write.csv(macrocomparison_results, "PerStreamMacro_weighting_schemes_comparison.csv", row.names = FALSE)
write.csv(macroweights_comparison,  "PerStreamMacro_weights_by_scheme.csv",           row.names = FALSE)
write.csv(hydrocomparison_results, "PerStreamHydro_weighting_schemes_comparison.csv", row.names = FALSE)
write.csv(hydroweights_comparison,  "PerStreamHydro_weights_by_scheme.csv",           row.names = FALSE)

write.csv(macrocomparison_results, "SpringsMacro_weighting_schemes_comparison.csv", row.names = FALSE)
write.csv(macroweights_comparison,  "SpringsMacro_weights_by_scheme.csv",           row.names = FALSE)
write.csv(hydrocomparison_results, "SpringsHydro_weighting_schemes_comparison.csv", row.names = FALSE)
write.csv(hydroweights_comparison,  "SpringsHydro_weights_by_scheme.csv",           row.names = FALSE)

################################################################################
#SPATIAL CONCORDANCE ANALYSIS
################################################################################

create_concordance_analysis <- function(composites_list, refugia_percentile = 80) {
  
  refugia_masks <- list()
  for (name in names(composites_list)) {
    comp                  <- composites_list[[name]]
    refugia_masks[[name]] <- as.integer(comp >= quantile(comp, refugia_percentile / 100))
  }
  
  concordance  <- Reduce("+", refugia_masks)
  n_scenarios  <- length(composites_list)
  #These essentially are computing stats related to proportion of previous iterations have same HUC in refugia
  concordance_stats <- list(
    total_scenarios    = n_scenarios,
    all_scenarios      = sum(concordance == n_scenarios),
    pct_all_scenarios  = 100 * sum(concordance == n_scenarios) / length(concordance),
    most_scenarios     = sum(concordance >= 0.8 * n_scenarios),
    pct_most_scenarios = 100 * sum(concordance >= 0.8 * n_scenarios) / length(concordance),
    few_scenarios      = sum(concordance <= 0.2 * n_scenarios),
    pct_few_scenarios  = 100 * sum(concordance <= 0.2 * n_scenarios) / length(concordance),
    mean_concordance   = mean(concordance),
    median_concordance = median(concordance)
  )
  
  return(list(concordance = concordance, stats = concordance_stats))
}

all_macrocomposites <- c(
  list(Baseline = macrobaseline_composite),
  macroloo_composites,
  macroscheme_composites
)

concordance_analysis <- create_concordance_analysis(
  composites_list    = all_macrocomposites,
  refugia_percentile = 80
)

concordance       <- concordance_analysis$concordance
concordance_stats <- concordance_analysis$stats

saveRDS(concordance, "macrorefugia_concordance.rds")

all_hydrocomposites <- c(
  list(Baseline = hydrobaseline_composite),
  hydroloo_composites,
  hydroscheme_composites
)

concordance_analysis <- create_concordance_analysis(
  composites_list    = all_hydrocomposites,
  refugia_percentile = 80
)

concordance       <- concordance_analysis$concordance
concordance_stats <- concordance_analysis$stats

saveRDS(concordance, "hydrorefugia_concordance.rds")


########################################
############### VISUALIZATION#########
##########################################

###############################################################################
# PLOT BASELINE WEIGHTS
################################################################################
######## Gives optimized weights with equal weight as a red line

png("baseline_weights.png", width = 2400, height = 1600, res = 300)
par(mar = c(8, 5, 4, 2))
barplot(macrobaseline_weights,
        names.arg = macroindicator_names,
        las = 2,
        col = "steelblue",
        ylab = "Weight",
        main = "Optimized Indicator Weights",
        ylim = c(0, max(macrobaseline_weights) * 1.2))
abline(h = 1/length(macroindicator_names), col = "red", lty = 2, lwd = 2)
legend("topright",
       legend = sprintf("Equal weight (%.3f)", 1/length(macroindicator_names)),
       col = "red", lty = 2, lwd = 2)
dev.off()


################################################################################
# 8. VISUALIZE LOO RESULTS
################################################################################

################# name elements 
# Equal weights
equal_weights <- rep(1/ncol(macroindicators), ncol(macroindicators))
names(equal_weights) <- macroindicator_names

# Equal-weight composite; function defined in LOO_Ecological Optimizaiton
equal_composite <- create_composite_index(macroindicators, equal_weights)

# Compare the two
pearson_r <- cor(baseline_composite, equal_composite, method = "pearson")
spearman_r <- cor(baseline_composite, equal_composite, method = "spearman")

# Refugia overlap, using top 20%
baseline_refugia <- baseline_composite >= quantile(baseline_composite, 0.80)
equal_refugia <- equal_composite >= quantile(equal_composite, 0.80)

overlap <- sum(baseline_refugia & equal_refugia)
jaccard <- overlap / (sum(baseline_refugia) + sum(equal_refugia) - overlap)
pct_overlap <- 100 * overlap / sum(baseline_refugia)


################
# 1. Scatter plot - baseline vs equal weight composite. 
################
### specific to the optimized and equal weight comparisons. Could do for others by changing name in plot line. 
### each dot = HUC
#### Will provide interpretation with example graphs for more info. For now, what you want is tight to line. Clustering at one end
#### or another indicates spatial trends in HUC scores. E.g. reptiles clustered in lower values reflecting fewer high scoring huc

png("equal_vs_optimized_composite.png", width = 2400, height = 2400, res = 300)
par(mar = c(5, 5, 4, 2))
plot(baseline_composite, equal_composite,
     xlab = "Optimized Composite Index",
     ylab = "Equal Weight Composite Index",
     main = "Optimized vs Equal Weight Composite\n",
     pch = 16, col = rgb(0, 0, 1, 0.3), cex = 0.5)
abline(0, 1, col = "red", lwd = 2, lty = 2)
legend("topleft",
       legend = c(sprintf("Pearson r = %.3f", pearson_r),
                  sprintf("Spearman r = %.3f", spearman_r)),
       bty = "n")
dev.off()


###################
# 2. Spatial overlap visualization
##################
# without category neither, because most fall within this category (skews bar chart).If Neither desired add another ifelse 
## ifelse((!baseline_refugia & !equal_refugia, "Neither") and a color in overlap_colors 
## overlap_colors <- c(...., "Neither" = "black")

#This shows the number of huc that are considered refugia for both and individually. Higher both is better congruence
# specific to optimized vs equal comparisons but can be made for others by changing names in ifelse lines
# best use is comparative (e.g. compare plots among your grous to see which show more or less "unique" HUC)

png("refugia_overlap.png", width = 2400, height = 1600, res = 300)
par(mar = c(6, 5, 4, 2))

overlap_category <- ifelse(baseline_refugia & equal_refugia, "Both",
                           ifelse(baseline_refugia & !equal_refugia, "Optimized only",
                                  ifelse(!baseline_refugia & equal_refugia, "Equal only",
                                         NA)))

overlap_colors <- c("Both" = "darkgreen",
                    "Optimized only" = "steelblue",
                    "Equal only" = "darkorange"
)

barplot(table(overlap_category),
        col = overlap_colors,
        ylab = "Number of Pixels",
        main = sprintf("Refugia Agreement\nJaccard = %.3f, Overlap = %.1f%%",
                       jaccard, pct_overlap),
        las = 1)
dev.off()

################################################################################
# VISUALIZE WEIGHTING SCHEMES COMPARISON
################################################################################
##########
##side by side bar plots of weights for each method. Note for current analysis equal weighting and inverse are always equal
### consider modifying to eliminate inverse elements
### busy graphic and I cannot get the second barplot to look right. Still, might be of interet to see how differnt methods
### treat indicators.
###########
png("weighting_schemes_comparison.png", width = 3600, height = 1800, res = 300)
par(mfrow = c(1, 2), mar = c(10, 5, 4, 2))

weights_matrix          <- as.matrix(weights_pivot[, -1])
rownames(weights_matrix) <- weights_pivot$Indicator

barplot(t(weights_matrix),
        beside      = TRUE,
        las         = 2,
        col         = c("steelblue", "coral", "forestgreen", "darkorange"),
        ylab        = "Weight",
        main        = "Indicator Weights by Scheme",
        legend.text = colnames(weights_matrix),
        args.legend = list(x = "topright", cex = 0.7))
abline(h = 1 / length(indicator_names), col = "red", lty = 2)

x_pos <- barplot(comparison_results$Pearson_r,
                 names.arg = gsub("_", " ", comparison_results$Weighting_Scheme),
                 las  = 2,
                 col  = "steelblue",
                 ylab = "Value",
                 main = "Agreement with Optimized Baseline",
                 ylim = c(0.8, 1.05)) # this should be adjusted to account for x-axis on 0.
points(x_pos, comparison_results$Pct_Spatial_Agreement / 100,
       pch = 19, col = "coral", cex = 1.5)
abline(h = 0.9, col = "red", lty = 2)
legend("bottomright",
       legend = c("Pearson r", "Spatial Agreement (fraction)"),
       pch    = c(15, 19),
       col    = c("steelblue", "coral"))

dev.off()

################################################################################
# VISUALIZE CONCORDANCE DISTRIBUTION
################################################################################

########
#Creating concordance visualizations
############
### I will break this up in future runs. I prefer the visualization from the first barplot but it may just depend on the scenarios
### you will have alot more and the result might look better with graphics on right.
png("refugia_concordance_distribution.png", width = 3600, height = 1800, res = 300)
par(mfrow = c(1, 2), mar = c(6, 5, 4, 2))

hist_data <- hist(concordance,
                  breaks = seq(-0.5, length(all_composites) + 0.5, by = 1),
                  plot   = FALSE)
barplot(hist_data$counts,
        names.arg = 0:length(all_composites),
        col  = "steelblue",
        xlab = "Number of Scenarios Identifying as Refugia",
        ylab = "Frequency (pixels)",
        main = "Refugia Concordance Distribution")

total_pixels <- length(concordance)
for (i in 1:length(hist_data$counts)) {
  if (hist_data$counts[i] > 0) {
    pct <- 100 * hist_data$counts[i] / total_pixels
    text(i - 0.5, hist_data$counts[i],
         labels = sprintf("%.1f%%", pct),
         pos = 3, cex = 0.7)
  }
}

sorted_concordance <- sort(concordance, decreasing = TRUE)
cumulative_pct     <- 100 * (1:length(sorted_concordance)) / length(sorted_concordance)

plot(sorted_concordance, cumulative_pct,
     type = "l", lwd = 2, col = "steelblue",
     xlab = "Number of Scenarios Identifying as Refugia",
     ylab = "Cumulative % of Pixels",
     main = "Cumulative Concordance Distribution",
     xlim = c(0, length(all_composites)),
     ylim = c(0, 100))
grid()
abline(h = 10,                          col = "red",    lty = 2, lwd = 2)
abline(v = 0.8 * length(all_composites), col = "orange", lty = 2, lwd = 2)
legend("bottomleft",
       legend = c("Top 20% of pixels", "80% of scenarios"),
       col = c("red", "orange"), lty = 2, lwd = 2)

dev.off()


########################################
## GENERATE REPORT
#######################################

## Format structure

summary_report <- sprintf("
%s
CLIMATE REFUGIA SENSITIVITY ANALYSIS - SUMMARY REPORT- n
%s

DATE: %s

DATA SUMMARY:
-------------
Number of pixels: %s
Number of indicators: %d
Indicators: %s
Refugia definition: Top 20%% of composite index values

BASELINE WEIGHTS (Optimized Equal Contribution):
------------------------------------------------
%s

LEAVE-ONE-OUT ANALYSIS RESULTS:
-------------------------------
Mean Pearson correlation: %.3f
  Range: %.3f - %.3f

Mean spatial overlap: %.1f%%
  Range: %.1f%% - %.1f%%

Mean Jaccard index: %.3f
  Range: %.3f - %.3f

Most influential indicator: %s
Least influential indicator: %s

ALTERNATIVE WEIGHTING SCHEMES:
------------------------------
Mean Pearson correlation with baseline: %.3f
  Range: %.3f - %.3f

Mean spatial agreement: %.1f%%
  Range: %.1f%% - %.1f%%

SPATIAL CONCORDANCE:
-------------------
Total scenarios analyzed: %d
Pixels in all scenarios: %d (%.2f%%)
Pixels in >=80%% scenarios: %d (%.2f%%)

INTERPRETATION:
--------------
",
                          paste(rep("=", 80), collapse = ""),
                          paste(rep("=", 80), collapse = ""),
                          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                          format(nrow(indicators), big.mark = ","),
                          ncol(indicators),
                          paste(indicator_names, collapse = ", "),
                          paste(sprintf("%-30s %.4f (%.2f%%)",
                                        indicator_names, baseline_weights, baseline_weights * 100),
                                collapse = "\n"),
                          mean(loo_results$Pearson_r),
                          min(loo_results$Pearson_r),
                          max(loo_results$Pearson_r),
                          mean(loo_results$Pct_Spatial_Overlap),
                          min(loo_results$Pct_Spatial_Overlap),
                          max(loo_results$Pct_Spatial_Overlap),
                          mean(loo_results$Jaccard_Index),
                          min(loo_results$Jaccard_Index),
                          max(loo_results$Jaccard_Index),
                          loo_results$Removed_Indicator[which.min(loo_results$Pearson_r)],
                          loo_results$Removed_Indicator[which.max(loo_results$Pearson_r)],
                          mean(comparison_results$Pearson_r),
                          min(comparison_results$Pearson_r),
                          max(comparison_results$Pearson_r),
                          mean(comparison_results$Pct_Spatial_Agreement),
                          min(comparison_results$Pct_Spatial_Agreement),
                          max(comparison_results$Pct_Spatial_Agreement),
                          concordance_stats$total_scenarios,
                          concordance_stats$all_scenarios,
                          concordance_stats$pct_all_scenarios,
                          concordance_stats$most_scenarios,
                          concordance_stats$pct_most_scenarios
)

# Interpretation flags- provided where results fit certain criteria. NOTE: These are arbitrarily defined and may be changed by user
if (min(loo_results$Pearson_r) > 0.90) {
  summary_report <- paste0(summary_report,
                           "HIGH STABILITY: All correlations > 0.90 indicate robust index.\n")
} else if (min(loo_results$Pearson_r) > 0.85) {
  summary_report <- paste0(summary_report,
                           "MODERATE STABILITY: Most correlations > 0.85 suggest reasonable robustness.\n")
} else {
  summary_report <- paste0(summary_report,
                           "LOWER STABILITY: Some correlations < 0.85 suggest index sensitivity.\n")
}

if (min(loo_results$Pct_Spatial_Overlap) > 85) {
  summary_report <- paste0(summary_report,
                           "HIGH SPATIAL AGREEMENT: >85% overlap indicates stable refugia identification.\n")
} else if (min(loo_results$Pct_Spatial_Overlap) > 75) {
  summary_report <- paste0(summary_report,
                           "MODERATE SPATIAL AGREEMENT: >75% overlap suggests reasonable stability.\n")
} else {
  summary_report <- paste0(summary_report,
                           "VARIABLE SPATIAL PATTERNS: <75% overlap indicates sensitive refugia boundaries.\n")
}

if (min(comparison_results$Pearson_r) > 0.90) {
  summary_report <- paste0(summary_report,
                           "METHOD ROBUST: Alternative weighting schemes produce consistent results.\n")
}

summary_report <- paste0(summary_report, sprintf("

FILES GENERATED:
---------------
- loo_sensitivity_results.csv
- weighting_schemes_comparison.csv
- weights_by_scheme.csv
- refugia_concordance.rds
- baseline_weights.png
- equal_vs_optimized_composite.png
- refugia_overlap.png
- weighting_schemes_comparison.png
- refugia_concordance_distribution.png

%s
", paste(rep("=", 80), collapse = "")))

writeLines(summary_report, "sensitivity_analysis_summary_.txt")


################################################################################
# FORMATTED TABLES 
################################################################################


####### Table 1. Leave-One-Out Sensitivity Analysis Results
loo_table <- loo_results[, c("Removed_Indicator", "Pearson_r", "Spearman_r",
                             "Pct_Spatial_Overlap", "Jaccard_Index", "Pct_Area_Change")]
colnames(loo_table) <- c("Excluded Indicator", "Pearson r", "Spearman rho",
                         "Spatial Overlap (%)", "Jaccard Index", "Area Change (%)")
## I did not get values for Area change, which is probably why I was confused by my table when we reviewed. 
## I forget but there is a good reason for this..

print(loo_table, row.names = FALSE, digits = 3)


####### Table 2. Alternative Weighting Schemes Comparison
comp_table <- comparison_results[, c("Weighting_Scheme", "Pearson_r", "Spearman_r",
                                     "Pct_Spatial_Agreement", "Jaccard_Index",
                                     "Pct_Area_Difference")]
colnames(comp_table) <- c("Weighting Scheme", "Pearson r", "Spearman rho",
                          "Spatial Agreement (%)", "Jaccard Index", "Area Difference (%)")
comp_table$`Weighting Scheme` <- gsub("_", " ", comp_table$`Weighting Scheme`)

print(comp_table, row.names = FALSE, digits = 3)


write.csv(loo_table,   "table1_loo_results.csv",          row.names = FALSE)
write.csv(comp_table,  "table2_weighting_comparison.csv", row.names = FALSE)

################################################################################
# TEXT
################################################################################


results_text <- sprintf("
SENSITIVITY ANALYSIS RESULTS

Correlations between the full index and reduced indices (with one
indicator removed) ranged from %.3f to %.3f (Pearson) and %.3f to %.3f (Spearman),
with a mean of %.3f (Table 1). Spatial overlap of refugia areas between full and
reduced indices averaged %.1f%%, ranging from %.1f%% to %.1f%%. Jaccard similarity
indices ranged from %.3f to %.3f.

Removal of %s had the largest effect on the composite index (r = %.3f, spatial
overlap = %.1f%%), while removal of %s had minimal impact (r = %.3f, spatial
overlap = %.1f%%).

Correlations between alternative weighting and optimized baseline weighting range from
r = %.3f to %.3f; (Table 2) with spatial agreement from %.1f%% to %.1f%%. Refugia
area estimates varied by %.1f%% to %.1f%% across weighting schemes.

Spatial concordance analysis across all %d scenarios identified %s pixels (%.2f%%)
as core refugia areas recognized in all scenarios, and %s pixels (%.2f%%) identified
in >=80%% of scenarios. These core areas represent high-confidence refugia robust to
methodological choices.
",
                        min(loo_results$Pearson_r),
                        max(loo_results$Pearson_r),
                        min(loo_results$Spearman_r),
                        max(loo_results$Spearman_r),
                        mean(loo_results$Pearson_r),
                        mean(loo_results$Pct_Spatial_Overlap),
                        min(loo_results$Pct_Spatial_Overlap),
                        max(loo_results$Pct_Spatial_Overlap),
                        min(loo_results$Jaccard_Index),
                        max(loo_results$Jaccard_Index),
                        loo_results$Removed_Indicator[which.min(loo_results$Pearson_r)],
                        min(loo_results$Pearson_r),
                        loo_results$Pct_Spatial_Overlap[which.min(loo_results$Pearson_r)],
                        loo_results$Removed_Indicator[which.max(loo_results$Pearson_r)],
                        max(loo_results$Pearson_r),
                        loo_results$Pct_Spatial_Overlap[which.max(loo_results$Pearson_r)],
                        min(comparison_results$Pearson_r),
                        max(comparison_results$Pearson_r),
                        min(comparison_results$Pct_Spatial_Agreement),
                        max(comparison_results$Pct_Spatial_Agreement),
                        min(comparison_results$Pct_Area_Difference),
                        max(comparison_results$Pct_Area_Difference),
                        concordance_stats$total_scenarios,
                        format(concordance_stats$all_scenarios,  big.mark = ","),
                        concordance_stats$pct_all_scenarios,
                        format(concordance_stats$most_scenarios, big.mark = ","),
                        concordance_stats$pct_most_scenarios
)



writeLines(results_text, "results_text.txt")


