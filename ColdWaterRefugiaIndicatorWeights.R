##Final step: use Z scores to calculate indicator weights and composite index (each HUC12's refugia potential for the habitat group)##
setwd("E:/2023_CurrentNM_Work/R/Tables")

library(nloptr)
library(tidyverse)


##Load function for calculating composite index##
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



  
##Import z-scores from RefigiaZscores script##    
#hydrorefugia indicators#
data<-ColdWater_Z_topoveg
data<-subset(data, select =-c(1,2,3))##removes response variables

# Run optimization with clear bounds
result_topoveg <- optimize_composite_index(
  data,
  min_weight = 0.05,  # 5% minimum
  max_weight = 0.30,  # 30% maximum
  verbose = TRUE
)


export_data_topoveg =cbind(as.data.frame(data), composite_index = result_topoveg$composite_index)   
write.csv(export_data_topoveg, "coldwatertopovegcompositendex.csv", row.names=FALSE) 




##Macrorefugia indicators
data<-ColdWater_Z_clim
data<-subset(data, select =-c(1,2,3))##removes response variables

result_clim <- optimize_composite_index(
  data,
  min_weight = 0.05,  # 5% minimum
  max_weight = 0.30,  # 30% maximum
  verbose = TRUE
)

export_data_clim =cbind(as.data.frame(data), composite_index = result_clim$composite_index)   
write.csv(export_data_clim, "coldwaterclimcompositendex.csv", row.names=FALSE) 




###Take weights from results, calculate z-scores again, but with HUC12s, create composite index again, and make a table with HUC12s for mapping##

HUC_variables<-read.csv("ColdwaterJoin.csv")
names(HUC_variables)

ColdWater = subset(HUC_variables, select=-c(NAME,HUC8,FishCount, SpringDens, Pct10deg, Diff_Bio1_Curr_50_MEAN, PctChg_Bio12_MEAN, PctChg_Bio13_MEAN, PctChg_Bio14_MEAN, MeanDifETH12, MeanDifHMIH12,MeanDifSMCH12, PctFPShr, PctFPWet, mean_avwatstr, cti_mean, curve_mean,tpi_mean))

names(ColdWater)
                                              
                                             
ColdWater$Back_rcp45_rev = ColdWater$Back_rcp45_95_55_MEAN*-1 

ColdWater$Diff_bio8_rev = ColdWater$Diff_Bio8_50_cur_MEAN*-1 

ColdWater$Diff_bio9_rev = ColdWater$Diff_Bio9_50_cur_MEAN*-1 

ColdWater$Diff_bio5_rev = ColdWater$Diff_Bio5_50_curr_MEAN*-1 

ColdWater$MeanDifPNVH12_rev = ColdWater$MeanDifPNVH12*-1 

ColdWater$StrTemCha2040_rev = ColdWater$StrTemCha2040*-1 

ColdWater$hli_mean_rev = ColdWater$hli_mean*-1 

ColdWater$sbd250m_100bd_mean_rev = ColdWater$sbd250m_100bd_mean*-1 
names(ColdWater)

#Hydrorefugia indicators

ColdWater_Z_topoveg = subset(ColdWater, select=c(1,12:14,16,18,21:22,24:25,32:33))
names(ColdWater_Z_topoveg)


#compute Z scores 
columns_to_stand1 = c(2:12) ###Keeps HUC12 column in untransformed state

ColdWater_Z_topoveg [columns_to_stand1]= lapply(ColdWater_Z_topoveg[columns_to_stand1], scale) 
##For some reason I had to add an additional column to stand, which adds a climate feature##
## new code to keep unchanged HUC12 with data 

##Add Z scores x weights to calculate final composite index for hydrorefugia

coldwater_composite_index_topoveg<-ColdWater_Z_topoveg 
names(coldwater_composite_index_topoveg)
coldwater_composite_index_topoveg$topovegSumW =(coldwater_composite_index_topoveg$MeanCanHei*0.05) + (coldwater_composite_index_topoveg$PctMarWetMeaH12*0.3) +(coldwater_composite_index_topoveg$PctNatSemNatWoo12*0.05) +  
  
 (coldwater_composite_index_topoveg$PctTreCanBuf*0.05) + (coldwater_composite_index_topoveg$dem_mean*0.05497487) + (coldwater_composite_index_topoveg$aspect_sh3_mean*0.13004978) +  
  
  (coldwater_composite_index_topoveg$geomorph_sh3_std*0.16497534) + (coldwater_composite_index_topoveg$PctCarbKarstH8*0.05) + (coldwater_composite_index_topoveg$PctVolcKarstH8*0.05) + 
  
  (coldwater_composite_index_topoveg$hli_mean_rev*0.05) + (coldwater_composite_index_topoveg$sbd250m_100bd_mean_rev*0.05)

#normalize composite index to fall between 0 and 1: 
coldwater_composite_index_topoveg$topovegCompositeStd =(coldwater_composite_index_topoveg$topovegSumW-min(coldwater_composite_index_topoveg$topovegSumW))/ 
  
  (max(coldwater_composite_index_topoveg$topovegSumW)-min(coldwater_composite_index_topoveg$topovegSumW)) 

write.csv(coldwater_composite_index_topoveg,"coldwater_composite_index_topoveg20250113.csv", row.names=FALSE) 

##Macrorefugia indicators

ColdWater_Z_clim = subset(ColdWater, select=c(1,5:6,9:10,26:31))
names(ColdWater_Z_clim)

#compute Z scores 
columns_to_stand1 = c(2:11) ###Keeps HUC12 column in untransformed state

ColdWater_Z_clim [columns_to_stand1]= lapply(ColdWater_Z_clim[columns_to_stand1], scale) 

##Add Z scores x weights to calculate final composite index for macrorefugia
coldwater_composite_index_clim<-ColdWater_Z_clim 
names(coldwater_composite_index_clim)
coldwater_composite_index_clim$climSumW =(coldwater_composite_index_clim$PctChange_Bio18_MEAN*0.08824778) + (coldwater_composite_index_clim$PctChange_Bio19_MEAN*0.05) +(coldwater_composite_index_clim$MeanDifSWEH12*0.23836151) +  
  
  (coldwater_composite_index_clim$MeanJunFloPctCh2040*0.05927713) + (coldwater_composite_index_clim$Back_rcp45_rev*0.05) + (coldwater_composite_index_clim$Diff_bio8_rev*0.11194191) +  
  
  (coldwater_composite_index_clim$Diff_bio9_rev*0.05) + (coldwater_composite_index_clim$Diff_bio5_rev*0.05) + (coldwater_composite_index_clim$MeanDifPNVH12_rev*0.05) + 
  
  (coldwater_composite_index_clim$StrTemCha2040_rev*0.25217168)

#normalize composite index to fall between 0 and 1: 
coldwater_composite_index_clim$climCompositeStd =(coldwater_composite_index_clim$climSumW-min(coldwater_composite_index_clim$climSumW))/ 
  
  (max(coldwater_composite_index_clim$climSumW)-min(coldwater_composite_index_clim$climSumW)) 

write.csv(coldwater_composite_index_clim,"coldwater_composite_index_clim20250113.csv", row.names=FALSE) 


###Combine summed weighted Zs, composite indices, Z-scores for each HUC12, and then calculate mean composites from the two indices###
coldwater_CI<-full_join(coldwater_composite_index_topoveg,coldwater_composite_index_clim,"HUC12")
write.csv(coldwater_CI,"coldwater_CI_20250113.csv", row.names=FALSE) 

##Calculate percentiles for the composite scores##
coldwater_CI_Percentiles<-read.csv("coldwater_CI_20250113.csv")
names(coldwater_CI_Percentiles)
coldwater_CI_Percentiles$sum_CI<-coldwater_CI_Percentiles$topovegCompositeStd + coldwater_CI_Percentiles$climCompositeStd
coldwater_CI_Percentiles$Ave_CI<-coldwater_CI_Percentiles$sum_CI / 2

quantile(coldwater_CI_Percentiles$topovegCompositeStd,probs=seq(0,1,1/5))
quantile(coldwater_CI_Percentiles$climCompositeStd,probs=seq(0,1,1/5))
quantile(coldwater_CI_Percentiles$Ave_CI,probs=seq(0,1,1/5))

coldwater_CI_Percentiles$TopoVegTop20<-ifelse (coldwater_CI_Percentiles$topovegCompositeStd >= 0.23484425, "Y", "N") 
coldwater_CI_Percentiles$ClimTop20<-ifelse (coldwater_CI_Percentiles$climCompositeStd >= 0.7591668, "Y", "N") 
coldwater_CI_Percentiles$AveCITop20<-ifelse (coldwater_CI_Percentiles$Ave_CI >= 0.46809485, "Y", "N") 
write.csv(coldwater_CI_Percentiles,"coldwater_CI_Percentiles.csv", row.names=FALSE) 


