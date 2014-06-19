#Multi-Class Summary Function
#Based on caret:::twoClassSummary
# From: http://moderntoolmaking.blogspot.com/2012/07/error-metrics-for-multi-class-problems.html

# RES: disable compilation for debugging
# require(compiler)
# multiClassSummary <- cmpfun(function (data, lev = NULL, model = NULL){
multiClassSummary <- function (data, lev = NULL, model = NULL){
    
  #Load Libraries
  require(Metrics)
  require(caret)
  
  #Check data
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  
  #Calculate custom one-vs-all stats for each class
  prob_stats <- lapply(levels(data[, "pred"]), function(class){
    
    #Grab one-vs-all data for the class
    pred <- ifelse(data[, "pred"] == class, 1, 0)
    obs  <- ifelse(data[,  "obs"] == class, 1, 0)
    prob <- data[,class]
    
    #Calculate one-vs-all AUC and logLoss and return
    cap_prob <- pmin(pmax(prob, .000001), .999999)
    prob_stats <- c(auc(obs, prob), logLoss(obs, cap_prob))
    names(prob_stats) <- c('ROC', 'logLoss')
    return(prob_stats) 
  })
  prob_stats <- do.call(rbind, prob_stats)
  rownames(prob_stats) <- paste('Class:', levels(data[, "pred"]))
  
  #Calculate confusion matrix-based statistics
  CM <- confusionMatrix(data[, "pred"], data[, "obs"])
  
  #Aggregate and average class-wise stats
  #Todo: add weights
  # RES: support two classes here as well
  #browser() # Debug
  if (length(levels(data[, "pred"])) == 2) {
    class_stats <- c(CM$byClass, prob_stats[1,])
  } else {
    class_stats <- cbind(CM$byClass, prob_stats)
    class_stats <- colMeans(class_stats)
  }
  
  # Aggregate overall stats
  overall_stats <- c(CM$overall)
  
  # Combine overall with class-wise stats and remove some stats we don't want 
  stats <- c(overall_stats, class_stats)
  stats <- stats[! names(stats) %in% c('AccuracyNull', 
                                       'Prevalence', 'Detection Prevalence')]
  
  # Clean names
  names(stats) <- gsub('[[:blank:]]+', '_', names(stats))

  if (length(levels(data[, "pred"]) == 2)) {
    # Change name ordering to place most useful first
    # May want to remove some of these eventually
    stats <- stats[c("ROC", "Sensitivity", "Specificity", "Accuracy", "Kappa", "logLoss",
                     "AccuracyLower", "AccuracyUpper", "AccuracyPValue", "McnemarPValue",
                     "Pos_Pred_Value", "Neg_Pred_Value", "Detection_Rate",
                     "Balanced_Accuracy")]
  }
  
  return(stats)
}
#})

# RES: compare with

# defaultSummary
# function (data, lev = NULL, model = NULL) 
# {
#   if (is.character(data$obs)) 
#     data$obs <- factor(data$obs, levels = lev)
#   postResample(data[, "pred"], data[, "obs"])
# }
# <environment: namespace:caret>
  
# twoClassSummary
# function (data, lev = NULL, model = NULL) 
# {
#   require(pROC)
#   if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
#     stop("levels of observed and predicted data do not match")
#   rocObject <- try(pROC::roc(data$obs, data[, lev[1]]), silent = TRUE)
#   rocAUC <- if (class(rocObject)[1] == "try-error") 
#     NA
#   else rocObject$auc
#   out <- c(rocAUC, sensitivity(data[, "pred"], data[, "obs"], 
#                                lev[1]), specificity(data[, "pred"], data[, "obs"], lev[2]))
#   names(out) <- c("ROC", "Sens", "Spec")
#   out
# }
# <environment: namespace:caret>
  
