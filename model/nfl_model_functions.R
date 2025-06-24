library(gbm3)

run_gbm = function(df, response, t, i_range, s_range, n_range, b_range)
{
  if ("weight" %in% colnames(df))
  {
    df = df[, sapply(df, function(x) length(unique(x)) > 1 && !is.list(x))] %>% rename('player_weight' = 'weight')
  }
  
  df = df %>%
    mutate(across(where(is.character), as.factor))
  
  df <- df %>%
    mutate(across(everything(), ~ ifelse(is.nan(.x), NA, .x))) %>%
    rename_with(.cols = matches(" "),
                .fn = ~gsub(' ', '_', .x)) %>%
    select(all_of(names(.)[nchar(names(.)) <= 100]))
  
  df[,response] = as.numeric(as.character(df[,response]))

  tuning = rbind()
  for (i in i_range)
  {
    print(i)
    for (s in s_range)
    {
      print(s)
      for (n in n_range)
      {
        print(n)
        for (b in b_range)
        {
          print(b)

          formula = as.formula(paste(response, "~ ."))
          model = gbm(formula = formula,
                      data = df,
                      distribution = 'bernoulli',
                      n.trees = t,
                      interaction.depth = i,
                      shrinkage = s,
                      n.minobsinnode = n,
                      cv.folds = 5,
                      train.fraction = 1,
                      bag.fraction = b)
          
          best_tree = gbm.perf(model, method = "cv", plot.it = FALSE)
          
          model_best_tree =  gbm(formula = formula,
                                 data = df,
                                 distribution = 'bernoulli',
                                 n.trees = best_tree,
                                 interaction.depth = i,
                                 shrinkage = s,
                                 n.minobsinnode = n,
                                 cv.folds = 5,
                                 train.fraction = 1,
                                 bag.fraction = b)
          
          
          # 2. Get cross-validated predicted probabilities

          pred_probs <- 1 / (1 + exp(-1*(model_best_tree$cv_fitted)))
          actual <- model_best_tree$gbm_data_obj$y[1:(0.8*nrow(df))]
          
          # 3. Convert probabilities to binary predictions
          pred_class_table = data.frame()
          for (p in seq(0.05,1,0.05))
          {
            pred_classes = ifelse(pred_probs > p, 1, 0)
            precision = (sum(pred_classes == 1 & actual == 1))/(sum(pred_classes == 1))
            recall = (sum(pred_classes == 1 & actual == 1))/(sum(actual == 1))
            pred_class_table = rbind(pred_class_table, c(p, precision, recall))
          }
          colnames(pred_class_table) = c('p', 'precision', 'recall')
          
          optimal = (pred_class_table %>% filter(recall > 0.1))[which.min(abs(pred_class_table$precision - pred_class_table$recall)),]
          
          
          tuning = rbind(tuning, cbind(trees = best_tree, idepth = i, shrink = s, nmino = n, bag = b, optimal))
          }
      }
    }
  }
  return(tuning)
}

 