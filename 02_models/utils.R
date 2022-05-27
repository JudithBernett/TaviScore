crossvalidation <- function(f, table){
  folds <- split(1:nrow(table),
                 ceiling(seq_along(1:nrow(table))/ceiling(nrow(table)/10)))
  # compute model on full dataset
  s <- summary(coxph(as.formula(f), table))
  # compute mean auc in ten-fold cv
  auc <- sapply(1:length(folds), function(i) {
    train 		<- table[unlist(folds[-i]),]
    test 		<- table[unlist(folds[i]),]
    train.fit 	<- coxph(as.formula(f), train)
    auc			<- AUC.cd(Surv(train$time,train$event), Surv(test$time,test$event),
                    predict(train.fit), predict(train.fit, newdata=test),
                    sort(unique(test$time)))
    return(auc$iauc)
  })
  mauc <- mean(auc)
  sig <- t.test(auc, mu=.5)
  print(paste('10 fold CV',mauc))
  print(sig)
}

crossvalidation2 <- function(f, table){
  tableEvent0 <- table[table$event == 0, ]
  tableEvent1 <- table[table$event == 1, ]
  
  fold0 <- split(1:nrow(tableEvent0),
                 ceiling(seq_along(1:nrow(tableEvent0))/ceiling(nrow(tableEvent0)/10)))
  fold1 <- split(1:nrow(tableEvent1),
                 ceiling(seq_along(1:nrow(tableEvent1))/ceiling(nrow(tableEvent1)/10)))
  # compute model on full dataset
  s <- summary(coxph(as.formula(f), table))
  fits <- lapply(1:length(fold0), function(i) {
    train0 		<- table[unlist(fold0[-i]),]
    test0 		<- table[unlist(fold0[i]),]
    train1  <- table[unlist(fold1[-i]), ]
    test1 <- table[unlist(fold1[i]), ]
    
    train <- rbind(train0, train1)
    test <- rbind(test0, test1)
    
    train.fit 	<- coxph(as.formula(f), train)
    auc			<- AUC.cd(Surv(train$time,train$event), Surv(test$time,test$event),
                    predict(train.fit), predict(train.fit, newdata=test),
                    sort(unique(test$time)))
    
    train.fit[["event"]] <-  train$event
    train.fit[["auc"]] <-  auc$iauc
    
    return(train.fit)
  })
  residualsTable <- lapply(fits, function(x){
    tmp <- data.table(event = x$event, residuals = x$residuals)
    return(tmp)
  })
  auc <- sapply(fits, function(x) return(x$auc))
  mauc <- mean(auc)
  sig <- t.test(auc, mu=.5)
  print(paste('10 fold CV',mauc))
  print(sig)
  return(residualsTable)
}



permutationTest <- function(formula, table, number){
  concordances <- sapply(1:number, function(i){
    permutationEvent <- sample(table$event)
    permutationTable <- data.table(event = permutationEvent, table[, -c("event")])
    coxPermut <- coxph(as.formula(formula), permutationTable)
    
    return(coxPermut$concordance[6])  
  })
  return(concordances)
}


model2table <- function(table, model){
  modeltmp <- data.table(
    event = table$event,
    linear.predictors = model$linear.predictors,
    residuals = model$residuals
  )
  for(name in names(model$coefficients)){
    modeltmp <- modeltmp[, eval(name) := table[, get(name)] * model$coefficients[name]]
  }
  return(modeltmp)
}

visualizePredictors <- function(table, model){
  # colorblind-friendly palette
  cbPalette <-
    c(
      "#999999",
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7"
    )
  
  modeltmp <- model2table(table, model)
  g <- ggplot(modeltmp,
         aes(
           x = as.factor(event),
           y = linear.predictors,
           fill =  as.factor(event)
         )) +
    geom_violin() +
    scale_fill_manual(values = cbPalette[c(1, 6)], name = "Deceased\nwithin\none year") +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    labs(x = "", y = "Centered linear predictors")
  return(g)
}
