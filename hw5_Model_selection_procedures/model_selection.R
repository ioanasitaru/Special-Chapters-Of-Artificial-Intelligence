data_source = data.frame(read.csv("house.dat", header=T))
Y = data_source$PRICE
X = data_source[-1]
alpha = 0.05


forward = function(X, Y) {
  all_variables = seq(1,ncol(X))
  submodel_variables = c()
   
  while(TRUE) {
    available_variables = setdiff(all_variables, submodel_variables)
    best_var_index = NULL
    min_p_value = 0.05
    for( var in available_variables){
      evaluated_submodel = c(submodel_variables, var)
  
      #evaluate p-value for each model and save the model with the lowest p-value
      submodel = X[, evaluated_submodel]

      submodel = data.frame(submodel)
      fit = lm(Y ~ ., data=submodel)
      summary(fit)
      
      p_value_vector = summary(fit)$coefficients[,"Pr(>|t|)"]
      print(p_value_vector)
      p_value = tail(p_value_vector, n=1)
      
      if( p_value < alpha && p_value < min_p_value)
      {
        min_p_value = p_value
        best_var_index = var
      }
    }
    if(is.null(best_var_index)){
      cat("Selected variables: ",names(X)[submodel_variables], "\n")
      return(submodel_variables)
    }
    submodel_variables = c(submodel_variables, best_var_index)
  }
}


backward = function(X, Y) {
  all_variables = seq(1,ncol(X))
  submodel_variables = seq(1,ncol(X))
  
  while(TRUE) {
    worst_var_index = NULL
    alpha = 0.9
    
    submodel = X[, submodel_variables]
    submodel = data.frame(submodel)
    fit = lm(Y ~ ., data=submodel)
    summary(fit)
    p_value_vector = summary(fit)$coefficients[,"Pr(>|t|)"]
    
    current_max_p_value = max(p_value_vector[-1])
    print(p_value_vector)
    cat("Max:",current_max_p_value, "\n")
    
    if( current_max_p_value > alpha) {
      max_p_value = current_max_p_value
      worst_var_index = match(current_max_p_value, p_value_vector[-1])
    }
    
    if (is.null(worst_var_index)) {
      cat("Selected variables: ",names(X)[submodel_variables], "\n")
      return(submodel_variables)
    }
    cat("Removing:", names(X)[submodel_variables[worst_var_index]],"\n")
    submodel_variables = submodel_variables[-worst_var_index]
    
  }
}

stepwise = function(X, Y) {
  all_variables = seq(1,ncol(X))
  alpha_f=0.05
  submodel_variables = c()
  #forward step
  while(TRUE) {
    available_variables = setdiff(all_variables, submodel_variables)
    best_var_index = NULL
    min_p_value = 0.05
    for(var in available_variables) {
      evaluated_submodel = c(submodel_variables, var)
      
      #evaluate p-value for each model and save the model with the lowest p-value
      submodel = X[, evaluated_submodel]
      submodel = data.frame(submodel)
      fit = lm(Y ~ ., data=submodel)
      summary(fit)
      
      p_value_vector = summary(fit)$coefficients[,"Pr(>|t|)"]
      print(p_value_vector)
      p_value = tail(p_value_vector, n=1)
      
      if( p_value < alpha_f && p_value < min_p_value)
      {
        min_p_value = p_value
        best_var_index = var
      }
    }
    if(is.null(best_var_index)){
      cat("Selected variables: ",names(X)[submodel_variables], "\n")
      return(submodel_variables)
    }
    submodel_variables = c(submodel_variables, best_var_index)
    
    
    #backward call
    while(TRUE) {
      worst_var_index = NULL
      alpha_b = 0.9
      
      submodel = X[, submodel_variables]
      submodel = data.frame(submodel)
      fit = lm(Y ~ ., data=submodel)
      summary(fit)
      p_value_vector = summary(fit)$coefficients[,"Pr(>|t|)"]
      
      current_max_p_value = max(p_value_vector[-1])
      print(p_value_vector)
      cat("Max:",current_max_p_value, "\n")
      
      if( current_max_p_value > alpha_b) {
        max_p_value = current_max_p_value
        worst_var_index = match(current_max_p_value, p_value_vector[-1])
      }
      
      if (is.null(worst_var_index)) {
        cat("Selected variables: ",names(X)[submodel_variables], "\n")
        break
      }
      cat("Removing:", names(X)[submodel_variables[worst_var_index]],"\n")
      submodel_variables = submodel_variables[-worst_var_index]
      
    }
  }
}


forward_variables = forward(X, Y)
backward_variables = backward(X, Y)
stepwise_variables = stepwise(X, Y)
best_model = c(1,2,4,5,6,9,10,13)

best_model_predictions = predict(lm(Y~.,data=X[,best_model]), X[,best_model])
forward_predictions = predict(lm(Y ~., data=X[,forward_variables]), X[,forward_variables])
backward_predictions = predict(lm(Y ~., data=X[,backward_variables]), X[,backward_variables])
stepwise_predictions = predict(lm(Y ~., data=X[,stepwise_variables]), X[,stepwise_variables])


plot(targ, targ, main="Predictions", 
     xlab="y ", ylab="yhat", pch=19)
points(targ, forward_predictions, col=rgb(1,0,0,0.5), bg = rgb(1,0,0,0.5), pch=24)                 #red-forward
points(targ, backward_predictions, col=rgb(0,1,0,0.5), bg = rgb(0,1,0,0.5), pch=24)                #green-backward!
points(targ, stepwise_predictions, col=rgb(0,0,1,0.7), bg = rgb(0,0,1,0.7), pch=8)                 #blue-stepwise
points(targ, best_model_predictions, col=rgb(0.5,0.5,0.5,0.7), bg=rgb(0.5,0.5,0.5,0.7), pch=24)    #black-best
abline(0,1)

