data_source = data.frame(read.csv("house.dat", header=T))
Y = data_source$PRICE
X = data_source[-1]

estimate_beta = function(X, Y) {
  return(solve(t(X) %*% X) %*% t(X) %*% Y)
}

rss = function(Y, Y_estimated, variable_number, Y_estimated_full_model) {
  return(sum((Y-Y_estimated) ** 2))
}

rsquare = function(Y, Y_estimated, variable_number, Y_estimated_full_model) {
  exp_mean = mean(Y)
  total_ss = sum((Y - exp_mean) ** 2)
  return(1 - (rss(Y, Y_estimated)/(total_ss)))
}

rsquare_adjusted = function(Y, Y_estimated, variable_number, Y_estimated_full_model) {
  r_squared = rsquare(Y, Y_estimated, variable_number, Y_estimated_full_model)
  entry_number = length(Y)
  return(1-(1-r_squared)*((entry_number-13-1)/(entry_number - variable_number-1)))
}


cp = function(Y, Y_estimated, variable_number, Y_estimated_full_model) {
  rss_p = rss(Y, Y_estimated, variable_number, Y_estimated_full_model)  
  entry_number = length(Y)
  S_squared = rss(Y, Y_estimated_full_model, 14, Y_estimated_full_model)/ (entry_number-13-1)

  return(rss_p / S_squared - (entry_number-2*(variable_number+1)))
}

cp_minus_p = function(Y, Y_estimated, variable_number, Y_estimated_full_model) {
  cp_result = cp(Y, Y_estimated, variable_number, Y_estimated_full_model)
  return(abs(cp_result-(variable_number+1)))
}

estimation_Y_for_submodel = function(submodel, Y) {
  betas = estimate_beta(as.matrix(submodel), Y)
  estimates = as.vector(as.matrix(submodel) %*% betas)
  return(estimates)
}

generate_models = function(variable_number, X, Y, score_function, minimize, matrix) {
  all_combinations = combn(ncol(X), variable_number)
  best_i = NULL
  best_score = NULL
  
  Y_estimated_full_model = estimation_Y_for_submodel(cbind(Intercept=1, X), Y)
  
  for (i in 1:ncol(all_combinations)) {
    submodel = X[, all_combinations[,i]]
    submodel = cbind(Intercept=1, submodel)
   
    estimates = estimation_Y_for_submodel(submodel, Y)
    
    current_score = score_function(Y, estimates, variable_number, Y_estimated_full_model)
    if(is.null(best_score) || minimize && current_score < best_score || !minimize && current_score > best_score) {
      best_score = current_score
      best_i = i
    }
  }
  
  #add best combination of "variable_number" variables to the matrix
  i=1
  matrix[variable_number][i] = best_score
  for(col_name in colnames(X)[all_combinations[,best_i]]){
    i=i+1
    matrix[variable_number,i] = col_name
  }
  return(matrix)
}

dimensions=seq(1,13)
rss_matrix = matrix(nrow = 13, ncol = 14)
r_square_matrix = matrix(nrow = 13, ncol = 14)
r_square_adj_matrix = matrix(nrow = 13, ncol = 14)
cp_matrix = matrix(nrow = 13, ncol = 14)
 
for(nr_variables in dimensions) {
  rss_matrix = generate_models(nr_variables, X=X, Y=Y, score_function = rss, minimize=TRUE, rss_matrix)
}
print(rss_matrix)
plot(dimensions, rss_matrix[,1], type="l", main = "RSS")

for(nr_variables in dimensions) {
  r_square_matrix = generate_models(nr_variables, X=X, Y=Y, score_function = rsquare, minimize=FALSE, r_square_matrix)
}
print(r_square_matrix)
plot(dimensions, r_square_matrix[,1], type="l", main = "R square")

for(nr_variables in dimensions) {
  r_square_adj_matrix = generate_models(nr_variables, X=X, Y=Y, score_function = rsquare_adjusted, minimize=FALSE, r_square_adj_matrix)
}
print(r_square_adj_matrix)
plot(dimensions, r_square_adj_matrix[,1], type="l", main = "R square ajusted")

for(nr_variables in dimensions) {
  cp_matrix = generate_models(nr_variables, X=X, Y=Y, score_function = cp_minus_p, minimize=TRUE, cp_matrix)
}
print(cp_matrix)
plot(dimensions, cp_matrix[,1], type="l", main = "|Cp-p|")

print(rss_matrix)
print(r_square_matrix)
print(r_square_adj_matrix)
print(cp_matrix)

