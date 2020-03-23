
library(glmnet)
set.seed(50)
data=read.csv("F:/UCD Business Analytics/2 Winter Quarter/442 Advance Stats/Homework 3/Cars_Data.csv", header=T)
y=data[,17]
x=as.matrix(data[,2:16])
n = nrow(data)

# Partition training and test data sets in 80:20 split
m = 0.8*n									
ts = sample(1:n,m)						

# Training dataset
x_train = x[ts,]							
y_train = y[ts]

# Testing dataset
x_test = x[-ts,]
y_test = y[-ts]

##################### Q1 ##########################

CV_Coef <- function(alpha,reg_name)
{
  #set.seed(100)
  model = glmnet(x_train, y_train, alpha = alpha)  
# 3-fold cross-validation
  cven=cv.glmnet(x_train, y_train, type.measure="mse", nfolds = 3, alpha = alpha,lambda.min.ratio=1e-8)      
# plot of MSE vs Lambda
  title = paste("Plot at alpha =", alpha)
  plot(cven, main = title, cex.main = 0.8)		
# best lambda --> one that minimizes mse
  lam_est = cven$lambda.min
  plot(model, xvar="lambda", label=TRUE, main=reg_name)
  abline(v=lam_est,untf=FALSE)
  coeff = coef(model, lam_est)
  return(cbind(lam_est,coeff))
}


best_alpha <- function(EN)
{
  for(i in 1:length(EN))
  {
    alpha = i/10
    model = glmnet(x_train, y_train, alpha = alpha)
    est = coef(model, s = EN[i])
    # Prediction using model at lambda min
    y_hat = predict(model, s = EN[i], newx = x_test)
    en[i,1] = sum((y_hat - y_test)^2)
    en[i,2] = alpha
    
  }

  return(en[which(en$sse == min(en$sse)),2])
}


#Lasso
Lasso <- CV_Coef(1,'Lasso')
lasso_lambda <- Lasso[1,1] #.1084, 0.437
lasso_lambda

#Ridge
Ridge<- CV_Coef(0,'Ridge')
ridge_lambda <- Ridge[1,1] #14.005,11.079
ridge_lambda

#Elastic Net
alpha_en = seq(0.1,0.9,0.1)
lambda_EN <- seq(1,9,1)
#alpha_EN <- sapply(alpha_en, CV_Coef)
for (i in 1:9){
  lambda_EN[i] <- CV_Coef(alpha_en[i],'Elastic Net')
}
en <- as.data.frame(matrix(ncol = 2, nrow = 0))
x <- c("sse", "alpha")
colnames(en) <- x
best_alpha_en <- best_alpha(lambda_EN) #0.1

EN<- CV_Coef(best_alpha_en,'EN')
En_lambda <- EN[1,1] #.18515,0.127
En_lambda

##################### Q2 ##########################
#Plotted above


############################ Q3 ######################
lambda_df <- cbind(Lasso[1,1],Ridge[1,1],EN[1,1])
colnames(lambda_df) <- c("Lasso_Coef", "Ridge_Coef","ElasticNet_Coof")

lasso_df <- as.data.frame(as.matrix(Lasso[,2]))
ridge_df <- as.data.frame(as.matrix(Ridge[,2]))
elasticNet_df <- as.data.frame(as.matrix(EN[,2]))

final_df <- cbind(lasso_df, ridge_df, elasticNet_df)
colnames(final_df) <- c("Lasso_Coef", "Ridge_Coef","ElasticNet_Coof")
final_df

############################ Q4 #####################


q4_data <- data[,-1]

ols_model <- lm(Overall.Preference ~ ., data = q4_data)
ols_estimates <- ols_model$coefficients
ols_estimates[is.na(ols_estimates)] <- 0
lasso_estimates <- as.vector(Lasso[,2])
ridge_estimates <- as.vector(Ridge[,2])
EN_estimates <- as.vector(EN[,2])

bias_perc <- function(col1, col2){
bias_percentage = rep(NA, length(col1))  
 for (i in 1:length(col1)){
   check <- col2[i]
   check2 <- col1[i]
   if (check != 0 & check2 != 0){
    bias_percentage[i] <- ((col1[i] - col2[i])/col2[i])*100
   }
   else {
    bias_percentage[i] <- 0
   }
 }
return(bias_percentage)
}


ols_lasso_biasper <- bias_perc(lasso_estimates, ols_estimates)
ols_ridge_biasper <- bias_perc(ridge_estimates, ols_estimates)
ols_en_biasper <- bias_perc(EN_estimates, ols_estimates)

lasso_df <- as.matrix(lasso_estimates)
ridge_df <- as.matrix(ridge_estimates)
elasticNet_df <- as.matrix(EN_estimates)
lasso_bias_df <- as.matrix(ols_lasso_biasper)

ridge_bias_df <- as.matrix(ols_ridge_biasper)
en_bias_df <- as.matrix(ols_en_biasper)
ols_df <- as.matrix(ols_estimates)
final_df <- as.data.frame(cbind(ols_df, lasso_df, ridge_df , elasticNet_df ,
                                lasso_bias_df, ridge_bias_df, en_bias_df))


colnames(final_df) <- c("ols_df", "lasso_df", "ridge_df" , "elasticNet_df" ,
                        "lasso_bias_df", "ridge_bias_df", "en_bias_df")
final_df

