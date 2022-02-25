
if (!require(c("aplore3","numDeriv","stats","optextras"))) {
  install.packages(c("aplore3","numDeriv","stats","optextras"))
  library(aplore3)
  library(numDeriv)
  library(stats)
  library(optextras)
  library(dplyr)
}
  
# load the dataset & apply changes

icu_data <- as.data.frame(icu[c("gender","age","sta")])
icu_data$gender <- ifelse(icu_data$gender == "Male",1,0)
icu_data$sta <- ifelse(icu_data$sta == "Died",0,1)
head(icu_data)
nrow(icu_data)

# create the log likelihood function for logistic regression model

likefunc <- function(par){
  con <- par[1]
  beta1 <- par[2]
  beta2 <- par[3]
  
  y <- icu_data$sta
  x1 <- icu_data$age
  x2 <- icu_data$gender
  
  eq <- con + beta1*x1 + beta2*x2
  sig <- exp(eq)/(1+exp(eq))
  
  logl <- -sum(y*log(sig)+(1-y)*log(1-sig))
  logl
}


# Maximize the log-likelihood function

sol <- optimx(par = c(const  = 0,
                      beta1 = 0, 
                      beta2 = 0), 
              fn = likefunc, 
              gr=NULL,
              control = list(trace = 0,all.methods = TRUE))

# print optimization reuslts

summary(sol, order="convcode")%>% 
  select(c("const","beta1","beta2","value"))
attributes(coef(sol))$dimnames[[1]]

# Apply the glm function to get coefficients

glm_model <- glm(sta ~ age + gender, 
                 data = icu_data,
                 family = binomial(link = "logit"))


# Comparison 

glm_coef <- unname(coef(glm_model))
ll_coef <- coef(sol)

lapply(1:nrow(ll_coef), function(i){
  
  optimisation_algorithm <- attributes(ll_coef)$dimnames[[1]][i]
  
  mle_glm1 <- (ll_coef[i, "const" ] - glm_coef[1])
  mle_glm2 <- (ll_coef[i, "beta1"] - glm_coef[2])
  mle_glm3 <- (ll_coef[i, "beta2"] - glm_coef[3])
  
  mean_difference <- mean(mle_glm1, mle_glm2, mle_glm3, na.rm = TRUE)
  
  data.frame(optimisation_algorithm, mean_difference)
  
}) %>% bind_rows() %>% 
      filter(!is.na(mean_difference)) %>% 
      mutate(mean_difference = abs(mean_difference)) %>% 
      arrange(mean_difference)

