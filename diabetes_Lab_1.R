### Lab 1

data("diabetes", package = "mplot")
# help('diabetes', package='mplot')
str(diabetes)  # structure of the diabetes
pairs(diabetes)  # traditional pairs plot
boxplot(diabetes)  # always a good idea to check for gross outliers

pairsD3::shinypairs(diabetes)  # interactive pairs plot of the data set
d3heatmap::d3heatmap(cor(diabetes))
Hmisc::describe(diabetes, digits = 1)  # summary of the diabetes data

M0 = lm(y ~ 1, data = diabetes)  # Null model
M1 = lm(y ~ ., data = diabetes)  # Full model
summary(M1)
dim(diabetes)

fact_test = c(log(442), 2)

R_SQ = NULL

i = 1
for (n in fact_test) {
  ## backward step
  step.back.aic = step(M1,
                       direction = "backward",
                       trace = FALSE,
                       k = n)
  j <- summary(step.back.aic)  
  R_SQ[i] = j$r.squared
  i = i + 1
}


for (n in fact_test) {
  ## foreward step
  step.forward.aic = step(M0, scope = list(lower = M0, upper = M1),
                       direction = "forward",
                       trace = FALSE,
                       k = n)
  j <- summary(step.forward.aic)  
  R_SQ[i] = j$r.squared
  i = i + 1
}

summary(step.forward.aic)

### add var to see effect
add1(step.forward.aic, test = "F", scope = M1)

### Leave one out cross-validation
loocv = function(fit){
  n = length(fit$residuals)
  index = 1:n
  e = rep(NA, n)
  for (i in index) {
    refit = update(fit, subset = index != i)
    pred = predict(refit, data = fit$model[i,] )
    e[i] <-  model.frame(fit)[i,1] - pred
  }
  return(mean(e^2))
}

simple.model = lm(y ~ bmi + ltg + map, data = diabetes)
step.model = lm(y ~ bmi + ltg + map + tc + sex + ldl, data = diabetes)
loocv(step.model)
loocv(simple.model)
summary(step.model)$adj.r.square
summary(simple.model)$adj.r.square