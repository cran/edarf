## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(error = TRUE)

## ------------------------------------------------------------------------
library(edarf)

data(iris)
library(randomForest)
fit <- randomForest(Species ~ ., iris)
pd <- partial_dependence(fit, data = iris, var = "Petal.Width")
print(pd)

## ---- fig.width = 8, fig.height = 4--------------------------------------
plot_pd(pd)

## ---- fig.width = 8, fig.height = 4--------------------------------------
pd_list <- partial_dependence(fit, data = iris, c("Petal.Width", "Petal.Length"), interaction = FALSE)
plot_pd(pd_list)

## ---- fig.width = 8, fig.height = 8--------------------------------------
pd_int <- partial_dependence(fit, data = iris, c("Sepal.Length", "Sepal.Width"), interaction = TRUE)
plot_pd(pd_int, facet = "Sepal.Width")

## ---- fig.width = 8, fig.height = 5--------------------------------------
imp <- variable_importance(fit, var = colnames(iris)[-5],
  type = "aggregate", nperm = 2, data = iris)
plot_imp(imp)

## ---- fig.width = 8, fig.height = 5--------------------------------------
imp_class <- variable_importance(fit, var = colnames(iris)[-5],
  type = "local", nperm = 2, data = iris)
plot_imp(imp_class)

## ---- fig.width = 8, fig.height = 6--------------------------------------
fit <- randomForest(Fertility ~ ., data = swiss, proximity = TRUE)
imp_local <- variable_importance(fit, var = colnames(swiss)[-1], type = "local",
  data = swiss)
plot_imp(imp_local)

## ---- fig.width = 8, fig.height = 8--------------------------------------
fit <- randomForest(Species ~ ., iris, proximity = TRUE)
prox <- extract_proximity(fit)
pca <- prcomp(prox, scale = TRUE)
plot_prox(pca, color = iris$Species, color_label = "Species", size = 2)

## ---- fig.width = 8, fig.height = 5--------------------------------------
fit <- randomForest(hp ~ ., mtcars)
pred <- predict(fit, newdata = mtcars, OOB = TRUE)
plot_pred(pred, mtcars$hp,
           outlier_idx = which(abs(pred - mtcars$hp) > .5 * sd(mtcars$hp)),
           labs = row.names(mtcars))

