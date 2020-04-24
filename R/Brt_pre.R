#' @title brt predictions
#' @export

Brt_pre = function(variabledf, opti = F,  ntree = 1000, y_varname = c("day_value", "night_value", "value_mean"),  tree.complexity = 6,bag.fraction = 0.5,shrinkage= 0.01,  training, test, grepstring = "ROAD|pop|temp|wind|Rsp|OMI|eleva|coast", ...) {
  prenres = paste(y_varname, "|", grepstring, sep = "")
  pre_mat = subset_grep(variabledf[training, ], prenres)
  x_test = variabledf[test, ]

  if (opti) {
    Xmat = subset_grep(variabledf[training, ], grepstring)
    rf3 <- gbm.step(data = pre_mat, gbm.x = names(Xmat), gbm.y = y_varname, family = "gaussian",  n.trees = ntree, tree.complexity = tree.complexity, shrinkage = shrinkage, bag.fraction = bag.fraction)
    ntree = rf3$gbm.call$best.trees
  } else {
    formu = as.formula(paste(y_varname, "~.", sep = ""))
    gbm1 = gbm(formula = formu, data = pre_mat, distribution = "gaussian", n.trees = ntree, tree.complexity= tree.complexity, shrinkage = shrinkage, bag.fraction =  bag.fraction)
    print(gbm1)
  }
  predict.gbm(gbm1, x_test, n.trees = ntree, type = "response")
}
