#' variable influence of BRT (gbm)
#' @export
Brt_imp = function(variabledf, opti = F, ntree = 1000, y_varname = c("day_value", "night_value", "value_mean"),   interaction.depth = 6,bag.fraction = 0.5,shrinkage= 0.01, training, test, grepstring, ...) {
    prenres = paste(y_varname, "|", grepstring, sep = "")
    pre_mat = subset_grep(variabledf[training, ], prenres)



    if (opti) {
        Xmat = subset_grep(variabledf[training, ], grepstring)
        rf3 <- gbm.step(data = pre_mat, gbm.x = names(Xmat), gbm.y = y_varname, family = "gaussian", n.trees=ntree, tree.complexity =  interaction.depth, shrinkage = shrinkage, bag.fraction = bag.fraction)
        ntree = rf3$gbm.call$best.trees
    } else {
        formu = as.formula(paste(y_varname, "~.", sep = ""))
        rf3 = gbm(formula = formu, data = pre_mat, distribution = "gaussian", n.trees = ntree, interaction.depth = interaction.depth, shrinkage = shrinkage, bag.fraction = bag.fraction)
    }

    m = summary(rf3, method = permutation.test.gbm, plotit = F)
    rownames(m) = m$var
    m = m %>% select(rel.inf)
}

