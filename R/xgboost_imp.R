#' return variable importance xgboost
#' @export
xgboost_imp = function(variabledf, max_depth = 4, eta = 0.02, gamma=0, nthread = 2,xgb_lambda=0.001, nrounds = 300,  subsample = 0.7, y_varname = c("day_value", "night_value", "value_mean"), training, test, grepstring , ...) {
    prenres = paste(y_varname, "|", grepstring, sep = "")
    sub_mat = subset_grep(variabledf, prenres)
    sub_matx = subset_grep(variabledf, grepstring)
    pre_mat = sub_mat[training, ]
    y_train = sub_mat[training, y_varname]

    x_test = sub_mat[test, ]
    y_test = sub_mat[test, y_varname]

    dfmatrix = as.matrix(pre_mat)
    outputvec = variabledf[training, y_varname]
    bst <- xgboost(data = dfmatrix, label = outputvec, gamma= gamma, max_depth = max_depth, lambda = xgb_lambda, eta = eta,  subsample = subsample, nthread = nthread, nrounds = nrounds, verbose = 0)

   importance <- xgb.importance(feature_names = names(pre_mat), model = bst)

    V3 = data.frame(importance)
    V3 = V3[, c(1, 2)]

    # xgboost return different number of features.
    allfeature = data.frame(Feature = names(sub_matx), Gain = 0)

    m = merge(allfeature, V3, "Feature", all.x = T)
    rownames(m) = m$Feature
    m = m %>% dplyr::select(Gain.y)
    m[is.na(m)] = 0
    names(m) = c("imp_val")
    return(m)
}
