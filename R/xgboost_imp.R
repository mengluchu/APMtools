#' return variable importance xgboost
#' @export
xgboost_imp = function(variabledf, max_depth, eta, gamma,  xgb_lambda, xgb_alpha, nrounds,  subsample, y_varname = c("day_value", "night_value", "value_mean"), training=nrow(variabledf),  grepstring , ...) {
    pre_mat =  subset_grep(variabledf, grepstring )%>%dplyr::select(-y_varname)
    x_train = pre_mat[training,]
    y_train = variabledf[training, y_varname]

    dfmatrix = as.matrix(x_train)

    bst <- xgboost(data = dfmatrix, label = y_train, gamma= gamma, max_depth = max_depth, lambda = xgb_lambda, alpha = xgb_alpha,eta = eta,  subsample = subsample, nrounds = nrounds, verbose = 0)

   importance <- xgb.importance(feature_names = names(pre_mat), model = bst)

    V3 = data.frame(importance)
    V3 = V3[, c(1, 2)]

    # xgboost return different number of features.
    allfeature = data.frame(Feature = names(pre_mat), Gain = 0)

    m = merge(allfeature, V3, "Feature", all.x = T)
    rownames(m) = m$Feature
    m = m %>% dplyr::select(Gain.y)
    m[is.na(m)] = 0
    names(m) = c("imp_val")
    return(m)
}
