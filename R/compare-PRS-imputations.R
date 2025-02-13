library(missForest)
# compare imputation methods
if (FALSE){
  ind <- which(!is.na(as.matrix(D[PRS_orig_vars]))) |> sample(1000)
  D_tmp <- D[PRS_orig_vars]
  D_tmp <- as.matrix(D_tmp)
  D_tmp[ind] <- NA
  tmp <- missForest(as.matrix(D_tmp), variablewise = TRUE)
  # compute imputation with colwise mean
  imp.colwise_mean <- apply(D_tmp, 2, \(x) {x[is.na(x)] <- mean(x, na.rm=TRUE); x})
  imp.rowwise_mean <- apply(D_tmp, 1, \(x) {x[is.na(x)] <- mean(x, na.rm=TRUE); x})|>t()
  # compute RMSE
  mean((tmp$ximp[ind] - as.matrix(D[PRS_orig_vars])[ind])^2)
  mean((imp.rowwise_mean[ind] - as.matrix(D[PRS_orig_vars])[ind])^2)
  mean((imp.colwise_mean[ind] - as.matrix(D[PRS_orig_vars])[ind])^2)
  # CONCLUSION:
  # worst:  colwise mean
  # better: rowwise mean
  # best:   missForest
}
