limma_eBayes <- function(mat, design, contrasts) {
  cm <- limma::makeContrasts(contrasts = contrasts, levels = design)
  fit <- limma::lmFit(mat, design = design)

  result <- limma::contrasts.fit(fit = fit, contrasts = cm)

  if(any(is.na(mat))) {
    for(i in contrasts) {
      covariates = strsplit(i, " - ") %>% unlist()
      single_contrast <- limma::makeContrasts(contrasts = i, levels = design[, covariates])
      single_contrast_fit <- contrasts.fit(fit[, covariates], single_contrast)
      result$coefficients[, i] <- single_contrast_fit$coefficients[, 1]
      result$stdev.unscaled[, i] <- single_contrast_fit$stdev.unscaled[, 1]
    }
  }
  result <- eBayes(result)
  result
}
