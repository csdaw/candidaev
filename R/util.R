val_filter <- function(data, op = c("==", "<=", ">="), pat, val) {
  # refer to operator by name
  op <- as.name(op)

  # get sum of values in column group
  col_sum <- apply(data[, c(grep(pat, colnames(data)))], 1, sum)

  # subset data based on sum of values in column group
  subset(data, sapply(col_sum, op, val))
}

val_filter2 <- function(data, logic = c("and", "or"), op = c("==", "<=", ">="),
                        pat1, val1, pat2, val2) {
  # refer to operator by name
  op <- as.name(op)

  # get sum of values in first group of columns
  sum1 <- apply(data[, c(grep(pat1, colnames(data)))], 1, sum)

  # get sum of values in second group of columns
  sum2 <- apply(data[, c(grep(pat2, colnames(data)))], 1, sum)

  # subset data based on sum of values in two column groups
  if(logic == "and") {
    subset(data, sapply(sum1, op, val1) & sapply(sum2, op, val2))
  } else if (logic == "or") {
    subset(data, sapply(sum1, op, val1) | sapply(sum2, op, val2))
  }
}

na_filter <- function(data, op = c("==", "<=", ">="), pat, val) {
  # refer to operator by name
  op <- as.name(op)

  # get number of NA in column group
  n_na <- apply(is.na(data[, c(grep(pat, colnames(data)))]), 1, sum)

  # subset data based on number of NA in column group
  subset(data, sapply(n_na, op, val))
}

na_filter2 <- function(data, logic = c("and", "or"), op = c("==", ">=", "<="),
                       pat1, val1, pat2, val2) {
  # refer to operator by name
  op <- as.name(op)

  # get number of NA in first group of columns
  n_na1 <- apply(is.na(data[, c(grep(pat1, colnames(data)))]), 1, sum)

  # get number of NA in second group of columns
  n_na2 <- apply(is.na(data[, c(grep(pat2, colnames(data)))]), 1, sum)

  # subset data based on number of NA in two column groups
  if(logic == "and") {
    subset(data, sapply(n_na1, op, val1) & sapply(n_na2, op, val2))
  } else if (logic == "or") {
    subset(data, sapply(n_na1, op, val1) | sapply(n_na2, op, val2))
  }
}

convert_lfq <- function(df, exd) {
  # define vector of labels from experimental design
  lfq_cols <- exd[["label"]]

  # show error if inputs are not correct class or structure
  assertthat::assert_that(is.data.frame(df),
                          is.data.frame(exd),
                          assert_exd(exd))
  # show error if LFQ columns in experimental design don't exactly match LFQ
  # columns in proteinGroups data frame
  assertthat::assert_that(identical(lfq_cols, colnames(select(df, one_of(lfq_cols)))))

  # select LFQ data and convert to matrix with UniProt accessions as rownames
  mat <- df %>%
    select(Majority.protein.IDs, one_of(lfq_cols)) %>%
    tibble::remove_rownames() %>%
    tibble::column_to_rownames(var = "Majority.protein.IDs") %>%
    as.matrix()

  # give columns shorter names and log2 transform LFQ intensities
  colnames(mat) <- exd[["ID"]]
  mat[mat == 0] <- NA
  mat <- log2(mat)
  return(mat)
}

# don't export this function
lfq_summary <- function(lfq_mat) {
  result <- list(Mean = mean(lfq_mat, na.rm = TRUE),
              Median = median(lfq_mat, na.rm = TRUE),
              Std.dev = sd(lfq_mat, na.rm = TRUE),
              Coeff.var = sd(lfq_mat, na.rm = TRUE)/mean(lfq_mat, na.rm = TRUE)*100,
              Min = min(lfq_mat, na.rm = TRUE),
              Max = max(lfq_mat, na.rm = TRUE),
              Range = max(lfq_mat, na.rm = TRUE) - min(lfq_mat, na.rm = TRUE),
              N.valid.val = length(na.omit(lfq_mat)),
              N.na.val = sum(is.na(lfq_mat)),
              Prcnt.na = sum(is.na(lfq_mat))/length(lfq_mat)*100
              )
}

lfq_summarise <- function(lfq_mat) {
  df <- as.data.frame(lfq_mat, stringsAsFactors = FALSE)
  result <- lapply(df, lfq_summary)
  result <- as.data.frame(do.call(rbind, result))
  return(result)
}

QRILC_impute <- function(lfq_mat) {
  imp <- imputeLCMD::impute.QRILC(lfq_mat)
  imp <- imp[[1]]
}

combine_result <- function(lfq_de, tt) {
  result <- cbind(lfq_de,
                  tt[match(rownames(lfq_de), rownames(tt)), ],
                  significant = tt[, "adj.P.Val"] < 0.01)
  return(result)
}
