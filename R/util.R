val_filter <- function(df, pattern, value) {
  df[apply(df[, c(grep(pattern, colnames(df)))], 1, sum) >= value, ]
}

val_filter2 <- function(df, pattern1, value1, pattern2, value2) {
  df[apply(df[, c(grep(pattern1, colnames(df)))] > 1, 1, sum) >= value1 |
       apply(df[, c(grep(pattern2, colnames(df)))] > 1, 1, sum) >= value2, ]
}

na_filter <- function(df, logic, pattern, value) {
  if(logic == "<=") {
  df[apply(is.na(df[, c(grep(pattern, colnames(df)))]), 1, sum) <= value, ]
  } else if(logic == "=") {
    df[apply(is.na(df[, c(grep(pattern, colnames(df)))]), 1, sum) == value, ]
  } else if(logic == ">=") {
    df[apply(is.na(df[, c(grep(pattern, colnames(df)))]), 1, sum) >= value, ]
  }
}

na_filter2 <- function(df, logic, pattern1, value1, pattern2, value2) {
  if(logic == "and") {
  df[apply(is.na(df[, c(grep(pattern1, colnames(df)))]), 1, sum) <= value1 &
       apply(is.na(df[, c(grep(pattern2, colnames(df)))]), 1, sum) <= value2, ]
  } else if(logic == "or") {
    df[apply(is.na(df[, c(grep(pattern1, colnames(df)))]), 1, sum) <= value1 |
         apply(is.na(df[, c(grep(pattern2, colnames(df)))]), 1, sum) <= value2, ]
  }
}

na_filter3 <- function(df, logic, pattern1, value1, pattern2, value2) {
  if(logic == "and") {
    df[apply(is.na(df[, c(grep(pattern1, colnames(df)))]), 1, sum) == value1 &
         apply(is.na(df[, c(grep(pattern2, colnames(df)))]), 1, sum) == value2, ]
  } else if(logic == "or") {
    df[apply(is.na(df[, c(grep(pattern1, colnames(df)))]), 1, sum) == value1 |
         apply(is.na(df[, c(grep(pattern2, colnames(df)))]), 1, sum) == value2, ]
  }
}

na_filter4 <- function(df, logic, pattern1, value1, pattern2, value2) {
  if(logic == "and") {
    df[apply(is.na(df[, c(grep(pattern1, colnames(df)))]), 1, sum) >= value1 &
         apply(is.na(df[, c(grep(pattern2, colnames(df)))]), 1, sum) >= value2, ]
  } else if(logic == "or") {
    df[apply(is.na(df[, c(grep(pattern1, colnames(df)))]), 1, sum) >= value1 |
         apply(is.na(df[, c(grep(pattern2, colnames(df)))]), 1, sum) >= value2, ]
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

match_uniprot <- function(up_col, reftable, fill_col, match_col) {
  stringr::str_extract_all(up_col, "[^;]+") %>%
    lapply(., function(x) match(x, reftable[[match_col]])) %>%
    Map("[", list(as.character(reftable[[fill_col]])), .) %>%
    lapply(., function(x) paste(x, collapse = ";")) %>%
    unlist()
}
