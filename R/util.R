fill_blank <- function(df, blank_col, fill_col) {
  df[[blank_col]] <- factor(ifelse(df[[blank_col]] == "",
                                   df[[fill_col]],
                                   df[[blank_col]]))
  return(df)
}

fill_na <- function(df, na_col, fill_col) {
  df[[na_col]][is.na(df[[na_col]])] <- ""
  df[[na_col]] <- factor(ifelse(df[[na_col]] == "",
                                df[[fill_col]],
                                df[[na_col]]))
  return(df)
}

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

add_newcol <- function(value_col, lookup_col, match_col) {
  value_col[match(match_col, lookup_col)]
}

col_clean <- function(df) {
  colnames(df) <- gsub("\\.\\.+", ".", colnames(df))
  colnames(df) <- gsub("\\.$", "", colnames(df))
  return(df)
}


convert_lfq <- function(pg, exd) {
  # define vector of labels from experimental design
  lfq_cols <- exd[["label"]]

  # show error if inputs are not correct class, structure or if number of
  # LFQ columns in pg does not match number of labels in experimental design.
  assertthat::assert_that(is.data.frame(pg),
                          is.data.frame(exd),
                          assert_exd(exd),
                          length(rownames(exd)) == length(select(pg, one_of(lfq_cols))))
  # select LFQ data and convert to matrix with UniProt accessions as rownames
  df <- pg %>%
    select(Majority.protein.IDs, one_of(lfq_cols)) %>%
    remove_rownames() %>%
    column_to_rownames(var = "Majority.protein.IDs") %>%
    as.matrix()

  # give columns shorter names and log2 transform LFQ intensities
  colnames(df) <- exd[["ID"]]
  df[df == 0] <- NA
  df <- log2(df)
  return(df)
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
