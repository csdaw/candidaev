fill_blank <- function(df, blank_col, fill_col) {
  df[[blank_col]] <- factor(ifelse(df[[blank_col]] == "",
                                   df[[fill_col]],
                                   df[[blank_col]]))
  return(df)
}

val_filter <- function(df, pattern, value) {
  df[apply(df[, c(grep(pattern, colnames(df)))], 1, sum) >= value, ]
}

val_filter2 <- function(df, pattern1, value1, pattern2, value2) {
  df[apply(df[, c(grep(pattern1, colnames(df)))] > 1, 1, sum) >= value1 |
       apply(df[, c(grep(pattern2, colnames(df)))] > 1, 1, sum) >= value2, ]
}

na_filter <- function(df, pattern, value) {
  df[apply(is.na(df[, c(grep(pattern, colnames(df)))]), 1, sum) <= value, ]
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
