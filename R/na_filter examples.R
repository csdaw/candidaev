# example for NA_filter2
df1 <- data.frame(c1 = c(NA, NA, 1), c2 = c(NA, NA, 4), c3 = c(NA, NA, 8),
                  d1 = c(NA, 1, NA), d2 = c(NA, 6, NA), d3 = c(5, 8, 9))

df2 <- na_filter2(df1, "or", "<=", pat1 = "c.", val1 = 1, pat2 = "d.", val2 = 1)


mat1 <- matrix(data = c(NA,NA,1, NA,NA,4, NA,NA,8, NA,NA,1,  NA,6,NA, 5,8,9),
              nrow = 3, ncol = 6, dimnames = list(c(1, 2, 3),
                                                  c("c1", "c2", "c3", "d1", "d2", "d3")))

mat2 <- na_filter2(mat1, "or", "<=", pat1 = "c.", val1 = 1, pat2 = "d.", val2 = 1)


# example for NA_filter
df1 <- data.frame(c1 = c(NA, NA, 1), c2 = c(NA, NA, 4), c3 = c(NA, NA, 8),
                  d1 = c(NA, 1, NA), d2 = c(NA, 6, NA), d3 = c(5, 8, 9))

df2 <- na_filter(df1, "<=", pat = "c.", val = 2)


mat1 <- matrix(data = c(NA,NA,1, NA,NA,4, NA,NA,8, NA,NA,1,  NA,6,NA, 5,8,9),
               nrow = 3, ncol = 6, dimnames = list(c(1, 2, 3),
                                                   c("c1", "c2", "c3", "d1", "d2", "d3")))

mat2 <- na_filter(mat1, "<=", pat = "c.", val = 2)


#


xxx <- testf(df1)
