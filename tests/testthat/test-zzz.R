test_that("Jar dst is found",{
  v6 <- .bioformats_jar_dst("6")
  expect_identical("_6.jar",substring(v6,nchar(v6) - 5,nchar(v6)))
})
