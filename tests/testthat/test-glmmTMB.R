context("glmmTMB")

library(glmmTMB)

data("iris")
iris$RR <- rep(1:15,each=10)
m <- glmmTMB(round(Sepal.Length,0) ~ Sepal.Width * Species + (1 | RR), iris,family="poisson")

mm <- add_se(m, "Species")

test_that("correct intercept values are returned",{
  expect_equal(mm$Coef,c(2.924146,4.210626,4.385867),tolerance=1e-4)
})

mm2 <- add_se(m, "Species","Sepal.Width")

test_that("correct slope values are returned",{
  expect_equal(mm2$Coef,c(1.168810,1.138780,1.143436),tolerance=1e-4)
})
