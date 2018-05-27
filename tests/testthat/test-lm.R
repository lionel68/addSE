context("lm")

data("iris")
m <- lm(Sepal.Length ~ Sepal.Width * Species, iris)

mm <- add_se(m, "Species")

test_that("correct intercept values are returned",{
  expect_equal(mm$Coef,c(2.639001,3.539735,3.906836),tolerance=1e-4)
})

mm2 <- add_se(m, "Species","Sepal.Width")

test_that("correct slope values are returned",{
  expect_equal(mm2$Coef,c(0.6904897,0.8650777,0.9015345),tolerance=1e-4)
})

#factor-factor interactions
iris$RR <- factor(rep(letters[1:3],times=50))

m <- lm(Sepal.Length ~ RR * Species,iris)
