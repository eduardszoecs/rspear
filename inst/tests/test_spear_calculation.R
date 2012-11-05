context("SPEAR Calculation")

require(rspear)
data(spear_example)

sp <- spear(x = spear_example,  
      taxa = "Taxon", 
      group = c("Year", "Site"), 
      abundance = "Abundance", 
      region = "Eurasia")

test_that("Check results from example data", {
  expect_that(round(sp$spear$SPEAR, 2),
              equals(c(35.01, 63.24, 34.99, 58.64, 42.31, 19.38, 28.16, 30.65)))
})