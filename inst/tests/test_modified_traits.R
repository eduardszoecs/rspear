context("Modify trait values")

require(rspear)
data(spear_example)

sp <- spear(x = spear_example,  
            taxa = "Taxon", 
            group = c("Year", "Site"), 
            abundance = "Abundance", 
            region = "Eurasia")
traits_modi <- sp$traits
traits_modi[traits_modi$taxa_matched %in% "Baetis rhodani", "exposed"] <- c(1,1)
sp_modi <- spear(spear_example , 
                 taxa = names(spear_example)[1], abundance = names(spear_example)[2],
                 group = names(spear_example)[3:4], traits = traits_modi)

test_that("Check results from modified traits", {
  expect_that(round(sp_modi$spear$SPEAR, 2),
              equals(c(43.16, 63.24, 34.99, 58.64, 42.31, 19.38, 28.16, 30.65)))
})