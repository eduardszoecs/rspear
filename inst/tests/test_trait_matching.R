context("Trait Matching")

require(rspear)
data(spear_example)
###traits <- 

sp <- spear(x = spear_example,  
      taxa = "Taxon", 
      group = c("Year", "Site"), 
      abundance = "Abundance", 
      region = "Eurasia", 
      traits = traits)

test_that("Check unmatched species", {
  expect_true(is.na(sp$traits[sp$traits$taxa_data == "xxxxxxxxx", 2]))
  expect_that(sp$traits[sp$traits$taxa_data == "xxxxxxxxx", "SPEAR"], 
              equals(0))
})

test_that("Check partial match", {
  expect_that(sp$traits[sp$traits$taxa_data == "Baetis rodani", "taxa_matched"],
              equals("Baetis rhodani"))
  expect_that(sp$traits[sp$traits$taxa_data == "Baetis rodani", "match_val"],
              equals(0.1))
})

test_that(" Check direct matches", {
  direct <- na.omit(sp$traits[sp$traits$match_val == -1, ])
  expect_true(all(direct$taxa_data == direct$taxa_matched))
})