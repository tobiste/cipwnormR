## code to prepare `DATASET` dataset goes here

env <- new.env() 
load("data-raw/Andes.RData", envir = env)
Andes <- env$Andes
usethis::use_data(Andes, overwrite = TRUE)

load("data-raw/AtomWeight.RData", envir = env)
AtomWeight <- env$AtomWeight
usethis::use_data(AtomWeight, overwrite = TRUE)

load("data-raw/MinWeight.RData", envir = env)
MinWeight <- env$MinWeight
usethis::use_data(MinWeight, overwrite = TRUE)

load("data-raw/OxiWeight.RData", envir = env)
OxiWeight <- env$OxiWeight
usethis::use_data(OxiWeight, overwrite = TRUE)

load("data-raw/TestTAS.RData", envir = env)
TestTAS <- env$TestTAS
usethis::use_data(TestTAS, overwrite = TRUE)





