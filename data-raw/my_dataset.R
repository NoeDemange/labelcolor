## code to prepare `my_dataset` dataset goes here

my_dataset <- read.csv(file="D:/documents/stage/stage g.sapriel/Dossier_NOE/stage 2/dataset/RameauEnv_Foret2UNIMARC2.csv", header = TRUE,row.names =1)
usethis::use_data(my_dataset, overwrite = TRUE)
