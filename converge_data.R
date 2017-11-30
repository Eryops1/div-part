#clean routine
rm (list=ls())

#Here all csv files obtained from each loops are converged into one file wich is the basis for graphics
####Read data from each time slice####
Cam <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/01_CamDat.csv", 
                   row.names = NULL, sep=";")
Ord <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/02_OrdDat.csv", 
                row.names = NULL, sep=";")
Sil <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/03_SiluDat.csv", 
                row.names = NULL, sep=";")
Devon <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/04_DevoDat.csv", 
                 row.names = NULL, sep=";")
Carb <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/05_CarboDat.csv", 
                row.names = NULL, sep=";")
Perm1 <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/06_Perm1Dat.csv", 
                row.names = NULL, sep=";")
Perm2 <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/07_Perm2Dat.csv", 
                row.names = NULL, sep=";")
Perm3 <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/08_Perm3Dat.csv", 
                row.names = NULL, sep=";")
Trias <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/09_TriasDat.csv", 
                row.names = NULL, sep=";")
Jur1 <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/10_Jura1Dat.csv", 
                row.names = NULL, sep=";")
Jur2<- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/11_Jura2Dat.csv", 
                row.names = NULL, sep=";")
Jur3 <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/12_Jura3Dat.csv", 
                row.names = NULL, sep=";")
Cret1 <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/13_Creta1Dat.csv", 
                row.names = NULL, sep=";")
Cret2 <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/14_Creta2Dat.csv", 
                row.names = NULL, sep=";")
Paleog <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/15_PaleogDat.csv", 
                row.names = NULL, sep=";")
Neog1 <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/16_Neog1Dat.csv", 
                row.names = NULL, sep=";")
Neog2 <- read.csv("//naturkundemuseum-berlin.de/MuseumDFSRoot/Benutzer/richard.hofmann/Desktop/final/data_scripts/loop_results/17_Neog2Dat.csv", 
                row.names = NULL, sep=";")


####Lets Put the data together and save as csv####
diversities <- rbind(Cam, Ord, Sil, Devon, Carb, Perm1, Perm2, Perm3, Trias, Jur1, Jur2, Jur3, Cret1, Cret2, Paleog, Neog1, Neog2)

write.csv(diversities, file = "diversities.csv")

