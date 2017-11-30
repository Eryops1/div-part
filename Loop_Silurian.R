#clean routine
rm (list=ls())
#install.packages("vegan")
library(vegan)

####Get Data####

##local if downloaded to your computer 
#mydata <- read.csv("//INSERTPATH/Silurian-vetted.csv", row.names = NULL, sep=",")

#from web
mydata <- read.csv(url("https://raw.githubusercontent.com/fossilrich/div-part/master/Silurian-vetted.csv"), row.names = NULL, sep=";")

####Preparation####
mydata <- mydata[-nrow(mydata),] #just delete phyllum row which is not needed in this analysis

##standardize environments: Environmental setting of samples is erratic in the PBDB. Here we assign each possible category to a more robust classification scheme  
levels(mydata$collections.environment)[levels(mydata$collections.environment)%in%c("reef", "reef, buildup or bioherm","platform/shelf-margin reef", "buildup or bioherm", "perireef or subreef")] <- "reef"
levels(mydata$collections.environment)[levels(mydata$collections.environment)%in%c("deep-water indet.","deep subtidal ramp","offshore indet.","offshore shelf","deep subtidal indet.","deep subtidal shelf", "offshore", "offshore ramp")] <- "deep subtidal"
levels(mydata$collections.environment)[levels(mydata$collections.environment)%in%c("basinal (siliciclastic)","basinal (carbonate)")] <- "basin"
levels(mydata$collections.environment)[levels(mydata$collections.environment)%in%c("interdistributary bay", "estuary/bay", "delta plain", "delta front", "lagoonal/restricted shallow subtidal","paralic indet.","lagoonal", "peritidal", "prodelta","marginal marine indet.")] <- "marginal marine"
levels(mydata$collections.environment)[levels(mydata$collections.environment)%in%c("foreshore", "sand shoal", "shoreface", "transition zone/lower shoreface", "coastal indet.", "open shallow subtidal", "shallow subtidal indet.")] <- "shallow marine"
levels(mydata$collections.environment)[levels(mydata$collections.environment)%in%c("0", "","unknown", "marine indet.", "carbonate indet.")] <- "unknown"
levels(mydata$collections.environment)[levels(mydata$collections.environment)%in%"slope"] <- "slope"

##Define Objects needed for analysis
formation.name <- sort(unique(mydata$collections.formation))
datasets <- paste0("Formation", 1:length(formation.name))

Formationnames <- (formation.name)
CollpF <- c() #this will be a vector which saves the numbers of Collections per Formation
Age <- c() #Define a Vector wich collects the Age of each Formation subset
AlphaForm <- c() #assigns numbers created during the loop to a vector here called "alpha" as referring to alpha diviersities
BetaWForm <- c() # assigns numbers created during the to a vector here called "gamma" as referring to gamma diviersities
GammaForm <- c() # assigns numbers created during the to a vector here called "beta" as referring to beta diviersities
BetaSForm <- c() # a vector for additional Beta, here Sorensen or whatever
BetaJForm <- c()
RefForm <- c()  # a vector for adding the number of References per Formation
Maxage <- c()
Minage <- c()
Environments <- c()
xyz <- paste(formation.name, "environments") #Names for files


####Loop for creating submatrices for each formation#####
for(i in 1:length(formation.name)){
  temp <- mydata[mydata$collections.formation==formation.name[i],] #create a tempory matrice for each formation
  temp <- temp[,which(!apply(temp,2,FUN = function(x){all(x == 0)}))] #delete all species with zero occurences
  assign(datasets[i], temp)
  
  Age <- c(Age, mean(temp$collections.ma_mid)) #extract the mean age of each of those temps and saves them in the Vector "Age"
  Maxage <- c(Maxage, max(temp$collections.ma_mid))
  Minage <- c(Minage, min(temp$collections.ma_mid))
  RefForm <- c(RefForm, length(unique(temp$collections.reference_no)))
  Formation <- data.frame(formation.name)
  #Plate <- c(Plate, unique(temp$collections.plate)[1])
  Environments <- c(Environments, length(unique(temp$collections.environment)))

  temp <- temp[,-c(1:10)] #delete columns 1 to 10
  
  CollpF <- c(CollpF, nrow(temp))
  
  Alpha <- c() #assigns numbers created during the loop to a vector here called "alpha" as referring to alpha diviersities
  BetaW <- c() # assigns numbers created during the to a vector here called "gamma" as referring to gamma diviersities
  Gamma <- c() # assigns numbers created during the to a vector here called "beta" as referring to beta diviersities
  BetaS <- c() # assigns numbers..-
  BetaJ <- c()
  
  for(j in 1:500)
    { ###does everything inside {} n (here 500) times###
    tempt <- t(temp)  ###transpose (t,()) yields a matrix but not a dataframe 
    tempt <- as.data.frame(tempt) ###defines matrix as dataframe which is necessary to work with it###
    Subtempt <- sample(tempt, size = 20, replace = FALSE) ###samples 30 columns of the dataframe at random###
    Subtemp <- t(Subtempt) #transpose back because, well because...
    
    NewSubtemp <- Subtemp[,which(!apply(Subtemp,2,FUN = function(x){all(x == 0)}))] #removes all species(columns) with n = 0)
    species <- specnumber(NewSubtemp) #extracts species numbers of each sample of the subset, specnumber is a funciton in "Vegan"
    
    Alpha <- c(Alpha, mean(species)) #calculates the mean (=average alpha diversity of the subset) and puts it into our created vector "alpha"
    Gamma <- c(Gamma, ncol(NewSubtemp)) #calculates the overall diversity of the subset and puts it into our created vector
    BetaW <- c(BetaW, ncol(NewSubtemp)/mean((species)-1)) #calculates beta diversity (whittaker)
    
    ###make a matrix to calculate betadiversities using betadiver of "Vegan"
    Betamat <- NewSubtemp [,-1] #delete first column (samples) to make "betadiv" work on the matrix
    
    Sorensen <- betadiver(Betamat, method="sor")
    BetaS <- c(BetaS,1-mean(Sorensen))
    
    Jaccard <- betadiver(Betamat, method="j")
    BetaJ <- c(BetaJ, 1-mean(Jaccard))
  }
  
   ####now we collect all diversities from each run for further processing
  if(i==1){
    Alpha.Cam <- data.frame(Alpha)
    }else{Alpha.Cam <- cbind(Alpha.Cam, Alpha)}

  if(i==1){4
    BetaW.Cam <- data.frame(BetaW)
    }else{BetaW.Cam <- cbind(BetaW.Cam, BetaW)}
  
 if(i==1){
    BetaJ.Cam <- data.frame(BetaJ)
    }else{BetaJ.Cam <- cbind(BetaJ.Cam, BetaJ)}
  
  if(i==1){
    BetaS.Cam <- data.frame(BetaS)
    }else{BetaS.Cam <- cbind(BetaS.Cam, BetaS)}
  
 if(i==1){
    Gamma.Cam <- data.frame(Gamma)
    }else{Gamma.Cam <- cbind(Gamma.Cam, Gamma)}
  
  ####Vectors just for mean diversities per formation  
AlphaForm <- c(AlphaForm, mean(Alpha)) 
GammaForm <- c(GammaForm, mean(Gamma))
BetaWForm <- c(BetaWForm, mean(BetaW))
BetaSForm <- c(BetaSForm, mean(BetaS)) 
BetaJForm <- c(BetaJForm, mean(BetaJ))
  
  print(i) #to see the progress
  
} #End of looping

####Aggregating File####

Duration <- (Maxage - Minage)

SiluDat <- cbind(Age, AlphaForm, BetaWForm, BetaSForm, BetaJForm, GammaForm, CollpF, 
                Duration, RefForm, Formation, Environments)

write.csv(SiluDat, file = "03_SiluDat.csv")
