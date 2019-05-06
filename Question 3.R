install.packages("cluster")
install.packages("factoextra")
install.packages("Hmisc")

library("cluster")
library("factoextra")
library("Hmisc")


v <- read.csv("fullspecs.csv", sep=",")

cn <- colnames(v)

which(colnames(v))
cn[which(grepl("2019", cn, fixed=TRUE)==TRUE)]

new.rows <- rep(NA,ncol(v)-1)
new.rows <- colnames(v[2:ncol(v)])

v.T <- t(v[,2:ncol(v)])

# Set the column headings from the first column in the original table
colnames(v.T) <- v[,1] 
v.T[5,1]
vm <- as.data.frame(v.T)

cn <- cn[-1]
vm$Model <- cn

# Delete empty columns
vm.subset <- subset(vm, select = -c(108:234))
vm.subset <- subset(vm.subset, select = -c(35,46,49,59,62,65,66,69,71,73,76,101,106,107))



rownames(vm.subset) <- c()

colnames(vm.subset)
sapply(vm.subset, class)
vm.subset$MSRP <- as.numeric(vm.subset$MSRP)

names(vm.subset)[names(vm.subset) == "Passenger Capacity"] <- 'Passenger.Capacity'
names(vm.subset)[names(vm.subset) == "Passenger Doors"] <- 'Passenger.Doors'
names(vm.subset)[names(vm.subset) == "Base Curb Weight (lbs)"] <- 'Base.Curb.Weight'
names(vm.subset)[names(vm.subset) == "Front Hip Room (in)"] <- 'Front.Hip.Room'
names(vm.subset)[names(vm.subset) == "Front Leg Room (in)"] <- 'Front.Leg.Room'
names(vm.subset)[names(vm.subset) == "Second Shoulder Room (in)"] <- 'Second.Shoulder.Room'
names(vm.subset)[names(vm.subset) == "Passenger Volume (ft³)"] <- 'Passenger.Volume'
names(vm.subset)[names(vm.subset) == "Second Head Room (in)"] <- 'Second.Head.Room'
names(vm.subset)[names(vm.subset) == "Front Shoulder Room (in)"] <- 'Front.Shoulder.Room'
names(vm.subset)[names(vm.subset) == "Second Hip Room (in)"] <- 'Second.Hip.Room'
names(vm.subset)[names(vm.subset) == "Front Head Room (in)"] <- 'Front.Head.Room'
names(vm.subset)[names(vm.subset) == "Second Leg Room (in)"] <- 'Second.Leg.Room'
names(vm.subset)[names(vm.subset) == "Wheelbase (in)"] <- 'Wheelbase'
names(vm.subset)[names(vm.subset) == "Min Ground Clearance (in)"] <- 'Min.Ground.Clearance'
names(vm.subset)[names(vm.subset) == "Track Width, Front (in)"] <- 'Track.Width.Front'
names(vm.subset)[names(vm.subset) == "Width, Max w/o mirrors (in)"] <- 'Width.max.wo.mirrors'
names(vm.subset)[names(vm.subset) == "Track Width, Rear (in)"] <- 'Track.width.rear'
names(vm.subset)[names(vm.subset) == "Height, Overall (in)"] <- 'Height.overall'
names(vm.subset)[names(vm.subset) == "Cargo Volume to Seat 1 (ft³)"] <- 'Cargo.Volume.Seat1'
names(vm.subset)[names(vm.subset) == "Cargo Volume to Seat 2 (ft³)"] <- 'Cargo.Volume.Seat2'
names(vm.subset)[names(vm.subset) == "Cargo Volume to Seat 3 (ft³)"] <- 'Cargo.Volume.Seat3'
names(vm.subset)[names(vm.subset) == "Fuel Tank Capacity, Approx (gal)"] <- 'Fuel.Tank.Capacity'
names(vm.subset)[names(vm.subset) == "Fuel Economy Est-Combined (MPG)"] <- 'Fuel.Economy.combinedMPG'
names(vm.subset)[names(vm.subset) == "EPA Fuel Economy Est - City (MPG)"] <- 'EPA.Fuel.Economy.CityMPG'
names(vm.subset)[names(vm.subset) == "EPA Fuel Economy Est - Hwy (MPG)"] <- 'EPA.Fuel.Economy.HwyMPG'

subset2 <- subset ( vm.subset, select = c(Passenger.Capacity,Passenger.Doors,
                                          Base.Curb.Weight:EPA.Fuel.Economy.HwyMPG,Model
))
rownames(subset2) <- c()

subset2[,-ncol(subset2)] <- sapply(subset2[,-ncol(subset2)], as.numeric)
sapply(subset2, class)

subset2 <- subset ( vm.subset, select = c(MSRP,
                                          Passenger.Capacity,Passenger.Doors,
                                          Base.Curb.Weight:EPA.Fuel.Economy.HwyMPG,
                                          First.Gear.Ratio,
                                          Sixth.Gear.Ratio,
                                          Fourth.Gear.Ratio,
                                          Seventh.Gear.Ratio,
                                          Second.Gear.Ratio,
                                          Reverse.Ratio,
                                          Fifth.gear.Ratio,
                                          Eighth.gear.Ratio,
                                          Trans.Type,
                                          Third.Gear.Ratio,
                                          Final.Drive.Axle.Ratio,
                                          Model))
rownames(subset2) <- c()
ncol(subset2)
# ALL  MODELS: INTERIOR DIMENSIONS

subset2[,-ncol(subset2)] <- sapply(subset2[,-ncol(subset2)], as.numeric)
temp4 <- as.matrix(subset2[,-ncol(subset2)])

for ( i in 1:ncol(temp4)){
  which(is.na(temp4[,i])==TRUE)
  temp4[which(is.na(temp4[,i])==TRUE),i] <- 0
}

vc <- varclus(temp4, "spearman")
plot(vc, cex = 0.6)

km18 <- kmeans(temp4[,7:13], 3)
km$cluster
table(km$cluster)
fviz_cluster(list(data = temp4[,7:13],cluster = km18$cluster))


# GEAR ATTRIBUTES
x <- subset (subset2, select = c(First.Gear.Ratio,
Sixth.Gear.Ratio,
Fourth.Gear.Ratio,
Seventh.Gear.Ratio,
Second.Gear.Ratio,
Reverse.Ratio,
Fifth.gear.Ratio,
Eighth.gear.Ratio,
Trans.Type,
Third.Gear.Ratio,
Final.Drive.Axle.Ratio))

for ( i in 1:ncol(x)){
  which(is.na(x[,i])==TRUE)
  x[which(is.na(x[,i])==TRUE),i] <- 0
}

kmx <- kmeans(x, 3)
fviz_cluster(list(data = x,cluster = kmx$cluster))
