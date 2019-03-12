# 1. Import data ----

# path to folder that contains multiple .csv files
folder <- "C:/Users/pro_hector/Desktop/DEN/DENT/"

# create a list of all .csv files in folder
file_list <- list.files(path=folder, pattern="*.csv") 

# remove dates from the name of the listed files
file_list_nodate<-sub("\\_2.*", "", file_list)

# create a dataframe for every listed file using the the names without date
for (i in 1:length(file_list_nodate)){
  assign(file_list_nodate[i], read.csv(paste(folder, file_list[i], sep=""),header=TRUE,sep=";")
  )}

# 2. Data Quality 1 ----

# Apply modifications based on business requirements whilst data extractions are not modified

# remove items with same description or meaning
D_TEMA <- D_TEMA[!duplicated(D_TEMA$DESC_TEMA),]

rep_ids <- c(21,41)
D_TEMA <- subset(D_TEMA,!D_TEMA$ID_TEMA %in% rep_ids)

# remove obsolete ids
remove <- c(82,102,281,282,301,322,323,338,441,468)
D_INF_INFRACCIO <- D_INF_INFRACCIO[!D_INF_INFRACCIO$ID_GRUP_INFRACCIO %in% remove,]

# Rank groupings with the same infraction ID, then choose the propper ones (in this case rank = 1)
D_INF_INFRACCIO <- D_INF_INFRACCIO[order(D_INF_INFRACCIO$ID_INFRACCIO,D_INF_INFRACCIO$ID_GRUP_INFRACCIO),]
D_INF_INFRACCIO$rank <- sequence(rle(D_INF_INFRACCIO$ID_INFRACCIO)$lengths)
D_INF_INFRACCIO <- subset(D_INF_INFRACCIO,D_INF_INFRACCIO$rank == 1 )
D_INF_INFRACCIO <- D_INF_INFRACCIO[,1:2]

# remove duplicates
D_GRUP_INFRACCIO <- D_GRUP_INFRACCIO[!duplicated(D_GRUP_INFRACCIO$DESC_GRUP_INFRACCIO),]

# update obsolete codes
D_GRUP_INFRACCIO$ID_TEMA[D_GRUP_INFRACCIO$ID_TEMA == "121" | D_GRUP_INFRACCIO$ID_TEMA == "181"] <- 61

# filter rows per active items (vigent = 1)
D_NORM_INFRACCIO <- subset(D_NORM_INFRACCIO,D_NORM_INFRACCIO$VIGENT == 1 )
D_NORMATIVA <- subset(D_NORMATIVA,D_NORMATIVA$VIGENT == 1 )
D_AREA <- subset(D_AREA,D_AREA$VIGENT == 1 )
# remove unnecessary columns
D_NORM_INFRACCIO <- D_NORM_INFRACCIO[,1:3]
D_NORMATIVA <- D_NORMATIVA[,1:4]
D_AREA <- D_AREA[,1:2]

# 3. JOINS ----

library(dplyr)

# DATABASE TABLE: INFRACCIO TEMA
d <- left_join(D_INF_INFRACCIO, D_GRUP_INFRACCIO, by="ID_GRUP_INFRACCIO")
TBD_INFRACCIO_TEMA <- left_join(d, D_TEMA, by="ID_TEMA")

# DATABASE TABLE: DIMENSIÓ ORGANITZATIVA
a <- left_join(D_TORN, D_UNITAT, by="ID_UNITAT")
b <- left_join(a, D_DIVISIO, by="ID_DIVISIO")
TBD_ORGANITZATIVA <- left_join(b, D_COS, by="ID_COS")

# DATABASE TABLE: DIMENSIÓ ENVIAMENT
TBD_ENVIAMENT <- left_join(D_SUB_DESTINATARI, D_DESTINATARI, by="ID_DESTINATARI")

# DATABASE TABLE: DIMENSIÓ NORMATIVA
c <- left_join(D_NORM_INFRACCIO, D_NORMATIVA, by="ID_NORMATIVA")
TBD_NORMATIVA <- left_join(c, D_AREA, by="ID_AREA")

# DATABASE TABLE: F_DENUNCIES
e <- F_TRANSIT_DENUNCIES[,c(1,6)]
F_DENUNCIES <- left_join(F_DENUNCIES, e, by="ID_BUTLLETA")

# 4. Convert UTM ED50 coordinates to WGS84 ----

dataset <- F_DENUNCIES

library(rgdal)

dataset <- within(dataset, X_ORIG[is.na(X_ORIG)] <- 0)
dataset <- within(dataset, Y_ORIG[is.na(Y_ORIG)] <- 0)

xcoord <- dataset$X_ORIG
ycoord <- dataset$Y_ORIG 

UTM <- CRS("+proj=utm +zone=31+datum=WGS84")
utmcoor <- SpatialPoints(cbind(xcoord , ycoord), proj4string =UTM)
longlatcoor <- spTransform(utmcoor, CRS("+proj=longlat"))

dataset$long <- longlatcoor$xcoord
dataset$lat <- longlatcoor$ycoord

dataset <- within(dataset, long[X_ORIG == 0] <- NA)
dataset <- within(dataset, lat[Y_ORIG == 0] <- NA)
dataset <- within(dataset, X_ORIG[X_ORIG == 0] <- NA)
dataset <- within(dataset, Y_ORIG[Y_ORIG == 0] <- NA)

output <- as.data.frame(dataset, stringsAsFactors = FALSE)
F_DENUNCIES <- output


# 5. Set up tables to the database format ----

colnames(TBD_ORGANITZATIVA) <- c("Codi torn","Descripció torn","Codi unitat","Descripció unitat",
                                 "Codi divisió","Descripció divisió","Codi cos","Descripció cos")

TBD_ORGANITZATIVA_AGRUPACIO <- M_ORGANITZATIVA_AGRUPACIO
colnames(TBD_ORGANITZATIVA_AGRUPACIO) <- c("Codi agrupació","Descripció agrupació","Codi divisió"," Descripció divisió",
                                           "Codi unitat"," Descripció unitat","Àmbit")

TBD_BUTLLETI <- D_BUTLLETI
colnames(TBD_BUTLLETI) <- c(" Codi butlletí"," Descripció butlletí")

TBD_DINAMICA <- D_DINAMICA
colnames(TBD_DINAMICA) <- c("Codi dinàmica","Descripció dinàmica")

TBD_INFRACTOR_MENOR <- D_MENOR
colnames(TBD_INFRACTOR_MENOR) <- c("Codi menor","Descripció menor")

colnames(TBD_INFRACCIO_TEMA) <- c("Codi infracció","Codi grup infracció","Descripció grup infracció","Codi tema","Descripció tema")

TBD_DENUNCIES <- D_DENUNCIES
colnames(TBD_DENUNCIES) <- c("Codi butlleta","Codi origen", "Descripció origen","Codi causa estat",
                             "Descripció causa estat","Codi estat Galileo" ,"Descripció estat Galileo","Codi lliurada",
                             "Descripció lliurada","Codi IMH_MIR","Descripció IMH_MIR","Codi material intervingut","Descripció material intervingut")

colnames(TBD_NORMATIVA) <- c("Codi infraccio","Descripció infraccio","Codi normativa","Descripció normativa","Codi tipus normativa","Codi area","Descripció area")

colnames(TBD_ENVIAMENT) <- c("Codi subdestinatari","Descripció subdestinatari","Codi destinatari","Descripció destinatari")

colnames(F_DENUNCIES) <- c("ID_TEMPORAL","ID_GEO","Codi infracció", "Codi torn","Codi subdestinatari",
                           "Codi butlletí","Codi dinàmica","Codi butlleta","NUM_DENUNCIA","IMPORT",
                           "IMPORT_INMEDIAT","IMPORT_DESC","DATA_VIGENT","DATA_NO_VIGENT","Codi menor",
                           "CARRER_ORIG","NUM_ORIG","X_ORIG","Y_ORIG","CONS_BUTLLETA","Codi tipus vehicle")
TBF_DENUNCIES <- F_DENUNCIES[c(8,1,2,6,5,4,7,3,20,21,15,9,10,11,12,16,17,18,19,13,14)]

TBD_PDA_DENUNCIA <- D_PDA_DENUNCIA
colnames(TBD_PDA_DENUNCIA) <- c("Codi butlleta","Codi tipus denuncia","Descripció tipus denuncia",
                                "Codi tipus butlleta","Descripció tipus butlleta","Codi pagada","Descripció pagada","Codi notificada","Descripció notificada")

TBD_TIPUS_VEHICLE <- D_TIPUS_VEHICLE
colnames(TBD_TIPUS_VEHICLE) <- c("Codi tipus vehicle","Descripció tipus vehicle")

# 6. Generate .CSV files ready to upload to the database

dir <- "C:/Users/pro_hector/Desktop/DEN/DENF/"

write.table(TBD_ORGANITZATIVA,file = paste(dir,"TBD_ORGANITZATIVA.csv",sep= ""),row.names = FALSE,col.names = TRUE,sep=",")
write.table(TBD_ORGANITZATIVA_AGRUPACIO,file = paste(dir,"TBD_ORGANITZATIVA_AGRUPACIO.csv",sep= ""),row.names = FALSE,col.names = TRUE,sep=",")
write.table(TBD_BUTLLETI,file = paste(dir,"TBD_BUTLLETI.csv",sep= ""),row.names = FALSE,col.names = TRUE,sep=",")
write.table(TBD_DINAMICA,file = paste(dir,"TBD_DINAMICA.csv",sep= ""),row.names = FALSE,col.names = TRUE,sep=",")
write.table(TBD_INFRACTOR_MENOR,file = paste(dir,"TBD_INFRACTOR_MENOR.csv",sep= ""),row.names = FALSE,col.names = TRUE,sep=",")
write.table(TBD_NORMATIVA,file = paste(dir,"TBD_NORMATIVA.csv",sep= ""),row.names = FALSE,col.names = TRUE,sep=",")
write.table(TBD_ENVIAMENT,file = paste(dir,"TBD_ENVIAMENT.csv",sep= ""),row.names = FALSE,col.names = TRUE,sep=",")
write.table(TBD_INFRACCIO_TEMA,file = paste(dir,"TBD_INFRACCIO_TEMA.csv",sep= ""),row.names = FALSE,col.names = TRUE,sep=",")
write.table(TBD_DENUNCIES,file = paste(dir,"TBD_DENUNCIES.csv",sep= ""),row.names = FALSE,col.names = TRUE,sep=",")
write.table(TBF_DENUNCIES,file = paste(dir,"TBF_DENUNCIES.csv",sep= ""),row.names = FALSE,col.names = TRUE,sep=",")
write.table(TBD_PDA_DENUNCIA,file = paste(dir,"TBD_PDA_DENUNCIA.csv",sep= ""),row.names = FALSE,col.names = TRUE,sep=",")
