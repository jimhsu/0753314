我打算寫關於這次選舉的分析。
--------------------------------------------------------------
# Insttalling Pkg
packageNames <- c("dplyr", "stringr", "data.table",
                  "ggplot2", "maptools", "knitr", 
                  "mapproj", "RColorBrewer")
install.packages(packageNames)

# Required Lib
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(maptools)
library(knitr)
library(mapproj)
library(RColorBrewer)

# Reading Dataframe
df <- fread("lab11_2018_tw_election_by_party.csv", h = T)
df <- data.frame(df)

# Reading Shapefile
tw_shp <- readShapeSpatial("gadm36_TWN_2.shp")
tw_map <- fortify(tw_shp)

# Processing Dataset
chinese_name <- c("金門縣", "連江縣", "高雄市", "新北市", "臺中市",
                  "臺南市", "臺北市", "彰化縣", "嘉義市", "嘉義縣",
                  "新竹市", "新竹縣", "花蓮縣", "基隆市", "苗栗縣",
                  "南投縣", "澎湖縣", "屏東縣", "臺東縣", "桃園市",
                  "宜蘭縣", "宜蘭縣")

mydata <- data.frame(NAME_1=tw_shp$NAME_2,
                     NAME_2=chinese_name)
mydata$id <- 0
for(i in 0:22){
  mydata$id[i] <- i
}
tw_map$id <- as.character(as.integer(tw_map$id)+1)
mydata$party <- df$party
tw.plot<-merge(tw_map,mydata,by="id",all.x=T)


# Plotting
tw_ppl_dist <- ggplot() +
  geom_polygon(data = subset(tw.plot, tw.plot$NAME_2 != "嘉義市"), 
               aes(x = long, y = lat, group = group), fill = subset(tw.plot, tw.plot$NAME_2 != "嘉義市")$party, 
               color = "black", size = 0.25) + 
  geom_polygon(data = subset(tw.plot, tw.plot$NAME_2 == "嘉義市"), 
               aes(x = long, y = lat, group = group), fill = subset(tw.plot, tw.plot$NAME_2 == "嘉義市")$party, 
               color = "black", size = 0.25) +
               coord_map()+
               labs(title="2018 Election Result of Taiwan", 
                    x ="Latitude", y = "Longitude")

tw_ppl_dist


--------------------------------------------------------------------------
# Insttalling Pkg
packageNames <- c("dplyr", "stringr", "data.table",
                  "ggplot2", "maptools", "knitr", 
                  "mapproj", "RColorBrewer")
install.packages(packageNames)

# Required Lib
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(maptools)
library(knitr)
library(mapproj)
library(RColorBrewer)
final_C<-fread("107_councilor.csv", h = T)
##---------------------------------台北----------------------------------------
tp<-final_C[1:121,]
tp_kmt<-tp[tp$推薦政黨=="中國國民黨",]
tp_dpp<-tp[tp$推薦政黨=="民主進步黨",]
##各政黨參選人數
tp_party<-table(tp$推薦政黨)
tp_party<- data.frame(tp_party)
tp_party[tp_party$Var1=="中國國民黨",]
tp_party[tp_party$Var1=="民主進步黨",]
##因為各政黨參選人數不同，要除以人數，再做比較
sum(tp_kmt$得票數)/sum(tp$得票數)/tp_party[tp_party$Var1=="中國國民黨",]$Freq
sum(tp_dpp$得票數)/sum(tp$得票數)/tp_party[tp_party$Var1=="民主進步黨",]$Freq
##---------------------------------新北----------------------------------------
ntp<-final_C[243:354,]
ntp_kmt<-ntp[ntp$推薦政黨=="中國國民黨",]
ntp_dpp<-ntp[ntp$推薦政黨=="民主進步黨",]
##各政黨參選人數
ntp_party<-table(ntp$推薦政黨)
ntp_party<- data.frame(ntp_party)
ntp_party[ntp_party$Var1=="中國國民黨",]
ntp_party[ntp_party$Var1=="民主進步黨",]
##因為各政黨參選人數不同，要除以人數，再做比較
sum(ntp_kmt$得票數)/sum(ntp$得票數)/ntp_party[ntp_party$Var1=="中國國民黨",]$Freq
sum(ntp_dpp$得票數)/sum(ntp$得票數)/ntp_party[ntp_party$Var1=="民主進步黨",]$Freq
##---------------------------------桃園----------------------------------------
ty<-final_C[581:693,]
ty_kmt<-ty[ty$推薦政黨=="中國國民黨",]
ty_dpp<-ty[ty$推薦政黨=="民主進步黨",]
##各政黨參選人數
ty_party<-table(ty$推薦政黨)
ty_party<- data.frame(ty_party)
ty_party[ty_party$Var1=="中國國民黨",]
ty_party[ty_party$Var1=="民主進步黨",]
##因為各政黨參選人數不同，要除以人數，再做比較
sum(ty_kmt$得票數)/sum(ty$得票數)/ty_party[ty_party$Var1=="中國國民黨",]$Freq
sum(ty_dpp$得票數)/sum(ty$得票數)/ty_party[ty_party$Var1=="民主進步黨",]$Freq
##---------------------------------台中----------------------------------------
tc<-final_C[355:473,]
tc_kmt<-tc[tc$推薦政黨=="中國國民黨",]
tc_dpp<-tc[tc$推薦政黨=="民主進步黨",]
##各政黨參選人數
tc_party<-table(tc$推薦政黨)
tc_party<- data.frame(tc_party)
tc_party[tc_party$Var1=="中國國民黨",]
tc_party[tc_party$Var1=="民主進步黨",]
##因為各政黨參選人數不同，要除以人數，再做比較
sum(tc_kmt$得票數)/sum(tc$得票數)/tc_party[tc_party$Var1=="中國國民黨",]$Freq
sum(tc_dpp$得票數)/sum(tc$得票數)/tc_party[tc_party$Var1=="民主進步黨",]$Freq
##---------------------------------台南----------------------------------------
tn<-final_C[474:580,]
tn_kmt<-tn[tn$推薦政黨=="中國國民黨",]
tn_dpp<-tn[tn$推薦政黨=="民主進步黨",]
##各政黨參選人數
tn_party<-table(tn$推薦政黨)
tn_party<- data.frame(tn_party)
tn_party[tn_party$Var1=="中國國民黨",]
tn_party[tn_party$Var1=="民主進步黨",]
##因為各政黨參選人數不同，要除以人數，再做比較
sum(tn_kmt$得票數)/sum(tn$得票數)/tn_party[tc_party$Var1=="中國國民黨",]$Freq
sum(tn_dpp$得票數)/sum(tn$得票數)/tn_party[tc_party$Var1=="民主進步黨",]$Freq
##---------------------------------高雄----------------------------------------
kao<-final_C[122:242,]
kao_kmt<-kao[kao$推薦政黨=="中國國民黨",]
kao_dpp<-kao[kao$推薦政黨=="民主進步黨",]
##各政黨參選人數
kao_party<-table(kao$推薦政黨)
kao_party<- data.frame(kao_party)
kao_party[kao_party$Var1=="中國國民黨",]
kao_party[kao_party$Var1=="民主進步黨",]
##因為各政黨參選人數不同，要除以人數，再做比較
sum(kao_kmt$得票數)/sum(kao$得票數)/kao_party[kao_party$Var1=="中國國民黨",]$Freq
sum(kao_dpp$得票數)/sum(kao$得票數)/kao_party[kao_party$Var1=="民主進步黨",]$Freq

-------------------------------------------------------------------------------------------
 Insttalling Pkg
packageNames <- c("dplyr", "stringr", "data.table",
                  "ggplot2", "maptools", "knitr", 
                  "mapproj", "RColorBrewer")
install.packages(packageNames)

# Required Lib
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(maptools)
library(knitr)
library(mapproj)
library(RColorBrewer)

# Reading Dataframe
df_1 <- fread("lab11_councilor_by_party.csv", h = T)
df_1 <- data.frame(df_1)

# Reading Shapefile
tw_shp <- readShapeSpatial("gadm36_TWN_2.shp")
tw_map <- fortify(tw_shp)

# Processing Dataset
chinese_name <- c("金門縣", "連江縣", "高雄市", "新北市", "臺中市",
                  "臺南市", "臺北市", "彰化縣", "嘉義市", "嘉義縣",
                  "新竹市", "新竹縣", "花蓮縣", "基隆市", "苗栗縣",
                  "南投縣", "澎湖縣", "屏東縣", "臺東縣", "桃園市",
                  "宜蘭縣", "宜蘭縣")

mydata <- data.frame(NAME_1=tw_shp$NAME_2,
                     NAME_2=chinese_name)
mydata$id <- 0
for(i in 0:22){
  mydata$id[i] <- i
}
tw_map$id <- as.character(as.integer(tw_map$id)+1)
mydata$party <- df_1$party
tw.plot<-merge(tw_map,mydata,by="id",all.x=T)


# Plotting
tw_ppl_dist <- ggplot() +
  geom_polygon(data = subset(tw.plot, tw.plot$NAME_2 != "嘉義市"), 
               aes(x = long, y = lat, group = group), fill = subset(tw.plot, tw.plot$NAME_2 != "嘉義市")$party, 
               color = "black", size = 0.25) + 
  geom_polygon(data = subset(tw.plot, tw.plot$NAME_2 == "嘉義市"), 
               aes(x = long, y = lat, group = group), fill = subset(tw.plot, tw.plot$NAME_2 == "嘉義市")$party, 
               color = "black", size = 0.25) +
  coord_map()+
  labs(title="2018 Election Result of Taiwan", 
       x ="Latitude", y = "Longitude")

tw_ppl_dist
