# Import data & data cleaning STEP 1 : uninterested predictors
# Fill missing value with Na

library("MASS")

rawxdata = read.csv("TRAINING.csv", sep=",", header=T, na.strings=c(""))
rawydata = read.csv("TRAINING LABEL.csv", sep=",", header=T, na.strings=c(""))
head(rawxdata)
head(rawydata)

rawdata = merge(rawydata, rawxdata, by=c('id'))
head(rawdata)

sum(is.na(rawdata)) #46094 missing values

# Data Cleaning STEP 2: drop not interested variables
# Create a new dataframe without unintersted predictors -- dataframe: techdata
# The predictors that are mentioned in article. 
# The uninterested predictors are as follows.

drop = names(rawdata) %in% c("id","wpt_name", "subvillage","lga", "ward", "scheme_name",
                             "date_recorded", "num_private", "region","recorded_by",
                             "scheme_management","payment_type","quantity_group") 
data1 = rawdata[!drop]
head(data1)
summary.data.frame(data1)

sum(is.na(data1)) #13680 missing values--origin

# Data Cleaning STEP 3: change not meaningful value to NA
# data1: all suspicious values are changed to NA, except the construction_year with value 0
# data1: 59400 obs with 28 variables

#Change all the suspicious values to NA
data1[data1=="unknown" | data1=="Unknown" | data1=="Unknown Installer" | data1=="-" | data1==""] = NA
data1[data1=="unknown"] = NA
sum(is.na(data1)) #27845 missing values (NA)
data1[data1=="Unknown"] = NA
sum(is.na(data1)) #27852 missing values (NA)
data1[data1=="Unknown Installer"] = NA
sum(is.na(data1)) #27853 missing values (NA)
data1[data1=="-"] = NA
sum(is.na(data1)) #27856 missing values (NA)

#Change all the 0s to NA, including funder, installer, longitude (construciton_year)
#construction_year is not included in this process
#construction_year is with lots of 0s.So I want to leave this in a seperate dataset.
#We can check whether these cases have great prediction power or variance that we can't ignore. 
#If you want just use the one without construction_year with 0 value. 
#You can use the final dataset directly.

data1$funder [data1$funder==0] = NA
sum(is.na(data1)) #28633 missing values (NA)
data1$installer [data1$installer==0] = NA
sum(is.na(data1)) #29410 missing values (NA)
data1$longitude [data1$longitude==0] = NA
sum(is.na(data1)) #31222 missing values (NA)

# Data Cleaning STEP 4: delete NA value in data1
# data3: all suspicious values are deleted, except the construction_year with value 0
# data3: 44105 obs with 28 variables

data3 = na.omit(data1)
sum(is.na(data3))
summary.data.frame(data3)

# Data Cleaning STEP 5: change the construction_year with value 0 to NA
# data2: all suspicious values are changed to NA, including construction_year
# data2: 59400 obs with 28 variables

data2 = data1
data2$construction_year [data1$construction_year==0] = NA

# Data Cleaning STEP 6: delete NA value in data2, operation focus on construction_year
# data4: all suspicious values are deleted, including construction_year
# data4: 30024 obs with 28 variables

data4 = na.omit(data2)
sum(is.na(data4))
summary.data.frame(data4)

# Data Cleaning STEP 7: export dataset
write.csv(data1,file='data1.csv',row.names=FALSE)
write.csv(data2,file='data2.csv',row.names=FALSE)


# Data preprocess -- combine "functional needs repair" & "nonfunctional" in data 3&4
# data5: process based on data3
# data6: process based on data4

data3$status_group2 = ifelse(data3$status_group == "functional","functional","non functional")
data3$status_group3 = ifelse(data3$status_group2 == "functional",1, 0)

data4$status_group2 = ifelse(data4$status_group == "functional","functional","non functional")
data4$status_group3 = ifelse(data4$status_group2 == "functional",1, 0)

# Export dataset
write.csv(data3,file='data3.csv',row.names=FALSE)
write.csv(data4,file='data4.csv',row.names=FALSE)




#####################################################################################
# Whether construction_year with 0 value matters
#####################################################################################



#####################################################################################
# Multiple Correspondence Analysis with 2 values in "status" (Parameter of interest)
#####################################################################################
library(FactoMineR)
library(factoextra)

#seperate num and cat variables into 2 subset:data4Num, data4Cat
head(data4)
Num = c("amount_tsh","gps_height","longitude","latitude","population")
data4Num = data4[Num]
head(data4Num)

dropNum = names(data4) %in% c("amount_tsh","gps_height","longitude","latitude","population") 
data4Cat = data4[!dropNum]
head(data4Cat)

# Plot the frequency of variables 

for (i in 1:ncol(data4Cat)) {
  plot(data4Cat[,i], main=colnames(data4Cat)[i],
       ylab = "Count", col="steelblue")
}

data4Cat = as.matrix(as.data.frame(data4Cat))
mca1 = MCA(data4Cat, graph = FALSE)


#####################################################################################
# Logistics Analysis with 2 values in "status" (Parameter of interest)
#####################################################################################



#####################################################################################
# Multiple Logistics Analysis with 2 values in "status" (Parameter of interest)
#####################################################################################

library(nnet)
data4Cat$status_group4 = relevel(data4Cat$status_group, ref = "non functional")
test1 = multinom(status_group4 ~ funder, data = data4Cat)
