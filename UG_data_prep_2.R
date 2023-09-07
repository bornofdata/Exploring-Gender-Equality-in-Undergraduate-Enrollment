rm(list = ls())
library(dplyr)

###############
## Read Data ##
###############

major <- c(13, 14, 26, 27, 40, 52) # 13: Education, 14: Engineering, 26: BioSc, 27: Math, 40: PhySc, 52: Business
race.male <-  c("EFAIANM", "DVEFAIM", "EFBKAAM", "EFHISPM", "EFWHITM", "EFUNKNM", "EFNRALM")
race.female <-  c("EFAIANW", "DVEFAIW", "EFBKAAW", "EFHISPW", "EFWHITW", "EFUNKNW", "EFNRALW")
p <- length(major)
q <- length(race.male)
T <- 13

all.school.id <- list()

for (t in 1:T){
  yr <- 1994 + 2*(t-1)
  df1 <- read.csv(paste("/Users/alhajidot/Documents/BGSU/Project/UG_data/", yr, ".csv", sep=""), header=TRUE)
  all.school.id[[t]] <- unique(df1$UNITID)
}

common.school.id <- intersect(all.school.id[[1]], all.school.id[[2]])

for (t in 3:T){
  common.school.id <- intersect(common.school.id, all.school.id[[t]])
}

school.id <- unique(common.school.id)
n <- length(school.id)
Male <- array(NA, dim = c(n,p,q,T))
Female <- array(NA, dim = c(n,p,q,T))
Y <- array(NA, dim = c(n,p,q,T))

state_info <- read.csv(paste("/Users/alhajidot/Documents/BGSU/Project/hd2019.csv"), header=TRUE)
state_info <- state_info[,c(1,6)]
school.id1 <- cbind(school.id, 1)
colnames(school.id1) <- c("UNITID", "none")
state_info_common <- merge(school.id1, state_info, by.x = "UNITID", by.y = "UNITID", all.x = TRUE, all.y = FALSE)
state_info_common <- state_info_common[,-2]

save(state_info_common, file = "/Users/alhajidot/Documents/BGSU/Project/Data Extraction/data.Rdata")

for (t in 1:4){
  yr <- 1994 + 2*(t-1)
  df1 <- read.csv(paste("/Users/alhajidot/Documents/BGSU/Project/UG_data/", yr, ".csv", sep=""), header=TRUE)
  df1 <- df1[,-3]
  df2 <- df1 %>% 
    group_by(UNITID,	CIPCODE) %>% 
    dplyr::summarise(across(everything(), sum)) 
  
  for (i in 1:n){
    schl_data.m <- as.data.frame(df2[which(df2$UNITID == school.id[i]),c("UNITID", "CIPCODE",race.male)])
    schl_data.f <- as.data.frame(df2[which(df2$UNITID == school.id[i]),c("UNITID", "CIPCODE",race.female)])
    for (j in 1:dim(schl_data.m)[1]){
      if (sum(match(major,schl_data.m$CIPCODE[j], nomatch=0)) > 0){
        major.index = which(match(major,schl_data.m$CIPCODE[j]) == TRUE)
        for (k in 1:q){
          Male[i,major.index,k,t] <- schl_data.m[j,2+k]
          Female[i,major.index,k,t] <- schl_data.f[j,2+k]
        } 
      }
    }
  }
}

for (t in 5:13){
  yr <- 1994 + 2*(t-1)
  df2 <- read.csv(paste("/Users/alhajidot/Documents/BGSU/Project/UG_data/", yr, ".csv", sep=""), header=TRUE)
  
  for (i in 1:n){
    schl_data.m <- as.data.frame(df2[which(df2$UNITID == school.id[i]),c("UNITID", "CIPCODE",race.male)])
    schl_data.f <- as.data.frame(df2[which(df2$UNITID == school.id[i]),c("UNITID", "CIPCODE",race.female)])
    for (j in 1:dim(schl_data.m)[1]){
      if (sum(match(major,schl_data.m$CIPCODE[j], nomatch=0)) > 0){
        major.index = which(match(major,schl_data.m$CIPCODE[j]) == TRUE)
        for (k in 1:q){
          Male[i,major.index,k,t] <- schl_data.m[j,2+k]
          Female[i,major.index,k,t] <- schl_data.f[j,2+k]
        } 
      }
    }
  }
}

## Combine "American Indian or Alaska Native" and "Asian /Native Hawaiian or Other Pacific Islander"
## Drop Race/ethnicity unknown
## Drop Engineering because more 50% time it is missing for all racial categories for all years  
## Drop Education because many colleges didn't have it in initial years

Male.old <- Male
Female.old <- Female
p <- 4 # 26: BioSc, 27: Math, 40: PhySc, 52: Business
q <- 5 # "EFAIANM"+"DVEFAIM", "EFBKAAM", "EFHISPM", "EFWHITM", "EFNRALM"

Male <- array(NA, dim = c(n,p,q,T))
Female <- array(NA, dim = c(n,p,q,T))
Y <- array(NA, dim = c(n,2,p,q,T))

for (i in 1:n){
  for (j in 1:p){
    for (k in 1:q){
      for (t in 1:T){
        if (k == 1){
          Male[i,j,k,t] <- Male.old[i,j+2,k,t] + Male.old[i,j+2,k+1,t]
          Female[i,j,k,t] <- Female.old[i,j+2,k,t] + Female.old[i,j+2,k+1,t]
        }else if (k == 5){
          Male[i,j,k,t] <- Male.old[i,j+2,k+2,t]
          Female[i,j,k,t] <- Female.old[i,j+2,k+2,t]
        }else{
          Male[i,j,k,t] <- Male.old[i,j+2,k+1,t]
          Female[i,j,k,t] <- Female.old[i,j+2,k+1,t]
        }
      }
    }
  }
}


for (i in 1:n){
  for (j in 1:p){
    for (k in 1:q){
      for (t in 1:T){
        Y[i,1,j,k,t] <- Female[i,j,k,t]
        Y[i,2,j,k,t] <- Male[i,j,k,t]
      }
    }
  }
}

save(Y, file = "/Users/alhajidot/Documents/BGSU/Project/Data Extraction/data/Y.Rdata")

Y.complete <- complete.cases(Y)