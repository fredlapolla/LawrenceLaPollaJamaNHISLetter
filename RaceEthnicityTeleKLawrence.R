library(tidyverse)
nhis <- read.csv("adult20.csv")


nhis$VIRAPP12M_A <-factor(nhis$VIRAPP12M_A, labels = c("Yes","No", "Refused", "Not Ascertained", "DK"))
nhis$VIRAPPCVD_A <-factor(nhis$VIRAPPCVD_A, labels = c("Yes","No", "Refused",  "DK"))



nhis$RACEALLP_A <- factor(nhis$RACEALLP_A, labels = c("White only", "Black/AfriAm only", 
                          "Asian only", "AIAN only", "AIAN and other group", "Other single and Mult",
                          "Refused", "Not ascertained", "DK"))
nhis$HISP_A <- factor(nhis$HISP_A, labels = c("Yes", "No"))
nhis$HISPALLP_A <- factor(nhis$HISPALLP_A, labels = c("Hispanic", "NH White only", "NH Black only", "NH Asian only",
                                                      "NH AIAN only", "NH AIAN and any other", "Other single and mult"
                                                      ))
nhis$HISDETP_A <- factor(nhis$HISDETP_A, labels = c("Hispanic (Mex/MexAM)"
                                                    , "Hispanic (other)","Not Hisp", "Not Ascertained", "DK" ))
nhis$AGE65 <- factor(nhis$AGE65, labels = c("less than 65", "65 or older", "refused", "DK"))
#to remove the 85+ category and the 999s
nhis$AGE84U <- ifelse(nhis$AGEP_A < 85, nhis$AGEP_A ,NA)
# to account for the many 85+ ers
nhis$AGE85D <- ifelse(nhis$AGEP_A <85, 1, ifelse(
                     nhis$AGEP_A == 85, 2, NA)) 
nhis$AGE85D <- factor(nhis$AGE85D, labels = c("Under 85", "85 and up"))

#dropping refused and DK because too few (one each)
nhis$SEX_A <- factor(nhis$SEX_A, labels = c("Male", "Female", NA, NA))

nhis$ORIENT_A <- factor(nhis$ORIENT_A, labels = c("GayLesbian",
                                                  "straight",
                                                  "Bisexual",
                                                  "Something else",
                                                  "DK",
                                                  "Refused",
                                                  "Not Ascertained"))


nhis$MARITAL_A <- factor(nhis$MARITAL_A, labels = c("Married", "Living with partner", "Neither", "Refused", "Not Ascertained", "DK"))

nhis$NOTCOV_A <- factor(nhis$NOTCOV_A, labels = c("Not covered", "Covered", "DK"))

nhis$EMPWRKLSWK_A <- factor(nhis$EMPWRKLSWK_A, labels = c("Yes","No","DK"))

#separating out faminctc_a due to compound var
nhis$FAMINCTCU250 <- ifelse(nhis$FAMINCTC_A < 250000, nhis$FAMINCTC_A, NA)
nhis$FAMINCTD <- ifelse(nhis$FAMINCTC_A < 250000, 1, 2)
nhis$FAMINCTD <- factor(nhis$FAMINCTD, labels = c("Under 250k", "250K+"))

nhis$EDUC_A <- factor(nhis$EDUC_A, labels = c("Never", "Grade 1-11", "12Grade No Dip", "GED", "HS Grad", "Some College", "AD occupational", "AD academic", "BA/BS", "Masters", "Prof degree", "PhD/EdD", "Refused", "DK"))
nhis$URBRRL <- factor(nhis$URBRRL, labels = c("Large central Metro", "Large Fringe Metro", "Small/Med Metro", "Nonmetro"))
nhis$REGION <- factor(nhis$REGION, labels = c("Northeast", "Midwest", "South", "West"))

nhis$PHSTAT_A <- factor(nhis$PHSTAT_A, labels = c("Excellent","Very Good", "Good", "Fair", "Poor", "Refused","DK"))

nhis$MHRX_A <- factor(nhis$MHRX_A, labels = c("Yes", "No", "Refused", "Not ascertained", "DK"))
nhis$MHTHRPY_A <- factor(nhis$MHTHRPY_A, labels = c("Yes",
                                                  "No", "Refused", "Not ascertained","DK"))

nhis$CVDDIAG_A <- factor(nhis$CVDDIAG_A, labels = c("Yes", "No", "Refused", "Not Ascertained", "DK"))
nhis$CVDRSLT_A <- factor(nhis$CVDRSLT_A, labels = c("Yes",
                                                    "No", "Did not receive results", "DK"))

nhis$DLYCARE_A <- factor(nhis$DLYCARE_A, labels = c("Yes", "No", "Refused", "Not Ascertained","DK"))
nhis$DNGCARE_A <- factor(nhis$DNGCARE_A, labels = c("Yes", "No", "Refused", "Not Ascertined", "DK"))

nhis$USUALPL_A <- factor(nhis$USUALPL_A, labels = c("Yes", "there is NO place", "there is MORE than One place", "Refused","Not ascertained", "DK"))

nhis$USPLKIND_A <- factor(nhis$USPLKIND_A, labels = c("Doc office", "Urgent care pharm", "ER", "VA", "Other","No most often", "Refused","Not ascertained", "DK"))

nhis$HOSPONGT_A <- factor(nhis$HOSPONGT_A, labels = c("Yes", "No", "Refused", "Not Ascertained", "DK"))


#making subset
IntVar <- nhis[,c("VIRAPP12M_A", "VIRAPPCVD_A","RACEALLP_A","HISP_A","HISPALLP_A","HISDETP_A","AGEP_A", "AGE65", "AGE84U","AGE85D","SEX_A", "ORIENT_A", "MARITAL_A","NOTCOV_A", "EMPWRKLSWK_A", "FAMINCTC_A", "FAMINCTCU250","FAMINCTD","EDUC_A","URBRRL","REGION","PHSTAT_A","MHRX_A","MHTHRPY_A","CVDDIAG_A","CVDRSLT_A","DLYCARE_A","DNGCARE_A","USUALPL_A","USPLKIND_A","HOSPONGT_A")]

IntVar$EDUC_SIM <- recode_factor(IntVar$EDUC_A, "Never"="HS or Less",
                                 "Grade 1-11"="HS or Less", "12Grade No Dip"="HS or Less",
                                 "GED"="HS or Less", "HS Grad"="HS or Less",
                                 "Some College"="Some College or Degree","AD occupational"="Some College or Degree",
                                 "AD academic"="Some College or Degree","BA/BS"="Some College or Degree",
                                 "Masters"="Masters/Prof", "Prof degree"="Masters/Prof",
                                 "PhD/EdD" = "PhD/EdD", "Refused"= "NA", "DK"="NA")

levels(IntVar$VIRAPP12M_A)[3:5]<- NA
#to populate table of Virapp 12 vs race
table(nhis$RACEALLP_A, nhis$VIRAPP12M_A)
#to find if sign differences appear
chisq.test(nhis$RACEALLP_A,nhis$VIRAPP12M_A)

table(IntVar[,3], IntVar[,1])
#table(IntVar[,3], IntVar[,1])[1,1]+table(IntVar[,3], IntVar[,1])[1,2]

racePerc <- matrix(nrow = nrow(table(IntVar[,3], IntVar[,1])
), ncol =5)
for(i in 1:6){
  
  racePerc[i,1] <- table(IntVar[,3], IntVar[,1])[i,1]
  racePerc[i,2] <-round(100*(table(IntVar[,3], IntVar[,1])[i,1]/(table(IntVar[,3], IntVar[,1])[i,1]+table(IntVar[,3], IntVar[,1])[i,2])), digits = 2)
  racePerc[i,3] <- table(IntVar[,3], IntVar[,1])[i,2]
  racePerc[i,4] <-round(100*(table(IntVar[,3], IntVar[,1])[i,2]/(table(IntVar[,3], IntVar[,1])[i,1]+table(IntVar[,3], IntVar[,1])[i,2])), digits =2)
  racePerc[i,5] <-table(IntVar[,3], IntVar[,1])[i,1]+table(IntVar[,3], IntVar[,1])[i,2]
  }
racePerc

sum(racePerc[,1])
sum(racePerc[,3])
sum(racePerc[,5])
sum(racePerc[,1])/sum(racePerc[,5]) * 100
sum(racePerc[,3])/sum(racePerc[,5]) * 100
chisq.test(table(IntVar[,3], IntVar[,1]))

#hisp

# to make a table where column 1 is Yes group 1 by Yes VM, 2 is percentage of each group for Yes VM appt, column 3 is Group 1 by No VM, column 5 is each group percent No VM, column 5 is Total for group
hispPerc <- matrix(nrow = nrow(table(IntVar[,4], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,4], IntVar[,1]))){
  hispPerc[i,1] <- table(IntVar[,4], IntVar[,1])[i,1]
  hispPerc[i,2] <-round(100*(table(IntVar[,4], IntVar[,1])[i,1]/(table(IntVar[,4], IntVar[,1])[i,1]+table(IntVar[,4], IntVar[,1])[i,2])), digits = 2)
  hispPerc[i,3] <- table(IntVar[,4], IntVar[,1])[i,2]
  hispPerc[i,4] <-round(100*(table(IntVar[,4], IntVar[,1])[i,2]/(table(IntVar[,4], IntVar[,1])[i,1]+table(IntVar[,4], IntVar[,1])[i,2])), digits =2)
  hispPerc[i,5] <-table(IntVar[,4], IntVar[,1])[i,1]+table(IntVar[,4], IntVar[,1])[i,2]
  
  }
hispPerc
#to find total N for group
sum(hispPerc[,1])
sum(hispPerc[,3])
sum(hispPerc[,5])
sum(hispPerc[,1])/sum(hispPerc[,5]) * 100
sum(hispPerc[,3])/sum(hispPerc[,5]) * 100
chisq.test(table(IntVar[,4], IntVar[,1]))


#Hispall
hispAllPerc <- matrix(nrow = nrow(table(IntVar[,5], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,5], IntVar[,1]))){
  hispAllPerc[i,1] <- table(IntVar[,5], IntVar[,1])[i,1]
  hispAllPerc[i,2] <-round(100*(table(IntVar[,5], IntVar[,1])[i,1]/(table(IntVar[,5], IntVar[,1])[i,1]+table(IntVar[,5], IntVar[,1])[i,2])), digits = 2)
  hispAllPerc[i,3] <- table(IntVar[,5], IntVar[,1])[i,2]
  hispAllPerc[i,4] <-round(100*(table(IntVar[,5], IntVar[,1])[i,2]/(table(IntVar[,5], IntVar[,1])[i,1]+table(IntVar[,5], IntVar[,1])[i,2])), digits =2)
  hispAllPerc[i,5] <-table(IntVar[,5], IntVar[,1])[i,1]+table(IntVar[,5], IntVar[,1])[i,2]
  
}
hispAllPerc
#to find total N for group
sum(hispAllPerc[,1])
sum(hispAllPerc[,3])
sum(hispAllPerc[,5])
sum(hispAllPerc[,1])/sum(hispAllPerc[,5]) * 100
sum(hispAllPerc[,3])/sum(hispAllPerc[,5]) * 100
(table(IntVar[,5], IntVar[,1]))
chisq.test(table(IntVar[,5], IntVar[,1]))


#Hisdetp
hispdetPerc <- matrix(nrow = nrow(table(IntVar[,6], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,6], IntVar[,1]))){
  hispdetPerc[i,1] <- table(IntVar[,6], IntVar[,1])[i,1]
  hispdetPerc[i,2] <-round(100*(table(IntVar[,6], IntVar[,1])[i,1]/(table(IntVar[,6], IntVar[,1])[i,1]+table(IntVar[,6], IntVar[,1])[i,2])), digits = 2)
  hispdetPerc[i,3] <- table(IntVar[,6], IntVar[,1])[i,2]
  hispdetPerc[i,4] <-round(100*(table(IntVar[,6], IntVar[,1])[i,2]/(table(IntVar[,6], IntVar[,1])[i,1]+table(IntVar[,6], IntVar[,1])[i,2])), digits =2)
  hispdetPerc[i,5] <-table(IntVar[,6], IntVar[,1])[i,1]+table(IntVar[,6], IntVar[,1])[i,2]
  
}
hispdetPerc
#to find total N for group
sum(hispdetPerc[,1])
sum(hispdetPerc[,3])
sum(hispdetPerc[,5])
sum(hispdetPerc[,1])/sum(hispdetPerc[,5]) * 100
sum(hispdetPerc[,3])/sum(hispdetPerc[,5]) * 100
chisq.test(table(IntVar[,6], IntVar[,1]))
(table(IntVar[,6], IntVar[,1]))

#skipping age for now
#Sex
sexPerc <- matrix(nrow = nrow(table(IntVar[,11], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,11], IntVar[,1]))){
  sexPerc[i,1] <- table(IntVar[,11], IntVar[,1])[i,1]
  sexPerc[i,2] <-round(100*(table(IntVar[,11], IntVar[,1])[i,1]/(table(IntVar[,11], IntVar[,1])[i,1]+table(IntVar[,11], IntVar[,1])[i,2])), digits = 2)
  sexPerc[i,3] <- table(IntVar[,11], IntVar[,1])[i,2]
  sexPerc[i,4] <-round(100*(table(IntVar[,11], IntVar[,1])[i,2]/(table(IntVar[,11], IntVar[,1])[i,1]+table(IntVar[,11], IntVar[,1])[i,2])), digits =2)
  sexPerc[i,5] <-table(IntVar[,11], IntVar[,1])[i,1]+table(IntVar[,11], IntVar[,1])[i,2]
  
}
sexPerc
#to find total N for group
sum(sexPerc[,1])
sum(sexPerc[,3])
sum(sexPerc[,5])
sum(sexPerc[,1])/sum(sexPerc[,5]) * 100
sum(sexPerc[,3])/sum(sexPerc[,5]) * 100
chisq.test(table(IntVar[,11], IntVar[,1]))
(table(IntVar[,11], IntVar[,1]))


#Orientation
OrPerc <- matrix(nrow = nrow(table(IntVar[,12], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,12], IntVar[,1]))){
  OrPerc[i,1] <- table(IntVar[,12], IntVar[,1])[i,1]
  OrPerc[i,2] <-round(100*(table(IntVar[,12], IntVar[,1])[i,1]/(table(IntVar[,12], IntVar[,1])[i,1]+table(IntVar[,12], IntVar[,1])[i,2])), digits = 2)
  OrPerc[i,3] <- table(IntVar[,12], IntVar[,1])[i,2]
  OrPerc[i,4] <-round(100*(table(IntVar[,12], IntVar[,1])[i,2]/(table(IntVar[,12], IntVar[,1])[i,1]+table(IntVar[,12], IntVar[,1])[i,2])), digits =2)
  OrPerc[i,5] <-table(IntVar[,12], IntVar[,1])[i,1]+table(IntVar[,12], IntVar[,1])[i,2]
  
}
OrPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(OrPerc[1:4,1])
sum(OrPerc[1:4,3])
sum(OrPerc[1:4,5])
sum(OrPerc[1:4,1])/sum(OrPerc[1:4,5]) * 100
sum(OrPerc[1:4,3])/sum(OrPerc[1:4,5]) * 100
chisq.test(table(IntVar[,12], IntVar[,1]))
(table(IntVar[,12], IntVar[,1]))

#Marital
MarPerc <- matrix(nrow = nrow(table(IntVar[,13], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,13], IntVar[,1]))){
  MarPerc[i,1] <- table(IntVar[,13], IntVar[,1])[i,1]
  MarPerc[i,2] <-round(100*(table(IntVar[,13], IntVar[,1])[i,1]/(table(IntVar[,13], IntVar[,1])[i,1]+table(IntVar[,13], IntVar[,1])[i,2])), digits = 2)
  MarPerc[i,3] <- table(IntVar[,13], IntVar[,1])[i,2]
  MarPerc[i,4] <-round(100*(table(IntVar[,13], IntVar[,1])[i,2]/(table(IntVar[,13], IntVar[,1])[i,1]+table(IntVar[,13], IntVar[,1])[i,2])), digits =2)
  MarPerc[i,5] <-table(IntVar[,13], IntVar[,1])[i,1]+table(IntVar[,13], IntVar[,1])[i,2]
  
}
MarPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(MarPerc[1:3,1])
sum(MarPerc[1:3,3])
sum(MarPerc[1:3,5])
sum(MarPerc[1:3,1])/sum(MarPerc[1:3,5]) * 100
sum(MarPerc[1:3,3])/sum(MarPerc[1:3,5]) * 100
chisq.test(table(IntVar[,13], IntVar[,1]))
(table(IntVar[,13], IntVar[,1]))

#InsCoverage
CoveragePerc <- matrix(nrow = nrow(table(IntVar[,14], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,14], IntVar[,1]))){
  CoveragePerc[i,1] <- table(IntVar[,14], IntVar[,1])[i,1]
  CoveragePerc[i,2] <-round(100*(table(IntVar[,14], IntVar[,1])[i,1]/(table(IntVar[,14], IntVar[,1])[i,1]+table(IntVar[,14], IntVar[,1])[i,2])), digits = 2)
  CoveragePerc[i,3] <- table(IntVar[,14], IntVar[,1])[i,2]
  CoveragePerc[i,4] <-round(100*(table(IntVar[,14], IntVar[,1])[i,2]/(table(IntVar[,14], IntVar[,1])[i,1]+table(IntVar[,14], IntVar[,1])[i,2])), digits =2)
  CoveragePerc[i,5] <-table(IntVar[,14], IntVar[,1])[i,1]+table(IntVar[,14], IntVar[,1])[i,2]
  
}
CoveragePerc
#to find total N for group
#Excluding DK, Refused and NA
sum(CoveragePerc[1:2,1])
sum(CoveragePerc[1:2,3])
sum(CoveragePerc[1:2,5])
sum(CoveragePerc[1:2,1])/sum(CoveragePerc[1:2,5]) * 100
sum(CoveragePerc[1:2,3])/sum(CoveragePerc[1:2,5]) * 100
chisq.test(table(IntVar[,14], IntVar[,1]))
(table(IntVar[,14], IntVar[,1]))


#Employed last week
WorkPerc <- matrix(nrow = nrow(table(IntVar[,15], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,15], IntVar[,1]))){
  WorkPerc[i,1] <- table(IntVar[,15], IntVar[,1])[i,1]
  WorkPerc[i,2] <-round(100*(table(IntVar[,15], IntVar[,1])[i,1]/(table(IntVar[,15], IntVar[,1])[i,1]+table(IntVar[,15], IntVar[,1])[i,2])), digits = 2)
  WorkPerc[i,3] <- table(IntVar[,15], IntVar[,1])[i,2]
  WorkPerc[i,4] <-round(100*(table(IntVar[,15], IntVar[,1])[i,2]/(table(IntVar[,15], IntVar[,1])[i,1]+table(IntVar[,15], IntVar[,1])[i,2])), digits =2)
  WorkPerc[i,5] <-table(IntVar[,15], IntVar[,1])[i,1]+table(IntVar[,15], IntVar[,1])[i,2]
  
}
WorkPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(WorkPerc[1:2,1])
sum(WorkPerc[1:2,3])
sum(WorkPerc[1:2,5])
sum(WorkPerc[1:2,1])/sum(WorkPerc[1:2,5]) * 100
sum(WorkPerc[1:2,3])/sum(WorkPerc[1:2,5]) * 100
chisq.test(table(IntVar[,15], IntVar[,1]))
(table(IntVar[,15], IntVar[,1]))


#Education
EduPerc <- matrix(nrow = nrow(table(IntVar[,32], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,32], IntVar[,1]))){
  EduPerc[i,1] <- table(IntVar[,32], IntVar[,1])[i,1]
  EduPerc[i,2] <-round(100*(table(IntVar[,32], IntVar[,1])[i,1]/(table(IntVar[,32], IntVar[,1])[i,1]+table(IntVar[,32], IntVar[,1])[i,2])), digits = 2)
  EduPerc[i,3] <- table(IntVar[,32], IntVar[,1])[i,2]
  EduPerc[i,4] <-round(100*(table(IntVar[,32], IntVar[,1])[i,2]/(table(IntVar[,32], IntVar[,1])[i,1]+table(IntVar[,32], IntVar[,1])[i,2])), digits =2)
  EduPerc[i,5] <-table(IntVar[,32], IntVar[,1])[i,1]+table(IntVar[,32], IntVar[,1])[i,2]
  
}
EduPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(EduPerc[,1])
sum(EduPerc[,3])
sum(EduPerc[,5])
sum(EduPerc[,1])/sum(EduPerc[,5]) * 100
sum(EduPerc[,3])/sum(EduPerc[,5]) * 100
chisq.test(table(IntVar[,32], IntVar[,1]))
(table(IntVar[,32], IntVar[,1]))


#Urban
UrbanPerc <- matrix(nrow = nrow(table(IntVar[,20], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,20], IntVar[,1]))){
  UrbanPerc[i,1] <- table(IntVar[,20], IntVar[,1])[i,1]
  UrbanPerc[i,2] <-round(100*(table(IntVar[,20], IntVar[,1])[i,1]/(table(IntVar[,20], IntVar[,1])[i,1]+table(IntVar[,20], IntVar[,1])[i,2])), digits = 2)
  UrbanPerc[i,3] <- table(IntVar[,20], IntVar[,1])[i,2]
  UrbanPerc[i,4] <-round(100*(table(IntVar[,20], IntVar[,1])[i,2]/(table(IntVar[,20], IntVar[,1])[i,1]+table(IntVar[,20], IntVar[,1])[i,2])), digits =2)
  UrbanPerc[i,5] <-table(IntVar[,20], IntVar[,1])[i,1]+table(IntVar[,20], IntVar[,1])[i,2]
  
}
UrbanPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(UrbanPerc[,1])
sum(UrbanPerc[,3])
sum(UrbanPerc[,5])
sum(UrbanPerc[,1])/sum(UrbanPerc[,5]) * 100
sum(UrbanPerc[,3])/sum(UrbanPerc[,5]) * 100
chisq.test(table(IntVar[,20], IntVar[,1]))
(table(IntVar[,20], IntVar[,1]))



#Region
RegionPerc <- matrix(nrow = nrow(table(IntVar[,21], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,21], IntVar[,1]))){
  RegionPerc[i,1] <- table(IntVar[,21], IntVar[,1])[i,1]
  RegionPerc[i,2] <-round(100*(table(IntVar[,21], IntVar[,1])[i,1]/(table(IntVar[,21], IntVar[,1])[i,1]+table(IntVar[,21], IntVar[,1])[i,2])), digits = 2)
  RegionPerc[i,3] <- table(IntVar[,21], IntVar[,1])[i,2]
  RegionPerc[i,4] <-round(100*(table(IntVar[,21], IntVar[,1])[i,2]/(table(IntVar[,21], IntVar[,1])[i,1]+table(IntVar[,21], IntVar[,1])[i,2])), digits =2)
  RegionPerc[i,5] <-table(IntVar[,21], IntVar[,1])[i,1]+table(IntVar[,21], IntVar[,1])[i,2]
  
}
RegionPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(RegionPerc[,1])
sum(RegionPerc[,3])
sum(RegionPerc[,5])
sum(RegionPerc[,1])/sum(RegionPerc[,5]) * 100
sum(RegionPerc[,3])/sum(RegionPerc[,5]) * 100
chisq.test(table(IntVar[,21], IntVar[,1]))
(table(IntVar[,21], IntVar[,1]))

#health status

#to drop IDK and refused
# levels(IntVar$PHSTAT_A)[6:7] <- NA
statPerc <- matrix(nrow = nrow(table(IntVar[,22], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,22], IntVar[,1]))){
  statPerc[i,1] <- table(IntVar[,22], IntVar[,1])[i,1]
  statPerc[i,2] <-round(100*(table(IntVar[,22], IntVar[,1])[i,1]/(table(IntVar[,22], IntVar[,1])[i,1]+table(IntVar[,22], IntVar[,1])[i,2])), digits = 2)
  statPerc[i,3] <- table(IntVar[,22], IntVar[,1])[i,2]
  statPerc[i,4] <-round(100*(table(IntVar[,22], IntVar[,1])[i,2]/(table(IntVar[,22], IntVar[,1])[i,1]+table(IntVar[,22], IntVar[,1])[i,2])), digits =2)
  statPerc[i,5] <-table(IntVar[,22], IntVar[,1])[i,1]+table(IntVar[,22], IntVar[,1])[i,2]
  
}
statPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(statPerc[,1])
sum(statPerc[,3])
sum(statPerc[,5])
sum(statPerc[,1])/sum(statPerc[,5]) * 100
sum(statPerc[,3])/sum(statPerc[,5]) * 100
chisq.test(table(IntVar[,22], IntVar[,1]))
(table(IntVar[,22], IntVar[,1]))

#Mental Health RX
#levels(IntVar$MHRX_A)[3:5]<- NA

MHPerc <- matrix(nrow = nrow(table(IntVar[,23], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,23], IntVar[,1]))){
  MHPerc[i,1] <- table(IntVar[,23], IntVar[,1])[i,1]
  MHPerc[i,2] <-round(100*(table(IntVar[,23], IntVar[,1])[i,1]/(table(IntVar[,23], IntVar[,1])[i,1]+table(IntVar[,23], IntVar[,1])[i,2])), digits = 2)
  MHPerc[i,3] <- table(IntVar[,23], IntVar[,1])[i,2]
  MHPerc[i,4] <-round(100*(table(IntVar[,23], IntVar[,1])[i,2]/(table(IntVar[,23], IntVar[,1])[i,1]+table(IntVar[,23], IntVar[,1])[i,2])), digits =2)
  MHPerc[i,5] <-table(IntVar[,23], IntVar[,1])[i,1]+table(IntVar[,23], IntVar[,1])[i,2]
  
}
MHPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(MHPerc[,1])
sum(MHPerc[,3])
sum(MHPerc[,5])
sum(MHPerc[,1])/sum(MHPerc[,5]) * 100
sum(MHPerc[,3])/sum(MHPerc[,5]) * 100
chisq.test(table(IntVar[,23], IntVar[,1]))
(table(IntVar[,23], IntVar[,1]))


#Mental Health Therapy
levels(IntVar$MHTHRPY_A)[3:5]<- NA

ther <- matrix(nrow = nrow(table(IntVar[,24], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,24], IntVar[,1]))){
  ther[i,1] <- table(IntVar[,24], IntVar[,1])[i,1]
  ther[i,2] <-round(100*(table(IntVar[,24], IntVar[,1])[i,1]/(table(IntVar[,24], IntVar[,1])[i,1]+table(IntVar[,24], IntVar[,1])[i,2])), digits = 2)
  ther[i,3] <- table(IntVar[,24], IntVar[,1])[i,2]
  ther[i,4] <-round(100*(table(IntVar[,24], IntVar[,1])[i,2]/(table(IntVar[,24], IntVar[,1])[i,1]+table(IntVar[,24], IntVar[,1])[i,2])), digits =2)
  ther[i,5] <-table(IntVar[,24], IntVar[,1])[i,1]+table(IntVar[,24], IntVar[,1])[i,2]
  
}
ther
#to find total N for group
#Excluding DK, Refused and NA
sum(ther[,1])
sum(ther[,3])
sum(ther[,5])
sum(ther[,1])/sum(ther[,5]) * 100
sum(ther[,3])/sum(ther[,5]) * 100
chisq.test(table(IntVar[,24], IntVar[,1]))
(table(IntVar[,24], IntVar[,1]))

#Covid Dx test
levels(IntVar$CVDDIAG_A)[3:5]<- NA

CVDdxPerc <- matrix(nrow = nrow(table(IntVar[,25], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,25], IntVar[,1]))){
  CVDdxPerc[i,1] <- table(IntVar[,25], IntVar[,1])[i,1]
  CVDdxPerc[i,2] <-round(100*(table(IntVar[,25], IntVar[,1])[i,1]/(table(IntVar[,25], IntVar[,1])[i,1]+table(IntVar[,25], IntVar[,1])[i,2])), digits = 2)
  CVDdxPerc[i,3] <- table(IntVar[,25], IntVar[,1])[i,2]
  CVDdxPerc[i,4] <-round(100*(table(IntVar[,25], IntVar[,1])[i,2]/(table(IntVar[,25], IntVar[,1])[i,1]+table(IntVar[,25], IntVar[,1])[i,2])), digits =2)
  CVDdxPerc[i,5] <-table(IntVar[,25], IntVar[,1])[i,1]+table(IntVar[,25], IntVar[,1])[i,2]
  
}
CVDdxPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(CVDdxPerc[,1])
sum(CVDdxPerc[,3])
sum(CVDdxPerc[,5])
sum(CVDdxPerc[,1])/sum(CVDdxPerc[,5]) * 100
sum(CVDdxPerc[,3])/sum(CVDdxPerc[,5]) * 100
chisq.test(IntVar[,25], IntVar[,1])
(table(IntVar[,25], IntVar[,1]))

#Covid Res
levels(IntVar$CVDRSLT_A)[3:4]<- NA

CVDResPerc <- matrix(nrow = nrow(table(IntVar[,26], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,26], IntVar[,1]))){
  CVDResPerc[i,1] <- table(IntVar[,26], IntVar[,1])[i,1]
  CVDResPerc[i,2] <-round(100*(table(IntVar[,26], IntVar[,1])[i,1]/(table(IntVar[,26], IntVar[,1])[i,1]+table(IntVar[,26], IntVar[,1])[i,2])), digits = 2)
  CVDResPerc[i,3] <- table(IntVar[,26], IntVar[,1])[i,2]
  CVDResPerc[i,4] <-round(100*(table(IntVar[,26], IntVar[,1])[i,2]/(table(IntVar[,26], IntVar[,1])[i,1]+table(IntVar[,26], IntVar[,1])[i,2])), digits =2)
  CVDResPerc[i,5] <-table(IntVar[,26], IntVar[,1])[i,1]+table(IntVar[,26], IntVar[,1])[i,2]
  
}
CVDResPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(CVDResPerc[,1])
sum(CVDResPerc[,3])
sum(CVDResPerc[,5])
sum(CVDResPerc[,1])/sum(CVDResPerc[,5]) * 100
sum(CVDResPerc[,3])/sum(CVDResPerc[,5]) * 100
chisq.test(IntVar[,26], IntVar[,1])
(table(IntVar[,26], IntVar[,1]))

#Delay Care due to Covid 
levels(IntVar$DLYCARE_A)[3:5]<- NA

CVDDelPerc <- matrix(nrow = nrow(table(IntVar[,27], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,27], IntVar[,1]))){
  CVDDelPerc[i,1] <- table(IntVar[,27], IntVar[,1])[i,1]
  CVDDelPerc[i,2] <-round(100*(table(IntVar[,27], IntVar[,1])[i,1]/(table(IntVar[,27], IntVar[,1])[i,1]+table(IntVar[,27], IntVar[,1])[i,2])), digits = 2)
  CVDDelPerc[i,3] <- table(IntVar[,27], IntVar[,1])[i,2]
  CVDDelPerc[i,4] <-round(100*(table(IntVar[,27], IntVar[,1])[i,2]/(table(IntVar[,27], IntVar[,1])[i,1]+table(IntVar[,27], IntVar[,1])[i,2])), digits =2)
  CVDDelPerc[i,5] <-table(IntVar[,27], IntVar[,1])[i,1]+table(IntVar[,27], IntVar[,1])[i,2]
  
}
CVDDelPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(CVDDelPerc[,1])
sum(CVDDelPerc[,3])
sum(CVDDelPerc[,5])
sum(CVDDelPerc[,1])/sum(CVDDelPerc[,5]) * 100
sum(CVDDelPerc[,3])/sum(CVDDelPerc[,5]) * 100
chisq.test(IntVar[,27], IntVar[,1])
(table(IntVar[,27], IntVar[,1]))

#Did not get Care due to Covid 
levels(IntVar$DNGCARE_A)[3:5]<- NA

CVDDNGPerc <- matrix(nrow = nrow(table(IntVar[,28], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,28], IntVar[,1]))){
  CVDDNGPerc[i,1] <- table(IntVar[,28], IntVar[,1])[i,1]
  CVDDNGPerc[i,2] <-round(100*(table(IntVar[,28], IntVar[,1])[i,1]/(table(IntVar[,28], IntVar[,1])[i,1]+table(IntVar[,28], IntVar[,1])[i,2])), digits = 2)
  CVDDNGPerc[i,3] <- table(IntVar[,28], IntVar[,1])[i,2]
  CVDDNGPerc[i,4] <-round(100*(table(IntVar[,28], IntVar[,1])[i,2]/(table(IntVar[,28], IntVar[,1])[i,1]+table(IntVar[,28], IntVar[,1])[i,2])), digits =2)
  CVDDNGPerc[i,5] <-table(IntVar[,28], IntVar[,1])[i,1]+table(IntVar[,28], IntVar[,1])[i,2]
  
}
CVDDNGPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(CVDDNGPerc[,1])
sum(CVDDNGPerc[,3])
sum(CVDDNGPerc[,5])
sum(CVDDNGPerc[,1])/sum(CVDDNGPerc[,5]) * 100
sum(CVDDNGPerc[,3])/sum(CVDDNGPerc[,5]) * 100
chisq.test(IntVar[,28], IntVar[,1])
(table(IntVar[,28], IntVar[,1]))

#usual place of care
levels(IntVar$USUALPL_A)[4:6]<- NA

UsualPlacePerc <- matrix(nrow = nrow(table(IntVar[,29], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,29], IntVar[,1]))){
  UsualPlacePerc[i,1] <- table(IntVar[,29], IntVar[,1])[i,1]
  UsualPlacePerc[i,2] <-round(100*(table(IntVar[,29], IntVar[,1])[i,1]/(table(IntVar[,29], IntVar[,1])[i,1]+table(IntVar[,29], IntVar[,1])[i,2])), digits = 2)
  UsualPlacePerc[i,3] <- table(IntVar[,29], IntVar[,1])[i,2]
  UsualPlacePerc[i,4] <-round(100*(table(IntVar[,29], IntVar[,1])[i,2]/(table(IntVar[,29], IntVar[,1])[i,1]+table(IntVar[,29], IntVar[,1])[i,2])), digits =2)
  UsualPlacePerc[i,5] <-table(IntVar[,29], IntVar[,1])[i,1]+table(IntVar[,29], IntVar[,1])[i,2]
  
}
UsualPlacePerc
#to find total N for group
#Excluding DK, Refused and NA
sum(UsualPlacePerc[,1])
sum(UsualPlacePerc[,3])
sum(UsualPlacePerc[,5])
sum(UsualPlacePerc[,1])/sum(UsualPlacePerc[,5]) * 100
sum(UsualPlacePerc[,3])/sum(UsualPlacePerc[,5]) * 100
chisq.test(IntVar[,29], IntVar[,1])
(table(IntVar[,29], IntVar[,1]))


#usual kind of care provider 
levels(IntVar$USPLKIND_A)[7:9]<- NA

UsualKindPerc <- matrix(nrow = nrow(table(IntVar[,30], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,30], IntVar[,1]))){
  UsualKindPerc[i,1] <- table(IntVar[,30], IntVar[,1])[i,1]
  UsualKindPerc[i,2] <-round(100*(table(IntVar[,30], IntVar[,1])[i,1]/(table(IntVar[,30], IntVar[,1])[i,1]+table(IntVar[,30], IntVar[,1])[i,2])), digits = 2)
  UsualKindPerc[i,3] <- table(IntVar[,30], IntVar[,1])[i,2]
  UsualKindPerc[i,4] <-round(100*(table(IntVar[,30], IntVar[,1])[i,2]/(table(IntVar[,30], IntVar[,1])[i,1]+table(IntVar[,30], IntVar[,1])[i,2])), digits =2)
  UsualKindPerc[i,5] <-table(IntVar[,30], IntVar[,1])[i,1]+table(IntVar[,30], IntVar[,1])[i,2]
  
}
UsualKindPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(UsualKindPerc[,1])
sum(UsualKindPerc[,3])
sum(UsualKindPerc[,5])
sum(UsualKindPerc[,1])/sum(UsualKindPerc[,5]) * 100
sum(UsualKindPerc[,3])/sum(UsualKindPerc[,5]) * 100
chisq.test(IntVar[,30], IntVar[,1])
(table(IntVar[,30], IntVar[,1]))


#hospitalized overnight past 12 months 
levels(IntVar$HOSPONGT_A)[3:5]<- NA

HospOverPerc <- matrix(nrow = nrow(table(IntVar[,31], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,31], IntVar[,1]))){
  HospOverPerc[i,1] <- table(IntVar[,31], IntVar[,1])[i,1]
  HospOverPerc[i,2] <-round(100*(table(IntVar[,31], IntVar[,1])[i,1]/(table(IntVar[,31], IntVar[,1])[i,1]+table(IntVar[,31], IntVar[,1])[i,2])), digits = 2)
  HospOverPerc[i,3] <- table(IntVar[,31], IntVar[,1])[i,2]
  HospOverPerc[i,4] <-round(100*(table(IntVar[,31], IntVar[,1])[i,2]/(table(IntVar[,31], IntVar[,1])[i,1]+table(IntVar[,31], IntVar[,1])[i,2])), digits =2)
  HospOverPerc[i,5] <-table(IntVar[,31], IntVar[,1])[i,1]+table(IntVar[,31], IntVar[,1])[i,2]
  
}
HospOverPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(HospOverPerc[,1])
sum(HospOverPerc[,3])
sum(HospOverPerc[,5])
sum(HospOverPerc[,1])/sum(HospOverPerc[,5]) * 100
sum(HospOverPerc[,3])/sum(HospOverPerc[,5]) * 100
chisq.test(IntVar[,31], IntVar[,1])
(table(IntVar[,31], IntVar[,1]))

##### Column 2 ########################
########################################
levels(IntVar$VIRAPPCVD_A)[3:4]<-NA

#to populate table of VIRAPPCVD_A  vs race
levels(IntVar$RACEALLP_A)[7:9] <- NA
table(nhis$RACEALLP_A, nhis$VIRAPPCVD_A)
#to find if sign differences appear
chisq.test(nhis$RACEALLP_A,nhis$VIRAPPCVD_A)

table(IntVar[,3], IntVar[,2])
#table(IntVar[,3], IntVar[,1])[1,1]+table(IntVar[,3], IntVar[,1])[1,2]

racePerc <- matrix(nrow = nrow(table(IntVar[,3], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,3], IntVar[,2]))){
  racePerc[i,1] <- table(IntVar[,3], IntVar[,2])[i,1]
  racePerc[i,2] <-round(100*(table(IntVar[,3], IntVar[,2])[i,1]/(table(IntVar[,3], IntVar[,2])[i,1]+table(IntVar[,3], IntVar[,2])[i,2])), digits = 2)
  racePerc[i,3] <- table(IntVar[,3], IntVar[,2])[i,2]
  racePerc[i,4] <-round(100*(table(IntVar[,3], IntVar[,2])[i,2]/(table(IntVar[,3], IntVar[,2])[i,1]+table(IntVar[,3], IntVar[,2])[i,2])), digits =2)
  racePerc[i,5] <-table(IntVar[,3], IntVar[,2])[i,1]+table(IntVar[,3], IntVar[,2])[i,2]
  
  }
racePerc
sum(racePerc[,1])
#to find total N for group
sum(racePerc[,1])
sum(racePerc[,3])
sum(racePerc[,5])
sum(racePerc[,1])/sum(racePerc[,5]) * 100
sum(racePerc[,3])/sum(racePerc[,5]) * 100
chisq.test(IntVar[,3], IntVar[,2])

#hisp

# to make a table where column 1 is Yes group 1 by Yes VM, 2 is percentage of each group for Yes VM appt, column 3 is Group 1 by No VM, column 5 is each group percent No VM, column 5 is Total for group
hispPerc <- matrix(nrow = nrow(table(IntVar[,4], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,4], IntVar[,2]))){
  hispPerc[i,1] <- table(IntVar[,4], IntVar[,2])[i,1]
  hispPerc[i,2] <-round(100*(table(IntVar[,4], IntVar[,2])[i,1]/(table(IntVar[,4], IntVar[,2])[i,1]+table(IntVar[,4], IntVar[,2])[i,2])), digits = 2)
  hispPerc[i,3] <- table(IntVar[,4], IntVar[,2])[i,2]
  hispPerc[i,4] <-round(100*(table(IntVar[,4], IntVar[,2])[i,2]/(table(IntVar[,4], IntVar[,2])[i,1]+table(IntVar[,4], IntVar[,2])[i,2])), digits =2)
  hispPerc[i,5] <-table(IntVar[,4], IntVar[,2])[i,1]+table(IntVar[,4], IntVar[,2])[i,2]
  
}
hispPerc
#to find total N for group
sum(hispPerc[,1])
sum(hispPerc[,3])
sum(hispPerc[,5])
sum(hispPerc[,1])/sum(hispPerc[,5]) * 100
sum(hispPerc[,3])/sum(hispPerc[,5]) * 100
chisq.test(IntVar[,4], IntVar[,2])


#Hispall
hispAllPerc <- matrix(nrow = nrow(table(IntVar[,5], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,5], IntVar[,2]))){
  hispAllPerc[i,1] <- table(IntVar[,5], IntVar[,2])[i,1]
  hispAllPerc[i,2] <-round(100*(table(IntVar[,5], IntVar[,2])[i,1]/(table(IntVar[,5], IntVar[,2])[i,1]+table(IntVar[,5], IntVar[,2])[i,2])), digits = 2)
  hispAllPerc[i,3] <- table(IntVar[,5], IntVar[,2])[i,2]
  hispAllPerc[i,4] <-round(100*(table(IntVar[,5], IntVar[,2])[i,2]/(table(IntVar[,5], IntVar[,2])[i,1]+table(IntVar[,5], IntVar[,2])[i,2])), digits =2)
  hispAllPerc[i,5] <-table(IntVar[,5], IntVar[,2])[i,1]+table(IntVar[,5], IntVar[,2])[i,2]
  
}
hispAllPerc
#to find total N for group
sum(hispAllPerc[,1])
sum(hispAllPerc[,3])
sum(hispAllPerc[,5])
sum(hispAllPerc[,1])/sum(hispAllPerc[,5]) * 100
sum(hispAllPerc[,3])/sum(hispAllPerc[,5]) * 100
(table(IntVar[,5], IntVar[,2]))
chisq.test(IntVar[,5], IntVar[,2])


#Hisdetp
levels(IntVar$HISDETP_A)[4:5]<-NA
hispdetPerc <- matrix(nrow = nrow(table(IntVar[,6], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,6], IntVar[,2]))){
  hispdetPerc[i,1] <- table(IntVar[,6], IntVar[,2])[i,1]
  hispdetPerc[i,2] <-round(100*(table(IntVar[,6], IntVar[,2])[i,1]/(table(IntVar[,6], IntVar[,2])[i,1]+table(IntVar[,6], IntVar[,2])[i,2])), digits = 2)
  hispdetPerc[i,3] <- table(IntVar[,6], IntVar[,2])[i,2]
  hispdetPerc[i,4] <-round(100*(table(IntVar[,6], IntVar[,2])[i,2]/(table(IntVar[,6], IntVar[,2])[i,1]+table(IntVar[,6], IntVar[,2])[i,2])), digits =2)
  hispdetPerc[i,5] <-table(IntVar[,6], IntVar[,2])[i,1]+table(IntVar[,6], IntVar[,2])[i,2]
  
}
hispdetPerc
#to find total N for group
sum(hispdetPerc[,1])
sum(hispdetPerc[,3])
sum(hispdetPerc[,5])
sum(hispdetPerc[,1])/sum(hispAllPerc[,5]) * 100
sum(hispdetPerc[,3])/sum(hispAllPerc[,5]) * 100
chisq.test(table(IntVar[,6], IntVar[,2]))
(table(IntVar[,6], IntVar[,2]))

#skipping age for now
#Sex
sexPerc <- matrix(nrow = nrow(table(IntVar[,11], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,11], IntVar[,2]))){
  sexPerc[i,1] <- table(IntVar[,11], IntVar[,2])[i,1]
  sexPerc[i,2] <-round(100*(table(IntVar[,11], IntVar[,2])[i,1]/(table(IntVar[,11], IntVar[,2])[i,1]+table(IntVar[,11], IntVar[,2])[i,2])), digits = 2)
  sexPerc[i,3] <- table(IntVar[,11], IntVar[,2])[i,2]
  sexPerc[i,4] <-round(100*(table(IntVar[,11], IntVar[,2])[i,2]/(table(IntVar[,11], IntVar[,2])[i,1]+table(IntVar[,11], IntVar[,2])[i,2])), digits =2)
  sexPerc[i,5] <-table(IntVar[,11], IntVar[,2])[i,1]+table(IntVar[,11], IntVar[,2])[i,2]
  
}
sexPerc
#to find total N for group
sum(sexPerc[,1])
sum(sexPerc[,3])
sum(sexPerc[,5])
sum(sexPerc[,1])/sum(sexPerc[,5]) * 100
sum(sexPerc[,3])/sum(sexPerc[,5]) * 100
chisq.test(table(IntVar[,11], IntVar[,2]))
(table(IntVar[,11], IntVar[,2]))


#Orientation
levels(IntVar[,12])[5:7]<- NA
OrPerc <- matrix(nrow = nrow(table(IntVar[,12], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,12], IntVar[,2]))){
  OrPerc[i,1] <- table(IntVar[,12], IntVar[,2])[i,1]
  OrPerc[i,2] <-round(100*(table(IntVar[,12], IntVar[,2])[i,1]/(table(IntVar[,12], IntVar[,2])[i,1]+table(IntVar[,12], IntVar[,2])[i,2])), digits = 2)
  OrPerc[i,3] <- table(IntVar[,12], IntVar[,2])[i,2]
  OrPerc[i,4] <-round(100*(table(IntVar[,12], IntVar[,2])[i,2]/(table(IntVar[,12], IntVar[,2])[i,1]+table(IntVar[,12], IntVar[,2])[i,2])), digits =2)
  OrPerc[i,5] <-table(IntVar[,12], IntVar[,2])[i,1]+table(IntVar[,12], IntVar[,2])[i,2]
  
}
OrPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(OrPerc[1:4,1])
sum(OrPerc[1:4,3])
sum(OrPerc[1:4,5])
sum(OrPerc[1:4,1])/sum(OrPerc[1:4,5]) * 100
sum(OrPerc[1:4,3])/sum(OrPerc[1:4,5]) * 100
chisq.test(table(IntVar[,12], IntVar[,2]))
(table(IntVar[,12], IntVar[,2]))

#Marital
levels(IntVar$MARITAL_A)[4:6]<- NA
MarPerc <- matrix(nrow = nrow(table(IntVar[,13], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,13], IntVar[,2]))){
  MarPerc[i,1] <- table(IntVar[,13], IntVar[,2])[i,1]
  MarPerc[i,2] <-round(100*(table(IntVar[,13], IntVar[,2])[i,1]/(table(IntVar[,13], IntVar[,2])[i,1]+table(IntVar[,13], IntVar[,2])[i,2])), digits = 2)
  MarPerc[i,3] <- table(IntVar[,13], IntVar[,2])[i,2]
  MarPerc[i,4] <-round(100*(table(IntVar[,13], IntVar[,2])[i,2]/(table(IntVar[,13], IntVar[,2])[i,1]+table(IntVar[,13], IntVar[,2])[i,2])), digits =2)
  MarPerc[i,5] <-table(IntVar[,13], IntVar[,2])[i,1]+table(IntVar[,13], IntVar[,2])[i,2]
  
}
MarPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(MarPerc[1:3,1])
sum(MarPerc[1:3,3])
sum(MarPerc[1:3,5])
sum(MarPerc[1:3,1])/sum(MarPerc[1:3,5]) * 100
sum(MarPerc[1:3,3])/sum(MarPerc[1:3,5]) * 100
chisq.test(table(IntVar[,13], IntVar[,2]))
(table(IntVar[,13], IntVar[,2]))

#InsCoverage
levels(IntVar$NOTCOV_A)[3]<-NA
CoveragePerc <- matrix(nrow = nrow(table(IntVar[,14], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,14], IntVar[,2]))){
  CoveragePerc[i,1] <- table(IntVar[,14], IntVar[,2])[i,1]
  CoveragePerc[i,2] <-round(100*(table(IntVar[,14], IntVar[,2])[i,1]/(table(IntVar[,14], IntVar[,2])[i,1]+table(IntVar[,14], IntVar[,2])[i,2])), digits = 2)
  CoveragePerc[i,3] <- table(IntVar[,14], IntVar[,2])[i,2]
  CoveragePerc[i,4] <-round(100*(table(IntVar[,14], IntVar[,2])[i,2]/(table(IntVar[,14], IntVar[,2])[i,1]+table(IntVar[,14], IntVar[,2])[i,2])), digits =2)
  CoveragePerc[i,5] <-table(IntVar[,14], IntVar[,2])[i,1]+table(IntVar[,14], IntVar[,2])[i,2]
  
}
CoveragePerc
#to find total N for group
#Excluding DK, Refused and NA
sum(CoveragePerc[1:2,1])
sum(CoveragePerc[1:2,3])
sum(CoveragePerc[1:2,5])
sum(CoveragePerc[1:2,1])/sum(CoveragePerc[1:2,5]) * 100
sum(CoveragePerc[1:2,3])/sum(CoveragePerc[1:2,5]) * 100
chisq.test(table(IntVar[,14], IntVar[,2]))
(table(IntVar[,14], IntVar[,2]))


#Employed last week
levels(IntVar$EMPWRKLSWK_A)[3]<-NA
WorkPerc <- matrix(nrow = nrow(table(IntVar[,15], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,15], IntVar[,2]))){
  WorkPerc[i,1] <- table(IntVar[,15], IntVar[,2])[i,1]
  WorkPerc[i,2] <-round(100*(table(IntVar[,15], IntVar[,2])[i,1]/(table(IntVar[,15], IntVar[,2])[i,1]+table(IntVar[,15], IntVar[,2])[i,2])), digits = 2)
  WorkPerc[i,3] <- table(IntVar[,15], IntVar[,2])[i,2]
  WorkPerc[i,4] <-round(100*(table(IntVar[,15], IntVar[,2])[i,2]/(table(IntVar[,15], IntVar[,2])[i,1]+table(IntVar[,15], IntVar[,2])[i,2])), digits =2)
  WorkPerc[i,5] <-table(IntVar[,15], IntVar[,2])[i,1]+table(IntVar[,15], IntVar[,2])[i,2]
  
}
WorkPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(WorkPerc[1:2,1])
sum(WorkPerc[1:2,3])
sum(WorkPerc[1:2,5])
sum(WorkPerc[1:2,1])/sum(WorkPerc[1:2,5]) * 100
sum(WorkPerc[1:2,3])/sum(WorkPerc[1:2,5]) * 100
chisq.test(table(IntVar[,15], IntVar[,2]))
(table(IntVar[,15], IntVar[,2]))


#Education
EduPerc <- matrix(nrow = nrow(table(IntVar[,32], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,32], IntVar[,2]))){
  EduPerc[i,1] <- table(IntVar[,32], IntVar[,2])[i,1]
  EduPerc[i,2] <-round(100*(table(IntVar[,32], IntVar[,2])[i,1]/(table(IntVar[,32], IntVar[,2])[i,1]+table(IntVar[,32], IntVar[,2])[i,2])), digits = 2)
  EduPerc[i,3] <- table(IntVar[,32], IntVar[,2])[i,2]
  EduPerc[i,4] <-round(100*(table(IntVar[,32], IntVar[,2])[i,2]/(table(IntVar[,32], IntVar[,2])[i,1]+table(IntVar[,32], IntVar[,2])[i,2])), digits =2)
  EduPerc[i,5] <-table(IntVar[,32], IntVar[,2])[i,1]+table(IntVar[,32], IntVar[,2])[i,2]
  
}
EduPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(EduPerc[,1])
sum(EduPerc[,3])
sum(EduPerc[,5])
sum(EduPerc[,1])/sum(EduPerc[,5]) * 100
sum(EduPerc[,3])/sum(EduPerc[,5]) * 100
chisq.test(table(IntVar[,32], IntVar[,2]))
(table(IntVar[,32], IntVar[,2]))


#Urban
UrbanPerc <- matrix(nrow = nrow(table(IntVar[,20], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,20], IntVar[,2]))){
  UrbanPerc[i,1] <- table(IntVar[,20], IntVar[,2])[i,1]
  UrbanPerc[i,2] <-round(100*(table(IntVar[,20], IntVar[,2])[i,1]/(table(IntVar[,20], IntVar[,2])[i,1]+table(IntVar[,20], IntVar[,2])[i,2])), digits = 2)
  UrbanPerc[i,3] <- table(IntVar[,20], IntVar[,2])[i,2]
  UrbanPerc[i,4] <-round(100*(table(IntVar[,20], IntVar[,2])[i,2]/(table(IntVar[,20], IntVar[,2])[i,1]+table(IntVar[,20], IntVar[,2])[i,2])), digits =2)
  UrbanPerc[i,5] <-table(IntVar[,20], IntVar[,2])[i,1]+table(IntVar[,20], IntVar[,2])[i,2]
  
}
UrbanPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(UrbanPerc[,1])
sum(UrbanPerc[,3])
sum(UrbanPerc[,5])
sum(UrbanPerc[,1])/sum(UrbanPerc[,5]) * 100
sum(UrbanPerc[,3])/sum(UrbanPerc[,5]) * 100
chisq.test(table(IntVar[,20], IntVar[,2]))
(table(IntVar[,20], IntVar[,2]))



#Region
RegionPerc <- matrix(nrow = nrow(table(IntVar[,21], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,21], IntVar[,2]))){
  RegionPerc[i,1] <- table(IntVar[,21], IntVar[,2])[i,1]
  RegionPerc[i,2] <-round(100*(table(IntVar[,21], IntVar[,2])[i,1]/(table(IntVar[,21], IntVar[,2])[i,1]+table(IntVar[,21], IntVar[,2])[i,2])), digits = 2)
  RegionPerc[i,3] <- table(IntVar[,21], IntVar[,2])[i,2]
  RegionPerc[i,4] <-round(100*(table(IntVar[,21], IntVar[,2])[i,2]/(table(IntVar[,21], IntVar[,2])[i,1]+table(IntVar[,21], IntVar[,2])[i,2])), digits =2)
  RegionPerc[i,5] <-table(IntVar[,21], IntVar[,2])[i,1]+table(IntVar[,21], IntVar[,2])[i,2]
  
}
RegionPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(RegionPerc[,1])
sum(RegionPerc[,3])
sum(RegionPerc[,5])
sum(RegionPerc[,1])/sum(RegionPerc[,5]) * 100
sum(RegionPerc[,3])/sum(RegionPerc[,5]) * 100
chisq.test(table(IntVar[,21], IntVar[,2]))
(table(IntVar[,21], IntVar[,2]))

#health status

#to drop IDK and refused
# levels(IntVar$PHSTAT_A)[6:7] <- NA
statPerc <- matrix(nrow = nrow(table(IntVar[,22], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,22], IntVar[,2]))){
  statPerc[i,1] <- table(IntVar[,22], IntVar[,2])[i,1]
  statPerc[i,2] <-round(100*(table(IntVar[,22], IntVar[,2])[i,1]/(table(IntVar[,22], IntVar[,2])[i,1]+table(IntVar[,22], IntVar[,2])[i,2])), digits = 2)
  statPerc[i,3] <- table(IntVar[,22], IntVar[,2])[i,2]
  statPerc[i,4] <-round(100*(table(IntVar[,22], IntVar[,2])[i,2]/(table(IntVar[,22], IntVar[,2])[i,1]+table(IntVar[,22], IntVar[,2])[i,2])), digits =2)
  statPerc[i,5] <-table(IntVar[,22], IntVar[,2])[i,1]+table(IntVar[,22], IntVar[,2])[i,2]
  
}
statPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(statPerc[,1])
sum(statPerc[,3])
sum(statPerc[,5])
sum(statPerc[,1])/sum(statPerc[,5]) * 100
sum(statPerc[,3])/sum(statPerc[,5]) * 100
chisq.test(table(IntVar[,22], IntVar[,2]))
(table(IntVar[,22], IntVar[,2]))

#Mental Health RX
#levels(IntVar$MHRX_A)[3:5]<- NA

MHPerc <- matrix(nrow = nrow(table(IntVar[,23], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,23], IntVar[,2]))){
  MHPerc[i,1] <- table(IntVar[,23], IntVar[,2])[i,1]
  MHPerc[i,2] <-round(100*(table(IntVar[,23], IntVar[,2])[i,1]/(table(IntVar[,23], IntVar[,2])[i,1]+table(IntVar[,23], IntVar[,2])[i,2])), digits = 2)
  MHPerc[i,3] <- table(IntVar[,23], IntVar[,2])[i,2]
  MHPerc[i,4] <-round(100*(table(IntVar[,23], IntVar[,2])[i,2]/(table(IntVar[,23], IntVar[,2])[i,1]+table(IntVar[,23], IntVar[,2])[i,2])), digits =2)
  MHPerc[i,5] <-table(IntVar[,23], IntVar[,2])[i,1]+table(IntVar[,23], IntVar[,2])[i,2]
  
}
MHPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(MHPerc[,1])
sum(MHPerc[,3])
sum(MHPerc[,5])
sum(MHPerc[,1])/sum(MHPerc[,5]) * 100
sum(MHPerc[,3])/sum(MHPerc[,5]) * 100
chisq.test(table(IntVar[,23], IntVar[,2]))
(table(IntVar[,23], IntVar[,2]))


#Mental Health Therapy
levels(IntVar$MHTHRPY_A)[3:5]<- NA

ther <- matrix(nrow = nrow(table(IntVar[,24], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,24], IntVar[,2]))){
  ther[i,1] <- table(IntVar[,24], IntVar[,2])[i,1]
  ther[i,2] <-round(100*(table(IntVar[,24], IntVar[,2])[i,1]/(table(IntVar[,24], IntVar[,2])[i,1]+table(IntVar[,24], IntVar[,2])[i,2])), digits = 2)
  ther[i,3] <- table(IntVar[,24], IntVar[,2])[i,2]
  ther[i,4] <-round(100*(table(IntVar[,24], IntVar[,2])[i,2]/(table(IntVar[,24], IntVar[,2])[i,1]+table(IntVar[,24], IntVar[,2])[i,2])), digits =2)
  ther[i,5] <-table(IntVar[,24], IntVar[,2])[i,1]+table(IntVar[,24], IntVar[,2])[i,2]
  
}
ther
#to find total N for group
#Excluding DK, Refused and NA
sum(ther[,1])
sum(ther[,3])
sum(ther[,5])
sum(ther[,1])/sum(ther[,5]) * 100
sum(ther[,3])/sum(ther[,5]) * 100
chisq.test(table(IntVar[,24], IntVar[,2]))
(table(IntVar[,24], IntVar[,2]))

#Covid Dx test
levels(IntVar$CVDDIAG_A)[3:5]<- NA

CVDdxPerc <- matrix(nrow = nrow(table(IntVar[,25], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,25], IntVar[,2]))){
  CVDdxPerc[i,1] <- table(IntVar[,25], IntVar[,2])[i,1]
  CVDdxPerc[i,2] <-round(100*(table(IntVar[,25], IntVar[,2])[i,1]/(table(IntVar[,25], IntVar[,2])[i,1]+table(IntVar[,25], IntVar[,2])[i,2])), digits = 2)
  CVDdxPerc[i,3] <- table(IntVar[,25], IntVar[,2])[i,2]
  CVDdxPerc[i,4] <-round(100*(table(IntVar[,25], IntVar[,2])[i,2]/(table(IntVar[,25], IntVar[,2])[i,1]+table(IntVar[,25], IntVar[,2])[i,2])), digits =2)
  CVDdxPerc[i,5] <-table(IntVar[,25], IntVar[,2])[i,1]+table(IntVar[,25], IntVar[,2])[i,2]
  
}
CVDdxPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(CVDdxPerc[,1])
sum(CVDdxPerc[,3])
sum(CVDdxPerc[,5])
sum(CVDdxPerc[,1])/sum(CVDdxPerc[,5]) * 100
sum(CVDdxPerc[,3])/sum(CVDdxPerc[,5]) * 100
chisq.test(IntVar[,25], IntVar[,2])
(table(IntVar[,25], IntVar[,2]))

#Covid Res
levels(IntVar$CVDRSLT_A)[3:4]<- NA

CVDResPerc <- matrix(nrow = nrow(table(IntVar[,26], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,26], IntVar[,2]))){
  CVDResPerc[i,1] <- table(IntVar[,26], IntVar[,2])[i,1]
  CVDResPerc[i,2] <-round(100*(table(IntVar[,26], IntVar[,2])[i,1]/(table(IntVar[,26], IntVar[,2])[i,1]+table(IntVar[,26], IntVar[,2])[i,2])), digits = 2)
  CVDResPerc[i,3] <- table(IntVar[,26], IntVar[,2])[i,2]
  CVDResPerc[i,4] <-round(100*(table(IntVar[,26], IntVar[,2])[i,2]/(table(IntVar[,26], IntVar[,2])[i,1]+table(IntVar[,26], IntVar[,2])[i,2])), digits =2)
  CVDResPerc[i,5] <-table(IntVar[,26], IntVar[,2])[i,1]+table(IntVar[,26], IntVar[,2])[i,2]
  
}
CVDResPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(CVDResPerc[,1])
sum(CVDResPerc[,3])
sum(CVDResPerc[,5])
sum(CVDResPerc[,1])/sum(CVDResPerc[,5]) * 100
sum(CVDResPerc[,3])/sum(CVDResPerc[,5]) * 100
chisq.test(IntVar[,26], IntVar[,2])
(table(IntVar[,26], IntVar[,2]))

#Delay Care due to Covid 
levels(IntVar$DLYCARE_A)[3:5]<- NA

CVDDelPerc <- matrix(nrow = nrow(table(IntVar[,27], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,27], IntVar[,2]))){
  CVDDelPerc[i,1] <- table(IntVar[,27], IntVar[,2])[i,1]
  CVDDelPerc[i,2] <-round(100*(table(IntVar[,27], IntVar[,2])[i,1]/(table(IntVar[,27], IntVar[,2])[i,1]+table(IntVar[,27], IntVar[,2])[i,2])), digits = 2)
  CVDDelPerc[i,3] <- table(IntVar[,27], IntVar[,2])[i,2]
  CVDDelPerc[i,4] <-round(100*(table(IntVar[,27], IntVar[,2])[i,2]/(table(IntVar[,27], IntVar[,2])[i,1]+table(IntVar[,27], IntVar[,2])[i,2])), digits =2)
  CVDDelPerc[i,5] <-table(IntVar[,27], IntVar[,2])[i,1]+table(IntVar[,27], IntVar[,2])[i,2]
  
}
CVDDelPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(CVDDelPerc[,1])
sum(CVDDelPerc[,3])
sum(CVDDelPerc[,5])
sum(CVDDelPerc[,1])/sum(CVDDelPerc[,5]) * 100
sum(CVDDelPerc[,3])/sum(CVDDelPerc[,5]) * 100
chisq.test(IntVar[,27], IntVar[,2])
(table(IntVar[,27], IntVar[,2]))

#Did not get Care due to Covid 
levels(IntVar$DNGCARE_A)[3:5]<- NA

CVDDNGPerc <- matrix(nrow = nrow(table(IntVar[,28], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,28], IntVar[,2]))){
  CVDDNGPerc[i,1] <- table(IntVar[,28], IntVar[,2])[i,1]
  CVDDNGPerc[i,2] <-round(100*(table(IntVar[,28], IntVar[,2])[i,1]/(table(IntVar[,28], IntVar[,2])[i,1]+table(IntVar[,28], IntVar[,2])[i,2])), digits = 2)
  CVDDNGPerc[i,3] <- table(IntVar[,28], IntVar[,2])[i,2]
  CVDDNGPerc[i,4] <-round(100*(table(IntVar[,28], IntVar[,2])[i,2]/(table(IntVar[,28], IntVar[,2])[i,1]+table(IntVar[,28], IntVar[,2])[i,2])), digits =2)
  CVDDNGPerc[i,5] <-table(IntVar[,28], IntVar[,2])[i,1]+table(IntVar[,28], IntVar[,2])[i,2]
  
}
CVDDNGPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(CVDDNGPerc[,1])
sum(CVDDNGPerc[,3])
sum(CVDDNGPerc[,5])
sum(CVDDNGPerc[,1])/sum(CVDDNGPerc[,5]) * 100
sum(CVDDNGPerc[,3])/sum(CVDDNGPerc[,5]) * 100
chisq.test(IntVar[,28], IntVar[,2])
(table(IntVar[,28], IntVar[,2]))

#usual place of care
levels(IntVar$USUALPL_A)[4:6]<- NA

UsualPlacePerc <- matrix(nrow = nrow(table(IntVar[,29], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,29], IntVar[,2]))){
  UsualPlacePerc[i,1] <- table(IntVar[,29], IntVar[,2])[i,1]
  UsualPlacePerc[i,2] <-round(100*(table(IntVar[,29], IntVar[,2])[i,1]/(table(IntVar[,29], IntVar[,2])[i,1]+table(IntVar[,29], IntVar[,2])[i,2])), digits = 2)
  UsualPlacePerc[i,3] <- table(IntVar[,29], IntVar[,2])[i,2]
  UsualPlacePerc[i,4] <-round(100*(table(IntVar[,29], IntVar[,2])[i,2]/(table(IntVar[,29], IntVar[,2])[i,1]+table(IntVar[,29], IntVar[,2])[i,2])), digits =2)
  UsualPlacePerc[i,5] <-table(IntVar[,29], IntVar[,2])[i,1]+table(IntVar[,29], IntVar[,2])[i,2]
  
}
UsualPlacePerc
#to find total N for group
#Excluding DK, Refused and NA
sum(UsualPlacePerc[,1])
sum(UsualPlacePerc[,3])
sum(UsualPlacePerc[,5])
sum(UsualPlacePerc[,1])/sum(UsualPlacePerc[,5]) * 100
sum(UsualPlacePerc[,3])/sum(UsualPlacePerc[,5]) * 100
chisq.test(IntVar[,29], IntVar[,2])
(table(IntVar[,29], IntVar[,2]))


#usual kind of care provider 
levels(IntVar$USPLKIND_A)[7:9]<- NA

UsualKindPerc <- matrix(nrow = nrow(table(IntVar[,30], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,30], IntVar[,2]))){
  UsualKindPerc[i,1] <- table(IntVar[,30], IntVar[,2])[i,1]
  UsualKindPerc[i,2] <-round(100*(table(IntVar[,30], IntVar[,2])[i,1]/(table(IntVar[,30], IntVar[,2])[i,1]+table(IntVar[,30], IntVar[,2])[i,2])), digits = 2)
  UsualKindPerc[i,3] <- table(IntVar[,30], IntVar[,2])[i,2]
  UsualKindPerc[i,4] <-round(100*(table(IntVar[,30], IntVar[,2])[i,2]/(table(IntVar[,30], IntVar[,2])[i,1]+table(IntVar[,30], IntVar[,2])[i,2])), digits =2)
  UsualKindPerc[i,5] <-table(IntVar[,30], IntVar[,2])[i,1]+table(IntVar[,30], IntVar[,2])[i,2]
  
}
UsualKindPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(UsualKindPerc[,1])
sum(UsualKindPerc[,3])
sum(UsualKindPerc[,5])
sum(UsualKindPerc[,1])/sum(UsualKindPerc[,5]) * 100
sum(UsualKindPerc[,3])/sum(UsualKindPerc[,5]) * 100
chisq.test(IntVar[,30], IntVar[,2])
(table(IntVar[,30], IntVar[,2]))


#hospitalized overnight past 12 months 
levels(IntVar$HOSPONGT_A)[3:5]<- NA

HospOverPerc <- matrix(nrow = nrow(table(IntVar[,31], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,31], IntVar[,2]))){
  HospOverPerc[i,1] <- table(IntVar[,31], IntVar[,2])[i,1]
  HospOverPerc[i,2] <-round(100*(table(IntVar[,31], IntVar[,2])[i,1]/(table(IntVar[,31], IntVar[,2])[i,1]+table(IntVar[,31], IntVar[,2])[i,2])), digits = 2)
  HospOverPerc[i,3] <- table(IntVar[,31], IntVar[,2])[i,2]
  HospOverPerc[i,4] <-round(100*(table(IntVar[,31], IntVar[,2])[i,2]/(table(IntVar[,31], IntVar[,2])[i,1]+table(IntVar[,31], IntVar[,2])[i,2])), digits =2)
  HospOverPerc[i,5] <-table(IntVar[,31], IntVar[,2])[i,1]+table(IntVar[,31], IntVar[,2])[i,2]
  
}
HospOverPerc
#to find total N for group
#Excluding DK, Refused and NA
sum(HospOverPerc[,1])
sum(HospOverPerc[,3])
sum(HospOverPerc[,5])
sum(HospOverPerc[,1])/sum(HospOverPerc[,5]) * 100
sum(HospOverPerc[,3])/sum(HospOverPerc[,5]) * 100
chisq.test(IntVar[,31], IntVar[,2])
(table(IntVar[,31], IntVar[,2]))

#################AGE##########################
#dichotomous Over Under 85
#dichotomous Over Under 85

Over85Perc <- matrix(nrow = nrow(table(IntVar[,10], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,10], IntVar[,1]))){
  Over85Perc[i,1] <- table(IntVar[,10], IntVar[,1])[i,1]
  Over85Perc[i,2] <-round(100*(table(IntVar[,10], IntVar[,1])[i,1]/(table(IntVar[,10], IntVar[,1])[i,1]+table(IntVar[,10], IntVar[,1])[i,2])), digits = 2)
  Over85Perc[i,3] <- table(IntVar[,10], IntVar[,1])[i,2]
  Over85Perc[i,4] <-round(100*(table(IntVar[,10], IntVar[,1])[i,2]/(table(IntVar[,10], IntVar[,1])[i,1]+table(IntVar[,10], IntVar[,1])[i,2])), digits =2)
  Over85Perc[i,5] <-table(IntVar[,10], IntVar[,1])[i,1]+table(IntVar[,10], IntVar[,1])[i,2]
  
}
Over85Perc
#to find total N for group
#Excluding DK, Refused and NA
sum(Over85Perc[,1])
sum(Over85Perc[,3])
sum(Over85Perc[,5])
sum(Over85Perc[,1])/sum(Over85Perc[,5]) * 100
sum(Over85Perc[,3])/sum(Over85Perc[,5]) * 100
chisq.test(IntVar[,10], IntVar[,1])
(table(IntVar[,10], IntVar[,1]))



Over85Perc <- matrix(nrow = nrow(table(IntVar[,10], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,10], IntVar[,2]))){
  Over85Perc[i,1] <- table(IntVar[,10], IntVar[,2])[i,1]
  Over85Perc[i,2] <-round(100*(table(IntVar[,10], IntVar[,2])[i,1]/(table(IntVar[,10], IntVar[,2])[i,1]+table(IntVar[,10], IntVar[,2])[i,2])), digits = 2)
  Over85Perc[i,3] <- table(IntVar[,10], IntVar[,2])[i,2]
  Over85Perc[i,4] <-round(100*(table(IntVar[,10], IntVar[,2])[i,2]/(table(IntVar[,10], IntVar[,2])[i,1]+table(IntVar[,10], IntVar[,2])[i,2])), digits =2)
  Over85Perc[i,5] <-table(IntVar[,10], IntVar[,2])[i,1]+table(IntVar[,10], IntVar[,2])[i,2]
  
}
Over85Perc
#to find total N for group
#Excluding DK, Refused and NA
sum(Over85Perc[,1])
sum(Over85Perc[,3])
sum(Over85Perc[,5])
sum(Over85Perc[,1])/sum(Over85Perc[,5]) * 100
sum(Over85Perc[,3])/sum(Over85Perc[,5]) * 100
chisq.test(IntVar[,10], IntVar[,2])
(table(IntVar[,10], IntVar[,2]))


##############Income######################
IncomePerc <- matrix(nrow = nrow(table(IntVar[,18], IntVar[,1])
), ncol =5)
for(i in 1:nrow(table(IntVar[,18], IntVar[,1]))){
  IncomePerc[i,1] <- table(IntVar[,18], IntVar[,1])[i,1]
  IncomePerc[i,2] <-round(100*(table(IntVar[,18], IntVar[,1])[i,1]/(table(IntVar[,18], IntVar[,1])[i,1]+table(IntVar[,18], IntVar[,1])[i,2])), digits = 2)
  IncomePerc[i,3] <- table(IntVar[,18], IntVar[,1])[i,2]
  IncomePerc[i,4] <-round(100*(table(IntVar[,18], IntVar[,1])[i,2]/(table(IntVar[,18], IntVar[,1])[i,1]+table(IntVar[,18], IntVar[,1])[i,2])), digits =2)
  IncomePerc[i,5] <-table(IntVar[,18], IntVar[,1])[i,1]+table(IntVar[,18], IntVar[,1])[i,2]
  
}
IncomePerc
#to find total N for group
#Excluding DK, Refused and NA
sum(IncomePerc[,1])
sum(IncomePerc[,3])
sum(IncomePerc[,5])
sum(IncomePerc[,1])/sum(IncomePerc[,5]) * 100
sum(IncomePerc[,3])/sum(IncomePerc[,5]) * 100
chisq.test(IntVar[,18], IntVar[,1])
(table(IntVar[,18], IntVar[,1]))



IncomePerc <- matrix(nrow = nrow(table(IntVar[,18], IntVar[,2])
), ncol =5)
for(i in 1:nrow(table(IntVar[,18], IntVar[,2]))){
  IncomePerc[i,1] <- table(IntVar[,18], IntVar[,2])[i,1]
  IncomePerc[i,2] <-round(100*(table(IntVar[,18], IntVar[,2])[i,1]/(table(IntVar[,18], IntVar[,2])[i,1]+table(IntVar[,18], IntVar[,2])[i,2])), digits = 2)
  IncomePerc[i,3] <- table(IntVar[,18], IntVar[,2])[i,2]
  IncomePerc[i,4] <-round(100*(table(IntVar[,18], IntVar[,2])[i,2]/(table(IntVar[,18], IntVar[,2])[i,1]+table(IntVar[,18], IntVar[,2])[i,2])), digits =2)
  IncomePerc[i,5] <-table(IntVar[,18], IntVar[,2])[i,1]+table(IntVar[,18], IntVar[,2])[i,2]
  
}
IncomePerc
#to find total N for group
#Excluding DK, Refused and NA
sum(IncomePerc[,1])
sum(IncomePerc[,3])
sum(IncomePerc[,5])
sum(IncomePerc[,1])/sum(IncomePerc[,5]) * 100
sum(IncomePerc[,3])/sum(IncomePerc[,5]) * 100
chisq.test(IntVar[,18], IntVar[,2])
(table(IntVar[,18], IntVar[,2]))

#to calc income as continuous v virtual
wilcox.test(IntVar$FAMINCTCU250 ~ IntVar$VIRAPP12M_A, paired = F)
wilcox.test(IntVar$FAMINCTCU250 ~ IntVar$VIRAPPCVD_A, paired = F)

#to calc second quarter population
table(nhis$INTV_QRT, nhis$RECTYPE)[3]+table(nhis$INTV_QRT, nhis$RECTYPE)[4]





#########################################################
################## Model ################################
#all referred to Field et al. ch 8 on log regression

mod1a<-glm(VIRAPP12M_A ~ HISPALLP_A,family = binomial,na.action = na.omit, data = IntVar)
summary(mod1a)
mod1<-glm(VIRAPP12M_A ~ RACEALLP_A + HISP_A,family = binomial,na.action = na.omit, data = IntVar)
summary(mod1)

mod2<-glm(VIRAPP12M_A ~ RACEALLP_A + HISP_A + AGE84U + 
            FAMINCTCU250  + SEX_A + ORIENT_A +
            NOTCOV_A + EMPWRKLSWK_A + EDUC_SIM + URBRRL
          + REGION + PHSTAT_A + MHRX_A + MHTHRPY_A +
          CVDDIAG_A + DLYCARE_A + DNGCARE_A + USUALPL_A
          +USPLKIND_A + HOSPONGT_A,
          family = binomial,na.action = na.omit, data = IntVar)
summary(mod2)
modelChi_2 <- mod2$null.deviance-mod2$deviance
modelChi_2
chidf_2 <- mod2$df.null - mod2$df.residual

chisqprob_2 <- 1 - pchisq(modelChi_2, chidf_2)
chisqprob_2

 #hosmer lemeshow
R2.hl_2 <- modelChi_2/mod2$null.deviance
R2.hl_2
#cox snell
#11036 is n in model
R.cs_2 <- 1 - exp((mod2$deviance - mod2$null.deviance) / 11043)
R.cs_2

#nagelkerke
R.n_2 <- R.cs_2 / (1-(exp(-(mod2$null.deviance/11043))))
R.n_2

#odds ratios
exp(mod2$coefficients)
#conf intervals
exp(confint(mod2))


# trying a more restrained model 
#note is a worse fit
#mod3<-glm(VIRAPP12M_A ~ RACEALLP_A + HISP_A + AGE84U + 
 #           FAMINCTCU250  + SEX_A + EDUC_SIM + URBRRL + REGION +
  #        PHSTAT_A + MHRX_A + MHTHRPY_A +
   #         CVDDIAG_A + DLYCARE_A + USPLKIND_A  ,
    #      family = binomial,na.action = na.omit, data = IntVar)
#summary(mod3)
#modelChi <- mod3$null.deviance-mod3$deviance
#modelChi
#chidf <- mod3$df.null - mod3$df.residual

#chisqprob <- 1 - pchisq(modelChi, chidf)
#chisqprob

#hosmer lemeshow
#R2.hl <- modelChi/mod3$null.deviance
#R2.hl
#cox snell
#11036 is n in model
#R.cs <- 1 - exp((mod3$deviance - mod3$null.deviance) / 11434)
#R.cs

#nagelkerke
#R.n <- R.cs / (1-(exp(-(mod3$null.deviance/11434))))
#R.n

#odds ratios
#exp(mod3$coefficients)
#conf intervals
#exp(confint(mod3))

#table of probs and resides
teleData <- matrix(nrow = 11043, ncol = 6)
teleData[,1] <- fitted(mod2)
teleData[,2] <- rstandard(mod2)
teleData[,3] <- rstudent(mod2)
teleData[,4] <- dfbeta(mod2)
teleData[,5] <- dffits(mod2)
teleData[,6] <- hatvalues( mod2)
colnames(teleData)<- c("predictedProb","StandardResid","StudentResid","DFBeta", "DFFit","Leverage")
View(teleData) 

#lev = number of predictors + 1 / n, per field
#for mod 2
lev <- 21 / 11043

IntVarMod2 <- IntVar[,c("VIRAPP12M_A" , "RACEALLP_A",  "HISP_A", "AGE84U" , 
            "FAMINCTCU250",   "SEX_A" , "ORIENT_A" ,
            "NOTCOV_A" , "EMPWRKLSWK_A" , "EDUC_SIM" , "URBRRL",
          "REGION",  "PHSTAT_A",  "MHRX_A" , "MHTHRPY_A",
          "CVDDIAG_A" , "DLYCARE_A" , "DNGCARE_A" , "USUALPL_A",
          "USPLKIND_A", "HOSPONGT_A")]
IntVarMod2 <- na.omit(IntVarMod2)

IntVarMod2 <- cbind(IntVarMod2, teleData)
modComp <- matrix(nrow=40, ncol = 9)
modComp[,1]<- exp(mod2$coefficients)
modComp[,2:3]<-exp(confint(mod2))
colnames(modComp)<- c("mod2 odds", "mod 2 lower CI", "mod2 upper CI","mod3 odds", "mod 3 lower CI", "mod3 upper CI","mod4 odds", "mod 4 lower CI", "mod4 upper CI" )
rownames(modComp) <- names(mod2$coefficients)

#investigate outliers
IntVarOutLiers <- IntVarMod2[which(IntVarMod2$Leverage>(3*lev)),]
View(IntVarOutLiers)
###seeing what happens if remove outliers
IntVarOutLiersRemoved <- IntVarMod2[which(!IntVarMod2$Leverage>3*lev),]

mod3<-glm(VIRAPP12M_A ~ RACEALLP_A + HISP_A + AGE84U + 
            FAMINCTCU250  + SEX_A + ORIENT_A +
            NOTCOV_A + EMPWRKLSWK_A + EDUC_SIM + URBRRL
          + REGION + PHSTAT_A + MHRX_A + MHTHRPY_A +
            CVDDIAG_A + DLYCARE_A + DNGCARE_A + USUALPL_A
          +USPLKIND_A + HOSPONGT_A,
          family = binomial,na.action = na.omit, data = IntVarOutLiers)
summary(mod3)

modelChi_3 <- mod3$null.deviance-mod3$deviance
modelChi_3
chidf_3 <- mod3$df.null - mod3$df.residual
chisqprob_3 <- 1 - pchisq(modelChi_3, chidf_3)
chisqprob_3

R2.hl_3 <- modelChi_3/mod3$null.deviance
R2.hl_3
#cox snell
#11036 is n in model
R.cs_3 <- 1 - exp((mod3$deviance - mod3$null.deviance) / 11036)
R.cs_3

#nagelkerke
R.n_3 <- R.cs_3 / (1-(exp(-(mod2$null.deviance/11036))))
R.n_3


modComp[,4]<-exp(mod3$coefficients)
modComp[,5:6]<-exp(confint(mod3))

mod4<-glm(VIRAPP12M_A ~ RACEALLP_A + HISP_A + AGE84U + 
            FAMINCTCU250  + SEX_A + ORIENT_A +
            NOTCOV_A + EMPWRKLSWK_A + EDUC_SIM + URBRRL
          + REGION + PHSTAT_A + MHRX_A + MHTHRPY_A +
            CVDDIAG_A + DLYCARE_A + DNGCARE_A + USUALPL_A
          +USPLKIND_A + HOSPONGT_A,
          family = binomial,na.action = na.omit, data = IntVarOutLiersRemoved)
summary(mod4)
modComp[1:11,7]<-exp(mod4$coefficients[1:11])
modComp[14:38,7]<-exp(mod4$coefficients[12:36])
modComp[40,7]<-exp(mod4$coefficients[37])

modComp[1:11,8:9]<-exp(confint(mod4)[1:11])
modComp[14:38,8:9]<-exp(confint(mod4)[12:36])
modComp[40,8:9]<-exp(confint(mod4)[37])
#modComp$Mod2Sig <- ifelse(modComp[,2]<1 & modComp[,3]> 1, 0, 1)
modCompdf <- data.frame(modComp)
colnames(modCompdf)<- c("mod2 odds", "mod 2 lower CI", "mod2 upper CI","mod3 odds", "mod 3 lower CI", "mod3 upper CI","mod4 odds", "mod 4 lower CI", "mod4 upper CI" )
modCompdf$Mod2Sig <- ifelse(modCompdf[,2]<1 & modCompdf[,3]> 1, 0, 1)
modCompdf$Mod3Sig <- ifelse(modCompdf[,5]<1 & modCompdf[,6]> 1, 0, 1)
modCompdf$Mod4Sig <- ifelse(modCompdf[,8]<1 & modCompdf[,9]> 1, 0, 1)
write.csv(modCompdf, "modcomp.csv")


modelChi_4 <- mod4$null.deviance-mod4$deviance
modelChi_4
chidf_4 <- mod4$df.null - mod4$df.residual
chisqprob_4 <- 1 - pchisq(modelChi_4, chidf_4)
chisqprob_4

R2.hl_4 <- modelChi_4/mod4$null.deviance
R2.hl_4
#cox snell
#11036 is n in model
R.cs_4 <- 1 - exp((mod4$deviance - mod4$null.deviance) / 11036)
R.cs_4

#nagelkerke
R.n_4 <- R.cs_4 / (1-(exp(-(mod4$null.deviance/11036))))
R.n_4





IntVarMod2$LargeResid <- ifelse(IntVarMod2$StandardResid > 2, 1, ifelse(IntVarMod2$StandardResid < -2, 1, 0))
IntVarMod2$LargeResid <- factor(IntVarMod2$LargeResid)
IntVarMod2$cooks <-cooks.distance(mod2)
IntVarMod2$covar <-covratio(mod2)

library(dplyr)
LargeResid <- filter(IntVarMod2, IntVarMod2$LargeResid == 1)
vif(mod2)
sum(vif(mod2)[,1])/21
1/vif(mod2)


mod7<-glm(VIRAPPCVD_A ~ RACEALLP_A + HISP_A + AGE84U + 
            FAMINCTCU250  + SEX_A + ORIENT_A +
            NOTCOV_A + EMPWRKLSWK_A + EDUC_SIM + URBRRL
          + REGION + PHSTAT_A + MHRX_A + MHTHRPY_A +
            CVDDIAG_A + DLYCARE_A + DNGCARE_A + USUALPL_A
          +USPLKIND_A + HOSPONGT_A,
          family = binomial,na.action = na.omit, data = IntVar)
summary(mod7)
modelChi <- mod7$null.deviance-mod7$deviance
modelChi
chidf <- mod7$df.null - mod7$df.residual

chisqprob <- 1 - pchisq(modelChi, chidf)
chisqprob

#hosmer lemeshow
R2.hl <- modelChi/mod7$null.deviance
R2.hl
#cox snell
#3680 is n in model
R.cs <- 1 - exp((mod7$deviance - mod7$null.deviance) / 3680)
R.cs

#nagelkerke
R.n <- R.cs / (1-(exp(-(mod7$null.deviance/3680))))
R.n

#odds ratios
exp(mod7$coefficients)
#conf intervals
exp(confint(mod7))

covData <- matrix(nrow = 3680, ncol = 6)
covData[,1] <- fitted(mod7)
covData[,2] <- rstandard(mod7)
covData[,3] <- rstudent(mod7)
covData[,4] <- dfbeta(mod7)
covData[,5] <- dffits(mod7)
covData[,6] <- hatvalues( mod7)
colnames(covData)<- c("predictedProb","StandardResid","StudentResid","DFBeta", "DFFit","Leverage")
View(covData) 
IntVarMod7 <- IntVar[,c("VIRAPPCVD_A" , "RACEALLP_A",  "HISP_A", "AGE84U" , 
                        "FAMINCTCU250",   "SEX_A" , "ORIENT_A" ,
                        "NOTCOV_A" , "EMPWRKLSWK_A" , "EDUC_SIM" , "URBRRL",
                        "REGION",  "PHSTAT_A",  "MHRX_A" , "MHTHRPY_A",
                        "CVDDIAG_A" , "DLYCARE_A" , "DNGCARE_A" , "USUALPL_A",
                        "USPLKIND_A", "HOSPONGT_A")]
IntVarMod7 <- na.omit(IntVarMod7)

IntVarMod7 <- cbind(IntVarMod7, covData)
#lev = number of predictors + 1 / n, per field
#for mod 2
lev7 <- 21 / 3680
IntVarOutLiers7 <- IntVarMod7[which(IntVarMod7$Leverage>(3*lev7)),]
View(IntVarOutLiers)

IntVarMod7$LargeResid <- ifelse(IntVarMod7$StandardResid > 2, 1, ifelse(IntVarMod7$StandardResid < -2, 1, 0))
IntVarMod7$LargeResid <- factor(IntVarMod7$LargeResid)
IntVarMod7$cooks <-cooks.distance(mod7)
IntVarMod7$covar <-covratio(mod7)

library(dplyr)
LargeResid7 <- filter(IntVarMod7, IntVarMod7$LargeResid == 1)
vif(mod7)
sum(vif(mod7)[,1])/21
1/vif(mod7)

############################################3
##############Compare Outliers and Not######

VirAppMat <- matrix(nrow = 2, ncol = 2)
VirAppMat[1,1:2]<- table(IntVarOutLiers[,1])
VirAppMat[2,1:2]<- table(IntVarOutLiersRemoved[,1])
chisq.test(VirAppMat)
#sig

raceMat <-  matrix(nrow = 2, ncol = 6)
raceMat[1,1:6]<- table(IntVarOutLiers[,2])
raceMat[2,1:6]<- table(IntVarOutLiersRemoved[,2])
chisq.test(raceMat)
#sig

HispMat <- matrix(nrow = 2, ncol = 2)
HispMat[1,1:2]<- table(IntVarOutLiers[,3])
HispMat[2,1:2]<- table(IntVarOutLiersRemoved[,3])
chisq.test(HispMat)
#sig
t.test(IntVarOutLiers[,4], IntVarOutLiersRemoved[,4], paired = F)
#sig
t.test(IntVarOutLiers[,5], IntVarOutLiersRemoved[,5], paired = F)
#sig
sexMat <- matrix(nrow = 2, ncol = 2)
sexMat[1,1:2]<- table(IntVarOutLiers[,6])
sexMat[2,1:2]<- table(IntVarOutLiersRemoved[,6])
chisq.test(sexMat)
#sig
orienMat <- matrix(nrow = 2, ncol = 4)
orienMat[1,1:4]<- table(IntVarOutLiers[,7])
orienMat[2,1:4]<- table(IntVarOutLiersRemoved[,7])
chisq.test(orienMat)
#sig

covMat <- matrix(nrow = 2, ncol = 2)
covMat[1,1:2]<- table(IntVarOutLiers[,8])
covMat[2,1:2]<- table(IntVarOutLiersRemoved[,8])
chisq.test(covMat)
#sig

empMat <- matrix(nrow = 2, ncol = 2)
empMat[1,1:2]<- table(IntVarOutLiers[,9])
empMat[2,1:2]<- table(IntVarOutLiersRemoved[,9])
chisq.test(empMat)
#sig

eduMat <- matrix(nrow = 2, ncol = 4)
eduMat[1,1:4]<- table(IntVarOutLiers[,10])
eduMat[2,1:4]<- table(IntVarOutLiersRemoved[,10])
chisq.test(eduMat)
#sig

urbMat <- matrix(nrow = 2, ncol = 4)
urbMat[1,1:4]<- table(IntVarOutLiers[,11])
urbMat[2,1:4]<- table(IntVarOutLiersRemoved[,11])
chisq.test(urbMat)
#sig

regionMat <- matrix(nrow = 2, ncol = 4)
regionMat[1,1:4]<- table(IntVarOutLiers[,12])
regionMat[2,1:4]<- table(IntVarOutLiersRemoved[,12])
chisq.test(regionMat)
#sig

healthMat<-matrix(nrow = 2, ncol = 5)
healthMat[1,1:5]<- table(IntVarOutLiers[,13])
healthMat[2,1:5]<- table(IntVarOutLiersRemoved[,13])
chisq.test(healthMat)
#sig

mhdxMat <- matrix(nrow = 2, ncol = 2)
mhdxMat[1,1:2]<- table(IntVarOutLiers[,14])
mhdxMat[2,1:2]<- table(IntVarOutLiersRemoved[,14])
chisq.test(mhdxMat)
#sig

mhtherMat <- matrix(nrow = 2, ncol = 2)
mhtherMat[1,1:2]<- table(IntVarOutLiers[,15])
mhtherMat[2,1:2]<- table(IntVarOutLiersRemoved[,15])
chisq.test(mhtherMat)
#sig

CVDdxMat <- matrix(nrow = 2, ncol = 2)
CVDdxMat[1,1:2]<- table(IntVarOutLiers[,16])
CVDdxMat[2,1:2]<- table(IntVarOutLiersRemoved[,16])
chisq.test(CVDdxMat)
#sig

delayMat <- matrix(nrow = 2, ncol = 2)
delayMat[1,1:2]<- table(IntVarOutLiers[,17])
delayMat[2,1:2]<- table(IntVarOutLiersRemoved[,17])
chisq.test(delayMat)
#sig

denyMat <- matrix(nrow = 2, ncol = 2)
denyMat[1,1:2]<- table(IntVarOutLiers[,18])
denyMat[2,1:2]<- table(IntVarOutLiersRemoved[,18])
chisq.test(denyMat)
#sig

UsualMat <- matrix(nrow = 2, ncol = 3)
UsualMat[1,1:3]<- table(IntVarOutLiers[,19])
UsualMat[2,1:3]<- table(IntVarOutLiersRemoved[,19])
#usual Mat's second column is zero, deleting to allow chisq to work
UsualMat <- UsualMat[,-2]
chisq.test(UsualMat)
#sig

careMat <- matrix(nrow = 2, ncol = 6)
careMat[1,1:6]<- table(IntVarOutLiers[,20])
careMat[2,1:6]<- table(IntVarOutLiersRemoved[,20])
chisq.test(careMat)
#sig

hospMat <- matrix(nrow = 2, ncol = 2)
hospMat[1,1:2]<- table(IntVarOutLiers[,21])
hospMat[2,1:2]<- table(IntVarOutLiersRemoved[,21])
chisq.test(hospMat)
#sig

### Experiment with interactions:
#Unclear if anything coming up will run as separate models
mod_int1<-glm(VIRAPP12M_A ~ RACEALLP_A + HISP_A + AGE84U + 
            FAMINCTCU250  + SEX_A + ORIENT_A +
            NOTCOV_A + EMPWRKLSWK_A + EDUC_SIM + URBRRL
          + REGION + PHSTAT_A + MHRX_A + MHTHRPY_A +
            CVDDIAG_A + DLYCARE_A + DNGCARE_A + USUALPL_A
          +USPLKIND_A + HOSPONGT_A   +  HISP_A:CVDDIAG_A,
          family = binomial, na.action = na.omit, data = IntVar)
summary(mod_int1)

#after swapping in variables, does not seem clear to have an interaction


#for the covid var


mod8<-glm(VIRAPPCVD_A ~ RACEALLP_A + HISP_A + AGE84U + 
            FAMINCTCU250  + SEX_A + ORIENT_A +
            NOTCOV_A + EMPWRKLSWK_A + EDUC_SIM + URBRRL
          + REGION + PHSTAT_A + MHRX_A + MHTHRPY_A +
            CVDDIAG_A + DLYCARE_A + DNGCARE_A + USUALPL_A
          +USPLKIND_A + HOSPONGT_A,
          family = binomial,na.action = na.omit, data = IntVar)
summary(mod8)
modelChi_8 <- mod8$null.deviance-mod8$deviance
modelChi_8
chidf_8 <- mod8$df.null - mod8$df.residual

chisqprob_8 <- 1 - pchisq(modelChi_8, chidf_8)
chisqprob_8

#hosmer lemeshow
R2.hl_8 <- modelChi/mod8$null.deviance
R2.hl_8
#cox snell
#11036 is n in model
R.cs_8 <- 1 - exp((mod8$deviance - mod8$null.deviance) / 3680)
R.cs_8

#nagelkerke
R.n_8 <- R.cs / (1-(exp(-(mod8$null.deviance/3680))))
R.n_8

#odds ratios
exp(mod8$coefficients)
#conf intervals
exp(confint(mod8))


#table of probs and resides
CVDteleData <- matrix(nrow = 3680, ncol = 6)
CVDteleData[,1] <- fitted(mod8)
CVDteleData[,2] <- rstandard(mod8)
CVDteleData[,3] <- rstudent(mod8)
CVDteleData[,4] <- dfbeta(mod8)
CVDteleData[,5] <- dffits(mod8)
CVDteleData[,6] <- hatvalues( mod8)
colnames(CVDteleData)<- c("predictedProb","StandardResid","StudentResid","DFBeta", "DFFit","Leverage")
View(CVDteleData) 

#lev = number of predictors + 1 / n, per field
#for mod 2
lev8 <- 21 / 3680

IntVarMod8 <- IntVar[,c("VIRAPPCVD_A" , "RACEALLP_A",  "HISP_A", "AGE84U" , 
                        "FAMINCTCU250",   "SEX_A" , "ORIENT_A" ,
                        "NOTCOV_A" , "EMPWRKLSWK_A" , "EDUC_SIM" , "URBRRL",
                        "REGION",  "PHSTAT_A",  "MHRX_A" , "MHTHRPY_A",
                        "CVDDIAG_A" , "DLYCARE_A" , "DNGCARE_A" , "USUALPL_A",
                        "USPLKIND_A", "HOSPONGT_A")]
IntVarMod8 <- na.omit(IntVarMod8)

IntVarMod8 <- cbind(IntVarMod8, CVDteleData)
IntVarMod8Outliers <- IntVarMod8[(which(IntVarMod8$Leverage>(3*lev8))),]
IntVarMod8NoOutliers <- IntVarMod8[(which(!IntVarMod8$Leverage>(3*lev8))),]


mod8coef<-exp(mod8$coefficients)
#conf intervals
mod8coef<- cbind(mod8coef,exp(confint(mod8)))



mod9<-glm(VIRAPPCVD_A ~ RACEALLP_A + HISP_A + AGE84U + 
            FAMINCTCU250  + SEX_A + ORIENT_A +
            NOTCOV_A + EMPWRKLSWK_A + EDUC_SIM + URBRRL
          + REGION + PHSTAT_A + MHRX_A + MHTHRPY_A +
            CVDDIAG_A + DLYCARE_A + DNGCARE_A + USUALPL_A
          +USPLKIND_A + HOSPONGT_A,
          family = binomial,na.action = na.omit, data = IntVarMod8Outliers)
summary(mod9)
modelChi_9 <- mod9$null.deviance-mod9$deviance
modelChi_9
chidf_9 <- mod9$df.null - mod9$df.residual

chisqprob_9 <- 1 - pchisq(modelChi_9, chidf_9)
chisqprob_9

#hosmer lemeshow
R2.hl_9 <- modelChi/mod9$null.deviance
R2.hl_9
#cox snell
#11036 is n in model
R.cs_9 <- 1 - exp((mod9$deviance - mod9$null.deviance) / 469)
R.cs_9

#nagelkerke
R.n_9 <- R.cs / (1-(exp(-(mod9$null.deviance/469))))
R.n_9

#odds ratios
mod9coef<- exp(mod9$coefficients)
#conf intervals
mod9coef<- cbind(mod9coef,exp(confint(mod9)))
write.csv(mod9coef, "mod9coef.csv")




mod10<-glm(VIRAPPCVD_A ~ RACEALLP_A + HISP_A + AGE84U + 
            FAMINCTCU250  + SEX_A + ORIENT_A +
            NOTCOV_A + EMPWRKLSWK_A + EDUC_SIM + URBRRL
          + REGION + PHSTAT_A + MHRX_A + MHTHRPY_A +
            CVDDIAG_A + DLYCARE_A + DNGCARE_A + USUALPL_A
          +USPLKIND_A + HOSPONGT_A,
          family = binomial,na.action = na.omit, data = IntVarMod8NoOutliers)
summary(mod10)
modelChi_10 <- mod10$null.deviance-mod10$deviance
modelChi_10
chidf_10 <- mod10$df.null - mod10$df.residual

chisqprob_10 <- 1 - pchisq(modelChi_10, chidf_10)
chisqprob_10

#hosmer lemeshow
R2.hl_10 <- modelChi/mod10$null.deviance
R2.hl_10
#cox snell
#11036 is n in model
R.cs_10 <- 1 - exp((mod10$deviance - mod10$null.deviance) / 3211)
R.cs_10

#nagelkerke
R.n_10 <- R.cs / (1-(exp(-(mod10$null.deviance/3211))))
R.n_10

#odds ratios
mod10coef<- exp(mod10$coefficients)
#conf intervals
mod10coef<- cbind(mod10coef,exp(confint(mod10)))
write.csv(mod10coef, "mod10coef.csv")

