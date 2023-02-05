library(dplyr)
mecha_car_table<-read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
mecha_car_lm<-lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD,data=mecha_car_table)
summary(mecha_car_lm)


suspension_coil_table<-read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
total_summary<-suspension_coil_table%>%summarise(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))
lot_summary<-suspension_coil_table%>%group_by(Manufacturing_Lot)%>%summarise(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))


t.test(suspension_coil_table$PSI,mu=1500)
t.test(subset(suspension_coil_table,Manufacturing_Lot="Lot1")$PSI,mu=1500)
t.test(subset(suspension_coil_table,Manufacturing_Lot="Lot2")$PSI,mu=1500)
t.test(subset(suspension_coil_table,Manufacturing_Lot="Lot3")$PSI,mu=1500)
