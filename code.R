orders = read.xlsx("flatiron_qs_orders_admins_july_16.xlsx", 1) 
administration = read.xlsx("flatiron_qs_orders_admins_july_16.xlsx", 2)
demographics = read.xlsx("flatiron_qs_orders_admins_july_16.xlsx", 3)
patients = read.xlsx("flatiron_qs_orders_admins_july_16.xlsx", 4)
practices = read.xlsx("flatiron_qs_orders_admins_july_16.xlsx", 5)

dim()
demographics[duplicated(demographics$patient_id),] 
##Question1 What is the average time elapsed between a patient’s initial diagnosis date and a patient’s first treatment? Does this time vary by gender?
orders2 = orders
orders2 = orders2[, -5]
orders2$order_date = as.Date(orders2$order_date, "%d-%b-%Y") 
patients[patients$patient_id=="h9993d", ]$diagnosis_date ->date1
min(administration[administration$patient_id=="h9993d", ]$administered_date) ->date2 
date2 - date1 # Time difference of 7501 days
subset2 = merge(patients, administration[, c(1, 4)], by.x = "patient_id", by.y = "patient_id")
subset2[with(subset2, order(subset2[,1], subset2[, 7])), ] ->test
for(i in nrow(patients)){
  
}
##Question2 How many patients are on nivolumab from 2012�2016?
subset1 = subset(administration, administration$administered_date>="2012-01-01" & administration$administered_date<="2016-12-31" &administration$drug_name=="nivolumab" )
length(unique(subset1$patient_id))

##Question3 Using the following risk stratification rules, please summarize the number of high, medium, and low risk patients.
for(i in 1:nrow(demographics2)){
  if((demographics2[i, 2]=="female"&demographics2[i, 4]=="NON_WHITE")|(demographics2[i, 2]=="male"&demographics2[i, 4]=="NON_WHITE" & demographics2[i, 3]>=70)|(demographics2[i, 2]=="unknown"&demographics2[i, 4]=="NON_WHITE" & demographics2[i, 3]>=70)){
    demographics2[i, 5] = "High_Risk"
  }
  else if((demographics2[i, 2]=="female"&demographics2[i, 4]=="WHITE" & demographics2[i, 3]>=75)|(demographics2[i, 2]=="male"&demographics2[i, 4]=="NON_WHITE" & demographics2[i, 3]<70)){
    demographics2[i, 5] = "Medium_Risk"
  }
  else if((demographics2[i, 2]=="female"&demographics2[i, 4]=="WHITE" & demographics2[i, 3]<75)|(demographics2[i, 2]=="male"&demographics2[i, 4]=="WHITE")|(demographics2[i, 2]=="unknown"&demographics2[i, 4]=="WHITE" & demographics2[i, 3]<75) ){
    demographics2[i, 5] = "Low_Risk"
  }
  else if((demographics2[i, 2]=="unknown"&demographics2[i, 4]=="WHITE" & demographics2[i, 3]>=75)){
    demographics2[i, 5] = "Medium_to_Low_Risk"
  }
  else if((demographics2[i, 2]=="unknown"&demographics2[i, 4]=="NON_WHITE" & demographics2[i, 3]<70)){
    demographics2[i, 5] = "High_to_Medium_Risk"
  }
}
## Put assumptions here

##Question4 Please create a visualization that could be used to help a medical researcher? understand how drug prevalence has changed over time.
##Due to the small sample size, a ggplot of drug prevalence based on year has been created. 
table(administration$drug_name)

p <- ggplot(test, aes(x=year, y=Freq, group=drug_name, label=test[,3])) +geom_line(aes(color=drug_name))+geom_point(aes(color=drug_name)) + geom_text(size=3.5) + labs(title = "Drug prevalence changed over years", x="Year", y ="Total Orders")
p



