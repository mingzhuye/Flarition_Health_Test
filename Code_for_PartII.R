#Data Processing Steps
library(xlsx)
##Initialize all 5 datasets, orders, administration, demographics, patients and practices. 
orders = read.xlsx("flatiron_qs_orders_admins_july_16.xlsx", 1) 
administration = read.xlsx("flatiron_qs_orders_admins_july_16.xlsx", 2)
demographics = read.xlsx("flatiron_qs_orders_admins_july_16.xlsx", 3)
patients = read.xlsx("flatiron_qs_orders_admins_july_16.xlsx", 4)
practices = read.xlsx("flatiron_qs_orders_admins_july_16.xlsx", 5)
##simple view of the datasets
head(orders)
head(administration)
head(patients)
head(demographics)
head(practices)
##look at structures of the datasets
str(orders)
str(administration)
str(demographics)
str(patients)
str(practices)
##look at dimensions of the datasets
dim(orders)
dim(administration)
dim(demographics)
dim(patients)
dim(practices)
## check whether there are duplicated observations in each dataset
orders[duplicated(orders$order_id), ]
administration[duplicated(administration$order_id), ] 
demographics[duplicated(demographics$patient_id), ] ##duplicated confirmed, need further clean
patients[duplicated(patients$patient_id), ]##duplicated confirmed, need further clean
practices[duplicated(practices), ] ##practices is clean
patients2 = patients[!duplicated(patients$patient_id), ]
demographics2 = demographics[!duplicated(demographics$patient_id), ]
length(unique(demographics2$patient_id));table(demographics2$race);table(demographics2$gender) 
## NOTE:demographics2 has consistent format and is tidy, except that there are four patients without gender information.

##Unify time formats in dataset order, administration and patients
fmts <- c("%d-%b-%Y", "%Y-%m-%d") ##observed from the datasets' structure, there are two time formats mixed in one column. 
orders2 = orders[, -5]
orders2$order_date = as.Date(as.numeric(apply(outer(orders2$order_date, fmts, as.Date), 1, na.omit)), "1970-01-01")
administration2 = administration
administration2$administered_date = as.Date(as.numeric(apply(outer(administration2$administered_date, fmts, as.Date), 1, na.omit)), "1970-01-01")
patients2$diagnosis_date = as.Date(as.numeric(apply(outer(patients2$diagnosis_date, fmts, as.Date), 1, na.omit)), "1970-01-01")
patients2$advanced_diagnosis_date = as.Date(as.numeric(apply(outer(patients2$advanced_diagnosis_date, fmts, as.Date), 1, na.omit)), "1970-01-01")

##Intersection and differences among datasets
length(unique(orders2$patient_id)) ##99
length(unique(administration2$patient_id)) ##100 
length(unique(patients$patient_id)) ##99
setdiff(unique(patients2$patient_id), unique(orders2$patient_id)) #patient_id ="t8292i"
setdiff(unique(administration2$patient_id), unique(orders2$patient_id)) #patient_id = "t8292i"
setdiff(unique(orders2$patient_id), unique(patients2$patient_id))#patient_id = "p2814y"
setdiff(unique(administration2$patient_id), unique(patients2$patient_id))#patient_id ="p2814y"
##NOTE:patient_id contained in first column of administration2 is complete. While, patient_id in orders2 and patients2 separately misses "t8292i" and "p2814y". 

orders2[is.na(orders2), ]
administration2[is.na(administration2), ]
patients2[is.na(patients2$diagnosis_date), ]
patients2[is.na(patients2$advanced_diagnosis_date), ]
##NOTE: orders2 and administration2 have consistent format and clean without NAs.
##NOTE: patients is almost clean, except contain 2 NAs in column 'diagnosis_date', one NA in column 'advanced_diagnosis_date'.

#Questions of PartII
##1. Average time elapsed between initial diagnosis and first treatment
df1 = merge(patients2[, c(1,5,6)], administration2[,c(1,4)], by.x ="patient_id", by.y="patient_id", all.x=T)
df2 = df1[with(df1, order(df1[,1], df1[, 4])), ] 
df2[, 5] = df2[,4] - df2[, 2]
colnames(df2)[5] = "date_diff" ## observed from the merged table df2, there do exist cases where the first administered_date is before the initial diagnose date. Under such case, I will use the earlist administered_date after initial diagnose date for a patient.  
df2= df2[!is.na(df2$diagnosis_date), ] ## remove the NAs contained in diagnosis_date column.
dim(df2) 
subdf2 = data.frame(matrix(nrow=length(unique(df2$patient_id)), ncol=2))#initialize an empty data frame
subdf2[, 1] = unique(df2$patient_id)
for (i in 1:nrow(subdf2)){
  subdf2[i, 2] = sort(subset(df2, df2$patient_id==subdf2[i,1] & df2$date_diff>=0)$date_diff)[1] # to apply the smallest positive different days for each patient. 
}
subdf2 = merge(subdf2, demographics2[,1:2], by.x="X1", by.y="patient_id")
colnames(subdf2) = c("patient_id", "date_diff", "gender")
mean(subdf2$date_diff)#14025.56
mean(subset(subdf2, gender=="male")$date_diff)#12693.5
mean(subset(subdf2, gender=="female")$date_diff)#15114.65

##2. Patients on drug nivolumab from 2012-2016
administration2[,6] = format(administration2[, 4], "%Y")
colnames(administration2)[6] = 'year'
nivo = subset(administration2, year %in% c(2012, 2013, 2014, 2015) & drug_name == "nivolumab")
dim(nivo) #nrow = 100, ncol = 6
length(unique(nivo$order_id))  #100 unique order_ids
length(unique(nivo$patient_id)) #62 unique patient_ids

##3. Add new stratification to dataset demographics
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
##NOTE: Since there are 4 patients without gender information, they couldn't be added risk levels using original stratification rules. New classification was inspired from the origial ones, with 'High_to_Medium_Risk','Medium_to_Low_Risk ' being added. 
colnames(demographics2)[5] = 'Cancer_Risk_Level'
head(demographics2,3)
table(demographics2$Cancer_Risk_Level)

##4. How drug prevalence has changed over time
library(ggplot2)
administration2[,6] = format(administration2[, 4], "%Y")
colnames(administration2)[6] = 'year'
administration2[,6] = factor(administration2[,6 ])
table(administration2$drug_name, administration2$year)
drug = data.frame(table(administration2$drug_name, administration2$year))
drug$year #13 levels 
colnames(drug) = c("drug_name", "year", "Freq")
p1 = ggplot(drug, aes(x=year, y=Freq, group=drug_name, label=drug[,3]))+ geom_line(aes(color=drug_name))+geom_point(aes(color=drug_name)) + geom_text(size=3.5) + labs(title = "Drug prevalence changed over years", x="Year", y ="Total Orders")
p1
####Observed from the plot p1, "bevacizumab" and "erlotinib hcl"'s prevalence has been dramatically increased, while the other two are not as significantly increasing. Overall, the drug prevalence has been increased over years. 
drug2 = subset(drug, year %in% c(2005, 2009, 2010, 2011, 2012, 2013, 2014, 2015))
p2 = ggplot(drug2, aes(x=year, y=Freq, group=drug_name, label=drug2[,3]))+ geom_line(aes(color=drug_name))+geom_point(aes(color=drug_name)) + geom_text(size=3.5) + labs(title = "Drug prevalence changed over years", x="Year", y ="Total Orders")
p2



