library(data.table)
library(dplyr)

mdir = 'C:/Users/NORTH/source/incident_phenotyping/data/'

dataname = paste0(mdir,'MERCK_RCC_Codified_data_monthly_counts_2023-02-08.csv')

all_data = fread(dataname,data.table=F);
#print(all_data$patient_num)
data_people = split(all_data,all_data$patient_num)
n = length(data_people)

code_names = unique(all_data$code)
n_code = length(code_names)
col_names = c('ID','Y','T',code_names)
data_long = rep(0,3+n_code)
names(data_long) = col_names

for (i in 1:n){
  data_person = data_people[[i]]
  data_date = split(data_person,data_person$month_num)
  n_date = length(data_date)
  data = data.frame(matrix(0,n_date,3+n_code))
  names(data) = col_names

  for (j in  1:n_date){
    data_person_date = data_date[[j]]
    apply(data_person_date,1,function (x)
    {data[j,as.character(x['code'])] <- as.numeric(x['count'])})
  }

  data_long = rbind(data_long,data)

}

fwrite(data.frame(data_long), file = paste0(mdir,'data_longitudinal'))