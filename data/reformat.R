library(data.table)
library(dplyr)

mdir = 'C:/Users/NORTH/source/incident_phenotyping/data/'

embeddingname = paste0(mdir,'VA_embed_500_20220607.csv')

dataname = paste0(mdir,'MERCK_RCC_Codified_data_monthly_counts_2023-02-08.csv')

all_data = fread(dataname,data.table=F);
all_embeddings = fread(embeddingname,data.table=F);
# fwrite(data.frame(t(data.frame(all_embeddings)),header=T),file = paste0(mdir,'embedding_selected_rcc.csv'))

code_names = unique(all_data$code)
#print(all_data$patient_num)
#data_people = split(all_data,all_data$patient_num)
#n_people = length(data_people)

flag = 0
threshold = 0.1


n_code = length(code_names)
# n_code = dim(all_embeddings)[1]

emb_anchor <- select(filter(all_embeddings, codes %in% 'PheCode:189.11'),-codes)
# print(emb_anchor)
score_list = list()
#top_list = rep(0,400)

#print('PheCode:189.11' %in% code_names)

# Calculating codes similarity from the codes in EHR data
for (i in 1:n_code){

  code_name = as.character(code_names[i])
  # print(code_name)
  if (!(code_name %in% all_embeddings$codes)){
    next
  }

  embed_i = select(filter(all_embeddings, codes %in% code_name),-codes)
  score <- sum( embed_i * emb_anchor) / (sqrt(sum(embed_i ^ 2)) * sqrt(sum(emb_anchor ^ 2)))
  if (score >= threshold){
    print(code_name)
    score_list <- c(score_list,code_name)

    if (flag == 0){
      embedding_selected <- t(filter(all_embeddings, codes %in% code_name))
      flag = 1
    }
    else{
      embedding_selected <- cbind(embedding_selected,t(filter(all_embeddings, codes %in% code_name)))
    }
  }

  if (i %% 100 == 0){
    print(i)
    print(paste0(as.character(i/n_code*100),'%'))
  }
}


print(length(score_list))
pp = score_list
score_list <- data.frame(score_list)
names(score_list) = pp
fwrite(score_list,file = paste0(mdir,'codes_selected.csv'))
embedding_selected <- data.frame(embedding_selected)
colnames(embedding_selected) <- embedding_selected[1,]
embedding_selected <- embedding_selected[-1,]
fwrite(embedding_selected,file = paste0(mdir,'embedding_selected_RCC.csv'))

mdir = 'C:/Users/NORTH/source/incident_phenotyping/data/'

dataname = paste0(mdir,'MERCK_RCC_Codified_data_monthly_counts_2023-02-08.csv')
#codes_selected_name = paste0(mdir,'codes_selected.csv')

#codes_selected = fread(codes_selected_name,check.names=FALSE)
codes_selected = pp
all_data = fread(dataname,data.table=F,check.names=FALSE);
#print(all_data$patient_num)
data_people = split(all_data,all_data$patient_num)
n_people = length(data_people)

code_names = unique(all_data$code)
#n_code = length(code_names)
n_code = length(codes_selected)
col_names = c('ID','Y','T',codes_selected)
data_long = rep(0,3+n_code)
names(data_long) = col_names

for (i in 1:(n_people/8)){
  data_person = data_people[[i]]
  data_date = split(data_person,data_person$month_num)
  n_date = length(data_date)
  data = data.frame(matrix(0,n_date,3+n_code))
  names(data) = col_names

  for (j in  1:n_date){
    data_person_date = data_date[[j]]

    for (k in 1: dim(data_person_date)[1]){
        if(data_person_date[k,'code'] %in% codes_selected){
          data[j,data_person_date[k,'code']] <- as.numeric(data_person_date[k,'count'])
        }
    }
    data[j,'T'] <- max(data_person_date$month_num)

    # 在出现keycode数据点周围（-1，1）的数据，认为其label是1
    if ((max(data_person_date$month_num) >= -1 && max(data_person_date$month_num) <= 1) || 'PheCode:189.11' %in% data_person_date$code){
      data[j,'Y'] <- 1
    }
    else{
      data[j,'Y'] <- 0
    }

  }
  data[,'ID'] <- rep(max(data_person$patient_num),n_date)
  data_long = rbind(data_long,data)

  if (i %% 100 == 0){
    print(paste0(as.character((i/(n_people/8))*100),'%'))
  }
}

names(data_long) = col_names
fwrite(data_long[2:dim(data_long)[1],], file = paste0(mdir,'data_longitudinal.csv'))
print('Success------------')
