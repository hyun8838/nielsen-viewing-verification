library(dplyr)
library(hms)
########## 0918 ##############
MET_only_0918 <- names(df0918)

tx3_df_only_0918 <- tx3_df %>%
  filter(date == '20240918') %>%
  select(id) %>%
  unique() %>%
  as.vector()
tx3_df_only_0918 <- tx3_df_only_0918$id

tx3_df_filtered_only_0918 <- tx3_df_filtered %>%
  filter(date == '20240918') %>%
  select(id) %>%
  unique() %>%
  as.vector()
tx3_df_filtered_only_0918 <- tx3_df_filtered_only_0918$id

combined_df_filtered_only_0918 <- combined_df_filtered %>%
  filter(date == '20240918') %>%
  select(id) %>%
  unique() %>%
  as.vector()
combined_df_filtered_only_0918 <- combined_df_filtered_only_0918$id

length(combined_df_filtered_only_0918)
length(tx3_df_only_0918)
length(tx3_df_filtered_only_0918)


################ 0919 ####################
MET_only_0919 <- names(df0919)

tx3_df_only_0919 <- tx3_df %>%
  filter(date == '20240919') %>%
  select(id) %>%
  unique() %>%
  as.vector()
tx3_df_only_0919 <- tx3_df_only_0919$id

tx3_df_filtered_only_0919 <- tx3_df_filtered %>%
  filter(date == '20240919') %>%
  select(id) %>%
  unique() %>%
  as.vector()
tx3_df_filtered_only_0919 <- tx3_df_filtered_only_0919$id

combined_df_filtered_only_0919 <- combined_df_filtered %>%
  filter(date == '20240919') %>%
  select(id) %>%
  unique() %>%
  as.vector()
combined_df_filtered_only_0919 <- combined_df_filtered_only_0919$id


################ 0920 ####################
MET_only_0920 <- names(df0920)

tx3_df_only_0920 <- tx3_df %>%
  filter(date == '20240920') %>%
  select(id) %>%
  unique() %>%
  as.vector()
tx3_df_only_0920 <- tx3_df_only_0920$id

tx3_df_filtered_only_0920 <- tx3_df_filtered %>%
  filter(date == '20240920') %>%
  select(id) %>%
  unique() %>%
  as.vector()
tx3_df_filtered_only_0920 <- tx3_df_filtered_only_0920$id

combined_df_filtered_only_0920 <- combined_df_filtered %>%
  filter(date == '20240920') %>%
  select(id) %>%
  unique() %>%
  as.vector()
combined_df_filtered_only_0920 <- combined_df_filtered_only_0920$id






###### 미터기에만 존재 #####
setdiff(MET_only_0918, tx3_df_only_0918)

MET_diff_0918 <- setdiff(MET_only_0918, tx3_df_filtered_only_0918)
MET_diff_0919 <- setdiff(MET_only_0919, tx3_df_filtered_only_0919)
MET_diff_0920 <- setdiff(MET_only_0920, tx3_df_filtered_only_0920)

length(MET_diff_0918); length(MET_diff_0919); length(MET_diff_0920)

df0922[as.character(c(MET_diff_0919[331]))]

tx3_df_filtered %>%
  filter(id == '5600472' & date == '20240919')

combined_df_filtered %>%
  filter(id == '5600472' & date == '20240919')
df0921_5$'1403812'


###### tx3에만 존재 #####
# 한개도 X
tx3_diff_0918 <- setdiff(tx3_df_only_0918, MET_only_0918)
tx3_diff_0919 <- setdiff(tx3_df_only_0919, MET_only_0919)
tx3_diff_0920 <- setdiff(tx3_df_only_0920, MET_only_0920)

length(tx3_diff_0918); length(tx3_diff_0919); length(tx3_diff_0920); 



###### tx3 filtered(빈 ID 삭제)에만 존재 #####
# 한개도 X
tx3_filtered_diff_0918 <- setdiff(tx3_df_filtered_only_0918, MET_only_0918)
tx3_filtered_diff_0919 <- setdiff(tx3_df_filtered_only_0919, MET_only_0919)
tx3_filtered_diff_0920 <- setdiff(tx3_df_filtered_only_0920, MET_only_0920)


length(tx3_filtered_diff_0918); length(tx3_filtered_diff_0919); length(tx3_filtered_diff_0920); 



###### 우리 코드에만 존재 #####
comb_diff_0918 <- setdiff(combined_df_filtered_only_0918, tx3_df_filtered_only_0918)
comb_diff_0919 <- setdiff(combined_df_filtered_only_0919, tx3_df_filtered_only_0919)
comb_diff_0920 <- setdiff(combined_df_filtered_only_0920, tx3_df_filtered_only_0920)

length(comb_diff_0918); length(comb_diff_0919); length(comb_diff_0920)


df0919[as.character(c(comb_diff_0919[2]))]

tx3_df_filtered %>%
  filter(id == comb_diff_0919[2] & date == '20240919')

combined_df_filtered %>%
  filter(id == comb_diff_0919[2] & date == '20240919')





###### TX3 코드에만 존재 #####
tx3_comb_diff_0918 <- setdiff(tx3_df_filtered_only_0918, combined_df_filtered_only_0918)
tx3_comb_diff_0919 <- setdiff(tx3_df_filtered_only_0919, combined_df_filtered_only_0919)
tx3_comb_diff_0920 <- setdiff(tx3_df_filtered_only_0920, combined_df_filtered_only_0920)

length(tx3_comb_diff_0918); length(tx3_comb_diff_0919); length(tx3_comb_diff_0920)



df0918[as.character(c(tx3_comb_diff_0918[15]))]

tx3_df_filtered %>%
  filter(id == tx3_comb_diff_0918[5] & date == '20240918')

combined_df_filtered %>%
  filter(id == tx3_comb_diff_0919[2] & date == '20240919')


options(max.print = 2000)
df0919[as.character(c(comb_diff_0919[1]))]

tx3_df_filtered %>%
  filter(id == comb_diff_0919[1] & date == '20240919')

combined_df_filtered %>%
  filter(id == comb_diff_0919[1] & date == '20240919')


combined_df %>%
  filter(id == comb_diff[6] & date == '20240918') %>%
  select(gap) %>%
  colSums()/2


print(df0918_6_3[as.character(c(setdiff(MET_only_0918, tx3_df_only_0918)[178]))])
combined_df_filtered %>%
  filter(id == comb_diff[1] & date == '20240918')


sum(df0918_6_3[as.character(c(setdiff(MET_only_0918, tx3_df_only_0918)[178]))]$'5600604'$gap)





tx3_df %>%
  filter(id == setdiff(MET_only_0918, tx3_df_only_0918)[178] & date == '20240918')
