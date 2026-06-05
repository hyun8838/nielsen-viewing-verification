library(dplyr)
library(purrr)

combined_df_filtered_2 <- combined_df_filtered

for (date in names(merged_reject_list)) {
  reject_ids <- merged_reject_list[[date]]$id
  combined_df_filtered_2 <- combined_df_filtered_2 %>%
    filter(!(date == !!date & id %in% reject_ids))
}

# tx3_df_filtered에서 merged_reject_list의 id를 제거
tx3_df_filtered_2 <- tx3_df_filtered

for (date in names(merged_reject_list)) {
  reject_ids <- merged_reject_list[[date]]$id
  tx3_df_filtered_2 <- tx3_df_filtered_2 %>%
    filter(!(date == !!date & id %in% reject_ids))
}


combined_df_filtered_2 = combined_df_filtered_2 %>%
  distinct()
tx3_df_filtered_2 = tx3_df_filtered_2 %>%
  distinct()


# Helper 함수: 중복된 행 처리
resolve_duplicates <- function(df1, df2, by_cols) {
  df1_grouped <- df1 %>%
    group_by(across(all_of(by_cols))) %>%
    mutate(row_id = row_number()) %>%
    ungroup()
  
  df2_grouped <- df2 %>%
    group_by(across(all_of(by_cols))) %>%
    mutate(row_id = row_number()) %>%
    ungroup()
  
  inner_join(df1_grouped, df2_grouped, by = c(by_cols, "row_id"))
}

# 1. MatchingRows 계산
matching_rows <- resolve_duplicates(
  combined_df_filtered_2, tx3_df_filtered_2,
  c("id", "ariana_code", "start", "end", "date")
)

matching_counts <- matching_rows %>%
  group_by(date) %>%
  summarise(MatchingRows = n(), .groups = "drop")

# 2. StartMismatched 계산
start_mismatch_list <- combined_df_filtered_2 %>%
  anti_join(matching_rows, by = c("id", "ariana_code", "start", "end", "date")) %>%
  distinct(id, ariana_code, start, end, date) %>%
  inner_join(
    tx3_df_filtered_2 %>%
      anti_join(matching_rows, by = c("id", "ariana_code", "start", "end", "date")) %>%
      distinct(id, ariana_code, start, end, date),
    by = c("id", "ariana_code", "end", "date")
  ) %>%
  # filter(start.x != start.y) %>%
  # mutate(start = start.x) %>%
  # select(id, ariana_code, start, end, date) %>%
  group_split(date)


# 3. EndMismatched 계산
end_mismatch_list <- combined_df_filtered_2 %>%
  anti_join(matching_rows, by = c("id", "ariana_code", "start", "end", "date")) %>%
  distinct(id, ariana_code, start, end, date) %>%
  inner_join(
    tx3_df_filtered_2 %>%
      anti_join(matching_rows, by = c("id", "ariana_code", "start", "end", "date")) %>%
      distinct(id, ariana_code, start, end, date),
    by = c("id", "ariana_code", "start", "date")
  ) %>%
  # filter(end.x != end.y) %>%
  # mutate(end = end.x) %>%
  # distinct() %>%
  # select(id, ariana_code, start, end, date) %>%
  group_split(date)


# 4. ChnMismatched 계산
chn_mismatch_list <- combined_df_filtered_2 %>%
  anti_join(matching_rows, by = c("id", "ariana_code", "start", "end", "date")) %>%
  distinct(id, ariana_code, start, end, date) %>%
  inner_join(
    tx3_df_filtered_2 %>%
      anti_join(matching_rows, by = c("id", "ariana_code", "start", "end", "date")) %>%
      distinct(id, ariana_code, start, end, date),
    by = c("id", "start", "end", "date")
  ) %>%
  # filter(ariana_code.x != ariana_code.y) %>%
  # mutate(ariana_code = ariana_code.x) %>%
  # distinct() %>%
  # select(id, ariana_code, start, end, date) %>%
  group_split(date)

# 날짜별로 우리꺼랑 tx3에서 둘중 하나에서라도 리젝된 가구id 
merged_reject_list <- list()

# 날짜별로 데이터 합치기
for (date in unique(c(names(df_reject_list), names(tx3_reject_list)))) {
  # df_reject_list와 tx3_reject_list의 id 추출
  df_ids <- if (date %in% names(df_reject_list)) df_reject_list[[date]]$id else character()
  tx3_ids <- if (date %in% names(tx3_reject_list)) tx3_reject_list[[date]]$id else character()
  
  # 두 리스트의 ID 합치기 (중복 제거)
  combined_ids <- unique(c(df_ids, tx3_ids))
  
  # 날짜별 데이터프레임 생성
  merged_reject_list[[date]] <- data.frame(
    id = combined_ids,
    date = date,
    stringsAsFactors = FALSE
  )
}

merged_reject_list


###### ChnMismatched에 해당하는 id 추출 ######
print(chn_mismatch_list[[5]], n = 50)
unique(chn_mismatch_list[[8]][[2]])
chn_mismatch_list[[1]] %>%
  filter(ariana_code.x == "V295")


chn_ids <- chn_mismatch_list[[4]][[1]]
chn_start <- chn_mismatch_list[[4]][[3]]
chn_end <- chn_mismatch_list[[4]][[4]]

combined_df_filtered %>%
  filter(id == '2405411' & date == '20240918' )
chn_start[1]
tx3_df_filtered %>%
  filter(id == as.character(ids_1[1]) & date == '20240922')


as.character(chn_mismatch_list[[3]][[1]])

# 두 개의 id 벡터 생성
ids_1 <- as.character(chn_mismatch_list[[3]][[1]])
ids_2 <- as.vector(combined_df_filtered %>% filter(date == '20240917') %>% select(id) %>% mutate(id = as.character(id)))



combined_df_filtered %>%
  filter(id == as.character(ids_1[1]) & date == '20240922')

tx3_df_filtered %>%
  filter(id == as.character(ids_1[1]) & date == '20240922')


###### EndMismatched에 해당하는 id 추출 ######
print(end_mismatch_list[[8]], n = 50)
unique(end_mismatch_list[[8]][[1]])

end_mismatch_list[[8]] %>%
  filter(id == '3401499')


combined_df_filtered %>%
  filter(id == '1403755' & date == '20240922')

tx3_df_filtered %>%
  filter(id == '5401434' & date == '20240922') %>%
  arrange(start)


chn_mismatch_list[[1]] %>%
  filter(ariana_code.x == "V295")


chn_ids <- chn_mismatch_list[[4]][[1]]
chn_start <- chn_mismatch_list[[4]][[3]]
chn_end <- chn_mismatch_list[[4]][[4]]

combined_df_filtered %>%
  filter(id == '2405411' & date == '20240918' )
chn_start[1]
tx3_df_filtered %>%
  filter(id == as.character(ids_1[1]) & date == '20240922')


as.character(chn_mismatch_list[[3]][[1]])

# 두 개의 id 벡터 생성
ids_1 <- as.character(chn_mismatch_list[[3]][[1]])
ids_2 <- as.vector(combined_df_filtered %>% filter(date == '20240917') %>% select(id) %>% mutate(id = as.character(id)))



combined_df_filtered %>%
  filter(id == as.character(ids_1[1]) & date == '20240922')

tx3_df_filtered %>%
  filter(id == as.character(ids_1[1]) & date == '20240922')




######### DFOnly & TX3Only #############
# 5. DfOnly 계산
dfonly_list <- combined_df_filtered_2 %>%
  anti_join(matching_rows, by = c("id", "ariana_code", "start", "end", "date")) %>%
  anti_join(bind_rows(start_mismatch_list), by = c("id", "ariana_code", "start", "end", "date")) %>%
  anti_join(bind_rows(end_mismatch_list), by = c("id", "ariana_code", "start", "end", "date")) %>%
  anti_join(bind_rows(chn_mismatch_list), by = c("id","ariana_code", "start", "end", "date")) %>%
  group_split(date) %>%
  setNames(unique(combined_df_filtered_2$date))

# 6. TX3Only 계산
tx3only_list <- tx3_df_filtered_2 %>%
  anti_join(matching_rows, by = c("id", "ariana_code", "start", "end", "date")) %>%
  anti_join(bind_rows(start_mismatch_list), by = c("id", "ariana_code", "end", "date")) %>%
  anti_join(bind_rows(end_mismatch_list), by = c("id", "ariana_code", "start", "date")) %>%
  anti_join(bind_rows(chn_mismatch_list), by = c("id", "start", "end", "date")) %>%
  group_split(date) %>%
  setNames(unique(tx3_df_filtered_2$date))
###### TX3Only에 해당하는 id 추출 ######




unique(tx3only_list[[6]]$ariana_code)
tx3only_list[[6]] %>%
  filter(ariana_code == "V382")


combined_df_filtered %>%
  filter(id == '5401253' & date == '20240920' & ariana_code == "V16076")

tx3_df_filtered %>%
  filter(id == '2404265' & date == '20240920' & ariana_code == 'V188')


combined_df_filtered %>%
  filter(id == '3200223' & date == '20240920') 


tx3_df_filtered %>%
  filter(id == '2404265' & date == '20240920') %>%
  arrange(start)



###### DFOnly에 해당하는 id 추출 ######


unique(dfonly_list[[8]]$ariana_code)
tx3only_list[[6]] %>%
  filter(ariana_code == "V382")


combined_df_filtered %>%
  filter(id == '5401253' & date == '20240920' & ariana_code == "V16076")

tx3_df_filtered %>%
  filter(id == '2404265' & date == '20240920' & ariana_code == 'V188')


combined_df_filtered %>%
  filter(id == '3200223' & date == '20240920') 


tx3_df_filtered %>%
  filter(id == '2404265' & date == '20240920') %>%
  arrange(start)

