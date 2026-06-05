tx3_df %>% filter(ch_no != "V65535")
combined_df
nrow(tx3_df %>% filter(ch_no != "V65535"))
colnames(tx3_df %>% filter(ch_no != "V65535"))
nrow(combined_df)
colnames(combined_df)

# combined_test <- combined_df_filtered
########################### 데이터 갯수 차이 비교 ###########################
# 같은 date와 같은 id의 데이터 갯수 차이 계산
data_count_diff <- tx3_df_filtered %>%
  group_by(id, date) %>%
  summarise(tx3_count = n(), .groups = "drop") %>%
  full_join(
    combined_df_filtered %>%
      group_by(id, date) %>%
      summarise(combined_count = n(), .groups = "drop"),
    by = c("id", "date")
  ) %>%
  mutate(count_diff = coalesce(tx3_count, 0) - coalesce(combined_count, 0))

# 결과 출력
# 전체
print(data_count_diff)
tail(as.data.frame(data_count_diff)$count_diff)
#### test #####
data_count_diff_test <- tx3_df_filtered %>%
  group_by(id, date) %>%
  summarise(tx3_count = n(), .groups = "drop") %>%
  full_join(
    combined_test %>%
      group_by(id, date) %>%
      summarise(combined_count = n(), .groups = "drop"),
    by = c("id", "date")
  ) %>%
  mutate(count_diff = coalesce(tx3_count, 0) - coalesce(combined_count, 0))


sum(abs(data_count_diff$count_diff))
sum(abs(data_count_diff_test$count_diff))

as.data.frame(data_count_diff) %>%
  arrange(desc(count_diff)) %>%
  filter(!is.na(tx3_count) & !is.na(combined_count))


# ID 지정
unique(data_count_diff$id)
data_count_diff[data_count_diff$id == '4401038',]



sum(abs(data_count_diff$count_diff))
nrow(tx3_df_filtered) - nrow(combined_df_filtered)




########################### ID 갯수 차이 비교 ###########################
# 공통 ID, tx3_df에만 있는 ID, combined_df에만 있는 ID 분석
common_ids <- intersect(tx3_df_filtered$id, combined_df_filtered$id)
tx3_only_ids <- setdiff(tx3_df_filtered$id, combined_df_filtered$id)
combined_only_ids <- setdiff(combined_df_filtered$id, tx3_df_filtered$id)

# ID 리스트 저장
common_count_id_list <- common_ids
tx3_only_count_id_list <- tx3_only_ids
combined_only_count_id_list <- combined_only_ids

# 각각의 갯수 계산
common_count <- length(common_ids)
tx3_only_count <- length(tx3_only_ids)
combined_only_count <- length(combined_only_ids)

# 결과 출력
cat("공통 ID 갯수:", common_count, "\n")
cat("tx3_df에만 있는 ID 갯수:", tx3_only_count, "\n")
cat("combined_df에만 있는 ID 갯수:", combined_only_count, "\n")

# 리스트 결과 확인
cat("공통 ID 리스트:", toString(common_count_id_list), "\n")
cat("tx3_df에만 있는 ID 리스트:", toString(tx3_only_count_id_list), "\n")
cat("combined_df에만 있는 ID 리스트:", toString(combined_only_count_id_list), "\n")

########################### ID 확인 ###########################
# 빈 데이터인지 확인

test <- tx3_df_filtered %>%
  filter(id %in% tx3_only_count_id_list) %>%
  group_by(id) %>%
  summarise(row_count = n())
as.data.frame(test)


tx3_df_filtered[tx3_df_filtered$id == '7600827' & tx3_df_filtered$date == '20240921',] %>%
  arrange(start)
combined_df_filtered[combined_df_filtered$id == '7600827' & combined_df_filtered$date == '20240921',]

df0918_6_3$'1403100'
df0918_3$'1403100'

# combined_df_filtered[combined_df_filtered$id == '1403100' & combined_df_filtered$date == '20240918',]

##### 행 차이 #####
# (3) TX3에 있어야 할 기록
as.data.frame(data_count_diff) %>%
  arrange(count_diff) %>%
  filter(!is.na(tx3_count) & !is.na(combined_count))
head(combined_df_filtered)

tx3_filtered <- tx3_df_filtered %>%
  filter(id == '4401038' & date == '20240919') %>%
  arrange(start)

combined_filtered <- combined_df_filtered %>%
  filter(id == '4401038' & date == '20240919')

# start와 end를 기준으로 두 데이터에서 서로 다른 행 찾기
tx3_only <- anti_join(tx3_filtered, combined_filtered, by = c("start", "end"))
combined_only <- anti_join(combined_filtered, tx3_filtered, by = c("start", "end"))

# 결과 출력
tx3_only
combined_only





# (4) TX3에서 없어져야 할 기록
as.data.frame(data_count_diff) %>%
  arrange(desc(count_diff)) %>%
  filter(!is.na(tx3_count) & !is.na(combined_count))


##### ID (가구) 차이 #####
test_2 <- combined_df_filtered %>%
  filter(id %in% tx3_only_count_id_list) %>%
  group_by(id) %>%
  summarise(row_count = n())
as.data.frame(test)
