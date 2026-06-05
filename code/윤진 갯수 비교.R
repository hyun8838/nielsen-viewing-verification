# 날짜별로 tx3_df_filtered에 없는 id를 combined_df_filtered에서 제외

# 날짜별로 tx3_df_filtered의 id 집합 생성
tx3_ids_by_date <- tx3_df_filtered %>%
  select(date, id) %>%
  distinct()

# combined_df_filtered에서 tx3_df_filtered의 id만 포함하도록 필터링
filtered_combined_df <- combined_df_filtered %>%
  inner_join(tx3_ids_by_date, by = c("date", "id"))

# 날짜별 행 개수 계산
combined_counts <- as.data.frame(table(filtered_combined_df$date))
tx3_counts <- as.data.frame(table(tx3_df_filtered$date))

# 데이터프레임 병합 후 일치 여부 확인
merged_data <- merge(filtered_combined_df, tx3_df_filtered, 
                     by = c("id", "ariana_code", "start", "end", "date"), all = FALSE)
matching_counts <- as.data.frame(table(merged_data$date))

# combined_df_filtered에만 있는 행 개수 계산
combined_only <- anti_join(filtered_combined_df, tx3_df_filtered, by = c("id", "ariana_code", "start", "end", "date"))
combined_only_counts <- as.data.frame(table(combined_only$date))
colnames(combined_only_counts) <- c("Date", "DfOnlyRows")

# tx3_df_filtered에만 있는 행 개수 계산
tx3_only <- anti_join(tx3_df_filtered, filtered_combined_df, by = c("id", "ariana_code", "start", "end", "date"))
tx3_only_counts <- as.data.frame(table(tx3_only$date))
colnames(tx3_only_counts) <- c("Date", "TX3OnlyRows")

# 컬럼 이름 설정
colnames(combined_counts) <- c("Date", "DfRows")
colnames(tx3_counts) <- c("Date", "TX3Rows")
colnames(matching_counts) <- c("Date", "MatchingRows")

# 모든 데이터를 병합
final_counts <- merge(combined_counts, tx3_counts, by = "Date", all = TRUE)
final_counts <- merge(final_counts, matching_counts, by = "Date", all = TRUE)
final_counts <- merge(final_counts, combined_only_counts, by = "Date", all = TRUE)
final_counts <- merge(final_counts, tx3_only_counts, by = "Date", all = TRUE)
final_counts[is.na(final_counts)] <- 0  # NA 값을 0으로 대체

# 불일치 행 계산
final_counts <- final_counts %>%
  mutate(MismatchedRows = DfRows + TX3Rows - 2 * MatchingRows)

# 결과 출력
print(final_counts)
