##################################################################
###################### 전체 데이터 vs TX3 갯수 차이: 윤진 ######################
##################################################################

###################### 갯수 차이 ######################
# 날짜별 행 개수 계산
colnames(tx3_df) <- c("id", "ariana_code", "start", "end", "date")
combined_counts <- as.data.frame(table(combined_df_filtered$date))
tx3_counts <- as.data.frame(table(tx3_df_filtered$date))

# 데이터프레임 병합 후 일치 여부 확인
merged_data <- merge(combined_df_filtered, tx3_df_filtered, 
                     by = c("id", "ariana_code", "start", "end", "date"), all = FALSE)
matching_counts <- as.data.frame(table(merged_data$date))

# combined_df_filtered에만 있는 행 개수 계산
combined_only <- anti_join(combined_df_filtered, tx3_df_filtered, by = c("id", "ariana_code", "start", "end", "date"))
combined_only_counts <- as.data.frame(table(combined_only$date))
colnames(combined_only_counts) <- c("Date", "DfOnly")

# tx3_df_filtered에만 있는 행 개수 계산
tx3_only <- anti_join(tx3_df_filtered, combined_df_filtered, by = c("id", "ariana_code", "start", "end", "date"))
tx3_only_counts <- as.data.frame(table(tx3_only$date))
colnames(tx3_only_counts) <- c("Date", "TX3Only")

# 날짜별로 id, ariana_code, start, date는 같지만 end가 다른 행 개수 계산
end_mismatch <- combined_df_filtered %>%
  inner_join(tx3_df_filtered, by = c("id", "ariana_code", "start", "date")) %>%
  filter(end.x != end.y) %>%
  group_by(date) %>%
  summarise(EndMismatched = n()) %>%
  rename(Date = date)

# 날짜별로 id, ariana_code, end, date는 같지만 start가 다른 행 개수 계산
start_mismatch <- combined_df_filtered %>%
  inner_join(tx3_df_filtered, by = c("id", "ariana_code", "end", "date")) %>%
  filter(start.x != start.y) %>%
  group_by(date) %>%
  summarise(StartMismatched = n()) %>%
  rename(Date = date)

# 컬럼 이름 설정
colnames(combined_counts) <- c("Date", "DfRows")
colnames(tx3_counts) <- c("Date", "TX3Rows")
colnames(matching_counts) <- c("Date", "MatchingRows")

# 모든 데이터를 병합
final_counts <- merge(combined_counts, tx3_counts, by = "Date", all = TRUE)
final_counts <- merge(final_counts, matching_counts, by = "Date", all = TRUE)
final_counts <- merge(final_counts, combined_only_counts, by = "Date", all = TRUE)
final_counts <- merge(final_counts, tx3_only_counts, by = "Date", all = TRUE)
final_counts <- merge(final_counts, end_mismatch, by = "Date", all = TRUE)
final_counts <- merge(final_counts, start_mismatch, by = "Date", all = TRUE)
final_counts[is.na(final_counts)] <- 0  # NA 값을 0으로 대체

# 불일치 행 계산
final_counts <- final_counts %>%
  mutate(Mismatched = DfRows + TX3Rows - 2 * MatchingRows)

# 결과 출력
print(final_counts)

##################################################################
###################### 편집 규칙 별 갯수 차이: 현일 ######################
##################################################################
path = "C:/Users/legen/Desktop/Lab Project/닐슨/데이터/RData/"
load(paste0(path, "1.MET.RData"))
path = "C:/Users/legen/Desktop/Lab Project/닐슨/데이터/RData/"
load(paste0(path, "2.Constant_Viewing_1_&_2.RData"))
path = "C:/Users/legen/Desktop/Lab Project/닐슨/데이터/RData/"
load(paste0(path, "3.Combined_df.RData"))


###################### 전처리 -> 편집규칙 2번 ######################
# 날짜 범위 설정
dates <- sprintf("%04d", 915:922)

# 최종 결과 저장용 데이터프레임
cv_1_diff <- data.frame(date = character(), id = character(), count_9 = integer(), count_9_1 = integer(), diff = integer())

# 각 날짜별로 비교
for (date in dates) {
  # df_7과 df_9_1 객체 이름 생성
  df_9_name <- paste0("df", date, "_9")
  df_9_1_name <- paste0("df", date, "_9_1")
  
  # 데이터프레임 존재 여부 확인
  if (!exists(df_9_name) || !exists(df_9_1_name)) {
    cat(paste0("Skipping ", date, ": Data not found\n"))
    next
  }
  
  # 데이터 로드
  df_9 <- get(df_9_name)
  df_9_1 <- get(df_9_1_name)
  
  # 데이터프레임 생성
  diff_for_date <- data.frame(id = character(), count_9 = integer(), count_9_1 = integer(), diff = integer())
  
  for (id in names(df_9)) {
    # 데이터 갯수 계산
    count_9 <- nrow(df_9[[id]])
    count_9_1 <- if (id %in% names(df_9_1)) nrow(df_9_1[[id]]) else 0
    
    # 결과 저장
    diff_for_date <- rbind(diff_for_date, data.frame(id = id, count_9 = count_9, count_9_1 = count_9_1, diff = count_9 - count_9_1))
  }
  
  # 날짜 추가
  diff_for_date$date <- date
  
  # 최종 결과에 병합
  cv_1_diff <- rbind(cv_1_diff, diff_for_date)
}

# 날짜별 요약 통계 계산
cv_1_diff_summary <- cv_1_diff %>%
  group_by(date) %>%
  summarise(
    count_9 = sum(count_9, na.rm = TRUE),
    count_9_1 = sum(count_9_1, na.rm = TRUE),
    diff = sum(diff, na.rm = TRUE)
  )

# 최종 결과 출력
#print(cv_1_diff_summary)




###################### 편집규칙 2번 -> 편집규칙 3번 ######################
library(dplyr)

# 날짜 목록
dates <- c("0915", "0916", "0917", "0918", "0919", "0920", "0921", "0922")

# 전체 결과를 저장할 리스트
cv_2_results <- list(mismatched_9_1 = data.frame(), mismatched_9_2 = data.frame(), unmatched_ids = list())

# 모든 날짜 처리
for (date in dates) {
  # 데이터프레임 리스트 이름 동적 생성
  df_9_1_list <- get(paste0("df", date, "_9_1"))
  df_9_2_list <- get(paste0("df", date, "_9_2"))
  
  # ID 추출
  ids_9_1 <- names(df_9_1_list)
  ids_9_2 <- names(df_9_2_list)
  
  # 공통 ID 확인
  common_ids <- intersect(ids_9_1, ids_9_2)
  
  # 일치하지 않는 ID 저장
  unmatched_9_1 <- setdiff(ids_9_1, ids_9_2)
  unmatched_9_2 <- setdiff(ids_9_2, ids_9_1)
  cv_2_results$unmatched_ids[[paste0("df", date, "_9_1")]] <- unmatched_9_1
  cv_2_results$unmatched_ids[[paste0("df", date, "_9_2")]] <- unmatched_9_2
  
  # 공통 ID에 대해 비교 수행
  for (id in common_ids) {
    df_9_1 <- df_9_1_list[[id]]
    df_9_2 <- df_9_2_list[[id]]
    
    # 6_2에서 6_3과 start, end 값이 일치하지 않는 데이터 찾기
    mismatched_9_1 <- df_9_1 %>%
      anti_join(df_9_2, by = c("start", "end")) %>%
      mutate(
        date = paste0("2024", date),
        start = gsub(":", "", start),
        end = gsub(":", "", end),
        date = substr(date, 5, 12)  # '2024' 제거
      ) %>%
      select(id, ariana_code, start, end, date)
    
    # 6_3에서 6_2와 start, end 값이 일치하지 않는 데이터 찾기
    mismatched_9_2 <- df_9_2 %>%
      anti_join(df_9_1, by = c("start", "end")) %>%
      mutate(
        date = paste0("2024", date),
        start = gsub(":", "", start),
        end = gsub(":", "", end),
        date = substr(date, 5, 12)  # '2024' 제거
      ) %>%
      select(id, ariana_code, start, end, date)
    
    # 결과 병합
    if (nrow(mismatched_9_1) > 0) {
      cv_2_results$mismatched_9_1 <- bind_rows(cv_2_results$mismatched_9_1, mismatched_9_1)
    }
    
    if (nrow(mismatched_9_2) > 0) {
      cv_2_results$mismatched_9_2 <- bind_rows(cv_2_results$mismatched_9_2, mismatched_9_2)
    }
  }
}

# 결과 출력
cat("Mismatched data in 6_2 across all dates:\n")
print(cv_2_results$mismatched_9_1)

cat("\nMismatched data in 6_3 across all dates:\n")
print(cv_2_results$mismatched_9_2)

cat("\nUnmatched IDs across all dates:\n")
print(cv_2_results$unmatched_ids)


# 날짜별 수정된 갯수
cv_2_results$mismatched_9_1 %>%
  group_by(date) %>%
  summarise(row_count = n())




###################### 총 갯수 차이 ######################
# 날짜 범위 설정
dates <- sprintf("%04d", 915:922)

# 최종 결과 저장용 데이터프레임
cv_2_del <- data.frame(date = character(), id = character(), count_9_1 = integer(), count_9_2 = integer(), diff = integer())

# 각 날짜별로 비교
for (date in dates) {
  # df_9_1와 df_9_2 객체 이름 생성
  df_9_1_name <- paste0("df", date, "_9_1")
  df_9_2_name <- paste0("df", date, "_9_2")
  
  # 데이터프레임 존재 여부 확인
  if (!exists(df_9_1_name) || !exists(df_9_2_name)) {
    cat(paste0("Skipping ", date, ": Data not found\n"))
    next
  }
  
  # 데이터 로드
  df_9_1 <- get(df_9_1_name)
  df_9_2 <- get(df_9_2_name)
  
  # 데이터프레임 생성
  diff_for_date <- data.frame(id = character(), count_9_1 = integer(), count_9_2 = integer(), diff = integer())
  
  for (id in names(df_9_1)) {
    # 데이터 갯수 계산
    count_9_1 <- nrow(df_9_1[[id]])
    count_9_2 <- if (id %in% names(df_9_2)) nrow(df_9_2[[id]]) else 0
    
    # 결과 저장
    diff_for_date <- rbind(diff_for_date, data.frame(id = id, count_9_1 = count_9_1, count_9_2 = count_9_2, diff = count_9_1 - count_9_2))
  }
  
  # 날짜 추가
  diff_for_date$date <- date
  
  # 최종 결과에 병합
  cv_2_del <- rbind(cv_2_del, diff_for_date)
}

# 날짜별 요약 통계 계산
cv_2_del_summary <- cv_2_del %>%
  group_by(date) %>%
  summarise(
    count_9_1 = sum(count_9_1, na.rm = TRUE),
    count_9_2 = sum(count_9_2, na.rm = TRUE),
    diff = sum(diff, na.rm = TRUE)
  )

# 결과 확인
print(cv_2_del_summary)
cv_2_diff_summary <- cv_2_results$mismatched_9_1 %>%
  group_by(date) %>%
  summarise(row_count = n())




###################### 출력 ######################
# 결과 출력
print("6과 6_2 간의 차이:")
print(cv_1_diff_summary)


print("6_2과 6_3 간의 차이:")
print(cv_2_diff_summary)


###################### 특정 ID의 차이가 나는 데이터 출력 함수 ######################
library(dplyr)

# 특정 ID의 차이가 나는 데이터 출력 함수
print_diff_for_id <- function(target_id, df_9, df_9_1) {
  if (!(target_id %in% names(df_9))) {
    stop(paste("ID", target_id, "is not found in df0915_9"))
  }
  if (!(target_id %in% names(df_9_1))) {
    stop(paste("ID", target_id, "is not found in df0915_9_1"))
  }
  
  # 해당 ID의 데이터 추출
  data_9 <- df_9[[target_id]]
  data_9_1 <- df_9_1[[target_id]]
  
  # df0915_9에만 존재하는 행
  only_in_9 <- anti_join(data_9, data_9_1, by = c("id", "act", "chn", "date", "ariana_code", "start", "end"))
  
  # df0915_9_1에만 존재하는 행
  only_in_9_1 <- anti_join(data_9_1, data_9, by = c("id", "act", "chn", "date", "ariana_code", "start", "end"))
  
  # 결과 출력
  cat("\nRows only in df0915_9:\n")
  print(only_in_9)
  
  cat("\nRows only in df0915_9_1:\n")
  print(only_in_9_1)
}

# 사용 예시 (ID를 입력)
# print_diff_for_id("1403878", df0915_9, df0915_9_1)

###################### Constant Viewing으로 걸러진 데이터 예시 ######################

# 걸러지는 데이터 ID 확인
first_diff %>%
  arrange(desc(diff)) %>%
  head()

second_diff %>%
  arrange(desc(diff)) %>%
  head()


# 사용 예시 (ID를 입력)
# 전처리 -> constant viewing 1
print_diff_for_id("2406141", df0915_7, df0915_9_1)


# constant viewing 1 -> constant viewing 2
print_diff_for_id("1403878", df0915_9_1, df0915_9_2)




df0915_9_2$'1402738'
# 결과 출력
print(filtered_data)


combined_df_filtered[combined_df_filtered$id == '6600297' & combined_df_filtered$date == '20240919', ]



