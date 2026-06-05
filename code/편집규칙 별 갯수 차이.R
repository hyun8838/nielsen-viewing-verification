head(df0915_6$'1402738')
names(df0915_6)
head(df0915_6_2$'1402738')

head(df0915_6_3$'1402738')




########################## 전처리 -> 편집규칙 2번 ##########################
# 날짜 범위 설정
dates <- sprintf("%04d", 915:922)

# 최종 결과 저장용 데이터프레임
cv_1_diff <- data.frame(date = character(), id = character(), count_6 = integer(), count_6_2 = integer(), diff = integer())

# 각 날짜별로 비교
for (date in dates) {
  # df_7과 df_6_2 객체 이름 생성
  df_7_name <- paste0("df", date, "_7")
  df_6_2_name <- paste0("df", date, "_6_2")
  
  # 데이터프레임 존재 여부 확인
  if (!exists(df_7_name) || !exists(df_6_2_name)) {
    cat(paste0("Skipping ", date, ": Data not found\n"))
    next
  }
  
  # 데이터 로드
  df_7 <- get(df_7_name)
  df_6_2 <- get(df_6_2_name)
  
  # 데이터프레임 생성
  diff_for_date <- data.frame(id = character(), count_6 = integer(), count_6_2 = integer(), diff = integer())
  
  for (id in names(df_7)) {
    # 데이터 갯수 계산
    count_6 <- nrow(df_7[[id]])
    count_6_2 <- if (id %in% names(df_6_2)) nrow(df_6_2[[id]]) else 0
    
    # 결과 저장
    diff_for_date <- rbind(diff_for_date, data.frame(id = id, count_6 = count_6, count_6_2 = count_6_2, diff = count_6 - count_6_2))
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
    count_6 = sum(count_6, na.rm = TRUE),
    count_6_2 = sum(count_6_2, na.rm = TRUE),
    diff = sum(diff, na.rm = TRUE)
  )

# 최종 결과 출력
#print(cv_1_diff_summary)




########################## 편집규칙 2번 -> 편집규칙 3번 ##########################
library(dplyr)

# 날짜 목록
dates <- c("0915", "0916", "0917", "0918", "0919", "0920", "0921", "0922")

# 전체 결과를 저장할 리스트
cv_2_results <- list(mismatched_6_2 = data.frame(), mismatched_6_3 = data.frame(), unmatched_ids = list())

# 모든 날짜 처리
for (date in dates) {
  # 데이터프레임 리스트 이름 동적 생성
  df_6_2_list <- get(paste0("df", date, "_6_2"))
  df_6_3_list <- get(paste0("df", date, "_6_3"))
  
  # ID 추출
  ids_6_2 <- names(df_6_2_list)
  ids_6_3 <- names(df_6_3_list)
  
  # 공통 ID 확인
  common_ids <- intersect(ids_6_2, ids_6_3)
  
  # 일치하지 않는 ID 저장
  unmatched_6_2 <- setdiff(ids_6_2, ids_6_3)
  unmatched_6_3 <- setdiff(ids_6_3, ids_6_2)
  cv_2_results$unmatched_ids[[paste0("df", date, "_6_2")]] <- unmatched_6_2
  cv_2_results$unmatched_ids[[paste0("df", date, "_6_3")]] <- unmatched_6_3
  
  # 공통 ID에 대해 비교 수행
  for (id in common_ids) {
    df_6_2 <- df_6_2_list[[id]]
    df_6_3 <- df_6_3_list[[id]]
    
    # 6_2에서 6_3과 start, end 값이 일치하지 않는 데이터 찾기
    mismatched_6_2 <- df_6_2 %>%
      anti_join(df_6_3, by = c("start", "end")) %>%
      mutate(
        date = paste0("2024", date),
        start = gsub(":", "", start),
        end = gsub(":", "", end),
        date = substr(date, 5, 12)  # '2024' 제거
      ) %>%
      select(id, ariana_code, start, end, date)
    
    # 6_3에서 6_2와 start, end 값이 일치하지 않는 데이터 찾기
    mismatched_6_3 <- df_6_3 %>%
      anti_join(df_6_2, by = c("start", "end")) %>%
      mutate(
        date = paste0("2024", date),
        start = gsub(":", "", start),
        end = gsub(":", "", end),
        date = substr(date, 5, 12)  # '2024' 제거
      ) %>%
      select(id, ariana_code, start, end, date)
    
    # 결과 병합
    if (nrow(mismatched_6_2) > 0) {
      cv_2_results$mismatched_6_2 <- bind_rows(cv_2_results$mismatched_6_2, mismatched_6_2)
    }
    
    if (nrow(mismatched_6_3) > 0) {
      cv_2_results$mismatched_6_3 <- bind_rows(cv_2_results$mismatched_6_3, mismatched_6_3)
    }
  }
}

# 결과 출력
cat("Mismatched data in 6_2 across all dates:\n")
print(cv_2_results$mismatched_6_2)

cat("\nMismatched data in 6_3 across all dates:\n")
print(cv_2_results$mismatched_6_3)

cat("\nUnmatched IDs across all dates:\n")
print(cv_2_results$unmatched_ids)


# 날짜별 수정된 갯수
cv_2_results$mismatched_6_2 %>%
  group_by(date) %>%
  summarise(row_count = n())




################# 총 갯수 차이 ################
# 날짜 범위 설정
dates <- sprintf("%04d", 915:922)

# 최종 결과 저장용 데이터프레임
cv_2_del <- data.frame(date = character(), id = character(), count_6_2 = integer(), count_6_3 = integer(), diff = integer())

# 각 날짜별로 비교
for (date in dates) {
  # df_6_2와 df_6_3 객체 이름 생성
  df_6_2_name <- paste0("df", date, "_6_2")
  df_6_3_name <- paste0("df", date, "_6_3")
  
  # 데이터프레임 존재 여부 확인
  if (!exists(df_6_2_name) || !exists(df_6_3_name)) {
    cat(paste0("Skipping ", date, ": Data not found\n"))
    next
  }
  
  # 데이터 로드
  df_6_2 <- get(df_6_2_name)
  df_6_3 <- get(df_6_3_name)
  
  # 데이터프레임 생성
  diff_for_date <- data.frame(id = character(), count_6_2 = integer(), count_6_3 = integer(), diff = integer())
  
  for (id in names(df_6_2)) {
    # 데이터 갯수 계산
    count_6_2 <- nrow(df_6_2[[id]])
    count_6_3 <- if (id %in% names(df_6_3)) nrow(df_6_3[[id]]) else 0
    
    # 결과 저장
    diff_for_date <- rbind(diff_for_date, data.frame(id = id, count_6_2 = count_6_2, count_6_3 = count_6_3, diff = count_6_2 - count_6_3))
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
    count_6_2 = sum(count_6_2, na.rm = TRUE),
    count_6_3 = sum(count_6_3, na.rm = TRUE),
    diff = sum(diff, na.rm = TRUE)
  )

# 결과 확인
print(cv_2_del_summary)




########################## 출력 ##########################
# 결과 출력
print("6과 6_2 간의 차이:")
print(cv_1_diff_summary)


print("6_2과 6_3 간의 차이:")
print(cv_2_results)
nrow(cv_2_results$mismatched_6_2)

cv_2_results$mismatched_6_2 %>%
  group_by(date) %>%
  summarise(row_count = n())

########################## 특정 ID의 차이가 나는 데이터 출력 함수 ##########################
library(dplyr)

# 특정 ID의 차이가 나는 데이터 출력 함수
print_diff_for_id <- function(target_id, df_6, df_6_2) {
  if (!(target_id %in% names(df_6))) {
    stop(paste("ID", target_id, "is not found in df0915_6"))
  }
  if (!(target_id %in% names(df_6_2))) {
    stop(paste("ID", target_id, "is not found in df0915_6_2"))
  }
  
  # 해당 ID의 데이터 추출
  data_6 <- df_6[[target_id]]
  data_6_2 <- df_6_2[[target_id]]
  
  # df0915_6에만 존재하는 행
  only_in_6 <- anti_join(data_6, data_6_2, by = c("id", "act", "chn", "date", "ariana_code", "start", "end"))
  
  # df0915_6_2에만 존재하는 행
  only_in_6_2 <- anti_join(data_6_2, data_6, by = c("id", "act", "chn", "date", "ariana_code", "start", "end"))
  
  # 결과 출력
  cat("\nRows only in df0915_6:\n")
  print(only_in_6)
  
  cat("\nRows only in df0915_6_2:\n")
  print(only_in_6_2)
}

# 사용 예시 (ID를 입력)
# print_diff_for_id("1403878", df0915_6, df0915_6_2)

########################## Constant Viewing으로 걸러진 데이터 예시 ##########################

# 걸러지는 데이터 ID 확인
first_diff %>%
  arrange(desc(diff)) %>%
  head()

second_diff %>%
  arrange(desc(diff)) %>%
  head()


# 사용 예시 (ID를 입력)
# 전처리 -> constant viewing 1
print_diff_for_id("2406141", df0915_7, df0915_6_2)


# constant viewing 1 -> constant viewing 2
print_diff_for_id("1403878", df0915_6_2, df0915_6_3)




df0915_6_3$'1402738'
# 결과 출력
print(filtered_data)


combined_df_filtered[combined_df_filtered$id == '6600297' & combined_df_filtered$date == '20240919', ]



cv_1_diff %>%
  filter(diff > 0)


df0919_6$"1404187" %>%
  select('id', 'ariana_code', 'start', 'end', 'date')
df0919_6_2$"1404187" %>%
  select('id', 'ariana_code', 'start', 'end', 'date')
