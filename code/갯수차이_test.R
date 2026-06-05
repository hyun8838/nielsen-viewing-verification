library(dplyr)

# 결과를 저장할 리스트 생성
comparison_results <- list(mismatched_6_2 = data.frame(), mismatched_6_3 = data.frame(), unmatched_ids = list())

# ID 추출
ids_6_2 <- names(df0916_6_2)
ids_6_3 <- names(df0916_6_3)

# 공통 ID 확인
common_ids <- intersect(ids_6_2, ids_6_3)

# 일치하지 않는 ID 저장
unmatched_6_2 <- setdiff(ids_6_2, ids_6_3)
unmatched_6_3 <- setdiff(ids_6_3, ids_6_2)
comparison_results$unmatched_ids$df0916_6_2 <- unmatched_6_2
comparison_results$unmatched_ids$df0916_6_3 <- unmatched_6_3

# 공통 ID에 대해 비교 수행
for (id in common_ids) {
  df_6_2 <- df0916_6_2[[id]]
  df_6_3 <- df0916_6_3[[id]]
  
  # 6_2에서 6_3과 start, end 값이 일치하지 않는 데이터 찾기
  mismatched_6_2 <- df_6_2 %>%
    anti_join(df_6_3, by = c("start", "end")) %>%
    mutate(source = "6_2")
  
  # 6_3에서 6_2와 start, end 값이 일치하지 않는 데이터 찾기
  mismatched_6_3 <- df_6_3 %>%
    anti_join(df_6_2, by = c("start", "end")) %>%
    mutate(source = "6_3")
  
  # 결과 병합
  if (nrow(mismatched_6_2) > 0) {
    comparison_results$mismatched_6_2 <- bind_rows(comparison_results$mismatched_6_2, mismatched_6_2)
  }
  
  if (nrow(mismatched_6_3) > 0) {
    comparison_results$mismatched_6_3 <- bind_rows(comparison_results$mismatched_6_3, mismatched_6_3)
  }
}

# 결과 출력
cat("Mismatched data in 6_2:\n")
print(comparison_results$mismatched_6_2)

cat("\nMismatched data in 6_3:\n")
print(comparison_results$mismatched_6_3)

cat("\nUnmatched IDs:\n")
print(comparison_results$unmatched_ids)





################################################################################
################################################################################
################################################################################
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
names(cv_2_results$unmatched_ids)

# 날짜별 수정된 갯수
cv_2_results$mismatched_6_2 %>%
  group_by(date) %>%
  summarise(row_count = n())



################################################################################
################################################################################
################################################################################
library(dplyr)

# 새로운 데이터프레임을 생성하여 저장할 리스트
unmatched_ids_df <- data.frame()

# 날짜 목록
dates <- c("0915", "0916", "0917", "0918", "0919", "0920", "0921", "0922")

# unmatched_ids 리스트에서 데이터 생성
for (date in dates) {
  # df날짜_6_2에서 ID 추출
  df_6_2_ids <- cv_2_results$unmatched_ids[[paste0("df", date, "_6_2")]]
  
  # ID와 날짜 정보를 결합
  unmatched_6_2_df <- data.frame(
    id = df_6_2_ids,
    date = paste0("2024", date)
  )
  
  # 새로운 데이터프레임을 기존 데이터프레임에 병합
  unmatched_ids_df <- bind_rows(unmatched_ids_df, unmatched_6_2_df)
}
# 결과 출력
unmatched_ids_df



# 날짜별 수정된 갯수
unmatched_ids_df %>%
  group_by(date) %>%
  summarise(row_count = n())


################################################################################
################################################################################
################################################################################
names(cv_2_results$unmatched_ids)
cv_2_results$unmatched_ids$df0915_6_2[1]
df0915_6_2$"1403176"

# 빈 데이터프레임과 비어 있지 않은 데이터프레임을 저장할 변수
empty_ids <- list()
non_empty_ids <- list()

# 날짜 목록
dates <- c("0915", "0916", "0917", "0918", "0919", "0920", "0921", "0922")

# 모든 날짜에 대해 처리
for (date in dates) {
  # unmatched_ids에서 해당 날짜의 df_6_2의 ID들 추출
  df_6_2_ids <- cv_2_results$unmatched_ids[[paste0("df", date, "_6_2")]]
  
  # 빈 데이터프레임과 비어 있지 않은 데이터프레임을 나누기
  for (id in df_6_2_ids) {
    # df날짜_6_2에서 해당 ID의 데이터프레임을 추출
    df_6_2_df <- get(paste0("df", date, "_6_2"))[[id]]
    
    # 행 수가 0인지 아닌지 체크
    if (nrow(df_6_2_df) == 0) {
      empty_ids[[paste0(date, "_", id)]] <- id  # 빈 데이터프레임의 ID 저장
    } else {
      non_empty_ids[[paste0(date, "_", id)]] <- id  # 비어 있지 않은 데이터프레임의 ID 저장
    }
  }
}

# 결과 출력
cat("빈 데이터프레임의 ID 목록:\n")
print(empty_ids)

cat("\n비어 있지 않은 데이터프레임의 ID 목록:\n")
print(non_empty_ids)


