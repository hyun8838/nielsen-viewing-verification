test <- df0919_5

df0919_5$'2405782'

test$'2405782'

library(dplyr)

# df0919_6_3 데이터 예시에서 `act` 열에 'member X has entered' 또는 'member X has exited'로 필터링
df <- test$'2405782'
df$act
# member 관련 행만 필터링 (입장/퇴장)
member_acts <- df %>%
  filter(grepl("member", act))

# 행의 인덱스를 구하기 위해서 필터링한 데이터에서 member X의 entered와 exited 위치 찾기
get_member_ranges <- function(member_acts, member_id) {
  # 해당 멤버의 입장/퇴장 행 추출
  entered_rows <- which(grepl(paste0("member ", member_id, " has entered"), member_acts$act))
  exited_rows <- which(grepl(paste0("member ", member_id, " has exited"), member_acts$act))
  
  # 각 entered와 exited 사이의 행들을 포함하여 반환
  ranges <- list()
  
  for (i in 1:length(entered_rows)) {
    # 첫 번째 입장 후 해당 멤버의 퇴장까지
    entered_idx <- entered_rows[i]
    if (i <= length(exited_rows)) {
      exited_idx <- exited_rows[i]
      ranges[[i]] <- seq(entered_idx, exited_idx)
    }
  }
  
  # member X의 모든 입장/퇴장 쌍을 반환
  return(ranges)
}

# member 1과 member 2에 대해 각각 범위 찾기
member_1_ranges <- get_member_ranges(df, 1)
member_2_ranges <- get_member_ranges(df, 2)

# member 1과 member 2의 입장/퇴장 범위 병합
all_ranges <- c(member_1_ranges, member_2_ranges)

# 범위 내의 행은 남기고 나머지 행은 삭제
all_indices <- unlist(all_ranges)

# 최종적으로 삭제할 행은 범위 내에 포함되지 않는 행
final_df <- df %>%
  filter(row_number() %in% all_indices)

# 결과 확인
head(final_df)
final_df
test_list$'2405782'
nrow(final_df)
nrow(test_list$'2405782')

anti_join(final_df, test_list$'2405782', by = c("start"))
