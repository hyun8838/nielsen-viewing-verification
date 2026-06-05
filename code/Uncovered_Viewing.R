library(dplyr)
library(lubridate)

member_enter_exit <- df0915_4$"2406214" %>%
  mutate(index = row_number()) %>%  # 원본 데이터의 인덱스를 index 열로 추가
  filter(grepl("member [1-5] has (entered|exited)", act)) %>%  # 'member 1 has entered', 'member 1 has exited', ..., 'member 5 has entered', 'member 5 has exited' 활동 필터링
  select(id, time, act, index) 

# 시간 차이 계산 (entered도 살리고 exited에 대해서만 gap을 계산)
member_enter_exit <- member_enter_exit %>%
  mutate(time = as.POSIXct(time, format="%H:%M:%S", tz="UTC")) %>%  # time 열을 POSIXct로 변환
  group_by(id) %>%
  mutate(gap = if_else(act %in% c("member 1 has exited", "member 2 has exited", "member 3 has exited", "member 4 has exited", "member 5 has exited"), 
                       as.numeric(difftime(time, lag(time), units = "secs")), NA_real_)) %>%  # exited에 대해서만 시간 차이 계산
  ungroup()  # 그룹 해제

# 시간 차이 계산 (entered도 살리고 exited에 대해서만 gap을 계산)
member_enter_exit <- member_enter_exit %>%
  mutate(time = as.POSIXct(time, format="%H:%M:%S", tz="UTC")) %>%  # time 열을 POSIXct로 변환
  group_by(id) %>%
  mutate(gap = if_else(act %in% c("member 1 has exited", "member 2 has exited", "member 3 has exited", "member 4 has exited", "member 5 has exited"), 
                       as.numeric(difftime(time, lag(time), units = "secs")), NA_real_)) %>%  # exited에 대해서만 시간 차이 계산
  ungroup() %>%
  mutate(unviewing = if_else(abs(index - lag(index)) == 1, 1, 0))  # index 값 차이가 1인 경우 unviewing을 1로 설정, 아니면 0

# total_gap 계산 (NA 제외)
total_gap <- sum(member_enter_exit$gap, na.rm = TRUE)

# unviewing_gap 계산 (unviewing이 1인 행의 gap 값 합산)
unviewing_gap <- sum(member_enter_exit$gap[member_enter_exit$unviewing == 1], na.rm = TRUE)

# unviewing_gap / total_gap 계산
unviewing_gap_ratio <- unviewing_gap / total_gap

unviewing_gap_ratio

