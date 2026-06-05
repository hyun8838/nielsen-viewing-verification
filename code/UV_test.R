# R 코드 작성
# 데이터프레임을 df로 가져옵니다 (입력 데이터 예시)
df <- df0915_1[[1]]  # 입력 데이터가 여기로 제공된다고 가정
df <- df0915_2$"1403471"

# 시간 차이를 계산하는 함수 (24시간 이상 지원)
calculate_time_diff <- function(time1, time2) {
  time1_split <- as.numeric(unlist(strsplit(time1, ":")))
  time2_split <- as.numeric(unlist(strsplit(time2, ":")))
  
  # 시간을 초 단위로 변환 (24시간 이상 지원)
  seconds1 <- time1_split[1] * 3600 + time1_split[2] * 60 + time1_split[3]
  seconds2 <- time2_split[1] * 3600 + time2_split[2] * 60 + time2_split[3]
  
  # 시간 차이 반환
  return(seconds2 - seconds1)
}


##### sum_time #####
sum_time <- 0

sum_prev_end_idx <- 1

# "Off"가 포함된 행 찾기
sum_off_rows <- which(grepl("Off", df$act, ignore.case = TRUE))

# 각 "Off" 이벤트까지의 시간 차이를 누적
for (idx in sum_off_rows) {
  if (sum_prev_end_idx < idx) {
    start_time <- df$time[sum_prev_end_idx]
    end_time <- df$time[[idx]]
    sum_time <- sum_time + calculate_time_diff(start_time, end_time)
  }
  sum_prev_end_idx <- idx + 1
}
sum_time

##### viewing_sum #####
viewing_sum <- 0

viewing_prev_end_idx <- 1

# "Off"가 포함된 행 찾기
viewing_off_rows <- which(grepl("Off", df$act, ignore.case = TRUE))

# 각 "Off" 이벤트까지의 시간 차이를 누적
for (idx in viewing_off_rows) {
  if (viewing_prev_end_idx < idx) {
    # "Off" 이벤트 전까지의 데이터에서 첫 번째 "member has entered"와 마지막 "member has exited" 이벤트 찾기
    chunk_df <- df[viewing_prev_end_idx:idx, ]
    
    # 첫 번째 "member has entered" 찾기
    enter_idxs <- which(grepl("has entered", chunk_df$act))
    exit_idxs <- which(grepl("exited", chunk_df$act))
    
    # "has entered"와 "exited"가 모두 존재하는 경우에만 시간 차이 계산
    if (length(enter_idxs) > 0 && length(exit_idxs) > 0) {
      first_enter_idx <- enter_idxs[1]  # 첫 번째 "member has entered"
      first_enter_time <- chunk_df$time[first_enter_idx]
      
      last_exit_idx <- exit_idxs[length(exit_idxs)]  # 마지막 "member has exited"
      last_exit_time <- chunk_df$time[last_exit_idx]
      
      # 시간 차이 계산
      time_diff <- calculate_time_diff(first_enter_time, last_exit_time)
      viewing_sum <- viewing_sum + time_diff
    }
  }
  # 이전 "Off" 이벤트 인덱스 업데이트
  viewing_prev_end_idx <- idx + 1
}

viewing_sum

##### 50% 비율 계산 #####

# 비율 계산 및 조건에 맞는 행 삭제
if (sum_time > 0) {
  total_ratio <- viewing_sum / sum_time
  if (total_ratio <= 0.5) {
    df <- df[0, ]  # 모든 행 삭제
    df <- data.frame(
      id = 'UV',
      time = NA,
      act = NA,
      chn = NA,
      date = NA
    )
  }
}


############### 날짜 하나 반복문 ###############
df0915_2_1 <- list()

# 시간 차이를 계산하는 함수 (24시간 이상 지원)
calculate_time_diff <- function(time1, time2) {
  time1_split <- as.numeric(unlist(strsplit(time1, ":")))
  time2_split <- as.numeric(unlist(strsplit(time2, ":")))
  
  # 시간을 초 단위로 변환 (24시간 이상 지원)
  seconds1 <- time1_split[1] * 3600 + time1_split[2] * 60 + time1_split[3]
  seconds2 <- time2_split[1] * 3600 + time2_split[2] * 60 + time2_split[3]
  
  # 시간 차이 반환
  return(seconds2 - seconds1)
}

# 데이터프레임 리스트 (df0915_2에 포함된 모든 데이터프레임에 대해 적용)
for (df_index in 1:length(df0915_2)) {
  
  # 현재 데이터프레임 가져오기
  df <- df0915_2[[df_index]]
  
  # 고유 id 값 추출 (이 값이 NA나 빈 값이 아닌지 확인)
  df_idx <- unique(df$id)
  
  if (length(df_idx) == 0 || is.na(df_idx) || is.null(df_idx)) {
    # id가 없거나 비어있으면 건너뜁니다
    next
  }
  
  ##### sum_time #####
  sum_time <- 0
  sum_prev_end_idx <- 1
  
  # "Off"가 포함된 행 찾기
  sum_off_rows <- which(grepl("Off", df$act, ignore.case = TRUE))
  
  # 각 "Off" 이벤트까지의 시간 차이를 누적
  for (idx in sum_off_rows) {
    if (sum_prev_end_idx < idx) {
      start_time <- df$time[sum_prev_end_idx]
      end_time <- df$time[[idx]]
      sum_time <- sum_time + calculate_time_diff(start_time, end_time)
    }
    sum_prev_end_idx <- idx + 1
  }
  
  ##### viewing_sum #####
  viewing_sum <- 0
  viewing_prev_end_idx <- 1
  
  # "Off"가 포함된 행 찾기
  viewing_off_rows <- which(grepl("Off", df$act, ignore.case = TRUE))
  
  # 각 "Off" 이벤트까지의 시간 차이를 누적
  for (idx in viewing_off_rows) {
    if (viewing_prev_end_idx < idx) {
      # "Off" 이벤트 전까지의 데이터에서 첫 번째 "member has entered"와 마지막 "member has exited" 이벤트 찾기
      chunk_df <- df[viewing_prev_end_idx:idx, ]
      
      # 첫 번째 "member has entered" 찾기
      enter_idxs <- which(grepl("has entered", chunk_df$act))
      exit_idxs <- which(grepl("exited", chunk_df$act))
      
      # "has entered"와 "exited"가 모두 존재하는 경우에만 시간 차이 계산
      if (length(enter_idxs) > 0 && length(exit_idxs) > 0) {
        first_enter_idx <- enter_idxs[1]  # 첫 번째 "member has entered"
        first_enter_time <- chunk_df$time[first_enter_idx]
        
        last_exit_idx <- exit_idxs[length(exit_idxs)]  # 마지막 "member has exited"
        last_exit_time <- chunk_df$time[last_exit_idx]
        
        # 시간 차이 계산
        time_diff <- calculate_time_diff(first_enter_time, last_exit_time)
        viewing_sum <- viewing_sum + time_diff
      }
    }
    # 이전 "Off" 이벤트 인덱스 업데이트
    viewing_prev_end_idx <- idx + 1
  }
  
  ##### 50% 비율 계산 #####
  
  # 비율 계산 및 조건에 맞는 행 삭제
  if (sum_time > 0) {
    total_ratio <- viewing_sum / sum_time
    if (total_ratio <= 0.5) {
      df <- df[0, ]  # 모든 행 삭제
      df <- data.frame(
        id = 'UV',
        time = NA,
        act = NA,
        chn = NA,
        date = NA
      )
    }
  }
  
  # 결과를 리스트에 업데이트
  df0915_2_1[[as.character(df_idx)]] <- df
}


# 여기 차이점은 게스트 시청으로 삭제된 id
length(df0915_2)
length(df0915_2_1)


############### 전체 반복문 ###############

calculate_time_diff <- function(time1, time2) {
  time1_split <- as.numeric(unlist(strsplit(time1, ":")))
  time2_split <- as.numeric(unlist(strsplit(time2, ":")))
  
  # 시간을 초 단위로 변환 (24시간 이상 지원)
  seconds1 <- time1_split[1] * 3600 + time1_split[2] * 60 + time1_split[3]
  seconds2 <- time2_split[1] * 3600 + time2_split[2] * 60 + time2_split[3]
  
  # 시간 차이 반환
  return(seconds2 - seconds1)
}
# df날짜_2 -> df날짜_2_1
for (month_num in 15:22) {
  # 동적으로 데이터프레임 리스트를 가져오기
  df_list_name <- paste0("df09", month_num, "_3") 
  df_list_new_name <- paste0("df09", month_num, "_4")
  
  df_list <- get(df_list_name)  # 해당 리스트 가져오기
  df_list_new <- list()  # 새로운 리스트 생성
  
  # 시간 차이를 계산하는 함수 (24시간 이상 지원)
  calculate_time_diff <- function(time1, time2) {
    time1_split <- as.numeric(unlist(strsplit(time1, ":")))
    time2_split <- as.numeric(unlist(strsplit(time2, ":")))
    
    # 시간을 초 단위로 변환 (24시간 이상 지원)
    seconds1 <- time1_split[1] * 3600 + time1_split[2] * 60 + time1_split[3]
    seconds2 <- time2_split[1] * 3600 + time2_split[2] * 60 + time2_split[3]
    
    # 시간 차이 반환
    return(seconds2 - seconds1)
  }
  
  # 각 데이터프레임 처리
  for (df_index in 1:length(df_list)) {
    # 현재 데이터프레임 가져오기
    df <- df_list[[df_index]]
    
    # 고유 id 값 추출 (이 값이 NA나 빈 값이 아닌지 확인)
    df_idx <- unique(df$id)
    
    if (length(df_idx) == 0 || is.na(df_idx) || is.null(df_idx)) {
      # id가 없거나 비어있으면 건너뜁니다
      next
    }
    
    ##### sum_time #####
    sum_time <- 0
    sum_prev_end_idx <- 1
    
    # "Off"가 포함된 행 찾기
    sum_off_rows <- which(grepl("Off", df$act, ignore.case = TRUE))
    
    # 각 "Off" 이벤트까지의 시간 차이를 누적
    for (idx in sum_off_rows) {
      if (sum_prev_end_idx < idx) {
        start_time <- df$time[sum_prev_end_idx]
        end_time <- df$time[[idx]]
        sum_time <- sum_time + calculate_time_diff(start_time, end_time)
      }
      sum_prev_end_idx <- idx + 1
    }
    
    ##### viewing_sum #####
    viewing_sum <- 0
    viewing_prev_end_idx <- 1
    
    # "Off"가 포함된 행 찾기
    viewing_off_rows <- which(grepl("Off", df$act, ignore.case = TRUE))
    
    # 각 "Off" 이벤트까지의 시간 차이를 누적
    for (idx in viewing_off_rows) {
      if (viewing_prev_end_idx < idx) {
        # "Off" 이벤트 전까지의 데이터에서 첫 번째 "member has entered"와 마지막 "member has exited" 이벤트 찾기
        chunk_df <- df[viewing_prev_end_idx:idx, ]
        
        # 첫 번째 "member has entered" 찾기
        enter_idxs <- which(grepl("has entered", chunk_df$act))
        exit_idxs <- which(grepl("exited", chunk_df$act))
        
        # "has entered"와 "exited"가 모두 존재하는 경우에만 시간 차이 계산
        if (length(enter_idxs) > 0 && length(exit_idxs) > 0) {
          first_enter_idx <- enter_idxs[1]  # 첫 번째 "member has entered"
          first_enter_time <- chunk_df$time[first_enter_idx]
          
          last_exit_idx <- exit_idxs[length(exit_idxs)]  # 마지막 "member has exited"
          last_exit_time <- chunk_df$time[last_exit_idx]
          
          # 시간 차이 계산
          time_diff <- calculate_time_diff(first_enter_time, last_exit_time)
          viewing_sum <- viewing_sum + time_diff
        }
      }
      # 이전 "Off" 이벤트 인덱스 업데이트
      viewing_prev_end_idx <- idx + 1
    }
    
    ##### 50% 비율 계산 #####
    
    # 비율 계산 및 조건에 맞는 행 삭제
    if (sum_time > 0) {
      total_ratio <- viewing_sum / sum_time
      if (total_ratio <= 0.5) {
        df <- df[0, ]  # 모든 행 삭제
        df <- data.frame(
          id = 'UV',
          time = NA,
          act = NA,
          chn = NA,
          date = NA
        )
      }
    }
    
    # 결과를 새로운 리스트에 업데이트 (고유 id를 이름으로 사용)
    df_list_new[[as.character(df_idx)]] <- df
  }
  
  # 결과를 새로운 리스트에 저장
  assign(df_list_new_name, df_list_new)
}

length(df0922_2_1)
length(df0922_2)


####### Uncovered viewing으로 사라진 가구 갯수 세기 + 확인 #######

# UV의 갯수를 셀 변수 초기화
uv_count <- 0

# df0915_2_1 리스트의 각 데이터프레임에 대해 반복
for (df in df0922_2_1) {
  # 'id' 열이 'UV'인 행의 개수를 추가
  uv_count <- uv_count + sum(df$id == 'UV', na.rm = TRUE)
}

# 결과 출력
print(uv_count)
94
130
98
79
84
89
61
63

# df0915_2_1 리스트에서 id 열의 값이 'UV'인 데이터프레임의 이름 출력
for (df_name in names(df0916_2_1)) {
  # 현재 데이터프레임 가져오기
  df <- df0915_2_1[[df_name]]
  
  # 'id' 열이 'UV'인 행이 있는지 확인
  if ("UV" %in% df$id) {
    # 'UV'인 데이터프레임 이름 출력
    print(df_name)
  }
}

df0915_2_1$"8800249"
df0916_2_1$'1403431'
df0922_2$'3401795'
df0915_2$'9200561'
