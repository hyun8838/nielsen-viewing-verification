library(dplyr)
library(lubridate)


#setwd("C:/Users/legen/Desktop/Lab Project/닐슨/데이터/검증자료/")
getwd()

###################### 데이터 로드 ######################
#load("MET.RData")
#df0915$"1402943"
#df0915_2$"1402943"

# 리스트 생성
#data_list <- list(df0915_9 = df0915_9, df0916_9 = df0916_9, df0917_9 = df0917_9, 
#                  df0918_9 = df0918_9, df0919_9 = df0919_9, df0920_9 = df0920_9, 
#                  df0921_9 = df0921_9, df0922_9 = df0922_9)


data_list <- list(df0915_9 = df0915_9, df0916_9 = df0916_9, df0917_9 = df0917_9, 
                  df0918_9 = df0918_9, df0919_9 = df0919_9, df0920_9 = df0920_9, 
                  df0921_9 = df0921_9, df0922_9 = df0922_9)



###################### Constant Viewing 1 ######################
# 12시간을 초 단위로 변환
threshold_gap <- as.numeric(hours(12))

# constant_viewing_1
data_list <- lapply(data_list, function(sublist) {
  lapply(sublist, function(df) {
    df %>%
      mutate(
        start_unix = as.numeric(lubridate::hms(start)),
        end_unix = as.numeric(lubridate::hms(end)),
        gap = end_unix - start_unix
      ) %>%
      filter(gap < threshold_gap) # gap이 12시간 미만인 행만 유지
  })
})


#data_list$df0915_9$"1402738"



# 저장
# 데이터_전처리단계_편집규칙단계
df0915_9_1 <- data_list$df0915_9
df0916_9_1 <- data_list$df0916_9
df0917_9_1 <- data_list$df0917_9
df0918_9_1 <- data_list$df0918_9
df0919_9_1 <- data_list$df0919_9
df0920_9_1 <- data_list$df0920_9
df0921_9_1 <- data_list$df0921_9
df0922_9_1 <- data_list$df0922_9





###################### Constant Viewing 2 ######################

# 데이터 리스트 생성
data_list_2 <- list(df0915_9_2 = df0915_9_1, df0916_9_2 = df0916_9_1, df0917_9_2 = df0917_9_1,
                    df0918_9_2 = df0918_9_1, df0919_9_2 = df0919_9_1, df0920_9_2 = df0920_9_1,
                    df0921_9_2 = df0921_9_1, df0922_9_2 = df0922_9_1)

# constant_viewing_2
constant_viewing_2 <- function(df) {
  if (nrow(df) == 0) {
    warning("빈 데이터프레임이 입력되었습니다.")
    return(df)  # 빈 데이터프레임이 들어오면 그대로 반환
  }
  
  start_reject <- 7200  # 02:00:00
  end_reject <- 21600   # 06:00:00
  
  result <- list()  # 결과 저장할 리스트
  
  for (i in 1:nrow(df)) {
    start <- df$start_unix[i]
    end <- df$end_unix[i]
    
    # (1) 구간이 02:00 이전에 시작하고 06:00 이후에 끝나는 경우 (02:00, 06:00 포함)
    if (start <= start_reject & end >= end_reject) {
      # 첫 번째 구간: 02:00 이전
      new_row <- df[i, ]
      new_row$end_unix <- start_reject # - 1 # 2시까지 포함하는건가? 아니면 1초 빼기?
      new_row$end <- format(as.POSIXct(new_row$end_unix, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
      new_row$gap <- new_row$end_unix - new_row$start_unix
      result[[length(result) + 1]] <- new_row
      
      # 두 번째 구간: 06:00 이후
      new_row2 <- df[i, ]
      new_row2$start_unix <- end_reject
      new_row2$start <- format(as.POSIXct(new_row2$start_unix, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
      new_row2$gap <- new_row2$end_unix - new_row2$start_unix
      result[[length(result) + 1]] <- new_row2
    } 
    
    # (2) 02:00 ~ 06:00 사이에 포함되는 경우 그대로 유지
    else if (start > start_reject & end < end_reject) {
      result[[length(result) + 1]] <- df[i, ]
    } 
    
    # (3) start가 02:00:00 이전 시작이고 end가 06:00:00 이전 끝나는 경우
    else if (start <= start_reject & end < end_reject) {
      result[[length(result) + 1]] <- df[i, ]
    }
    
    # (4) start가 02:00:00 이후 시작이고 end가 06:00:00 이후(포함) 끝나는 경우
    else if (start > start_reject & end >= start_reject) {
      result[[length(result) + 1]] <- df[i, ]
    } 
    
    # (5) start와 end가 모두 06:00:00 이후인 경우
    else if (start >= end_reject & end >= end_reject) {
      result[[length(result) + 1]] <- df[i, ]
    }
    
    # (6) start와 end가 모두 02:00:00 이전전인 경우
    else if (start <= start_reject & end <= start_reject) {
      result[[length(result) + 1]] <- df[i, ]
    } 
    
    # (7) 기타: 경계 조건을 초과하지 않으면 그대로 유지
    else {
      result[[length(result) + 1]] <- df[i, ]
    }
    
  }
  
  # 결과 리스트를 데이터프레임으로 변환
  result_df <- bind_rows(result)
  
  result_df <- result_df[result_df$gap > 0, ]
  
  return(result_df)
}

# 전체 적용
apply_constant_viewing_2 <- function(data_list) {
  # 리스트 안에 있는 모든 데이터프레임에 함수 적용
  result_list <- lapply(data_list, function(df_list) {
    # 각 df_list의 데이터프레임에 함수 적용, 비어있는 데이터프레임은 제외
    processed_list <- lapply(df_list, function(df) {
      if (nrow(df) > 0) {
        return(constant_viewing_2(df))  # 데이터프레임이 비어있지 않으면 함수 적용
      } else {
        return(NULL)  # 비어있으면 NULL 반환
      }
    })
    
    # NULL 값을 제외한 결과만 반환
    processed_list <- processed_list[!sapply(processed_list, is.null)]
    
    return(processed_list)
  })
  
  return(result_list)
}


# df0915_9_2에 함수 적용
data_list_2 <- apply_constant_viewing_2(data_list_2)


#data_list_2$df0915_9_2


# 저장
# 데이터_전처리단계_편집규칙단계
df0915_9_2 <- data_list_2$df0915_9_2
df0916_9_2 <- data_list_2$df0916_9_2
df0917_9_2 <- data_list_2$df0917_9_2
df0918_9_2 <- data_list_2$df0918_9_2
df0919_9_2 <- data_list_2$df0919_9_2
df0920_9_2 <- data_list_2$df0920_9_2
df0921_9_2 <- data_list_2$df0921_9_2
df0922_9_2 <- data_list_2$df0922_9_2


#df0919_9_2$'2405782'[,c('id','chn','ariana_code', 'date', 'start', 'end')]
#tx3_df[tx3_df$id == '2405782' & tx3_df$date == '20240919',][1:95,]
#tx3_df %>%
#  filter(id == "2405782", date == "20240919") %>%
#  slice(1:94) %>%
#  arrange(start)
#nrow(df0919_9$'2405782'[,c('id','chn','ariana_code', 'date', 'start', 'end')])
#nrow(tx3_df[tx3_df$id == '2405782' & tx3_df$date == '20240919',])








###################### 이건 안돌려도 됨 ######################
###################### constant_viewing_2 함수 테스트 ######################
# test

test <- data.frame(
  id = "1402738",
  act = "DCAB",
  chn = 4370,
  date = "20240915",
  ariana_code = "V405",
  start = c("01:23:00", "03:20:00", "02:30:00", "07:00:00", "01:59:59", "02:00:01", "02:00:00", "01:50:00"),
  end = c("08:35:00", "05:00:00", "06:30:00", "09:00:00", "06:00:00", "06:00:00", "06:00:01", "05:59:59"),
  start_unix = as.numeric(lubridate::hms(c("01:23:00", "03:20:00", "02:30:00", "07:00:00", "01:59:59", "02:00:01", "02:00:00", "01:50:00"))),
  end_unix = as.numeric(lubridate::hms(c("08:35:00", "05:00:00", "06:30:00", "09:00:00", "06:00:00", "06:00:00", "06:00:01", "05:59:59"))),
  gap = as.numeric(lubridate::hms(c("08:35:00", "05:00:00", "06:30:00", "09:00:00", "06:00:00", "06:00:00", "06:00:01", "05:59:59")))
  - as.numeric(lubridate::hms(c("01:23:00", "03:20:00", "02:30:00", "07:00:00", "01:59:59", "02:00:01", "02:00:00", "01:50:00")))
)

test_result <- constant_viewing_2(test)
test
print(test_result)



getwd()
