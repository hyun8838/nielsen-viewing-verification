###########################------meter data------###########################
library(stringr)
library(dplyr)

# load("MET.RData")
############ 0. 패널 가구 분리 및 필요한 정보 추출 ############
#### log_data function ####
log_data <- function(file_path, date) {
  
  log_data <- readLines(file_path)
  
  panel_data_list <- list()
  current_panel <- NULL
  current_data <- list()
  
  for (line in log_data) {
    if (str_detect(line, "^ - .*panel //d+")) {
      new_panel <- str_extract(line, "panel //d+")
      panel_id <- as.numeric(str_extract(new_panel, "//d+"))
      
      if (!is.null(current_panel) && length(current_data) > 0) {
        panel_df <- bind_rows(current_data) %>% 
          mutate(id = current_panel, date = date) %>%  # id와 date 컬럼 추가 (date를 정수형으로 처리)
          select(id, time, act, chn, date)  # 필요한 컬럼 선택 및 순서 지정
        panel_data_list[[as.character(current_panel)]] <- panel_df
      }
      
      current_panel <- panel_id
      current_data <- list()
      
    } else if (str_detect(line, "^//d{2}://d{2}://d{2}")) {
      
      time <- str_extract(line, "^//d{2}://d{2}://d{2}")
      chn <- ifelse(str_detect(line, "Chn //d+"), 
                    as.numeric(str_extract(line, "(?<=Chn )//d+")), 
                    NA)
      act <- str_match(line, "BU //d{2} : (.+?)(?= on| has| IRKEY|$)")[,2]
      
      if (str_detect(line, "member //d+ has (entered|exited)")) {
        member_action <- str_match(line, "member (//d+) has (entered|exited)")
        act <- paste("member", member_action[,2], "has", member_action[,3])
      }
      
      if (str_detect(line, "guest //d+ has (entered|exited)")) {
        guest_action <- str_match(line, "guest (//d+) has (entered|exited)")
        act <- paste("guest", guest_action[,2], "has", guest_action[,3])
      }
      
      current_data <- append(current_data, list(data.frame(
        time = time,
        chn = chn,
        act = act,
        stringsAsFactors = FALSE
      )))
    }
  }
  
  if (!is.null(current_panel) && length(current_data) > 0) {
    panel_df <- bind_rows(current_data) %>%
      mutate(id = current_panel, date = date) %>%
      select(id, time, act, chn, date)
    panel_data_list[[as.character(current_panel)]] <- panel_df
  }
  return(panel_data_list)
}

# 예시: 20240915
#setwd("C:/Users/legen/Desktop/Lab Project/닐슨/데이터/검증자료/")
#path_0915 <- "C:/Users/legen/Desktop/연구실 프로젝트/닐슨/데이터/검증자료/MET20240915"
#df0915 <- log_data(path_0915,20240915)


#### data 생성 ####
# df0915, ..., df0922 생성
#for (date in seq(start_date, end_date)) {
#  path_0 <- paste0(path, "/MET", date)
#  var_name <- paste0("df", substr(date, 5, 8))
#  assign(var_name, log_data(path_0, date))
#}


# ID
#names(df0915)
#length(df0915)


#### 반복문으로 생성한 데이터 저장: RData ####

#for (date in seq(start_date, end_date)) {
#  # df0915,...,df0922
#  var_name <- paste0("df", substr(date, 5, 8))
#  # 파일 경로
#  file_path <- paste0(path, "/", var_name, ".RData")
#
#  save(list = var_name, file = file_path)
#}

############ df날짜 로드 ############
getwd()
setwd("C:/Users/legen/Desktop/Lab Project/닐슨/데이터/검증자료")
# RData 파일 경로 설정
file_paths <- c("df0915.RData", "df0916.RData", "df0917.RData", "df0918.RData", 
                "df0919.RData", "df0920.RData", "df0921.RData", "df0922.RData")

# 모든 RData 파일 불러오기
for (file_path in file_paths) {
  load(file_path)  # 각 파일의 객체가 자동으로 메모리에 로드됨
}

# 로드된 객체 이름 확인
ls()

# 예시: 개별 데이터프레임 접근
# df0915, df0916 등으로 접근 가능 (파일에 저장된 객체 이름을 알 필요)
head(df0915)


############ 1. 게스트만 시청한 경우 삭제 ############
# 특정 조건에 따라 가구를 삭제하는 함수
remove_guest_only_ids <- function(data_list) {
  cleaned_data_list <- list()
  
  for (i in seq_along(data_list)) {
    df <- data_list[[i]]
    
    if ("act" %in% colnames(df)) {  # act 열이 존재하는 경우만 처리
      # "member"와 "guest"의 포함 여부를 확인
      member_ids <- unique(df$id[grepl("member", df$act, ignore.case = TRUE)])
      guest_ids <- unique(df$id[grepl("guest", df$act, ignore.case = TRUE)])
      
      # guest만 포함된 가구 ID
      guest_only_ids <- setdiff(guest_ids, member_ids)
      
      # guest_only_ids에 해당하지 않는 행만 필터링
      cleaned_df <- df %>% filter(!(id %in% guest_only_ids))
      
      cleaned_data_list[[names(data_list)[i]]] <- cleaned_df
    } else {
      # act 열이 없으면 그대로 저장
      cleaned_data_list[[names(data_list)[i]]] <- df
    }
  }
  
  return(cleaned_data_list)
}


# df날짜_1
# 데이터 처리: df0915 ~ df0922
for (day in 15:22) {
  var_name <- paste0("df09", day)
  if (exists(var_name)) {
    data_list <- get(var_name)  # 데이터 가져오기
    cleaned_data_list <- remove_guest_only_ids(data_list)  # 조건에 따라 가구 삭제
    
    # 새로운 이름으로 덮어쓰기
    new_var_name <- paste0(var_name, "_1")
    assign(new_var_name, cleaned_data_list)  # 새로운 변수명에 할당
  }
}


# 게스트 시청기록 id 삭제
df_1_list_name <- c("df0915_1", "df0916_1", "df0917_1", "df0918_1", "df0919_1", "df0920_1", "df0921_1", "df0922_1")

for (list_name in df_1_list_name) {
  # 리스트 가져오기
  df_list <- get(list_name)
  
  # 데이터프레임이 비어있는지 확인하고 삭제
  df_list <- df_list[sapply(df_list, function(x) nrow(x) > 0)]  # 비어 있지 않은 데이터프레임만 남기기
  
  # 수정된 리스트를 다시 할당
  assign(list_name, df_list)
}

# df날짜 vs df날짜_1을 비교하면 게스트 기록으로 삭제된 가구를 알 수 있다.
length(df0915_1)
length(df0915)
# df0915_1$'1402738'



############ 2. 1분 단위로 시청기록 재정리 ############

# 초(second)를 30초 기준으로 반올림하여 1분 단위로 조정

#### adjust_time function####
adjust_time <- function(time_vector) {
  sapply(time_vector, function(t) {
    hms <- as.numeric(unlist(strsplit(t, ":")))
    hours <- hms[1]
    minutes <- hms[2]
    seconds <- hms[3]
    
    # 30초 기준 반올림
    if (seconds >= 30) {
      minutes <- minutes + 1
      seconds <- 0
    } else {
      seconds <- 0
    }
    
    # 60분을 넘어가는 경우
    if (minutes == 60) {
      minutes <- 0
      hours <- hours + 1
    }
    
    # 시간 문자열로 변환
    sprintf("%02d:%02d:%02d", hours, minutes, seconds)
  })
}

# df날짜_1
#### df0915 ~ df0922 각 리스트 time 열에 adjust_time 적용 ####
### df날짜_1 -> df날짜_2 ###
for (day in 15:22) {
  var_name <- paste0("df09", day, "_1")
  if (exists(var_name)) {
    data_list <- get(var_name)  
    # adjust_time 반복 적용
    for (i in seq_along(data_list)) { 
      if ("time" %in% names(data_list[[i]])) { 
        data_list[[i]]$time <- adjust_time(data_list[[i]]$time)
      }
    }
    new_var_name <- gsub("_1$", "_2", var_name)
    assign(new_var_name, data_list)
  # assign(var_name, data_list) 
  }
}


#df0915_2[26]]
#df0921_2[[342]]



############ 2-1. Uncovered Viewing ############
#### df날짜_2 -> df날짜_2_1 ####
calculate_time_diff <- function(time1, time2) {
  time1_split <- as.numeric(unlist(strsplit(time1, ":")))
  time2_split <- as.numeric(unlist(strsplit(time2, ":")))
  
  # 시간을 초 단위로 변환 (24시간 이상 지원)
  seconds1 <- time1_split[1] * 3600 + time1_split[2] * 60 + time1_split[3]
  seconds2 <- time2_split[1] * 3600 + time2_split[2] * 60 + time2_split[3]
  
  # 시간 차이 반환
  return(seconds2 - seconds1)
}
for (month_num in 15:22) {
  # 동적으로 데이터프레임 리스트를 가져오기
  df_list_name <- paste0("df09", month_num, "_2")  # df0915_2, df0916_2, ... , df0922_2
  df_list_new_name <- paste0("df09", month_num, "_2_1")  # df0915_2_1, df0916_2_1, ... , df0922_2_1
  
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






############ 3. 동일 채널 시청 기록 ############

# member entered 행의 chn에 앞의 행 chn 가져옴
# chn 값이 0 또는 65535이면 NA 값으로 바꿈
entered_chn <- function(data_list) {
  for (i in seq_along(data_list)) {
    df <- data_list[[i]] %>%
      mutate(
        chn = if_else(endsWith(act, "entered"), lag(chn), chn),
        chn = case_when(
          chn == 0 ~ NA_real_,
          chn == 65535 ~ NA_real_,
          grepl("- Rec.", act) ~ NA_real_,
          TRUE ~ chn
        )
      )
    
    # 추가 처리: act가 entered이고 chn이 NA인 행의 chn 값을 업데이트
    for (j in seq_len(nrow(df))) {
      if (endsWith(df$act[j], "entered") && is.na(df$chn[j])) {
        if (j > 1 && endsWith(df$act[j - 1], "entered") && !is.na(df$chn[j - 1])) {
          df$chn[j] <- df$chn[j - 1]
        }
      }
    }
    
    data_list[[i]] <- df
  }
  
  return(data_list)
}

# member_entered 적용해서 df09{day}_2로 저장
#### df날짜_2_1 -> df날짜_3 ####
for (day in 15:22) {
  var_name <- paste0("df09", day, "_2_1")
  if (exists(var_name)) {
    data_list <- get(var_name)  
    data_list <- entered_chn(data_list)
    new_var_name <- gsub("_2_1$", "_3", var_name)
    assign(new_var_name, data_list)
  }
}

# 동일 채널기록 제거
remove_duplicate_chn <- function(data_list) {
  for (i in seq_along(data_list)) {
    # 1. 연속된 동일 채널 시청기록 제거
    data_list[[i]] <- data_list[[i]] %>%
      mutate(class0 = (chn == lag(chn) & !is.na(chn) & !is.na(lag(chn)) & !startsWith(act, "member"))) %>%
      filter(!class0)
    
    # 2. 동일 채널 시청기록 중 playback 등의 로그가 있는 경우 처리
    data_list[[i]] <- data_list[[i]] %>%
      mutate(class1 = (endsWith(act, "on") | endsWith(act, "off") | startsWith(act, "member")),
             class2 = (lag(chn) %in% c(NA, 0, 65535) & !lag(class1) & 
                         chn == lag(chn, n = 2) & time == lag(time))) %>%
      mutate(class2 = if_else(is.na(class2), FALSE, class2)) %>%
      filter(!(class2 | lead(class2, default=FALSE))) %>%
      select(-class0, -class1, -class2)
  }
  return(data_list)
}


# remove_duplicate_chn 적용해서 df09{day}_3으로 저장
for (day in 15:22) {
  var_name <- paste0("df09", day, "_3")
  if (exists(var_name)) {
    data_list <- get(var_name)  
    data_list <- remove_duplicate_chn(data_list)
    new_var_name <- gsub("_3$", "_3", var_name)
    assign(new_var_name, data_list)
  }
}



############ 4. 채널 번호 변환 ############
library(readxl)
library(dplyr)


# 채널리스트 전처리
# path <- "/Users/kaaaaaag_/Desktop/닐슨 프로젝트/chn_list.xlsx"
path <- "C:/Users/legen/Desktop/Lab Project/닐슨/chn_list.xlsx"
mapping_data <- read_excel(path)
mapping_data <- as.data.frame(mapping_data)
mapping_data$CHN <- as.numeric(mapping_data$CHN)

mapping_data <- mapping_data[!is.na(mapping_data$CHN), ]
mapping_data <- mapping_data[!duplicated(mapping_data$CHN), ]
mapping_data <- mapping_data %>% rename(chn = CHN, ariana_code = 아리아나코드) %>% select(chn, ariana_code)


# 아리아나코드 변환 
add_ariana_code <- function(data_list, mapping_data) {
  lapply(data_list, function(df) {
    df %>%
      mutate(
        ariana_code = case_when(
          is.na(chn) ~ NA_character_,  # chn이 NA인 경우는 NA로 유지
          chn == 0 ~ "activate",  # chn이 0이면 "activate"
          chn == 65535 ~ "total",  # chn이 65535면 "total"
          chn %in% mapping_data$chn ~ mapping_data$ariana_code[match(chn, mapping_data$chn)],  # 매핑된 경우
          TRUE ~ "unknown"  # 매핑되지 않은 경우 "unknown" 처리
        )
      )
  })
}

#str(mapping_data)
#str(df0915_3$'1402738')
list_of_data <- list(df0915_3, df0916_3, df0917_3, df0918_3, df0919_3, df0920_3, df0921_3, df0922_3)
list_of_data <- lapply(list_of_data, add_ariana_code, mapping_data = mapping_data)

for (i in seq_along(list_of_data)) {
  assign(paste0("df09", 15 + i - 1, "_4"), list_of_data[[i]])
}

# df0915_4[[1]]



################ mapping 되지 않은 채널코드 확인용 ################
all_data <- list(df0915_4, df0916_4, df0917_4, df0918_4, df0919_4, df0920_4, df0921_4, df0922_4)
all_dates <- c("20240915", "20240916", "20240917", "20240918", "20240919", "20240920", "20240921", "20240922")
unknown_chn_summary <- list()

for (date_idx in seq_along(all_data)) {
  data_list <- all_data[[date_idx]]  
  date <- all_dates[date_idx]       
  
  unknown_channels <- c()  
  
  for (i in seq_along(data_list)) {
    unknown_df <- data_list[[i]][!is.na(data_list[[i]]$ariana_code) & data_list[[i]]$ariana_code == "unknown", ]
    if (nrow(unknown_df) > 0) {
      unique_chn <- unique(unknown_df$chn)  
      unknown_channels <- unique(c(unknown_channels, unique_chn))  
    }
  }
  unknown_chn_summary[[date]] <- unique(unknown_channels)
}

# 날짜별
for (date in names(unknown_chn_summary)) {
  print(paste("Unknown chn for date", date))
  print(unknown_chn_summary[[date]])
}

# 전체 
unknown_chn_overall <- unique(unlist(unknown_chn_summary))
print(unknown_chn_overall)


############ 5. start & end 변수 생성 ############
# 윤진 part
library(lubridate)

create_start_end <- function(data_list) {
  for (i in seq_along(data_list)) {
    data_list[[i]] <- data_list[[i]] %>%
      mutate(
        start = time,
        time = lubridate::hms(time),
        end = lead(time) - lubridate::seconds(1),
        end = if_else(is.na(end), NA_character_, 
                      sprintf("%02d:%02d:%02d", 
                              as.integer(end) %/% 3600, 
                              (as.integer(end) %% 3600) %/% 60, 
                              as.integer(end) %% 60))
      ) %>%
      select(-time)
  }
  return(data_list)
}

# create_start_end 적용해서 df09{day}_5으로 저장
for (day in 15:22) {
  var_name <- paste0("df09", day, "_4")
  if (exists(var_name)) {
    data_list <- get(var_name)  
    data_list <- create_start_end(data_list)
    new_var_name <- gsub("_4$", "_5", var_name)
    assign(new_var_name, data_list)
  }
}

#df0919_5$'2405782' %>% arrange(ariana_code)


# 성민 part
library(dplyr)
# c - ii - 1
remove_playback_logs <- function(data_list) {
  cleaned_data_list <- list()
  
  for (i in seq_along(data_list)) {
    df <- data_list[[i]]
    
    # Playback 로그를 제외한 데이터만 남기기
    cleaned_df <- df[!grepl("Playback", df$act), ]
    
    cleaned_data_list[[names(data_list)[i]]] <- cleaned_df
  }
  
  return(cleaned_data_list)
}

for (day in 15:22) {
  var_name <- paste0("df09", day, "_5")
  if (exists(var_name)) {
    data_list <- get(var_name)  
    cleaned_data_list <- remove_playback_logs(data_list)
    assign(var_name, cleaned_data_list)  # 이름 그대로 유지하여 저장
  }
}


# c - ii - 2
remove_no_member <- function(data_list) {
  for (i in seq_along(data_list)) {
    df <- data_list[[i]]
    member_list <- c()  # 현재 상태를 저장하는 벡터
    member_column <- character(nrow(df))  # 최종 member 값을 저장할 벡터
    
    for (j in seq_len(nrow(df))) {
      act <- df$act[j]
      
      # "member {숫자} has entered" 처리
      if (grepl("^member \\d+ has entered$", act)) {
        member_id <- sub("member (\\d+) has entered", "\\1", act)
        member_list <- union(member_list, member_id)  # 중복 방지 추가
      }
      
      # "member {숫자} has exited" 처리
      if (grepl("^member \\d+ has exited$", act)) {
        member_id <- sub("member (\\d+) has exited", "\\1", act)
        member_list <- setdiff(member_list, member_id)  # 해당 멤버 제거
      }
      
      # 현재 member_list를 문자열로 저장
      member_column[j] <- paste(member_list, collapse = "")
    }
    
    # 데이터프레임에 member 열 추가
    df$member <- member_column
    df <- df[df$member != "", ]
    df <- df %>%
      select(-member)
    data_list[[i]] <- df  # 결과를 리스트에 다시 저장
  }
  
  return(data_list)
}

# df0915_5 ~ df0922_5 처리
for (day in 15:22) {
  var_name <- paste0("df09", day, "_5")
  if (exists(var_name)) {
    data_list <- get(var_name)  
    cleaned_data_list <- remove_no_member(data_list)
    assign(var_name, cleaned_data_list)  # 이름 그대로 유지하여 저장
  }
}

#tail(df0919_5$'2405782')
#df0919_5$'2405782' %>% arrange(ariana_code)



# c - ii - 3
# 여기서 NA인 entered와 exited가 삭제됨!

remove_na_chn_logs <- function(data_list) {
  cleaned_data_list <- list()
  
  for (i in seq_along(data_list)) {
    df <- data_list[[i]]
    
    # chn 컬럼이 NA가 아닌 행만 남기기
    cleaned_df <- df[!is.na(df$chn), ]
    
    cleaned_data_list[[names(data_list)[i]]] <- cleaned_df
  }
  
  return(cleaned_data_list)
}

# df0915_5 ~ df0922_5 처리
for (day in 15:22) {
  var_name <- paste0("df09", day, "_5")
  if (exists(var_name)) {
    data_list <- get(var_name)  
    cleaned_data_list <- remove_na_chn_logs(data_list)
    assign(var_name, cleaned_data_list)  # 이름 그대로 유지하여 저장
  }
}

#test <- df0919_5$'1403173' %>% arrange(ariana_code)
#test2 <- tx3_df[tx3_df$id == '1403173' & tx3_df$date == '20240919',]
# nrow(test); nrow(test2)  


############ 6. 이상치 제거 ############

library(dplyr)
library(lubridate)

remove_invalid_time_ranges <- function(data_list) {
  cleaned_data_list <- list()
  
  for (i in seq_along(data_list)) {
    df <- data_list[[i]]
    
    # start와 end를 시간 형식으로 변환
    df <- df %>%
      mutate(
        start_time = hms(start),
        end_time = hms(end)
      )
    
    # end가 start보다 이전인 행 필터링
    valid_df <- df %>% filter(is.na(end_time) | is.na(start_time) | (end_time >= start_time))
    
    # 시간 형식 컬럼 삭제 후 결과 저장
    valid_df <- valid_df %>% select(-start_time, -end_time)
    
    cleaned_data_list[[names(data_list)[i]]] <- valid_df
  }
  
  return(cleaned_data_list)
}

# df0915_5 ~ df0922_5 처리하고 결과를 _6으로 저장
for (day in 15:22) {
  var_name <- paste0("df09", day, "_5")
  if (exists(var_name)) {
    data_list <- get(var_name)  # 기존 데이터 가져오기
    cleaned_data_list <- remove_invalid_time_ranges(data_list)  # 잘못된 시간 범위 제거
    new_var_name <- gsub("_5$", "_6", var_name)  # 이름 변경
    assign(new_var_name, cleaned_data_list)  # 결과 저장
  }
}


# test 단계
############ 7. unix 변환 ############
# df날짜_7 생성
# 처리할 리스트 이름 생성
input_list_names <- paste0("df09", 15:22, "_6")
output_list_names <- paste0("df09", 15:22, "_7")

# 변환 적용 및 새 리스트 저장
for (i in seq_along(input_list_names)) {
  # 현재 리스트를 가져와 변환 적용
  assign(
    output_list_names[i],
    lapply(get(input_list_names[i]), function(df) {
      df %>%
        mutate(
          start_unix = as.numeric(lubridate::hms(start)),
          end_unix =  as.numeric(lubridate::hms(end))
        )
    })
  )
}

# df0915_7$'1402738'








############ 잡동사니 ############
#path <- ""
#load(paste0(path, '/', 'MET.RData'))

save()

# 데이터 프레임 이름 생성
main_frames <- paste0("df09", 15:22)  # df0915부터 df0922까지
sub_frames <- unlist(lapply(15:22, function(i) paste0("df09", i, "_", 1:7)))  # df0915_1부터 df0922_7까지

# 모든 데이터 프레임 이름을 하나로 합치기
all_frames <- c(main_frames, sub_frames)

# MET.RData에 저장
save(list = all_frames, file = "MET.RData")

getwd()

# 확인: 저장된 객체 불러오기 (테스트)
load("MET.RData")
