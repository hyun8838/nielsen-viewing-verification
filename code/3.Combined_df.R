
###################### 최종 코드 데이터 형식 변환 ######################

# 날짜 리스트 생성
dates <- sprintf("09%02d", 15:22)  # "0915", "0916", ..., "0922"

# 병합 대상 데이터프레임 이름 생성
df_names <- paste0("df", dates, "_9_2")

# 존재하는 데이터프레임만 가져오기
df_list <- mget(df_names, ifnotfound = list(NULL))  # 없는 객체는 NULL로 처리

# 리스트 안의 데이터프레임 추출
extracted_dfs <- lapply(df_list, function(inner_list) {
  if (is.list(inner_list)) {
    do.call(rbind, inner_list)  # 리스트 내부 데이터프레임 병합
  } else {
    NULL
  }
})
# extracted_dfs$df0915_9_2

# NULL 필터링 (빈 데이터 제외)
extracted_dfs <- Filter(Negate(is.null), extracted_dfs)

# 최종 병합
combined_df <- do.call(rbind, extracted_dfs)


########################### TX3 파일 불러오기 ###########################
tx3_df <- function(file_path) {
  file_name <- basename(file_path)
  # 파일 이름에서 날짜 추출 (MMDD 형식)
  date <- sub("^(\\d{4})(\\d{2})(\\d{2}).*", "\\2\\3", file_name)  # MMDD 추출
  date <- paste0("2024", date)  # "2024"와 MMDD를 결합
  
  lines <- readLines(file_path)
  all_results <- list()
  current_result <- list()
  
  # tx3 파일에서 각각 가구를 분리하고 데이터프레임으로 만드는 함수
  v_data <- function(id, v_lines) {
    result <- list()
    
    for (line in v_lines) {
      if (grepl("^V", line)) {
        ariana_code <- sub("^(V\\d+)_.*", "\\1", line)
        matches <- regmatches(line, gregexpr("([a-zA-Z]{2})(\\d{6})(\\d{6})", line))[[1]]
        
        for (match in matches) {
          start <- substr(match, 3, 8)  # 첫 번째 6자리 숫자
          end <- substr(match, 9, 14)  # 두 번째 6자리 숫자
          result <- append(result, list(c(as.numeric(id), ariana_code, start, end, date)))
        }
      }
    }
    
    # 데이터프레임으로 변환
    if (length(result) > 0) {
      df <- as.data.frame(do.call(rbind, result), stringsAsFactors = FALSE)
      colnames(df) <- c("id", "ariana_code", "start", "end", "date")  # 컬럼 이름 명시적으로 지정
      df$id <- as.numeric(df$id)  # id를 numeric으로 변환
      return(df)
    } else {
      return(NULL)
    }
  }
  
  for (line in lines) {
    if (grepl("^H", line)) {  # H로 시작하는 줄 찾기
      if (length(current_result) > 0) {
        id <- as.numeric(sub("H(\\d+)_.*", "\\1", current_result[1]))
        df <- v_data(id, current_result[-1])
        
        if (is.null(df)) {
          # V 행이 없는 경우, 빈 데이터프레임 생성
          df <- data.frame(
            id = id,
            ariana_code = NA_character_,
            start = NA_character_,
            end = NA_character_,
            date = date,
            stringsAsFactors = FALSE
          )
        }
        
        all_results[[as.character(id)]] <- df  # 결과 저장
      }
      current_result <- list(line)  # 새로운 세트 시작
    } else {
      current_result <- append(current_result, line)  # 현재 세트에 줄 추가
    }
  }
  
  # 마지막 세트 처리
  if (length(current_result) > 0) {
    id <- as.numeric(sub("H(\\d+)_.*", "\\1", current_result[1]))
    df <- v_data(id, current_result[-1])
    
    if (is.null(df)) {
      # V 행이 없는 경우, 빈 데이터프레임 생성
      df <- data.frame(
        id = id,
        ariana_code = NA_character_,
        start = NA_character_,
        end = NA_character_,
        date = date,
        stringsAsFactors = FALSE
      )
    }
    
    all_results[[as.character(id)]] <- df
  }
  
  return(all_results)  # ID별 리스트 반환
}

# 작업 디렉토리 설정
setwd("C:/D/공부/대학원/연구실/프로젝트/닐슨/검증자료/")
files = list.files(pattern = ".tx3$", full.names = TRUE)

# 파일별로 리스트 생성 및 저장
for (file_path in files) {
  date <- as.character(sub("^(\\d{4})(\\d{2})(\\d{2}).*", "\\2\\3", basename(file_path)))  # MMDD 추출
  assign(paste0("tx", date), tx3_df(file_path))  # MMDD 형식으로 저장
}


# 병합 대상 데이터프레임 이름 생성
tx_names <- c("tx0915", "tx0916", "tx0917", "tx0918", "tx0919", "tx0920", "tx0921", "tx0922")

# 존재하는 데이터프레임만 가져오기
tx_list <- mget(tx_names, ifnotfound = list(NULL))  # 없는 객체는 NULL로 처리

# 리스트 안의 데이터프레임 추출
extracted_dfs <- lapply(tx_list, function(inner_list) {
  if (is.list(inner_list)) {
    do.call(rbind, inner_list)  # 리스트 내부 데이터프레임 병합
  } else {
    NULL
  }
})

# NULL 필터링 (빈 데이터 제외)
extracted_dfs <- Filter(Negate(is.null), extracted_dfs)

# 최종 병합
tx3_df <- do.call(rbind, extracted_dfs)
rownames(tx3_df) <- NULL


########################### 비교를 위한 데이터 통합 ###########################
# tx3_df의 ch_no를 ariana_code로 이름 변경
# 65535 채널 제외
tx3_df_filtered <- tx3_df %>%
  filter(!ariana_code %in% c("V65535", "V743", NA))

# combined_df에서 열 선택 후 ariana_code로 열 이름 변경
combined_df_filtered <- combined_df %>%
  select(id, ariana_code, start, end, date) %>%
  mutate(
    start = gsub(":", "", start),
    end = gsub(":", "", end),
    date = as.character(date)
  )



###################### 데이터 저장 ######################

save.image("C:/D/공부/대학원/연구실/프로젝트/닐슨/코드/3.Combined_df.RData")
