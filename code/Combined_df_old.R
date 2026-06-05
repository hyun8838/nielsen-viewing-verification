
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


########################### TX3 파일 불러오기기 ###########################
tx3_df = function(file_path) {
  
  file_name = basename(file_path)
  # 파일 이름에서 날짜 추출
  date = sub("^(\\d{8}).*", "\\1", file_name)
  
  lines = readLines(file_path)
  
  all_results = list()
  current_result = list()
  
  # tx3파일에서 각각 가구를 분리하고 데이터프레임으로 만드는 함수
  v_data = function(id, v_lines) {
    result = list()
    
    for (line in v_lines) {
      if (grepl("^V", line)) {
        ch_no = sub("^(V\\d+)_.*", "\\1", line)
        matches = regmatches(line, gregexpr("([a-zA-Z]{2})(\\d{6})(\\d{6})", line))[[1]]
        
        for (match in matches) {
          start = substr(match, 3, 8) # 첫 번째 6자리 숫자
          end = substr(match, 9, 14)  # 두 번째 6자리 숫자
          
          result = append(result, list(c(id, ch_no, start, end, date)))
        }
      }
    }
    
    # 데이터프레임으로 변환
    if (length(result) > 0) {
      return(as.data.frame(do.call(rbind, result), stringsAsFactors = FALSE))
    } else {
      return(NULL)
    }
  }
  
  for (line in lines) {
    # 가구의 시작을 찾기 위해 H로 시작하는 줄 찾기
    if (grepl("^H", line)) {
      
      if (length(current_result) > 0) {
        id = sub("H(\\d+)_.*", "\\1", current_result[1])
        df = v_data(id, current_result[-1])
        if (!is.null(df)) all_results = append(all_results, list(df))
      }
      # 새로운 세트 시작
      current_result = list(line)
    } else {
      # 현재 세트에 줄 추가
      current_result = append(current_result, line)
    }
  }
  
  # 마지막 세트 처리
  if (length(current_result) > 0) {
    id = sub("H(\\d+)_.*", "\\1", current_result[1])
    df = v_data(id, current_result[-1])
    if (!is.null(df)) all_results = append(all_results, list(df))
  }
  
  # 모든 결과를 하나의 데이터프레임으로 합침
  final_result = do.call(rbind, all_results)
  colnames(final_result) = c("id", "ch_no", "start", "end", "date")
  final_result$id = as.numeric(final_result$id)
  
  return(final_result)
}

# 경로 지정
setwd("C:/Users/legen/Desktop/Lab Project/닐슨/데이터/검증자료/")
files = list.files(pattern = ".tx3$", full.names = FALSE) # 파일명 불러오기

result = lapply(files, tx3_df)
tx3_df = do.call(rbind, result)


########################### 비교를 위한 데이터 통합 ###########################
# tx3_df의 ch_no를 ariana_code로 이름 변경
# 65535 채널 제외
tx3_df_filtered <- tx3_df %>%
  filter(!ch_no %in% c("V65535", "V743")) %>%
  rename(ariana_code = ch_no)

# combined_df에서 열 선택 후 ariana_code로 열 이름 변경
combined_df_filtered <- combined_df %>%
  select(id, ariana_code, start, end, date) %>%
  mutate(
    start = gsub(":", "", start),
    end = gsub(":", "", end),
    date = as.character(date)
  )


#head(combined_df_filtered); head(tx3_df_filtered)


###################### 데이터 저장 ######################
# 날짜 리스트 생성
dates <- sprintf("09%02d", 15:22)  # "0915", "0916", ..., "0922"

# 각 날짜별 데이터 이름 생성
df_names <- unlist(lapply(dates, function(date) {
  paste0("df", date, c("", "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9", "_9_1", "_9_2"))
}))

# 추가적인 고정 객체 이름들
extra_names <- c("combined_df", "combined_df_filtered", "tx3_df", "tx3_df_filtered")

# df_names와 extra_names 합치기
all_df_names <- c(df_names, extra_names)

# 결과 확인
print(all_df_names)

# 존재하는 객체만 가져오기
save_list <- mget(all_df_names, ifnotfound = list(NULL))  # 없는 객체는 NULL로 처리

# NULL이 아닌 객체만 필터링
#save_list <- Filter(Negate(is.null), save_list)

# MET.RData로 저장
save(list = names(save_list), file = "MET.RData")
