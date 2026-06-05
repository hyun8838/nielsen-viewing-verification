
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
          member = substr(match, 1, 2) # 알파벳 두 글자
          start = substr(match, 3, 8) # 첫 번째 6자리 숫자
          end = substr(match, 9, 14)  # 두 번째 6자리 숫자
          
          result = append(result, list(c(id, ch_no, member, start, end, date)))
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
  colnames(final_result) = c("id", "ch_no", "member", "start", "end", "date")
  final_result$id = as.numeric(final_result$id)
  
  return(final_result)
}

setwd("D:/공부/대학원/연구실/프로젝트/닐슨/검증자료")
files = list.files(pattern = ".tx3$", full.names = FALSE) # 파일명 불러오기

result = lapply(files, tx3_df)
final_result = do.call(rbind, result)
str(final_result)
final_result
