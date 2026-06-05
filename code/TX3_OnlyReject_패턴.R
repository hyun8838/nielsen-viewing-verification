# TX3와 비교하기 위한 원 MET 데이터
dates <- sprintf("09%02d", 15:22)
df_names <- paste0("df", dates)
df_list <- mget(df_names, ifnotfound = list(NULL))

extracted_dfs <- lapply(df_list, function(inner_list) {
  if (is.list(inner_list)) {
    do.call(rbind, inner_list)  # 리스트 내부 데이터프레임 병합
  } else {
    NULL
  }
})

# NULL 필터링 (빈 데이터 제외)
extracted_dfs <- Filter(Negate(is.null), extracted_dfs)

# 최종 병합
met_df <- do.call(rbind, extracted_dfs)
rownames(met_df) <- NULL

###############################################################################
################ TX3_Reject ################
# 원 MET 데이터에는 있는데 TX3에는 없는 날짜별 가구id (닐슨 측의 reject 가구)

library(dplyr)

# 날짜별로 고유한 id만 추출
met_unique <- met_df %>%
  select(date, id) %>%
  distinct()
met_unique$date <- as.character(met_unique$date)

tx3_unique <- tx3_df %>%
  select(date, id) %>%
  distinct()

# 날짜별로 met_df에는 있지만 tx3_df에는 없는 id 찾기
tx3_reject_list <- met_unique %>%
  anti_join(tx3_unique, by = c("date", "id")) %>%
  split(.$date)
# tx3_reject_list # tx3에서 리젝된 가구id

# 날짜별 tx3에서 리젝된 가구 수
tx3_reject_num <- sapply(tx3_reject_list, nrow)
tx3_reject_num

tx3_onlyreject_list <- setdiff(tx3_reject_list$'20240921'$id, df_reject_list$'20240921'$id)


# 조건에 해당하는 id를 저장할 리스트 생성
result_list <- list(
  member_guest = list(),
  guest_only = list(),
  playback_rec = list(),
  playback_only = list(),
  rec_only = list()
)

# tx3_onlyreject_list에 있는 숫자에 대해 반복
for (i in 1:length(tx3_onlyreject_list)) {
  id <- as.character(tx3_onlyreject_list[i])
  
  # 해당 id에 해당하는 데이터프레임
  df <- df0915[[id]]
  
  # act 열에서 각 조건에 맞는 데이터를 확인하고 저장
  if (any(grepl("member", df$act, ignore.case = TRUE)) && any(grepl("guest", df$act, ignore.case = TRUE))) {
    result_list$member_guest[[id]] <- df
  }
  if (any(grepl("guest", df$act, ignore.case = TRUE))) {
    result_list$guest_only[[id]] <- df
  }
  if (any(grepl("Playback", df$act, ignore.case = TRUE)) && any(grepl("Rec.", df$act, ignore.case = TRUE))) {
    result_list$playback_rec[[id]] <- df
  }
  if (any(grepl("Playback", df$act, ignore.case = TRUE))) {
    result_list$playback_only[[id]] <- df
  }
  if (any(grepl("Rec.", df$act, ignore.case = TRUE))) {
    result_list$rec_only[[id]] <- df
  }
}

# 5가지 패턴인 것들
tx3_onlyreject_patterns_list<- unique(c(names(result_list$member_guest), names(result_list$guest_only), names(result_list$playback_rec), names(result_list$playback_only), names(result_list$rec_only))
)
aa <- setdiff(tx3_onlyreject_list, tx3_onlyreject_patterns_list)
length(aa)
setdiff(tx3_onlyreject_patterns_list, tx3_onlyreject_list)
df0915[as.character(aa[5])]

df0915[as.character(aa[68])]
df0915[as.character(aa[64])]


df0915_9_2[as.character(aa[59])]
tx3_df_filtered %>%
  filter(id == as.character(aa[32]) & date == '20240915')

combined_df_filtered %>%
  filter(id == as.character(aa[32]) & date == '20240915')


df0915$'2405334'
# 결과 출력
names(result_list)[1]
names(result_list[names(result_list)[1]])
combined_df_filtered %>%
  filter(id == '2405334' & date == '20240915')
df0915$'2405334'
