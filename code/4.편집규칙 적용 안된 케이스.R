library(lubridate)

##################################################################
###################### 편집 규칙 적용 안된 case: 현일 ######################
##################################################################

###################### tx3에서 constant viewing 1 적용 X case ######################
tx3_cv1_x <- tx3_df_filtered %>%
  mutate(
    start_unix = as.numeric(hms(sub("(..)(..)(..)", "\\1:\\2:\\3", start))),
    end_unix = as.numeric(hms(sub("(..)(..)(..)", "\\1:\\2:\\3", end))),
    gap = end_unix - start_unix,
    time_diff = seconds_to_period(gap)
  ) %>%
  filter(gap > as.numeric(hms("12:00:00"))) %>%
  select('id', 'ariana_code', 'start', 'end', 'date', 'time_diff')

tx3_cv1_x

# TX3
#tx3_df_filtered[tx3_df_filtered$id == '2406260' & tx3_df_filtered$date == '20240918',] %>% arrange(start, end)
# 연구진 데이터
#combined_df_filtered[combined_df_filtered$id == '2406260' & combined_df_filtered$date == '20240918',]

###################### tx3에서 constant viewing 2 적용 X case ######################
tx3_cv2_x <- tx3_df_filtered %>%
  filter(start < "020001" & end < "070000" & end > "055959") %>%
  arrange(start, end)
tx3_cv2_x


# TX3
#tx3_df_filtered[tx3_df_filtered$id == '2404068' & tx3_df_filtered$date == '20240915',]
# 연구진 데이터
#combined_df_filtered[combined_df_filtered$id == '2404068' & combined_df_filtered$date == '20240915',]

#combined_df_filtered %>%
#  filter(start < "020001" & end < "070000" & end > "055959") %>%
#  arrange(start, end)