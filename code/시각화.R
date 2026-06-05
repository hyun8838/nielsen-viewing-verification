

########################### 시각화 ###########################

library(ggplot2)
library(dplyr)

# date별 행 개수 계산
date_counts <- id_df %>%
  group_by(date) %>%
  summarise(row_count = n())

# 그래프 생성
ggplot(date_counts, aes(x = as.Date(as.character(date), format = "%Y%m%d"), y = row_count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "blue") +
  labs(
    title = "Frequency of Rows by Date",
    x = "Date",
    y = "Row Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# date별 행 개수 계산
date_counts_tx3 <- id_tx3 %>%
  group_by(date) %>%
  summarise(row_count = n())

# 그래프 생성
ggplot(date_counts_tx3, aes(x = as.Date(as.character(date), format = "%Y%m%d"), y = row_count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "blue") +
  labs(
    title = "Frequency of Rows by Date",
    x = "Date",
    y = "Row Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


########################### combined ###########################
library(dplyr)
library(ggplot2)
# 두 데이터프레임에 구분 열 추가
date_counts <- combined_df_filtered %>%
  group_by(date) %>%
  summarise(row_count = n()) %>%
  mutate(source = "ours")


# date별 행 개수 계산
date_counts_tx3 <- tx3_df_filtered %>%
  group_by(date) %>%
  summarise(row_count = n())  %>%
  mutate(source = "tx3")



# 두 데이터프레임 병합
combined_data <- rbind(date_counts, date_counts_tx3)

par(mfrow = c(1, 1))

# 그래프 생성
ggplot(combined_data, aes(
  x = as.Date(as.character(date), format = "%Y%m%d"),
  y = row_count,
  fill = source
)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Frequency of Rows by Date (Comparison)",
    x = "Date",
    y = "Row Count",
    fill = "Source"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






########################### 여러개 ###########################
# ID 목록
id_list <- c('4401038', '1403526', '2406153', '7800563', '6600297', '8200796')

# 그래프를 위한 다중 플롯 설정
par(mfrow = c(2, 3))  # 2행 3열로 그래프 배열

library(ggplot2)
library(dplyr)
library(gridExtra)

# 그래프 저장용 리스트 초기화
plot_list <- list()

# 각 ID에 대해 그래프 생성
for (id in id_list) {
  # 데이터 필터링
  id_df <- combined_df_filtered[combined_df_filtered$id == id, ]
  id_tx3 <- tx3_df_filtered[tx3_df_filtered$id == id, ]
  
  date_counts <- id_df %>%
    group_by(date) %>%
    summarise(row_count = n()) %>%
    mutate(source = "ours")
  
  date_counts_tx3 <- id_tx3 %>%
    group_by(date) %>%
    summarise(row_count = n()) %>%
    mutate(source = "tx3")
  
  combined_data <- rbind(date_counts, date_counts_tx3)
  
  # 개별 그래프 생성
  p <- ggplot(combined_data, aes(
    x = as.Date(as.character(date), format = "%Y%m%d"),
    y = row_count,
    fill = source
  )) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    labs(
      title = paste("Frequency of Rows for ID:", id),
      x = "Date",
      y = "Row Count",
      fill = "Source"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # 리스트에 그래프 추가
  plot_list[[as.character(id)]] <- p
}

# 하나의 화면에 그래프 배열
do.call(grid.arrange, c(plot_list, ncol = 3))







