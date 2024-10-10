# packages install
install.packages("ggplot2")
install.packages("reshape2")
install.packages("rgl")

library(rgl)
library(ggplot2)
library(reshape2)

#### 시각화 
### Cluster 0
# 데이터 불러오기
churn <- read.csv("C:/Users/yonghwa/Desktop/churn.csv")
cluster_0 <- read.csv("C:/Users/yonghwa/Desktop/final_cluster_0.csv")
#cluster_1 <- read.csv("C:/Users/wldbs/Desktop/final_cluster_1.csv")

## 이탈자 제거하기
# '순번'이 churn에 있는 경우 해당 행을 제거
cluster_0 <- cluster_0[!(cluster_0$순번 %in% churn$col7), ]
dim(cluster_0)

# ggplot2 패키지 로드
library(ggplot2)

df <- cluster_0
# 자산잔액과 부채잔액의 범주별 빈도 계산
freq_table <- data.frame(
  자산잔액 = as.data.frame(table(df$자산잔액))$Freq,
  부채잔액 = as.data.frame(table(df$부채잔액))$Freq,
  범주 = 0:9  # 자산잔액과 부채잔액의 범주
)

# 데이터를 long 형태로 변환
freq_melted <- melt(freq_table, id.vars = "범주")

# 라인 플롯 그리기
ggplot(freq_melted, aes(x = 범주, y = value, color = variable, group = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Category", y = "Frequency", title = "Frequency Comparison by Asset and Liability Category_Cluster0") +
  scale_color_manual(values = c("#d2b48c", "#b22222"), labels = c("Asset Balance", "Liability Balance")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # 제목 가운데 정렬
  scale_y_continuous(labels = scales::comma)  # y축 값 일반 숫자 형식으로 표시


#######################################################
# 동일한 '순번'과 'col7' 값을 가진 데이터를 apps 데이터프레임에서 추출
result <- apps[apps$col7 %in% cluster_0$순번, ]

# 추출된 결과를 확인
print(result)

###apps 데이터가 정제된 게 아니라서, 이걸 정제하고 이 코드를 돌리면 될 듯

(freq_table <- as.data.frame(table(cluster_0$카드사용액)))
nrow(freq_table)
sum(freq_table$Freq)

ggplot(freq_table, aes(x = as.factor(Var1), y = Freq)) +
  geom_bar(stat = "identity", fill = "#b22222") +
  labs(x = "Card Usage Category", y = "Frequency", title = "Frequency by Card Usage Category") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

################################################################
# 필요한 라이브러리 로드
library(ggplot2)

# result 데이터의 col1에서 1부터 60까지의 숫자가 몇 번 나오는지 빈도 계산
freq_table <- table(factor(result$col1, levels = 1:60))

# 빈도를 데이터프레임으로 변환
freq_df <- data.frame(Number = as.numeric(names(freq_table)), Frequency = as.numeric(freq_table))

# y축의 과학적 표기법 비활성화 및 막대그래프 생성
ggplot(freq_df, aes(x = Number, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_y_continuous(labels = scales::comma) +  # 과학적 표기법 비활성화
  labs(x = "Number (1-60)", y = "Frequency", title = "Frequency of Numbers in col1 (1-60)") +
  theme_minimal()

##################################################3
# 동일한 '순번'과 'col7' 값을 가진 데이터를 apps 데이터프레임에서 추출
result <- apps[apps$col7 %in% cluster_0$순번, ]

# 추출된 결과를 확인
print(result)

###apps 데이터가 정제된 게 아니라서, 이걸 정제하고 이 코드를 돌리면 될 듯

# 필요한 라이브러리 로드
library(ggplot2)

# result 데이터의 col1에서 1부터 60까지의 숫자가 몇 번 나오는지 빈도 계산
freq_table <- table(factor(result$col1, levels = 1:60))

# 빈도를 데이터프레임으로 변환
freq_df <- data.frame(Number = as.numeric(names(freq_table)), Frequency = as.numeric(freq_table))

# y축의 과학적 표기법 비활성화 및 막대그래프 생성
ggplot(freq_df, aes(x = Number, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_y_continuous(labels = scales::comma) +  # 과학적 표기법 비활성화
  labs(x = "Number (1-60)", y = "Frequency", title = "Frequency of Numbers in col1 (1-60)") +
  theme_minimal()
#########################################################
# 데이터프레임의 각 열에서 결측치 개수 확인
colSums(is.na(cluster_0))

# 필요한 열만 추출
event_data <- cluster_0 %>%
  select(이벤트명, 이벤트타입, 이벤트ID, 클릭버튼)

# 각 변수의 빈도수 확인
table(event_data$이벤트명)
table(event_data$이벤트타입)
table(event_data$이벤트ID)
table(event_data$클릭버튼)


# 이벤트타입, 클릭버튼, 이벤트ID 간의 조합 빈도수 계산
event_combination <- event_data %>%
  count(이벤트타입, 클릭버튼, 이벤트ID) %>%
  arrange(desc(n))

# 상위 10개 조합 출력
head(event_combination, 10)



##################################
## 시간대별 빈도 수 확인 : 10시와 11시에 집중되어 있다.
df <- cluster_0
# '기준시간'을 datetime 형식으로 변환
df$기준시간 <- as.POSIXct(df$기준시간, format = "%H:%M:%S")
# '기준시간'에서 시간대(hour)만 추출하여 'time' 변수 생성
cluster_0$time <- format(df$기준시간, "%H")
# 결과 확인
head(cluster_0[, c("기준시간", "time")])


# 'time' 변수의 빈도 수 계산
time_counts <- table(cluster_0$time)

# 데이터프레임으로 변환
time_counts_df <- as.data.frame(time_counts)
colnames(time_counts_df) <- c("Hour", "Frequency")

# 그래프 그리기 (막대 그래프)
ggplot(time_counts_df, aes(x = Hour, y = Frequency)) +
  geom_bar(stat = "identity", fill = "#b22222") +
  labs(title = "Cluster0_Frequency by Hour of the Day",
       x = "Hour of the Day",
       y = "Frequency") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.major.y = element_line(linetype = "dashed", color = "gray70")) +  # y축에만 격자선 추가
  theme(plot.title = element_text(hjust = 0.5))  # 제목을 가운데 정렬

# 그래프 그리기 (라인 그래프)
ggplot(time_counts_df, aes(x = Hour, y = Frequency, group = 1)) +  # group = 1 추가
  geom_line(color = "#b22222", linewidth = 1) +   # 선 그래프에서 linewidth 사용
  geom_point(color = "#b22222", size = 2) +  # 데이터 포인트 추가
  labs(title = "Cluster0_Frequency by Hour of the Day",
       x = "Hour of the Day",
       y = "Frequency") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(panel.grid.major.y = element_line(linetype = "dashed", color = "gray70")) +  # y축에 격자선 추가
  theme(plot.title = element_text(hjust = 0.5))  # 제목을 가운데 정렬

# 이벤트별 빈도 수 확인
# 이벤트 타입별 빈도수 계산
event_counts <- table(cluster_0$이벤트타입)

# 데이터프레임으로 변환
event_counts_df <- as.data.frame(event_counts)
colnames(event_counts_df) <- c("Event_Type", "Frequency")

# 그래프 그리기 (선 그래프)
ggplot(event_counts_df, aes(x = Event_Type, y = Frequency, group = 1)) +
  geom_line(color = "#b22222", linewidth = 1.2) +   # 다크 레드 색상으로 선 그래프
  geom_point(color = "#b22222", size = 3) +          # 포인트 추가
  labs(title = "Cluster0_Frequency by Event Type (Line Plot)",
       x = "Event Type",
       y = "Frequency") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # x축 레이블 45도 회전
  theme(panel.grid.major.y = element_line(linetype = "dashed", color = "gray70")) +  # y축에 격자선 추가
  theme(plot.title = element_text(hjust = 0.5))  # 제목을 가운데 정렬

# cluster0내 성별과 나이 범주에 대한 그래프
# 연령대와 성별별 빈도수 계산
age_gender_counts <- as.data.frame(table(cluster_0$연령, cluster_0$성별))

# 열 이름 수정
colnames(age_gender_counts) <- c("Age_Group", "Gender", "Frequency")

# 막대 그래프 그리기
ggplot(age_gender_counts, aes(x = Age_Group, y = Frequency, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +  # 막대 그래프
  labs(title = "Distribution of Age Groups and Gender in Cluster0",
       x = "Age Group",
       y = "Frequency") +
  scale_fill_manual(values = c("#d2b48c", "#b22222")) +  # 추출된 색상으로 지정
  theme_minimal(base_size = 10) +  # 깔끔한 테마
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # x축 레이블 45도 회전
  plot.title = element_text(hjust = 0.5))  # 제목 가운데 정렬




################통계량
# 범주형 변수들의 빈도수 확인
summary(cluster_0)

# '성별' 변수를 예시로 각 범주형 변수의 빈도수 계산
table(cluster_0$카드사용액)

# 다른 범주형 변수들에 대한 빈도수 계산 (예: 연령, 이벤트명 등)
table(cluster_0$연령)
table(cluster_0$이벤트명)


#################
# 다시 시각화
###########################################################################3

library(ggplot2)

# 카드 사용액 범주의 빈도 계산
freq_table <- as.data.frame(table(cluster_0$카드사용액))

# 막대 그래프 생성
ggplot(freq_table, aes(x = as.factor(Var1), y = Freq)) +
  geom_bar(stat = "identity", fill = "#b22222") +
  labs(x = "카드 사용액 범주", y = "빈도수", title = "카드 사용액 범주별 빈도") +
  theme_minimal()



#### 주소지 분포 확인
# 필요한 패키지 로드
#install.packages("ggmap")
install.packages("sf")
install.packages("kormaps2014")
library(sf)           # 지도 데이터 처리
library(kormaps2014)  # 한국 지도 데이터
library(ggspatial)    # 지도에 추가 기능 제공
library(ggmap)
library(stringr)
library(dplyr)

# 구 추출 및 빈도수 계산
data <- cluster_0 %>%
  mutate(구 = str_extract(주소지, "부산\\s(.*)")) %>%
  group_by(구) %>%
  summarise(빈도수 = n()) %>%
  ungroup()
head(data)

#막대그래프 그리기
ggplot(data, aes(x = 구, y = 빈도수, group = 1)) +  # group = 1 추가
  geom_bar(stat = "identity", fill = "#b22222") +
  labs(title = "Cluster0_Frequency by address",
       x = "Adress",
       y = "Frequency") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(panel.grid.major.y = element_line(linetype = "dashed", color = "gray70")) + # y축에 격자선 추가
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # 제목 가운데 정렬
        axis.title.x = element_text(hjust = 0.5),  # x축 레이블 가운데 정렬
        axis.title.y = element_text(hjust = 0.5),  # y축 레이블 가운데 정렬
        axis.text.x = element_text(angle = 45, hjust = 1))  # x축 라벨 각도 조정


##################################################33
#### 주소지 분포 확인
# 필요한 패키지 로드
#install.packages("ggmap")
# dplyr 패키지 설치 및 로드
install.packages("dplyr")
library(dplyr)

# stringr 패키지 설치 및 로드
install.packages("stringr")
library(stringr)

library(ggmap)
library(stringr)
library(dplyr)

# 구 추출 및 빈도수 계산
data <- cluster_0 %>%
  mutate(구 = str_extract(주소지, "부산\\s(.*)")) %>%
  group_by(구) %>%
  summarise(빈도수 = n()) %>%
  ungroup()
head(data)

# 부산 구가 아닌 다른 변수 출력하기
부산_구_아님 <- cluster_0 %>%
  filter(!str_detect(주소지, "부산"))  # '부산'이 포함되지 않은 데이터 필터링

# 결과 확인
print(부산_구_아님$주소지)

#막대그래프 그리기
ggplot(data, aes(x = 구, y = 빈도수, group = 1)) +  # group = 1 추가
  geom_bar(stat = "identity", fill = "#b22222") +
  labs(title = "Cluster2_Frequency by address",
       x = "Address",
       y = "Frequency") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(panel.grid.major.y = element_line(linetype = "dashed", color = "gray70")) + # y축에 격자선 추가
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # 제목 가운데 정렬
        axis.title.x = element_text(hjust = 0.5),  # x축 레이블 가운데 정렬
        axis.title.y = element_text(hjust = 0.5),  # y축 레이블 가운데 정렬
        axis.text.x = element_text(angle = 45, hjust = 1))  # x축 라벨 각도 조정
