getwd()
# 데이터 불러오기
data <- read.csv("data.csv")

# 데이터 구조 확인
str(data)

library(dplyr)

# 결측값 확인
miss_var_summary <- data %>%
  summarise_all(~sum(is.na(.)))
print(miss_var_summary)


# boxplot 그리기
par(mfrow=c(2,2))
boxplot(data$시력.우., data = data, main = "시력.우.",  ylab = "Value")
boxplot(data$시력.좌. , data = data, main = "시력.좌.",  ylab = "Value")
boxplot(data$LDL콜레스테롤 , data = data, main = "LDL콜레스테롤",  ylab = "Value")
boxplot(data$요단백, data = data, main = "요단백", ylab = "Value")

# 결측치 대체
data$시력.좌. <- ifelse(is.na(data$시력.좌.), mean(data$시력.좌., na.rm = TRUE), data$시력.좌.)
data$시력.우. <- ifelse(is.na(data$시력.우.), mean(data$시력.우., na.rm = TRUE), data$시력.우.)

# 요단백 열의 최빈값 계산
table_result <- table(data$요단백)
mode_value <- as.numeric(names(table_result)[which.max(table_result)])

print(paste("Mode of 요단백:", mode_value))

# 요단백 열의 결측값을 1로 채우기
data$요단백 <- replace(data$요단백, is.na(data$요단백), 1)


# LDL콜레스테롤 열의 결측값을 중앙값으로 대체
ldl_cholesterol_median <- median(data$LDL콜레스테롤, na.rm = TRUE)

print(ldl_cholesterol_median)
data$LDL콜레스테롤 <- ifelse(is.na(data$LDL콜레스테롤), ldl_cholesterol_median, data$LDL콜레스테롤)


# 흡연여부 열의 최빈값 계산
smoking_result <- table(data$흡연상태)
smoking_value <- as.numeric(names(smoking_result)[which.max(smoking_result)])

print(paste("Mode of 흡연상태:", smoking_value))

# 흡연상태 열의 결측값을 1로 채우기
data$흡연상태 <- replace(data$흡연상태, is.na(data$흡연상태), 1)


data <- data[-c(1, 2)]


# 수정된 데이터 저장
write.csv(data, "modified_data.csv", row.names = TRUE, fileEncoding = "UTF-8")

data$당뇨병 <- ifelse(data$식전혈당.공복혈당. >= 126, 1, 0)

# "식전혈당.공복혈당." 변수 제거
data <- subset(data, select = -c(식전혈당.공복혈당.))

# 수정된 데이터 저장
write.csv(data, "modified_data1.csv", row.names = TRUE, fileEncoding = "UTF-8")


# 필요한 패키지 로드
library(rpart)
library(rpart.plot)
library(ggplot2)
library(reshape2)
library(Hmisc)


# 시력 변수 생성 후 데이터 불러오기
data_0 <- read.csv("modified_data.csv")

head(data_0)                     
str(data_0)

# 결측값 확인
miss_var_summary_0 <- data_0 %>%
  summarise_all(~sum(is.na(.)))
print(miss_var_summary_0)

# 훈련 데이터와 테스트 데이터로 나누기
set.seed(000)  # 재현성을 위한 시드 설정
train_index2 <- sample(1:nrow(data_0), 0.8 * nrow(data_0))  # 80%를 훈련 데이터로 사용
train_data2 <- data_0[train_index2, ]
test_data2 <- data_0[-train_index2, ]

# 모델 구축 - 예측 변수가 "성별코드"인 경우
model2 <- rpart(성별코드~ ., data = train_data2, method="class")
model2

#의사결정나무 시각화
rpart.plot(model2)

# 모델 재구축 및 평가
predictions2 <- predict(model2, test_data2, type = "class")

# 혼동 행렬 출력
conf_matrix2 <- table(predictions2, test_data2$성별코드)
print(conf_matrix2)

# 정확도 계산
accuracy2 <- sum(diag(conf_matrix2)) / sum(conf_matrix2)
print(paste("Accuracy:", accuracy2))

# 민감도 계산
sensitivity2 <- conf_matrix2[2, 2] / sum(conf_matrix2[2, ])
print(paste("Sensitivity (True Positive Rate):", sensitivity2))

# 특이도 계산
specificity2 <- conf_matrix2[1, 1] / sum(conf_matrix2[1, ])
print(paste("Specificity (True Negative Rate):", specificity2))


# 정확도 이외의 다른 평가 지표 계산
precision2 <- conf_matrix2[2, 2] / sum(conf_matrix2[, 2])
recall2 <- sensitivity2
f1_score2 <- 2 * (precision2 * recall2) / (precision2 + recall2)

print(paste("Precision:", precision2))
print(paste("Recall (Sensitivity):", recall2))
print(paste("F1 Score:", f1_score2))

# ROC 곡선 및 AUC 계산
library(pROC)             # pROC 패키지 로드

roc_curve2 <- roc(test_data2$성별코드, as.numeric(predictions2))
auc_value2 <- auc(roc_curve2)

# ROC 곡선 시각화
plot(roc_curve2, main = "ROC Curve", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "red")  # 랜덤 수준의 성능을 나타내는 대각선
legend("bottomright", legend = paste("AUC =", round(auc_value2, 3)), col = "blue", lty = 1)



summary(model2)

# 변수 중요도 확인
printcp(model2)

# 의사결정나무 뿌리 부분 해석
root_node <- model2$frame[1, ]
print("Root Node:")
print(root_node)

# 변수 중요도 확인
var_importance2 <- model2$variable.importance
print("Variable Importance:")
print(var_importance2)


plot(model2)
text(model2, all=T)




