# 필요한 패키지 로드
library(dplyr)
library(arules)
library(dplyr)
library(arulesViz)
library(MASS)

# 필요한 패키지 로드
library(caret)
library(arules)
library(dplyr)
install.packages("randomForest")
library(randomForest)

# 데이터 불러오기
data_1 <- read.csv("modified_data.csv")

# 종속 변수와 독립 변수 분리
x_columns <- data_1 %>% select(-성별코드)
y_columns <- data_1$성별코드

# 변수 선택 수행
ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10)
result <- rfe(x_columns, y_columns, sizes=c(1:10), rfeControl=ctrl)

# 선택된 변수 확인
selected_variables <- names(result$optVariables)

# 선택된 변수로 데이터 프레임 생성
selected_data <- data_1 %>% select(c("성별코드", selected_variables))

# 선택된 변수들로 연관규칙분석 수행
transactions <- as(selected_data, "transactions")
rules <- apriori(transactions, parameter = list(supp = 0.1, conf = 0.8))

rules_sort <- sort(rules, by = "lift", decreasing = TRUE)

# 결과 출력
inspect(rules_sort)





# 선택한 변수들만으로 데이터프레임 생성
selected_columns <- c( '성별코드', '연령대코드.5세단위.', '시도코드', '신장.5Cm단위.', '체중.5Kg.단위.', '허리둘레', '청력.좌.', '청력.우.', '수축기혈압', '이완기혈압', '식전혈당.공복혈당.', '총콜레스테롤', '흡연상태')
data_selected <- data_1[selected_columns]

# 명목형 변수 더미 변수로 변환
nominal_columns <- c('성별코드','청력.우.','청력.좌.', '성별코드')
data_selected_dummies <- as.data.frame(lapply(data_selected[nominal_columns], as.factor))

# 나머지 변수는 이산화 (discretization)
continuous_columns <- c('연령대코드.5세단위.', '시도코드', '신장.5Cm단위.', '체중.5Kg.단위.', '허리둘레', '수축기혈압', '이완기혈압', '식전혈당.공복혈당.', '총콜레스테롤')
data_selected_discretized <- discretizeDF(data_selected[continuous_columns])

# 명목형 변수와 이산화된 연속형 변수를 합치기
data_for_analysis <- cbind(data_selected_dummies, data_selected_discretized)

# 데이터 변환: 연관규칙분석은 transaction 형태의 데이터를 사용하므로 데이터를 변환합니다.
transactions <- as(data_for_analysis, "transactions")

# 연관규칙 분석 수행
rules <- apriori(transactions, parameter = list(supp = 0.5, conf = 0.9))

# 결과 출력
fix_rules <- sort(rules, by = "lift", decreasing = TRUE)

# 결과 출력
summary(fix_rules)



# 생성된 규칙 시각화
plot(rules)



