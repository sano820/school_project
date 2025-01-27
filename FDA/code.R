install.packages("gridExtra")

library(jsonlite)
library(ggplot2)
library(gganimate)
library(dplyr)
library(corrplot)
library(fda)
library(gridExtra) 
library(cluster)

json_data <- fromJSON("C:/Users/박상선의 노트북/OneDrive/바탕 화면/확률모형캡스톤디자인/whereable_dataset/data.json", flatten = FALSE)

# 데이터프레임으로 변환 시도
df <- as.data.frame(json_data)

jdata <- df %>%
  dplyr::select(timestamp, hashedEmail, deviceType, valueType, value)

# 리스트를 포함하는 열 확인
list_cols <- sapply(jdata, is.list)
list_columns <- names(list_cols[list_cols == TRUE])

# 리스트를 문자열로 변환
for (col in list_columns) {
  jdata[[col]] <- sapply(jdata[[col]], function(x) paste(toString(x), collapse = ","))
}

  
# CSV 파일로 저장
csv_file <- "C:/Users/박상선의 노트북/OneDrive/바탕 화면/확률모형캡스톤디자인/whereable_dataset/j_data.csv"
write.csv(jdata, csv_file, row.names = FALSE)

data <- read.csv("C:/Users/박상선의 노트북/OneDrive/바탕 화면/확률모형캡스톤디자인/whereable_dataset/j_data.csv")
str(data)
# 밀리초 단위를 초 단위로 변환
data$timestamp <- as.POSIXct(data$timestamp / 1000, origin = "1970-01-01", tz = "UTC")

# 변환된 데이터 확인
summary(data$timestamp)

data$value <- as.numeric(as.character(data$value))

# value 열의 원래 상태 확인
unique_values <- unique(data$value)  # value 열에서 고유 값 확인
print("Unique values in 'value' column:")
print(unique_values)

# value 열을 숫자로 변환 (변환 실패 값 확인)
data$value <- as.numeric(as.character(data$value))

# NA 값 확인
na_values <- data[is.na(data$value), ]
print("Rows with NA values in 'value' column:")
print(na_values)

# 또는 NA를 특정 값(예: 0)으로 대체
data$value[is.na(data$value)] <- 0

data$value <- as.numeric(as.character(data$value))


table(data$valueType)

# 모든 unique valueType 추출
value_types <- unique(data$valueType)

# 각 valueType 별로 데이터 필터링, 스무딩 및 시각화
plots <- list()
for (value_type in value_types) {
  # 데이터 필터링 및 스무딩
  type_data <- data %>%
    filter(valueType == value_type) %>%
    group_by(hashedEmail) %>%
    filter(n() >= 4) %>%  # 데이터 포인트 수가 4개 이상인 그룹만 유지
    do({
      num_points <- nrow(.)
      nbasis <- max(4, min(20, num_points - 1))  # nbasis는 최소 4 이상이어야 함
      basis <- create.bspline.basis(range = range(.$timestamp), nbasis = nbasis)
      smooth_fit <- smooth.basis(.$timestamp, .$value, fdParobj = basis)
      data.frame(timestamp = .$timestamp, smooth = eval.fd(.$timestamp, smooth_fit$fd))
    }) %>%
    ungroup()
  
  # 시각화
  plot <- ggplot(type_data, aes(x = timestamp, y = smooth, color = hashedEmail)) +
    geom_line() +
    theme_minimal() +
    ggtitle(paste(value_type, "by hashedEmail"))
  
  plots[[value_type]] <- plot
}

# 각 그래프를 JPEG 파일로 저장
for (value_type in names(plots)) {
  # 파일 경로와 이름 설정
  file_path <- paste("C:/Users/박상선의 노트북/OneDrive/바탕 화면/확률모형캡스톤디자인/whereable_dataset/graph/", value_type, ".jpg", sep = "")
  
  # 그래프 저장
  ggsave(file_path, plot = plots[[value_type]], width = 20, height = 8, dpi = 300)
}

#-------------------
# # 'DAILY_CALORIES' 데이터 필터링 및 스무딩
# calories_data <- data %>%
#   filter(valueType == "DAILY_CALORIES") %>%
#   group_by(hashedEmail) %>%
#   filter(n() >= 4) %>%  # 데이터 포인트 수가 4개 이상인 그룹만 유지
#   do({
#     num_points <- nrow(.)
#     nbasis <- max(4, min(20, num_points - 1))  # nbasis는 최소 4 이상이어야 함
#     basis <- create.bspline.basis(range = range(.$timestamp), nbasis = nbasis)
#     smooth_fit <- smooth.basis(.$timestamp, .$value, fdParobj = basis)
#     data.frame(timestamp = .$timestamp, smooth = eval.fd(.$timestamp, smooth_fit$fd))
#   }) %>%
#   ungroup()
# 
# # 'DAILY_DISTANCE' 데이터 필터링 및 스무딩
# distance_data <- data %>%
#   filter(valueType == "DAILY_DISTANCE") %>%
#   group_by(hashedEmail) %>%
#   filter(n() >= 4) %>%  # 데이터 포인트 수가 4개 이상인 그룹만 유지
#   do({
#     num_points <- nrow(.)
#     nbasis <- max(4, min(20, num_points - 1))  # nbasis는 최소 4 이상이어야 함
#     basis <- create.bspline.basis(range = range(.$timestamp), nbasis = nbasis)
#     smooth_fit <- smooth.basis(.$timestamp, .$value, fdParobj = basis)
#     data.frame(timestamp = .$timestamp, smooth = eval.fd(.$timestamp, smooth_fit$fd))
#   }) %>%
#   ungroup()
# 
# 
# 
# # 각각의 데이터 시각화
# calories_plot <- ggplot(calories_data, aes(x = timestamp, y = smooth, color = hashedEmail)) +
#   geom_line() +
#   theme_minimal() +
#   ggtitle("DAILY_CALORIES by hashedEmail")
# 
# distance_plot <- ggplot(distance_data, aes(x = timestamp, y = smooth, color = hashedEmail)) +
#   geom_line() +
#   theme_minimal() +
#   ggtitle("DAILY_DISTANCE by hashedEmail")
# 
# print(calories_plot)
# print(distance_plot)
# 
#-------------------------

# 동일한 기저 함수 생성
basis <- create.bspline.basis(rangeval = range(data$timestamp), nbasis = 20)

# 지표별 함수형 데이터 생성 함수
create_fd <- function(data_split, global_basis) {
  lapply(data_split, function(group_data) {
    num_points <- nrow(group_data)
    if (num_points >= 4) {  # 데이터 포인트가 4개 이상인 경우 처리
      nbasis <- min(20, num_points - 1)
      local_basis <- create.bspline.basis(rangeval = range(group_data$timestamp), nbasis = nbasis)
      smooth.basis(
        argvals = group_data$timestamp,
        y = group_data$value,
        fdParobj = local_basis
      )$fd
    } else {
      NULL  # 데이터 포인트가 부족한 경우 NULL 반환
    }
  })
}

# 각 지표별로 데이터 분리
split_data <- function(value_type, data) {
  filtered_data <- data %>%
    filter(valueType == value_type) %>%
    group_by(hashedEmail) %>%
    group_split()
  if (length(filtered_data) == 0) {
    stop(paste("No data found for valueType:", value_type))
  }
  return(filtered_data)
}

# 'DAILY_CALORIES' 및 'DAILY_DISTANCE'를 예제로 fd 객체 생성
calories_split <- split_data("DAILY_CALORIES", data)
calories_fd_list <- create_fd(calories_split, basis)

distance_split <- split_data("DAILY_DISTANCE", data)
distance_fd_list <- create_fd(distance_split, basis)

# FD 리스트에서 NULL 제거
calories_fd_list <- Filter(Negate(is.null), calories_fd_list)
distance_fd_list <- Filter(Negate(is.null), distance_fd_list)

# FD 객체 병합
calories_coefs <- do.call(cbind, lapply(calories_fd_list, function(fd_obj) fd_obj$coefs))
distance_coefs <- do.call(cbind, lapply(distance_fd_list, function(fd_obj) fd_obj$coefs))

# FD 객체 생성
calories_fd_combined <- fd(coefs = calories_coefs, basisobj = basis)
distance_fd_combined <- fd(coefs = distance_coefs, basisobj = basis)

# # FCCA 수행
# canonical_result <- cca.fd(calories_fd_combined, distance_fd_combined)
# 
# # FCCA 결과 요약 및 시각화
# summary(canonical_result)
# plot(canonical_result, main = "Canonical Correlation Analysis")

# FLM 수행
flm_result <- fRegress(distance_fd_combined ~ calories_fd_combined)

# FLM 결과 요약 및 시각화
summary(flm_result)

# 회귀 계수 함수 시각화
plot(flm_result$betaestlist[[1]], main = "Regression Coefficient Function")

# # FPCA 수행
# calories_pca <- pca.fd(calories_fd_combined, nharm = 3)
# distance_pca <- pca.fd(distance_fd_combined, nharm = 3)
# 
# # FPCA 결과 요약 및 시각화
# summary(calories_pca)
# summary(distance_pca)
# 
# # 주성분 함수 시각화
# plot.pca.fd(calories_pca, main = "FPCA for DAILY_CALORIES")
# plot.pca.fd(distance_pca, main = "FPCA for DAILY_DISTANCE")
# 
# # FPCA 점수를 활용한 클러스터링
# library(cluster)
# 
# # 첫 번째와 두 번째 주성분 점수 추출
# calories_scores <- as.data.frame(calories_pca$scores[, 1:2])
# distance_scores <- as.data.frame(distance_pca$scores[, 1:2])
# 
# # 클러스터링 수행 (k-means)
# kmeans_result <- kmeans(calories_scores, centers = 3)
# 
# # 클러스터 시각화
# calories_scores$cluster <- as.factor(kmeans_result$cluster)
# ggplot(calories_scores, aes(x = V1, y = V2, color = cluster)) +
#   geom_point(size = 2) +
#   labs(title = "Clustering based on FPCA Scores", x = "PC1", y = "PC2") +
#   theme_minimal()
