#读取数据集
raw_bankchurners <- read.csv("D:/Research_Warton/BankChurners.csv")

# 复制数据集
data <- raw_bankchurners

# 去除CLIENTNUM列和最后一列
data <- subset(raw_bankchurners, select = -c(CLIENTNUM, 23))

# 检查空值
na_count <- sum(is.na(data))
# 去除含有空值的行
new_data <- data[complete.cases(data),]

# 删除包含'Unknown'的行
unknown_rows <- grepl("Unknown", data$Marital_Status) | grepl("Unknown", data$Education_Level) | grepl("Unknown", data$Income_Category) | grepl("Unknown", data$Card_Category)

data <- subset(data, !unknown_rows)


data <- subset(data, select = -c(21))

# 将'Existing Customer'替换为1，'Attrited Customer'替换为0
data$Attrition_Flag <- ifelse(data$Attrition_Flag == 'Existing Customer', 1, 0)

# 将'Gender'中的'M'替换为0，'F'替换为1
data$Gender <- ifelse(data$Gender == 'M', 0, 1)

# 将'Uneducated'替换为0，'High School'替换为1，'College'替换为2，'Graduate'替换为3，'Post-Graduate'替换为4，'Doctorate'替换为5
data$Education_Level <- ifelse(data$Education_Level == 'Uneducated', 0,
                               ifelse(data$Education_Level == 'High School', 1,
                                      ifelse(data$Education_Level == 'College', 2,
                                             ifelse(data$Education_Level == 'Graduate', 3,
                                                     ifelse(data$Education_Level == 'Post-Graduate', 4, 5)))))

# 将'Marital_Status'中的'S'替换为0，'M'替换为1
data$Marital_Status <- ifelse(data$Marital_Status == 'Married', 0, 1)

# 将'Card_Category'中的'Blue'替换为0，'Silver'替换为1,'Gold'替换成2
data$Card_Category <- ifelse(data$Card_Category == 'Blue', 0,
                               ifelse(data$Card_Category == 'Sliver', 1, 2))

# 将'Income_Category'中的值替换为对应数字
data$Income_Category <- ifelse(data$Income_Category == 'Less than $40K', 20,
                               ifelse(data$Income_Category == '$40K - $60K', 50,
                                      ifelse(data$Income_Category == '$60K - $80K', 70,
                                             ifelse(data$Income_Category == '$80K - $120K', 100, 120))))









# 安装并加载cluster、ggplot2包
if (!requireNamespace("cluster", quietly = TRUE)) {
  install.packages("cluster")
}
library(cluster)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# 使用PCA降维
pca <- prcomp(data, scale. = TRUE)
pca_data <- as.data.frame(pca$x[, 1:2]) # 取前两个主成分

# 计算距离矩阵
dist_matrix <- dist(data)

# 进行层次聚类
hc <- hclust(dist_matrix, method = "average")

# 选择不同的聚类数量进行切割
k_values <- 2:10
sil_width <- numeric(length(k_values))

# 计算不同切割点的轮廓系数
for (i in seq_along(k_values)) {
  k <- k_values[i]
  cluster_labels <- cutree(hc, k)
  sil <- silhouette(cluster_labels, dist_matrix)
  sil_width[i] <- mean(sil[, 3])
}

# 找到最佳切割点（轮廓系数最大）
best_k <- k_values[which.max(sil_width)]
cat("Best number of clusters:", best_k, "\n")

# 使用最佳切割点进行聚类
best_cluster_labels <- cutree(hc, best_k)

# 将聚类结果添加到降维后的数据集
pca_data$best_cluster_labels <- as.factor(best_cluster_labels)

# 可视化聚类结果
ggplot(pca_data, aes(x = PC1, y = PC2, color = best_cluster_labels)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  ggtitle("Hierarchical Clustering with PCA Visualization")