# Regression
# read in the data
raw_bankchurners<-read.csv('/Users/Martin/Desktop/Wharton - Business Analytics/project/BankChurners.csv')

# 复制数据集
data <- raw_bankchurners

# 去除CLIENTNUM列和最后一列
data <- subset(raw_bankchurners, select = -c(CLIENTNUM, 23))

# 检查空值
na_count <- sum(is.na(data))
# 去除含有空值的行
new_data <- data[complete.cases(data),]

summary(data)

data <- subset(data, select = -c(21))

# 将'Existing Customer'替换为1，'Attrited Customer'替换为0
data$Attrition_Flag <- ifelse(data$Attrition_Flag == 'Existing Customer', 1, 0)

# 将'Gender'中的'M'替换为0，'F'替换为1
data$Gender <- ifelse(data$Gender == 'M', 0, 1)

# 将'Income_Category'中的值替换为对应数字
data$Income_Category <- ifelse(data$Income_Category == 'Less than $40K', 20,
                               ifelse(data$Income_Category == '$40K - $60K', 50,
                                      ifelse(data$Income_Category == '$60K - $80K', 70,
                                             ifelse(data$Income_Category == '$80K - $120K', 100, 120))))

# 删除包含'Unknown'的行
unknown_rows <- grepl("Unknown", data$Marital_Status) | grepl("Unknown", data$Education_Level) | grepl("Unknown", data$Income_Category) | grepl("Unknown", data$Card_Category)

data <- subset(data, !unknown_rows)


# 查看每列的唯一值
sapply(data, function(x) length(unique(x)))

table(data$Education_Level)

# 选择需要进行哑变量编码的变量
categorical_vars <- c("Education_Level", "Marital_Status", "Card_Category")

# 通过model.matrix()函数创建哑变量矩阵
dummy_vars <- model.matrix(~.-1, data = data[, categorical_vars])
colnames(dummy_vars) <- sub("^[^_]+_([^\\.]+)\\..*$", "\\1", colnames(dummy_vars))

# 将哑变量矩阵和原始数据集中的其他变量合并
data_withdummy <- cbind(data[, !names(data) %in% categorical_vars], dummy_vars)


#### Normalization


# 确定需要归一化的列索引
columns_to_normalize <- 2:17

# 对指定列进行归一化处理
data_withdummy[, columns_to_normalize] <- scale(data_withdummy[, columns_to_normalize])

# 输出归一化后的数据集
print(data_withdummy)




install.packages("xlsx")
install.packages("broom")

library(xlsx)
library(broom)

# 拟合回归模型
fit <- glm(formula = y ~ x1 + x2, data = mydata, family = binomial)

# 将回归结果保存到Excel文件

fit_summary <- tidy(fit_g)
write.xlsx(fit_summary, file = "regression_results.xlsx", sheetName = "Summary")

##

fit_summary <- tidy(fit_g_final)
write.xlsx(fit_summary, file = "regression_results_final.xlsx", sheetName = "Summary")


###### glm

# 将Attrition_Flag作为因变量，其余变量作为自变量
fit_g <- glm(Attrition_Flag ~. , data = data_withdummy)

# 输出回归结果
summary(fit_g)

fit_g_final <- step(fit_g)

AIC(fit_g_final)
BIC(fit_g_final)

summary(fit_g_final)



# 获取回归结果摘要信息
summary_fit <- summary(fit)

# 查看每个自变量的p值
p_values <- summary_fit$coefficients[, 4]

# 筛选不显著的自变量
not_significant_vars <- names(p_values[p_values > 0.05])

# 输出不显著自变量名称
not_significant_vars

class(not_significant_vars)
summary(not_significant_vars)

data_copy_g <- data_withdummy[, !names(data_withdummy) %in% not_significant_vars]
summary(data_copy)

# 将Attrition_Flag作为因变量，其余显著变量作为自变量
fit_new_g <- lm(Attrition_Flag ~. , data = data_copy_g)

# 输出回归结果
summary(fit_new_g)

colnames(data_copy_g)[15] <- "Education_LevelHigh_School"
colnames(data_copy_g)[16] <- "Education_LevelPost_Graduate"

# 用显著的自变量再做一次回归
fit2_g <- glm(Attrition_Flag ~ . - Avg_Open_To_Buy - Education_LevelHigh_School - Education_LevelPost_Graduate - Education_LevelUneducated, data = data_copy_g)

summary(fit2_g) # 查看新的回归分析结果

AIC(fit2_g)
BIC(fit2_g)