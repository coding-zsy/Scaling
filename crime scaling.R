library(ggplot2)
library(plyr)
library(ggbreak)
library(ggforce)
library(ggpubr)
library(ggpmisc)
library(tidyr)
library(dplyr)

theme_set(theme_bw())
setwd ("D:/output")


#============================模块一：频数分布图=============================


urtdata <- read.csv("D:/A大创/印度犯罪标度律/A三国/excel数据/america_rate.csv", 
                    header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空


# 假设 urtdata 已经读取，并包含 mdrt 和 ltfrt 两列
# 将数据转换为长格式
urt_long <- urtdata %>%
  select(bgrt, ltfrt) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# 计算中位数
median_values <- urt_long %>%
  group_by(Variable) %>%
  summarize(median_val = round(median(Value, na.rm = TRUE), 2))

# 创建标签字符串
median_labels <- paste0(median_values$Variable, ": ", median_values$median_val)

# 生成替代的显示名称（用于图例和标注）
name_map <- c("bgrt" = "Burglary", "ltfrt" = "LarcenyTheft")



# 使用 name_map 替换变量名，生成用于 annotate 的标签
median_labels <- paste0(name_map[median_values$Variable], ": ", median_values$median_val)

urt <- ggplot(urt_long, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5, color = "black",size=0.35) +  # 密度曲线填充并加黑边
  geom_vline(data = median_values, aes(xintercept = median_val, color = Variable),
             linetype = "dashed", size = 0.35, show.legend = FALSE) +  # 添加中位数虚线
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(0, 4500)) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = paste("Median\n", paste(median_labels, collapse = "\n")), 
           hjust = 1.3, vjust = 1.5, size = 1.8) +  # 添加右上角中位数注释
  #  scale_fill_manual(values = c("bgrt" = "#F4A582", "ltfrt" = "#92C5DE"),
  #                   labels = name_map) +  # 修改图例显示名称
  #scale_color_manual(values = c("bgrt" = "#B2182B", "ltfrt" = "#2166AC"),
  #                  labels = name_map) +  # 修改虚线颜色图例标签
  scale_fill_manual(values = c("bgrt" = "#E7A3A3", "ltfrt" = "#8FAED1"),
                    labels = name_map) +  # 修改图例显示名称
  scale_color_manual(values = c("bgrt" = "#D86A6A", "ltfrt" = "#5967BB"),
                     labels = name_map) +  # 修改虚线颜色图例标签
  labs(x = "Crime Rate (per 100,000)", y = "Density",title = "United States (2022)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black"),
        axis.text=element_text(size=6), 
        axis.title.y=element_text(size=7),
        axis.title.x=element_text(size=7),
        plot.title = element_text(size = 8, hjust = 0.5),
        legend.position = c(0.7, 0.6),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 5))





frtdata <- read.csv("D:/A大创/印度犯罪标度律/A三国/excel数据/france_rate.csv", 
                    header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空


# 假设 urtdata 已经读取，并包含 mdrt 和 ltfrt 两列
# 将数据转换为长格式
frt_long <- frtdata %>%
  select(bgrt, ltfrt) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# 计算中位数
median_values2 <- frt_long %>%
  group_by(Variable) %>%
  summarize(median_val2 = round(median(Value, na.rm = TRUE), 2))

# 创建标签字符串
median_labels2 <- paste0(median_values2$Variable, ": ", median_values2$median_val2)

# 生成替代的显示名称（用于图例和标注）
name_map <- c("bgrt" = "Burglary", "ltfrt" = "Theft")

# 使用 name_map 替换变量名，生成用于 annotate 的标签
median_labels2 <- paste0(name_map[median_values2$Variable], ": ", median_values2$median_val2)

frt <- ggplot(frt_long, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5, color = "black",size=0.35) +  # 密度曲线填充并加黑边
  geom_vline(data = median_values2, aes(xintercept = median_val2, color = Variable),
             linetype = "dashed", size = 0.35, show.legend = FALSE) +  # 添加中位数虚线
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(0, 2500)) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = paste("Median\n", paste(median_labels2, collapse = "\n")), 
           hjust = 1.3, vjust = 1.5, size = 1.8) +  # 添加右上角中位数注释
  scale_fill_manual(values = c("bgrt" = "#E7A3A3", "ltfrt" = "#8FAED1"),
                    labels = name_map) +  # 修改图例显示名称
  scale_color_manual(values = c("bgrt" = "#D86A6A", "ltfrt" = "#5967BB"),
                     labels = name_map) +  # 修改虚线颜色图例标签
  labs(x = "Crime Rate (per 100,000)", y = "Density",title = "France (2024)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black"),
        axis.text=element_text(size=6), 
        axis.title.y=element_text(size=7),
        axis.title.x=element_text(size=7),
        plot.title = element_text(size = 8, hjust = 0.5),
        legend.position = c(0.75, 0.6),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 5))






irtdata <- read.csv("D:/A大创/印度犯罪标度律/A三国/excel数据/india_rate.csv", 
                    header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空


# 假设 urtdata 已经读取，并包含 mdrt 和 ltfrt 两列
# 将数据转换为长格式
irt_long <- irtdata %>%
  select(bgrt, ltfrt) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# 计算中位数
median_values3 <- irt_long %>%
  group_by(Variable) %>%
  summarize(median_val3 = round(median(Value, na.rm = TRUE), 2))

# 创建标签字符串
median_labels3 <- paste0(median_values3$Variable, ": ", median_values3$median_val3)

# 生成替代的显示名称（用于图例和标注）
name_map <- c("bgrt" = "Burglary", "ltfrt" = "Theft")

# 使用 name_map 替换变量名，生成用于 annotate 的标签
median_labels3 <- paste0(name_map[median_values3$Variable], ": ", median_values3$median_val3)

irt <- ggplot(irt_long, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5, color = "black", size = 0.35) +  # 密度曲线填充并加黑边
  geom_vline(data = median_values3, aes(xintercept = median_val3, color = Variable),
             linetype = "dashed", size = 0.35, show.legend = FALSE) +  # 添加中位数虚线
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(0, 130)) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = paste("Median\n", paste(median_labels3, collapse = "\n")), 
           hjust = 1.3, vjust = 1.5, size = 1.8) +  # 添加右上角中位数注释
  scale_fill_manual(values = c("bgrt" = "#E7A3A3", "ltfrt" = "#8FAED1"),
                    labels = name_map) +  # 修改图例显示名称
  scale_color_manual(values = c("bgrt" = "#D86A6A", "ltfrt" = "#5967BB"),
                     labels = name_map) +  # 修改虚线颜色图例标签
  labs(x = "Crime Rate (per 100,000)", y = "Density",title = "India (2021)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black"),
        axis.text=element_text(size=6), 
        axis.title.y=element_text(size=7),
        axis.title.x=element_text(size=7),
        plot.title = element_text(size = 8, hjust = 0.5),
        legend.position = c(0.75, 0.6),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 5))


tiff(file="图11 频数.tiff.tiff", res = 600, width = 3600, height = 1200, compression = "lzw")
ggarrange(urt,frt,irt,
          nrow = 1,ncol=3,
          labels = c( "a", "b","c"),
          #label.y = 1.025, label.x = -0.026, # 默认是 1，可以设置 >1 向上移动
          #heights = c(1, 1.4),  # 设置每个图的相对宽度比例
          align = "h",font.label = list(size = 11))
dev.off()
getwd()






###########################美国
calc_beta_ci <- function(data, crime_var){
  data <- subset(data, data[[crime_var]] > 0 & data$Population > 0)
  if(nrow(data) <= 5){
    return(c(0, 0, 0))
  } else {
    model <- lm(log10(data[[crime_var]]) ~ log10(Population), data = data)
    ci <- confint(model)
    return(c(coef(model)[2], ci[2,1], ci[2,2]))
  }
}

crimekinds <- c("Murder","Rape","Robbery","Aggravated","Burglary","LarcenyTheft","MotorTheft","Arson")
years <- 2001:2022
CrimeData <- read.csv("D:/A大创/印度犯罪标度律/A美国/picparts/American_Place_Crime_From_2001_to_2022.csv")

# 处理总犯罪
total_results <- do.call(rbind, lapply(years, function(y){
  year_data <- subset(CrimeData, Year == y)
  year_data$Crime <- year_data$Violent.crime + year_data$Property.crime
  c(y, calc_beta_ci(year_data, "Crime"))
}))
draw_total <- as.data.frame(total_results)
colnames(draw_total) <- c("Year", "beta", "CILower", "CIUpper")
draw_total$CrimeType <- "Total Crime"


# 多犯罪处理
draw_all <- draw_total

for (crime in crimekinds){
  crime_result <- do.call(rbind, lapply(years, function(y){
    year_data <- subset(CrimeData, Year == y)
    c(y, calc_beta_ci(year_data, crime))
  }))
  df <- as.data.frame(crime_result)
  colnames(df) <- c("Year", "beta", "CILower", "CIUpper")
  df$CrimeType <- crime
  draw_all <- rbind(draw_all, df)
}
# 数据选择
selected_data <- CrimeData[, c("State", "City", "Population", "Murder", "LarcenyTheft", "Year")]

# pivot
CrimeData_long <- selected_data %>%
  pivot_longer(cols = c(Murder, LarcenyTheft),
               names_to = "type",
               values_to = "case")

# 筛选 2022 年
CrimeData_long2 <- CrimeData_long %>%
  filter(Year == 2022,
         Population > 0,
         case > 0)

# 去除异常值（两倍标准差外）
remove_outliers <- function(df, fold = 2){
  df <- df %>% mutate(
    log_pop = log10(Population),
    log_case = log10(case)
  )
  model <- lm(log_case ~ log_pop, data = df)
  preds <- predict(model)
  resids <- df$log_case - preds
  res_mean <- mean(resids, na.rm = TRUE)
  res_sd <- sd(resids, na.rm = TRUE)
  df$keep <- resids > (res_mean - fold * res_sd) & resids < (res_mean + fold * res_sd)
  df_clean <- df[df$keep, ]
  return(df_clean)
}

# 对每类犯罪分别去异常值
CrimeData_cleaned <- CrimeData_long2 %>%
  group_by(type) %>%
  group_modify(~remove_outliers(.x)) %>%
  ungroup()

# 拟合图
us_p1 <- ggplot(CrimeData_cleaned, 
                aes(log10(Population),
                    log10(case),
                    colour = factor(type))) +
  # 点使用浅色
  geom_point(aes(colour=factor(type)), size=0.5, shape=21, stroke = 0.3) +
  # 线使用手动指定的深色
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1,
    aes(group = type),
    color = NA
  ) +
  # 单独为每个类型加拟合线（深色）
  geom_smooth(data = subset(CrimeData_long2, type == "Murder"),
              method = "lm", se = FALSE, linewidth = 0.7, color = "#D86A6A") +  # Murder 深蓝
  geom_smooth(data = subset(CrimeData_long2, type == "LarcenyTheft"),
              method = "lm", se = FALSE, linewidth = 0.7, color = "#5967BB") +  # LarcenyTheft 深红
  # 点的颜色
  scale_color_manual(values = c(
    "Murder" = "#E7A3A3",          # 浅hong
    "LarcenyTheft" = "#8FAED1"     # 浅蓝
  )) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = '~~~')),
    formula = y ~ x,
    parse = TRUE,
    size = 2,
    vstep = 0.055
  ) +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(
    title = "United States (2022)",  # 添加标题
    x = expression(paste(Log[10], "[Population]")),
    y = expression(paste(Log[10], "[Crimes]"))
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),
        axis.text = element_text(size = 6), 
        axis.title.y = element_text(size = 6),
        axis.title.x = element_text(size = 7),
        plot.title = element_text(size = 8, hjust = 0.5),  # 设置标题大小与居中
        legend.position = c(0.2, 0.7),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(3, 'mm'),
        legend.text = element_text(size = 5),
        legend.key = element_blank()   )       # 去掉小黑框






###========  去除两倍标准差
calc_beta_ci <- function(data, crime_var, fold = 2){
  # 去除非法值（log前为正）
  data <- subset(data, data[[crime_var]] > 0 & data$Population > 0)
  
  if(nrow(data) <= 5){
    return(c(0, 0, 0))
  } else {
    # 初始回归
    model <- lm(log10(data[[crime_var]]) ~ log10(Population), data = data)
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    
    # 计算残差
    data$residuals <- log10(data[[crime_var]]) - (slope * log10(data$Population) + intercept)
    
    # 去除两倍标准差之外的异常值
    res_mean <- mean(data$residuals, na.rm = TRUE)
    res_sd <- sd(data$residuals, na.rm = TRUE)
    data_clean <- subset(data, 
                         residuals > res_mean - fold * res_sd & 
                           residuals < res_mean + fold * res_sd)
    
    if(nrow(data_clean) <= 5){
      return(c(0, 0, 0))
    }
    
    # 再次拟合回归
    clean_model <- lm(log10(data_clean[[crime_var]]) ~ log10(Population), data = data_clean)
    ci <- confint(clean_model)
    
    return(c(coef(clean_model)[2], ci[2, 1], ci[2, 2]))  # β, CI下限, CI上限
  }
}



crimekinds <- c("Murder","Rape","Robbery","Aggravated","Burglary","LarcenyTheft","MotorTheft","Arson")
years <- 2001:2022
CrimeData <- read.csv("D:/A大创/印度犯罪标度律/A美国/picparts/American_Place_Crime_From_2001_to_2022.csv")

# 总犯罪处理
total_results <- do.call(rbind, lapply(years, function(y){
  year_data <- subset(CrimeData, Year == y)
  year_data$Crime <- year_data$Violent.crime + year_data$Property.crime
  c(y, calc_beta_ci(year_data, "Crime"))
}))
draw_total <- as.data.frame(total_results)
colnames(draw_total) <- c("Year", "beta", "CILower", "CIUpper")
draw_total$CrimeType <- "Total Crime"

# 分犯罪类型处理
draw_all <- draw_total
for (crime in crimekinds){
  crime_result <- do.call(rbind, lapply(years, function(y){
    year_data <- subset(CrimeData, Year == y)
    c(y, calc_beta_ci(year_data, crime))
  }))
  df <- as.data.frame(crime_result)
  colnames(df) <- c("Year", "beta", "CILower", "CIUpper")
  df$CrimeType <- crime
  draw_all <- rbind(draw_all, df)
}



# 替换CrimeType中图例显示的名称
draw_all$CrimeType[draw_all$CrimeType == "Aggravated"] <- "Aggravated assault"

draw_all <- subset(draw_all, !CrimeType %in% c("Total Crime"))
# 假设你希望的顺序是：
# 1. 添加自定义顺序（注意用空因子“——”来断开列）
desired_order <- c("Murder", "Rape", "Arson", "Aggravated assault",
                   "LarcenyTheft", "MotorTheft", "Robbery", "Burglary")

# 2. 将CrimeType设为factor
draw_all$CrimeType <- factor(draw_all$CrimeType, levels = desired_order)

# 3. 设置颜色（注意给“——”一个透明颜色）
color_vals <- c(
  "Murder" = "#c82423", 
  "Rape" = "#f79059",
  "Arson" = "#fa8878",  
  "Aggravated assault" = "#ffbe7a",
  "Burglary" = "#c29484",
  "LarcenyTheft" = "#82afda",
  "Robbery" = "#9bbf8a",
  "MotorTheft" = "#8dcec8" 
)

us_p2 <- ggplot(draw_all, aes(x = Year, y = beta, color = CrimeType)) +
  geom_point(size = 0.45) +
  geom_line(aes(linetype = CrimeType, size = CrimeType)) +  # 添加linetype映射
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_color_manual(values = color_vals, na.translate = FALSE) +
  scale_size_manual(values = c(
    "Murder" = 0.35, 
    "Rape" = 0.35,
    "Arson" = 0.35,  
    "Aggravated assault" = 0.35,
    "Burglary" = 0.35,
    "LarcenyTheft" = 0.35,
    "MotorTheft" = 0.35,
    "Robbery" = 0.35
  )) +
  scale_linetype_manual(values = c(  # 添加线型映射
    "Murder" = "solid", 
    "Rape" = "solid",
    "Arson" = "solid",  
    "Aggravated assault" = "solid",
    "Burglary" = "solid",
    "LarcenyTheft" = "dashed",
    "MotorTheft" = "dashed",
    "Robbery" = "dashed"
  )) +
  labs(x = "Year", y ="Scaling exponent (β)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=6), 
        axis.title=element_text(size=7))+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 5, margin = margin(b = 0.3)),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.size = unit(0.1, "cm"),  # 使用"lines"单位而不是"cm"
        legend.key.height = unit(0.05, "cm"),  # 减小键高度
        legend.key.width = unit(0.2, "cm"),   # 减小键宽度
        legend.spacing.x = unit(0.05, "cm"),      # 水平间距
        legend.spacing.y = unit(0.01, "cm"),     # 垂直间距
        legend.background = element_blank(),  # 去掉外框
        legend.key = element_blank()          # 去掉小黑框
  )+
  guides(color = guide_legend(ncol = 2, byrow = FALSE),
         size = "none",  # 隐藏线条粗细图例
         linetype = "none"  # 隐藏线型图例（避免重复）
  )              


###########################法国
#基础数据读取与处理 
mydata <- read.csv("D:/A大创/印度犯罪标度律/A法国/下载数据/urban数据2.csv", 
                   header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空
mydata <- mydata %>%
  mutate(classe = ifelse(
    classe == "Violent theft without weapons",
    "Theft without weapons",
    classe
  ))
mydata$faits <- as.numeric(mydata$faits)

mydata <- subset(mydata, !classe %in% c("Other assault and battery", "Theft of vehicle accessories"))

#两倍标准差筛选
fold <-2 
new.land8 <- data.frame()
expo.cross <- data.frame()

i <-2016
j <-"Assault and battery"
for (i in unique(mydata$year))
{
  for (j in unique(mydata$classe)){
    origi.data <- subset(mydata, year == i& classe ==j)
    mod <-lm(log10(origi.data$faits) ~ log10(origi.data$pop))
    slope <-round(coef(mod)[2],4)
    intercept <- round(coef(mod)[1],3)
    
    origi.data <- transform(origi.data,
                            residu =log10(origi.data$faits)-
                              (slope*log10(origi.data$pop)+intercept))
    
    new.data <- subset(origi.data, 
                       residu > (mean(residu, na.rm = T)-
                                   fold*sd(residu, na.rm = T))&
                         residu < (mean(residu, na.rm = T)+
                                     fold*sd(residu, na.rm = T)) )
    
    result<-lm(log10(new.data$faits) ~ log10(new.data$pop))
    slope2 <-round(coef(result)[2],3)
    intercept2 <-round(coef(result)[1],3)
    r_square2<-round(summary(result)$r.squared,3)
    low2 <- round(confint.lm(result)[2],4)#95%置信区间下界
    up2 <- round(confint.lm(result)[4],4)#95%置信区间上界
    new.data <- transform(new.data, SAMI = log10(new.data$faits)-
                            (slope2*log10(new.data$pop)+intercept2))
    
    tempdata<-data.frame(year =i, type=j, expo.raw = slope, exponent=slope2, 
                         low2.5=low2, up97.5=up2, 
                         r_square=  r_square2,  intercept=  intercept2)
    
    new.land8 <- rbind(new.land8, new.data)
    expo.cross  <- rbind(expo.cross, tempdata)
    print(i)
    print(j)
  }
}
expo.cross
new.land8



mydata2 <- new.land8[new.land8$year == 2024, ]
mydata2 <- subset(mydata2, !classe %in% c("Vehicle theft"))


mydata2 <- mydata2 %>%
  mutate(classe = ifelse(
    classe == "Residential burglary",
    "Burglary",
    classe
  ))
mydata2 <- mydata2 %>%
  mutate(classe = ifelse(
    classe == "Non-violent theft against individuals",
    "Theft against individuals",
    classe
  ))

subset_data <- subset(mydata2, classe %in% c("Burglary", "Theft against individuals"))



# 设置图例顺序
subset_data$classe <- factor(subset_data$classe, 
                             levels = c("Theft against individuals", "Burglary"))
fc_p1 <- ggplot(subset_data, 
                aes(log10(pop),
                    log10(faits),
                    colour=factor(classe))) +
  geom_point(size = 0.5, shape = 21, stroke = 0.3)+
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1,
    aes(group = classe),
    color = NA
  ) +
  # 单独为每个类型加拟合线（深色）
  geom_smooth(data = subset(subset_data, classe == "Burglary"),
              method = "lm", se = FALSE, linewidth = 0.7, color = "#D86A6A") +  # Murder 深蓝
  geom_smooth(data = subset(subset_data, classe == "Theft against individuals"),
              method = "lm", se = FALSE, linewidth = 0.7, color = "#5967BB") +  # LarcenyTheft 深红
  # 点的颜色
  scale_color_manual(values = c(
    "Burglary" = "#E7A3A3",          # 浅蓝
    "Theft against individuals" = "#8FAED1"     # 浅红
  )) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = '~~~')),
    formula = y ~ x,
    parse = TRUE,
    size = 2,
    vstep = 0.055 # 修正字体设置
  ) +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(
    title = "France (2024)",
    x = expression(paste(Log[10],"[Population]")),  # 修改X轴名称
    y = expression(paste(Log[10],"[Crimes]"))# 使用相同的形式设置Y轴名称
  ) +
  #scale_color_manual(values = region_colors) +  # 按 region 指定颜色
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black"),
        plot.title = element_text(size = 8, hjust = 0.5)  # 设置标题大小与居中
  ) +
  theme(axis.text=element_text(size=6), 
        axis.title.y=element_text(size=6.3),axis.title.x=element_text(size=7))+
  theme(legend.position = c (0.3,0.7),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 5),
        legend.key = element_blank() )         # 去掉小黑框


mydata <- read.csv("D:/A大创/印度犯罪标度律/A法国/输出图/crime_regression_classe_results_filteredoutlier.csv", 
                   header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空


mydata1 <- subset(mydata, !classe %in% c("Vehicle theft","Armed theft","Domestic assault and battery"))
mydata1 <- mydata1 %>%
  mutate(classe = ifelse(
    classe == "Residential burglary",
    "Burglary",
    classe
  ))
mydata1 <- mydata1 %>%
  mutate(classe = ifelse(
    classe == "Non-violent theft against individuals",
    "Theft against individuals",
    classe
  ))


# 1. 添加自定义顺序（注意用空因子“——”来断开列）
desired_order2 <- c("Sexual violence", "Assault and battery", "Drug trafficking", "Burglary",
                    "Fraud","Drug use","Theft against individuals", "Theft from vehicles", "Theft without weapons","Property damage")

# 2. 将CrimeType设为factor
mydata1$classe <- factor(mydata1$classe, levels = desired_order2)

fc_p2 <- ggplot(mydata1, aes(x = year, y = beta, group = classe, color = classe)) +
  geom_point(size=0.45) +  # 保持点的大小不变
  geom_line(aes(size = classe, linetype = classe)) +  # 新增linetype映射
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(x="Year", y="Scaling exponent (β)") +
  scale_color_manual(values = c(
    "Theft without weapons" = "#9bbf8a",  # Total 配色
    "Drug trafficking" = "#fa8878", 
    "Sexual violence" = "#f79059",
    "Assault and battery" = "#ffbe7a",
    "Property damage" = "#add3e2",
    "Burglary" = "#c29484",
    "Theft against individuals" = "#82afda",
    "Theft from vehicles" = "#8dcec8",
    "Fraud" = "#c2bdde",
    "Drug use" = "#3480b8"
  )) +  # 设置新配色
  # 统一线条粗细为0.35
  scale_size_manual(values = c(
    "Theft without weapons" = 0.35,
    "Drug trafficking" = 0.35,
    "Sexual violence" = 0.35,
    "Assault and battery" = 0.35,
    "Property damage" = 0.35,
    "Burglary" = 0.35,
    "Theft against individuals" = 0.35,
    "Theft from vehicles" = 0.35,
    "Fraud" = 0.35,
    "Drug use" = 0.35
  )) +
  # 根据原粗细设置线型（原0.5为实线，原0.25为虚线）
  scale_linetype_manual(values = c(
    "Theft without weapons" = "dashed",  # 原0.25
    "Drug trafficking" = "solid",        # 原0.5
    "Sexual violence" = "solid",         # 原0.5
    "Assault and battery" = "solid",     # 原0.5
    "Property damage" = "dashed",        # 原0.25
    "Burglary" = "solid",                # 原0.5
    "Theft against individuals" = "dashed",  # 原0.25
    "Theft from vehicles" = "dashed",    # 原0.25
    "Fraud" = "dashed",                  # 原0.25
    "Drug use" = "dashed"                # 原0.25
  )) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=6), 
        axis.title=element_text(size=7)) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 5, margin = margin(b = 0.3)),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.size = unit(0.1, "cm"),  # 使用"lines"单位而不是"cm"
        legend.key.height = unit(0.05, "cm"),  # 减小键高度
        legend.key.width = unit(0.2, "cm"),   # 减小键宽度
        legend.spacing.x = unit(0.05, "cm"),      # 水平间距
        legend.spacing.y = unit(0.01, "cm"),     # 垂直间距
        legend.background = element_blank(),  # 去掉外框
        legend.key = element_blank() )+         # 去掉小黑框
  guides(
    color = guide_legend(ncol = 2, byrow = FALSE),
    size = "none",       # 隐藏大小图例
    linetype = "none"    # 隐藏线型图例（避免重复）
  )


#################################印度
mydata <- read.csv("D:/A大创/印度犯罪标度律/excel数据/crime分类/Crime总3.csv", 
                   header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空

mydata_long <- mydata %>%
  pivot_longer(cols = c(`Murder`, 
                        `Rape`, 
                        `Kidnapping.....Abduction`, 
                        `Robbery`, 
                        `Burglary`,
                        `Theft`,
                        `Cheating`,
                        `Hurt`,
                        `Cruelty.by..Husband.or..Relatives`,
                        `Causing..Death.by..Negligence`),   # 需要转换的列
               names_to = "type",              # 新列名称，用来存储原先列名（即 type）
               values_to = "case")             # 新列名称，用来存储列的数值（即 case）

mydata_long <- mydata_long %>%
  mutate(
    type = recode(
      type,
      "Kidnapping.....Abduction" = "Kidnapping and Abduction",
      "Cruelty.by..Husband.or..Relatives" = "Cruelty by Relatives",
      "Causing..Death.by..Negligence" = "Causing Death by Negligence"
    )
  )


#两倍标准差筛选
fold <-2 
new.crime <- data.frame()
expo.cross <- data.frame()

i <-2001
j <-"Murder"
for (i in unique(mydata_long$year))
{
  for (j in unique(mydata_long$type)){
    origi.data <- subset(mydata_long, year == i& type ==j)
    mod <-lm(log10(origi.data$case) ~ log10(origi.data$population))
    slope <-round(coef(mod)[2],4)
    intercept <- round(coef(mod)[1],3)
    
    origi.data <- transform(origi.data,
                            residu =log10(origi.data$case)-
                              (slope*log10(origi.data$population)+intercept))
    
    new.data <- subset(origi.data, 
                       residu > (mean(residu, na.rm = T)-
                                   fold*sd(residu, na.rm = T))&
                         residu < (mean(residu, na.rm = T)+
                                     fold*sd(residu, na.rm = T)) )
    
    result<-lm(log10(new.data$case) ~ log10(new.data$pop))
    slope2 <-round(coef(result)[2],3)
    intercept2 <-round(coef(result)[1],3)
    r_square2<-round(summary(result)$r.squared,3)
    low2 <- round(confint.lm(result)[2],4)#95%置信区间下界
    up2 <- round(confint.lm(result)[4],4)#95%置信区间上界
    new.data <- transform(new.data, SAMI = log10(new.data$case)-
                            (slope2*log10(new.data$population)+intercept2))
    
    tempdata<-data.frame(year =i, type=j, expo.raw = slope, exponent=slope2, 
                         low2.5=low2, up97.5=up2, 
                         r_square=  r_square2,  intercept=  intercept2)
    
    new.crime <- rbind(new.crime, new.data)
    expo.cross  <- rbind(expo.cross, tempdata)
    print(i)
    print(j)
  }
}
expo.cross
new.crime


indiadata1 <- new.crime[new.crime$year == 2021, ]
indiadata1 <- subset(indiadata1, type %in% c("Murder", "Theft"))
# 设置图例顺序
indiadata1$type <- factor(indiadata1$type, 
                          levels = c("Theft", "Murder"))
id_p1 <- ggplot(indiadata1, 
                aes(log10(population),
                    log10(case),
                    colour=factor(type))) +
  geom_point(size=0.5, shape=21, stroke = 0.3) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1,
    aes(group = type),
    color = NA
  ) +
  # 单独为每个类型加拟合线（indiadata1）
  geom_smooth(data = subset(indiadata1, type == "Murder"),
              method = "lm", se = FALSE, linewidth = 0.7, color = "#D86A6A") +  # Murder 深蓝
  geom_smooth(data = subset(indiadata1, type == "Theft"),
              method = "lm", se = FALSE, linewidth = 0.7, color = "#5967BB") +  # LarcenyTheft 深红
  # 点的颜色
  scale_color_manual(values = c(
    "Murder" = "#E7A3A3",          # 浅hong
    "Theft" = "#8FAED1"     # 浅蓝
  )) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = '~~~')),
    formula = y ~ x,
    parse = TRUE,
    size = 2,
    vstep = 0.055 # 修正字体设置
  ) +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(title = "India (2021)",
       x = expression(paste(Log[10],"[Population]")),  # 修改X轴名称
       y = expression(paste(Log[10],"[Crimes]"))  # 使用相同的形式设置Y轴名称
  ) +
  #scale_color_manual(values = region_colors) +  # 按 region 指定颜色
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black"),
        plot.title = element_text(size = 8, hjust = 0.5)) +
  theme(axis.text=element_text(size=6), 
        axis.title=element_text(size=6.3),axis.title.x=element_text(size=7))+
  theme(legend.position = c (0.2,0.7),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 5),
        legend.key = element_blank() )         # 去掉小黑框


mydata4 <- read.csv("D:/A大创/印度犯罪标度律/excel数据/crime分类/india_regression_results_filteredoutlier.csv", 
                    header=TRUE, sep=",",na.strings = 0)

mydata4 <- mydata4 %>%
  mutate(type = ifelse(
    type == "Kidnapping and Abduction",
    "Kidnapping&Abduction",
    type
  ))
# 1. 添加自定义顺序（注意用空因子“——”来断开列）
desired_order3 <- c("Murder", "Rape", "Kidnapping&Abduction", "Burglary","Hurt",
                    "Causing Death by Negligence", "Theft", "Cheating","Robbery","Cruelty by Relatives")

# 2. 将CrimeType设为factor
mydata4$type <- factor(mydata4$type, levels = desired_order3)

id_p2 <- ggplot(mydata4, aes(x = year, y = beta, group = type, color = type)) +
  geom_line(aes(size = type, linetype = type), show.legend = FALSE) +
  geom_point(size = 0.45) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(x = "Year", y = "Scaling exponent (β)") +
  scale_x_continuous(breaks = c(2001, 2011, 2021)) +
  scale_color_manual(values = c(
    "Kidnapping&Abduction" = "#fa8878",
    "Murder" = "#c82423",
    "Rape" = "#f79059",
    "Hurt" = "#ffbe7a",
    "Robbery" = "#9bbf8a",
    "Burglary" = "#c29484",
    "Theft" = "#82afda",
    "Cheating" = "#c2bdde",
    "Cruelty by Relatives" = "#fee1cf",
    "Causing Death by Negligence" = "#3480b8"
  )) +
  scale_size_manual(values = c(
    "Kidnapping&Abduction" = 0.35,
    "Murder" = 0.35,
    "Rape" = 0.35,
    "Hurt" = 0.35,
    "Robbery" = 0.35,
    "Burglary" = 0.35,
    "Theft" = 0.35,
    "Cheating" = 0.35,
    "Cruelty by Relatives" = 0.35,
    "Causing Death by Negligence" = 0.35
  )) +
  scale_linetype_manual(values = c(
    "Kidnapping&Abduction" = "solid",
    "Murder" = "solid",
    "Rape" = "solid",
    "Hurt" = "solid",
    "Robbery" = "dashed",
    "Burglary" = "solid",
    "Theft" = "dashed",
    "Cheating" = "dashed",
    "Cruelty by Relatives" = "solid",
    "Causing Death by Negligence" = "dashed"
  )) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black"),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 7),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.direction = "horizontal",
    legend.position = "bottom",
    # 关键修改：调整图例相关参数
    legend.key.size = unit(0.1, "cm"),  # 使用"lines"单位而不是"cm"
    legend.key.height = unit(0.05, "cm"),  # 减小键高度
    legend.key.width = unit(0.2, "cm"),   # 减小键宽度
    legend.spacing.x = unit(0.05, "cm"),      # 水平间距
    legend.spacing.y = unit(0.01, "cm"),     # 垂直间距
    #legend.margin = margin(t = -0.2, unit = "cm"),  # 减少图例上方的边距
    legend.background = element_blank(),
    legend.key = element_blank()
  ) +
  guides(color = guide_legend(
    ncol = 2,
    byrow = TRUE,
    keyheight = unit(0.5, "lines"),  # 在guide_legend中再次设置键高度
    default.unit = "line"            # 设置默认单位
  ))



tiff(file="图2.1 country_crime_scaling.tiff", res = 600, width = 3600, height = 1270, compression = "lzw")
ggarrange(us_p1,fc_p1,id_p1,
          nrow = 1,ncol=3,
          labels = c( "a", "b","c"),
          #label.y = 1.025, label.x = -0.026, # 默认是 1，可以设置 >1 向上移动
          #heights = c(1, 1.2),  # 设置每个图的相对宽度比例
          #align = "h",
          font.label = list(size = 11))
dev.off()
getwd()

tiff(file="图2.2 country_crime_scaling.tiff", res = 600, width = 3600, height = 1600, compression = "lzw")
ggarrange(us_p2,fc_p2,id_p2,
          nrow = 1,ncol=3,
          labels = c( "d","e", "f"),
          #label.y = 1.025, label.x = -0.026, # 默认是 1，可以设置 >1 向上移动
          #heights = c(1, 1.2),  # 设置每个图的相对宽度比例
          align = "h",
          font.label = list(size = 11))
dev.off()
getwd()



library("ggplot2")
library("dplyr")
library("tidyr")
library("stringr")
library("openxlsx")
library("ggpubr")
library("patchwork")
library("sf")
setwd("E:/ZhangTao/Science/犯罪标度律/USCode/MechanismCode")
crime_kinds<-c("Murder","Rape","Robbery","Aggravated","Burglary","LarcenyTheft","MotorTheft")
crime_kinds2<-c("Murder","Rape","Robbery","Aggravated","Burglary","LarcenyTheft","MotorTheft","Arson")

show_crime_type<-function(types){
  if(types=="LarcenyTheft"){
    return("Larceny-Theft")
  }else if(types=="MotorTheft"){
    return("Motor Vehicle Theft")
  }else if(types=="Aggravated"){
    return("Aggravated assault")
  }else{
    return(types)
  }
}


# Remove outliers
# select_type includes Crime, Guardian, and Offense
# state denotes different states (with Oregon handled differently)
Outlier_Remove <- function(data, select_type){
  
  dat <- data
  col_name <- ""
  if(select_type == "Guardian"){
    col_name <- "Law_Employees"
  }else{
    col_name <- "value"
  }
  
  copy_dat <- dat
  
  dat$log_popu <- log10(dat$Population)
  dat$log_value <- log10(dat[[col_name]])
  
  ori_model <- lm(log_value ~ log_popu, data = dat)
  
  dat$res <- residuals(ori_model)
  
  res_mean <- mean(dat$res, na.rm = TRUE)
  res_sd <- sd(dat$res, na.rm = TRUE)
  
  dat$outlier <- abs(dat$res - res_mean) > 2 * res_sd
  final_index <- which(dat$outlier == FALSE)
  
  result <- copy_dat[final_index, ]
  return(result)
}



# Plot crime distribution maps for four states (crime, supervision, arrests)
# and determine whether the model's results align with the fitted data
Mechanism_Pic_V1<-function(){
  
  
  select_type<-"LarcenyTheft"
  select_type_2<-"Murder"
  show_type<-show_crime_type(select_type)
  show_type2<-show_crime_type(select_type_2)
  colours<-c("#4BAB25","#94CE67","#F1CBE4","#E783C1","#CF1B89")
  colours2<-c("#E0F7FA","#80DEEA","#26C6DA","#FFA726","#E65100")
  
  ####################################### Mapping Crime Distribution Patterns in Florida ####################################################
  fl_shp<-st_read("./Data/Geography/Florida/FDEM_-_Regions.shp")%>%
    st_transform(crs = 4326)
  fl_crime_data<-read.csv("./Data/Florida/Florida Crime Data.csv")
  fl_crime_long<-pivot_longer(fl_crime_data,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft),
                              names_to = "crime_type",values_to = "value")%>%
    subset(value>0&Population>0&crime_type==select_type&Year==max(unique(fl_crime_data$Year)))
  fl_crime_long2<-pivot_longer(fl_crime_data,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft),
                               names_to = "crime_type",values_to = "value")%>%
    subset(value>0&Population>0&crime_type==select_type_2&Year==max(unique(fl_crime_data$Year)))
  
  fl_crime_long$County<-toupper(fl_crime_long$County)
  fl_crime_long2$County<-toupper(fl_crime_long2$County)
  fl_shp$NAME[fl_shp$NAME=="OSCEOLA"]<-"OSECOLA"
  fl_shp$NAME<-paste0(fl_shp$NAME," County")
  fl_shp$NAME<-toupper(fl_shp$NAME)
  fl_crime_shp<-fl_shp %>%
    left_join(fl_crime_long, by = c("NAME" = "County"))
  fl_crime_shp2<-fl_shp %>%
    left_join(fl_crime_long2, by = c("NAME" = "County"))
  
  breaks <- c(0,100,1000,2500,5000,max(fl_crime_shp$value))
  brk_labels<-c("0 ~ 100","100 ~ 1000","1000 ~ 2500","2500 ~ 5000","> 5000")
  fl_crime_shp$color_level <- cut(fl_crime_shp$value, breaks = breaks, include.lowest = TRUE, right = FALSE)
  
  breaks2 <- c(0,10,50,100,150,max(fl_crime_shp2$value, na.rm = TRUE)+1)
  brk_labels2<-c("0 ~ 10","10 ~ 50","50 ~ 100","100 ~ 150","> 150")
  fl_crime_shp2$color_level <- cut(fl_crime_shp2$value, breaks = breaks2, include.lowest = TRUE, right = FALSE)
  
  # Fig 4a
  fl_crime_distribution_LT<-ggplot(data = fl_crime_shp)+
    geom_sf(aes(fill=color_level))+
    labs(fill=paste0("The number of\n ",show_type," offense","\n(",max(unique(fl_crime_data$Year)),")"))+
    scale_fill_manual(values = colours2,labels=brk_labels)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
          axis.title = element_blank(),
          legend.title = element_text(size = 11, hjust = 0.5,face = "bold"),
          legend.text = element_text(size = 10, vjust = 0.5),
          legend.position = c(0.26,0.42),
          legend.direction = "vertical",
          legend.background = element_rect(fill = "transparent"),
          aspect.ratio = 1)
  
  print(fl_crime_distribution_LT)
  ggsave("./Pic/Figure4_parts/a.jpg",fl_crime_distribution_LT,width = 4,height = 4,dpi = 300)
  
  # Fig 4c
  fl_crime_distribution_MD<-ggplot(data = fl_crime_shp2)+
    geom_sf(aes(fill=color_level))+
    labs(fill=paste0("The number of\n ",show_type2," offense","\n(",max(unique(fl_crime_data$Year)),")"))+
    scale_fill_manual(values = colours,labels=brk_labels2)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
          axis.title = element_blank(),
          legend.title = element_text(size = 11, hjust = 0.5,face = "bold"),
          legend.text = element_text(size = 10, vjust = 0.5),
          legend.position = c(0.26,0.42),
          legend.direction = "vertical",
          legend.background = element_rect(fill = "transparent"),
          aspect.ratio = 1)
  
  print(fl_crime_distribution_MD)
  ggsave("./Pic/Figure4_parts/a2.jpg",fl_crime_distribution_MD,width = 4,height = 4,dpi = 300)
  
  ######################################## Mapping Crime, Arrests, and Probation in Florida for the Year 2020 ###############################
  fl_guardian<-read.csv("./Data/Florida/Florida Law Enforcement Data.csv")
  fl_population<-fl_crime_data[,c("County","Year","Population")]
  fl_guardian<-merge(fl_guardian,fl_population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
    subset(Law_Employees>0&Population>0&Year==max(unique(fl_crime_data$Year)))
  
  fl_guardian <- Outlier_Remove(data = fl_guardian, select_type = "Guardian")
  
  fl_offenser<-read.csv("./Data/Florida/Florida Arrest Data.csv")
  fl_offenser_long<-pivot_longer(fl_offenser,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft),
                                 names_to = "crime_type",values_to = "value")%>%
    subset(value>0&Population>0&crime_type==select_type&Year==max(unique(fl_crime_data$Year)))
  
  fl_offenser_long2<-pivot_longer(fl_offenser,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft),
                                  names_to = "crime_type",values_to = "value")%>%
    subset(value>0&Population>0&crime_type==select_type_2&Year==max(unique(fl_crime_data$Year)))
  
  fl_offenser_long <- Outlier_Remove(data = fl_offenser_long, select_type = "Offense")
  fl_offenser_long2 <- Outlier_Remove(data = fl_offenser_long2, select_type = "Offense")
  
  
  guardian_plot <- fl_guardian %>%
    select(County, Year, Population, Value = Law_Employees) %>%
    mutate(Type = "Guardian")
  guardian_model<-lm(log10(Law_Employees)~log10(Population),data = fl_guardian)
  
  offense_plot <- fl_offenser_long %>%
    select(County, Year, Population, Value = value) %>%
    mutate(Type = "Offenser")
  offenser_model<-lm(log10(value)~log10(Population),data=fl_offenser_long)
  
  offense_plot2 <- fl_offenser_long2 %>%
    select(County, Year, Population, Value = value) %>%
    mutate(Type = "Offenser")
  offenser_model2<-lm(log10(value)~log10(Population),data=fl_offenser_long2)
  
  fl_crime_long <- Outlier_Remove(data = fl_crime_long, select_type = "Crime")
  fl_crime_long2 <- Outlier_Remove(data = fl_crime_long2, select_type = "Crime")
  
  crime_plot <- fl_crime_long %>%
    select(County, Year, Population, Value = value) %>%
    mutate(Type = show_type)
  crime_model<-lm(log10(value)~log10(Population),data = fl_crime_long)
  
  crime_plot2 <- fl_crime_long2 %>%
    select(County, Year, Population, Value = value) %>%
    mutate(Type = show_type2)
  crime_model2<-lm(log10(value)~log10(Population),data = fl_crime_long2)
  
  combined_plot <- bind_rows(guardian_plot, offense_plot, crime_plot)
  combined_line<-data.frame(Type=c("Guardian","Offenser",show_type),
                            inter=c(coef(guardian_model)[1],coef(offenser_model)[1],coef(crime_model)[1]),
                            slope=c(coef(guardian_model)[2],coef(offenser_model)[2],coef(crime_model)[2]),
                            r2=c(summary(guardian_model)$r.squared,summary(offenser_model)$r.squared,summary(crime_model)$r.squared))
  combined_text<-data.frame(Type=c("Guardian","Offenser",show_type),
                            texts=c(
                              paste("y=", round(coef(guardian_model)[1], 2), "+", round(coef(guardian_model)[2], 2), "x, R²=",round(summary(guardian_model)$r.squared,2)),
                              paste("y=", round(coef(offenser_model)[1], 2), "+", round(coef(offenser_model)[2], 2), "x, R²=",round(summary(offenser_model)$r.squared,2)),
                              paste("y=", round(coef(crime_model)[1], 2), "+", round(coef(crime_model)[2], 2), "x, R²=",round(summary(crime_model)$r.squared,2))
                            ),
                            x=c(3.8,3.8,3.8),
                            y=c(4.3,3.9,4.6))
  
  combined_plot2 <- bind_rows(guardian_plot, offense_plot2, crime_plot2)
  combined_line2<-data.frame(Type=c("Guardian","Offenser",show_type2),
                             inter=c(coef(guardian_model)[1],coef(offenser_model2)[1],coef(crime_model2)[1]),
                             slope=c(coef(guardian_model)[2],coef(offenser_model2)[2],coef(crime_model2)[2]),
                             r2=c(summary(guardian_model)$r.squared,summary(offenser_model2)$r.squared,summary(crime_model2)$r.squared))
  combined_text2<-data.frame(Type=c("Guardian","Offenser",show_type2),
                             texts=c(
                               paste("y=", round(coef(guardian_model)[1], 2), "+", round(coef(guardian_model)[2], 2), "x, R²=",round(summary(guardian_model)$r.squared,2)),
                               paste("y=", round(coef(offenser_model2)[1], 2), "+", round(coef(offenser_model2)[2], 2), "x, R²=",round(summary(offenser_model2)$r.squared,2)),
                               paste("y=", round(coef(crime_model2)[1], 2), "+", round(coef(crime_model2)[2], 2), "x, R²=",round(summary(crime_model2)$r.squared,2))
                             ),
                             x=c(3.8,3.8,3.8),
                             y=c(4.3,3.9,4.6))
  
  combined_plot$Type<-factor(combined_plot$Type,levels = c(show_type,"Guardian","Offenser"))
  combined_line$Type<-factor(combined_line$Type,levels = c(show_type,"Guardian","Offenser"))
  combined_text$Type<-factor(combined_text$Type,levels = c(show_type,"Guardian","Offenser"))
  
  combined_plot2$Type<-factor(combined_plot2$Type,levels = c(show_type2,"Guardian","Offenser"))
  combined_line2$Type<-factor(combined_line2$Type,levels = c(show_type2,"Guardian","Offenser"))
  combined_text2$Type<-factor(combined_text2$Type,levels = c(show_type2,"Guardian","Offenser"))
  
  # Fig 4b
  fl_scaling_larcenytheft<-ggplot(combined_plot,aes(x=log10(Population),y=log10(Value),color=Type))+
    geom_point(alpha=0.7,size=2)+
    labs(x="Log10[Population]",y="Log10[Value]")+
    scale_color_manual(values = c("#8FAED1", "#578D2F", "#FFABDF")) +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
          axis.title = element_text(size=12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          legend.position = c(1,0.05),
          legend.justification=c(1,0),
          legend.background = element_rect(fill = "transparent"))+
    geom_abline(data = combined_line,aes(intercept = inter,slope = slope,color=Type),linewidth=0.8,show.legend = FALSE)+
    geom_text(data = combined_text,aes(x=x,y=y,label = texts,color=Type),hjust = 0, size = 4.5,show.legend = FALSE)+
    guides(color = guide_legend(override.aes = list(size=5)))
  
  print(fl_scaling_larcenytheft)
  ggsave("./Pic/Figure4_parts/b.jpg",fl_scaling_larcenytheft,width = 4,height = 4,dpi = 300)
  
  # Fig 4d
  fl_scaling_murder<-ggplot(combined_plot2,aes(x=log10(Population),y=log10(Value),color=Type))+
    geom_point(alpha=0.7,size=2)+
    labs(x="Log10[Population]",y="Log10[Value]")+
    scale_color_manual(values = c("#c82423", "#578D2F", "#FFABDF")) +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
          axis.title = element_text(size=12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          legend.position = c(0,0.75),
          legend.justification=c(0,1),
          legend.background = element_rect(fill = "transparent"))+
    geom_abline(data = combined_line2,aes(intercept = inter,slope = slope,color=Type),linewidth=0.8,show.legend = FALSE)+
    geom_text(data = combined_text2,aes(x=x,y=y,label = texts,color=Type),hjust = 0, size = 4.5,show.legend = FALSE)+
    guides(color = guide_legend(override.aes = list(size=5)))
  
  print(fl_scaling_murder)
  ggsave("./Pic/Figure4_parts/c.jpg",fl_scaling_murder,width = 4,height = 4,dpi = 300)
  
  
  ###################### The gap between the model scaling index predictions and the actual scaling index #######################
  ###################### for different crime types across four states in different years                  #######################
  
  states<-c("Florida","North Carolina","Oregon","Wisconsin")
  line_plots<-list()
  line_counts<-1
  legend_data<-data.frame()
  for(state in states){
    
    state_params<-list()
    
    if(state=="Florida"){
      # Read crime data
      crime_data<-read.csv("./Data/Florida/Florida Crime Data.csv")
      population<-crime_data[,c("County","Year","Population")]
      crime_long<-pivot_longer(crime_data,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft),
                               names_to = "crime_type",values_to = "value")%>%
        subset(value>0&Population>0)
      # Effective guardian
      guardian<-read.csv("./Data/Florida/Florida Law Enforcement Data.csv")
      guardian<-merge(guardian,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
        subset(Law_Employees>0)
      # Individuals with criminal intent (offense)
      offenser<-read.csv("./Data/Florida/Florida Arrest Data.csv")
      offenser_long<-pivot_longer(offenser,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft),
                                  names_to = "crime_type",values_to = "value")%>%
        subset(value>0&Population>0)
    }else if(state=="North Carolina"){
      # Read crime data
      crime_data<-read.csv("./Data/North Carolina/North Carolina Crime Data.csv")
      population<-read.csv("./Data/North Carolina/North Carolina Population Data.csv")
      crime<-merge(crime_data,population,by.x = c("County","Year"),by.y = c("County","Year"))
      
      crime_long<-pivot_longer(crime,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                               names_to = "crime_type",values_to = "value")%>%
        subset(value>0&Population>0)
      
      # Individuals with criminal intent (offense)
      offenser<-read.csv("./Data/North Carolina/North Carolina Arrest Data.csv")
      offenser<-merge(offenser,population,by.x = c("County","Year"),by.y = c("County","Year"))
      offenser_long<-pivot_longer(offenser,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                                  names_to = "crime_type",values_to = "value")%>%
        subset(value>0&Population>0)
      
      # Effective guardian
      guardian<-read.csv("./Data/North Carolina/North Carolina Law Employees Data.csv")
      guardian<-merge(guardian,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
        subset(Law_Employees>0)
    }else if(state=="Oregon"){
      # Read crime data
      crime_data<-read.csv("./Data/Oregon/Oregon Crime Data.csv")
      population<-read.csv("./Data/Oregon/Oregon Population Data.csv")
      crime_long<-merge(crime_data,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
        subset(value>0&Population>0)
      # Effective guardian
      guardian<-read.csv("./Data/Oregon/Oregon Law Employees Data.csv")
      guardian<-merge(guardian,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
        subset(Law_Employees>0)
      # Individuals with criminal intent (offense)
      offenser<-read.csv("./Data/Oregon/Oregon Arrest Data.csv")
      offenser_long<-merge(offenser,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
        subset(value>0&Population>0)
    }else if(state=="Wisconsin"){
      
      # Read crime data
      crime_data<-read.csv("./Data/Winsconsin/Winsconsin Crime Data.csv")
      population<-read.csv("./Data/Winsconsin/Winsconsin Population Data.csv")
      crime<-merge(crime_data,population,by.x = c("County","Year"),by.y = c("County","Year"))
      crime_long<-pivot_longer(crime,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                               names_to = "crime_type",values_to = "value")%>%
        subset(value>0&Population>0)
      
      # Effective guardian
      guardian<-read.csv("./Data/Winsconsin/Winsconsin Law Employees Data.csv")
      guardian<-merge(guardian,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
        subset(Law_Employees>0)
      
      # Individuals with criminal intent (offense)
      offenser<-read.csv("./Data/Winsconsin/Winsconsin Arrest Data.csv")
      offenser<-merge(offenser,population,by.x = c("County","Year"),by.y = c("County","Year"))
      offenser_long<-pivot_longer(offenser,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                                  names_to = "crime_type",values_to = "value")%>%
        subset(value>0&Population>0)
    }
    
    if(state=="Florida"){
      crimekinds<-crime_kinds
    }else{
      crimekinds<-crime_kinds2
    }
    
    for(year in unlist(unique(crime_long$Year))){
      for(crimekind in crimekinds){
        
        crime_k<-""
        if(crimekind=="Aggravated"){
          crime_k<-"Aggravated assault"
        }else if(crimekind=="LarcenyTheft"){
          crime_k<-"Larceny-Theft"
        }else if(crimekind=="MotorTheft"){
          crime_k<-"Motor vehicle Theft"
        }else{
          crime_k<-crimekind
        }
        
        yk_crime<-subset(crime_long,Year==year&crime_type==crimekind)
        yk_offenser<-subset(offenser_long,Year==year&crime_type==crimekind)
        yk_guardian<-subset(guardian,Year==year)
        
        yk_crime <- Outlier_Remove(data = yk_crime, select_type = "Crime")
        yk_offenser <- Outlier_Remove(data = yk_offenser, select_type = "Offense")
        yk_guardian <- Outlier_Remove(data = yk_guardian, select_type = "Guardian")
        
        yk_crime_model<-lm(log10(value)~log10(Population),data = yk_crime)
        yk_offenser_model<-lm(log10(value)~log10(Population),data = yk_offenser)
        yk_guardian_model<-lm(log10(Law_Employees)~log10(Population),data = yk_guardian)
        
        temp<-data.frame(State=state,Crime_type=crime_k,Year=year,
                         Crime_beta=coef(yk_crime_model)[2],Offenser_beta=coef(yk_offenser_model)[2],Guardian_beta=coef(yk_guardian_model)[2])
        state_params<-rbind(state_params,temp)
      }
    }
    
    rownames(state_params) <- seq(1, nrow(state_params))
    
    state_params$x<-state_params$Crime_beta
    state_params$y<-1-state_params$Guardian_beta+state_params$Offenser_beta
    
    if(state=="Florida"){
      sunxu<-c("Murder","Rape","Robbery","Aggravated assault","Burglary","Larceny-Theft","Motor vehicle Theft")
    }else{
      sunxu<-c("Murder","Rape","Robbery","Aggravated assault","Burglary","Larceny-Theft","Motor vehicle Theft","Arson")
    }
    state_params$Crime_type<-factor(state_params$Crime_type,levels = sunxu)
    legend_data<-rbind(legend_data,state_params)
    
    tmodel<-lm(y~x,data = state_params)
    tslope<-coef(tmodel)[2]
    tintercept<-coef(tmodel)[1]
    
    # Fig 4e-h
    line_p<-ggplot(state_params,aes(x=x,y=y,shape=Crime_type,color=Crime_type))+
      geom_point(size=3,stroke = 1.5)+
      geom_abline(slope = tslope,intercept = tintercept,color="black",linetype = "dashed")+
      scale_color_manual(
        breaks = c("Murder","Rape","Aggravated assault","Burglary","Arson",
                   "Robbery","Larceny-Theft","Motor vehicle Theft"),
        values = c(
          "Murder" = "#c82423", 
          "Rape" = "#f79059",
          "Robbery" = "#9bbf8a",
          "Aggravated assault" = "#ffbe7a",
          "Burglary" = "#c29484",
          "Larceny-Theft" = "#82afda",
          "Motor vehicle Theft" = "#8dcec8",
          "Arson" = "#fa8878"
        ))+
      scale_shape_manual(
        breaks = c("Murder","Rape","Aggravated assault","Burglary","Arson",
                   "Robbery","Larceny-Theft","Motor vehicle Theft"),
        values = c(
          "Murder"             = 16,  
          "Rape"               = 16,
          "Aggravated assault" = 16,
          "Burglary"           = 16,
          "Arson"              = 16,
          "Robbery"            = 1,  
          "Larceny-Theft"      = 1,
          "Motor vehicle Theft"= 1
        )) +
      labs(x = "Scaling Exponent β", y = "Model-Predicted Exponent", color = "Crime Type",shape="Crime Type")+
      geom_abline(slope = 1, intercept = 0, color = "gray")+
      annotate("text",x=max(max(state_params$x),max(state_params$y)),y=min(min(state_params$x),min(state_params$y)),
               label=paste0("y = ",round(tintercept,2)," + ",round(tslope,2),"x, R² = ",round(summary(tmodel)$r.squared, 2)),
               color="black",hjust=1,size=5)+
      geom_hline(yintercept = 1,linetype = "dashed", color = "gray")+
      geom_vline(xintercept = 1,linetype = "dashed", color = "gray")+
      coord_cartesian(xlim = c(min(min(state_params$x),min(state_params$y))-0.05,max(max(state_params$x),max(state_params$y))+0.05), 
                      ylim = c(min(min(state_params$x),min(state_params$y))-0.05,max(max(state_params$x),max(state_params$y))+0.05))+
      annotate("text",x=min(min(state_params$x),min(state_params$y)),y=max(max(state_params$x),max(state_params$y))+0.02,label=state,color="black",hjust=0,size=5)+
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
            axis.title = element_text(size=12),
            axis.text = element_text(size = 12),
            axis.text.y = element_text(angle = 90),
            legend.position = "none",
            legend.justification=c(0,1),
            legend.background = element_rect(fill = "transparent"),
            legend.title = element_text(size = 8, hjust = 0.5,face = "bold"),
            legend.text = element_text(size=6),
            legend.box = "horizontal",
            legend.spacing.x = unit(0.05,"mm"),
            legend.spacing.y = unit(0.05,"mm"),
            legend.box.background = element_rect(color = "black", linetype = "dashed", fill = NA))+
      guides(shape=guide_legend(ncol=1,title.position="top",override.aes = list(size=4)),
             color=guide_legend(ncol=1,title.position="top",override.aes = list(size=4)))
    
    print(line_p)
    filename<-paste0("./Pic/Figure4_parts/d_",line_counts,".jpg")
    ggsave(filename,line_p,width = 4,height = 4,dpi = 300)
    
    line_plots[[line_counts]]<-line_p
    line_counts<-line_counts+1
  }
  
  # legend of Fig 4e-h
  legend_plot2<-ggplot(legend_data,aes(x=x,y=y,shape=Crime_type,color=Crime_type))+
    geom_point(stroke = 1.5)+
    labs(title = "图1")+
    labs(x = "β", y = "1+α-γ", color = "Crime Type",shape="Crime Type")+
    scale_color_manual(
      breaks = c("Murder","Rape","Aggravated assault","Burglary","Arson",
                 "Robbery","Larceny-Theft","Motor vehicle Theft"),
      values = c(
        "Murder" = "#c82423", 
        "Rape" = "#f79059",
        "Robbery" = "#9bbf8a",
        "Aggravated assault" = "#ffbe7a",
        "Burglary" = "#c29484",
        "Larceny-Theft" = "#82afda",
        "Motor vehicle Theft" = "#8dcec8",
        "Arson" = "#fa8878"
      ))+
    scale_shape_manual(
      breaks = c("Murder","Rape","Aggravated assault","Burglary","Arson",
                 "Robbery","Larceny-Theft","Motor vehicle Theft"),
      values = c(
        "Murder"             = 16,   # 实心圆
        "Rape"               = 16,
        "Aggravated assault" = 16,
        "Burglary"           = 16,
        "Arson"              = 16,
        "Robbery"            = 1,    # 空心圆
        "Larceny-Theft"      = 1,
        "Motor vehicle Theft"= 1
      )) +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
          legend.position = "bottom",
          legend.direction = "horizontal")+
    guides(shape=guide_legend(ncol=8,title.position="top",override.aes = list(size=4)),
           color=guide_legend(ncol=8,title.position="top",override.aes = list(size=4)))
  print(legend_plot2)
  lg2<-ggplotGrob(legend_plot2)
  legend2<-which(sapply(lg2$grobs,function(x) x$name) == "guide-box")
  legend2<-lg2$grobs[[legend2]]
  ggsave("./Pic/Figure4_parts/legend2.jpg",legend2,width = 8,height = 1,dpi = 300)
  
  final_plots<-ggarrange(
    ggarrange(fl_crime_distribution_LT,fl_scaling_larcenytheft,fl_scaling_murder,line_plots[[1]],nrow = 1,ncol = 4,widths = c(1,1,1,1),labels = c("a","b","c","d"),font.label = list(size=20)),
    ggarrange(line_plots[[2]],line_plots[[3]],line_plots[[4]],legend,nrow = 1,ncol = 4,widths = c(1,1,1,1),labels = c("e","f","g",""),font.label = list(size=20)),
    nrow = 2,ncol = 1,heights = c(1,1)
    
  )
  
  print(final_plots)
  
  
  ggsave("./Pic/Figure4V1.jpg",final_plots,width = 14,height=8,dpi=300)
  
  
}

Mechanism_Pic_SV1<-function(){
  
  select_types<-c("Murder","LarcenyTheft")
  
  ######################################## Mapping the Distribution of Two Types of Crime in North Carolina ############################
  for(select_type in select_types){
    
    show_type<-show_crime_type(select_type)
    
    shp<-st_read("./Data/Geography/North Carolina/North_Carolina_State_and_County_Boundary_Polygons.shp")%>%
      st_transform(crs = 4326)
    crime_data<-read.csv("./Data/North Carolina/North Carolina Crime Data.csv")
    crime_long<-pivot_longer(crime_data,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                             names_to = "crime_type",values_to = "value")%>%
      subset(crime_type==select_type&Year==max(unique(crime_data$Year)))
    
    crime_long$County<-toupper(crime_long$County)
    shp$County<-paste0(shp$County," County")
    shp$County<-toupper(shp$County)
    crime_shp<-shp %>%
      left_join(crime_long, by = c("County" = "County"))
    
    fillname<-paste0("The number of\n ",show_type,"\noffense")
    fig_title<-paste0("North Carolina - ", select_type, " (",max(unique(crime_data$Year)),")")
    
    if(select_type=="Murder"){
      colours<-c("#94CE67","#4BAB25","#F1CBE4","#E783C1","#CF1B89")
      breaks<-c(0,5,10,20,50,max(crime_shp$value))
      brk_labels<-c("0 ~ 5","5 ~ 10","10 ~ 20","20 ~ 50","> 50")
      label_pos <- c(0.15, 0.22)
    }else{
      colours<-c("#E0F7FA","#80DEEA","#26C6DA","#FFA726","#E65100")
      breaks<-c(0,1000,5000,10000,15000,max(crime_shp$value))
      brk_labels<-c("0 ~ 1000","1000 ~ 5000","5000 ~ 10000","10000 ~ 15000","> 15000")
      label_pos <- c(0.1, 0.22)
    }
    crime_shp$color_level <- cut(crime_shp$value, breaks = breaks, include.lowest = TRUE, right = FALSE)
    
    # Fig S4 ab left
    crime_distribute<-ggplot(data = crime_shp)+
      geom_sf(aes(fill=color_level))+
      labs(fill=fillname,title = fig_title)+
      coord_sf(xlim = c(st_bbox(crime_shp)["xmin"],st_bbox(crime_shp)["xmax"]), ylim = c(st_bbox(crime_shp)["ymin"]-3,st_bbox(crime_shp)["ymax"]+1))+
      scale_fill_manual(values = colours,labels=brk_labels,na.value = "gray")+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5,size = 10),
            panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
            axis.title = element_blank(),
            title = element_text(size=14,face="bold",hjust = 0.5),
            legend.title = element_text(size = 8, hjust = 0.5,face = "bold"),
            legend.text = element_text(size = 7, vjust = 0.5,face = "bold"),
            legend.position = label_pos,
            legend.direction = "horizontal",
            legend.justification = c(0, 0.5),
            legend.key.width = unit(0.4, "cm"),
            legend.key.height = unit(0.4, "cm"),
            legend.background = element_rect(fill = "transparent"))+
      guides(fill = guide_legend(ncol = 2))
    print(crime_distribute)
    
    
    filename_fig<-paste0("./Pic/FigureS4_parts/NC_",select_type,".jpg")
    ggsave(filename_fig,crime_distribute,width = 4,height = 4,dpi = 300)
  }
  
  ######################################## Mapping the Distribution of Two Types of Crime in Oregon ############################
  for(select_type in select_types){
    
    show_type<-show_crime_type(select_type)
    
    shp<-st_read("./Data/Geography/Oregon/BLM_OR_County_Boundaries_Polygon_Hub.shp")%>%
      st_transform(crs = 4326)%>%
      filter(grepl("OR", COBCODE))
    crime_long<-read.csv("./Data/Oregon/Oregon Crime Data.csv")%>%
      subset(crime_type==select_type)
    crime_long<-subset(crime_long,Year==max(unique(crime_long$Year)))
    
    crime_long$County<-toupper(crime_long$County)
    shp$COUNTY_NAM<-paste0(shp$COUNTY_NAM," County")
    shp$COUNTY_NAM<-toupper(shp$COUNTY_NAM)
    crime_shp<-shp %>%
      left_join(crime_long, by = c("COUNTY_NAM" = "County"))
    
    fillname<-paste0("The number of\n ",show_type,"\noffense")
    fig_title<-paste0("Oregon - ", select_type, " (",max(unique(crime_long$Year)),")")
    
    if(select_type=="Murder"){
      colours<-c("#4BAB25","#F1CBE4","#E783C1","#CF1B89")
      breaks<-c(0,5,10,15,max(crime_shp$value,na.rm = TRUE))
      brk_labels<-c("0 ~ 5","5 ~ 10","10 ~ 15","> 15")
    }else{
      colours<-c("#E0F7FA","#80DEEA","#26C6DA","#FFA726","#E65100")
      breaks<-c(0,500,2500,5000,8000,max(crime_shp$value,na.rm = TRUE))
      brk_labels<-c("0 ~ 500","500 ~ 2500","2500 ~ 5000","5000 ~ 8000","> 8000")
    }
    crime_shp$color_level <- cut(crime_shp$value, breaks = breaks, include.lowest = TRUE, right = FALSE,labels = brk_labels)
    
    # Fig S4 cd left
    crime_distribute<-ggplot(data = crime_shp)+
      geom_sf(aes(fill=color_level))+
      labs(fill=fillname,title = fig_title)+
      coord_sf(xlim = c(st_bbox(crime_shp)["xmin"],st_bbox(crime_shp)["xmax"]), ylim = c(st_bbox(crime_shp)["ymin"]-1,st_bbox(crime_shp)["ymax"]))+
      scale_fill_manual(values = colours,labels=brk_labels,na.value = "gray")+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5,size=10),
            panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
            axis.title = element_blank(),
            title = element_text(size=14,face="bold",hjust = 0.5),
            legend.title = element_text(size = 8, hjust = 0.5,face = "bold"),
            legend.text = element_text(size =7, vjust = 0.5,face = "bold"),
            legend.position = c(0.1, 0.1),
            legend.direction = "horizontal",
            legend.justification = c(0, 0.5),
            legend.key.width = unit(0.4, "cm"),
            legend.key.height = unit(0.4, "cm"),
            legend.background = element_rect(fill = "transparent"))+
      guides(fill = guide_legend(ncol = 3))
    print(crime_distribute)
    
    
    filename_fig<-paste0("./Pic/FigureS4_parts/Oregon_",select_type,".jpg")
    ggsave(filename_fig,crime_distribute,width = 4,height = 4,dpi = 300)
  }
  
  ######################################## Mapping the Distribution of Two Types of Crime in WIsconsin ############################
  for(select_type in select_types){
    
    show_type<-show_crime_type(select_type)
    
    shp<-st_read("./Data/Geography/Wisconsin/WI_CensusTL_Counties_2019.shp")%>%
      st_transform(crs = 4326)
    crime_data<-read.csv("./Data/Winsconsin/Winsconsin Crime Data.csv")
    crime_long<-pivot_longer(crime_data,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                             names_to = "crime_type",values_to = "value")%>%
      subset(crime_type==select_type&Year==max(unique(crime_data$Year)))
    
    crime_long$County<-toupper(crime_long$County)
    shp$NAMELSAD<-toupper(shp$NAMELSAD)
    crime_shp<-shp %>%
      left_join(crime_long, by = c("NAMELSAD" = "County"))
    
    fillname<-paste0("The number of\n ",show_type,"\noffense")
    fig_title<-paste0("Wisconsin - ", select_type, " (",max(unique(crime_data$Year)),")")
    
    if(select_type=="Murder"){
      colours<-c("#94CE67","#4BAB25","#F1CBE4","#E783C1","#CF1B89")
      breaks<-c(0,1,5,10,15,max(crime_shp$value))
      brk_labels<-c("0 ~ 1","1 ~ 5","5 ~ 10","10 ~ 15","> 15")
      label_pos <- c(0.5, 0.85)
    }else{
      colours<-c("#E0F7FA","#80DEEA","#26C6DA","#FFA726","#E65100")
      breaks<-c(0,250,1000,2500,5000,max(crime_shp$value))
      brk_labels<-c("0 ~ 250","250 ~ 1000","1000 ~ 2500","2500 ~ 5000","> 5000")
      label_pos <- c(0.45, 0.85)
    }
    crime_shp$color_level <- cut(crime_shp$value, breaks = breaks, include.lowest = TRUE, right = FALSE)
    
    
    # Fig S4 ef left
    crime_distribute<-ggplot(data = crime_shp)+
      geom_sf(aes(fill=color_level))+
      labs(fill=fillname,title = fig_title)+
      coord_sf(xlim = c(st_bbox(crime_shp)["xmin"]-0.2,st_bbox(crime_shp)["xmax"]), ylim = c(st_bbox(crime_shp)["ymin"],st_bbox(crime_shp)["ymax"]))+
      scale_fill_manual(values = colours,labels=brk_labels,na.value = "gray")+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5,size=10),
            panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
            axis.title = element_blank(),
            title = element_text(size=14,face="bold",hjust = 0.5),
            legend.title = element_text(size = 8, hjust = 0.5,face = "bold"),
            legend.text = element_text(size = 7, vjust = 0.5,face = "bold"),
            legend.position = c(0.01, 0.2),
            legend.direction = "vertical",
            legend.justification = c(0, 0.5),
            legend.key.width = unit(0.4, "cm"),
            legend.key.height = unit(0.4, "cm"),
            legend.background = element_rect(fill = "transparent"))+
      guides(fill = guide_legend(nrow=5))
    print(crime_distribute)
    
    
    filename_fig<-paste0("./Pic/FigureS4_parts/Wisconsin_",select_type,".jpg")
    ggsave(filename_fig,crime_distribute,width = 4,height = 4,dpi = 300)
  }
  
  ############################################## Scale fitting for crime, arrests, and supervision ######################################
  
  # Plot the fit for the selected crime type
  for(select_type in select_types){
    show_type<-show_crime_type(select_type)
    states<-c("North Carolina","Oregon","Wisconsin")
    for(state in states){
      
      if(state=="North Carolina"){
        
        # Read crime data
        crime_data<-read.csv("./Data/North Carolina/North Carolina Crime Data.csv")
        population<-read.csv("./Data/North Carolina/North Carolina Population Data.csv")
        crime<-merge(crime_data,population,by.x = c("County","Year"),by.y = c("County","Year"))
        
        crime_long<-pivot_longer(crime,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                                 names_to = "crime_type",values_to = "value")%>%
          subset(value>0&Population>0&crime_type==select_type&Year==max(unique(crime_data$Year)))
        
        # Individuals with criminal intent (offense)
        offenser<-read.csv("./Data/North Carolina/North Carolina Arrest Data.csv")
        offenser<-merge(offenser,population,by.x = c("County","Year"),by.y = c("County","Year"))
        offenser_long<-pivot_longer(offenser,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                                    names_to = "crime_type",values_to = "value")%>%
          subset(value>0&Population>0&crime_type==select_type&Year==max(unique(crime_data$Year)))
        
        # Effective guardian
        guardian<-read.csv("./Data/North Carolina/North Carolina Law Employees Data.csv")
        guardian<-merge(guardian,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
          subset(Law_Employees>0&Year==max(unique(crime_data$Year)))
      }else if(state=="Oregon"){
        
        # Read crime data
        crime_data<-read.csv("./Data/Oregon/Oregon Crime Data.csv")
        population<-read.csv("./Data/Oregon/Oregon Population Data.csv")
        crime_long<-merge(crime_data,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
          subset(value>0&Population>0&crime_type==select_type&Year==max(unique(crime_data$Year)))
        # Effective guardian
        guardian<-read.csv("./Data/Oregon/Oregon Law Employees Data.csv")
        guardian<-merge(guardian,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
          subset(Law_Employees>0&Year==max(unique(crime_data$Year)))
        # Individuals with criminal intent (offense)
        offenser<-read.csv("./Data/Oregon/Oregon Arrest Data.csv")
        offenser_long<-merge(offenser,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
          subset(value>0&Population>0&crime_type==select_type&Year==max(unique(crime_data$Year)))
      }else if(state=="Wisconsin"){
        # Read crime data
        crime_data<-read.csv("./Data/Winsconsin/Winsconsin Crime Data.csv")
        population<-read.csv("./Data/Winsconsin/Winsconsin Population Data.csv")
        crime<-merge(crime_data,population,by.x = c("County","Year"),by.y = c("County","Year"))
        crime_long<-pivot_longer(crime,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                                 names_to = "crime_type",values_to = "value")%>%
          subset(value>0&Population>0&crime_type==select_type&Year==max(unique(crime_data$Year)))
        # Effective guardian
        guardian<-read.csv("./Data/Winsconsin/Winsconsin Law Employees Data.csv")
        guardian<-merge(guardian,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
          subset(Law_Employees>0&Year==max(unique(crime_data$Year)))
        # Individuals with criminal intent (offense)
        offenser<-read.csv("./Data/Winsconsin/Winsconsin Arrest Data.csv")
        offenser<-merge(offenser,population,by.x = c("County","Year"),by.y = c("County","Year"))
        offenser_long<-pivot_longer(offenser,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                                    names_to = "crime_type",values_to = "value")%>%
          subset(value>0&Population>0&crime_type==select_type&Year==max(unique(crime_data$Year)))
      }
      
      guardian <- Outlier_Remove(data = guardian, select_type = "Guardian")
      offenser_long <- Outlier_Remove(data = offenser_long, select_type = "Offense")
      crime_long <- Outlier_Remove(data = crime_long, select_type = "Crime")
      
      guardian_plot <- guardian %>%
        select(County, Year, Population, Value = Law_Employees) %>%
        mutate(Type = "Guardian")
      guardian_model<-lm(log10(Law_Employees)~log10(Population),data = guardian)
      
      
      offense_plot <- offenser_long %>%
        select(County, Year, Population, Value = value) %>%
        mutate(Type = "Offenser")
      offenser_model<-lm(log10(value)~log10(Population),data=offenser_long)
      
      crime_plot <- crime_long %>%
        select(County, Year, Population, Value = value) %>%
        mutate(Type = show_type)
      crime_model<-lm(log10(value)~log10(Population),data = crime_long)
      
      combined_plot <- bind_rows(guardian_plot, offense_plot, crime_plot)
      combined_line<-data.frame(Type=c("Guardian","Offenser",show_type),
                                inter=c(coef(guardian_model)[1],coef(offenser_model)[1],coef(crime_model)[1]),
                                slope=c(coef(guardian_model)[2],coef(offenser_model)[2],coef(crime_model)[2]),
                                r2=c(summary(guardian_model)$r.squared,summary(offenser_model)$r.squared,summary(crime_model)$r.squared))
      combined_text<-data.frame(Type=c("Guardian","Offenser",show_type),
                                texts=c(
                                  paste("y=", round(coef(guardian_model)[1], 2), "+", round(coef(guardian_model)[2], 2), "x, R²=",round(summary(guardian_model)$r.squared,2)),
                                  paste("y=", round(coef(offenser_model)[1], 2), "+", round(coef(offenser_model)[2], 2), "x, R²=",round(summary(offenser_model)$r.squared,2)),
                                  paste("y=", round(coef(crime_model)[1], 2), "+", round(coef(crime_model)[2], 2), "x, R²=",round(summary(crime_model)$r.squared,2))
                                ),
                                x=c(3.3,3.3,3.3),
                                y=c(4.2,3.8,4.6))
      
      combined_plot$Type<-factor(combined_plot$Type,levels = c(show_type,"Guardian","Offenser"))
      combined_line$Type<-factor(combined_line$Type,levels = c(show_type,"Guardian","Offenser"))
      combined_text$Type<-factor(combined_text$Type,levels = c(show_type,"Guardian","Offenser"))
      
      if(select_type == "Murder"){
        color_values <- c("#c82423", "#578D2F", "#FFABDF")
      }else{
        color_values <- c("#8FAED1", "#578D2F", "#FFABDF")
      }
      
      # Fig S4 a-f right
      p<-ggplot(combined_plot,aes(x=log10(Population),y=log10(Value),color=Type))+
        geom_point(alpha=0.7,size=2)+
        labs(x="lg(Population)",y="lg(Value)")+
        scale_color_manual(values = color_values) +
        theme(panel.background = element_rect(fill = "transparent", colour = NA),
              panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
              axis.title = element_text(size=14),
              axis.text.x = element_text(size = 12,angle = 90),
              axis.text.y = element_text(size = 12),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              legend.position = c(0,0.75),
              legend.justification=c(0,1),
              text = element_text(family = "serif"),
              legend.background = element_rect(fill = "transparent"))+
        geom_abline(data = combined_line,aes(intercept = inter,slope = slope,color=Type),linewidth=0.8,show.legend = FALSE)+
        geom_text(data = combined_text,aes(x=x,y=y,label = texts,color=Type),hjust = 0, size = 4.5,show.legend = FALSE)+
        guides(color = guide_legend(override.aes = list(size=5)))
      print(p)
      
      filename<-paste0("./Pic/FigureS4_parts/",state,"_",select_type,"_scaling.jpg")
      ggsave(filename,p,width = 4,height = 4,dpi = 300)
      
    }
  }
}



library("ggplot2")
library("ggpubr")
library("dplyr")
library("tidyr")
library("grid")
library("gridExtra")
library("cowplot")
library("patchwork")
library("RColorBrewer")
library("sf")
library("classInt")
library("stringr")
library("openxlsx")
library("leaflet")
library("datasets")
library("htmlwidgets")
library("ggspatial")
library("ggrepel")
library("reshape2")
setwd("E:/ZhangTao/Science/犯罪标度律/USCode")


# Remove Outliers
Outlier_Remove_PointsFitting_Kinds <- function(crime_data, crime_type){
  
  dat <- crime_data
  valid_index <- which(dat[[crime_type]] > 0 & dat$Population > 0)
  valid_dat <- dat[valid_index, ]
  
  copy_dat <- valid_dat
  
  valid_dat$log_popu <- log10(valid_dat$Population)
  valid_dat$log_crime <- log10(valid_dat[[crime_type]])
  
  ori_model <- lm(log_crime ~ log_popu, data = valid_dat)
  
  valid_dat$res <- residuals(ori_model)
  
  res_mean <- mean(valid_dat$res, na.rm = TRUE)
  res_sd <- sd(valid_dat$res, na.rm = TRUE)
  
  valid_dat$outlier <- abs(valid_dat$res - res_mean) > 2 * res_sd
  final_index <- which(valid_dat$outlier == FALSE)
  
  result <- copy_dat[final_index, ]
  return(result)
}


# Draw the fitted scatter plots of various crime types in different years.
PointsFitting_kinds<-function(){
  years<-2001:2022
  
  for(year in years){
    CrimeData <- read.csv("./PyCode/ProcessedData/Crime Statistic/American_Place_Crime_From_2001_to_2022.csv")
    CrimeData <- subset(CrimeData, Year == year)
    kinds <- c(
      "Murder",
      "Rape",
      "Robbery",
      "Aggravated",
      "Burglary",
      "LarcenyTheft",
      "MotorTheft",
      "Arson"
    )
    
    slopelist<-list()
    interceptList<-list()
    R2list<-list()
    ci<-list()
    counts<-1
    
    for (kind in kinds) {
      validdf <- Outlier_Remove_PointsFitting_Kinds(CrimeData, kind)
      
      validdf$logPopu <- log10(validdf$Population)
      validdf$logCrime <- log10(validdf[, kind])
      
      model <- lm(logCrime ~ logPopu, data = validdf)
      summaryModel <- summary(model)
      
      slopelist[[counts]]<-coef(model)[2]
      interceptList[[counts]]<-coef(model)[1]
      R2list[[counts]]<-round(summaryModel$r.squared, 2)
      
      cit<-confint(model)
      ci[[counts]]<-list()
      ci[[counts]][[1]]<-cit[2,1]
      ci[[counts]][[2]]<-cit[2,2]
      ci[[counts]][[3]]<-kind
      ci[[counts]][[4]]<-summary(model)$coefficients[,"Pr(>|t|)"]
      
      counts<-counts+1
    }
    
    
    CrimeData_long <- pivot_longer(CrimeData[,c("State","City","Population","Murder","Rape","Robbery","Aggravated","Burglary","LarcenyTheft","MotorTheft","Arson")], 
                                   cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson), names_to = "CrimeType", values_to = "num")
    CrimeData_long<-subset(CrimeData_long,num>0)
    AggIndex<-which(CrimeData_long$CrimeType=="Aggravated")
    CrimeData_long$CrimeType[AggIndex]<-"Aggravated assault"
    LarIndex<-which(CrimeData_long$CrimeType=="LarcenyTheft")
    CrimeData_long$CrimeType[LarIndex]<-"Larceny-Theft"
    MotIndex<-which(CrimeData_long$CrimeType=="MotorTheft")
    CrimeData_long$CrimeType[MotIndex]<-"Motor vehicle Theft"
    CrimeData_long$CrimeType <- factor(CrimeData_long$CrimeType, levels = c("Murder","Rape","Robbery","Aggravated assault","Burglary","Larceny-Theft","Motor vehicle Theft","Arson"))
    
    fitData<-data.frame(CrimeType = c("Murder","Rape","Robbery","Aggravated assault","Burglary","Larceny-Theft","Motor vehicle Theft","Arson"), Slope = unlist(slopelist), Intercept = unlist(interceptList),R2=unlist(R2list))
    fitData$CrimeType<-factor(fitData$CrimeType, levels = c("Murder","Rape","Robbery","Aggravated assault","Burglary","Larceny-Theft","Motor vehicle Theft","Arson"))
    
    p<-ggplot(data=CrimeData_long)+
      geom_point(mapping = aes(x = log10(Population), y = log10(num)),size=0.1)+
      labs(x= "Log10[Population]",y="Log10[Crime]")+
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
            strip.text = element_text(size = 14, hjust = 0.5,face = "bold"),
            strip.background = element_rect(fill = NA, color = NA),
            axis.title = element_text(size=16),
            axis.text = element_text(size = 14),)+
      scale_y_continuous(breaks = seq(0,5,by=1),limits = c(0,5))+
      scale_x_continuous(breaks = seq(1,6,by=1),limits=c(1,6))+
      facet_wrap(~ CrimeType,ncol = 4,nrow = 2)+
      geom_abline(data = fitData, aes(intercept = Intercept, slope = Slope, group = 1), color = "red",linewidth=0.8)+
      geom_text(data = fitData, aes(x = 1, y = 4.8, label=paste("y=", round(as.numeric(Intercept), 2), "+", round(as.numeric(Slope), 2), "x")), 
                color = "blue", size = 4.5,hjust=0)+
      geom_text(data = fitData, aes(x = 1, y = 3.9, label=paste("R²:", round(R2, 2))), 
                color = "blue", size = 4.5,hjust=0)
    print(p)
    ggsave(
      str_c("./RCode/Pic/Scatter plots of different crime types in different years/",year,".jpg"),
      p,
      width = 8,
      height = 4,
      dpi = 300
    )
  }
}
