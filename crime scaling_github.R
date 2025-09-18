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






