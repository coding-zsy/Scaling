## Import the required libraries
library("ggplot2")
library("dplyr")
library("tidyr")
library("stringr")
library("openxlsx")
library("ggpubr")
library("patchwork")
library("sf")
library(ggbreak)
library(ggforce)
library(ggpmisc)

theme_set(theme_bw())
setwd("C:/Users/13593/Desktop/crime_scaling")


#=============================== Figure. 1 ============================================
urtdata <- read.csv("C:/Users/13593/Desktop/crime_scaling/data/US/america_rate.csv", 
                    header=TRUE, sep=",",na.strings = 0) 

# Convert the dataset to long format
urt_long <- urtdata %>%
  select(bgrt, ltfrt) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Calculate the median values
median_values <- urt_long %>%
  group_by(Variable) %>%
  summarize(median_val = round(median(Value, na.rm = TRUE), 2))

# Create label strings for the median values
median_labels <- paste0(median_values$Variable, ": ", median_values$median_val)

# Create display names for legend and annotation
name_map <- c("bgrt" = "Burglary", "ltfrt" = "LarcenyTheft")

# Replace variable names using name_map for annotation
median_labels <- paste0(name_map[median_values$Variable], ": ", median_values$median_val)

urt <- ggplot(urt_long, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5, color = "black",size=0.35) +  # Density curve with fill and black border
  geom_vline(data = median_values, aes(xintercept = median_val, color = Variable),
             linetype = "dashed", size = 0.35, show.legend = FALSE) +  # Add dashed line for median
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(0, 4500)) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = paste("Median\n", paste(median_labels, collapse = "\n")), 
           hjust = 1.3, vjust = 1.5, size = 1.8) +  # Add top-right median annotation
  scale_fill_manual(values = c("bgrt" = "#E7A3A3", "ltfrt" = "#8FAED1"),
                    labels = name_map) +  # Modify legend labels
  scale_color_manual(values = c("bgrt" = "#D86A6A", "ltfrt" = "#5967BB"),
                     labels = name_map) +  # Modify dashed line color labels
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



frtdata <- read.csv("C:/Users/13593/Desktop/crime_scaling/data/France/france_rate.csv", 
                    header=TRUE, sep=",",na.strings = 0) # Read data and treat 0 as NA

# Convert the dataset to long format
frt_long <- frtdata %>%
  select(bgrt, ltfrt) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Calculate the median values
median_values2 <- frt_long %>%
  group_by(Variable) %>%
  summarize(median_val2 = round(median(Value, na.rm = TRUE), 2))

# Create label strings for the median values
median_labels2 <- paste0(median_values2$Variable, ": ", median_values2$median_val2)

# Create display names for legend and annotation
name_map <- c("bgrt" = "Burglary", "ltfrt" = "Theft")

# Replace variable names using name_map for annotation
median_labels2 <- paste0(name_map[median_values2$Variable], ": ", median_values2$median_val2)

frt <- ggplot(frt_long, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5, color = "black",size=0.35) +  # Density curve with fill and black border
  geom_vline(data = median_values2, aes(xintercept = median_val2, color = Variable),
             linetype = "dashed", size = 0.35, show.legend = FALSE) +  # Add dashed line for median
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(0, 2500)) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = paste("Median\n", paste(median_labels2, collapse = "\n")), 
           hjust = 1.3, vjust = 1.5, size = 1.8) +  # Add top-right median annotation
  scale_fill_manual(values = c("bgrt" = "#E7A3A3", "ltfrt" = "#8FAED1"),
                    labels = name_map) +  # Modify legend labels
  scale_color_manual(values = c("bgrt" = "#D86A6A", "ltfrt" = "#5967BB"),
                     labels = name_map) +  # Modify dashed line color labels
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



irtdata <- read.csv("C:/Users/13593/Desktop/crime_scaling/data/India/india_rate.csv", 
                    header=TRUE, sep=",",na.strings = 0) # Read data and treat 0 as NA

# Convert the dataset to long format
irt_long <- irtdata %>%
  select(bgrt, ltfrt) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Calculate the median values
median_values3 <- irt_long %>%
  group_by(Variable) %>%
  summarize(median_val3 = round(median(Value, na.rm = TRUE), 2))

# Create label strings for the median values
median_labels3 <- paste0(median_values3$Variable, ": ", median_values3$median_val3)

# Create display names for legend and annotation
name_map <- c("bgrt" = "Burglary", "ltfrt" = "Theft")

# Replace variable names using name_map for annotation
median_labels3 <- paste0(name_map[median_values3$Variable], ": ", median_values3$median_val3)

irt <- ggplot(irt_long, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5, color = "black", size = 0.35) +  # Density curve with fill and black border
  geom_vline(data = median_values3, aes(xintercept = median_val3, color = Variable),
             linetype = "dashed", size = 0.35, show.legend = FALSE) +  # Add dashed line for median
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(0, 130)) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = paste("Median\n", paste(median_labels3, collapse = "\n")), 
           hjust = 1.3, vjust = 1.5, size = 1.8) +  # Add top-right median annotation
  scale_fill_manual(values = c("bgrt" = "#E7A3A3", "ltfrt" = "#8FAED1"),
                    labels = name_map) +  # Modify legend labels
  scale_color_manual(values = c("bgrt" = "#D86A6A", "ltfrt" = "#5967BB"),
                     labels = name_map) +  # Modify dashed line color labels
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


tiff(file="Figure 1.tiff", res = 600, width = 3600, height = 1200, compression = "lzw")
ggarrange(urt,frt,irt,
          nrow = 1,ncol=3,
          labels = c( "a", "b","c"),
          align = "h",font.label = list(size = 11))
dev.off()
getwd()



#=============================== Figure. 2 ============================================

############## Fig. 2 US
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
CrimeData <- read.csv("C:/Users/13593/Desktop/crime_scaling/data/US/American_Place_Crime_From_2001_to_2022.csv")

# Process total crime
total_results <- do.call(rbind, lapply(years, function(y){
  year_data <- subset(CrimeData, Year == y)
  year_data$Crime <- year_data$Violent.crime + year_data$Property.crime
  c(y, calc_beta_ci(year_data, "Crime"))
}))
draw_total <- as.data.frame(total_results)
colnames(draw_total) <- c("Year", "beta", "CILower", "CIUpper")
draw_total$CrimeType <- "Total Crime"


# Process multiple crime types
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

selected_data <- CrimeData[, c("State", "City", "Population", "Murder", "LarcenyTheft", "Year")]

# Pivot data to long format
CrimeData_long <- selected_data %>%
  pivot_longer(cols = c(Murder, LarcenyTheft),
               names_to = "type",
               values_to = "case")

# Filter data for year 2022
CrimeData_long2 <- CrimeData_long %>%
  filter(Year == 2022,
         Population > 0,
         case > 0)

# Remove outliers (beyond 2 standard deviations)
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

# Remove outliers by crime type
CrimeData_cleaned <- CrimeData_long2 %>%
  group_by(type) %>%
  group_modify(~remove_outliers(.x)) %>%
  ungroup()

# Regression fitting plot
us_p1 <- ggplot(CrimeData_cleaned, 
                aes(log10(Population),
                    log10(case),
                    colour = factor(type))) +
  # Light-colored scatter points
  geom_point(aes(colour=factor(type)), size=0.5, shape=21, stroke = 0.3) +
  # Placeholder for regression lines (not shown directly)
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1,
    aes(group = type),
    color = NA
  ) +
  # Add regression line for each crime type (darker colors)
  geom_smooth(data = subset(CrimeData_long2, type == "Murder"),
              method = "lm", se = FALSE, linewidth = 0.7, color = "#D86A6A") +  # Murder (red)
  geom_smooth(data = subset(CrimeData_long2, type == "LarcenyTheft"),
              method = "lm", se = FALSE, linewidth = 0.7, color = "#5967BB") +  # LarcenyTheft (blue)
  # Point colors
  scale_color_manual(values = c(
    "Murder" = "#E7A3A3",          # light red
    "LarcenyTheft" = "#8FAED1"     # light blue
  )) +
  # Add regression equation and R²
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
    title = "United States (2022)",  # Add title
    x = expression(paste(Log[10], "[Population]")),
    y = expression(paste(Log[10], "[Crimes]"))
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black"),
        axis.text = element_text(size = 6), 
        axis.title.y = element_text(size = 6),
        axis.title.x = element_text(size = 7),
        plot.title = element_text(size = 8, hjust = 0.5),  # Title size and centering
        legend.position = c(0.2, 0.7),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size = unit(3, 'mm'),
        legend.text = element_text(size = 5),
        legend.key = element_blank()   )       # Remove black box from legend


calc_beta_ci <- function(data, crime_var, fold = 2){
  # Remove invalid values (positive before log)
  data <- subset(data, data[[crime_var]] > 0 & data$Population > 0)
  
  if(nrow(data) <= 5){
    return(c(0, 0, 0))
  } else {
    # Initial regression
    model <- lm(log10(data[[crime_var]]) ~ log10(Population), data = data)
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    
    # Compute residuals
    data$residuals <- log10(data[[crime_var]]) - (slope * log10(data$Population) + intercept)
    
    # Remove outliers beyond 2 SD
    res_mean <- mean(data$residuals, na.rm = TRUE)
    res_sd <- sd(data$residuals, na.rm = TRUE)
    data_clean <- subset(data, 
                         residuals > res_mean - fold * res_sd & 
                           residuals < res_mean + fold * res_sd)
    
    if(nrow(data_clean) <= 5){
      return(c(0, 0, 0))
    }
    
    # Refit regression after cleaning
    clean_model <- lm(log10(data_clean[[crime_var]]) ~ log10(Population), data = data_clean)
    ci <- confint(clean_model)
    
    return(c(coef(clean_model)[2], ci[2, 1], ci[2, 2]))  # β, CI lower, CI upper
  }
}


crimekinds <- c("Murder","Rape","Robbery","Aggravated","Burglary","LarcenyTheft","MotorTheft","Arson")
years <- 2001:2022
CrimeData <- read.csv("C:/Users/13593/Desktop/crime_scaling/data/US/American_Place_Crime_From_2001_to_2022.csv")

# Process total crime
total_results <- do.call(rbind, lapply(years, function(y){
  year_data <- subset(CrimeData, Year == y)
  year_data$Crime <- year_data$Violent.crime + year_data$Property.crime
  c(y, calc_beta_ci(year_data, "Crime"))
}))
draw_total <- as.data.frame(total_results)
colnames(draw_total) <- c("Year", "beta", "CILower", "CIUpper")
draw_total$CrimeType <- "Total Crime"

# Process individual crime types
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


# Replace names in CrimeType for display
draw_all$CrimeType[draw_all$CrimeType == "Aggravated"] <- "Aggravated assault"

draw_all <- subset(draw_all, !CrimeType %in% c("Total Crime"))
# Define the desired order
# 1. Set custom order (use “——” as a visual separator if needed)
desired_order <- c("Murder", "Rape", "Arson", "Aggravated assault",
                   "LarcenyTheft", "MotorTheft", "Robbery", "Burglary")

# 2. Convert CrimeType to factor
draw_all$CrimeType <- factor(draw_all$CrimeType, levels = desired_order)

# 3. Define color palette (assign transparent color to separators if needed)
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
  geom_line(aes(linetype = CrimeType, size = CrimeType)) +  # Add linetype mapping
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
  scale_linetype_manual(values = c(  # Define line type mapping
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
        legend.key.size = unit(0.1, "cm"),  
        legend.key.height = unit(0.05, "cm"),  
        legend.key.width = unit(0.2, "cm"),   
        legend.spacing.x = unit(0.05, "cm"),      
        legend.spacing.y = unit(0.01, "cm"),     
        legend.background = element_blank(),  
        legend.key = element_blank()          
  )+
  guides(color = guide_legend(ncol = 2, byrow = FALSE),
         size = "none",  
         linetype = "none"  
  )


############ Fig. 2 France

mydata <- read.csv("C:/Users/13593/Desktop/crime_scaling/data/France/france_crime.csv", 
                   header=TRUE, sep=",",na.strings = 0) # Set 0 values as NA when reading data
mydata <- mydata %>%
  mutate(classe = ifelse(
    classe == "Violent theft without weapons",
    "Theft without weapons",
    classe
  ))
mydata$faits <- as.numeric(mydata$faits)

mydata <- subset(mydata, !classe %in% c("Other assault and battery", "Theft of vehicle accessories"))

# Filter data using 2 times the standard deviation
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
    low2 <- round(confint.lm(result)[2],4)# 95% CI lower bound
    up2 <- round(confint.lm(result)[4],4)# 95% CI upper bound
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



# Set legend order
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
  # Add individual regression lines for each category (dark colors)
  geom_smooth(data = subset(subset_data, classe == "Burglary"),
              method = "lm", se = FALSE, linewidth = 0.7, color = "#D86A6A") +  # Burglary
  geom_smooth(data = subset(subset_data, classe == "Theft against individuals"),
              method = "lm", se = FALSE, linewidth = 0.7, color = "#5967BB") +  # Theft against individuals
  # Point colors
  scale_color_manual(values = c(
    "Burglary" = "#E7A3A3",          
    "Theft against individuals" = "#8FAED1"     
  )) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = '~~~')),
    formula = y ~ x,
    parse = TRUE,
    size = 2,
    vstep = 0.055 # Adjust label spacing
  ) +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(
    title = "France (2024)",
    x = expression(paste(Log[10],"[Population]")),  # Modify X-axis label
    y = expression(paste(Log[10],"[Crimes]"))       # Modify Y-axis label
  ) +
  #scale_color_manual(values = region_colors) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black"),
        plot.title = element_text(size = 8, hjust = 0.5)  # Adjust title size and center
  ) +
  theme(axis.text=element_text(size=6), 
        axis.title.y=element_text(size=6.3),axis.title.x=element_text(size=7))+
  theme(legend.position = c (0.3,0.7),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 5),
        legend.key = element_blank() )         # Remove small black border

mydata <- read.csv("C:/Users/13593/Desktop/crime_scaling/data/France/crime_regression_classe_results_filteredoutlier.csv", 
                   header=TRUE, sep=",",na.strings = 0) # Set 0 values as NA when reading data

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


# 1. Add custom order (use an empty factor “——” to separate columns)
desired_order2 <- c("Sexual violence", "Assault and battery", "Drug trafficking", "Burglary",
                    "Fraud","Drug use","Theft against individuals", "Theft from vehicles", "Theft without weapons","Property damage")

# 2. Set CrimeType as factor
mydata1$classe <- factor(mydata1$classe, levels = desired_order2)


fc_p2 <- ggplot(mydata1, aes(x = year, y = beta, group = classe, color = classe)) +
  geom_point(size=0.45) +  # Keep point size unchanged
  geom_line(aes(size = classe, linetype = classe)) +  # Add linetype mapping
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(x="Year", y="Scaling exponent (β)") +
  scale_color_manual(values = c(
    "Theft without weapons" = "#9bbf8a",  
    "Drug trafficking" = "#fa8878", 
    "Sexual violence" = "#f79059",
    "Assault and battery" = "#ffbe7a",
    "Property damage" = "#add3e2",
    "Burglary" = "#c29484",
    "Theft against individuals" = "#82afda",
    "Theft from vehicles" = "#8dcec8",
    "Fraud" = "#c2bdde",
    "Drug use" = "#3480b8"
  )) +  
  # Unify line width as 0.35
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
  # Set line type (solid for 0.5, dashed for 0.25)
  scale_linetype_manual(values = c(
    "Theft without weapons" = "dashed",  
    "Drug trafficking" = "solid",        
    "Sexual violence" = "solid",         
    "Assault and battery" = "solid",     
    "Property damage" = "dashed",        
    "Burglary" = "solid",                
    "Theft against individuals" = "dashed",  
    "Theft from vehicles" = "dashed",    
    "Fraud" = "dashed",                  
    "Drug use" = "dashed"                
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
        legend.key.size = unit(0.1, "cm"),  
        legend.key.height = unit(0.05, "cm"),  
        legend.key.width = unit(0.2, "cm"),   
        legend.spacing.x = unit(0.05, "cm"),      
        legend.spacing.y = unit(0.01, "cm"),     
        legend.background = element_blank(),  
        legend.key = element_blank() )+         
  guides(
    color = guide_legend(ncol = 2, byrow = FALSE),
    size = "none",       
    linetype = "none"    
  )




###########Fig. 2 India
mydata <- read.csv("C:/Users/13593/Desktop/crime_scaling/data/India/india_crime.csv", 
                   header=TRUE, sep=",",na.strings = 0) # Set 0 values as NA when reading data

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
                        `Causing..Death.by..Negligence`),   # Columns to be transformed
               names_to = "type",              # New column name to store the original column names (crime type)
               values_to = "case")             # New column name to store numeric values (crime count)

mydata_long <- mydata_long %>%
  mutate(
    type = recode(
      type,
      "Kidnapping.....Abduction" = "Kidnapping and Abduction",
      "Cruelty.by..Husband.or..Relatives" = "Cruelty by Relatives",
      "Causing..Death.by..Negligence" = "Causing Death by Negligence"
    )
  )


# Two-standard-deviation filtering
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
    low2 <- round(confint.lm(result)[2],4)# Lower bound of 95% CI
    up2 <- round(confint.lm(result)[4],4)# Upper bound of 95% CI
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
# Set legend order
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
  # Add regression line separately for each crime type (indiadata1)
  geom_smooth(data = subset(indiadata1, type == "Murder"),
              method = "lm", se = FALSE, linewidth = 0.7, color = "#D86A6A") +  # Murder (red)
  geom_smooth(data = subset(indiadata1, type == "Theft"),
              method = "lm", se = FALSE, linewidth = 0.7, color = "#5967BB") +  # Theft (blue)
  # Color settings for points
  scale_color_manual(values = c(
    "Murder" = "#E7A3A3",          # Light red
    "Theft" = "#8FAED1"            # Light blue
  )) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = '~~~')),
    formula = y ~ x,
    parse = TRUE,
    size = 2,
    vstep = 0.055 # Adjust font spacing
  ) +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(title = "India (2021)",
       x = expression(paste(Log[10],"[Population]")),  # Modify X-axis label
       y = expression(paste(Log[10],"[Crimes]"))       # Modify Y-axis label
  ) +
  #scale_color_manual(values = region_colors) +  # Specify colors by region
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
        legend.key = element_blank() )         # Remove small black box


mydata4 <- read.csv("C:/Users/13593/Desktop/crime_scaling/data/India/india_regression_results_filteredoutlier.csv", 
                    header=TRUE, sep=",",na.strings = 0)

mydata4 <- mydata4 %>%
  mutate(type = ifelse(
    type == "Kidnapping and Abduction",
    "Kidnapping&Abduction",
    type
  ))
# 1. Add a custom order (use “——” to visually separate columns)
desired_order3 <- c("Murder", "Rape", "Kidnapping&Abduction", "Burglary","Hurt",
                    "Causing Death by Negligence", "Theft", "Cheating","Robbery","Cruelty by Relatives")

# 2. Set CrimeType as factor
mydata4$type <- factor(mydata4$type, levels = desired_order3)


id_p2 <- ggplot(mydata4, aes(x = year, y = beta, group = type, color = type)) +
  geom_line(aes(size = type, linetype = type)) +  # Add linetype mapping
  geom_point(size=0.45) +  # Keep point size unchanged
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(x="Year", y="Scaling exponent (β)") +
  scale_x_continuous(breaks = c(2001, 2011, 2021)) +
  scale_color_manual(values = c(
    "Kidnapping&Abduction" = "#fa8878",  # Total color scheme
    "Murder" = "#c82423", 
    "Rape" = "#f79059",
    "Hurt" = "#ffbe7a",
    "Robbery" = "#9bbf8a",
    "Burglary" = "#c29484",
    "Theft" = "#82afda",
    "Cheating" = "#c2bdde",
    "Cruelty by Relatives" = "#fee1cf",
    "Causing Death by Negligence" = "#3480b8"
  )) +  # Set custom color palette
  # Set uniform line width to 0.35
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
  # Define line styles (solid or dashed)
  scale_linetype_manual(values = c(
    "Kidnapping&Abduction" = "solid",  # originally 0.5
    "Murder" = "solid",                # originally 0.5
    "Rape" = "solid",                  # originally 0.5
    "Hurt" = "solid",                  # originally 0.5
    "Robbery" = "dashed",              # originally 0.25
    "Burglary" = "solid",              # originally 0.5
    "Theft" = "dashed",                # originally 0.25
    "Cheating" = "dashed",             # originally 0.25
    "Cruelty by Relatives" = "solid",  # originally 0.5
    "Causing Death by Negligence" = "dashed"  # originally 0.25
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
        legend.spacing.y = unit(0, "cm"),   # Adjust vertical spacing
        legend.key.size = unit(0.1, "cm"),
        legend.key.height = unit(0.05, "cm"),  # Legend symbol height
        legend.key.width  = unit(0.2, "cm"),   # Legend symbol width
        legend.background = element_blank(),   # Remove legend border
        legend.key = element_blank())+         # Remove small black box
  guides(
    color = guide_legend(ncol = 2, byrow = FALSE),
    size = "none",       # Hide size legend
    linetype = "none"    # Hide linetype legend (to avoid duplication)
  )


tiff(file="Figure 2 country_crime_scaling.tiff", res = 600, width = 3600, height = 2800, compression = "lzw")
ggarrange(us_p1,fc_p1,id_p1,us_p2,fc_p2,id_p2,
          nrow = 2,ncol=3,
          labels = c( "a", "b","c","d","e", "f"),
          align = "h",font.label = list(size = 11))
dev.off()
getwd()
#================================== Figure. 4 =================================================
## Defining pre-variables
fl_crimekinds <- c("Murder", "Rape", "Robbery", "Aggravated", "Burglary", 
                   "LarcenyTheft", "MotorTheft")
other_crimekinds <- c("Murder", "Rape", "Robbery", "Aggravated", "Burglary", "LarcenyTheft",
                      "MotorTheft", "Arson")



## This function is used to remove outliers that appear during the fitting process.
## The standard is: twice the standard deviation
## The funciton is used in Figure 4 and S4
Outlier_Remove <- function(data, select_type){
  
  dat <- data
  col_name <- ""
  if(select_type == "Guardian"){col_name <- "Law_Employees"}
  else{col_name <- "value"}
  
  copy_dat <- dat
  dat$log_popu <- log10(dat$Population)
  dat$log_value <- log10(dat[[col_name]])
  
  ori_model <-lm(log_value ~ log_popu, data = dat)
  dat$res <-residuals(ori_model)
  
  res_mean <- mean(dat$res, na.rm = TRUE)
  res_sd <- sd(dat$res, na.rm = TRUE)
  
  dat$outlier <- abs(dat$res - res_mean) > 2 * res_sd
  final_index <- which(dat$outlier == FALSE)
  result <- copy_dat[final_index, ]
  return(result)
}



## This function is used to convert the crime type abbreviation into the crime type full name
Show_Crime_Type <- function(types){
  if(types == "Aggravated"){return("Aggravated assault")}
  else if(types == "LarcenyTheft"){return("Larceny-Theft")}
  else if(types == "MotorTheft"){return("Motor vehicle Theft")}
  else{return(types)}
}




## Figure 4
Figure4 <- function(){
  
  murthf <- Show_Crime_Type("Murder")
  larthf <- Show_Crime_Type("LarcenyTheft")
  mur_colors <- c("#4BAB25", "#94CE67", "#F1CBE4", "#E783C1", "#CF1B89")
  lar_colors <- c("#E0F7FA", "#80DEEA", "#26C6DA", "#FFA726", "#E65100")
  
  
  # Map the spatial distribution of two crime types in Florida
  # Fig 4a and Fig 4c
  
  fl_shp <- st_read("./data/Geography/Florida/FDEM_-_Regions.shp") %>% st_transform(crs = 4326)
  
  fl_crime_data <- read.csv("./data/Florida/Florida Crime Data.csv")
  fl_murder <- pivot_longer(fl_crime_data, cols = c(Murder, Rape, Robbery, Aggravated, Burglary, LarcenyTheft, MotorTheft),
                            names_to = "crime_type", values_to = "value") %>%
    subset(value > 0 & Population > 0 & crime_type == "Murder" & Year == max(unique(fl_crime_data$Year)))
  fl_larthf <- pivot_longer(fl_crime_data, cols = c(Murder, Rape, Robbery, Aggravated, Burglary, LarcenyTheft, MotorTheft),
                            names_to = "crime_type", values_to = "value") %>%
    subset(value > 0 & Population > 0 & crime_type == "LarcenyTheft" & Year == max(unique(fl_crime_data$Year)))
  
  fl_shp$NAME[fl_shp$NAME=="OSCEOLA"]<-"OSECOLA"
  fl_shp$NAME<-paste0(fl_shp$NAME," County")
  fl_shp$NAME<-toupper(fl_shp$NAME)
  fl_murder$County <- toupper(fl_murder$County)
  fl_larthf$County <- toupper(fl_larthf$County)
  
  # Fig 4a
  fl_larthf_shp <- fl_shp %>% left_join(fl_larthf, by = c("NAME" = "County"))
  larthf_breaks <- c(0, 100, 1000, 2500, 5000, max(fl_larthf_shp$value))
  larthf_labels <- c("0 ~ 100", "100 ~ 1000", "1000 ~ 2500", "2500 ~ 5000", "> 5000")
  fl_larthf_shp$color_level <- cut(fl_larthf_shp$value, breaks = larthf_breaks, 
                                   include.lowest = TRUE, right = FALSE)
  fig4a <- ggplot(data = fl_larthf_shp) +
    geom_sf(aes(fill = color_level)) +
    labs(fill = paste0("The number of\n ", larthf, " offense", "\n(", max(unique(fl_crime_data$Year)), ")")) +
    scale_fill_manual(values = lar_colors, labels = larthf_labels)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.title = element_blank(),
          legend.title = element_text(size = 11, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 10, vjust = 0.5),
          legend.position = c(0.26, 0.42),
          legend.direction = "vertical",
          legend.background = element_rect(fill = "transparent"))
  print(fig4a)
  ggsave("./figure/Fig 4/a.jpg", fig4a, width = 4, height = 4, dpi = 300)
  
  # Fig 4c
  fl_murder_shp <- fl_shp %>% left_join(fl_murder, by = c("NAME" = "County"))
  murder_breaks <- c(0, 10, 50, 100, 150, max(fl_murder_shp$value, na.rm = TRUE) + 1)
  murder_labels <- c("0 ~ 10", "10 ~ 50", "50 ~ 100", "100 ~ 150", "> 150")
  fl_murder_shp$color_level <- cut(fl_murder_shp$value, breaks = murder_breaks,
                                   include.lowest = TRUE, right = FALSE) 
  fig4c <- ggplot(data = fl_murder_shp) +
    geom_sf(aes(fill = color_level)) +
    labs(fill = paste0("The number of\n ", murthf, " offense", "\n(", max(unique(fl_crime_data$Year)), ")")) +
    scale_fill_manual(values = mur_colors, labels = murder_labels)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.title = element_blank(),
          legend.title = element_text(size = 11, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 10, vjust = 0.5),
          legend.position = c(0.26, 0.42),
          legend.direction = "vertical",
          legend.background = element_rect(fill = "transparent"))
  print(fig4c)
  ggsave("./figure/Fig 4/c.jpg", fig4c, width = 4, height = 4, dpi = 300)
  
  
  # Fitting the scaling relationship between the number of crimes, arrests, and guardians in Florida in 2020 and the population size
  # Fig 4b and 4d
  
  fl_popu <- fl_crime_data[, c("County", "Year", "Population")]
  fl_guardian <- read.csv("./data/Florida/Florida Law Enforcement Data.csv") %>%
    merge(fl_popu, by.x = c("County", "Year"), by.y = c("County", "Year")) %>%
    subset(Law_Employees > 0 & Population > 0 & Year == max(unique(fl_crime_data$Year))) %>%
    Outlier_Remove(select_type = "Guardian")
  
  fl_offender <- read.csv("./data/Florida/Florida Arrest Data.csv")
  fl_offender_murthf <- pivot_longer(fl_offender, cols = c(Murder, Rape, Robbery, Aggravated, Burglary, LarcenyTheft, MotorTheft),
                                     names_to = "crime_type", values_to = "value") %>%
    subset(value > 0 & Population > 0 & crime_type == "Murder" & Year == max(unique(fl_crime_data$Year))) %>%
    Outlier_Remove(select_type = "Offense")
  fl_offender_larthf <- pivot_longer(fl_offender, cols = c(Murder, Rape, Robbery, Aggravated, Burglary, LarcenyTheft, MotorTheft),
                                     names_to = "crime_type", values_to = "value") %>%
    subset(value > 0 & Population > 0 & crime_type == "LarcenyTheft" & Year == max(unique(fl_crime_data$Year))) %>%
    Outlier_Remove(select_type = "Offense")
  
  fl_crime_murthf <- Outlier_Remove(data = fl_murder, select_type = "Crime")
  fl_crime_larthf <- Outlier_Remove(data = fl_larthf, select_type = "Crime")
  
  guardian_plot <- fl_guardian %>% 
    select(County, Year, Population, Value = Law_Employees) %>%
    mutate(Type = "Guardian")
  offender_murthf_plot <- fl_offender_murthf %>%
    select(County, Year, Population, Value = value) %>%
    mutate(Type = "Offender")
  offender_larthf_plot <- fl_offender_larthf %>%
    select(County, Year, Population, Value = value) %>%
    mutate(Type = "Offender")
  crime_murthf_plot <- fl_crime_murthf %>%
    select(County, Year, Population, Value = value) %>%
    mutate(Type = murthf)
  crime_larthf_plot <- fl_crime_larthf %>%
    select(County, Year, Population, Value = value) %>%
    mutate(Type = larthf)
  
  guardian_model <- lm(log10(Law_Employees) ~ log10(Population), data = fl_guardian)
  offender_murthf_model <- lm(log10(value) ~ log10(Population), data = fl_offender_murthf)
  offender_larthf_model <- lm(log10(value) ~ log10(Population), data = fl_offender_larthf)
  crime_murthf_model <- lm(log10(value) ~ log10(Population), data = fl_crime_murthf)
  crime_larthf_model <- lm(log10(value) ~ log10(Population), data = fl_crime_larthf)
  
  murthf_plot <- bind_rows(guardian_plot, offender_murthf_plot, crime_murthf_plot)
  murthf_line <- data.frame(Type = c("Guardian", "Offender", murthf),
                            inter = c(coef(guardian_model)[1], 
                                      coef(offender_murthf_model)[1], 
                                      coef(crime_murthf_model)[1]),
                            slope = c(coef(guardian_model)[2],
                                      coef(offender_murthf_model)[2],
                                      coef(crime_murthf_model)[2]),
                            r2 = c(summary(guardian_model)$r.squared,
                                   summary(offender_murthf_model)$r.squared,
                                   summary(crime_murthf_model)$r.squared))
  murthf_text <- data.frame(Type = c("Guardian", "Offender", murthf),
                            texts = c(paste("y=", round(coef(guardian_model)[1], 2), "+",
                                            round(coef(guardian_model)[2], 2), "x, R²=",
                                            round(summary(guardian_model)$r.squared, 2)),
                                      paste("y=", round(coef(offender_murthf_model)[1], 2), "+",
                                            round(coef(offender_murthf_model)[2], 2), "x, R²=",
                                            round(summary(offender_murthf_model)$r.squared, 2)),
                                      paste("y=", round(coef(crime_murthf_model)[1], 2), "+",
                                            round(coef(crime_murthf_model)[2], 2), "x, R²=",
                                            round(summary(crime_murthf_model)$r.squared, 2))),
                            x = c(3.8, 3.8, 3.8),
                            y = c(4.3, 3.9, 4.6))
  murthf_plot$Type <- factor(murthf_plot$Type, levels = c(murthf, "Guardian", "Offender"))
  murthf_line$Type <- factor(murthf_line$Type, levels = c(murthf, "Guardian", "Offender"))
  murthf_text$Type <- factor(murthf_text$Type, levels = c(murthf, "Guardian", "Offender"))
  
  larthf_plot <- bind_rows(guardian_plot, offender_larthf_plot, crime_larthf_plot)
  larthf_line <- data.frame(Type = c("Guardian", "Offender", larthf),
                            inter = c(coef(guardian_model)[1], 
                                      coef(offender_larthf_model)[1], 
                                      coef(crime_larthf_model)[1]),
                            slope = c(coef(guardian_model)[2],
                                      coef(offender_larthf_model)[2],
                                      coef(crime_larthf_model)[2]),
                            r2 = c(summary(guardian_model)$r.squared,
                                   summary(offender_larthf_model)$r.squared,
                                   summary(crime_larthf_model)$r.squared))
  larthf_text <- data.frame(Type = c("Guardian", "Offender", larthf),
                            texts = c(paste("y=", round(coef(guardian_model)[1], 2), "+",
                                            round(coef(guardian_model)[2], 2), "x, R²=",
                                            round(summary(guardian_model)$r.squared, 2)),
                                      paste("y=", round(coef(offender_larthf_model)[1], 2), "+",
                                            round(coef(offender_larthf_model)[2], 2), "x, R²=",
                                            round(summary(offender_larthf_model)$r.squared, 2)),
                                      paste("y=", round(coef(crime_larthf_model)[1], 2), "+",
                                            round(coef(crime_larthf_model)[2], 2), "x, R²=",
                                            round(summary(crime_larthf_model)$r.squared, 2))),
                            x = c(3.8, 3.8, 3.8),
                            y = c(4.3, 3.9, 4.6))
  larthf_plot$Type <- factor(larthf_plot$Type, levels = c(larthf, "Guardian", "Offender"))
  larthf_line$Type <- factor(larthf_line$Type, levels = c(larthf, "Guardian", "Offender"))
  larthf_text$Type <- factor(larthf_text$Type, levels = c(larthf, "Guardian", "Offender"))
  
  # Fig 4b
  fig4b <- ggplot(larthf_plot, aes(x = log10(Population), y = log10(Value), color = Type)) +
    geom_point(alpha = 0.7, size = 2) +
    labs(x="Log10[Population]", y="Log10[Value]") +
    scale_color_manual(values = c("#8FAED1", "#578D2F", "#FFABDF")) +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          legend.position = c(1, 0.05),
          legend.justification=c(1, 0),
          legend.background = element_rect(fill = "transparent")) +
    geom_abline(data = larthf_line, aes(intercept = inter, slope = slope, color = Type), linewidth=0.8, show.legend = FALSE) +
    geom_text(data = larthf_text, aes(x = x, y = y, label = texts, color = Type),hjust = 0, size = 4.5,show.legend = FALSE) +
    guides(color = guide_legend(override.aes = list(size=5)))
  print(fig4b)
  ggsave("./figure/Fig 4/b.jpg", fig4b, width = 4, height = 4, dpi = 300)
  
  # Fig 4d
  fig4d <- ggplot(murthf_plot, aes(x = log10(Population), y = log10(Value), color = Type)) +
    geom_point(alpha = 0.7, size = 2) +
    labs(x="Log10[Population]", y="Log10[Value]") +
    scale_color_manual(values = c("#c82423", "#578D2F", "#FFABDF")) +
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          legend.position = c(0, 0.75),
          legend.justification=c(0, 1),
          legend.background = element_rect(fill = "transparent")) +
    geom_abline(data = murthf_line, aes(intercept = inter, slope = slope, color = Type), linewidth=0.8, show.legend = FALSE) +
    geom_text(data = murthf_text, aes(x = x, y = y, label = texts, color = Type),hjust = 0, size = 4.5,show.legend = FALSE) +
    guides(color = guide_legend(override.aes = list(size=5)))
  print(fig4d)
  ggsave("./figure/Fig 4/d.jpg", fig4d, width = 4, height = 4, dpi = 300)
  
  
  # Comparison between the model-predicted exponent and the true scaling exponent
  # Fig 4e-4f
  
  states <- c("Florida", "North Carolina", "Oregon", "Wisconsin")
  line_plots <- list()
  line_counts <- 1
  legend_data <- data.frame()
  figure_ids <- c("e", "f", "g", "h")
  for(state in states){
    
    state_params <- list()
    if(state == "Florida"){
      crime_data <- read.csv("./data/Florida/Florida Crime Data.csv")
      population <- crime_data[, c("County", "Year", "Population")]
      crime_long <- pivot_longer(crime_data, cols = c(Murder, Rape, Robbery, Aggravated, Burglary, LarcenyTheft, MotorTheft),
                                 names_to = "crime_type", values_to = "value") %>%
        subset(value > 0 & Population > 0)
      guardian <- read.csv("./data/Florida/Florida Law Enforcement Data.csv")
      guardian <- merge(guardian, population, by.x = c("County", "Year"), by.y = c("County", "Year")) %>%
        subset(Law_Employees > 0)
      offenser <- read.csv("./data/Florida/Florida Arrest Data.csv")
      offenser_long <- pivot_longer(offenser, cols = c(Murder, Rape, Robbery, Aggravated, Burglary, LarcenyTheft, MotorTheft),
                                    names_to = "crime_type", values_to = "value") %>%
        subset(value > 0 & Population > 0)
    }else if(state == "North Carolina"){
      crime_data <- read.csv("./data/North Carolina/North Carolina Crime Data.csv")
      population <- read.csv("./data/North Carolina/North Carolina Population Data.csv")
      crime <- merge(crime_data, population, by.x = c("County", "Year"), by.y = c("County", "Year"))
      crime_long <- pivot_longer(crime, cols = c(Murder, Rape, Robbery, Aggravated, Burglary, LarcenyTheft, MotorTheft, Arson),
                                 names_to = "crime_type", values_to = "value") %>%
        subset(value > 0 & Population > 0)
      guardian <- read.csv("./data/North Carolina/North Carolina Law Employees Data.csv")
      guardian <- merge(guardian, population, by.x = c("County", "Year"), by.y = c("County", "Year")) %>%
        subset(Law_Employees > 0)
      offenser <- read.csv("./data/North Carolina/North Carolina Arrest Data.csv")
      offenser <- merge(offenser, population, by.x = c("County", "Year"), by.y = c("County", "Year"))
      offenser_long <- pivot_longer(offenser, cols = c(Murder, Rape, Robbery, Aggravated, Burglary, LarcenyTheft, MotorTheft, Arson),
                                    names_to = "crime_type", values_to = "value") %>%
        subset(value > 0 & Population > 0)
    }else if(state == "Oregon"){
      crime_data <- read.csv("./data/Oregon/Oregon Crime Data.csv")
      population <- read.csv("./data/Oregon/Oregon Population Data.csv")
      crime_long <- merge(crime_data, population, by.x = c("County", "Year"), by.y = c("County", "Year")) %>%
        subset(value > 0 & Population > 0)
      guardian <- read.csv("./data/Oregon/Oregon Law Employees Data.csv")
      guardian <- merge(guardian, population, by.x = c("County", "Year"), by.y = c("County", "Year")) %>%
        subset(Law_Employees > 0)
      offenser <- read.csv("./data/Oregon/Oregon Arrest Data.csv")
      offenser_long <- merge(offenser, population, by.x = c("County", "Year"), by.y = c("County", "Year")) %>%
        subset(value > 0 & Population > 0)
    }else if(state == "Wisconsin"){
      crime_data <- read.csv("./data/Wisconsin/Winsconsin Crime Data.csv")
      population <- read.csv("./data/Wisconsin/Winsconsin Population Data.csv")
      crime <- merge(crime_data, population, by.x = c("County", "Year"), by.y = c("County", "Year"))
      crime_long <- pivot_longer(crime, cols = c(Murder, Rape, Robbery, Aggravated, Burglary, LarcenyTheft, MotorTheft, Arson),
                                 names_to = "crime_type", values_to = "value") %>%
        subset(value > 0 & Population > 0)
      guardian <- read.csv("./data/Wisconsin/Winsconsin Law Employees Data.csv")
      guardian <- merge(guardian, population, by.x = c("County", "Year"), by.y = c("County", "Year")) %>%
        subset(Law_Employees > 0)
      offenser <- read.csv("./data/Wisconsin/Winsconsin Arrest Data.csv")
      offenser <- merge(offenser, population, by.x = c("County", "Year"), by.y = c("County", "Year"))
      offenser_long <- pivot_longer(offenser, cols = c(Murder, Rape, Robbery, Aggravated, Burglary, LarcenyTheft, MotorTheft, Arson),
                                    names_to = "crime_type", values_to = "value") %>%
        subset(value > 0 & Population > 0)
    }
    
    if(state == "Florida"){crimekinds <- fl_crimekinds}
    else{crimekinds <- other_crimekinds}
    
    for(year in unlist(unique(crime_long$Year))){
      for(crimekind in crimekinds){
        
        crime_k <- Show_Crime_Type(types = crimekind)
        yk_crime <- subset(crime_long, Year == year & crime_type == crimekind)
        yk_offenser <- subset(offenser_long, Year == year & crime_type == crimekind)
        yk_guardian <- subset(guardian, Year == year)
        
        yk_crime <- Outlier_Remove(data = yk_crime, select_type = "Crime")
        yk_offenser <- Outlier_Remove(data = yk_offenser, select_type = "Offense")
        yk_guardian <- Outlier_Remove(data = yk_guardian, select_type = "Guardian")
        
        yk_crime_model <- lm(log10(value) ~ log10(Population), data = yk_crime)
        yk_offenser_model <- lm(log10(value) ~ log10(Population), data = yk_offenser)
        yk_guardian_model <- lm(log10(Law_Employees) ~ log10(Population), data = yk_guardian)
        
        temp <- data.frame(State = state, Crime_type = crime_k, Year = year,
                           Crime_beta = coef(yk_crime_model)[2], Offenser_beta = coef(yk_offenser_model)[2], Guardian_beta = coef(yk_guardian_model)[2])
        state_params <- rbind(state_params, temp)
      }
    }
    
    rownames(state_params) <- seq(1, nrow(state_params))
    
    state_params$x <- state_params$Crime_beta
    state_params$y <- 1 - state_params$Guardian_beta + state_params$Offenser_beta
    
    if(state=="Florida"){
      sunxu <- c("Murder", "Rape", "Robbery", "Aggravated assault", "Burglary", "Larceny-Theft", "Motor vehicle Theft")
    }else{
      sunxu <- c("Murder", "Rape", "Robbery", "Aggravated assault", "Burglary", "Larceny-Theft", "Motor vehicle Theft", "Arson")
    }
    state_params$Crime_type <- factor(state_params$Crime_type, levels = sunxu)
    legend_data <- rbind(legend_data, state_params)
    
    tmodel <- lm(y~x,data = state_params)
    tslope <- coef(tmodel)[2]
    tintercept <- coef(tmodel)[1]
    
    # Fig 4e-4h
    line_p <- ggplot(state_params, aes(x = x, y = y, shape = Crime_type, color = Crime_type)) +
      geom_point(size = 3, stroke = 1.5) +
      geom_abline(slope = tslope, intercept = tintercept, color="black", linetype = "dashed") +
      scale_color_manual(
        breaks = c("Murder", "Rape", "Aggravated assault", "Burglary", "Arson",
                   "Robbery", "Larceny-Theft", "Motor vehicle Theft"),
        values = c(
          "Murder" = "#c82423", 
          "Rape" = "#f79059",
          "Robbery" = "#9bbf8a",
          "Aggravated assault" = "#ffbe7a",
          "Burglary" = "#c29484",
          "Larceny-Theft" = "#82afda",
          "Motor vehicle Theft" = "#8dcec8",
          "Arson" = "#fa8878"
        )) +
      scale_shape_manual(
        breaks = c("Murder", "Rape", "Aggravated assault", "Burglary", "Arson",
                   "Robbery", "Larceny-Theft", "Motor vehicle Theft"),
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
      labs(x = "Scaling Exponent β", y = "Model-Predicted Exponent", color = "Crime Type", shape = "Crime Type") +
      geom_abline(slope = 1, intercept = 0, color = "gray") +
      annotate("text", x = max(max(state_params$x), max(state_params$y)), y = min(min(state_params$x), min(state_params$y)),
               label = paste0("y = ", round(tintercept, 2)," + ", round(tslope, 2),"x, R² = ", round(summary(tmodel)$r.squared, 2)),
               color = "black", hjust=1, size=5) +
      geom_hline(yintercept = 1,linetype = "dashed", color = "gray") +
      geom_vline(xintercept = 1,linetype = "dashed", color = "gray") +
      coord_cartesian(xlim = c(min(min(state_params$x),min(state_params$y))-0.05,max(max(state_params$x),max(state_params$y))+0.05), 
                      ylim = c(min(min(state_params$x),min(state_params$y))-0.05,max(max(state_params$x),max(state_params$y))+0.05))+
      annotate("text",x=min(min(state_params$x),min(state_params$y)),y=max(max(state_params$x),max(state_params$y))+0.02,label=state,color="black",hjust=0,size=5)+
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.text.y = element_text(angle = 90),
            legend.position = "none",
            legend.justification = c(0, 1),
            legend.background = element_rect(fill = "transparent"),
            legend.title = element_text(size = 8, hjust = 0.5, face = "bold"),
            legend.text = element_text(size = 6),
            legend.box = "horizontal",
            legend.spacing.x = unit(0.05, "mm"),
            legend.spacing.y = unit(0.05, "mm"),
            legend.box.background = element_rect(color = "black", linetype = "dashed", fill = NA)) +
      guides(shape=guide_legend(ncol = 1,title.position = "top", override.aes = list(size = 4)),
             color=guide_legend(ncol = 1,title.position = "top", override.aes = list(size = 4)))
    
    print(line_p)
    filename <- paste0("./figure/Fig 4/", figure_ids[[line_counts]], ".jpg")
    ggsave(filename, line_p, width = 4, height = 4, dpi = 300)
    
    line_plots[[line_counts]] <- line_p
    line_counts <- line_counts + 1
  }
  
  # legend of Fig 4e-h
  legend_plot <- ggplot(legend_data, aes(x = x, y = y, shape = Crime_type, color = Crime_type)) +
    geom_point(stroke = 1.5) +
    labs(title = "图1") +
    labs(x = "β", y = "1+α-γ", color = "Crime Type", shape = "Crime Type") +
    scale_color_manual(
      breaks = c("Murder", "Rape", "Aggravated assault", "Burglary", "Arson",
                 "Robbery", "Larceny-Theft", "Motor vehicle Theft"),
      values = c(
        "Murder" = "#c82423", 
        "Rape" = "#f79059",
        "Robbery" = "#9bbf8a",
        "Aggravated assault" = "#ffbe7a",
        "Burglary" = "#c29484",
        "Larceny-Theft" = "#82afda",
        "Motor vehicle Theft" = "#8dcec8",
        "Arson" = "#fa8878"
      )) +
    scale_shape_manual(
      breaks = c("Murder", "Rape", "Aggravated assault", "Burglary", "Arson",
                 "Robbery", "Larceny-Theft", "Motor vehicle Theft"),
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
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(shape = guide_legend(ncol = 8, title.position = "top", override.aes = list(size = 4)),
           color = guide_legend(ncol = 8, title.position = "top", override.aes = list(size = 4)))
  lg <- ggplotGrob(legend_plot)
  legend <- which(sapply(lg$grobs, function(x) x$name) == "guide-box")
  legend <- lg$grobs[[legend]]
  ggsave("./figure/Fig 4/legend.jpg", legend, width = 8, height = 1, dpi = 300)
}



#================================== Figure. S1 =================================================
# pivot
CrimeData_long3 <- CrimeData %>%
  pivot_longer(cols = c(Violent.crime,Murder,Rape,Robbery,Aggravated,Property.crime,Burglary,LarcenyTheft,MotorTheft,Arson),
               names_to = "type",
               values_to = "case")

CrimeData_long3 <- subset(CrimeData_long3, !type %in% c("Violent.crime","Property.crime"))


CrimeData_long4 <- CrimeData_long3 %>%
  filter(Year == 2022,
         Population > 0,
         case > 0)

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

CrimeData_cleaned2 <- CrimeData_long4 %>%
  group_by(type) %>%
  group_modify(~remove_outliers(.x)) %>%
  ungroup()

CrimeData_cleaned2 <- CrimeData_cleaned2 %>%
  mutate(type = ifelse(
    type == "Aggravated",
    "Aggravated assault",
    type
  ))

p1 <- ggplot(CrimeData_cleaned2, 
             aes(log10(Population),
                 log10(case))) +
  geom_point(size=0.25, shape=21, stroke = 0.3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8,colour="red") +  
  stat_poly_eq(
    aes(label = after_stat(paste(
      "atop(", ..eq.label.., ",", ..adj.rr.label.., ")"
    ))),
    formula = y ~ x,
    parse = TRUE,
    size = 2.1, 
    colour = "blue"  
  ) +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(
    x = expression(paste(Log[10],"[Population]")),  
    y = expression(paste(Log[10],"[Sum of Crimes (2022)]"))  
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=9)) +
  theme(legend.title = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 7)) +
  facet_wrap(
    ~type, 
    nrow = 2
  )+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 6.5, face = "bold")) 

print(p1)

tiff(file = "Figure S1 US_filteroutlier.tiff", width = 3600, height = 1800, res = 600, compression = "lzw")
print(p1)
dev.off()

#================================== Figure. S2 =================================================
mydata2 <- new.crime[new.crime$year == 2021, ]

facet_labels <- c("Murder","Rape", 
                  "Kidnapping and\n Abduction", 
                  "Robbery", 
                  "Burglary",
                  "Theft",
                  "Cheating",
                  "Hurt",
                  "Cruelty by Relatives",
                  "Causing Death\n by Negligence") 

p2 <- ggplot(mydata2, 
             aes(log10(population),
                 log10(case))) +
  geom_point(size=0.25, shape=21, stroke = 0.3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8,colour="red") +  
  stat_poly_eq(
    aes(label = after_stat(paste(
      "atop(", ..eq.label.., ",", ..adj.rr.label.., ")"
    ))),
    formula = y ~ x,
    parse = TRUE,
    size = 2.1,  
    colour = "blue"  
  ) +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(
    x = expression(paste(Log[10],"[Population]")),  
    y = expression(paste(Log[10],"[Sum of Crimes (2021)]"))  
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=9)) +
  theme(legend.title = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 7)) +
  facet_wrap(
    ~type, 
    labeller = as_labeller(setNames(facet_labels, unique(mydata2$type))),
    nrow = 2
  ) +
  theme(strip.background = element_blank(),  
        strip.text = element_text(size = 6.5, face = "bold")) 

print(p2)

tiff(file = "Figure S2 India_filteroutlier.tiff", width = 3600, height = 1800, res = 600, compression = "lzw")
print(p2)
dev.off()

#================================== Figure. S3 =================================================
mydata3 <- new.land8[new.land8$year == 2024, ]
mydata3 <- subset(mydata3, !classe %in% c("Vehicle theft","Armed theft","Domestic assault and battery"))
mydata3 <- mydata3 %>%
  mutate(classe = ifelse(
    classe == "Residential burglary",
    "Burglary",
    classe
  ))
mydata3 <- mydata3 %>%
  mutate(classe = ifelse(
    classe == "Non-violent theft against individuals",
    "Theft against individuals",
    classe
  ))

# Plotting
p3 <- ggplot(mydata3, 
             aes(log10(pop),
                 log10(faits))) +
  geom_point(size=0.25, shape=21, stroke = 0.3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8,colour="red") +  
  stat_poly_eq(
    aes(label = after_stat(paste(
      "atop(", ..eq.label.., ",", ..adj.rr.label.., ")"
    ))),
    formula = y ~ x,
    parse = TRUE,
    size = 2.1,  # Adjust formula font size
    colour = "blue"  # Change text color to blue
  ) +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(
    x = expression(paste(Log[10],"[Population]")),  
    y = expression(paste(Log[10],"[Sum of Crimes (2024)]"))  
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=9)) +
  theme(legend.title = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 7)) +
  facet_wrap(
    ~classe, 
    nrow = 2
  )+
  theme(strip.background = element_blank(),  # Remove facet title background
        strip.text = element_text(size = 6.5, face = "bold"))  # Bold facet titles


# Display the plot
print(p3)

facet_wrap(
  ~classe, 
  labeller = as_labeller(setNames(facet_labels, unique(mydata3$classe))),
  nrow = 2
)

# Save as 3600x3600 pixel TIFF image with LZW compression
tiff(file = "Figure S3 France_filteroutlier.tiff", width = 3600, height = 1800, res = 600, compression = "lzw")
print(p3)
dev.off()




#==================================== Figure S4=========================================
FigureS4 <- function(){
  
  select_types <- c("Murder", "LarcenyTheft")
  
  # Map the spatial distribution of two crimes in North Carolina
  # Fig S4a and S4b's left panel
  for(select_type in select_types){
    
    show_type <- Show_Crime_Type(select_type)
    
    shp <- st_read("./data/Geography/North Carolina/North_Carolina_State_and_County_Boundary_Polygons.shp") %>%
      st_transform(crs = 4326)
    crime_data <- read.csv("./data/North Carolina/North Carolina Crime Data.csv")
    crime_long <- pivot_longer(crime_data,cols = c(Murder, Rape, Robbery, Aggravated, Burglary, LarcenyTheft, MotorTheft, Arson),
                               names_to = "crime_type", values_to = "value") %>%
      subset(crime_type == select_type & Year == max(unique(crime_data$Year)))
    
    crime_long$County <- toupper(crime_long$County)
    shp$County <- paste0(shp$County, " County")
    shp$County <- toupper(shp$County)
    crime_shp<-shp %>%
      left_join(crime_long, by = c("County" = "County"))
    
    fillname <- paste0("The number of\n ", show_type, "\noffense")
    fig_title <- paste0("North Carolina - ", select_type, " (", max(unique(crime_data$Year)), ")")
    
    if(select_type == "Murder"){
      colours<-c("#94CE67", "#4BAB25", "#F1CBE4", "#E783C1", "#CF1B89")
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
    
    # Fig S4a and 4b's left panel
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
    
    filename_fig<-paste0("./figure/Fig S4/NC_left_",select_type,".jpg")
    ggsave(filename_fig,crime_distribute,width = 4,height = 4,dpi = 300)
  }
  
  
  # Map the spatial distribution of two crimes in Oregon
  # Fig S4c and S4d's left panel
  for(select_type in select_types){
    
    show_type<-Show_Crime_Type(select_type)
    
    shp<-st_read("./data/Geography/Oregon/BLM_OR_County_Boundaries_Polygon_Hub.shp")%>%
      st_transform(crs = 4326)%>%
      filter(grepl("OR", COBCODE))
    crime_long<-read.csv("./data/Oregon/Oregon Crime Data.csv")%>%
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
    
    # Fig S4c and S4d's left panel
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
    
    
    filename_fig<-paste0("./figure/Fig S4/OR_left_",select_type,".jpg")
    ggsave(filename_fig,crime_distribute,width = 4,height = 4,dpi = 300)
  }
  
  
  # Map the spatial distribution of two crimes in Wisconsin
  # Fig S4e and S4f's left panel
  for(select_type in select_types){
    
    show_type<-Show_Crime_Type(select_type)
    
    shp<-st_read("./data/Geography/Wisconsin/WI_CensusTL_Counties_2019.shp")%>%
      st_transform(crs = 4326)
    crime_data<-read.csv("./data/Wisconsin/Winsconsin Crime Data.csv")
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
    
    
    # Fig S4e and S4f's left panel
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
    
    
    filename_fig<-paste0("./figure/Fig S4/WI_left_",select_type,".jpg")
    ggsave(filename_fig,crime_distribute,width = 4,height = 4,dpi = 300)
  }
  
  
  # Plotting the scaling relationship between the number of crimes, arrests, and number of guardians for two crime types in three states and population size
  # Fig S4a-S4f's right panel
  filename_parts <- c("NC", "OR", "WI")
  for(select_type in select_types){
    show_type<-Show_Crime_Type(select_type)
    states<-c("North Carolina","Oregon","Wisconsin")
    state_counts <- 1
    for(state in states){
      
      if(state == "North Carolina"){
        crime_data<-read.csv("./data/North Carolina/North Carolina Crime Data.csv")
        population<-read.csv("./data/North Carolina/North Carolina Population Data.csv")
        crime<-merge(crime_data,population,by.x = c("County","Year"),by.y = c("County","Year"))
        
        crime_long<-pivot_longer(crime,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                                 names_to = "crime_type",values_to = "value")%>%
          subset(value>0&Population>0&crime_type==select_type&Year==max(unique(crime_data$Year)))
        guardian<-read.csv("./data/North Carolina/North Carolina Law Employees Data.csv")
        guardian<-merge(guardian,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
          subset(Law_Employees>0&Year==max(unique(crime_data$Year)))
        offenser<-read.csv("./data/North Carolina/North Carolina Arrest Data.csv")
        offenser<-merge(offenser,population,by.x = c("County","Year"),by.y = c("County","Year"))
        offenser_long<-pivot_longer(offenser,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                                    names_to = "crime_type",values_to = "value")%>%
          subset(value>0&Population>0&crime_type==select_type&Year==max(unique(crime_data$Year)))
      }else if(state == "Oregon"){
        crime_data<-read.csv("./data/Oregon/Oregon Crime Data.csv")
        population<-read.csv("./data/Oregon/Oregon Population Data.csv")
        crime_long<-merge(crime_data,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
          subset(value>0&Population>0&crime_type==select_type&Year==max(unique(crime_data$Year)))
        guardian<-read.csv("./data/Oregon/Oregon Law Employees Data.csv")
        guardian<-merge(guardian,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
          subset(Law_Employees>0&Year==max(unique(crime_data$Year)))
        offenser<-read.csv("./data/Oregon/Oregon Arrest Data.csv")
        offenser_long<-merge(offenser,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
          subset(value>0&Population>0&crime_type==select_type&Year==max(unique(crime_data$Year)))
      }else if(state == "Wisconsin"){
        crime_data<-read.csv("./data/Wisconsin/Winsconsin Crime Data.csv")
        population<-read.csv("./data/Wisconsin/Winsconsin Population Data.csv")
        crime<-merge(crime_data,population,by.x = c("County","Year"),by.y = c("County","Year"))
        crime_long<-pivot_longer(crime,cols = c(Murder,Rape,Robbery,Aggravated,Burglary,LarcenyTheft,MotorTheft,Arson),
                                 names_to = "crime_type",values_to = "value")%>%
          subset(value>0&Population>0&crime_type==select_type&Year==max(unique(crime_data$Year)))
        guardian<-read.csv("./data/Wisconsin/Winsconsin Law Employees Data.csv")
        guardian<-merge(guardian,population,by.x = c("County","Year"),by.y = c("County","Year"))%>%
          subset(Law_Employees>0&Year==max(unique(crime_data$Year)))
        offenser<-read.csv("./data/Wisconsin/Winsconsin Arrest Data.csv")
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
        mutate(Type = "Offender")
      offenser_model<-lm(log10(value)~log10(Population),data=offenser_long)
      
      crime_plot <- crime_long %>%
        select(County, Year, Population, Value = value) %>%
        mutate(Type = show_type)
      crime_model<-lm(log10(value)~log10(Population),data = crime_long)
      
      combined_plot <- bind_rows(guardian_plot, offense_plot, crime_plot)
      combined_line<-data.frame(Type=c("Guardian","Offender",show_type),
                                inter=c(coef(guardian_model)[1],coef(offenser_model)[1],coef(crime_model)[1]),
                                slope=c(coef(guardian_model)[2],coef(offenser_model)[2],coef(crime_model)[2]),
                                r2=c(summary(guardian_model)$r.squared,summary(offenser_model)$r.squared,summary(crime_model)$r.squared))
      combined_text<-data.frame(Type=c("Guardian","Offender",show_type),
                                texts=c(
                                  paste("y=", round(coef(guardian_model)[1], 2), "+", round(coef(guardian_model)[2], 2), "x, R²=",round(summary(guardian_model)$r.squared,2)),
                                  paste("y=", round(coef(offenser_model)[1], 2), "+", round(coef(offenser_model)[2], 2), "x, R²=",round(summary(offenser_model)$r.squared,2)),
                                  paste("y=", round(coef(crime_model)[1], 2), "+", round(coef(crime_model)[2], 2), "x, R²=",round(summary(crime_model)$r.squared,2))
                                ),
                                x=c(3.3,3.3,3.3),
                                y=c(4.2,3.8,4.6))
      
      combined_plot$Type<-factor(combined_plot$Type,levels = c(show_type,"Guardian","Offender"))
      combined_line$Type<-factor(combined_line$Type,levels = c(show_type,"Guardian","Offender"))
      combined_text$Type<-factor(combined_text$Type,levels = c(show_type,"Guardian","Offender"))
      
      if(select_type == "Murder"){
        color_values <- c("#c82423", "#578D2F", "#FFABDF")
      }else{
        color_values <- c("#8FAED1", "#578D2F", "#FFABDF")
      }
      
      # Fig S4a-S4f's right panel
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
      
      filename<-paste0("./figure/Fig S4/",filename_parts[state_counts],"_right_",select_type,".jpg")
      ggsave(filename,p,width = 4,height = 4,dpi = 300)
      state_counts <- state_counts + 1
    }
  }
}

#================================== Figure. S5==========================================
data4 <- read.csv("C:/Users/13593/Desktop/crime_scaling/data/India/2011urbanization_rate.csv", 
                  header=TRUE, sep=",",na.strings = 0) # Set 0 as NA when reading data

# 1. Filter data separately and add the column 'urbanrate2'
filtered_data_0.6 <- data4[!is.na(data4$urbanrate) & data4$urbanrate > 0.6, ]
filtered_data_0.6$urbanrate2 <- 0.6

filtered_data_0.5 <- data4[!is.na(data4$urbanrate) & data4$urbanrate > 0.5, ]
filtered_data_0.5$urbanrate2 <- 0.5

filtered_data_0.4 <- data4[!is.na(data4$urbanrate) & data4$urbanrate > 0.4, ]
filtered_data_0.4$urbanrate2 <- 0.4

filtered_data_0.3 <- data4[!is.na(data4$urbanrate) & data4$urbanrate > 0.3, ]
filtered_data_0.3$urbanrate2 <- 0.3

filtered_data_0.2 <- data4[!is.na(data4$urbanrate) & data4$urbanrate > 0.2, ]
filtered_data_0.2$urbanrate2 <- 0.2

# 2. Combine all filtered datasets
all_filtered_data <- rbind(filtered_data_0.6, filtered_data_0.5, filtered_data_0.4, filtered_data_0.3, filtered_data_0.2)

# 3. Plot using ggplot with regression and facet
u1 <- ggplot(all_filtered_data, 
            aes(log10(population),
                log10(Total))) +
  geom_point(size=0.7, shape=21, stroke = 0.3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, colour="red") +  
  stat_poly_eq(
    aes(label = after_stat(paste(
      "atop(", ..eq.label.., ",", ..adj.rr.label.., ")"
    ))),
    formula = y ~ x,
    parse = TRUE,
    size = 2.1,  # Adjust formula font size
    colour = "blue"  # Change formula color to blue
  ) +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(
    x = expression(paste(Log[10], "[Population]")),  
    y = expression(paste(Log[10], "[Total Crimes]"))  
  ) +
  facet_wrap(~urbanrate2, labeller = labeller(urbanrate2 = c(
    "0.6" = "Urbanization rate > 60%",
    "0.5" = "Urbanization rate > 50%",
    "0.4" = "Urbanization rate > 40%",
    "0.3" = "Urbanization rate > 30%",
    "0.2" = "Urbanization rate > 20%"
  )), nrow = 1
  ) +
  theme(strip.background = element_blank(),  # Remove facet label box
        strip.text = element_text(size = 6.5, face = "bold"))+  # Bold facet title
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text = element_text(size = 7.5), 
        axis.title = element_text(size = 8)) +
  theme(legend.title = element_blank(),
        legend.key.size = unit(3, 'mm'),
        legend.text = element_text(size = 7))



# Define the urbanization thresholds (from 0.2 to 0.6)
urban_thresholds <- c(0.2, 0.3, 0.4, 0.5, 0.6)
# Get crime variable column names (assuming columns 7–17 are crime types)
crime_columns <- names(data4)[7:17]
# Initialize a list to store results
result_list <- list()
# Loop through each urbanization threshold
for (threshold in urban_thresholds) {
  # Filter data
  filtered_data <- data4[!is.na(data4$urbanrate) & data4$urbanrate > threshold, ]
  # Skip if data is insufficient
  if (nrow(filtered_data) < 3) next
  # Initialize dataframe for current threshold
  threshold_results <- data.frame(
    CrimeType = character(),
    Slope = numeric(),
    CI_Lower = numeric(),
    CI_Higher = numeric(),
    stringsAsFactors = FALSE
  )
  # Loop through each crime type
  for (crime in crime_columns) {
    # Skip missing values
    valid_data <- filtered_data[!is.na(filtered_data[[crime]]) & 
                                  filtered_data[[crime]] > 0, ]
    # Check data sufficiency
    if (nrow(valid_data) >= 3) {
      # Fit linear model
      fit <- lm(log10(get(crime)) ~ log10(population), data = valid_data)
      # Extract slope and confidence interval
      slope <- coef(fit)[2]
      ci <- confint(fit, "log10(population)", level = 0.95)
      # Store results
      threshold_results <- rbind(threshold_results, data.frame(
        CrimeType = crime,
        Slope = round(slope, 3),
        CI_Lower = round(ci[1], 3),
        CI_Higher = round(ci[2], 3),
        stringsAsFactors = FALSE
      ))
    } else {
      # Fill NA if data insufficient
      threshold_results <- rbind(threshold_results, data.frame(
        CrimeType = crime,
        Slope = NA,
        CI_Lower = NA,
        CI_Higher = NA,
        stringsAsFactors = FALSE
      ))
    }
  }
  # Save current threshold results into list
  result_list[[paste0("Urban > ", threshold * 100, "%")]] <- threshold_results
}
# Merge results into one wide table
final_table <- Reduce(function(x, y) {
  merge(x, y, by = "CrimeType", suffixes = c("", "_new"))
}, result_list)
# Check column names to ensure correct generation
print(names(final_table))
# Clean up suffixes in column names
names(final_table) <- gsub("\\.x$", "", names(final_table))  
# Generate proper column names such as Slope_20%, CI_Lower_20%, CI_Higher_20%, etc.
slope_cols <- paste0("Slope_", urban_thresholds * 100, "%")
ci_lower_cols <- paste0("CI_Lower_", urban_thresholds * 100, "%")
ci_higher_cols <- paste0("CI_Higher_", urban_thresholds * 100, "%")
# Combine all columns
final_column_names <- c("CrimeType", 
                        as.vector(rbind(slope_cols, ci_lower_cols, ci_higher_cols)))
# Update column names
colnames(final_table) <- final_column_names
# Ensure correct column order
final_table <- final_table[, final_column_names]
# Print final table
print(final_table) 

# Save as CSV to the specified directory
write.csv(final_table, "C:/Users/13593/Desktop/crime_scaling/data/India/2011scaling_exponents_results.csv", row.names = FALSE)



data5 <- read.csv("C:/Users/13593/Desktop/crime_scaling/data/India/2011urbanization_scaling_exponents_results.csv", 
                  header=TRUE, sep=",",na.strings = 0) # Set 0 as NA when reading data
y_values <- data.frame(
  CrimeType = c("Murder", "Rape","Kidnapping and Abduction",
                "Robbery",
                "Burglary",
                "Theft",
                "Cheating",
                "Hurt",
                "Cruelty by Relatives",
                "Causing Death by Negligence",
                "Total"),
  y_value = c(0.8623,
              0.6897,
              0.895,
              0.8464,
              0.7983,
              1.0177,
              1.1271,
              1.1159,
              1.2918,
              1.0367,
              1.0715  ))
u2 <- ggplot(final_table, aes(x = rate, y = Slope, group = CrimeType, color = CrimeType)) +
  geom_point(size = 1.2) +
  geom_line(size = 0.6) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Higher), width = 0.2, size = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  facet_wrap(~ CrimeType, scales = "free_y") +
  # Key modification: define discrete scale for x-axis
  scale_x_discrete(
    limits = c("0%", "20%", "30%", "40%", "50%", "60%"),  
    labels = c("Fullset", "20%", "30%", "40%", "50%", "60%")  
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 6, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black"),
    axis.text.x = element_text(
      size = 6,
      angle = 30,  
      hjust = 1,   
      vjust = 1    
    ),
    axis.text.y = element_text(size = 6.5), 
    axis.title = element_text(size = 9),
    legend.position = "none"
  ) +
  labs(x = "Urbanization Rate", y = "Scaling Exponent (2011)")


tiff(file="Figure S5 urbanization rate scaling.tiff", res = 600, width = 3600, height = 3700, compression = "lzw")
ggarrange(u1,u2,
          nrow = 2,ncol=1,
          labels = c( "a", "b"),
          align = "h",font.label = list(size = 11),heights = c(1, 2.6))
dev.off()
getwd()

