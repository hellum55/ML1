# mutate missing values manually (not needed in this case)
pumpkin_data <- pumpkin_data %>%
  mutate(Mostly.High
         = replace(Mostly.High,
                   is.na(Mostly.High),
                   mean(Mostly.High, na.rm = TRUE)),
         Mostly.Low
         = replace(Mostly.Low,
                   is.na(Mostly.Low),
                   mean(Mostly.Low, na.rm = TRUE)))

#Manually imputation:
pumpkin_data$Item.Size <- impute(pumpkin_data$Item.Size)
pumpkin_data$Color <- impute(pumpkin_data$Color)
pumpkin_data$Variety <- impute(pumpkin_data$Variety)
pumpkin_data$Origin <- impute(pumpkin_data$Origin)

plot_missing(pumpkin_data)

# Retain only pumpkins with the string "bushel"
pumpkin_data <- pumpkin_data %>% 
  filter(str_detect(string = Package, pattern = "bushel"))

# Normalize the pricing so that you show the pricing per bushel, not per 1 1/9 or 1/2 bushel
pumpkin_data <- pumpkin_data %>% 
  mutate(price = case_when(
    str_detect(Package, "1 1/9") ~ price/(1.1),
    str_detect(Package, "1/2") ~ price*2,
    TRUE ~ price))

#manually dropping NZV variable (not needed in this case)
pumpkin_data <- pumpkin_data %>%
  select(-c(Repack))

#Check for correlation:
library(corrplot)
# Obtain correlation matrix
corr_mat <- cor(baked_train)
corrplot(corr_mat, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black", cl.pos = "n", order = "original")


pumpkin_data <- pumpkin_data %>%
  mutate(Item.Size = case_when(
    Item.Size == 'sml' ~ 0,
    Item.Size == 'med' ~ 1,
    Item.Size == 'med-lge' ~ 2,
    Item.Size == 'lge' ~ 3,
    Item.Size == 'xlge' ~ 4,
    Item.Size == 'exjbo' ~ 5,
    TRUE ~ as.numeric(Item.Size)  # If there are other values, keep them as is (or handle accordingly)
  ))