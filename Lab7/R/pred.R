library(tidyverse)

library(nycflights13)

library(caret)


# reading data 



flights1 <- nycflights13::flights

weather1 <- nycflights13::weather





# Data prep




weather1 <- weather1 %>%
  
  dplyr::select(-year, -month, -day, -hour)


datfram1 <- left_join(flights1, weather1, by = c("origin", "time_hour")) 




weathna <- datfram1 %>%
  
  filter(is.na(temp)) %>%
  
  group_by(origin, month) %>%
  
  summarise(missing = n()) %>%
  
  spread(month, missing)



weathna


lapply(datfram1, FUN = function(x){summary(x)})



# removing all values which do not have effect on the modelling

datfram1 <- datfram1[complete.cases(datfram1), ] %>%
  
  dplyr::select(-year)





# Reducing the amount of data used for analysis

datfram1 <- datfram1 %>%
  
  sample_frac(.1)





# selecting data we thing is good enough for modelling 

cor_matrix <- cor(datfram1[sapply(datfram1, is.numeric)])



has_effect <- cor_matrix[cor_matrix[,"arr_delay"] >= abs(0.15) ,"arr_delay"]



formula_short <- arr_delay ~ dep_delay + dep_time + hour





# removing unecessary data 

datfram1 <- datfram1 %>%
  
  dplyr::select(-c(sched_dep_time, carrier, flight, tailnum, dest,
                   
                   minute, time_hour, wind_gust))


# Creating test and training data

traindat<- createDataPartition(y = datfram1$arr_delay, 
                                
                                p = .8, 
                                
                                list = FALSE)



training <- datfram1[traindat, ]

tmp <- datfram1[-traindat, ]



# Creating test data that is valid

validat<- createDataPartition(y = tmp$arr_delay, p = .75, list = FALSE)



validate <- tmp[validat, ]

testing <- tmp[-validat, ]



rm(tmp)





# Sampling method is set 


resamp<- trainControl(method = "repeatedcv", 
                     
                     repeats = 3)





# Creating models



# Model 1

start <- Sys.time()

lmr_fit1 <- train(arr_delay ~ .,
                  
                  data = training,
                  
                  method = "ridge",
                  
                  trControl = resamp,
                  
                  preProc = "scale")

end <- Sys.time()

end-start





# Predicting

lmr_class1 <- predict(lmr_fit1, newdata = validate)



# Evaluation

lmr_fit1



df_lmr_fit1 <- validate %>%
  
  dplyr::select(arr_delay) %>%
  
  cbind(., fitted_values = predict(lmr_fit1, newdata = validate)) %>%
  
  mutate(resid = arr_delay - fitted_values)





gg1_lmr_fit1 <- ggplot(data = df_lmr_fit1, aes(x = arr_delay, y = fitted_values)) +
  
  geom_point(aes(alpha = .01)) +
  
  geom_abline(slope = 1, intercept = 0, colour = "red", size = 0.5)



gg2_lmr_fit1 <- ggplot(data = df_lmr_fit1, aes(x = fitted_values, y = resid)) +
  
  geom_point(aes(alpha = .01)) +
  
  geom_smooth(method = "loess", se = FALSE, colour = "red", size = .5)



postResample(lmr_class1, validate$arr_delay)







# second model 

lmr_fit2 <- train(formula_short,
                  
                  data = training,
                  
                  method = "ridge",
                  
                  trControl = ctrl,
                  
                  preProc = "scale")



# Predicting

lmr_class2 <- predict(lmr_fit2, newdata = validate)




lmr_fit2



df_lmr_fit2 <- validate %>%
  
  dplyr::select(arr_delay) %>%
  
  cbind(., fitted_values = predict(lmr_fit2, newdata = validate)) %>%
  
  mutate(resid = arr_delay - fitted_values)





gg1_lmr_fit2 <- ggplot(data = df_lmr_fit2, aes(x = arr_delay, y = fitted_values)) +
  
  geom_point(aes(alpha = .01)) +
  
  geom_abline(slope = 1, intercept = 0, colour = "red", size = 0.5)



gg2_lmr_fit2 <- ggplot(data = df_lmr_fit2, aes(x = fitted_values, y = resid)) +
  
  geom_point(aes(alpha = .01)) +
  
  geom_smooth(method = "loess", se = FALSE, colour = "red", size = .5)



postResample(lmr_class2, validate$arr_delay)



# capable model



resamp <- resamples(list(rreg_all_vars = lmr_fit1,
                         
                         rreg_high_cor = lmr_fit2))



summary(resamp)

# End of code