#Renaming datafile
df = NYSERDA_Electric_Vehicle_Drive_Clean_Rebate_Data_Beginning_2017

#Keeping only Annual Petroleum Reductions (gallons)
df2 = df[c(10)]

#Mean of Annual Petroleum Reductions (gallons)
mean(df2$`Annual Petroleum Reductions (gallons)`)
mean_petred = mean(df2$`Annual Petroleum Reductions (gallons)`)
mean_petred
#Median/Summary of Annual Petroleum Reductions (gallons)
summary(df2$`Annual Petroleum Reductions (gallons)`)

#Range of Annual Petroleum Reductions (gallons)
max(df2$`Annual Petroleum Reductions (gallons)`) - min(df2$`Annual Petroleum Reductions (gallons)`)
range = max(df2$`Annual Petroleum Reductions (gallons)`) - min(df2$`Annual Petroleum Reductions (gallons)`)

#Mode of Annual Petroleum Reductions (gallons)

#Unadjusted variance of Annual Petroleum Reductions (gallons)
un_var = var(df2$`Annual Petroleum Reductions (gallons)`)
un_var
#Adjusted variance of Annual Petroleum Reductions (gallons)
N = length(df2$`Annual Petroleum Reductions (gallons)`)
ad_var = sum((df2$`Annual Petroleum Reductions (gallons)`-mean(df2$`Annual Petroleum Reductions (gallons)`))^2)/(N)
rm(s_sq, S_sqpop, mean_ghgred, mean_rebate, popsd, Range, a, b, dfa, dfa1)
ad_var
#Unadjusted standard deviation of Annual Petroleum Reductions (gallons)
un_sd = sd(df2$`Annual Petroleum Reductions (gallons)`)
un_sd
#Adjusted standard deviation Annual Petroleum Reductions (gallons)
ad_sd = sqrt(sum((df2$`Annual Petroleum Reductions (gallons)`-mean(df2$`Annual Petroleum Reductions (gallons)`))^2)/(N))
ad_sd

#Histogram of Annual Petroleum Reductions (gallons)
hist(df2$`Annual Petroleum Reductions (gallons)`)
hist(df2$`Annual Petroleum Reductions (gallons)`, main = "Annual Petroleum Reductions (gallons)", xlab = "Annual Petroleum Redutions (gallons)", col = "Turquoise")

#Reduced Target Population
summary(df2$`Annual Petroleum Reductions (gallons)`)
df3 = data.frame(c(350.58,440.11,577.50,508.29,592.89,503.6,575.74,451.54))
names(df3)[1] = "Reduced Target Population"

#Mean of Reduced Target Population
mean_red = mean(df3$`Reduced Target Population`)
mean_red
#Unadjusted variance of Reduced Target Population
un_var_red = var(df3$`Reduced Target Population`)
un_var_red
#Adjusted variance of Reduced Target Population
N_red = length(df3$`Reduced Target Population`)
ad_var_red = sum((df3$`Reduced Target Population`-mean(df3$`Reduced Target Population`))^2)/N_red
ad_var_red
#Unadjusted standard deviation of Reduced Target Population
Un_sd_red = sd(df3$`Reduced Target Population`)
Un_sd_red
#Adjusted standard deviation of Reduced Target Population
ad_sd_red = sqrt(ad_var_red)
ad_sd_red
#Densityplot of Reduced Target Population
Density = density(df3$`Reduced Target Population`)
polygon(Density, col = "Turquoise")
plot(Density, main = "Annual Petroleum Reductions (gallons)")

#Sampling Distribution
samples = combn(df3$`Reduced Target Population` , 3)

#Convert to a Data Frame
samples1 = data.frame(samples)

#Sampling distribution of the sample means
sample_means = colMeans(samples1)

#Histogram of sampling distribution of sample means
hist(sample_means, main = 'Frequency Histogram of the Sampling Distribution of the Sample Means', xlab = 'Sample Means')

#Expectation of the sampling distribution of the sample means
Expectation_samplemeans = mean(sample_means)
Expectation_samplemeans
#Variance of the sampling distribution of the sample means - y bar
Var_y_bar = (1-(3/8))*(un_var/3)
Var_y_bar
#Compare sample 1
var_sample1 = var(samples1$X26)
var_sample1
#Compare sample 2
var_sample2 = var(samples1$X37)
var_sample2
#Compare sample 3
var_sample3 = var(samples1$X54)
var_sample3

knitr::stitch('final project final final.r')
