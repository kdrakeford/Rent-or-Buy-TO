#Don't know if I need to run this every time: (library was previously on OneDrive)
#.libPaths( c( .libPaths(), "C:/Users/keith/Documents/R/win-library/3.4") )
library(tidyverse)

setwd("C:/Users/keith/Documents/R/R Projects")
#source("multiplot.R") #Loads multiplot function formerly included in ggplot2
source("Mortgage projection/Mortgage function.R")

format.money  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}
inv <- 80000 #The initial down payment or starting investment
inv_fees <- 0.01
prop_price <- 400000
condo_fees <- 400*12
prop_tax <- prop_price*0.006355 #Rounded Toronto residential property tax rate
maintenance <- 1000
o_ins <- 500
r_ins <- 250
o_util <- 1080
r_util <- 660
rent <- (1800*12)
mortgage(prop_price - inv, 5, 25, plotData = F)
own_cost_25 <- #Total ownership costs over 25 years
  sum(aDFyear$Annual_Interest) +
  (condo_fees + maintenance + o_ins + o_util + prop_tax) * 25
rent_cost_25 <- #Total renting costs over 25 years
  (r_ins + r_util + rent)
ann_inv <- (own_cost_25 - rent_cost_25)/25  #The amount to be invested each year, 
                                            # i.e. the amount that would have been spent
                                            # on maintaining the mortgage/condo
hpi <- as.data.frame(read.csv("Mortgage projection/HPI.csv"))
tsxi <- as.data.frame(read.csv("Mortgage projection/SP_TSX annual.csv"))
tsxi <- tsxi[, c(1:3)]

hpi$HPI_adj <- as.numeric(gsub('%', '', hpi$HPI.less.Inf))/100

tsxi$TSX_change <- as.numeric(tsxi$TSX_change)/100 #converts % to decimal

#Histogram of hpi and tsxi
hpi$group <- 'hpi'
hpi2 <- hpi[, c(1, 5, 6)]
colnames(hpi2) <- c("Year", "Return", "Group")
tsxi$group <- 'tsxi'
tsxi2 <- tsxi[, c(1, 3, 4)]
colnames(tsxi2) <- c("Year", "Return", "Group")
tsxi2$Return <- tsxi2$Return/100
#Data frame with just the coordinates of the .95 quantile ranges
q_x_hpi <- c(quantile(hpi2$Return, c(0.05, 0.95)))
q_x_tsxi <- c(quantile(tsxi2$Return, c(0.05, 0.95)))
q_hpi_range <- data.frame(cbind(q_x_hpi, c(1, 1)))
q_tsxi_range <- data.frame(cbind(q_x_tsxi, c(1, 1)))

return_comp <- rbind(hpi2, tsxi2)
ggplot() + 
    geom_histogram(data = return_comp, aes(return_comp$Return, fill = return_comp$Group),
                   alpha = 0.5, position = 'identity', 
                   breaks = seq(from=-0.6, to=0.6, by=0.05)) +
    geom_vline(aes(xintercept=median(return_comp[c(return_comp$Group == 'hpi'), 2])),
               color="#F8766D", size=1, linetype = 'dashed') +
    geom_vline(aes(xintercept=median(return_comp[c(return_comp$Group == 'tsxi'), 2])),
               color="#00BFC4", size=1, linetype = 'dashed') + 
    geom_line(data = q_hpi_range, aes(x = q_x_hpi, y = 1), colour = '#b5564f', size = 2) +
    geom_line(data = q_tsxi_range, aes(x = q_x_tsxi, y = 2), colour = '#009ba0', size = 2) +
    geom_text(aes(
        label = paste(round(median(
                                return_comp[c(return_comp$Group == 'hpi'), 2]*100
                                    ), 1), "%", sep = ""
                ),
        x = median(return_comp[c(return_comp$Group == 'hpi'), 2]),
        y = 20, hjust = -0.1)) + 
    geom_text(aes(
        label = paste(round(median(
                                return_comp[c(return_comp$Group == 'tsxi'), 2]*100
                                    ), 1), "%", sep = ""
                ),
        x = median(return_comp[c(return_comp$Group == 'tsxi'), 2]),
        y = 18, hjust = -0.1)) +
    labs(title="Distribution of annual TSX returns and HPI growth", y = "Frequency") +
    scale_x_continuous(name = "Annual growth", labels = scales::percent)


#Simulation of property equity based on samples from hpi
sim_equ <- NULL  #make sure to run this to empy the collector vector
year <- 2017     #run this too
equ_log <- data.frame(Year = c(seq(from=2018, to=2042)))
sample_hpi <- NULL
for (i2 in c(1:1000)) {
  val = prop_price
  equ_log <- cbind(equ_log, c(seq(from=0, length.out=25, by=0)))#Creates empty column to be filled with
  sample_hpi <- sample(hpi$HPI_adj, 25, replace = FALSE)        # this sim's property value each year
  for (i3 in c(1:length(sample_hpi))) {
    val <- val*(1+sample_hpi[i3])
    year <- year + 1
    equ_log[[i2+1]][i3] <- val #records the value for each year of each simulation
  }
  sim_equ <- c(sim_equ, val - own_cost_25) 
}
#Create a timeline of the median property value across all sims for each year   
equ_yr <- data.frame(
    Year = c(seq(from=2018, to=2042)),
    Equity = c(apply(equ_log, 1, median))
    )
equ_yr <- cbind(equ_yr,
    Equ_net = c(equ_yr$Equity - own_cost_25/25*(equ_yr$Year - 2018 + 1)),
    Equ_low_85 = apply(equ_log, 1, quantile, probs = 0.05) - own_cost_25/25*(equ_yr$Year - 2018 + 1),
    Equ_high_85 = apply(equ_log, 1, quantile, probs = 0.95) - own_cost_25/25*(equ_yr$Year - 2018 + 1)
)

# Applying sampled annual returns to a starting investment
sim_inv <- NULL #make sure to run this to empy the collector vector
year <- 2017     #run this too
inv_log <- data.frame(Year = c(seq(from=2018, to=2042)))
sample_tsxi <- NULL
for (i3 in c(1:1000)) {
    prpl <- inv
    inv_log <- cbind(inv_log, c(seq(from=0, length.out=25, by=0)))
    sample_tsxi <- sample(tsxi$TSX_change, 25, replace = FALSE)
    for (i4 in c(1:length(sample_tsxi))) {
        prpl <- prpl*(1+sample_tsxi[i4]-inv_fees) + ann_inv
        year <- year + 1
        inv_log[[i3+1]][i4] <- prpl #records the value for each year of each simulation
    }
    sim_inv <- c(sim_inv, prpl - rent_cost_25)
}
#Create a timeline of the median investment value across all sims for each year   
inv_yr <- data.frame(
    Year = c(seq(from=2018, to=2042)),
    Investment = c(apply(inv_log, 1, median))
)
inv_yr <- cbind(inv_yr,
    Inv_net = c(inv_yr$Investment - rent_cost_25/25*(inv_yr$Year - 2018 + 1)),
    Inv_low_85 = apply(inv_log, 1, quantile, probs = 0.05) - rent_cost_25/25*(inv_yr$Year - 2018 + 1),
    Inv_high_85 = apply(inv_log, 1, quantile, probs = 0.95) - rent_cost_25/25*(inv_yr$Year - 2018 + 1)
)

#Combined histogram - investment vs equity simulations:
inv_df <- c()
equ_df <- c()
inv_df <- as.data.frame(sim_inv)
colnames(inv_df) <- c('mean')
equ_df <- as.data.frame(sim_equ)
colnames(equ_df) <- c('mean')

#Combine both datasets into one data frame. Add column "sim" to distinguish
# between which dataset each row came from. 
inv_df$sim <- 'inv'
equ_df$sim <- 'equ'
val_sims <- rbind(inv_df, equ_df)
#Data frame with just the coordinates of the .95 quantile ranges
q_x_e <- quantile(val_sims[c(val_sims$sim == 'equ'), 1], c(0.05, 0.95))/1000000
q_x_i <- quantile(val_sims[c(val_sims$sim == 'inv'), 1], c(0.05, 0.95))/1000000
q_e_range <- data.frame(cbind(q_x_e, c(1, 1)))
q_i_range <- data.frame(cbind(q_x_i, c(1, 1)))

#Base histogram
com_hist <- ggplot() + 
    geom_histogram(data = val_sims, aes(mean/1000000, fill = sim),
                   alpha = 0.5, position = 'identity', bins = 50) +
    geom_vline(aes(xintercept=median(val_sims[c(val_sims$sim == 'equ'), 1])/1000000),
               color="#F8766D", size=1, linetype = 'dashed') +
    geom_vline(aes(xintercept=median(val_sims[c(val_sims$sim == 'inv'), 1])/1000000),
           color="#00BFC4", size=1, linetype = 'dashed')
#Bin heights to be used for mean labels
bin_height <- ggplot_build(com_hist)$data[[1]][["y"]]
equ_bin_max <- max(bin_height[c(ggplot_build(com_hist)$data[[1]]["group"] == 1)])
inv_bin_max <- max(bin_height[c(ggplot_build(com_hist)$data[[1]]["group"] == 2)])
#Add text labels:
com_hist <- com_hist +
    geom_text(aes(label = paste(round(median(val_sims[c(val_sims$sim == 'equ'), 1])/1000000, 2), "M"),
                  x = median(val_sims[c(val_sims$sim == 'equ'), 1])/1000000,
                  y = equ_bin_max*1.1, hjust = -0.1)) + 
    geom_text(aes(label = paste(round(median(val_sims[c(val_sims$sim == 'inv'), 1])/1000000, 2), "M"),
                  x = median(val_sims[c(val_sims$sim == 'inv'), 1])/1000000,
                  y = inv_bin_max*1.1, hjust = -0.1))+
    geom_line(data = q_e_range, aes(x = q_x_e, y = equ_bin_max*0.5), colour = '#b5564f', size = 2) +
    geom_line(data = q_i_range, aes(x = q_x_i, y = inv_bin_max*0.5), colour = '#009ba0', size = 2) +
    labs(title="1000 Simulations of sampled TSX returns and HPI growth", y = "Count of simulations") +
    scale_x_continuous(name = "Investment value, millions", labels = scales::comma)

com_hist

#Timeline of property/investment value
timeline <- merge(equ_yr, inv_yr)
ggplot(data = timeline, aes(timeline)) + 
    geom_line(aes(x = Year, y = Equ_net/1000000), colour = "#F8766D", size = 1.5) + 
    geom_line(aes(x = Year, y = Inv_net/1000000), colour = "#00BFC4", size = 1.5) +
    geom_ribbon(aes(x = Year, ymin = Inv_low_85/1000000, ymax = Inv_high_85/1000000), 
                alpha = 0.15, fill = "#00BFC4") +
    geom_ribbon(aes(x = Year, ymin = Equ_low_85/1000000, ymax = Equ_high_85/1000000), 
                alpha = 0.15, fill = "#F8766D") +
    scale_y_continuous(name = "Value, million", labels = scales::comma)

#In how many simulations did equ outperform inv?
sim_comp <- c()
sim_comp <- data.frame(cbind(equ_df[, 1], inv_df[, 1]))
colnames(sim_comp) <- c("equ", "inv")
sim_comp$equ_better <- c(sim_comp$equ > sim_comp$inv)
sum(sim_comp$equ_better)

#Would like to scatterplot the entire simulation timeline
equ_log_2 <- NULL
inv_log_2 <- NULL
for (i in c(1:(length(equ_log[1, ])-1))) {
    for (i2 in c(1:length(equ_log[, 1]))) {
        equ_log_2 <- rbind(equ_log_2, c(equ_log[i2, 1], equ_log[i2, i+1], i, i2))
        inv_log_2 <- rbind(inv_log_2, c(inv_log[i2, 1], inv_log[i2, i+1], i, i2))
    }
}

equ_log_2 <- equ_log_2[as.integer(runif(1000)*25000), ]
inv_log_2 <- inv_log_2[as.integer(runif(1000)*25000), ]
length(equ_log_2)

ggplot(data = data.frame(equ_log_2)) + 
    geom_point(aes(x = inv_log_2[, 1], y = inv_log_2[,2]), colour = "#00BFC4", size = 1.5, alpha = 0.25) + 
    geom_point(aes(x = equ_log_2[, 1], y = equ_log_2[,2]), colour = "#F8766D", size = 1.5, alpha = 0.25) + 
    scale_y_continuous(name = "Value, million", labels = scales::comma)

#Histogram of equity simulations
equ_hist <- ggplot(data = as.data.frame(sim_equ), aes(sim_equ/1000000)) +
    geom_histogram(fill = "lightblue", col = "dark grey", alpha = 0.5) +
    geom_vline(aes(xintercept=median(sim_equ/1000000)),
               color="red", size=1) + 
    geom_vline(aes(xintercept=quantile(sim_equ/1000000, 0.05)),
               color="purple", linetype="dashed", size=1) + 
    geom_vline(aes(xintercept=quantile(sim_equ/1000000, 0.95)),
               color="purple", linetype="dashed", size=1) + 
    geom_text(aes(label = paste(round(median(sim_equ/1000000), 2), 'M'),
                  x = median(sim_equ/1000000), y = 150, hjust = -0.1))+
    scale_x_continuous(name = "Investment value, Millions", labels = scales::comma) +
    labs(title="1000 Simulations of Property Equity Growth", y = "Count of simulations")

equ_hist

#Histogram of the investment simulations:
inv_hist <- ggplot(data = as.data.frame(sim_inv), aes(sim_inv/1000000)) +
    geom_histogram(fill = "#00BFC4", alpha = 0.5) +
    geom_vline(aes(xintercept=mean(sim_inv/1000000)),
               color="#00BFC4", size=1) + 
    geom_vline(aes(xintercept=quantile(sim_inv/1000000, 0.05)),
               color="purple", linetype="dashed", size=1) + 
    geom_vline(aes(xintercept=quantile(sim_inv/1000000, 0.95)),
               color="purple", linetype="dashed", size=1) + 
    geom_text(aes(label = paste(format.money(round(median(sim_inv/1000000), 2)), "M"),
                  x = median(sim_inv/1000000), y = 150, hjust = -0.1)) +
    scale_x_continuous(name = "Investment value, Millions", labels = scales::comma) +
    labs(title="1000 Simulations of Investment Over 25 Yrs", y = "Count of simulations")

inv_hist

#mortgage schedule
mortgage(320000, 5, 25, plotData = F) #function(P=500000, I=6, L=30, amort=T, plotData=T)
aDFyear

format.money(quantile(sim_inv, c(0.1, 0.5, 0.9)))

# Select 25 random values from c_hpi_change, and store the sample mean in
# a vector. Repeat 1,000 times. 

h_sim_mean <- c() #make sure to run this to empy the collector vector
for (i2 in c(1:1000)) {
  sample_hpi <- sample(hpi$HPI_adj, 25, replace = FALSE)
  h_sim_mean <- c(h_sim_mean, mean(sample_hpi))
}

#Histogram of the hpi simulations:
hpi_hist <- ggplot(data = as.data.frame(h_sim_mean), aes(h_sim_mean)) +
  geom_histogram(fill = "skyblue", col = "dark grey") +
  geom_vline(aes(xintercept=mean(h_sim_mean)),
             color="red", size=1) + 
  geom_vline(aes(xintercept=quantile(h_sim_mean, 0.05)),
             color="purple", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=quantile(h_sim_mean, 0.95)),
             color="purple", linetype="dashed", size=1) + 
  geom_text(aes(label = paste(round(mean(h_sim_mean)*100, 1), '%'),
                x = mean(h_sim_mean), y = 150, hjust = -0.1))

hpi_hist

#Select random values from S&P annual returns index, and store the sample mean
# in a vector.Repeat 1,000 times. 

i_sim_mean <- c() #make sure to run this to empy the collector vector
i_sim_med <- c() #make sure to run this to empy the collector vector
for (i3 in c(1:1000)) {
  sample_tsxi <- sample(tsxi$TSX_change, 25, replace = FALSE)
  i_sim_mean <- c(i_sim_mean, mean(sample_tsxi))
  i_sim_med <- c(i_sim_med, median(sample_tsxi))
}

#Histogram of the S&P simulations:
ggplot(data = as.data.frame(i_sim_mean), aes(i_sim_mean)) +
  geom_histogram(fill = "green", col = "dark grey",
                 title = "Mean HPI change per simulation") +
  geom_vline(aes(xintercept=mean(i_sim_mean)),
             color="red", size=1) + 
  geom_vline(aes(xintercept=quantile(i_sim_mean, 0.05)),
             color="purple", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=quantile(i_sim_mean, 0.95)),
             color="purple", linetype="dashed", size=1) + 
  geom_text(aes(label = paste(round(mean(i_sim_mean)*100, 1), '%'),
                x = mean(i_sim_mean), y = 150, hjust = -0.1)) + 
  geom_text(aes(label = paste(round(mean(i_sim_med)*100, 1), '%'),
                x = mean(i_sim_mean), y = 130, hjust = -0.1)) +
  labs(title="1000 Simulations of S&P Index Growth", y = "Count of simulations")


#Combined histogram - avg annual returns:
tsxi_df <- c()
hpi_df <- c()
tsxi_df <- as.data.frame(i_sim_mean)
colnames(tsxi_df) <- c('mean')
hpi_df <- as.data.frame(h_sim_mean)
colnames(hpi_df) <- c('mean')
#Combine both datasets into one data frame. Add column "sim" to distinguish
# between which dataset each row came from. 
tsxi_df$sim <- 'tsxi'
hpi_df$sim <- 'hpi'
return_sims <- rbind(tsxi_df, hpi_df)

#Plot the overlapping histograms:
ggplot(data = return_sims, aes(mean, fill = sim)) + 
  geom_histogram(alpha = 0.5, position = 'identity') +
  geom_vline(aes(xintercept=median(return_sims[c(return_sims$sim == 'tsxi'), 1])),
             color="blue", size=1) +
  geom_vline(aes(xintercept=median(return_sims[c(return_sims$sim == 'hpi'), 1])),
             color="red", size=1) + 
  geom_text(aes(label = paste(round(median(return_sims[c(return_sims$sim == 'tsxi'), 1])*100, 2), "%"),
                x = median(return_sims[c(return_sims$sim == 'tsxi'), 1]), y = 150, hjust = -0.1)) + 
  geom_text(aes(label = paste(round(median(return_sims[c(return_sims$sim == 'hpi'), 1])*100, 2), "%"),
                x = median(return_sims[c(return_sims$sim == 'hpi'), 1]), y = 175, hjust = -0.1)) +
  scale_x_continuous(name = "Value after 25 yrs, Millions", labels = scales::comma) +
  labs(title="1000 of Sampled S&P returns and HPI Y/Y growth", y = "Count of simulations")

#Examine tsxi and HPI
returns <- merge(hpi, tsxi, all.x = T)
ggplot(data = returns) +
  geom_line(aes(x = Year, y = HPI_adj, colour = "HPI"), size = 1.5, alpha = 0.5) + 
  geom_line(aes(x = Year, y = TSX_change, colour = "TSX"), size = 1.5, alpha = 0.5) +
  scale_colour_manual(values = c("HPI" = "red", "TSX" = "blue"),
                      labels = c("HPI", "TSX"))

#Test whether HPI an TSX fluctuations are correlated:
cor.test(
  tsxi$TSX_change, 
  hpi$HPI_adj[c( #gets just the HPI years that overlap with TSX_change
    (length(hpi$HPI) - length(tsxi$TSX_change) + 1):length(hpi$HPI)
  )])
