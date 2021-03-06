
library(tidyverse)

setwd("C:/Users/keith/Documents/R/R Projects")
#source("multiplot.R") #Loads multiplot function formerly included in ggplot2
source("Mortgage projection/Mortgage function.R")

#Function to provide a prompt and receive an integer input from the user
readinteger <- function(query)
{ 
    n <- readline(prompt=query)
    return(as.integer(n))
}

#Function to provide a prompt and receive an integer input from the user:
sell_cost <- function(prop_val) {
                return(500 #legal fees
                + 0.04*prop_val #agent commission, 4%
                + 0.02*prop_val #land transfer tax
                + 1000) #staging
}


# Parameters --------------------------------------------------------------

use_defaults <- cat("Default values for this simulation are:
downpayment = $80,000
prop_price = $400,000
condo_fees = $500/month
maintenance = $100/month
property insurance = $500/year
utilities, if owner = $90/month
renter's insurance = $250/year
utilities, if renting = $55/month
rent = $1,600/month
Index fund fees = 1% \n\n
")

#If script is run line-by-line, this provides an opportunity to
# input other values for the simulation. 
use_defaults <- readline("Use default values? Enter y or n: ")

if (use_defaults != 'n') {
    inv <- 80000 #The initial down payment or starting investment
    prop_price <- 400000
    condo_fees <- 500*12
    maintenance <- 1200
    o_ins <- 500
    o_util <- 90*12
    r_ins <- 250
    r_util <- 55*12
    rent <- (1600*12)
    inv_fees <- 0.01
}
if (use_defaults == 'n') {
    inv <- readinteger("What is the downpayment? ")
    prop_price <- readinteger("What is the purchase price of the property? ")
    condo_fees <- readinteger("What are the monthly condo feels? ") * 12
    maintenance <- readinteger("What are anticipated annual maintenance costs? ")
    o_ins <- readinteger("How much would property insurance cost annually? ")
    r_ins <- readinteger("How much would rental insurance cost annually? ")
    o_util <- readinteger("How much would utilities cost per month if you owned the property? ") * 12
    r_util <- readinteger("How much would utilities cost per month if you were renting? ")
    rent <- readinteger("What is the rent per month? ")
    inv_fees <- readinteger("How much are the investment managment fees? (%) ") / 100
}

prop_tax <- prop_price*0.006355 #Rounded Toronto residential property tax rate
mortgage(prop_price - inv, 5, 25, plotData = F)
buy_cost <- (1800) #legal fees)
own_cost_25 <- #Ownership costs over 25 years, not including mortgage interest
  (condo_fees + maintenance + o_ins + o_util + prop_tax) * 25
rent_cost_25 <- #Total renting costs over 25 years
  (r_ins + r_util + rent) * 25
ann_inv <- 
    (sum(aDFyear$Annual_Payment) + own_cost_25 - rent_cost_25)/25  #The amount to be invested each year, 
                                            # i.e. the amount that would have been spent
                                            # on maintaining the mortgage/condo


# Data load ---------------------------------------------------------------

hpi <- as.data.frame(read.csv("Mortgage projection/HPI.csv"))
tsxi <- as.data.frame(read.csv("Mortgage projection/SP_TSX annual.csv"))
tsxi <- tsxi[, c(1:5)]

hpi$Growth <-  as.numeric(gsub('%', '', hpi$HPI_adj_for_infl))/100
tsxi$Growth <- as.numeric(gsub('%', '', tsxi$TSX_adj_for_infl))/100 #converts % to decimal

#Histogram of hpi and tsxi
hpi$Type <- 'hpi'
tsxi$Type <- 'tsxi'
#Data frame with just the coordinates of the .95 quantile ranges
q_x_hpi <- c(quantile(hpi$Growth, c(0.05, 0.95)))
q_x_tsxi <- c(quantile(tsxi$Growth, c(0.05, 0.95)))
q_hpi_range <- data.frame(cbind(q_x_hpi, c(1, 1)))
q_tsxi_range <- data.frame(cbind(q_x_tsxi, c(1, 1)))


# Returns Histogram -------------------------------------------------------

return_comp <- rbind(hpi[, c("Year", "Growth", "Type")], tsxi[, c("Year", "Growth", "Type")])
ggplot() + 
    geom_histogram(data = return_comp, aes(return_comp$Growth, fill = return_comp$Type),
                   alpha = 0.5, position = 'identity', 
                   breaks = seq(from=-0.6, to=0.6, by=0.05)) +
    geom_vline(aes(xintercept=median(return_comp[c(return_comp$Type == 'hpi'), 2])),
               color="#F8766D", size=1, linetype = 'dashed') +
    geom_vline(aes(xintercept=median(return_comp[c(return_comp$Type == 'tsxi'), 2])),
               color="#00BFC4", size=1, linetype = 'dashed') + 
    geom_line(data = q_hpi_range, aes(x = q_x_hpi, y = 1), colour = '#b5564f', size = 2) +
    geom_line(data = q_tsxi_range, aes(x = q_x_tsxi, y = 2), colour = '#009ba0', size = 2) +
    geom_text(aes(
        label = paste(round(median(
                                return_comp[c(return_comp$Type == 'hpi'), 2]*100
                                    ), 1), "%", sep = ""
                ),
        x = median(return_comp[c(return_comp$Type == 'hpi'), 2]),
        y = 20, hjust = -0.1)) + 
    geom_text(aes(
        label = paste(round(median(
                                return_comp[c(return_comp$Type == 'tsxi'), 2]*100
                                    ), 1), "%", sep = ""
                ),
        x = median(return_comp[c(return_comp$Type == 'tsxi'), 2]),
        y = 18, hjust = -0.1)) +
    labs(title="Distribution of Annual TSX Returns and HPI Growth", 
         y = "Frequency",
         fill = "",
         hpi = "test") +
    scale_x_continuous(name = "Annual growth", labels = scales::percent)


# "Buy" Simulation --------------------------------------------------------

#Simulation of property equity based on samples from hpi
sim_equ <- NULL  #make sure to run this before loop to reset sim
equ_log <- data.frame(Year = c(seq(from=2018, to=2043)))
sample_hpi <- NULL
sim_equ_gross <- NULL
for (i2 in c(1:1000)) {
  val = prop_price
  equ_log <- data.frame(cbind(equ_log, c(rep(0, 26))))      #Creates empty column to be filled with
  colnames(equ_log)[i2+1] <- as.character(i2)               # this sim's property value each year
  sample_hpi <- sample(hpi$Growth, 25, replace = TRUE)
  for (i3 in c(1:(length(sample_hpi)+1))) {
      if (i3 > 1) {#Don't apply the growth on the first year (want to record the original value)
          val <- val*(1+sample_hpi[i3-1])
      }
      equ_log[i3, i2+1] <- val #records the value for each year of each simulation
  }
  sim_equ_gross <- c(sim_equ_gross, val)
  sale_cost <- sell_cost(val) 
  sim_equ <- c(sim_equ, val - own_cost_25 - sum(aDFyear$Annual_Interest) - sale_cost) 
}
#Apply the sell_cost to the last year of equ_log: 
equ_log[26, c(2:1001)] <- equ_log[26, c(2:1001)]-sell_cost(sim_equ_gross)

#Create a timeline of the median property value across all sims for each year   
equ_yr <- data.frame(
    Year = c(seq(from=2018, to=2043)),
    Equity = c(apply(equ_log[, -1], 1, median))
    )
equ_yr <- cbind(equ_yr,
    Own_cost = c(0, rep(own_cost_25/25, 25)),
    Cost_year = c(0, equ_yr$Year[-1] - 2019 + 1),
    Ttl_int = c(0, cumsum(aDFyear$Annual_Interest)),
    Cost = c(0, -1 * own_cost_25/25*(equ_yr$Year[-1] - 2019 + 1) - cumsum(aDFyear$Annual_Interest)),
    Equ_net = c(equ_yr$Equity[1], 
                equ_yr$Equity[-1] - own_cost_25/25*(equ_yr$Year[-1] - 2019 + 1) 
                    - cumsum(aDFyear$Annual_Interest)),
    Equ_low_85 = c(equ_yr$Equity[1], apply(equ_log, 1, quantile, probs = 0.05)[-1]
                    - own_cost_25/25*(equ_yr$Year[-1] - 2019 + 1) - cumsum(aDFyear$Annual_Interest)),
    Equ_high_85 = c(equ_yr$Equity[1], apply(equ_log, 1, quantile, probs = 0.95)[-1]
                    - own_cost_25/25*(equ_yr$Year[-1] - 2019 + 1) - cumsum(aDFyear$Annual_Interest))
)
equ_yr$Equity[length(equ_yr$Equity)] = equ_yr$Equity[length(equ_yr$Equity)] + median(sell_cost(sim_equ_gross))


# "Rent" Simulation -------------------------------------------------------

# Applying sampled annual returns to a starting investment
sim_inv <- NULL #make sure to run this to reset the sim
inv_log <- data.frame(Year = c(seq(from=2018, to=2043)))
sample_tsxi <- NULL
for (i3 in c(1:1000)) {
    prpl <- inv
    inv_log <- data.frame(cbind(inv_log, c(rep(0, 26))))
    colnames(inv_log)[i3+1] <- as.character(i3)
    sample_tsxi <- sample(tsxi$Growth, 25, replace = TRUE)
    for (i4 in c(1:(length(sample_tsxi)+1))) {
        if (i4 > 1) { #Don't apply the growth on the first year (want to record the original value)
            prpl <- (prpl*(1+sample_tsxi[i4-1]-inv_fees) + ann_inv) #Invest the $ saved by not having a mortgage
            }
        inv_log[i4, i3+1] <- prpl #records the value for each year of each simulation
    }
    sim_inv <- c(sim_inv, prpl - rent_cost_25)
}
#Create a timeline of the median investment value across all sims for each year   
inv_yr <- data.frame(
    Year = c(seq(from=2018, to=2043)),
    Investment = c(apply(inv_log[, -1], 1, median))
)
inv_yr <- cbind(inv_yr,
    Inv_net = c(inv_yr$Investment[1], inv_yr$Investment[-1] - rent_cost_25/25*(inv_yr$Year[-1] - 2019 + 1)),
    Inv_low_85 = c(inv_yr$Investment[1], apply(inv_log, 1, quantile, probs = 0.05)[-1]
                   - rent_cost_25/25*(inv_yr$Year[-1] - 2019 + 1)),
    Inv_high_85 = c(inv_yr$Investment[1], apply(inv_log, 1, quantile, probs = 0.95)[-1]
                    - rent_cost_25/25*(inv_yr$Year[-1] - 2019 + 1))
)

#Combined histogram - investment vs equity simulations:
inv_df <- c()
equ_df <- c()
inv_df <- as.data.frame(sim_inv)
colnames(inv_df) <- c('mean')
equ_df <- as.data.frame(sim_equ)
colnames(equ_df) <- c('mean')


# Results Histogram -------------------------------------------------------

#Combine both datasets into one data frame. Add column "sim" to distinguish
# between which dataset each row came from. 
equ_df$sim <- 'Buy'
inv_df$sim <- 'Rent'
val_sims <- rbind(inv_df, equ_df)
#Data frame with just the coordinates of the .95 quantile ranges
q_x_e <- quantile(val_sims[c(val_sims$sim == 'Buy'), 1], c(0.05, 0.95))/1000
q_x_i <- quantile(val_sims[c(val_sims$sim == 'Rent'), 1], c(0.05, 0.95))/1000
q_e_range <- data.frame(cbind(q_x_e, c(1, 1)))
q_i_range <- data.frame(cbind(q_x_i, c(1, 1)))

#Base histogram
com_hist <- ggplot() + 
    geom_histogram(data = val_sims, aes(mean/1000, fill = sim),
                   alpha = 0.5, position = 'identity', bins = 50) +
    geom_vline(aes(xintercept=median(val_sims[c(val_sims$sim == 'Buy'), 1])/1000),
               color="#F8766D", size=1, linetype = 'dashed') +
    geom_vline(aes(xintercept=median(val_sims[c(val_sims$sim == 'Rent'), 1])/1000),
           color="#00BFC4", size=1, linetype = 'dashed')
#Bin heights to be used for mean labels
bin_height <- ggplot_build(com_hist)$data[[1]][["y"]]
equ_bin_max <- max(bin_height[c(ggplot_build(com_hist)$data[[1]]["group"] == 1)])
inv_bin_max <- max(bin_height[c(ggplot_build(com_hist)$data[[1]]["group"] == 2)])
#Add text labels:
com_hist <- com_hist +
    geom_text(aes(label = paste(round(median(val_sims[c(val_sims$sim == 'Buy'), 1])/1000, 0), "K"),
                  x = median(val_sims[c(val_sims$sim == 'Buy'), 1])/1000,
                  y = equ_bin_max*1.1, hjust = -0.1)) + 
    geom_text(aes(label = paste(round(median(val_sims[c(val_sims$sim == 'Rent'), 1])/1000, 0), "K"),
                  x = median(val_sims[c(val_sims$sim == 'Rent'), 1])/1000,
                  y = inv_bin_max*1.1, hjust = -0.1)) +
    geom_line(data = q_e_range, aes(x = q_x_e, y = equ_bin_max*0.05, colour = "Buy"), size = 2) +
    geom_line(data = q_i_range, aes(x = q_x_i, y = inv_bin_max*0.1, colour = "Rent"), size = 2) +
    scale_colour_manual(name = "Range of \n90% of Simulations:", 
    values = c("Buy"="#b5564f", "Rent"="#009ba0")) + 
               labs(title="1000 Simulations of Sampled TSX Returns and HPI Growth",
               y = "Count of Simulations",
               x = "Asset Value Minus Costs, Thousands",
               fill = "")

com_hist


# Results Timelines -------------------------------------------------------

#Timeline of property/investment value
timeline <- merge(equ_yr, inv_yr)
ggplot(data = timeline, aes(timeline)) + 
    geom_line(aes(x = Year, y = Equ_net/1000, colour = "Buy"), size = 1.5) + 
    geom_line(aes(x = Year, y = Inv_net/1000, colour = "Rent"), size = 1.5) +
    geom_ribbon(aes(x = Year, ymin = Equ_low_85/1000, ymax = Equ_high_85/1000), 
                alpha = 0.15, fill = "#F8766D") +
    geom_ribbon(aes(x = Year, ymin = Inv_low_85/1000, ymax = Inv_high_85/1000), 
                alpha = 0.15, fill = "#00BFC4") +
    scale_y_continuous(name = "Median Simulation Value, Thousandss") +
    scale_x_continuous(limits = c(min(timeline$Year), max(timeline$Year)+1), name = NULL) +
    geom_text(aes(label = paste(round(tail(Equ_net, 1)/1000, 0), "K"),
                 x = tail(Year, 1),
                 y = 0.7*tail(Equ_net, 1)/1000)) +
    geom_text(aes(label = paste(round(tail(Inv_net, 1)/1000, 0), "K"),
                  x = tail(Year, 1),
                  y = 1.2*tail(Inv_net, 1)/1000)) +
    scale_colour_manual(name = NULL, 
                        values =c('Buy'='#F8766D','Rent'='#00BFC4'), labels = c('Buy','Rent')) +
    labs(title="Timeline of Simulations")

#Timeline with the median total value of the home/investment before costs
ggplot(data = timeline, aes(timeline)) + 
    geom_line(aes(x = Year, y = Equity/1000, colour = "Buy", linetype = "Asset Value"), size = 1) + 
    geom_line(aes(x = Year, y = Equ_net/1000, colour = "Buy", linetype = "Net Value"), size = 1) + 
    geom_line(aes(x = Year, y = Investment/1000, colour = "Rent", linetype = "Asset Value"), size = 1) +
    geom_line(aes(x = Year, y = Inv_net/1000, colour = "Rent", linetype = "Net Value"), size = 1) + 
    scale_y_continuous(limits = c(0, NA), name = "Median Simulation Value, Thousands", 
                       labels = scales::comma) +
    scale_x_continuous(limits = c(min(timeline$Year), max(timeline$Year)+1), name = NULL) +
    geom_text(aes(label = paste(round(tail(Equ_net, 1)/1000, 0), "K"),
                  x = tail(Year, 1),
                  y = 0.8*tail(Equ_net, 1)/1000)) +
    geom_text(aes(label = paste(round(tail(Inv_net, 1)/1000, 0), "K"),
                  x = tail(Year, 1),
                  y = 1.1*tail(Inv_net, 1)/1000)) + 
    geom_text(aes(label = paste(round(tail(Equity, 1)/1000, 0), "K"),
                  x = tail(Year, 1),
                  y = 0.8*tail(Equity, 1)/1000)) +
    geom_text(aes(label = paste(round(tail(Investment, 1)/1000, 0), "K"),
                  x = tail(Year, 1),
                  y = 1.1*tail(Investment, 1)/1000)) +
    scale_linetype_manual(name = NULL, values = c("Net Value" = "solid", "Asset Value" = "dashed")) +
    scale_colour_manual(name = NULL, values =c('Buy'='#F8766D','Rent'='#00BFC4'), labels = c('Buy','Rent')) +
    labs(title="Timeline of Simulations - With and Without Costs")


# Miscellaneous -----------------------------------------------------------

#In how many simulations did rent outperform buy?
sim_comp <- c()
sim_comp <- data.frame(cbind(equ_df[, 1], inv_df[, 1]))
colnames(sim_comp) <- c("equ", "inv")
sim_comp$equ_better <- c(sim_comp$equ < sim_comp$inv)
sum(sim_comp$equ_better)

#Lets go back to historical performance - what would have been the best choice in each year 
# since 1981 (the ealiest date we have TSX data)?

hpi$HPI_Growth <- hpi$Growth
tsxi$TSXI_Growth <- tsxi$Growth

rw_df <- merge(hpi[, c("Year", "HPI_Growth")], 
               tsxi[, c("Year", "TSXI_Growth")], 
               all.y = T) 
rw_equ <- NULL
rw_inv <- NULL
loop_test <- NULL
for (i in c(1:(2017-1980-25))) {    # Loop will run 12 times, the number of different 25-year 
    val = prop_price                # time frames between 1980 and 2017.
    prpl = inv
    for (i2 in c(1:25)) {
        real_hpi = rw_df[i-1+i2, "HPI_Growth"]
        real_tsxi = rw_df[i-1+i2, "TSXI_Growth"]
        val <- val*(1+real_hpi)
        prpl <- prpl*(1+real_tsxi-inv_fees) + ann_inv
        loop_test <- rbind(loop_test, c(1979+i, i, i2, real_hpi, val, real_tsxi, prpl))
    }
    rw_equ <- c(rw_equ, val - own_cost_25)
    rw_inv <- c(rw_inv, prpl - rent_cost_25 - sum(aDFyear$Annual_Interest))
}

rw_sim <- data.frame(cbind(c(1980:1991), rw_equ, rw_inv, "Diff" = round(rw_inv - rw_equ, 0)))

#Comparion of equ outcomes without interest
ggplot(data = timeline, aes(timeline)) + 
    geom_line(aes(x = Year, y = Equ_net/1000), colour = "#F8766D", size = 1) + 
    geom_line(aes(x = Year, y = Inv_net/1000), colour = "#00BFC4", size = 1) +
    geom_line(aes(x = Year, y = Equity/1000), colour = "#F8766D", size = 1, linetype = "dashed") + 
    geom_line(aes(x = Year, y = Equ_no_inrst/1000), colour = "#f4a8a1", size = 1, linetype = "dotted") + 
    geom_line(aes(x = Year, y = Equ_no_others/1000), colour = "#f2afa9", size = 1, linetype = "dashed") + 
    scale_y_continuous(limits = c(0, NA), name = "Value, thousands", labels = scales::comma)

# Assumption tests:
#Test whether HPI an TSX fluctuations are correlated:
cor.test(
  tsxi$Growth, 
  hpi$Growth[c( #gets just the HPI years that overlap with TSX_Growth
    (length(hpi$HPI) - length(tsxi$Growth) + 1):length(hpi$HPI)
  )])
