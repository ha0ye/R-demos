library(rEDM)


data()

mel_rates <- read.csv("Desktop/sect_16_table.05.csv", skip = 4, nrows = 41, stringsAsFactors = FALSE)[-1, 1:2]
names(mel_rates) <- c("year", "incidence")
mel_rates$year <- as.numeric(mel_rates$year)

lin_reg <- lm(incidence ~ year, data = mel_rates)
lin_reg$residuals

sunspot <- read.delim("Desktop/SN_y_tot_V2.0.csv", sep = ";", header = FALSE)
sunspot[,1] <- sunspot[,1] - 0.5
sunspot <- sunspot[, 1:2]
plot(sunspot[,1], sunspot[,2], type = "l")
lines(1700:1988, sunspot.year*sqrt(2), col = "blue")

df <- data.frame(year = mel_rates$year, 
                 mel_resid = lin_reg$residuals, 
                 sunspot = sunspot[match(mel_rates$year, sunspot[,1]),2])
ccm_out <- ccm(df, E = 4, lib_column = "mel_resid", target_column = "sunspot", 
               random_libs = FALSE, lib_sizes = seq(from = NROW(df), to = 10, by = -5))
ccm_means(ccm_out)
