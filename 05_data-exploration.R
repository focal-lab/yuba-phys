DATADIR = "/Users/anjumgujral/Box Sync/yuba-phys_data/traits"
trait_data <- read.csv("/Users/anjumgujral/Box Sync/yuba-phys_data/traits/yuba-phys-data.csv")
hist(trait_data$stand_basal_area)
plot(trait_data$height_m~trait_data$DBH_cm)

install.packages('GGally')
library('GGally')
ggpairs(trait_data, columns = 1:14)
boxplot(trait_data$PAR_μmol~trait_data$plot)


