nuclear_data <- read.table(file = "Obligs/nuclear.dat"  , header = TRUE, sep = "\t")
y = log(nuclear_data$cost)