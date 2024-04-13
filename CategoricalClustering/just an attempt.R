
my_data <- read.csv('Complete.csv', stringsAsFactors = TRUE)


features <- c( 'Marke', 'Produktart', 'Verpackungsart', 'Material')


my_data$group_ID <- as.integer(interaction(my_data[features]))


head(my_data)
