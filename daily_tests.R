



library(ggplot2)



library(reshape2)







######

number_of_tests <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_tests.csv")

number_of_tests[,1] <- as.Date(number_of_tests[,1])

number_of_tests_plot <- ggplot(number_of_tests, aes(x=DATE, y=TESTS, fill=TESTS)) +
	geom_bar(stat = "identity", fill="#95dee3") +
	geom_text(aes(y=TESTS,label=TESTS), angle=90, hjust=1, size=3) +
	scale_x_date(date_breaks = "days" , date_labels = "%a %m / %d") +
	ggtitle("Amount of COVID-19 tests performed in Belgium per day") +
	theme_minimal() +
	xlab("") +
	ylab("") +
	labs(caption = "Source: https://epistat.sciensano.be/Data/COVID19BE_tests.csv") +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))


number_of_tests_plot

ggsave(number_of_tests_plot, filename = paste0(make.names(Sys.time()),"_tests_per_day.png"), device = "png", width = 10, height = 8)
######



cum_testno <- number_of_tests
cum_testno$TESTS <- cumsum(number_of_tests$TESTS)

number_of_tests_cum_plot <- ggplot(cum_testno, aes(x=DATE, y=TESTS, fill=TESTS)) +
	geom_area(fill="#98b4d4") +
	#geom_bar(stat = "identity", fill="#98b4d4") +
	#geom_text(aes(y=TESTS,label=TESTS), angle=90, hjust=1, size=3) +
	scale_x_date(date_breaks = "days" , date_labels = "%a %m / %d") +
	ggtitle("Amount of COVID-19 tests performed in Belgium") +
	theme_minimal() +
	xlab("") +
	ylab("") +
	labs(caption = "Source: https://epistat.sciensano.be/Data/COVID19BE_tests.csv") +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))


number_of_tests_cum_plot
ggsave(number_of_tests_cum_plot, filename = paste0(make.names(Sys.time()),"_tests_cum.png"), device = "png", width = 10, height = 8)





## Convirmed cases



agesex <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")




agesex[,"DATE"] <- as.Date(agesex[,"DATE"])


totals <- aggregate(agesex$CASES, by=list(Category=agesex$DATE), FUN=sum)
colnames(totals) <- c("DATE","CASES")

Sys.setlocale("LC_TIME", "C")



ggplot(totals, aes(x=DATE,y=CASES)) +
	geom_bar(stat = "identity")







merged <- merge(number_of_tests, totals, by="DATE")
merged$RATIO <- merged$CASES / merged$TESTS

molten <- melt(merged, id.vars="DATE")


ggplot(molten, aes(x=DATE,y=value, fill=variable)) +
	geom_bar(stat = "identity", col="black") +
	facet_grid(vars(factor(variable, levels = c("CASES","TESTS","RATIO"))), scales = "free") +
	scale_x_date(date_breaks = "weeks" , date_labels = "%b / %d") +
	xlab("") +
	ylab("") +
	ggtitle("Daily new cases and tests in Belgium") +
	labs(caption = "Source: https://epistat.sciensano.be/Data/COVID19BE_tests.csv", subtitle = paste0("as reported on ", Sys.time())) +
	scale_fill_brewer(palette = "Set1") +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),legend.position = "none")



ggsave(filename = paste0("cases_test_ratio_summary_",Sys.Date(),".png"), device = "png", scale = 1.5)

