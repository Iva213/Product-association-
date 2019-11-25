## Market basket analysis - product association 
## version 2

############################################

## Getting started 
setwd("/Users/Iva/Desktop/Data Analytics & Machine Learning/R/Data Analytics II")

library(arules)
library(arulesViz)
library(dplyr)
library(fpc)
library(data.table)
library(stringr)
library(sqldf)
library(DescTools)

set.seed(123)

#electron <- read.csv("ElectronidexTransactions2017.csv")
electron <- read.transactions("ElectronidexTransactions2017.csv", format = "basket", sep = ",")
#NO product brands - e.g. laptop, desktop, mouse etc. 
electron.gen <- read.transactions("ElectronidexTransactions2017_types.csv", format = "basket", sep = ",")

############################################

## Visualise data 
inspect(electron) # You can view the transactions. 
length(electron) # Number of transactions.
size(electron) # Number of items per transaction
LIST(electron) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(electron)# To see the item labels

itemFrequencyPlot(electron, topN = 15, type = "absolute")
itemFrequencyPlot(electron.gen)
image(electron)
image(sample(electron, 100))

frequent.items <- eclat(electron.gen, parameter = list(supp = 0.07, maxlen = 15))
inspect(sort(frequent.items, by = "support"))
itemFrequencyPlot(electron.gen, topN = 15, type = "absolute")

############################################

## Association mining 
#electron.rules = increasing support (0.02) gives 0 rules; 0.55 conf gives 9 rules 

electron.rules <- apriori(electron, parameter = list(supp = 0.01, conf = 0.50, minlen = 2))
electron.gen.rules <- apriori(electron.gen, parameter = list(supp = 0.08, conf = 0.55, minlen = 2))

#Sort by order of confidence / lift / support 
electron.rules <- sort(electron.rules, by = "lift", decreasing = TRUE) 
electron.gen.rules <- sort(electron.gen.rules, by = "lift", decreasing = TRUE)

inspect(electron.rules)
inspect(electron.gen.rules)

#Remove redundant rules - THERE ARE 0 
#subset.rules <- which(colSums(is.subset(electron.rules, electron.rules)) > 1) 
is.redundant(electron.gen.rules, measure = "lift")

#Look for strongest rules -- Fisher's Exact Test
is.significant(electron.gen.rules, electron.gen)

#Look at specific rules
#subset.rules <- inspect(subset(electron.gen.rules, lhs %in% "Desktop"))

laptop.rules <- apriori(electron.gen, parameter =list (supp=0.01, conf = 0.5, minlen = 2), 
                  appearance = list(default = "lhs", rhs = "Laptops"))
desktop.rules <- apriori(electron.gen, parameter =list (supp=0.01, conf = 0.5, minlen = 2), 
                        appearance = list(default = "lhs", rhs = "Desktop"))
monitor.rules <- apriori(electron.gen, parameter =list (supp=0.01, conf = 0.5, minlen = 2), 
                         appearance = list(default = "lhs", rhs = "Monitors"))

laptop.rules <- sort(laptop.rules, by = "lift", decreasing = TRUE) 
inspect(head(laptop.rules, 5))
write.csv(top.10.laptop, "top.10.laptop.csv")
desktop.rules <- sort(desktop.rules, by = "lift", decreasing = TRUE) 
inspect(head(desktop.rules, 5))

monitor.rules <- sort(monitor.rules, by = "lift", decreasing = TRUE) 
inspect(head(monitor.rules, 5))

plot(monitor.rules[1:5], method = "graph", cex = 0.9,
     control = list(type = "items"), shading = "lift")

############################################

## Visualisation 

plot(monitor.rules, method = "scatterplot", shading = "lift", main = "Monitor purchases")

plot(electron.gen.rules[1:15], method = "graph", cex = 1,
     control = list(type = "items"), shading = "lift")
plot(electron.gen.rules, method="grouped")
inspectDT(electron.gen.rules)

plot(electron.gen.rules, measure=c("support", "lift"), shading = "confidence", interactive = TRUE)

