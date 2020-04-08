#" 
#  Analysis on terrorist dataset. Dataset inlcudes three colunms. 
#  --> 1st Colunmn = Terrorist Organization 
#  --> 2nd Colunmn = Country name (it is the name of the country in which the corresponding org. attacked)
#  --> 3rd columnn = Number of atrakcs done by the terror org
#"

library(plotrix)

# Read csv data
csvData = read.csv('terrorist_data.csv', sep = ',')
print(csvData)

# get the details of the country which has maximum number of attacaks
maxAttack = subset(csvData, Number.of.Attacks == max(as.numeric(as.character(Number.of.Attacks))))
print(maxAttack)

# pie chart for number of attacks done by the given terror organization
pieData = subset(csvData, Number.of.Attacks > 10)
#      print(pieData)
terOrg = pieData$Terrorist.Organization
#      print(terOrg)
noOfAttacks = pieData$Number.of.Attacks
#      print(noOfAttacks)
pie(noOfAttacks, labels = terOrg, main = "Pie chart of attacks done by the various terrorist organization.")

# Bar plot for which organiztion had done how many numbers of attack
par(mar=c(13, 6, 6, 6)) # Change the margin of bar to display it completely
barplot(noOfAttacks, names.arg = terOrg, main = "Bar plot for which organiztion had done how many numbers of attacks", ylim = c(0, 100), xlab = "Terrorist Organization", ylab = "Number of Attacks", las = 2)
country = pieData$Country
barplot(c(terOrg), name.arg = country, main = "Number of Terrorist Organization attacked the given Countries", las = 2, xlab = "Country", ylab = "Terrorist Organization")

# analysis for terror groups in pakistan and number of attacks done by each terror group
par(mar=c(13, 6, 6, 6)) 
pakData = subset(csvData, Country == "Pakistan")
barplot(pakData$Number.of.Attacks, names.arg = pakData$Terrorist.Organization, ylim = c(0, 20), main = "Bar for plot for terror groups in Pakistan", xlab = "Terror Groups in Pakistan", ylab = "No. of Attacks.", las = 2)


# analysis on which country have how many number of terror attacks.
par(mar=c(7, 3, 3, 3))
tableData = table(csvData$Country)
barplot(tableData, las = 2, ylim = c(0, 40))

# Visualization for number of attacks done by which country and which terror organization
ggplot(data = pieData, mapping = aes(x = Country, y = Number.of.Attacks)) +
  geom_point(aes(color = Terrorist.Organization)) +
  geom_jitter(alpha = 0.3, color = "tomato") +
  theme(axis.text.x = element_text(angle=270)) +
  labs(title = "Attacks made by given Terrorist Organization on given Countries",
       x = "Country",
       y = "Number of Attacks")