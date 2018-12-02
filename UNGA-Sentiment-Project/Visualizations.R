# load the libraries you'll need
library(tidyverse)
library(ggrepel)
library(cowplot)
library(countrycode)
library(ggcorrplot)
# data set with the details and sentiment scores of the floor speeches
UN <- read.csv('/Users/brodyvogel/Desktop/UN_Floor_Speeches_Sentiment.csv')
# convert COW to country name
UN$Country <- countrycode(UN$Country.Code, 'iso3c', 'country.name.en')
########################### SUMMARY STUFF, BY YEAR ##################################################################
# compute summary statistics for the speeches by year
Yr_Avg <- aggregate(UN, list(UN$Year), FUN = mean, na.rm = T)
# this is just the average length of a floor speech, by year
Yr_Avg_length <-  Yr_Avg %>% select(Group.1, Length)
# visualize the average length of a floor speech by year
Yr_Avg_length_plot <- ggplot(Yr_Avg_length, aes(x = Group.1, y = Length)) +
  geom_point(size = 4) +
  stat_smooth(method = 'loess', color = 'blue', se = F, linetype = 'dashed', show.legend = F) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5), axis.title.x = element_blank()) +
  scale_x_continuous(Yr_Avg_length$Group.1, breaks = Yr_Avg_length$Group.1, labels = Yr_Avg_length$Group.1) +
  labs(title = 'Length of Floor Speeches Over Time', y = 'Average Length (in Words)')
Yr_Avg_length_plot
########################### SUMMARIES BY TOPIC, BY YEAR ######################################################
# this collects and shows details of the floor speeches by specific category (subject being discussed), by year  
Yr_ICCT <- ggplot(subset(Yr_Avg, !is.na(Yr_Avg$ICCT.Sent.)), aes(x = Group.1, y = ICCT.Sent., size = ICCT)) +
  geom_point(fill = 'gray') +
  stat_smooth(method = 'loess', color = 'blue', se = F, linetype = 'dashed', show.legend = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5), axis.title.x = element_blank()) +
  scale_x_continuous(Yr_Avg$Group.1[!is.na(Yr_Avg$ICCT.Sent.)], labels = Yr_Avg$Group.1[!is.na(Yr_Avg$ICCT.Sent.)], breaks = Yr_Avg$Group.1[!is.na(Yr_Avg$ICCT.Sent.)]) +
  labs(y = 'Average Sentiment Score', title = 'International Criminal Court and Tribunals', size = 'Avg. Mentions \n per Speech')

Yr_ICSID <- ggplot(subset(Yr_Avg, !is.na(Yr_Avg$ICSID.Sent.)), aes(x = Group.1, y = ICSID.Sent., size = ICSID)) +
  geom_point(fill = 'gray') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5), axis.title.x = element_blank()) +
  scale_x_continuous(Yr_Avg$Group.1[!is.na(Yr_Avg$ICSID.Sent.)], labels = Yr_Avg$Group.1[!is.na(Yr_Avg$ICSID.Sent.)], breaks = Yr_Avg$Group.1[!is.na(Yr_Avg$ICSID.Sent.)]) +
  labs(y = 'Average Sentiment Score', title = 'International Centre for Settlement of Investment Disputes', size = 'Avg. Mentions \n per Speech')

Yr_IMF <- ggplot(subset(Yr_Avg, !is.na(Yr_Avg$IMF.Sent.)), aes(x = Group.1, y = IMF.Sent., size = IMF)) +
  geom_point(fill = 'gray') +
  stat_smooth(method = 'loess', color = 'blue', se = F, linetype = 'dashed', show.legend = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5), axis.title.x = element_blank()) +
  scale_x_continuous(Yr_Avg$Group.1[!is.na(Yr_Avg$IMF.Sent.)], labels = Yr_Avg$Group.1[!is.na(Yr_Avg$IMF.Sent.)], breaks = Yr_Avg$Group.1[!is.na(Yr_Avg$IMF.Sent.)]) +
  labs(y = 'Average Sentiment Score', title = 'International Monetary Fund', size = 'Avg. Mentions \n per Speech')

Yr_IFIs <- ggplot(subset(Yr_Avg, !is.na(Yr_Avg$IFIs.Sent.)), aes(x = Group.1, y = IFIs.Sent., size = IFIs)) +
  geom_point(fill = 'gray') +
  stat_smooth(method = 'loess', color = 'blue', se = F, linetype = 'dashed', show.legend = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5), axis.title.x = element_blank()) +
  scale_x_continuous(Yr_Avg$Group.1[!is.na(Yr_Avg$IFIs.Sent.)], labels = Yr_Avg$Group.1[!is.na(Yr_Avg$IFIs.Sent.)], breaks = Yr_Avg$Group.1[!is.na(Yr_Avg$IFIs.Sent.)]) +
  labs(y = 'Average Sentiment Score', title = 'International Finance Institutions', size = 'Avg. Mentions \n per Speech')

Yr_Investment.Arbitration <- ggplot(subset(Yr_Avg, !is.na(Yr_Avg$Investment.Arbitration.Sent.)), aes(x = Group.1, y = Investment.Arbitration.Sent., size = Investment.Arbitration)) +
  geom_point(fill = 'gray') +
  stat_smooth(method = 'loess', color = 'blue', se = F, linetype = 'dashed', show.legend = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5), axis.title.x = element_blank()) +
  scale_x_continuous(Yr_Avg$Group.1[!is.na(Yr_Avg$Investment.Arbitration.Sent.)], labels = Yr_Avg$Group.1[!is.na(Yr_Avg$Investment.Arbitration.Sent.)], breaks = Yr_Avg$Group.1[!is.na(Yr_Avg$Investment.Arbitration.Sent.)]) +
  labs(y = 'Average Sentiment Score', title = 'Investment Arbitration', size = 'Avg. Mentions \n per Speech')

Yr_NIEO <- ggplot(subset(Yr_Avg, !is.na(Yr_Avg$NIEO.Sent.)), aes(x = Group.1, y = NIEO.Sent., size = NIEO)) +
  geom_point(fill = 'gray') +
  stat_smooth(method = 'loess', color = 'blue', se = F, linetype = 'dashed', show.legend = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5), axis.title.x = element_blank()) +
  scale_x_continuous(Yr_Avg$Group.1[!is.na(Yr_Avg$NIEO.Sent.)], labels = Yr_Avg$Group.1[!is.na(Yr_Avg$NIEO.Sent.)], breaks = Yr_Avg$Group.1[!is.na(Yr_Avg$NIEO.Sent.)]) +
  labs(y = 'Average Sentiment Score', title = 'New International Economic Order', size = 'Avg. Mentions \n per Speech')

Yr_UN.Human.Rights.System <- ggplot(subset(Yr_Avg, !is.na(Yr_Avg$UN.Human.Rights.System.Sent.)), aes(x = Group.1, y = UN.Human.Rights.System.Sent., size = UN.Human.Rights.System)) +
  geom_point(fill = 'gray') +
  stat_smooth(method = 'loess', color = 'blue', se = F, linetype = 'dashed', show.legend = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5), axis.title.x = element_blank()) +
  scale_x_continuous(Yr_Avg$Group.1[!is.na(Yr_Avg$UN.Human.Rights.System.Sent.)], labels = Yr_Avg$Group.1[!is.na(Yr_Avg$UN.Human.Rights.System.Sent.)], breaks = Yr_Avg$Group.1[!is.na(Yr_Avg$UN.Human.Rights.System.Sent.)]) +
  labs(y = 'Average Sentiment Score', title = 'UN Human Rights System', size = 'Avg. Mentions \n per Speech')

Yr_UNCITRAL <- ggplot(subset(Yr_Avg, !is.na(Yr_Avg$UNCITRAL.Sent.)), aes(x = Group.1, y = UNCITRAL.Sent., size = UNCITRAL)) +
  geom_point(fill = 'gray') +
  stat_smooth(method = 'loess', color = 'blue', se = F, linetype = 'dashed', show.legend = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5), axis.title.x = element_blank()) +
  scale_x_continuous(Yr_Avg$Group.1[!is.na(Yr_Avg$UNCITRAL.Sent.)], labels = Yr_Avg$Group.1[!is.na(Yr_Avg$UNCITRAL.Sent.)], breaks = Yr_Avg$Group.1[!is.na(Yr_Avg$UNCITRAL.Sent.)]) +
  labs(y = 'Average Sentiment Score', title = 'United Nations Commission on International Trade Law', size = 'Avg. Mentions \n per Speech')

Yr_WTO.GATT <- ggplot(subset(Yr_Avg, !is.na(Yr_Avg$WTO.GATT.Sent.)), aes(x = Group.1, y = WTO.GATT.Sent., size = WTO.GATT)) +
  geom_point(fill = 'gray') +
  stat_smooth(method = 'loess', color = 'blue', se = F, linetype = 'dashed', show.legend = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5), axis.title.x = element_blank()) +
  scale_x_continuous(Yr_Avg$Group.1[!is.na(Yr_Avg$WTO.GATT.Sent.)], labels = Yr_Avg$Group.1[!is.na(Yr_Avg$WTO.GATT.Sent.)], breaks = Yr_Avg$Group.1[!is.na(Yr_Avg$WTO.GATT.Sent.)]) +
  labs(y = 'Average Sentiment Score', title = 'World Trade Organization / General Agreement on Tariffs and Trade', size = 'Avg. Mentions \n per Speech')

Yr_World.Bank <- ggplot(subset(Yr_Avg, !is.na(Yr_Avg$World.Bank.Sent.)), aes(x = Group.1, y = World.Bank.Sent., size = World.Bank)) +
  geom_point(fill = 'gray') +
  stat_smooth(method = 'loess', color = 'blue', se = F, linetype = 'dashed', show.legend = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5), axis.title.x = element_blank()) +
  scale_x_continuous(Yr_Avg$Group.1[!is.na(Yr_Avg$World.Bank.Sent.)], labels = Yr_Avg$Group.1[!is.na(Yr_Avg$World.Bank.Sent.)], breaks = Yr_Avg$Group.1[!is.na(Yr_Avg$World.Bank.Sent.)]) +
  labs(y = 'Average Sentiment Score', title = 'World Bank', size = 'Avg. Mentions \n per Speech')

# create a grid of summaries for the specifc subject plots
# ICCT, ICSID, Investment Arbitration, and UNCITRAL are not interesting (hardly ever come up)
plot_grid(Yr_ICCT, Yr_ICSID, Yr_IFIs, Yr_IMF, Yr_Investment.Arbitration, Yr_NIEO,
          ncol = 2, nrow = 3)
plot_grid(Yr_UN.Human.Rights.System, Yr_UNCITRAL, Yr_WTO.GATT, Yr_World.Bank, ncol = 2, nrow = 2)
########################### SUMMARY STUFF, BY COUNTRY ##################################################################
Country_Avg <- aggregate(UN, list(UN$Country), FUN = mean, na.rm = T)
# average length of a floor speech for each country, cut down to top and bottom 10
Country_Avg_length <-  Country_Avg %>% select(Group.1, Length) %>% arrange(desc(Length))
C_A_l_top <- Country_Avg_length %>% top_n(10)
C_A_l_bottom <- Country_Avg_length %>% top_n(-10)
Country_Avg_length <- rbind(C_A_l_top, C_A_l_bottom)
# visualize the most and least talkative countries
Country_Avg_length_plot <- ggplot(Country_Avg_length, aes(x = reorder(Group.1, Length), y = Length)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  coord_flip() + 
  labs(title = "10 Most and Least Talkative Countries", x = '', y = 'Average Speech Length, in Words')
Country_Avg_length_plot
########################### SUMMARIES BY TOPIC, BY COUNTRY ##################################################################
# this builds dataframes for the top and bottom ten countries (top being most positive of subject, bottom being most negative)
ICCT_top <- Country_Avg %>% top_n(10, ICCT.Sent.) %>% arrange(desc(ICCT.Sent.))
ICCT_bot <- Country_Avg %>% top_n(-10, ICCT.Sent.) %>% arrange(desc(ICCT.Sent.))
Country_ICCT <- rbind(ICCT_top, ICCT_bot) %>% select(Group.1, ICCT, ICCT.Sent.)

ICSID_top <- Country_Avg %>% top_n(10, ICSID.Sent.) %>% arrange(desc(ICSID.Sent.))
ICSID_bot <- Country_Avg %>% top_n(-10, ICSID.Sent.) %>% arrange(desc(ICSID.Sent.))
Country_ICSID <- rbind(ICSID_top, ICSID_bot) %>% select(Group.1, ICSID, ICSID.Sent.)

IFIs_top <- Country_Avg %>% top_n(10, IFIs.Sent.) %>% arrange(desc(IFIs.Sent.))
IFIs_bot <- Country_Avg %>% top_n(-10, IFIs.Sent.) %>% arrange(desc(IFIs.Sent.))
Country_IFIs <- rbind(IFIs_top, IFIs_bot) %>% select(Group.1, IFIs, IFIs.Sent.)

IMF_top <- Country_Avg %>% top_n(10, IMF.Sent.) %>% arrange(desc(IMF.Sent.))
IMF_bot <- Country_Avg %>% top_n(-10, IMF.Sent.) %>% arrange(desc(IMF.Sent.))
Country_IMF <- rbind(IMF_top, IMF_bot) %>% select(Group.1, IMF, IMF.Sent.)

Investment.Arbitration_top <- Country_Avg %>% top_n(10, Investment.Arbitration.Sent.) %>% arrange(desc(Investment.Arbitration.Sent.))
Investment.Arbitration_bot <- Country_Avg %>% top_n(-10, Investment.Arbitration.Sent.) %>% arrange(desc(Investment.Arbitration.Sent.))
Country_Investment.Arbitration <- rbind(Investment.Arbitration_top, Investment.Arbitration_bot) %>% select(Group.1, Investment.Arbitration, Investment.Arbitration.Sent.)

NIEO_top <- Country_Avg %>% top_n(10, NIEO.Sent.) %>% arrange(desc(NIEO.Sent.))
NIEO_bot <- Country_Avg %>% top_n(-10, NIEO.Sent.) %>% arrange(desc(NIEO.Sent.))
Country_NIEO <- rbind(NIEO_top, NIEO_bot) %>% select(Group.1, NIEO, NIEO.Sent.)

UN.Human.Rights.System_top <- Country_Avg %>% top_n(10, UN.Human.Rights.System.Sent.) %>% arrange(desc(UN.Human.Rights.System.Sent.))
UN.Human.Rights.System_bot <- Country_Avg %>% top_n(-10, UN.Human.Rights.System.Sent.) %>% arrange(desc(UN.Human.Rights.System.Sent.))
Country_UN.Human.Rights.System <- rbind(UN.Human.Rights.System_top, UN.Human.Rights.System_bot) %>% select(Group.1, UN.Human.Rights.System, UN.Human.Rights.System.Sent.)

UNCITRAL_top <- Country_Avg %>% top_n(10, UNCITRAL.Sent.) %>% arrange(desc(UNCITRAL.Sent.))
UNCITRAL_bot <- Country_Avg %>% top_n(-10, UNCITRAL.Sent.) %>% arrange(desc(UNCITRAL.Sent.))
Country_UNCITRAL <- rbind(UNCITRAL_top, UNCITRAL_bot) %>% select(Group.1, UNCITRAL, UNCITRAL.Sent.)

WTO.GATT_top <- Country_Avg %>% top_n(10, WTO.GATT.Sent.) %>% arrange(desc(WTO.GATT.Sent.))
WTO.GATT_bot <- Country_Avg %>% top_n(-10, WTO.GATT.Sent.) %>% arrange(desc(WTO.GATT.Sent.))
Country_WTO.GATT <- rbind(WTO.GATT_top, WTO.GATT_bot) %>% select(Group.1, WTO.GATT, WTO.GATT.Sent.)

World.Bank_top <- Country_Avg %>% top_n(10, World.Bank.Sent.) %>% arrange(desc(World.Bank.Sent.))
World.Bank_bot <- Country_Avg %>% top_n(-10, World.Bank.Sent.) %>% arrange(desc(World.Bank.Sent.))
Country_World.Bank <- rbind(World.Bank_top, World.Bank_bot) %>% select(Group.1, World.Bank, World.Bank.Sent.)
# this visualizes the countries that speak most positively and most negatively about each subject
Country_ICCT_plot <- ggplot(Country_ICCT, aes(x = reorder(Group.1, ICCT.Sent.), y = ICCT.Sent., alpha = ICCT)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() + 
  labs(title = "Average ICCT Sentiment in Country's Speeches", x = '', y = '', alpha = 'Avg. Mentions \n per Speech')

Country_ICSID_plot <- ggplot(Country_ICSID, aes(x = reorder(Group.1, ICSID.Sent.), y = ICSID.Sent., alpha = ICSID)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() + 
  labs(title = "Average ICSID Sentiment in Country's Speeches", x = '', y = '', alpha = 'Avg. Mentions \n per Speech')

Country_IFIs_plot <- ggplot(Country_IFIs, aes(x = reorder(Group.1, IFIs.Sent.), y = IFIs.Sent., alpha = IFIs)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() + 
  labs(title = "Average IFIs Sentiment in Country's Speeches", x = '', y = '', alpha = 'Avg. Mentions \n per Speech')

Country_IMF_plot <- ggplot(Country_IMF, aes(x = reorder(Group.1, IMF.Sent.), y = IMF.Sent., alpha = IMF)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() + 
  labs(title = "Average IMF Sentiment in Country's Speeches", x = '', y = '', alpha = 'Avg. Mentions \n per Speech')

Country_Investment.Arbitration_plot <- ggplot(Country_Investment.Arbitration, aes(x = reorder(Group.1, Investment.Arbitration.Sent.), y = Investment.Arbitration.Sent., alpha = Investment.Arbitration)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() + 
  labs(title = "Average Investment Arbitration Sentiment in Country's Speeches", x = '', y = '', alpha = 'Avg. Mentions \n per Speech')

Country_NIEO_plot <- ggplot(Country_NIEO, aes(x = reorder(Group.1, NIEO.Sent.), y = NIEO.Sent., alpha = NIEO)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() + 
  labs(title = "Average NIEO Sentiment in Country's Speeches", x = '', y = '', alpha = 'Avg. Mentions \n per Speech')

Country_UN.Human.Rights.System_plot <- ggplot(Country_UN.Human.Rights.System, aes(x = reorder(Group.1, UN.Human.Rights.System.Sent.), y = UN.Human.Rights.System.Sent., alpha = UN.Human.Rights.System)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() + 
  labs(title = "Average UN Human Rights System Sentiment in Country's Speeches", x = '', y = '', alpha = 'Avg. Mentions \n per Speech')

Country_UNCITRAL_plot <- ggplot(Country_UNCITRAL, aes(x = reorder(Group.1, UNCITRAL.Sent.), y = UNCITRAL.Sent., alpha = UNCITRAL)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() + 
  labs(title = "Average UNCITRAL Sentiment in Country's Speeches", x = '', y = '', alpha = 'Avg. Mentions \n per Speech')

Country_WTO.GATT_plot <- ggplot(Country_WTO.GATT, aes(x = reorder(Group.1, WTO.GATT.Sent.), y = WTO.GATT.Sent., alpha = WTO.GATT)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() + 
  labs(title = "Average WTO/GATT Sentiment in Country's Speeches", x = '', y = '', alpha = 'Avg. Mentions \n per Speech')

Country_World.Bank_plot <- ggplot(Country_World.Bank, aes(x = reorder(Group.1, World.Bank.Sent.), y = World.Bank.Sent., alpha = World.Bank)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() + 
  labs(title = "Average World Bank Sentiment in Country's Speeches", x = '', y = '', alpha = 'Avg. Mentions \n per Speech')

# create plot grids of the interesting visuals from above
plot_grid(Country_ICCT_plot, Country_ICSID_plot, Country_IFIs_plot, Country_IMF_plot, ncol = 2, nrow = 2)
plot_grid(Country_Investment.Arbitration_plot, Country_UN.Human.Rights.System_plot, Country_NIEO_plot, Country_UNCITRAL_plot, ncol = 2, nrow = 2)
plot_grid(Country_WTO.GATT_plot, Country_World.Bank_plot, ncol = 2, nrow = 1)

# these plots emphasize HOW OFTEN each country is discussing each topic, and also shows their sentiment towards it

Country_Avg_ICCT <- ggplot(Country_Avg, aes(x = ICCT.Sent., y = ICCT, label = Group.1)) +
  geom_point(color = 'black', fill = 'blue', size = 4, pch = 21, stroke = 1.25) + 
  geom_text_repel(aes(label=ifelse(ICCT>quantile(ICCT, .95),as.character(Group.1),'')),hjust=-0.2,vjust=-0.2, size = 3) + 
  labs(title = "Average ICCT Mentions and Sentiment in Countries' Speeches", x = 'Avg. Sentiment', y = 'Avg. Mentions')


Country_Avg_ICSID <- ggplot(Country_Avg, aes(x = ICSID.Sent., y = ICSID, label = Group.1)) +
  geom_point(color = 'black', fill = 'blue', size = 4, pch = 21, stroke = 1.25) + 
  geom_text_repel(aes(label=ifelse(ICSID>quantile(ICSID, .95),as.character(Group.1),'')),hjust=-0.2,vjust=-0.2, size = 3) +
  labs(title = "Average ICSID Mentions and Sentiment in Countries' Speeches", x = 'Avg. Sentiment', y = 'Avg. Mentions')


Country_Avg_IFIs <- ggplot(Country_Avg, aes(x = IFIs.Sent., y = IFIs, label = Group.1)) +
  geom_point(color = 'black', fill = 'blue', size = 4, pch = 21, stroke = 1.25) + 
  geom_text_repel(aes(label=ifelse(IFIs>quantile(IFIs, .95),as.character(Group.1),'')),hjust=-0.2,vjust=-0.2, size = 3) +
  labs(title = "Average IFIs Mentions and Sentiment in Countries' Speeches", x = 'Avg. Sentiment', y = 'Avg. Mentions')


Country_Avg_IMF <- ggplot(Country_Avg, aes(x = IMF.Sent., y = IMF, label = Group.1)) +
  geom_point(color = 'black', fill = 'blue', size = 4, pch = 21, stroke = 1.25) + 
  geom_text_repel(aes(label=ifelse(IMF>quantile(IMF, .95),as.character(Group.1),'')),hjust=-0.2,vjust=-0.2, size = 3) +
  labs(title = "Average IMF Mentions and Sentiment in Countries' Speeches", x = 'Avg. Sentiment', y = 'Avg. Mentions')


Country_Avg_Investment.Arbitration <- ggplot(Country_Avg, aes(x = Investment.Arbitration.Sent., y = Investment.Arbitration, label = Group.1)) +
  geom_point(color = 'black', fill = 'blue', size = 4, pch = 21, stroke = 1.25) + 
  geom_text_repel(aes(label=ifelse(Investment.Arbitration>quantile(Investment.Arbitration, .95),as.character(Group.1),'')),hjust=-0.2,vjust=-0.2, size = 3) +
  labs(title = "Average Investment Arbitration Mentions and Sentiment in Countries' Speeches", x = 'Avg. Sentiment', y = 'Avg. Mentions')


Country_Avg_NIEO <- ggplot(Country_Avg, aes(x = NIEO.Sent., y = NIEO, label = Group.1)) +
  geom_point(color = 'black', fill = 'blue', size = 4, pch = 21, stroke = 1.25) + 
  geom_text_repel(aes(label=ifelse(NIEO>quantile(NIEO, .95),as.character(Group.1),'')),hjust=-0.2,vjust=-0.2, size = 3) +
  labs(title = "Average NIEO Mentions and Sentiment in Countries' Speeches", x = 'Avg. Sentiment', y = 'Avg. Mentions')


Country_Avg_UN.Human.Rights.System <- ggplot(Country_Avg, aes(x = UN.Human.Rights.System.Sent., y = UN.Human.Rights.System, label = Group.1)) +
  geom_point(color = 'black', fill = 'blue', size = 4, pch = 21, stroke = 1.25) + 
  geom_text_repel(aes(label=ifelse(UN.Human.Rights.System>quantile(UN.Human.Rights.System, .95),as.character(Group.1),'')),hjust=-0.2,vjust=-0.2, size = 3) +
  labs(title = "Average UN Human Rights System Mentions and Sentiment in Countries' Speeches", x = 'Avg. Sentiment', y = 'Avg. Mentions')


Country_Avg_UNCITRAL <- ggplot(Country_Avg, aes(x = UNCITRAL.Sent., y = UNCITRAL, label = Group.1)) +
  geom_point(color = 'black', fill = 'blue', size = 4, pch = 21, stroke = 1.25) + 
  geom_text_repel(aes(label=ifelse(UNCITRAL>quantile(UNCITRAL, .95),as.character(Group.1),'')),hjust=-0.2,vjust=-0.2, size = 3) +
  labs(title = "Average UNCITRAL Mentions and Sentiment in Countries' Speeches", x = 'Avg. Sentiment', y = 'Avg. Mentions')


Country_Avg_WTO.GATT <- ggplot(Country_Avg, aes(x = WTO.GATT.Sent., y = WTO.GATT, label = Group.1)) +
  geom_point(color = 'black', fill = 'blue', size = 4, pch = 21, stroke = 1.25) + 
  geom_text_repel(aes(label=ifelse(WTO.GATT>quantile(WTO.GATT, .95),as.character(Group.1),'')),hjust=-0.2,vjust=-0.2, size = 3) +
  labs(title = "Average WTO/GATT Mentions and Sentiment in Countries' Speeches", x = 'Avg. Sentiment', y = 'Avg. Mentions')


Country_Avg_World.Bank <- ggplot(Country_Avg, aes(x = World.Bank.Sent., y = World.Bank, label = Group.1)) +
  geom_point(color = 'black', fill = 'blue', size = 4, pch = 21, stroke = 1.25) + 
  geom_text_repel(aes(label=ifelse(World.Bank>quantile(World.Bank, .95),as.character(Group.1),'')),hjust=-0.2,vjust=-0.2, size = 3) +
  labs(title = "Average World Bank Mentions and Sentiment in Countries' Speeches", x = 'Avg. Sentiment', y = 'Avg. Mentions')


# make plot grids for the scatter plots
plot_grid(Country_Avg_ICCT, Country_Avg_IFIs, Country_Avg_ICSID, ncol = 1, nrow = 3)
plot_grid(Country_Avg_IMF, Country_Avg_Investment.Arbitration, Country_Avg_NIEO, nrow = 3, ncol = 1)
plot_grid(Country_Avg_UN.Human.Rights.System, Country_Avg_UNCITRAL, nrow = 2, ncol = 1)
plot_grid(Country_Avg_WTO.GATT, Country_Avg_World.Bank, ncol = 1, nrow = 2)

# This part builds the correlogram for the sample of ten countries' sentiment surrounding international organizations
Country_Avg_Sent <- Country_Avg %>% select(Group.1, ICCT.Sent., ICSID.Sent., IFIs.Sent., IMF.Sent.,
                                           Investment.Arbitration.Sent., NIEO.Sent., UN.Human.Rights.System.Sent.,
                                           UNCITRAL.Sent., WTO.GATT.Sent., World.Bank.Sent.)
# set the names of the columns to the countries, after transposing the dataframe
Country_Avg_Sent <- setNames(data.frame(t(Country_Avg_Sent[,-1])), Country_Avg_Sent[,1])
# select some countries
Country_Avg_Sent_Samp <- Country_Avg_Sent %>% select('Australia', 'Denmark', 'France', 'Germany', 'India', 'Japan', 
                                                     'Republic of Korea', 'Russian Federation', 'United States of America',
                                                     'Zimbabwe')
# calculate the correlations
Country_Avg_Sent_Corrs_Samp <- cor(Country_Avg_Sent_Samp, use = 'pairwise.complete.obs')
# build the correlogram
cor_plot <- ggcorrplot(Country_Avg_Sent_Corrs_Samp, hc.order = TRUE, 
                       type = "lower",
                       outline.color = 'black',
                       lab = TRUE, 
                       lab_size = 3, 
                       method="circle", 
                       colors = c("tomato2", "white", "springgreen3"),
                       digits = 1,
                       title="Correlogram of Countries' Sentiment",
                       legend.title = 'Correlation',
                       ggtheme=theme_gray())
# check it out
cor_plot

# this part is for the specific results in section 4.2 of the paper
  # build a dataframe of all sentiment scores for the US and Japan
Japan_US <- UN %>%
            filter(Country %in% c('Japan', 'United States of America')) %>%
            select(Country, Year, ICCT.Sent., ICSID.Sent., IFIs.Sent., IMF.Sent., Investment.Arbitration.Sent.,
                   NIEO.Sent., UN.Human.Rights.System.Sent., UNCITRAL.Sent., WTO.GATT.Sent., World.Bank.Sent.)
  # calculate the average sentiment by year
avg_sent <- rowMeans(Japan_US[, 3:ncol(Japan_US)], na.rm = T)
Japan_US$avg_sent <- avg_sent
  # build the plot
final_plot <- ggplot(Japan_US, aes(x = Year, y = avg_sent, color = Country)) +
              scale_x_continuous(limits = c(1970, 2017), breaks = seq(1970, 2017, 3)) +
              geom_point(size = 4) +
              geom_line() + 
              theme_minimal() + 
              labs(title = 'Comparison of American and Japanese Sentiment', x = 'Year', y = 'Average Sentiment')
final_plot
