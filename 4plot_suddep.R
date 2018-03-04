library(reshape2)
library(ggplot2)
library(scales)
library(dplyr)

d <- read.csv2("data/drug_court_decision_stats_2009-2016.csv", stringsAsFactors = F)

# Melt data
d.m <- melt(d,id.vars = "year", measure.vars = c("imprisonment",
                                                 "probation",
                                                 "fine",
                                                 "amnesty",
                                                 "other_outcomes")) ## just melt(dat) should work

# Calculate percentages

d.m <- d.m %>%
  group_by(year) %>%
  do(mutate(., perc = value / sum(value) * 100))

d.m$perc_label <- paste0(sprintf("%.0f", d.m$perc), "%")
d.m$perc_label[d.m$perc_label %in% c('0%',"1%", "2%")] <- ''

totals <- d.m %>%
  group_by(year) %>%
  summarize(total = sum(value))
totals$label <- paste(format(totals$total, big.mark = " "))


p <- ggplot() +
  geom_bar(data = d.m, aes(x = year, y = value,fill=variable), stat="identity") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.box='vertical',
        legend.margin=margin(t = 0,
                             r = 0,
                             b = 0,
                             l = 0,
                             unit='cm'),
        legend.key = element_rect(size = 0),
        legend.key.size = unit(1.5, 'lines'),
        axis.text.x = element_text(size=12))+
  scale_y_continuous(name='Количество осужденных', labels = NULL)+
  scale_x_continuous(breaks=c(2009:2016))+
  geom_text(data = d.m, aes(x = year, y = value,fill=variable, label = perc_label), 
            position = position_stack(vjust = 0.5))+
  geom_text(data = totals, aes(year, total + 3000, label = label, fill = NULL)) +
  labs(x="Год", fill="Основное наказание:") +
  scale_fill_manual(labels = c("Лишение свободы",
                               "Условное осуждение\nк лишению свободы",
                               "Штраф",
                               "Освобождение от наказания\n(включая амнистию)",
                               "Другие наказания\n(ограничение свободы, работы и т.д.)"),
                    values = c('grey80', 'grey70', 'grey60', 'grey50', 'grey40'))

p

#png(width = 10, height = 5, units = 'in', 
#    filename = "suddep_dynamics_2009-2016.png",
#    type = "cairo", antialias = "subpixel", family = "malgun", res = 400)
#p
#dev.off()

