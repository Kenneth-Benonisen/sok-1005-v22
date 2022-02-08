# pakker
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(ggrepel)

# henter info fra nettsiden
nyt_data <- "https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json"

# omdanner til lesbar format.
import <- fromJSON(nyt_data)

# Oppgave 1 --------------------------------------------------------------------
graf <- ggplot(import, aes(x=fully_vaccinated_pct_of_pop, y= deaths_per_100k)) +
  geom_point(size=2, alpha=0.5, shape=21) +
  geom_text_repel(label=import$name, nudge_y=0.30, col=factor("dark green")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks=c(0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80), 
                     limits = c(0.45,0.80)) +
  theme_bw() +
  xlab("Andel av befolkningen som er vaksinert") +
  ylab("Månedlig antall døde per 100 000") +
  ggtitle("Covid-19 døde siden utviklingen av tilgjengelig vaksine sammenlignet med vaksinasjonsrate")


graf + 
  geom_segment(aes(x=0.60, y=12.5, xend=0.55, yend=15), arrow = arrow(length=unit(0.5,"cm"))) +
  geom_text(aes(label = "Lower vaccination rate, higher death rate", x=0.63, y=12.3)) +
  geom_segment(aes(x=0.75, y=7.5, xend=0.80, yend=5), arrow = arrow(length=unit(0.5,"cm"))) +
  geom_text(aes(label = "Higher vaccination rate, lower death rate", x=0.75, y=7.7))



# Oppgave 2 --------------------------------------------------------------------

lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = import)

# Intercept: Forteller oss gjennomsnittelig antall døde er hvis ingen er vaksinert. Månedlig, per 100 000. 

# Full faccinated pct of pop: Viser Effekten av vaksinen på intercept.

# Dette samsvarer med linjen som blir produsert av geom_smooth som viser oss at vaksinen fører til færre dødsfall. 

graf + 
  geom_segment(aes(x=0.60, y=12.5, xend=0.55, yend=15), arrow = arrow(length=unit(0.5,"cm"))) +
  geom_text(aes(label = "Lav vaksinasjonsrate, \n høyere dødsrate", x=0.63, y=12.3)) +
  geom_segment(aes(x=0.75, y=7.5, xend=0.80, yend=5), arrow = arrow(length=unit(0.5,"cm"))) +
  geom_text(aes(label = "Høyere vaksinasjonsrate, \n lavere dødsrate", x=0.75, y=8)) +
  geom_smooth(method = lm)

