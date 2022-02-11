library(tidyverse)
library(rvest)
library(janitor)


# Oppgaven er laget av Kenneth Benonisen og samarbeidet med Tore Birkelund. 


# Henter nettsiden
html <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

# Insipiserer og omgjør til liste som inneholder tables. 
nettsiden <- html %>% 
  html_nodes(xpath ="//div") %>% 
  html_table(trim = TRUE)

# Sjekker om dette er tabbelene vi ser etter.
nettsiden[[35]]

# Oppretter en tabell for bilene
bilene <- nettsiden[[35]] %>% 
  slice(1:34) %>% 
  select(1:4)

bilene <- bilene %>% row_to_names(row_number = 1)

# filtrere bort bilene med X i stopp
bilene <- bilene %>% 
  slice(1:18, 20:25, 27:33)

# Rydder opp i tabellen til plotting. 
bilene <- bilene %>% 
  separate(`WLTP-tall`, sep = "/", into=c("wltp","kWh"))

bilene$STOPP <- gsub("km","", as.character(bilene$STOPP))

bilene$wltp <- gsub("km","", as.character(bilene$wltp))

# Omdanner tallene til numeric. 
bilene$wltp <- as.numeric(bilene$wltp)

bilene$STOPP <- as.numeric(bilene$STOPP)

# Oppgave 1 ------------------------------------------------------------

ggplot(bilene, aes(wltp, STOPP)) +
  geom_point() +
  scale_x_continuous(name="Markedsført rekkevidde", limits=c(200,600)) +
  scale_y_continuous(name="Reel rekkevidde", limits=c(200,600)) +
  geom_abline(col="red") +
  theme_bw() +
  ggtitle("Reel rekkevidde som funksjon av markedsført rekkevidde")
  


# Oppgave 2-----------------------------------------------------------

# Ser på regresjonsanalyse av datasettet.
regresjonsanalyse <- lm(STOPP ~ wltp, data = bilene)

summary(regresjonsanalyse)


# Intercept forteller oss den de har grunnlag for å selge bilene med den rekkevidden som er oppført. 

# Ettersom at intercepten er negativt med -26.6, kan vi si at den reele rekkevidden er mindre enn den markedsførte. 

# wltp har et estimat på 0.86712 som gjør at for hver kilometer bilen kjører får vi den reelle effekten i forhold til den markedsførte. 


ggplot(bilene, aes(wltp, STOPP)) +
  geom_point(aes(color="Reel")) +
  labs(color = "Rekkevidder") +
  ggtitle("Reel rekkevidde som funksjon av markedsført rekkevidde") +
  scale_x_continuous(name="Markedsført rekkevidde", limits=c(200,650)) +
  scale_y_continuous(name="Reel rekkevidde", limits=c(200,650)) +
  geom_abline(aes(intercept=0, slope=1, color="Markedsført"), show.legend = FALSE) +
  geom_smooth(method = lm, aes(color="lm_smooth")) +
  scale_color_manual(values = c("Reel"="black", "Markedsført" = "firebrick", "lm_smooth"="turquoise3"),
                     labels=c("Reel", "Markedsført", "lm_smooth")) +
  theme_bw()



