plasma=read.csv("Plasma.csv")
str(plasma)
library(dplyr)

#MCP1
plasma.summary=plasma %>%
  group_by(SUBJECT) %>% summarise(
    sd = sd(MCP1, na.rm = TRUE),
    MCP1 = mean(MCP1) )
bar=ggplot(plasma.summary, aes(x = fct_rev(SUBJECT), y = MCP1))
mcp1=bar + geom_col(aes(fill = SUBJECT),color="black") +
  scale_fill_manual(values = c("BLUE","RED")) + ylab("MCP1 (pg/mL)") + 
  geom_errorbar(aes(ymin = MCP1-sd, ymax = MCP1+sd), width = 0.2) + 
  annotate("text", x=1.5, y=800, label= "p=0.013") + xlab("SUBJECT")

#IP10
plasma.summary=plasma %>%
  group_by(SUBJECT) %>% summarise(
    sd = sd(IP10, na.rm = TRUE),
    IP10 = mean(IP10) )
bar=ggplot(plasma.summary, aes(x = fct_rev(SUBJECT), y = IP10))
ip10=bar + geom_col(aes(fill = SUBJECT),color="black") +
  scale_fill_manual(values = c("BLUE","RED")) + ylab("IP10 (pg/mL)") + 
  geom_errorbar(aes(ymin = IP10-sd, ymax = IP10+sd), width = 0.2) + 
  annotate("text", x=1.5, y=140, label= "p=0.014") + xlab("SUBJECT")

#GRObeta
plasma.summary=plasma %>%
  group_by(SUBJECT) %>% summarise(
    sd = sd(GRObeta, na.rm = TRUE),
    GRObeta = mean(GRObeta) )
bar=ggplot(plasma.summary, aes(x = fct_rev(SUBJECT), y = GRObeta))
grobeta=bar + geom_col(aes(fill = SUBJECT),color="black") +
  scale_fill_manual(values = c("BLUE","RED")) + ylab("GRObeta (pg/mL)") + 
  geom_errorbar(aes(ymin = GRObeta-sd, ymax = GRObeta+sd), width = 0.2) + 
  annotate("text", x=1.5, y=1550, label= "p=0.032") + xlab("SUBJECT")

#RANTES
plasma.summary=plasma %>%
  group_by(SUBJECT) %>% summarise(
    sd = sd(RANTES, na.rm = TRUE),
    RANTES = mean(RANTES) )
bar=ggplot(plasma.summary, aes(x = fct_rev(SUBJECT), y = RANTES))
rantes=bar + geom_col(aes(fill = SUBJECT),color="black") +
  scale_fill_manual(values = c("BLUE","RED")) + ylab("RANTES (pg/mL)") + 
  geom_errorbar(aes(ymin = RANTES-sd, ymax = RANTES+sd), width = 0.2) + 
  annotate("text", x=1.5, y=100000, label= "p=0.012") + xlab("SUBJECT")

#EGF
plasma.summary=plasma %>%
  group_by(SUBJECT) %>% summarise(
    sd = sd(EGF, na.rm = TRUE),
    EGF = mean(EGF) )
bar=ggplot(plasma.summary, aes(x = fct_rev(SUBJECT), y = EGF))
egf=bar + geom_col(aes(fill = SUBJECT),color="black") +
  scale_fill_manual(values = c("BLUE","RED")) + ylab("EGF (pg/mL)") + 
  geom_errorbar(aes(ymin = EGF-sd, ymax = EGF+sd), width = 0.2) + 
  annotate("text", x=1.5, y=200, label= "p=0.03") + xlab("SUBJECT")

pfigure = ggarrange(mcp1, ip10, grobeta, rantes, egf, 
                    labels = c("A", "B", "C", "D","E"),
                    ncol = 2, nrow = 3, 
                    common.legend = TRUE, legend = "bottom")
annotate_figure(pfigure,top = text_grob("Plasma Cytokines", color = "black",
                                        face = "bold", size = 14))

#-------------------------------------------------------------------------
ev=read.csv("EV.csv")
str(ev)

#IL17E
ev.summary=ev %>%
  group_by(SUBJECT) %>% summarise(
    sd = sd(IL17E, na.rm = TRUE),
    IL17E = mean(IL17E) )
ev.summary
bar=ggplot(ev.summary, aes(x = fct_rev(SUBJECT), y = IL17E))
il17e=bar + geom_col(aes(fill = SUBJECT),color="black") +
  scale_fill_manual(values = c("BLUE","RED")) + ylab("IL17E (pg/mL)") + 
  geom_errorbar(aes(ymin = IL17E-sd, ymax = IL17E+sd), width = 0.2) + 
  annotate("text", x=1.5, y=400, label= "p=0.002") + xlab("SUBJECT")

#IL12p70
ev.summary=ev %>%
  group_by(SUBJECT) %>% summarise(
    sd = sd(IL12p70, na.rm = TRUE),
    IL12p70 = mean(IL12p70) )
bar=ggplot(ev.summary, aes(x = fct_rev(SUBJECT), y = IL12p70))
il12p70=bar + geom_col(aes(fill = SUBJECT),color="black") +
  scale_fill_manual(values = c("BLUE","RED")) + ylab("IL12p70 (pg/mL)") + 
  geom_errorbar(aes(ymin = IL12p70-sd, ymax = IL12p70+sd), width = 0.2) + 
  annotate("text", x=1.5, y=25, label= "p=0.023") + xlab("SUBJECT")

#IL2
ev.summary=ev %>%
  group_by(SUBJECT) %>% summarise(
    sd = sd(IL2, na.rm = TRUE),
    IL2 = mean(IL2) )
bar=ggplot(ev.summary, aes(x = fct_rev(SUBJECT), y = IL2))
il2=bar + geom_col(aes(fill = SUBJECT),color="black") +
  scale_fill_manual(values = c("BLUE","RED")) + ylab("IL2 (pg/mL)") + 
  geom_errorbar(aes(ymin = IL2-sd, ymax = IL2+sd), width = 0.2) + 
  annotate("text", x=1.5, y=15, label= "p=0.016") + xlab("SUBJECT")

#GMCSF
ev.summary=ev %>%
  group_by(SUBJECT) %>% summarise(
    sd = sd(GMCSF, na.rm = TRUE),
    GMCSF = mean(GMCSF) )
bar=ggplot(ev.summary, aes(x = fct_rev(SUBJECT), y = GMCSF))
gmcsf=bar + geom_col(aes(fill = SUBJECT),color="black") +
  scale_fill_manual(values = c("BLUE","RED")) + ylab("GMCSF (pg/mL)") + 
  geom_errorbar(aes(ymin = GMCSF-sd, ymax = GMCSF+sd), width = 0.2) + 
  annotate("text", x=1.5, y=40, label= "p=0.031") + xlab("SUBJECT")

efigure = ggarrange(il17e, il12p70, il2, gmcsf, 
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
annotate_figure(efigure,top = text_grob("Extracellular Vesicle Cytokines", 
                                        color = "black",
                                        face = "bold", size = 14))