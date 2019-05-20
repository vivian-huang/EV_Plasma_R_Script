plasma=read.csv("Plasma.csv")
str(plasma)

#MCP1
dot=ggplot(plasma, aes(x = fct_rev(SUBJECT), y = MCP1))
#summary stats
dot + geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) + 
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("MCP1 (pg/mL)")
#boxplot
mcp1 = dot + geom_boxplot(width = 0.2) +
  geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("MCP1 (pg/mL)") + 
  annotate("text", x=1.5, y=1500, label= "p=0.013")
mcp1
#violinplot
dot + geom_violin(trim = FALSE) +
  geom_dotplot(binaxis='y', stackdir='center', aes(fill = SUBJECT)) +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("MCP1 (pg/mL)")

#IP10
dot=ggplot(plasma, aes(x = fct_rev(SUBJECT), y = IP10))
#summary stats
dot + geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) + 
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("IP10 (pg/mL)")
#boxplot
ip10 = dot + geom_boxplot(width = 0.2) +
  geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("IP10 (pg/mL)") + 
  annotate("text", x=1.5, y=200, label= "p=0.014") + xlab("SUBJECT")
#violinplot
dot + geom_violin(trim = FALSE) +
  geom_dotplot(binaxis='y', stackdir='center', aes(fill = SUBJECT)) +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("IP10 (pg/mL)")

#GRObeta
dot=ggplot(plasma, aes(x = fct_rev(SUBJECT), y = GRObeta))
#summary stats
dot + geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) + 
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("GRObeta (pg/mL)")
#boxplot
grobeta = dot + geom_boxplot(width = 0.2) +
  geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("GRObeta (pg/mL)") + 
  annotate("text", x=1.5, y=2300, label= "p=0.032") + xlab("SUBJECT")
#violinplot
dot + geom_violin(trim = FALSE) +
  geom_dotplot(binaxis='y', stackdir='center', aes(fill = SUBJECT)) +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("GRObeta (pg/mL)")

#RANTES
dot=ggplot(plasma, aes(x = fct_rev(SUBJECT), y = RANTES))
#summary stats
dot + geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) + 
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("RANTES (pg/mL)")
#boxplot
rantes = dot + geom_boxplot(width = 0.2) +
  geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("RANTES (pg/mL)") + 
  annotate("text", x=1.5, y=150000, label= "p=0.012") + xlab("SUBJECT")
#violinplot
dot + geom_violin(trim = FALSE) +
  geom_dotplot(binaxis='y', stackdir='center', aes(fill = SUBJECT)) +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("RANTES (pg/mL)")

#EGF
dot=ggplot(plasma, aes(x = fct_rev(SUBJECT), y = EGF))
#summary stats
dot + geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) + 
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("EGF (pg/mL)")
#boxplot
egf = dot + geom_boxplot(width = 0.2) +
  geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("EGF (pg/mL)") + 
  annotate("text", x=1.5, y=350, label= "p=0.03") + xlab("SUBJECT")
#violinplot
dot + geom_violin(trim = FALSE) +
  geom_dotplot(binaxis='y', stackdir='center', aes(fill = SUBJECT)) +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("EGF (pg/mL)")

pfigure = ggarrange(mcp1, ip10, grobeta, rantes, egf, 
                    labels = c("A", "B", "C", "D","E"),
                    ncol = 2, nrow = 3, 
                    common.legend = TRUE, legend = "bottom")
annotate_figure(pfigure,top = text_grob("Plasma Cytokines", color = "black",
                                        face = "bold", size = 14))

#-----------------------------------------------------------------------
ev=read.csv("EV.csv")
str(ev)

#IL17E
dot=ggplot(ev, aes(x = fct_rev(SUBJECT), y = IL17E))
#summary stats
dot + geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) + 
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("IL17E (pg/mL)")
#boxplot
il17e=dot + geom_boxplot(width = 0.2) +
  geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("IL17E (pg/mL)") + 
  annotate("text", x=1.5, y=450, label= "p=0.002") + xlab("SUBJECT")
#violinplot
dot + geom_violin(trim = FALSE) +
  geom_dotplot(binaxis='y', stackdir='center', aes(fill = SUBJECT)) +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("IL17E (pg/mL)")

#IL12p70
dot=ggplot(ev, aes(x = fct_rev(SUBJECT), y = IL12p70))
#summary stats
dot + geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) + 
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("IL12p70 (pg/mL)")
#boxplot
il12p70=dot + geom_boxplot(width = 0.2) +
  geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("IL12p70 (pg/mL)") + 
  annotate("text", x=1.5, y=40, label= "p=0.023") + xlab("SUBJECT")
#violinplot
dot + geom_violin(trim = FALSE) +
  geom_dotplot(binaxis='y', stackdir='center', aes(fill = SUBJECT)) +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("IL12p70 (pg/mL)")

#IL2
dot=ggplot(ev, aes(x = fct_rev(SUBJECT), y = IL2))
#summary stats
dot + geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) + 
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("IL2 (pg/mL)")
#boxplot
il2=dot + geom_boxplot(width = 0.2) +
  geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("IL2 (pg/mL)") + 
  annotate("text", x=1.5, y=25, label= "p=0.016") + xlab("SUBJECT")
#violinplot
dot + geom_violin(trim = FALSE) +
  geom_dotplot(binaxis='y', stackdir='center', aes(fill = SUBJECT)) +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("IL2 (pg/mL)")

#GMCSF
dot=ggplot(ev, aes(x = fct_rev(SUBJECT), y = GMCSF))
#summary stats
dot + geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) + 
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("GMCSF (pg/mL)")
#boxplot
gmcsf=dot + geom_boxplot(width = 0.2) +
  geom_dotplot(binaxis = "y", stackdir = "center", aes(fill = SUBJECT)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("GMCSF (pg/mL)") + 
  annotate("text", x=1.5, y=75, label= "p=0.031") + xlab("SUBJECT")
#violinplot
dot + geom_violin(trim = FALSE) +
  geom_dotplot(binaxis='y', stackdir='center', aes(fill = SUBJECT)) +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1)) +
  scale_fill_manual(values = c("BLUE", "RED")) + ylab("GMCSF (pg/mL)")

efigure = ggarrange(il17e, il12p70, il2, gmcsf, 
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
annotate_figure(efigure,top = text_grob("Extracellular Vesicle Cytokines", 
                                        color = "black",
                                        face = "bold", size = 14))
