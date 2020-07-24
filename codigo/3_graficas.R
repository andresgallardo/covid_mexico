# ## SCRIPT PARA HACER LAS GRAFICAS DUMBELL
## NOTAS: cargar e0_mexico del script: 2_tablas_vida
## autor: andrés gallardo

# library(ggplot2)
# library(ggalt)

#e0_mexico$nombres[e0_mexico$nombres == "Mexico"] <- "Edo. Mexico"
##MUJERES
svg("covid_mexico/f_escenarios.svg")
ggplot(e0_mexico, aes(y = reorder(nombres,e0_f_proyecciones),
                      x = e0_f_covid,
                      xend = e0_f_proyecciones)) +
  geom_dumbbell(aes(y = reorder(nombres,e0_f_proyecciones),
                    x = e0_f_covid,
                    xend = e0_f_ihme),
                colour="grey90",
                colour_x = "grey30",
                colour_xend = "grey50",
                size_x=1.4,
                size_xend=1.5)+
  geom_dumbbell(aes(y = reorder(nombres,e0_f_proyecciones),
                    x = e0_f_ihme,
                    xend = e0_f_ihme_sup),
                colour="grey90",
                colour_x = "grey50",
                colour_xend = "grey70",
                size_x=1.5,
                size_xend=1.6)+
  geom_dumbbell(aes(y = reorder(nombres,e0_f_proyecciones),
                    x = e0_f_covid,
                    xend = e0_f_proyecciones),
                colour="grey90",
                colour_x = "grey30",
                colour_xend = "black",
                size_x=1.4,
                size_xend=1.3)+
  scale_x_continuous(breaks=c(74,75,76,77,78,79))+
  labs(title = "",
       x = "Expectativa de Vida (años) \n (Diferencia en años respecto a CONAPO)",
       y = "")+
  theme_bw() +
  theme(legend.position="right",
        axis.text.y = element_text(size = 9),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.border=element_blank(),
        axis.line = element_line(size = .4, colour = "black"),
        axis.title.x = element_text(size = 9)) +
  scale_y_discrete(expand = expansion(mult = c(0.03, 0.03))) +
  geom_text(color="black", size=2, vjust=1.7, hjust=0,
            aes(x=e0_f_proyecciones, label=round(e0_f_proyecciones,2)))+
  geom_text(color="black", size=2, vjust=1.7, hjust=.1,
           aes(x=e0_f_covid, label=dif_f_co))+
  geom_text(aes(x=e0_f_ihme, label=dif_f_ih),
            color="black", size=2, vjust=1.7) +
  geom_text(aes(x=e0_f_ihme_sup, label=dif_f_ihs),
              color="black", size=2, vjust=1.7) +
  geom_text(aes(y="Ciudad de Mexico",x=79.42, label="CONAPO"),
            color="black", size=2.1, vjust=-1.05 , fontface="bold")+
  geom_text(aes(y="Ciudad de Mexico",x=78.58, label="Bajo"),
            color="grey30", size=2.1, vjust=-1.05, fontface="bold")+
  geom_text(aes(y="Ciudad de Mexico",x=78.02, label="Medio"),
          color="grey50", size=2.1, vjust=-1.05, fontface="bold")+
  geom_text(aes(y="Ciudad de Mexico",x=77.48, label="Alto"),
            color="grey70", size=2.1, vjust=-1.05, fontface="bold")
dev.off()

##HOMBRES
svg("covid_mexico/m_escenarios.svg")
ggplot(e0_mexico, aes(y = reorder(nombres,e0_m_proyecciones),
                      x = e0_m_covid,
                      xend = e0_m_proyecciones)) +
  geom_dumbbell(aes(y = reorder(nombres,e0_m_proyecciones),
                    x = e0_m_covid,
                    xend = e0_m_ihme),
                colour="grey90",
                colour_x = "grey30",
                colour_xend = "grey50",
                size_x=1.4,
                size_xend=1.5)+
  geom_dumbbell(aes(y = reorder(nombres,e0_m_proyecciones),
                    x = e0_m_ihme,
                    xend = e0_m_ihme_sup),
                colour="grey90",
                colour_x = "grey50",
                colour_xend = "grey70",
                size_x=1.5,
                size_xend=1.6)+
  geom_dumbbell(aes(y = reorder(nombres,e0_m_proyecciones),
                    x = e0_m_covid,
                    xend = e0_m_proyecciones),
                colour="grey90",
                colour_x = "grey30",
                colour_xend = "black",
                size_x=1.4,
                size_xend=1.3)+
  scale_x_continuous(breaks=c(66,67,68,69,70,71,72,73))+
  labs(title = "",
       x = "Expectativa de Vida (años) \n (Diferencia en años respecto a CONAPO)",
       y = "")+
  theme_bw() +
  theme(legend.position="right",
        axis.text.y = element_text(size = 9),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.border=element_blank(),
        panel.grid.major.y=element_blank(),
        axis.line = element_line(size = .4, colour = "black"),
        axis.title.x = element_text(size = 9)) +
  scale_y_discrete(expand = expansion(mult = c(0.03, 0.03))) +
  geom_text(color="black", size=2, vjust=1.7, hjust=0,
            aes(x=e0_m_proyecciones, label=round(e0_m_proyecciones,2)))+
  geom_text(color="black", size=2, vjust=1.7, hjust=0,
            aes(x=e0_m_covid, label=dif_m_co))+
  geom_text(aes(x=e0_m_ihme, label=dif_m_ih),
            color="black", size=2, vjust=1.7) +
  geom_text(aes(x=e0_m_ihme_sup, label=dif_m_ihs),
            color="black", size=2, vjust=1.7) +
  geom_text(aes(y="Ciudad de Mexico",x=73.50, label="CONAPO"),
            color="black", size=2.1, vjust=-1.05 , fontface="bold")+
  geom_text(aes(y="Ciudad de Mexico",x=71.79, label="Bajo"),
            color="grey30", size=2.1, vjust=-1.05, fontface="bold")+
  geom_text(aes(y="Ciudad de Mexico",x=70.68, label="Medio"),
            color="grey50", size=2.1, vjust=-1.05, fontface="bold")+
  geom_text(aes(y="Ciudad de Mexico",x=69.64, label="Alto"),
            color="grey70", size=2.1, vjust=-1.05, fontface="bold")
dev.off()
