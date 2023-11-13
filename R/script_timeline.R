# ------------------------------------------------------
# Plot timeline
#
#     Plot para descrever a linha do tempo de um determinado tema.
#
# Referencias:
# https://weiyangtham.github.io/timeline_quick/
# https://pharmacoecon.me/post/2021-04-18-timeline-graph/
# https://stackoverflow.com/questions/20695311/chronological-timeline-with-points-in-time-and-format-date
# https://stackoverflow.com/questions/7492274/draw-a-chronological-timeline-with-ggplot2
#
# vinicio.lima@arauco.com
# ------------------------------------------------------


#### timeline linhas de Plantio

library(ggplot2)
library(dplyr)
library(ggalt)
library(cowplot)
library(tibble)
library(lubridate)

#Create data to plot
data <- tribble( ~start_date, ~event, ~displ,
                 ymd(20220317), paste0(
                   "° Conversa inicial: Demandas Áreas de Implantação MS"), 1.2,
                ymd(20220428), paste0(
                  "° Projeto linhas teste Planter D-61 (Copy parallel line)", "\n",
                  "  - Alinhamento komatsu + Hexagon + Cartografia"),  - 0.25,
                 ymd(20220512), paste0(
                   "° Aprovação projeto linhas de plantio Barra do Porto", "\n",
                   "° Contratação serviço técnico Iguaçu Máquinas (R$ 5.0/ha)"), 0.96,
                ymd(20220630), paste0(
                  "° Replicação de linhas via Operation Center JD", "\n",
                  "Pistas A-B (linhas retas)"), -0.15,
                ymd(20220712), paste0(
                  "° Cotação", "\n", "AgroCAD", "\n",
                  "R$ 32.000/ano"), 0.09,
                ymd(20220728), paste0(
                  "° Negociação driver para exportação", "\n",
                  "via compra de máquinas John Deere"), -0.08,
                ymd(20220923), paste0(
                  "° Teste driver GLC guidance (Pista A-B)", "\n",
                  "via compra de máquinas John Deere"), 0.81,
                ymd(20221012), paste0(
                  "° Teste operacional Projeto Bananal", "\n",
                  "° 1ª aproximação linhas em nível (AgroCAD)",  "\n",
                  "  - Silvicultura + Igauçu Máquinas + Cartografia", "\n",
                  "  - linha mestra: n= 1/talhão"), 0.57,
                ymd(20221026), paste0(
                  "° Reunião AgroCAD", "\n",
                  " TI + Cartografia"), 0.36,
                 ymd(20221206), paste0(
                   "° Treinamento AgroCAD Iguaçu Máquinas", "\n",
                   "° 1º Rodada avaliação de aderência", "\n",
                   "     Projeto  Bananal"), 0.07,
                ymd(20230131), paste0(
                  "° Análise erosão Bananal", "\n",
                  "° 1ª Reunião de sinergia", "\n",
                  "  - Padronização da metodologia", "\n",
                  "  - linha mestra: n >= 3/talhão"), -0.16,
                ymd(20230317), paste0(
                  "° Treinamento AgroCAD Tecgraf", "\n",
                  "° Dificuldades da equipe operacional", "\n",
                  "com o consumo das informações"), -0.04,
                ymd(20230327), paste0(
                  "° 2º Rodada avaliação de aderência", "\n",
                  "     Projeto  Jataí", "\n",
                  "  - Alinhamento com líderes", "\n",
                  "    Problemas com linhas 'morro abaixo'", "\n",
                  "    Limitações com o modelo de elevação", "\n",
                  "  - Diponibilização de pendrives", "\n",
                  "  - importação/gravação de arquivos"), 1.0,
                ymd(20230412), paste0(
                  "° 3º Rodada avaliação de aderência", "\n",
                  "     Projetos  Jataí/Guaicurus", "\n",
                  "  - Alinhamento com líderes", "\n",
                  "  - Teste suavização de linhas", "\n",
                  "  - Mapa de direção de fluxo superficial"), 0.75,
                ymd(20230703), paste0(
                  "° Revisão e avaliação dos modelos de elevação", "\n",
                  "° Definição área de teste talhões Utu-Guaçu ", "\n",
                  "° Orçamento MDT Engesat", "\n",
                  "   WorldDEM Neo (R$ 6/ha)"), 0.5,
                ymd(20230719), paste0(
                  "° 4º Rodada avaliação de aderência", "\n",
                  "     Projeto teste Utu-Guaçu", "\n",
                  "  - MDT Drone", "\n",
                  "  - MDT WorldDEM Neo"), -0.17,
                ymd(20230809), paste0(
                  "° Contratação", "\n",
                  "  serviço drone/vant", "\n",
                  "  - Escopo arquivos formato .ecw"), 0.4,
                ymd(20230817), paste0(
                  "° Canal de comunicação líderes subsolagem", "\n",
                  "  - Feedback operacional", "\n",
                  "  - Consumo de informações"), -0.05,
                ymd(20230830), paste0(
                  "° Visita técnica Benchmark Mantena Florestal", "\n",
                  "  - Linhas de plantio", "\n",
                  "  - Projetos de talhonamento", "\n",
                  "  - Erosão e caminhos florestais"), 0.24)

#Function to shift x-axis to 0 adapted from link shown above

shift_axis <- function(p, xmin, xmax, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))),
                        ymax=y, ymin=y) +
    annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax,
             arrow = arrow(length = unit(0.01, "inches"))) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x=element_blank())

}


#Conditionally set whether text will be above or below the point
vjust = ifelse(data$displ > 0, -0.45, 1.5)

#plot
p1 <- data %>%
  ggplot(aes(start_date, displ)) +
  geom_lollipop(point.size = 0.5) +
  geom_text(aes(x = start_date, y = displ, label = event), data = data,
            hjust = 0, vjust = vjust, size = 2.15) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 8)) +
  expand_limits(x = c(ymd(20220305), ymd(20240101)), y = .8) +
  scale_x_date(breaks = scales::pretty_breaks(n = 15))

#and run the function from above
timeline <- shift_axis(p1, ymd(20220305), ymd(20240105))

timeline

# ggsave("./Timiline_linhas_de_plantio.png",
#        width = 500, height = 390, units = 'mm')
