library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(Rtsne)
library(ggrepel)
combined <- read_csv("combined_data.csv")
selectset <- combined |>
  select(ANZSCO_minor, Percentage_Covid,
         `Contact With Others`, `Outdoors, Under Cover`,
         `Face-to-Face Discussions`) |>
  mutate(Composite = `Contact With Others` * (5-`Outdoors, Under Cover`) *`Face-to-Face Discussions`,
    Occupation = case_when(ANZSCO_minor == "Health Therapy Professionals" ~"Health",
                                ANZSCO_minor == "Medical Practitioners" ~"Health",
                                ANZSCO_minor == "Midwifery and Nursing Professionals" ~"Health",
                                TRUE ~ "Other"))

ggplot(selectset, aes(x=Composite, y=Percentage_Covid,
                      colour=Occupation)) +
  geom_point() + geom_smooth(method="lm")
data_matrix <- as.matrix(combined[,3:ncol(combined)])
tsne <- Rtsne(data_matrix, dims = 2, perplexity=30, verbose=FALSE, theta=0.0, max_iter = 500, check_duplicates=FALSE)
tsne2D <- as.data.frame(tsne$Y)
tsne2D$occupation = combined$ANZSCO_minor
tsne2D$covid = combined$Percentage_Covid
teach_prison <- tsne2D %>% 
  filter(occupation %in% c("School Teachers", "Prison and Security Officers"))
ggplot(tsne2D, aes(x=V1,y=V2,colour=covid)) +
  geom_point() +
  scale_colour_viridis_c() +
  ggtitle("tsne diemsion reduction") +
  theme_minimal() +
  geom_text_repel(data=teach_prison, 
                  aes(label = occupation))
tsne2D |> filter(V2>0, V2<3, V1< -1,V1>-4) 

cartesian_close <- function(x, space=tsne2D){
  space$fromV1 <- space$V1[x]
  space$fromV2 <- space$V2[x]
  space$fromoccupation <- space$occupation[x]
  space$fromcovid <- space$covid[x]
  space <- space[-x,]
  space$distance <- ((space$V1 - space$fromV1)^2 + (space$V2 - space$fromV2)^2)^0.5
  neighbour <- space[which.min(space$distance),]
  return(neighbour)
}

nearby <- bind_rows(lapply(1:nrow(tsne2D), cartesian_close))
nearby$diffcov <- abs(nearby$covid - nearby$fromcovid)

sixcols= colorblind_pal()(6)
ggplot(combined,aes(y=Percentage_Covid)) +
  geom_smooth(aes(x=`Wear Common Protective or Safety Equipment such as Safety Shoes, Glasses, Gloves, Hearing Protection, Hard Hats, or Life Jackets_Every day`), 
              method="lm", se = FALSE, colour=sixcols[1], linetype=2) +
  geom_smooth(aes(x=`Work With Work Group or Team_Not important at all`), 
              method="lm", se = FALSE, colour=sixcols[2], linetype=2) +
  geom_smooth(aes(x=`Contact With Others_Constant contact with others`), 
              method="lm", se = FALSE, colour=sixcols[3]) +
  geom_smooth(aes(x=`Outdoors, Under Cover_Every day`), 
              method="lm", se = FALSE, colour=sixcols[4], linetype=2) +
  labs(title="Percent of workforce:
(black) Wearing common safety gear every day
(orange) Working with group or team not important
(blue) In close contact with others constantly
(green) Outdoors under cover every day
vs. percentage who caught covid by mid-August 2022",
x="\nPercentage of workforce w. job activity",
y="\nPercentage of workforce who caught covid\n") +
  theme_minimal()

ggdata <- combined %>%
  select(1:2,
         `Wear Common Protective or Safety Equipment such as Safety Shoes, Glasses, Gloves, Hearing Protection, Hard Hats, or Life Jackets_Every day`,
         `Work With Work Group or Team_Not important at all`,
         `Contact With Others_Constant contact with others`,
         `Outdoors, Under Cover_Every day`) %>%
  rename(`Some common protective gear worn daily\n(eyewear, gloves, masks, etc)` = 3,
         `Working with groups not important`= 4,
         `Constant contact with others` = 5,
         `Outdoors under cover every day` = 6) |> 
  gather(key="Activity", value="ActivityPercent", 3:6) |> 
  mutate(Activity = factor(Activity, levels=c("Constant contact with others",
                                              "Outdoors under cover every day",
                                              "Some common protective gear worn daily\n(eyewear, gloves, masks, etc)",
                                              "Working with groups not important"))) 
graf <- ggplot(ggdata, aes(x=ActivityPercent, y=Percentage_Covid, colour=Activity)) +
  facet_wrap(~Activity, ncol=2) +
  geom_point(alpha=.1, size=0.2) + 
  geom_smooth(method="lm", se=FALSE, linewidth=0.5) +
  labs(title="Percentage of workers engaging in particular activities
(O*NET, pre-covid) vs. Percentage of NZ workers catching covid
by mid-August 2022 (NZ govt OIA)\n",
x="\nPercentage of workers doing a thing\n",
y="\nPercentage of workers\ncatching covid\n") +
  scale_colour_colorblind() +
  scale_y_continuous(breaks=c(0,20,40), labels = c("none","some", "twice\nsome")) +
  coord_cartesian(y=c(0,NA))
  

theme_wkcv23 <- function(){
  # theme_minimal(base_family="OpenSans") %+replace% 
  theme_minimal(base_size = 6) %+replace% 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line.x = element_line(size=0.1),
          axis.text.x = element_text(size=6),
          axis.ticks.x = element_line(size=0.2),
          axis.line.y = element_line(size=0.1),
          axis.ticks.y = element_line(size=0.2),
          axis.text.y = element_text(size=6),
          strip.background = element_rect(fill= "#FFFFFF", colour="#EFEFEF"),
          strip.text = element_text(size = 6,
                                    margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")),
          strip.placement = "inside",
          panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
          panel.spacing = unit(0.7, "lines"),
          plot.title = element_text(size = 8,
                                    lineheight = 1.23,
                                    margin=margin(t = 0, r = 0, b = 10, l = 10, unit = "pt"),
                                    hjust=0),
          plot.subtitle = element_text(size = 7, hjust=0),
          plot.background = element_rect(fill = "#F5F5F5"),
          axis.title = element_text(size=7),
          plot.caption = element_text(margin=margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
                                      size=5, hjust=1),
          plot.caption.position = "plot",
          plot.margin = margin(12, 18, 12, 12, "pt"),
          legend.position = "none")
}

graf + theme_wkcv23()
ggsave("~/Desktop/change.png", width=1920, height=1080, units = "px")
