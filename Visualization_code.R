# Load packages
library(readxl)
library(ggplot2)
library(dplyr)
library(data.table)
library(scales)
library(cowplot)


# Visualization
print(data_prepost)
#Pre
P1 <- data_prepost %>%
  filter(Treatment =="Pre") %>% 
  select(-Treatment) %>% 
  setDT() %>% 
  melt(id.vars = "Level") %>% 
  ggplot(mapping = aes(x=Level,y=variable,fill =value)) +
  geom_tile(color="black") + 
  geom_text(mapping = aes(label=percent(value)),col="black") +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  theme_bw() +
  labs(y=NULL,fill="Percent",title = "Pre-Assessment")

P1
#Post
P2 <- data_prepost %>%
  filter(Treatment =="Post") %>% 
  select(-Treatment) %>% 
  setDT() %>% 
  melt(id.vars = "Level") %>% 
  ggplot(mapping = aes(x=Level,y=variable,fill =value)) +
  geom_tile(color="black") + 
  geom_text(mapping = aes(label=percent(value)),col="black") +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  theme_bw() +
  labs(y=NULL,fill="Percent",title = "Post-Assessment")

P2

P2 <- data_prepost %>%
  filter(Treatment =="Post") %>% 
  select(-Treatment) %>% 
  setDT() %>% 
  melt(id.vars = "Level") %>% 
  ggplot(mapping = aes(x=Level,y=variable,fill =value)) +
  geom_tile(color="black") + 
  geom_text(mapping = aes(label=percent(value)),col="black") +
  scale_fill_gradient2(low = "#FF0000",
                       mid = "#F4FF39",
                       high = "#419428") +
  
  theme_bw() +
  labs(y=NULL,fill="Percent",title = "Post-Assessment")

P2

#Plot together
plot_grid(P1,P2,ncol = 2)
ggsave(filename = "Pre_Post.jpg",
       height = 8,
       width = 15)

# Getting the mean value of all criteria for each level
Pre_1 <- data_prepost %>%
  filter(Treatment =="Pre") %>% 
  select(-Treatment) %>% 
  setDT() %>% 
  melt(id.vars = "Level") %>% 
  group_by(Level) %>% 
  summarise(mean=mean(value)) %>% 
  mutate(Treatment ="Pre")


Post_1 <- data_prepost %>%
  filter(Treatment =="Post") %>% 
  select(-Treatment) %>% 
  setDT() %>% 
  melt(id.vars = "Level") %>% 
  group_by(Level) %>% 
  summarise(mean=mean(value)) %>% 
  mutate(Treatment ="Post")  

# Together
bind_1 <- rbind.data.frame(Pre_1,Post_1) %>% 
  as.data.frame()

# Side by Side
ggplot(bind_1,aes(x=Level,y=mean,fill =Treatment)) +
  geom_bar(stat = "identity",position = "dodge", alpha=0.7) +
  theme_bw() +
  scale_y_continuous(n.breaks = 10) +
  scale_fill_manual(values = c("red","blue")) +
  geom_text(mapping = aes(label=percent(mean)),col="black",position = position_dodge(0.9),
            vjust =-0.3)+
  labs(y="Gemiddeld percentage",title = "Gemiddeld percentage", )

ggsave(filename = "Gemiddeld.jpg",
       height = 8,
       width = 15)
# summaries above chart  
bind_1 %>% group_by(Level) %>%
  summarise(sum=sum(mean))

# Data 2 _ analysis for each student
ggplot(data_percent, aes(x=Name,y=Percentage, fill =Pre_Post)) + 
  geom_bar(stat = "identity",position = "dodge", alpha=0.7) +
  theme_bw() +
  scale_y_continuous(n.breaks = 10) +
  scale_fill_manual(values = c("red","blue")) +
  coord_flip()+
  geom_text(mapping = aes(label=percent(Percentage)),col="black",position = position_dodge(0.9),
            hjust =-0.1)+
  labs(y="percentage",title = "Analyseren van nulmeting en nameting data",x=NULL)


ggsave(filename = "EachStudent.jpg",
       height = 8,
       width = 15)
ggplot(data_percent, aes(x=Name,y=Percentage, color =Pre_Post)) + 
  geom_point(size=5) +
  theme_bw() +
  scale_colour_manual(values = c("red","blue")) +
  scale_y_continuous(n.breaks = 10)+
  labs(y="percentage",title = "Student Performance",x=NULL,colour="Treatment")

# Difference of student performance
data_percent_2 %>% 
  mutate(difference = Post - Pre,pn=ifelse(difference < 0,"Negatief","Positief")) %>%
  ggplot(mapping = aes(x=Name,y=difference,fill=factor(pn))) +
  scale_fill_manual(values = c("red","green")) +
  geom_bar(stat = "identity",alpha=0.7) +
  theme_bw() +
  scale_y_continuous(n.breaks = 10) +
  coord_flip() +
  geom_text(mapping = aes(label=percent(difference)),col="black",position = position_dodge(0.9),
            hjust =-0.1)+
  labs(y="verschil",title = "Leerlingen prestatie",x=NULL,fill="Prestatie")

ggsave(filename = "Positive_negative.jpg",
       height = 8,
       width = 15)
# Difference of student performance
data_percent %>% 
  mutate(Color=ifelse(Color=="Grey",NA,Color)) %>% 
  ggplot(mapping = aes(x=Name,y=Percentage,fill=Color)) +
  scale_fill_manual(values = c("green","red","yellow"),label=c("a","c","d")) +
  geom_bar(stat = "identity",alpha=0.7, position ="dodge", ) +
  theme_bw() +
  facet_grid(~Pre_Post)+
  scale_y_continuous(n.breaks = 10) +
  coord_flip() +
  labs(y="Percentage",title = "Student Performance",x=NULL,fill="Level")


