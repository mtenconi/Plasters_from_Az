# Analysis:
# Linear regression:
data <- subset(new_data, group == "B" | group == "C")

LR <- lm(S ~ Ca, data)
print(summary(LR))

p <- ggplot(data, aes(x = S, y = Ca)) +
  geom_point(aes(color = group, fill = group))+
  geom_smooth(method = "lm", se = FALSE, color = "white")+
  ggtitle("Linear Regression")+ 
  annotate("text", x=21, y= 2, label = "R^2=0.93", color = "white")
p + theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background=element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend 
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect( fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    title = element_text(colour = "white"),
    axis.text = element_text(color = "white"), axis.line = element_line(color = "white"),
    axis.title = element_text(color = "white")
)

p2 <- ggplot(data, aes(x = S, y = Ca)) +
  geom_point(aes(color = group, fill = group))+
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  ggtitle("Linear Regression")+ 
  annotate("text", x=21, y= 2, label = "R^2=0.93")
p2 + theme(
      panel.background = element_rect(fill = "white",
                                      colour = "white"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.line = element_line(size = 0.2, 
                               linetype = 'solid',
                               color = "black"),
      legend.key = element_rect(colour = NA, fill = NA)
)  
  
  