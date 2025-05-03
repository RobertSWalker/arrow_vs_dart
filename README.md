# arrow_vs_dart
Classifier for North American arrows versus darts using measurements (length + width) of ethnographic projectile points to classify archaeological projectile points.

Data files and R script are for an upcoming publication with Briggs Buchanan and Marcus Hamilton. There is an accompanying shiny app here [https://robert-walker.shinyapps.io/ggshiny/](https://robert-walker.shinyapps.io/arrows_darts/).

![fig](https://github.com/RobertSWalker/arrow_vs_dart/blob/main/fig.jpg)
Figure. The best variables for classifying darts versus arrows are length, width, and Southwest. Our model with smoothed splines generates nonlinear decision boundaries (curved lines). The dotted line is the decision boundary with a cutoff of 0.15 for points from the Southwest, and the solid line is the decision boundary with a cutoff of 0.15 for points from other regions. This graph uses the expanded dataset of 38 darts and 219 arrows.
