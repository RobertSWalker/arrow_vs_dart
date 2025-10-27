# arrow_vs_dart
Classifier for North American arrows versus darts using measurements (length + width) of ethnographic projectile points to classify archaeological projectile points.

Data files and R script are for this publication: Buchanan B, Hamilton MJ, Walker RS. A New Method for Classifying Dart and Arrow Projectile Points. American Antiquity 2025:1-15. There is an accompanying shiny app here [https://robert-walker.shinyapps.io/ggshiny/](https://robert-walker.shinyapps.io/arrows_darts/).

![fig](https://github.com/RobertSWalker/arrow_vs_dart/blob/main/fig.jpg)
Figure. The best variables for classifying darts versus arrows are length, width, and Southwest. Our model with smoothed splines generates nonlinear decision boundaries (curved lines). The dotted line is the decision boundary with a cutoff of 0.15 for points from the Southwest, and the solid line is the decision boundary with a cutoff of 0.15 for points from other regions. This graph uses the expanded dataset of 51 darts and 220 arrows.
