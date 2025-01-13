# This is the server logic of a Shiny web application. You can run the application by clicking 'Run App' above.
library(shiny);library(brms);library(ggplot2);library(cowplot);library(readxl);theme_set(theme_cowplot());library(viridis)

df <- read_xlsx("Additional data.xlsx")
df$Southwest <- ifelse(df$Region == "Southwest", 1, 0)
df$type_factor <- factor(df$type)
df <- df[df$Continent == "North America",]
df$Southwest2 <- ifelse(df$Southwest == 1, "Southwest", "Other region")


dfpred <- read.csv("dfpred.csv")
m <- readRDS("m.Rds")

# Define server logic
server = function(input,output,session){
  observeEvent( input$Enter, {
    Length = input$Length
    Width = input$Width
    Southwest = input$Southwest
    t = data.frame(Length, Width, Southwest)
    t$Southwest <- as.numeric(t$Southwest)
    fits <- data.frame(fitted(m, type='response', t))
    t$type_est <- fits$Estimate
    cutoff <- as.numeric(input$Cutoff)
    t$type_est2 <- ifelse(t$type_est >= cutoff * 2, cutoff * 2 - .001, t$type_est) 
    t$odds1 <- t$type_est / (cutoff * 2 - t$type_est2)
    t$odds2 <- 1/t$odds1
    t$odds <- ifelse(t$odds1 > t$odds2, t$odds1, t$odds2)
    
    t$type <- ifelse(t$type_est > cutoff, "dart", 'arrow')
    preds <- data.frame(predict(m, type='response', t))
    t$Est.Error <- round(preds$Est.Error*100,1)
    
    type <- ifelse(t$type_est > cutoff, "dart", 'arrow')
    
    imputed_length <- round(5.03179+1.67141*input$Width2,2)
    #imputed_width <- round(8.310922+0.272973*input$Length2,2)
    
    output$plot_foo = renderPlot({
      ggplot(df, aes(x=Width, y=Length, color=type)) + 
        geom_tile(data = dfpred, mapping = aes(x = Width, y = Length, fill=fit), 
                  alpha = .7, size = 0, color = NA) + 
        geom_point(aes(color=type, shape = factor(Southwest2)), size=4, alpha = .6) +
        #geom_smooth(method=lm, se=F) + #scale_x_continuous(trans="log10") + scale_y_continuous(trans="log10") +
        geom_point(data = t, aes(color=type), pch = 1, size = 7, alpha =.6) +
        scale_color_discrete(type=c("darkblue","darkred"), name = "") +
        #scale_color_viridis(discrete = TRUE, option = "D")+
        scale_fill_viridis(discrete = F, breaks = c(0.01, .5, .97),
                           labels = c("arrow", "", "dart"), name="Prediction") +
        #guides(fill="", color=guide_legend(override.aes=list(fill=NA))) +
        #guides(fill = guide_legend(title=""), name ="") +
        xlab("Width (mm)") + ylab("Length (mm)") +
        #scale_fill_gradient(low="white", high="black", breaks = c(0.01, .5, .96),
        #                    labels = c("arrow", "", "dart"), name="Prediction") +
        #geom_abline(intercept = 300, slope = -10) +
        geom_segment(data = t, aes(x = 15, y = 95, xend = Width, yend = Length), 
                     arrow = arrow(length = unit(0.5, "cm")),
                     linetype = 2) +
        theme(legend.title=element_blank())
      
    })
     
    type <- ifelse(t$type_est > input$Cutoff, "dart", 'arrow')
    
   output$text <- renderText({
     
     ifelse(type == "dart",
          return(paste("<span style=\"color:darkred; font-size: 20px\">Congratulations, you have a DART!"))
    ,
          return(paste("<span style=\"color:darkblue; font-size: 20px\">Congratulations, you have an ARROW!"))
     )
      #return(paste("<span style=\"color:blue; font-size: 20px\">Congratulations, you have a </span>", type))
         
              })
   
   output$text2 <- renderText({
     
     #return(paste("<span style=\"color:red; font-size: 20px\">Imputed length is </span>", imputed_length, " mm"))
     return(paste0("Imputed length is ", imputed_length))
     
   })
   
   output$text3 <- renderText({
     
     #return(paste("<span style=\"color:red; font-size: 20px\">Imputed width is </span>", imputed_width, " mm"))
     return(paste0("Imputed width is ", imputed_width))
     
   })
   
   output$text4 <- renderText({
     
     return(paste0("(standard deviation of prediction is ", t$Est.Error, "% with odds of ", round(t$odds,2), " to 1)"))
     #return(h2(paste0("Standard deviation of estimate is </span>", t$Est.Error, "% with odds of ", round(t$odds,2), " to 1")))
     
   })
  })
}
