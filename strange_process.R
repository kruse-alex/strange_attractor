# packages
library(Rcpp)
library(tidyverse)
require(twitteR)
require(e1071)
require(png)

# do twitter auth
consumer_key = ""
consumer_secret = ""
access_token = ""
access_secret = ""

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

# cerate image
opt <-  theme(legend.position  = "none",
              panel.background = element_rect(fill="white", color="black"),
              plot.background  = element_rect(fill="white"),
              axis.ticks       = element_blank(),
              panel.grid       = element_blank(),
              axis.title       = element_blank(),
              axis.text        = element_blank())

cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a1, double a2, double a3, double a4, double a5, 
            double a6, double a7, double a8, double a9, double a10, 
            double a11, double a12, double a13, double a14) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = a1+a2*x[i-1]+ a3*y[i-1]+ a4*pow(fabs(x[i-1]), a5)+ a6*pow(fabs(y[i-1]), a7);
            y[i] = a8+a9*x[i-1]+ a10*y[i-1]+ a11*pow(fabs(x[i-1]), a12)+ a13*pow(fabs(y[i-1]), a14);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

    
for(i in 1:100000){
      res <- try({
        
        a1 <- round(sample(seq(-4,4, .1), 1), 1)
        a2 <- round(sample(seq(-4,4, .1), 1), 1)
        a3 <- round(sample(seq(-4,4, .1), 1), 1)
        a4 <- round(sample(seq(-4,4, .1), 1), 1)
        a5 <- round(sample(seq(-4,4, .1), 1), 1)
        a6 <- round(sample(seq(-4,4, .1), 1), 1)
        a7 <- round(sample(seq(-4,4, .1), 1), 1)
        a8 <- round(sample(seq(-4,4, .1), 1), 1)
        a9 <- round(sample(seq(-4,4, .1), 1), 1)
        a10 <- round(sample(seq(-4,4, .1), 1), 1)
        a11 <- round(sample(seq(-4,4, .1), 1), 1)
        a12 <- round(sample(seq(-4,4, .1), 1), 1)
        a13 <- round(sample(seq(-4,4, .1), 1), 1)
        a14 <- round(sample(seq(-4,4, .1), 1), 1)
        
        df <- createTrajectory(2000, 1, 1, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
        
        if(sum(is.infinite(df$x)) > 0){next}
        if(sum(is.infinite(df$y)) > 0){next}
        if(sum(is.nan(df$x)) > 0){next}
        if(sum(is.nan(df$y)) > 0){next}
        if(abs(cor.test(df$x,df$y)$estimate) > .05){next}
        print(kurtosis(df$x))
        print(kurtosis(df$y))
        
        if(is.nan(kurtosis(df$x)) == T){next}
        if(is.nan(kurtosis(df$y)) == T){next}
        
        if(kurtosis(df$y) < 40){next}
        if(kurtosis(df$x) < 40){next}

        mx <- quantile(df$x, probs = 0.25)
        Mx <- quantile(df$x, probs = 0.75)
        my <- quantile(df$y, probs = 0.25)
        My <- quantile(df$y, probs = 0.75)
        
        df %>% filter(x > mx, x < Mx, y > my, y < My) -> df
        if(nrow(df) == 0){next}
        
        })
      
      if(inherits(res, "try-error")){}
      break
}

df <- createTrajectory(20000000, 1, 1, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

mx <- quantile(df$x, probs = 0.25)
Mx <- quantile(df$x, probs = 0.75)
my <- quantile(df$y, probs = 0.25)
My <- quantile(df$y, probs = 0.75)

df %>% filter(x > mx, x < Mx, y > my, y < My) -> df

plot = ggplot(df) +
  geom_point(aes(x, y), shape=46, alpha=0.01, size=0, color="black") +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  #coord_fixed() + 
  opt

ggsave("strange.PNG", plot, height = 4, width = 4, units = 'in', dpi = 500)

big = readPNG("strange.PNG")

if(mean(big) < .95){

tweet(paste(format(Sys.time(), "%y_%m_%d_%H_%M_%S"),"strange_attractor",a1,a2,a3,a4,a5,a6,a7,a12,a13,a14,round(mean(big),2), sep = "_"), mediaPath = "strange.PNG")

}
