library(dplyr)
library(stats)
year <- 1997
form_num <- 2

pt <- 'C:/Users/14373/'
# pt <- 'C:/Users/12066/' old

filename <- paste0(pt,'Dropbox/2022 Winter/DataChallenge/data/Emplotment Equity data _ Metadata/',year,'/',year,'Form',form_num,'.csv')
df <- read.csv(filename)


#promotion ratio by year
re <- data.frame(array(dim=c(23,5)))

for (i in 1:23){
  year <- 1996+i
  form_num <- 2
  filename <- paste0(pt,'Dropbox/2022 Winter/DataChallenge/data/Emplotment Equity data _ Metadata/',year,'/',year,'Form',form_num,'.csv')
  
  count2019 <- read.csv(filename)
  count2019[is.na(count2019)] <- 0 
  count2019 <- count2019 %>%
    filter(OCCGROUP == "Overall")%>%
    summarise(sum(ALLCOUNT), sum(ALLWOMENCOUNT), sum(ABORIGALLCOUNT),sum(PWDALLCOUNT), sum(VISMINALLCOUNT))
  form_num <- 5
  filename <- paste0(pt,'Dropbox/2022 Winter/DataChallenge/data/Emplotment Equity data _ Metadata/',year,'/',year,'Form',form_num,'.csv')
  
  promotion2019 <- read.csv(filename)
  promotion2019[is.na(promotion2019)] <- 0 
  promotion2019 <- promotion2019 %>%
    filter(OCCGROUP == "Overall")%>%
    summarise(sum(TOTALPROMOTIONS),sum(TOTALWOMENPROMOTIONS), sum(TOTALABORIGALLPROMOTIONS),sum(TOTALPWDALLPROMOTIONS), sum(TOTALVISMINALLPROMOTIONS))
  re[i,] <- as.data.frame(promotion2019)/as.data.frame(count2019)
}

#minority ratio by year

re1 <- data.frame(array(dim=c(23,5)))
re1[,1] <- 1997:2019

for (i in 1:23){
  year <- 1996+i
  form_num <- 2
  filename <- paste0(pt,'Dropbox/2022 Winter/DataChallenge/data/Emplotment Equity data _ Metadata/',year,'/',year,'Form',form_num,'.csv')
  
  count2019 <- read.csv(filename)
  count2019[is.na(count2019)] <- 0 
  count <- count2019 %>%
    filter(OCCGROUP == "Overall")%>%
    summarise(sum(ALLCOUNT), sum(ALLWOMENCOUNT), sum(ABORIGMENCOUNT),sum(PWDMENCOUNT), sum(VISMINMENCOUNT))
  
  count <- as.data.frame(count)
  re1[i,2:5] <- count[2:5]/as.integer(count[1])
}
colnames(re1) <- c('year',colnames(count)[2:5])
write.csv(re1,file='re2.csv')



i <- 1
year <- 1996+i
form_num <- 4
filename <- paste0(pt,'Dropbox/2022 Winter/DataChallenge/data/Emplotment Equity data _ Metadata/',year,'/',year,'Form',form_num,'.csv')
df[is.na(df)] <- 0 


count_s <- df %>%
  group_by(EMPLOYERNAME) %>%
  filter(OCCGROUP == "Senior Managers"|OCCGROUP == "Middle and Other Managers")%>%
  summarise(sum(ALLCOUNT), sum(ALLWOMENCOUNT), sum(ABORIGALLCOUNT),sum(PWDALLCOUNT), sum(VISMINALLCOUNT)) %>%
  as.data.frame()

count_m <- df[df$EMPLOYERNAME%in% count_s$EMPLOYERNAME,] %>%
  group_by(EMPLOYERNAME) %>%
  filter(OCCGROUP != "Overall")%>%
  summarise(sum(ALLCOUNT), sum(ALLWOMENCOUNT), sum(ABORIGALLCOUNT),sum(PWDALLCOUNT), sum(VISMINALLCOUNT))%>%
  as.data.frame()


count_t <- df %>%
  filter(OCCGROUP != "Overall")%>%
  summarise(sum(ALLCOUNT), sum(ALLWOMENCOUNT), sum(ABORIGALLCOUNT),sum(PWDALLCOUNT), sum(VISMINALLCOUNT))%>%
  as.data.frame()


count_v <- df %>%
  group_by(EMPLOYERNAME) %>%
  filter(OCCGROUP != "Overall")%>%
  summarise(sum(ALLCOUNT), sum(ALLWOMENCOUNT), sum(ABORIGALLCOUNT),sum(PWDALLCOUNT), sum(VISMINALLCOUNT))%>%
  as.data.frame()

ratio <- count_t/as.numeric(count_t[1])

#std
st <- apply(count_v[,3:6]/count_v[,2],2,sd)
vec_s <- t((t(count_s[,3:6]/count_s[,2])-as.numeric(ratio[2:5]))/st)
norm_vec <- function(x) sqrt(sum(x^2))

norm_s <- apply(vec_s,1,norm_vec)

nvec_s <- vec_s/norm_s

vec_m <- t((t(count_m[,3:6]/count_m[,2])-as.numeric(ratio[2:5]))/st)

norm_m <- apply(vec_m,1,norm_vec)

nvec_m <- vec_m/norm_m

prod_re <- apply(nvec_m*nvec_s,1,sum)

plot(density(prod_re))

x <- data.frame(array(dim=c(400,1)))
x$company <- count_s$EMPLOYERNAME
x$corr <- prod_re
write.csv(x,file='corr_per_company.csv')
## beta estimate
beta_e <- function(beta){
  return(sum(apply(nvec_s-beta*nvec_m,1,norm_vec),na.rm = T))
}



B <- 1000
beta_y <- array(dim=c(23,3))
for (i in 1:23){
  year <- 1996+i
  form_num <- 4
  filename <- paste0(pt,'Dropbox/2022 Winter/DataChallenge/data/Emplotment Equity data _ Metadata/',year,'/',year,'Form',form_num,'.csv')
  df <- read.csv(filename)
  df[is.na(df)] <- 0 
  
  count_s <- df %>%
    group_by(EMPLOYERNAME) %>%
    filter(OCCGROUP == "Senior Managers"|OCCGROUP == "Middle and Other Managers")%>%
    summarise(sum(ALLCOUNT), sum(ALLWOMENCOUNT), sum(ABORIGALLCOUNT),sum(PWDALLCOUNT), sum(VISMINALLCOUNT)) %>%
    as.data.frame()
  
  count_m <- df[df$EMPLOYERNAME%in% count_s$EMPLOYERNAME,] %>%
    group_by(EMPLOYERNAME) %>%
    filter(OCCGROUP != "Overall")%>%
    summarise(sum(ALLCOUNT), sum(ALLWOMENCOUNT), sum(ABORIGALLCOUNT),sum(PWDALLCOUNT), sum(VISMINALLCOUNT))%>%
    as.data.frame()
  
  
  count_t <- df %>%
    filter(OCCGROUP != "Overall")%>%
    summarise(sum(ALLCOUNT), sum(ALLWOMENCOUNT), sum(ABORIGALLCOUNT),sum(PWDALLCOUNT), sum(VISMINALLCOUNT))%>%
    as.data.frame()
  
  
  count_v <- df %>%
    group_by(EMPLOYERNAME) %>%
    filter(OCCGROUP != "Overall")%>%
    summarise(sum(ALLCOUNT), sum(ALLWOMENCOUNT), sum(ABORIGALLCOUNT),sum(PWDALLCOUNT), sum(VISMINALLCOUNT))%>%
    as.data.frame()
  
  ratio <- count_t/as.numeric(count_t[1])
  
  #std
  f <- count_v[,3:6]/count_v[,2]
  f[is.na(f)] <- 0
  st <- apply(f,2,sd)
  ff <- count_s[,3:6]/count_s[,2]
  ff[is.na(ff)] <- 0
  vec_s <- t((t(ff)-as.numeric(ratio[2:5]))/st)
  norm_vec <- function(x) sqrt(sum(x^2))
  
  norm_s <- apply(vec_s,1,norm_vec)
  
  nvec_s <- vec_s/norm_s
  
  vec_m <- t((t(count_m[,3:6]/count_m[,2])-as.numeric(ratio[2:5]))/st)
  
  norm_m <- apply(vec_m,1,norm_vec)
  
  nvec_m <- vec_m/norm_m
  

  
  
  
  ## beta estimate
 
  
  
  beta_y[i,1] <- nlminb(start=0,objective=beta_e,lower=-0.99,upper=0.99)$par
  
  nvec_m0 <- nvec_m
  nvec_s0 <- nvec_s
  rd <- c()
  
  for(j in 1:B){
    set.seed(j)
    rl <- sample(c(1:dim(nvec_m0)[1]),dim(nvec_m0)[1],replace = T)
    nvec_m <- nvec_m0[rl,]
    nvec_s <- nvec_s0[rl,]
    rd[j] <- nlminb(start=0,objective=beta_e,lower=-0.99,upper=0.99)$par
  }
  beta_y[i,2:3] <- quantile(rd,prob=c(0.025,0.975))
}


write.csv(beta_y,file='beta.csv')








i <- 1
year <- 1996+i
form_num <- 4
filename <- paste0(pt,'Dropbox/2022 Winter/DataChallenge/data/Emplotment Equity data _ Metadata/',year,'/',year,'Form',form_num,'.csv')
df[is.na(df)] <- 0 


count_s <- df %>%
  group_by(EMPLOYERNAME) %>%
  filter(OCCGROUP == "Senior Managers"|OCCGROUP == "Middle and Other Managers")%>%
  summarise(sum(ALLCOUNT), sum(ALLWOMENCOUNT), sum(ABORIGALLCOUNT),sum(PWDALLCOUNT), sum(VISMINALLCOUNT)) %>%
  as.data.frame()









x <- c(1997:2019)
y <- beta_y[,1]
# plot of x and y :
plot(x,y,col=rgb(0.4,0.4,0.8,0.6),pch=16 , cex=1.2,ylim = c(0.2,1),
     cex.lab=1.5,xlab = 'Year',ylab = 'Correlation',
     main='Correlation of diversity between executive \n and non-executive levels across years') 

# Can we find a polynome that fit this function ?
model <- lm(y ~ x )

# I can get the features of this model :
#summary(model)
#model$coefficients
#summary(model)$adj.r.squared

# For each value of x, I can get the value of y estimated by the model, and add it to the current plot !
myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col=2, lwd=2 )  

# I add the features of the model to the plot
coeff <- round(model$coefficients , 2)
text(3, -70 , paste("Model : ",coeff[1] , " + " , coeff[2] , "*x"  , "+" , coeff[3] , "*x^2" , "+" , coeff[4] , "*x^3" , "\n\n" , "P-value adjusted = ",round(summary(model)$adj.r.squared,2)))


polygon(c(rev(x),x), c(rev(beta_y[,3]),beta_y[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)


legend("topright", legend = c('Estimated Correlation', 'Regression Line', 'Confidence Interval'), 
       bty = "n",
       col = c(rgb(0.4,0.4,0.8,0.6), "red",rgb(0.7,0.7,0.7,0.4)),
       lty = c(NA,1,NA),
       density=c(0,0,100),
       fill = c(rgb(0.4,0.4,0.8,0.6), "red",rgb(0.7,0.7,0.7,0.4)),
       pch=c(16,NA,NA),
       border = c(NA,NA,rgb(0.7,0.7,0.7,0.4)), 
       x.intersp=c(0.9,1,0.2)
)









x <- runif(300, min=-30, max=30) 
y <- -1.2*x^3 + 1.1 * x^2 - x + 10 + rnorm(length(x),0,100*abs(x)) 

# Basic plot of x and y :
plot(x,y,col=rgb(0.4,0.4,0.8,0.6), pch=16 , cex=1.3 , xlab="" , ylab="") 

# Can we find a polynome that fit this function ?
model <- lm(y ~ x + I(x^2) + I(x^3))

# I can get the features of this model :
#summary(model)
#model$coefficients
#summary(model)$adj.r.squared

#For each value of x, I can get the value of y estimated by the model, and the confidence interval around this value.
myPredict <- predict( model , interval="predict" )

#Finally, I can add it to the plot using the line and the polygon function with transparency.
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix , 1], col=2, lwd=2 )
polygon(c(rev(x[ix]), x[ix]), c(rev(myPredict[ ix,3]), myPredict[ ix,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)



