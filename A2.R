#i referred Prof's codes used in this course when completing this assignment.




install.packages("bayesm")
install.packages("mlogit")

library(dplyr)
library(bayesm)
library(tidyverse)
library(mfx)
library(mlogit)


set.seed(1234)

############Exercise 1#####################
#merge the two data sets
data(margarine)
choicePrice = margarine$choicePrice
demos = margarine$demos


############
#Question 1:Average and dispersion in product characteristics (price)
#calculate average price of each product
price= choicePrice%>%subset(select=-c(hhid,choice))
price%>%colMeans()


#calculate variance and standard deviation of price of each product
price%>%sapply(var)
price%>%sapply(sd)
############

############
#Question 2: Market share (choice frequency) and market share by product characteristics (choice frequency by price bins: below average, over average).
#get the data frame of actual choice made by individuals
choice_long=pivot_longer(choicePrice, c('PPk_Stk','PBB_Stk','PFl_Stk','PHse_Stk','PGen_Stk','PImp_Stk','PSS_Tub','PPk_Tub','PFl_Tub','PHse_Tub'), names_to="product_name", values_to="price")
choice_long=choice_long%>%mutate(product_number=case_when(
  product_name=="PPk_Stk"~1,
  product_name=="PBB_Stk"~2,
  product_name=="PFl_Stk"~3,
  product_name=="PHse_Stk"~4,
  product_name=="PGen_Stk"~5,
  product_name=="PImp_Stk"~6,
  product_name=="PSS_Tub"~7,
  product_name=="PPk_Tub"~8,
  product_name=="PFl_Tub"~9,
  product_name=="PHse_Tub"~10,
))
choice_long=choice_long%>%filter(choice==product_number)

#Market share (choice frequency)
table(choice_long$product_name)/nrow(choice_long)

#market share by product characteristics (choice frequency by price bins: below average, over average).
#i have tested whether there are some product prices equal to mean of prices. The result is none)
choice_long=choice_long%>%mutate(over_average=ifelse(price>mean(price),1,0))
#Market share (above average):
choice_long_over_average=choice_long%>%filter(over_average==1)
table(choice_long_over_average$product_name)/nrow(choice_long_over_average)
#Market share (below average):
choice_long_below_average=choice_long%>%filter(over_average==0)
table(choice_long_below_average$product_name)/nrow(choice_long_below_average)
#############


##############
#Question 3: Illustrate the mapping between observed attributes and choices. (Which customers are choosing which products?)

datw=left_join(choicePrice,demos,by='hhid')
head(datw,n=20)
#Mean of individual characteristics by choices.
summary=datw%>%group_by(choice)%>%summarise(Income_mean=mean(Income),mean_Fs3_4=mean(Fs3_4),mean_Fs5.=mean(Fs5.),mean_Fam_Size=mean(Fam_Size),mean_college=mean(college),mean_whtcollar=mean(whtcollar),mean_retired=mean(retired))
summary



############Exercise2####################

#recall that: price= choicePrice%>%subset(select=-c(hhid,choice)), the price inside the fucntion will be the same.
#use conditional logit
#use wide form
like_fun_c = function(param,datw)
{
  price = datw[,3:12]
  choice=datw$choice
  income=datw$Income
  fs3_4=datw$Fs3_4
  fs5=datw$Fs5.
  fam_size=datw$Fam_Size
  college=datw$college
  whtcollar=datw$whtcollar
  retired=datw$retired
  
  ni = nrow(datw)
  nj = 10
  ut = mat.or.vec(ni,nj)
  
  #parameter1-9 are used to store intercept. parameter 10 is the coefficient of price
  ut[,1]=param[10]*price[,1]
  for (j in 2:nj)
  {
    ut[,j] = param[j-1] + param[10]*price[,j]
  }
  
  prob   = exp(ut)            # exp(XB)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

param_c=runif(10,-10,10)
opt_c=optim(param_c,fn=like_fun_c,method="BFGS",control=list(trace=6,maxit=1000),datw=datw, hessian=TRUE)
opt_c
#the first 9 parameters is the intercept (setting the first choice as reference), the 10th parameter is the coefficient of price.





############Exercise3####################

# multinomial logit

like_fun_m = function(param,datw)
{
  price = datw[,3:12]
  choice=datw$choice
  income=datw$Income
  fs3_4=datw$Fs3_4
  fs5=datw$Fs5.
  fam_size=datw$Fam_Size
  college=datw$college
  whtcollar=datw$whtcollar
  retired=datw$retired
  
  ni = nrow(datw)
  nj = 10
  ut = mat.or.vec(ni,nj)
  #parameters in pn1 are intercepts(setting the first choice as reference)
  #parameters in pn2 are coefficients of income
  pn1    = param[1:(nj-1)]
  pn2    = param[(nj):(2*(nj-1))]
  pn3    = param[(2*(nj-1)+1):(3*(nj-1))]
  pn4    = param[(3*(nj-1)+1):(4*(nj-1))]
  pn5    = param[(4*(nj-1)+1):(5*(nj-1))]
  pn6    = param[(5*(nj-1)+1):(6*(nj-1))]

  ut[,1]= 0
  for (j in 2:nj)
  {
    ut[,j] = pn1[j-1]+income*pn2[j-1] + fam_size*pn3[j-1] + college*pn4[j-1] + whtcollar*pn5[j-1] + retired*pn6[j-1]
  }
  
  prob   = exp(ut)
  #some elements in ut may be so large that ext(ut) becomes Inf. Change Inf to the largest value which is not infinite in r (10^308)
  prob[is.infinite(prob)]=10^308
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001

  
  like = sum(log(probc))
  return(-like)
}

param_m=runif(54,-10,10)
like_fun_m(param_m,datw)
opt_m=optim(param_m,fn=like_fun_m,method="BFGS",control=list(trace=6,maxit=1000),datw=datw, hessian=TRUE)
opt_m

##############################









############Exercise4####################
#first model
opt_par_c=opt_c$par
marg_fun_c = function(param,datw)
{
  price = datw[,3:12]
  choice=datw$choice
  income=datw$Income
  fs3_4=datw$Fs3_4
  fs5=datw$Fs5.
  fam_size=datw$Fam_Size
  college=datw$college
  whtcollar=datw$whtcollar
  retired=datw$retired
  
  ni = nrow(datw)
  nj = 10
  ut = mat.or.vec(ni,nj)
  
  #parameter1-9 are used to store intercept. parameter 10 is the coefficient of price
  ut[,1]=param[10]*price[,1]
  for (j in 2:nj)
  {
    ut[,j] = param[j-1] + param[10]*price[,j]
  }
  
  prob   = exp(ut)            # exp(XB)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  marg=mat.or.vec(nj,nj)
  sum_marg=mat.or.vec(nj,nj)
  for (i in 1:ni){
    #loop i to get j*j matrix with dp/dx for different j and k
    #this matrix is the marginal effect for each observation i
    for (j in 1:nj){
      for(k in 1:nj){
        marg[k,j]=prob[i,j]*(ifelse(k==j,1,0)-prob[i,k])*param[10]
      }
    }
    #get mean(marginal effect)
    sum_marg=sum_marg+marg
    #make the marg matrix empty
    marg=mat.or.vec(nj,nj)
  }
  
  mean_marg=sum_marg/ni
  return(mean_marg)
}
marg_c=marg_fun_c(opt_par_c,datw)
marg_c




#######################second model
opt_par_m=opt_m$par
marg_fun_m = function(param,datw)
{
  price = datw[,3:12]
  choice=datw$choice
  income=datw$Income
  fs3_4=datw$Fs3_4
  fs5=datw$Fs5.
  fam_size=datw$Fam_Size
  college=datw$college
  whtcollar=datw$whtcollar
  retired=datw$retired
  
  ni = nrow(datw)
  nj = 10
  ut = mat.or.vec(ni,nj)
  
  #parameter1-9 are used to store intercept. parameter 10 is the coefficient of price
  pn1    = param[1:(nj-1)]
  pn2    = param[(nj):(2*(nj-1))]
  pn3    = param[(2*(nj-1)+1):(3*(nj-1))]
  pn4    = param[(3*(nj-1)+1):(4*(nj-1))]
  pn5    = param[(4*(nj-1)+1):(5*(nj-1))]
  pn6    = param[(5*(nj-1)+1):(6*(nj-1))]
  
  ut[,1]= 0
  for (j in 2:nj)
  {
    ut[,j] = pn1[j-1]+income*pn2[j-1] + fam_size*pn3[j-1] + college*pn4[j-1] + whtcollar*pn5[j-1] + retired*pn6[j-1]
  }
  
  prob   = exp(ut)
  #some elements in ut may be so large that ext(ut) becomes Inf. Change Inf to the largest value which is not infinite in r (10^308)
  prob[is.infinite(prob)]=10^308
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  
  marg=mat.or.vec(ni,nj)
  for (i in 1:ni){
  sum_beta=0
    for (j in 1:nj){
        prob_beta=prob[i,j]*param[(nj-1)+j]
        sum_beta=sum_beta+prob_beta
      }

    for (j in 1:nj){
      
        marg[i,j]=prob[i,j]*(param[(nj-1)+j]-sum_beta)
     
    }
  }
  mean_marg=colMeans(marg)

  return(mean_marg)
}
marg_m=marg_fun_m(opt_par_m,datw)
marg_m






############Exercise5####################
###############################mixed logit full data
like_fun_x = function(param,datw)
{
  price = datw[,3:12]
  choice=datw$choice
  income=datw$Income
  fs3_4=datw$Fs3_4
  fs5=datw$Fs5.
  fam_size=datw$Fam_Size
  college=datw$college
  whtcollar=datw$whtcollar
  retired=datw$retired
  
  ni = nrow(datw)
  nj = 10
  ut = mat.or.vec(ni,nj)
  
  pn1    = param[1:(nj-1)]
  pn2    = param[(nj):(2*(nj-1))]
  pn3    = param[(2*(nj-1)+1):(3*(nj-1))]
  pn4    = param[(3*(nj-1)+1):(4*(nj-1))]
  pn5    = param[(4*(nj-1)+1):(5*(nj-1))]
  pn6    = param[(5*(nj-1)+1):(6*(nj-1))]
  
  ut[,1]= param[(6*(nj-1))+1]*price[,1]
  for (j in 2:nj)
  {
    ut[,j] = pn1[j-1]+param[(6*(nj-1))+1]*price[,j]+income*pn2[j-1] + fam_size*pn3[j-1] + college*pn4[j-1] + whtcollar*pn5[j-1] + retired*pn6[j-1]
  }
  
  
 
  prob   = exp(ut)
  #some elements in ut are so large that ext(ut) becomes Inf. Change Inf to the largest value which is not infinite in r (10^308)
  prob[is.infinite(prob)]=10^308
  
  
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  
  
  
  like = sum(log(probc))
  return(-like)
}

param_x=runif(55,-10,10)
opt_x=optim(param_x,fn=like_fun_x,method="BFGS",control=list(trace=6,maxit=1000),datw=datw, hessian=TRUE)
opt_x
beta_f=opt_x$par
beta_f
######################



###############mixed logit using data without the last choice
like_fun_xr = function(param,datw)
{
  datw=datw%>%filter(choice!=10)
  price = datw[,3:11]
  choice=datw$choice
  income=datw$Income
  fs3_4=datw$Fs3_4
  fs5=datw$Fs5.
  fam_size=datw$Fam_Size
  college=datw$college
  whtcollar=datw$whtcollar
  retired=datw$retired
  
  ni = nrow(datw)
  nj = 9
  ut = mat.or.vec(ni,nj)
  pn1    = param[1:(nj-1)]
  pn2    = param[(nj):(2*(nj-1))]
  pn3    = param[(2*(nj-1)+1):(3*(nj-1))]
  pn4    = param[(3*(nj-1)+1):(4*(nj-1))]
  pn5    = param[(4*(nj-1)+1):(5*(nj-1))]
  pn6    = param[(5*(nj-1)+1):(6*(nj-1))]
  
  ut[,1]= param[(6*(nj-1))+1]*price[,1]
  for (j in 2:nj)
  {
    ut[,j] = pn1[j-1]+param[(6*(nj-1))+1]*price[,j]+income*pn2[j-1] + fam_size*pn3[j-1] + college*pn4[j-1] + whtcollar*pn5[j-1] + retired*pn6[j-1]
  }
  
  prob   = exp(ut)
  #some elements in ut are so large that ext(ut) becomes Inf. Change Inf to the largest value which is not infinite in r (10^308)
  prob[is.infinite(prob)]=10^308
  
  
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  
  
  
  like = sum(log(probc))
  return(-like)
}

param_xr=runif(49,-10,10)
opt_xr=optim(param_xr,fn=like_fun_xr,method="BFGS",control=list(trace=6,maxit=1000),datw=datw, hessian=TRUE)
opt_xr
beta_r=opt_xr$par
beta_r
########################




################### MTT
#modify beta_f to substitute it in to restricted function
beta_f_rest=beta_f[-c(9,18,27,36,45,54)]
mtt=-2*(-like_fun_xr(beta_f_rest,datw)+like_fun_xr(beta_r,datw))
mtt
qchisq(.95, df=length(beta_r))
qchisq(.99, df=length(beta_r))


#IIA violated at a confidence level 99%





































