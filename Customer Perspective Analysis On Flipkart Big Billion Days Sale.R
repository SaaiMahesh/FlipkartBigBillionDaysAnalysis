data<- read.csv('data.csv')

View(data)

#Q1. Does age affect the customers being aware of 'The Big Billion Days' sale?
#H0: Age does not affect customers being aware of 'The Big Billion Days' sale.
#H1: Age influences customers being aware of 'The Big Billion Days' sale.
#Performing chi-square test since both the variables are categorical.
chisq.test(data$Have.you.heard.about..The.Big.Billion.Days..,data$Your.age.group.range)
#Since p < 0.05, we accept alternative hypothesis i.e age affects customers being aware of 'The Big Billion Days' sale.

#Q2. Does age influence customers shopping during 'The Big Billion Days' sale?
#H0: Age does not influence customers shopping during 'The Big Billion Days' sale.
#H1: Age influences customers shopping during 'The Big Billion Days' sale.
#Performing chi-square test since both the variables are categorical.
chisq.test(data$Have.you.shopped.during..The.Big.Billion.Days..,data$Your.age.group.range)
#Since p > 0.05, we accept null hypothesis i.e age does not influence customers shopping during 'The Big Billion Days' sale.

#Q3. Does the customer's type of city that they live in affect their awareness of 'The Big Billion Days' sale?
#H0: Customer's type of city does not affect their awareness of 'The Big Billion Days' sale.
#H1: Customer's type of city affects their awareness of 'The Big Billion Days' sale.
chisq.test(data$Have.you.heard.about..The.Big.Billion.Days..,data$The.city.you.stay)
#Since p > 0.05, we accept null hypothesis i.e customer's type of city does not affect their awareness of 'The Big Billion Days' sale.

#Q4. Does the customer's type of city that they live in affect their shopping during 'The Big Billion Days' sale?
#H0: Customer's type of city does not affect their shopping during 'The Big Billion Days' sale.
#H1: Customer's type of city does affect their shopping during 'The Big Billion Days' sale.
chisq.test(data$Have.you.shopped.during..The.Big.Billion.Days..,data$The.city.you.stay)
#Since p > 0.05, we accept null hypothesis i.e customer's type of city does not affect their shopping during 'The Big Billion Days' sale.

#Q5. Does the customer's profession affect their awareness of 'The Big Billion Days' sale?
#H0: Customer's profession does not affect their awareness of 'The Big Billion Days' sale.
#H1: Customer's profession affects their awareness of 'The Big Billion Days' sale.
chisq.test(data$Profession.that.you.belong.to,data$Have.you.heard.about..The.Big.Billion.Days..)
#Since p < 0.05, we accept alternative hypothesis i.e customer's profession affects their awareness of 'The Big Billion Days' sale.

#Q6. Does the customer's profession affect their shopping during 'The Big Billion Days' sale?
#H0: Customer's profession does not affect their shopping during 'The Big Billion Days' sale.
#H1: Customer's profession affects their shopping during 'The Big Billion Days' sale.
chisq.test(data$Profession.that.you.belong.to,data$Have.you.shopped.during..The.Big.Billion.Days..)
#Since p > 0.05, we accept null hypothesis i.e customer's profession does not affect their shopping during 'The Big Billion Days' sale.

#Q7. Is there any association between the customers knowing about 'The Big Billion Days' sale and the customer's frequency of shopping?
#H0: There is no association between the customers knowing about 'The Big Billion Days' sale and the customer's frequency of shopping.
#H1: There is an association between the customers knowing about 'The Big Billion Days' sale and the customer's frequency of shopping.
chisq.test(data$Have.you.heard.about..The.Big.Billion.Days..,data$How.often.do.you.shop.from.Flipkart.)
#Since p > 0.05, we accept null hypothesis i.e there is no association between the customers knowing about 'The Big Billion Days' sale and the customer's frequency of shopping.

#Q8. Does customer satisfaction during the sale influence in making customers join the Flipkart Plus membership?
#H0: Customer satisfaction during the sale does not influence in making customers join the Flipkart Plus membership.
#H1: Customer satisfaction during the sale influences in making customers join the Flipkart Plus membership.
chisq.test(data$Are.you.satisfied.with.the.products.on.sale.during..The.Big.Billion.Days.,data$Would.you.consider.joining.Flipkart.Plus.Membership.to.avail.additional.benefits.from..The.Big.Billion.Days..)
#Since p < 0.05, we accept alternative hypothesis i.e customer satisfaction from the products on sale during 'The Big Billion Days' influences in making customers join the Flipkart Plus membership.

#Q9. Does frequency of shopping on Flipkart influence customers from joining Flipkart Plus Membership?
#H0: Frequency of shopping on Flipkart does not influence customers from joining Flipkart Plus Membership?
#H1: Frequency of shopping on Flipkart influences customers from joining Flipkart Plus Membership?
chisq.test(data$How.often.do.you.shop.from.Flipkart.,data$Would.you.consider.joining.Flipkart.Plus.Membership.to.avail.additional.benefits.from..The.Big.Billion.Days..)
#Since p > 0.05, we accept null hypothesis i.e frequency of shopping on Flipkart does not influence customers from joining Flipkart Plus Membership.

#Q10. Does customer profession influence in joining Flipkart Plus Membership?
#H0: Profession does not influence in joining Flipkart Plus Membership.
#H1: Profession influences in joining Flipkart Plus Membership.
chisq.test(data$Profession.that.you.belong.to,data$Would.you.consider.joining.Flipkart.Plus.Membership.to.avail.additional.benefits.from..The.Big.Billion.Days..)
#Since p > 0.05, we accept null hypothesis i.e profession does not influence customers joining in Flipkart Plus Membership.

#Q11. Does customer's satisfaction to shop on Flipkart compared to other e-commerce companies influence whether the customer has shopped during 'Big Billion Days'?
#H0: Customer's satisfaction to shop on Flipkart does not influence whether the customer has shopped during 'Big Billion Days'.
#H1: Customer's satisfaction to shop on Flipkart influences whether the customer has shopped during 'Big Billion Days'.
chisq.test(data$How.satisfied.would.you.be.when.considering.to.purchase.products.from.the.following.e.commerce.companies...Flipkart.,data$Have.you.shopped.during..The.Big.Billion.Days..)
#Since p < 0.05, we accept alternative hypothesis i.e customer's satisfaction to shop on Flipkart influences whether the customer has shopped during 'Big Billion Days'.


#Visualization of Multiple criteria dimensional analysis(MCDA)
#Comparing customers who have shopped and did not shop during big billions day sale to what factors(Quality,Price,Brand,Review,Discount Percentage) do they consider when they purchase a product during online sale season.
a<-as.factor(data$Have.you.shopped.during..The.Big.Billion.Days..)
bigbilliondayshopped<-as.numeric(a) #No is 1 and Yes is 2.


#Quality considered as a factor for purchase:
par(mfrow=c(3,2))
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Quality.==1],ylim=c(0,5),xlab='Customers shopping status during Big Billion Days',main='Quality considered as least important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Quality.==2],ylim=c(0,5),xlab='Customers shopping status during Big Billion Days',main='Quality considered as slightly important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Quality.==3],ylim=c(0,15),xlab='Customers shopping status during Big Billion Days',main='Quality considered as important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Quality.==4],ylim=c(0,30),xlab='Customers shopping status during Big Billion Days',main='Quality considered as fairly important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Quality.==5],ylim=c(0,35),xlab='Customers shopping status during Big Billion Days',main='Quality considered as very important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)

#Hypothesis testing
#If the customer did or did not shop during the Big Billion Days sale, does it influence their preference of choosing quality as a factor before purchasing a product during  the sale?
#H0: Customer shopping status during the Big Billion Days sale does not influence their preference of choosing quality as a factor before purchasing a product during sale.
#H1: Customer shopping status during the Big Billion Days sale does influence their preference of choosing quality as a factor before purchasing a product during sale.
chisq.test(data$Have.you.shopped.during..The.Big.Billion.Days..,data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Quality.)
#Since p > 0.05, we accept alternative hypothesis i.e customer shopping status during the Big Billion Days sale does not influence their preference of choosing quality as a factor before purchasing a product during sale.


#Brand considered as a factor for purchase:
par(mfrow=c(3,2))
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Brand.==1],ylim=c(0,5),xlab='Customers shopping status during Big Billion Days',main='Brand considered as least important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Brand.==2],ylim=c(0,5),xlab='Customers shopping status during Big Billion Days',main='Brand considered as slightly important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Brand.==3],ylim=c(0,20),xlab='Customers shopping status during Big Billion Days',main='Brand considered as important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Brand.==4],ylim=c(0,20),xlab='Customers shopping status during Big Billion Days',main='Brand considered as fairly important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Brand.==5],ylim=c(0,30),xlab='Customers shopping status during Big Billion Days',main='Brand considered as very important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)

#Hypothesis testing
#If the customer did or did not shop during the Big Billion Days sale, does it influence their preference of choosing brand as a factor before purchasing a product during  the sale?
#H0: Customer shopping status during the Big Billion Days sale does not influence their preference of choosing brand as a factor before purchasing a product during sale.
#H1: Customer shopping status during the Big Billion Days sale does influence their preference of choosing brand as a factor before purchasing a product during sale.
chisq.test(data$Have.you.shopped.during..The.Big.Billion.Days..,data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Brand.)
#Since p > 0.05, we accept null hypothesis i.e customer shopping status during the Big Billion Days sale does not influence their preference of choosing brand as a factor before purchasing a product during sale.


#Price considered as a factor for purchase:
par(mfrow=c(3,2))
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Price.==1],ylim=c(0,5),xlab='Customers shopping status during Big Billion Days',main='Price considered as least important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Price.==2],ylim=c(0,5),xlab='Customers shopping status during Big Billion Days',main='Price considered as slightly important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Price.==3],ylim=c(0,15),xlab='Customers shopping status during Big Billion Days',main='Price considered as important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Price.==4],ylim=c(0,30),xlab='Customers shopping status during Big Billion Days',main='Price considered as fairly important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Price.==5],ylim=c(0,30),xlab='Customers shopping status during Big Billion Days',main='Price considered as very important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)

#Hypothesis testing
#If the customer did or did not shop during the Big Billion Days sale, does it influence their preference of choosing price as a factor before purchasing a product during  the sale?
#H0: Customer shopping status during the Big Billion Days sale does not influence their preference of choosing price as a factor before purchasing a product during sale.
#H1: Customer shopping status during the Big Billion Days sale does influence their preference of choosing price as a factor before purchasing a product during sale.
chisq.test(data$Have.you.shopped.during..The.Big.Billion.Days..,data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Price.)
#Since p > 0.05, we accept null hypothesis i.e customer shopping status during the Big Billion Days sale does not influence their preference of choosing price as a factor before purchasing a product during sale.


#Reviews considered as a factor for purchase:
par(mfrow=c(3,2))
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Price.==1],ylim=c(0,5),xlab='Customers shopping status during Big Billion Days',main='Reviews considered as least important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Price.==2],ylim=c(0,5),xlab='Customers shopping status during Big Billion Days',main='Reviews considered as slightly important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Price.==3],ylim=c(0,15),xlab='Customers shopping status during Big Billion Days',main='Reviews considered as important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Price.==4],ylim=c(0,30),xlab='Customers shopping status during Big Billion Days',main='Reviews considered as fairly important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Price.==5],ylim=c(0,30),xlab='Customers shopping status during Big Billion Days',main='Reviews considered as very important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)

#Hypothesis testing
#If the customer did or did not shop during the Big Billion Days sale, does it influence their preference of choosing reviews as a factor before purchasing a product during  the sale?
#H0: Customer shopping status during the Big Billion Days sale does not influence their preference of choosing reviews as a factor before purchasing a product during sale.
#H1: Customer shopping status during the Big Billion Days sale does influence their preference of choosing reviews as a factor before purchasing a product during sale.
chisq.test(data$Have.you.shopped.during..The.Big.Billion.Days..,data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Reviews.)
#Since p > 0.05, we accept null hypothesis i.e customer shopping status during the Big Billion Days sale does not influence their preference of choosing reviews as a factor before purchasing a product during sale.


#Discount Percentage considered as a factor for purchase:
par(mfrow=c(3,2))
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Discount.Percentage.==1],ylim=c(0,5),xlab='Customers shopping status during Big Billion Days',main='Discount Percentage considered as least important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Discount.Percentage.==2],ylim=c(0,5),xlab='Customers shopping status during Big Billion Days',main='Discount Percentage considered as slightly important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Discount.Percentage.==3],ylim=c(0,15),xlab='Customers shopping status during Big Billion Days',main='Discount Percentage considered as important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Discount.Percentage.==4],ylim=c(0,30),xlab='Customers shopping status during Big Billion Days',main='Discount Percentage considered as fairly important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)
hist(bigbilliondayshopped[data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Discount.Percentage.==5],ylim=c(0,30),xlab='Customers shopping status during Big Billion Days',main='Discount Percentage considered as very important',col=c('red','green'),breaks=10)+legend("center", c("Red - Did not shop", "Green - Shopped"), col=c("red", "green"), lwd=5,cex=0.75)

#Hypothesis testing
#If the customer did or did not shop during the Big Billion Days sale, does it influence their preference of choosing discount percentage as a factor before purchasing a product during  the sale?
#H0: Customer shopping status during the Big Billion Days sale does not influence their preference of choosing discount percentage as a factor before purchasing a product during sale.
#H1: Customer shopping status during the Big Billion Days sale does influence their preference of choosing discount percentage as a factor before purchasing a product during sale.
chisq.test(data$Have.you.shopped.during..The.Big.Billion.Days..,data$On.a.scale.of.1.5..1.being.least.and.5.being.most...rate.the.following.important.factors.that.you.would.consider.while.shopping.during.the.sale.season...Discount.Percentage.)
#Since p > 0.05, we accept null hypothesis i.e customer shopping status during the Big Billion Days sale does not influence their preference of choosing discount percentage as a factor before purchasing a product during sale.

