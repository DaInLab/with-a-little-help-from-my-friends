
# Scatter plot
ExpNumViz(Carseats,target="Price",nlim=4,fname=NULL,col=NULL,Page=NULL,sample=1)
# Density plot
ExpNumViz(Carseats,target=NULL,nlim=10,sample=1)
# Bar plot
ExpCatViz(Carseats,target=NULL,clim=5,margin=2,sample=1)
# Box plot
ExpNumViz(Carseats,target="US",type=2,nlim=10,sample=1)
# Normality plot
ExpOutQQ(Carseats,nlim=10,sample=1)
# Co-ordinate plots
ExpParcoord(Carseats,Group="ShelveLoc",Stsize=c(10,15,20),Nvar=c("Price","Income","Advertising","Population","Age","Education"))


