library('ggplot2')
df <- mtcars
head(df)

qplot(wt,mpg,data=df)
qplot(wt,mpg,data =df,color=cyl) #according to color
qplot(wt,mpg,data=df,size=cyl) #according to size
qplot(wt,mpg,data=df,size=cyl,color=cyl) #or both

#using ggplot2

#using color
pl1 <- ggplot(data = df,aes(x=wt,y=mpg))
pl2 <- pl1+ geom_point(aes(color=cyl))
print(pl2)

#using color factor
pl2 <- pl1+ geom_point(aes(color=factor(cyl)))
print(pl2)

#using shape
pl2 <- pl1+ geom_point(aes ( shape=factor(cyl), color= factor(cyl) ) )
print(pl2)

#gradient scale
pl2 <- pl1+ geom_point(aes(color=cyl), size= 5) + scale_color_gradient(high='red',low='yellow')
print(pl2)
