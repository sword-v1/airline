# 查看数据源
str(mpg)

# 绘制简单散点图 
library(ggplot2)
p <- ggplot(data=mpg, mapping=aes(x=cty, y=hwy))
p + geom_point()

# 将年份映射到颜色属性
p <- ggplot(mpg,
              aes(x=cty, y=hwy, colour=factor(year)))
p + geom_point()

# 增加平滑曲线
p <- ggplot(mpg,
              aes(x=cty, y=hwy, colour=factor(year)))
p + geom_point() + stat_smooth()

p <- ggplot(mpg, aes(x=cty,y=hwy))
p + geom_point(aes(colour=factor(year)))+
  stat_smooth()

# 用标度来修改颜色取值
p + geom_point(aes(colour=factor(year)))+
  stat_smooth()+
  scale_color_manual(values =c('blue','red'))

# 将排量映射到散点大小
p + geom_point(aes(colour=factor(year),size=displ))+
  stat_smooth()+
  scale_color_manual(values =c('blue2','red4'))

# 利用facet分别显示不同年份的数据
p + geom_point(aes(colour=class, size=displ),
                alpha=0.5, position = "jitter")+ stat_smooth()+
  scale_size_continuous(range = c(4, 10))+
  facet_wrap(~ year, ncol=1)

# 增加图名幵精细修改图例
p <- ggplot(mpg, aes(x=cty, y=hwy))
p + geom_point(aes(colour=class,size=displ),
                 alpha=0.5,position = "jitter")+
  stat_smooth()+
  scale_size_continuous(range = c(4, 10))+
  facet_wrap(~ year,ncol=1)+
  opts(title='汽车油耗与型号')+
  labs(y='每加仑高速公路行驶距离',
       x='每加仑城市公路行驶距离')+
  guides(size=guide_legend(title='排量'),
         colour = guide_legend(title='车型',
                               override.aes=list(size=5)))