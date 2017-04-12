FontMap-of-China
===============
Use the font of EyesAsia to make a beautiful ChinaMap which circles each privince ploygon.
--------------

library the packages <br>

```r
library(rvest)           
library(dplyr)          
library(stringr)        
library(showtext)      
library(Cairo)          
library(RColorBrewer)   
library(ggplot2)        
library(grid)           
```

> 由于本文用到了一款中国行政区划的字体地图——EyesAsia，每一个行政区都是以一个字母代替的，所以需要获取该地图字体对应的索引表。该字体的开源项目主页为：[EyesAsia](https://github.com/HaoyunS/EyesAsia)

> 与此对应的，还有一款也很fashion的字体地图（StateFace），是美帝的行政区划字体地图。项目主页在这里:[stateface](https://propublica.github.io/stateface)


一共43个编号，以下是提取过程，因为是一个table，所以可以直接使用rvest非常便捷的表格抓取工具。<br>

```r
url<-"https://github.com/haoyuns/EyesAsia"
table<-read_html(url,encoding="utf-8")%>%html_table()%>%.[[2]]
table1<-table[table$lowercase!="",]
table2<-table[table$lowercase=="",]%>%.[,2:3]
table11<-table1[,1:2]%>%rename(case=lowercase)
table12<-table1[,3:4]%>%rename(case=UPPERCASE)
table13<-table2%>%rename(case=Content,Content=UPPERCASE)
tabledata<-rbind(table11,table12,table13)
```

筛选出中国的34个省级行政区

```r
tabledata$Cname<-str_extract(tabledata$Content,"[\\u4e00-\\u9fa5]+")
tabledata$Ename<-str_extract(tabledata$Content,"[^\\u4e00-\\u9fa5]+")%>%str_trim(side=c("right"))
tabledata<-tabledata[,-2]
setwd("D:/R/File")
write.table(tabledata,"EyesAsia.csv",sep=",",row.names=FALSE)
word<-c("日本","蒙古","朝鲜","韩国","青海湖","鄱阳湖","洞庭湖","太湖","洪泽湖")
mymapdata<-tabledata
mymapdata$m<-mymapdata$Cname %in% word
mymapdata<-mymapdata%>%filter(m==FALSE)%>%.[,1:3]
write.table(mymapdata,"EyesAsia.csv",sep=",",row.names=FALSE)
```

作图主要过程分为三部分：
=======================
步骤一：外围字体圆环图：
-----------------------

```r
#导入数据：
#生成一个虚拟指标，并分割为有序分段因子变量。
mymapdata<-read.csv("EyesAsia.csv",stringsAsFactors=FALSE,check.names=FALSE)
mymapdata<-transform(mymapdata,scale=5,peform=runif(34,20,50))
mymapdata$scale<-as.numeric(mymapdata$scale)
mymapdata$group<-cut(mymapdata$peform,breaks=c(20,26,32,38,44,50),levels=,labels=c("20~26","26~32","32~38","38~44","44~50"),order=TRUE)
mymapdata<-arrange(mymapdata,desc(peform));mymapdata$order=1:nrow(mymapdata)
mymapdata$order<-as.numeric(mymapdata$order)
```

作图函数：

```r
CairoPNG("chineserador.png",900,900)
showtext.begin()
ggplot(mymapdata,aes(order,scale,label=case))+
ylim(-6,6)+
coord_polar(theta="x",start=0)+
geom_text(aes(colour=group),family="myfont",size=20)+
scale_colour_brewer(palette="Greens",guide=FALSE)+
theme_minimal()+
theme(
panel.grid=element_blank(),
axis.title=element_blank(),
axis.text=element_blank(),
)
showtext.end()
dev.off()
```  

<div  align="center">    
<img src="https://github.com/ljtyduyu/FontMap-of-China/blob/master/Image/circle.png" width = "500" height = "500" alt="circle" align=center />
</div>

步骤二：接下来制作中心的中国地图
-----------------------------------------------------

其实针对中国省级地图素材而言，大部分shp格式的地图都是可以放心使用的，但是为了练习自己对于json数据的操控能力（毕竟是非常流行的web端数据存储格式），
这里我硬生生的抽取了json格式的中国地图数据，所以以下代码看着有些不适，请大家谨慎观看！<br>

```r
library(plyr)         
library(maptools)      
library(scales)       
library(jsonlite)
library(jsonview)
```

导入json格式中国地图：

```r
setwd("D:/R/mapdata/State/")
china_data<-fromJSON("china.json")
json_tree_view(china_data) 
```

<div  align="center">    
<img src="https://github.com/ljtyduyu/FontMap-of-China/blob/master/Image/jsonview.png" width = "400" height = "400" alt="jsontree" align=center />
</div>


最新发现的可以自动化解析并渲染json树结构的包，它不仅可以渲染json数据，也可以渲染xml、html格式的树结构：<br>

抽取行政区里列表信息：

```r
china_city_data<-china_data$features$properties[,c(1,3)]
names(china_city_data)[2]<-"region"
china_city_data$ID<-1:nrow(china_city_data)
china_city_data$size<-runif(34,900,1150)
china_city_data$group<-cut(china_city_data$size,breaks=c(900,950,1000,1050,1100,1150),labels=c("900~950","951~1000","1001~1050","1051~1100","1101~1150"),order=TRUE)
```

抽取行政区划边界经纬度多边形数据：（最艰难的部分）

```r
china_map_data<-china_data$features$geometry$coordinates
```

还时上次讲到的困难，中国某些省份辖区内有独立于主区域的分离区域（比如河北的廊坊，以及山东、及南部沿海多岛屿的省份）。<br>

今天这个json素材要比上次提取的那个安徽省的素材更加复杂，具体步骤也不详细讲解了，看不太懂就直接略过吧，反正代码写的也比较烂，基本写不出那种可以通用的代码！<br>

```r
num<-c();id<-c()
for( i in 1:length(china_map_data)){
citymapdata<-china_map_data[[i]]
num[i]<-length(citymapdata)
id<-1:i
a<-data.frame(id,num)
}
a[a$num<=2,]
   id num
12 12   2
14 14   2
```

```r
dim(china_map_data[[14]][[1]])=c(length(china_map_data[[14]][[1]])/2,2)
dim(china_map_data[[14]][[2]])=c(length(china_map_data[[14]][[2]])/2,2)
```

```r
mapdata1<-data.frame()
mapdata2<-data.frame()
for( i in 1:length(china_map_data)){
    citymapdata<-china_map_data[[i]]
        if (length(citymapdata)<=2){
            for(m in 1:length(citymapdata)){
                citymapdata1<-data.frame(citymapdata[[m]])%>%dplyr::rename(long=X1,lat=X2)
                citymapdata1$ID<-i
                citymapdata1$group<-as.numeric(paste0(i,".",m,1))
                citymapdata1$order<-1:nrow(citymapdata1)
             mapdata1<-rbind(mapdata1,citymapdata1,citymapdata2)
             }
        }else{
             dim(citymapdata)=c(length(citymapdata)/2,2)
             citymapdata2<-data.frame(citymapdata)%>%dplyr::rename(long=X1,lat=X2)
             citymapdata2$ID<-i
             citymapdata2$group<-as.numeric(paste0(i,".",1))
             citymapdata2$order<-1:nrow(citymapdata2)
         mapdata2<-rbind(mapdata2,citymapdata2)
        }
    mydatanew<-rbind(mapdata1,mapdata2)
}
```

至此经纬度的边界点信息也有了，接下来就可可以映射地图了：<br>

```r
mydatanew<-dplyr::arrange(mydatanew,ID,order)
```

合并经纬度边界点信息和行政区划信息。

```r
mydatanew_map_data<-merge(mydatanew,china_city_data[,c(2,3,4)])
```

预览地图素材是否可用：<br>

```r
ggplot(mydatanew_map_data,aes(long,lat,group=group))+geom_polygon(col="white",fill="grey")+
coord_map("polyconic")+
     theme(               
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()
          )
```

预览效果图：<br>


<div  align="center">    
<img src="https://github.com/ljtyduyu/FontMap-of-China/blob/master/Image/ploygon.png" width = "400" height = "400" alt="test" align=center />
</div>


最后放个大招，用两个地图品进行拼接，合并。
=======================================
第一款字体时最初提到的地图字体（需要事先下载哦）；第二款就是微软雅黑喽，渲染省份标签用的。

```r
font.add("myfont","EyesAsia-Regular.otf")
font.add("myyh","msyhl.ttc")
```

为了更加舒适的看圆环上的省份标签，这里给标签添加角度偏移量。

```r
circle<-seq(0,95,length=9)
circleALL<-rep(c(-circle,rev(circle[2:9])),2)
mymapdata$circle<-circleALL
```

鉴于ggplot极坐标下的首尾不衔接的缺陷，这里再查补一个缺失值。

```r
mymapdata<-arrange(mymapdata,order)
mapx<-mymapdata[mymapdata$order==34,]
mapx$order<-35;mapx$Cname=NA;mapx$case=NA
mymapdata1<-rbind(mymapdata,mapx)
```

所有的步骤都弄完之后，接下来将两幅图表存为对象。

```r
p1<-ggplot(mymapdata1,aes(x=order,y=scale))+
ylim(-6,7.5)+
coord_polar(theta="x",start=0)+
geom_text(aes(colour=group,label=case),family="myfont",size=15)+
geom_text(aes(y=scale+2,angle=circle,label=Cname),family="myyh",size=6,vjust=0.5,hjust=.5)+
scale_colour_brewer(palette="Greens",guide=FALSE)+
theme_minimal()+
theme(
panel.grid=element_blank(),
axis.title=element_blank(),
axis.text=element_blank(),
)
```

图表效果大致是这样的：<br>

<div  align="center">    
<img src="https://github.com/ljtyduyu/FontMap-of-China/blob/master/Image/circle.png" width = "450" height = "450" alt="circle" align=center />
</div>

```r
p2<-ggplot(china_city_data,aes(map_id=region,fill=group))+
geom_map(map=mydatanew_map_data,colour="white")+
expand_limits(x=mydatanew_map_data$long,y=mydatanew_map_data$lat)+
scale_fill_brewer(palette="YlOrRd",guide=FALSE)+
coord_map("polyconic")+
     theme(             
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.background=element_rect(I(0),linetype=0)
          )
```

图表效果大致是这样的：<br>

<div  align="center">    
<img src="https://github.com/ljtyduyu/FontMap-of-China/blob/master/Image/China.png" width = "500" height = "500" alt="ChinaMap" align=center />
</div>


拼接：<br>

```r
CairoPNG("chineserador.png",1000,1000)
showtext.begin()
vs <- viewport(width=0.95,height=0.95,x=0.5,y=0.5)    
print(p1,vp=vs)  
vs <- viewport(width=0.75,height=0.8,x=0.5,y=0.5)   
print(p2,vp=vs) 
showtext.end()
dev.off()
```

以下是最终的结果：<br>

<div  align="center">    
<img src="https://github.com/ljtyduyu/FontMap-of-China/blob/master/Image/Final.png" width = "600" height = "350" alt="Final" align=center />
</div>


#OK了，做完收工~

------------------------------------------------------------------------------------------------

联系方式：
----------------------------------------------------
wechat：ljty1991  <br>
Mail:578708965@qq.com <br>
个人公众号：数据小魔方（datamofang） <br>
团队公众号：EasyCharts <br>
qq交流群：[魔方学院]553270834

个人简介：
-------------------------------------------------
**杜雨** <br>
财经专业研究僧； <br>
伪数据可视化达人； <br>
文科背景的编程小白； <br>
喜欢研究商务图表与地理信息数据可视化，爱倒腾PowerBI、SAP DashBoard、Tableau、R ggplot2、Think-cell chart等诸如此类的数据可视化软件，创建并运营微信公众号“数据小魔方”。 <br>
Mail:578708965@qq.com <br>

<div  align="center">    
<img src="https://github.com/ljtyduyu/FontMap-of-China/blob/master/Image/resume.png" width = "500" height = "300" alt="resume" align=center />
</div>

-------------------------------------------

备注信息：
----------------------------------------------------
<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="知识共享许可协议" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />本作品采用<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">知识共享署名-非商业性使用 4.0 国际许可协议</a>进行许可。
