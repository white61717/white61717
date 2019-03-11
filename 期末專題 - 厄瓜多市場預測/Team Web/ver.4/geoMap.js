function initialGeoMap(){
	
    var width = "100%",
        height = "100%";

    var padding = {top:20,bottom:20,left:10,right:20}
	
	var geoMapDiv = d3.select('#geoMapDiv')
	
    var svg = geoMapDiv.append("svg")
        // .style("float","right")
        .attr("id","svgmap")
        .attr("width", width)
        .attr("height", height);

    // var wordDiv = geoMapDiv.append("div").attr("height",300).attr("width",1000)
    var word1 = svg.append("text").attr("x","6%").attr("y","12%").style("font-size","70px")
    var word2 = svg.append("text").attr("x","6%").attr("y","12%").attr("dy","7%").style("font-size","30px")
    var word3 = svg.append("text").attr("x","6%").attr("y","12%").attr("dy","12%").style("font-size","30px")

    //定義一個線性漸層
    var defs = svg.append("defs");

    var linearGradient = defs.append("linearGradient")
                            .attr("id","linearColor")
                            .attr("x1","0%")
                            .attr("y1","0%")
                            .attr("x2","100%")
                            .attr("y2","0%");


    //墨卡托投影
    var projection = d3.geo.mercator()
                            .center([-80, 0])           //經緯度
                            .scale(4500)                //縮放因數
                            .translate([980, 260]);        //基準點
    //定義地理路徑產生器
    var path = d3.geo.path()
                    .projection(projection);        //設定投影


    var width2 = "150%",
        height2 = "50%";

    var box = geoMapDiv.append("div")
            .attr("id","divbox")

    var svg2 = box.append("svg")
            .attr("width",width2)
            .attr("height",height2)
            // .attr("transform","translate(80,0)")
            .style("border","1px solid #DCDCDC")
            .attr('opacity','0.8')
            .style("background-color","white")
    var rectx = ["GROCERY I","BEVERAGES","PRODUCE","CLEANING","DAIRY","BREAD/BAKERY"]

    //長條
    var bar = svg2.selectAll("rect")
            .data(rectx)
            .enter()
            .append("rect")
            .attr("transform","translate(90,30)")
            .attr("x",function(d,i){
                return padding.left + i * 44 + 16
            })
            .attr("y",250)
            .attr("width",30)
            .attr("height",0)


    //座標軸
    var logscale = d3.scale.pow().exponent(0.5).domain([0,300000000]).range([0,250])
    logscale.domain([300000000,0])  
    var logaxis = d3.svg.axis().scale(logscale).orient("left").tickValues([0,1000000,10000000,100000000,200000000])   //.innerTickSize(-400)
    // d3.select("#divbox").select("svg").select("g").select("path").attr('opacity','0.2')
    svg2.append("g")
    // .attr("transform","translate(80,30)")
    .attr("class","axis").call(logaxis)

    logscale.domain([0,300000000])         
    var linear = d3.scale.linear().domain([0,7]).range([0,310]);
    var axisLinear = d3.svg.axis().scale(linear).orient("bottom")
                    .tickValues([1,2,3,4,5,6])
                    .tickSize(4,0)
                    .tickFormat(function(d,i){
                        var tempList = ["雜貨 I","飲料","生產",
                        "清潔","乳製品","麵包"]
                        return tempList[i]
                        })

    var x = svg2.append("g")
    // .attr("transform","translate(80,250)")
    .attr("class","axis")
    .attr("id","salesName").call(axisLinear);
    
    x.selectAll(".tick text")
     .style("font-family", "Microsoft JhengHei")
     .style("font-weight", "bold")
     .style("font-size", "12px")
    //  .attr("x",-7)                   
     .attr("y",10)                   


    //經緯度格線
    var eps = 1e-4
    var graticule = d3.geo.graticule()
                        .extent([[-180, -90],[180+eps, 90]])
                        .step([1,1])


    var grid = graticule()
    var gridPath = svg.append("path").datum(grid).attr("class","graticule").attr("d",path).attr('opacity','0.25').attr("transform","translate(50,2)")                  

    d3.json("sale.json", function(error, saledata){
                        
        console.log(saledata)

        d3.json("map.topojson", function(error, toporoot) {         //讀topojson
            if (error) 
                return console.error(error);
            
            console.log(toporoot);                          //輸出toporoot物件                          
            
            //將TopoJSON物件轉換成GeoJSON，儲存在georoot中
            var georoot = topojson.feature(toporoot,toporoot.objects.map);
            
            //輸出GeoJSON物件
            console.log(georoot);

            var groups = svg.append("g");

            var paths = groups.selectAll("path")
                    .data( georoot.features )
                    .enter()
                    .append("path")
                    .style("stroke","black")
                    .style("stroke-width","1px")
                    .attr("id",function(d){
                        return d.properties.name
                    })
                    .style("fill", "#ccc")
                    .attr("d", path );                  //畫出投影
                    console.log(georoot.features)

            var stateList = ["Imbabura","Tungurahua","Pichincha","Azuay","Bolivar","Chimborazo","Loja","Cotopaxi","Esmeraldas","Manabi",
            "Santo Domingo de los Tsachilas","Santa Elena","Guayas","El Oro","Los Rios","Pastaza"]


            var location = svg.append("circle")
                        .attr("r",4)
                        .style("fill","#CCDDFF")
                        .attr("class","location")
                        .attr("transform",function(d){
                            //計算標注點的位置
                            var coor = projection([-78.31, -0.14]);
                            return "translate("+ coor[0] + "," + coor[1] +")";
                        });
            svg.append("text")
                .text("Quito")
                .style("fill","#CCDDFF")
                .style("font-weight","bold")
                .attr("transform",function(d){
                    //計算標注點的位置
                    var coor = projection([-78.6 , -0.07]);
                    return "translate("+ coor[0] + "," + coor[1] +")";
                });

            paths.on("mouseover",function(d,i){

                d3.select(this)
                    .transition()
                    .duration(200)
                    .style("fill","#FF8888")
                //加字
                word1.text(d.properties.name).style("font-weight","bold")
                word2.text("Total sales : " + d.properties.sales.toLocaleString())
                word3.text("Store count : " + d.properties.store_nbr.toLocaleString())

                
                var stateName = d.properties.name
                    for(var j = 0 ; j <16 ; j++){
                        if (stateList[j] == stateName){
                            var stateNum =  j
                        }
                    }
                    
                bar.data(saledata.provinces[stateNum].sale)
                    .transition()
                    .attr("fill","#77DDFF")
                    .attr("x",function(d,i){
                        return padding.left + i * 44 + 16
                    })
                    .attr("y",function(d,i){
                        return 250 - logscale(d.unit_sales)
                    })
                    .attr("width",30)
                    .attr("height",function(d){return logscale(d.unit_sales)})
            }) 
            
            d3.json("tourism.json", function(error, valuedata){
                // console.log(valuedata)
                //將讀取到的資料存到陣列values，令其索引號為各省的名稱
                var values = [];
                for(var i=0; i<valuedata.provinces.length; i++){
                    var name = valuedata.provinces[i].name;
                    var value = valuedata.provinces[i].value;
                    values[name] = value;
                }

                //求最大值和最小值
                var maxvalue = d3.max(valuedata.provinces, function(d){ return d.value; });
                var minvalue = d3.min(valuedata.provinces, function(d){ return d.value; });
                console.log(valuedata.provinces)
                var meanvalue = d3.mean(valuedata.provinces, function(d){ return d.value; });
                //定義一個線性比例尺，將最小值和最大值之間的值映射到[0, 1]

                var linear = d3.scale.linear()
                                .domain([1000000,90000000,165152157, maxvalue])
                                .range([0, 1]);

                //定義最小值和最大值對應的彩色
                var a = d3.rgb(0,255,255);	//淺藍色
                var b = d3.rgb(0,0,255);	//藍色

                //彩色插值函數
                var computeColor = d3.interpolate(a,b);

            //設定線性漸層圖
                var stop1 = linearGradient.append("stop")
                                .attr("offset","0%")
                                .style("stop-color",a.toString());

                var stop2 = linearGradient.append("stop")
                                .attr("offset","100%")
                                .style("stop-color",b.toString());

                //加入一個矩形，並套用線性漸層
                var colorRect = svg.append("rect")
                            .attr("x", "82.6%")
                            .attr("y", "85%")
                            .attr("width", 75)
                            .attr("height", 5)
                            .attr("id","colorRect")
                            .style("fill","url(#" + linearGradient.attr("id") + ")");

                




            // d3.json("population.json", function(error, populationData){

            //     console.log(populationData)
            //     geoMapDiv.append("button").text("change").on("click",function(){
                    
            //         var maxvalue = d3.max(populationData.provinces, function(d){ return d.value; });
            //         var minvalue = d3.min(populationData.provinces, function(d){ return d.value; });
            //         var linear = d3.scale.linear()
            //                         .domain([minvalue, maxvalue])
            //                         .range([0, 1]);

            //         //定義最小值和最大值對應的彩色
            //         var a = d3.rgb(255,0,0);	//淺藍色
            //         var b = d3.rgb(0,255,0);	//藍色

            //         //彩色插值函數
            //         var computeColor = d3.interpolate(a,b);

            //         //設定線性漸層圖
            //         var stop1 = linearGradient.append("stop")
            //                         .attr("offset","0%")
            //                         .style("stop-color",a.toString());

            //         var stop2 = linearGradient.append("stop")
            //                         .attr("offset","100%")
            //                         .style("stop-color",b.toString());

            //     })

            // })


                //加入文字
                var minValueText = svg.append("text")
                            .attr("class","valueText")
                            .attr("x", "82.6%")
                            .attr("y", "85%")
                            .attr("dy", "-0.3em")
                            .text("1")
                            .style({
                                "font-size":7,
                            }).attr("fill","gray")

                var maxValueText = svg.append("text")
                            .attr("class","valueText")
                            .attr("x", "87.2%")
                            .attr("y", "85%")
                            .attr("dy", "-0.3em")
                            .text("600")
                            .attr("fill","gray")
                            .style({
                                "font-size":7,
                            })
                var psText = svg.append("text")
                            .attr("class","valueText")
                            .attr("x", "83.3%")
                            .attr("y", "87.5%")
                            .attr("dy", "-0.3em")
                            .text("銷售數量(百萬)")         
                            .attr("fill","gray")
                            .style({
                                "font-size":8.5,
                            })
                //設定各省份的填充色
                paths.style("fill", function(d,i){
                    var t = linear( values[d.properties.name] );
                    var color = computeColor(t);
                    return color.toString();
                }).on("mouseout",function(d,i){                 //放這裡是因為放裡面的時候tourism.json還沒載入
                    

                    if(values[d.properties.name]){              //如果map.topojson的name有tourism.json的值則
                        d3.select(this)
                            .transition()
                            .duration(200)
                            .style("fill",function(d,i){
                        var t = linear( values[d.properties.name] );
                        var color = computeColor(t);
                        
                        return color.toString();
                    })
                    }else{                                   //沒有則
                        d3.select(this)
                            .transition()
                            .duration(200)
                            .style("fill","#ccc")
                    }

                    
                    word1.text("")
                    word2.text("")
                    word3.text("")
                    

                    d3.selectAll("#divbox rect").attr("fill","#77DDFF")
                        .transition()
                        .attr("x",function(d,i){
                            return padding.left + i * 44 + 16
                        })
                        .attr("y",250)
                        .attr("width",30)
                        .attr("height",0)
                })    

            })        
        })
    });
}