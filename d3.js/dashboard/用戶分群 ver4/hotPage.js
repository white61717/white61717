function hotPage(data){
            
    var stringified = JSON.stringify(data)
    var stringify = stringified.replace(/ @ 食力foodNEXT‧食事求實的知識頻道/g,"")
    var jsonObject = JSON.parse(stringify);
    console.log(jsonObject)

    var dataPage = []
    for(i = 0 ; i < 5 ; i++){
        dataPage.push(jsonObject[i])
    }
    
    var w = 1400
    
    var h = 700
    
    var hotPageDiv = d3.select('#pieChartDiv')
    
    hotPageDiv.append('section')
        .attr("id","hotPage")
        .style('margin-top', '10%')
        .style('margin-right', '20%')
        .attr('width', w)
        .attr('height', h)
        .append('g')
        .attr('transform', `translate(${w / 2}, ${h / 2})`)
        // .style('position','absolute')

    var hotPageUl = hotPageDiv.select('section')
                            .append('ul')
    
    var hotPageUl2 = hotPageUl.append("ul")
            .style('font-size',"40px")
            .style("padding-bottom","50px")
            .style("padding-left","0px")
            .style('font-family', "Microsoft JhengHei")
            .style('font-weight','bold')
            .text('熱門頁面')

    hotPageUl2.append('span')
            .text("瀏覽人數")
            .style('float','right')
            .style('font-family', "Microsoft JhengHei")
            .style('font-weight','bold')

    var hotPageLi = hotPageUl.selectAll("li")
                            .data(dataPage)
                            .enter()
                            .append("li")
                            .style('font-size',"30px")
                            .style("padding-bottom","50px")
                            .style('list-style-type','decimal')
                            // .style('position','relative')

    hotPageLi.append("a").style("padding-bottom","50px")
                        .style('font-size',"30px")
                        .style('font-family', "Microsoft JhengHei")
                        .text(function(d,i){
                            if(dataPage[i].title.length<22){
                                return dataPage[i].title
                            }else{
                                return dataPage[i].title.slice(0, 22) + ' ...'
                            }
                        })
                        .attr('href',function(d,i){return "https://www.foodnext.net/" + dataPage[i].pageid})
                        .attr('title',function(d,i){return dataPage[i].title})
                        
    hotPageLi.append("span")
            .text(function(d,i){return dataPage[i].uv})
            .style('float','right')
            .attr('text-anchor', 'center')
            .style('font-size',"35px")

}