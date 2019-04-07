function trafficChart(data){
        
    // data = [23,12,64,23,12,6,7,9,34,55,76,23,43,60,13,55,95,62,34,85,23,51,82,13]
    console.log(data)


    
    var dataStageValue = 0
    var dataStage = []
    for(i = 0 ; i < data.length ; i+=4){
        for(j = 0 ; j < 4 ; j++){
            dataStageValue += data[i+j]
        }
        dataStage.push(dataStageValue)
    }
    console.log(dataStage)

    var pieChartDiv = d3.select('#pieChartDiv')
    pieChartDiv.append('section')
                .style('margin-top', '10%')

    var w = 1400
    var h = 700
    var outerR = Math.min(w, h) / 2.1
    var innerR = outerR * 0.7

    var color = d3.scaleOrdinal(["#FDB815","#F8941D", "#F01B26", "#D75729", "#a6d854", "#ffd92f"])

    var svg = pieChartDiv.select('section')
                        .append('svg')
                        .attr('width', w)
                        .attr('height', h)
                        .append('g')
                        .attr('transform', `translate(${w / 4},0)`)
    

    // function rightRoundedRect(x, y, width, height, radius) {
    //     return "M" + x + "," + y
    //         + "h" + (width - radius)
    //         + "a" + radius + "," + radius + " 0 0 1 " + radius + "," + radius
    //         + "v" + (height - 2 * radius)
    //         + "a" + radius + "," + radius + " 0 0 1 " + -radius + "," + radius
    //         + "h" + (radius - width)
    //         + "z";
    // }

    //比例尺
    
    var xScale = d3.scaleLinear()
    .domain([0, Math.round((Math.max(...dataStage))*1.2)])
    .range([0, 800])
    
    var yScale = d3.scaleLinear()
    .domain([0, dataStage.length+1])
    .range([0, 400])    
    
    
    svg.append("g")
    .attr("class", "trafficyAxis")
    .attr("transform", "translate(" + 0 + "," + -28 + ")")
    .call(d3.axisLeft(yScale)
    .tickValues([1,2,3,4,5,6])
    .tickFormat(function(d,i){
        var timeStage = ["1 - 4","5 - 8","9 - 12","13 - 16","17 - 20","21 - 24"]
        return timeStage[i]
    })
    .tickSizeInner(0)
    .tickSizeOuter(0));
    
    d3.selectAll(".trafficyAxis text").attr("x","-20")
    d3.selectAll(".trafficyAxis path").attr("opacity","0")
    
    
    svg.append("g")
    .attr("class", "trafficxAxis")
    .attr("transform", "translate(" + 0 + "," + 370 + ")")
    .call(d3.axisBottom(xScale)
    .ticks(5)
    .tickSizeInner(-400)
    .tickSizeOuter(0))
    
    d3.selectAll(".trafficxAxis line").attr("opacity","0.15")
    d3.selectAll(".trafficxAxis g:nth-child(2) > line").attr("opacity","0")
    d3.selectAll(".trafficxAxis path").attr("opacity","0.5")
    d3.selectAll(".trafficxAxis text").attr("y","0.35em")
    
    svg.selectAll(".trafficChart")
        .data(dataStage)
        .enter()
        .append("rect")
        .attr("class","trafficChart")
        .attr("transform", "translate(" + 0 + ", " + 18 + ")")
        .attr('y',function(d,i){return i * 57})
        .attr('fill','#4a90e2')
        // .attr('fill',function(d,i){
        //     return color(i)
        // })
        .attr('height','25px')
        .attr('width',0+1)
        .transition()
        .duration(1000)
        .attr('width',function(d,i){
            //xScale的range 除 domain的比例 乘 x在最大值的百分比
            return (800/1.2) * dataStage[i]/Math.round((Math.max(...dataStage)))  
        })

    svg.selectAll(".trafficText")
        .data(dataStage)
        .enter()
        .append("text")
        .text(0)
        .attr("class","trafficText")
        .attr("transform", "translate(" + 0 + ", " + 36 + ")")
        .attr('y',function(d,i){return i * 57})
        .attr('x',10)
        .attr('font-family','Microsoft JhengHei')
        .attr('font-weight','bold')
        .transition()
        .duration(1000)
        .attr('x',function(d,i){return (800/1.2) * dataStage[i]/Math.round((Math.max(...dataStage))) +10})
        .tween('text',function(d,i){
            var change = d3.interpolateRound(0, dataStage[i]);
            return function(t) {this.textContent = change(t)}
        })

}