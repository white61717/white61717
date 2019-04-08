function singlePieChart(data) {
            

    for(j = 3 ;j < data.length ; j++){
        data[3].click += data[j].click
        data[3].percentageNum += Math.round(data[j].percentageNum)
    }
    data[3].from = "Others"
    for(k = 4 ; k < data.length ; k++){
        delete data[k]
    }
    data.length = 4


    var pieChartDiv = d3.select('#pieChartDiv')
    pieChartDiv.append('section')
                .style('margin-top', '10%')

    var w = 1200
    var h = 700
    var outerR = Math.min(w, h) / 2.5
    var innerR = outerR * 0.7

    var svg = pieChartDiv.select('section')
        .append('svg')
        .attr('width', w)
        .attr('height', h)
        .append('g')
        .attr('transform', `translate(${w / 2}, ${h / 2})`)

    var color = d3.scaleOrdinal(["#FDB815","#F8941D", "#F01B26", "#D75729", "#a6d854", "#ffd92f"])

    var pie = d3.pie()
        .value(function(d, i) {
            return d.click
        })

    var arc = d3.arc()
        .innerRadius(innerR)
        .outerRadius(outerR)

    var pieData = data

    if (pieData == 'none') {

        svg.remove()

        pieChartDiv.select('section header')
            .append('h4')
            .text('無站外流量')
            .style('margin-top', '15%')
            .style('font-size', '30px')
            .style('font-weight', '600')
            .style('font-family', "Microsoft JhengHei")
    } else {
        //新增%數文字
        svg.append("text").attr("id","percentage")
                            .text(0 + "%")
                            .attr('font-weight', '600')
                            .attr('text-anchor', 'middle')
                            .attr("transform", "translate(" + -295 + ", " + 0 + ")")
                            .style('font-size', '80px')
                            .style('font-family', "Microsoft JhengHei")

        svg.append("text").attr("id","name")
                            .text("")
                            .attr('font-weight', '600')
                            .attr('text-anchor', 'middle')
                            .attr("transform", "translate(" + -295 + ", " + 0 + ")")
                            .attr("dy","60px")                                    
                            .style('font-size', '40px')
                            .style('font-family', "Microsoft JhengHei")

        svg.selectAll('path')
            .data(pie(pieData))
            .enter()
            .append('path')
            .attr('fill', function(d, i) { return color(i) })
            .attr('d', arc)
            .attr('stroke', '#ececec')
            .attr('stroke-width', '0px')
            .attr("transform", "translate(" + -300 + ", " + 0 + ")")
            .on("mouseover",function(d,i){
                d3.select(this).transition().attr("d",d3.arc().innerRadius(innerR).outerRadius(outerR * 1.05))

                svg.select("#percentage").transition()
                .attr("fill",color(i))
                .tween("text",  function() {
                    var change = d3.interpolateRound(0, data[i].percentageNum);
                        return function(t) {
                        this.textContent = change(t) + "%";
                    } 
                })

                svg.select("#name")
                    .text(function(){return data[i].from})
                    .attr("fill",color(i))
            }) 
            .on("mouseout",function(d,i){
                d3.select(this).transition().attr("d",d3.arc().innerRadius(innerR).outerRadius(outerR))
                // d3.select("#percentage").transition().text(0 + "%")
            })
            .each(function(d, i) { this._current = d })

        //長條圖
        svg.selectAll(".barChart")
            .data(data)
            .enter()
            .append("rect")
            .attr("class","barChart")
            .attr("transform", "translate(" + 160 + ", " + 0 + ")")
            .attr('y',function(d,i){return i *40})
            // .attr('fill','#a6d854')
            .attr('fill',function(d,i){
                return color(i)
            })
            .attr('height','25px')
            .attr('width',0)
            .transition()
            .duration(1000)
            .attr('width',function(d,i){
                return (500/1.2) * data[i].click  /  data[0].click
            })

        //比例尺
        var xScale = d3.scaleLinear()
            .domain([0, data.length+1])
            .range([0, 200])    

        var yScale = d3.scaleLinear()
            .domain([0, Math.round(data[0].click*1.2)])
            .range([0, 500])      
            
        svg.append("g")
            .attr("class", "xAxis")
            .attr("transform", "translate(" + 160 + "," + -30 + ")")
            .call(d3.axisLeft(xScale)
                    .tickValues([1,2,3,4])
                    .tickFormat(function(d,i){
                        return data[i].from
                    })
                    .tickSizeInner(0)
                    .tickSizeOuter(0));

        svg.append("g")
            .attr("class", "yAxis")
            .attr("transform", "translate(" + 160 + "," + 170 + ")")
            .call(d3.axisBottom(yScale)
                    .ticks(6)
                    .tickSizeOuter(0))

        var schemaR = 8

        svg.selectAll('.pieSchema')
            .data(pieData)
            .enter()
            .append('circle')
            .attr('class', 'pieSchema')
            .attr('r', schemaR)
            .attr('cx', w * 0.2 - schemaR * 2.5)
            .attr('cy', function(d, i) { return i * 30 - schemaR - h * 0.4 })
            .attr('fill', function(d, i) {
                return color(i)
            })
            console.log(pieData)
        
        svg.append('text')
            .selectAll('.pieText')
            .data(pieData)
            .enter()
            .append('tspan')
            .attr('class', 'pieText')
            .attr('x', w * 0.2  )
            .attr('y', function(d, i) { return i * 30 - h * 0.4 })
            .attr('font-size', '20px')
            .attr('font-weight', '600')
            .style('font-family', "Microsoft JhengHei")
            .attr('fill', function(d, i) {
                return color(i)
            })
            .attr('overflow', 'hidden')
            .text(function(d, i) {
                if ((d.click) > 10) {
                    return (d.from).slice(0, 10)
                } else {
                    return d.from
                }
            })

        svg.selectAll('.pieValue')
            .data(pieData)
            .enter()
            .append('text')
            .attr('class', 'pieValue')
            .attr('x', w * 0.5)
            .attr('y', function(d, i) { return i * 30 - h * 0.4 })
            .attr('text-anchor', 'end')
            .attr('font-size', '20px')
            .attr('font-weight', '400')
            .style('font-family', "Microsoft JhengHei")
            .attr('fill', function(d, i) {
                return color(i)
            })
            .text(function(d, i) {
                return d.click
            })

    }

}   