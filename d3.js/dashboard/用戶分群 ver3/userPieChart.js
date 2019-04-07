function userPieChart(data){

    var temp = {}
    temp.name = "既有用戶"
    temp.people = Object.values(data[0])[0]
    var temp2 = {}
    temp2.name = "新用戶"
    temp2.people = Object.values(data[0])[1]
    var data = []
    data.push(temp,temp2)
    console.log(data)

    var pieChartDiv = d3.select('#pieChartDiv')
    pieChartDiv.append('section')
                .style('margin-top', '10%')

    var w = 1400
    var h = 700
    var outerR = Math.min(w, h) / 2.1
    var innerR = outerR * 0.7

    var color = d3.scaleOrdinal(["#FDB815","#F8941D", "#F01B26", "#D75729", "#a6d854", "#ffd92f"])

    var pie = d3.pie()
                .value(function(d, i) {
                    return d.people
                })

    var arc = d3.arc()
                .innerRadius(innerR)
                .outerRadius(outerR)

    var svg = pieChartDiv.select('section')
                        .append('svg')
                        .attr('width', w)
                        .attr('height', h)
                        .append('g')
                        .attr('transform', `translate(${w / 2}, ${h / 2})`)

    svg.selectAll('path')
        .data(pie(data))
        .enter()
        .append('path')
        .attr('fill', function(d, i) { return color(i) })
        .attr('d', arc)
        .attr('stroke', '#ececec')
        .attr('stroke-width', '0px')
        .attr("transform", "translate(" + 0 + ", " + 0 + ")")
        .on("mouseover",function(d,i){
            d3.select(this)
                .transition()
                .attr("d",d3.arc().innerRadius(innerR).outerRadius(outerR * 1.05))

            svg.select("#percentage2")
                .transition()
                .attr("fill",color(i))
                .tween("text",  function() {
                    var change = d3.interpolateRound(0, data[i].people)
                        return function(t) {this.textContent = change(t) + "人"} 
                })

            svg.select("#name2")
                .text(function(){return data[i].name})
                .attr("fill",color(i))
        }) 
        .on("mouseout",function(d,i){
            d3.select(this)
            .transition()
            .attr("d",d3.arc().innerRadius(innerR).outerRadius(outerR))
        })
        .each(function(d, i) { this._current = d })

    svg.append("text").attr("id","percentage2")
                        .text(0 + "人")
                        .attr('font-weight', '600')
                        .attr('text-anchor', 'middle')
                        .attr("transform", "translate(" + 5 + ", " + 5 + ")")
                        .style('font-size', '80px')
                        .style('font-family', "Microsoft JhengHei")

    svg.append("text").attr("id","name2")
                        .text("")
                        .attr('font-weight', '600')
                        .attr('text-anchor', 'middle')
                        .attr("transform", "translate(" + 5 + ", " + 5 + ")")
                        .attr("dy","60px")                                    
                        .style('font-size', '40px')
                        .style('font-family', "Microsoft JhengHei")


    svg.selectAll("#text2")
        .data(pie(data))
        .enter()
        .append("text")
        .attr("id","text2")
        .attr("transform", function(d) {
            console.log(pie(data))
            return "translate(" + arc.centroid(d) + ")";
        })
        .attr('font-size', '40px')
        .attr('font-weight', '400')
        .attr("text-anchor", "middle")
        .attr("fill", "white")
        .attr("dy", ".35em")
        .text(function(d,i){
            return Math.round(data[i].people * 100 / (data[0].people + data[1].people)) + "%"
        })
        .style('font-family', "Microsoft JhengHei")
        .on("mouseover",function(d,i){
            svg.select("#percentage2")
                .transition()
                .attr("fill",color(i))
                .tween("text",  function() {
                    var change = d3.interpolateRound(0, data[i].people)
                        return function(t) {this.textContent = change(t) + "人"} 
                })

            svg.select("#name2")
                .text(function(){return data[i].name})
                .attr("fill",color(i))
        }) 
        .on("mouseout",function(d,i){
            d3.select(this)
            .transition()
            .attr("d",d3.arc().innerRadius(innerR).outerRadius(outerR))
        })
        .each(function(d, i) { this._current = d })
          
}
