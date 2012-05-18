RealtimeLine = {}

random = d3.random.normal(0, .2)

chart = (domain, interpolation, tick, valueFunction) ->
  data = d3.range(domain[1] + 1).map(valueFunction)
  
  margin =
    top: 6
    right: 0
    bottom: 6
    left: 40

  width = 960 - margin.right
  height = 120 - margin.top - margin.bottom

  x = d3.scale.linear().domain(domain).range([ 0, width ])
  y = d3.scale.linear().domain([ -1, 1 ]).range([ height, 0 ])

  line = d3.svg.line().interpolate(interpolation).x((d, i) ->
    x i
  ).y((d, i) ->
    y d
  )
  svg = d3.select("body")
    .append("p")
    .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
    
  svg.append("defs")
    .append("clipPath")
    .attr("id", "clip")
    .append("rect")
    .attr("width", width)
    .attr("height", height)
    
  svg.append("g")
    .attr("class", "y axis")
    .call(d3.svg.axis().scale(y).ticks(5).orient("left"))
    
  path = svg.append("g")
    .attr("clip-path", "url(#clip)")
    .append("path")
    .data([ data ])
    .attr("class", "line")
    .attr("d", line)
    
  tick path, line, data, x, valueFunction

tick = (path, line, data, x, valueFunction) ->
  data.push valueFunction()
  path.attr("d", line)
    .attr("transform", null)
    .transition()
    .duration(300)
    .ease("linear")
    .attr("transform", "translate(" + x(-1) + ")")
    .each("end", () -> tick(path, line, data, x, valueFunction))
  data.shift()

RealtimeLine.chart = chart
RealtimeLine.tick = tick

window.RealtimeLine = RealtimeLine