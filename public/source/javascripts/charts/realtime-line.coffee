class RealtimeLine
  constructor: (options) ->
    {
      @range,
      @yscale,
      @interpolation,
      @valueFunction,
      @selector,
      view
    } = options
    @domain = [0, @range - 1]
    @setView view

  setMargin: (margin) ->
    @view.margin = margin ? {}
    @view.margin.top ?= 0
    @view.margin.right ?= 0
    @view.margin.bottom ?= 0
    @view.margin.left ?= 0

  setView: (view) ->
    @view = {}
    @view.width = view.width ? 0
    @view.height = view.height ? 0
    @setMargin(view.margin)

      
  draw: () ->
    data = d3.range(@range).map(@valueFunction)
    
    width = @view.width - @view.margin.right
    height = @view.height - @view.margin.top - @view.margin.bottom

    x = d3.scale.linear().domain(@domain).range([ 0, width ])
    y = d3.scale.linear().domain(@yscale).range([ height, 0 ])

    line = d3.svg.line().interpolate(@interpolation).x((d, i) ->
      x i
    ).y((d, i) ->
      y d
    )
    
    svg = d3.select(@selector)
      .append("p")
      .append("svg")
      .attr("width", width + @view.margin.left + @view.margin.right)
      .attr("height", height + @view.margin.top + @view.margin.bottom)
      .append("g")
      .attr("transform", "translate(" + @view.margin.left + "," + @view.margin.top + ")")
      
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
      
    @tick path, line, data, x

  tick: (path, line, data, x) ->
    data.push @valueFunction()
    path.attr("d", line)
      .attr("transform", null)
      .transition()
      .duration(300)
      .ease("linear")
      .attr("transform", "translate(" + x(-1) + ")")
      .each("end", () => @tick(path, line, data, x))
    data.shift()

window.RealtimeLine = RealtimeLine