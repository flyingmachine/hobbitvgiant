window.Charts =
  randomFunction: (variation) ->
    () ->
      d3.random.normal(0, variation)