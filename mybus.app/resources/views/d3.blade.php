@extends('layouts.app')

@section('content')
  <div class="page page-test">
    <div id="graph"></div>
    <button class="btn btn-warning">Resample</button>
  </div>
@endsection

@section('endmatter')
  <script src="https://d3js.org/d3.v4.min.js"></script>
  <script>
    var data = d3.range(100).map(d3.randomBates(10));

    // var formatCount = d3.format(",.0f");

    var margin = {top: 10, right: 30, bottom: 30, left: 30},
        width = 400,
        height = 200;

    var x = d3.scaleLinear().rangeRound([0, width]);
    var bins = d3.histogram()
                 .domain(x.domain())
                 .thresholds(x.ticks(20))
                 (data);

    var y = d3.scaleLinear()
              .domain([0, d3.max(bins, function(d) { return d.length; })])
              .range([height, 0]);

    var svg = d3.select('#graph').append('svg')
                .attr("width", width + margin.left + margin.right)
                .attr("height", height + margin.top + margin.bottom)
                .append("g")
                  .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var bar = svg.selectAll(".bar")
                 .data(bins)
                 .enter().append("g")
                   .attr("class", "bar")
                   .attr("transform", function(d) {
                     return "translate(" + x(d.x0) + "," + y(d.length) + ")";
                   });
    bar.append("rect")
       .attr("x", 1)
       .attr("width", x(bins[0].x1) - x(bins[0].x0) - 1)
       .attr("height", function(d) { return height - y(d.length); });

    svg.append("g")
       .attr("class", "axis axis--x")
       .attr("transform", "translate(0, " + height + ")")
       .call(d3.axisBottom(x));

    $(".page-test button").on('click', function() {
      var newdata = d3.range(100).map(d3.randomBates(10));
      var newbins = d3.histogram()
                   .domain(x.domain())
                   .thresholds(x.ticks(20))
                   (newdata);
      var newy = d3.scaleLinear()
                .domain([0, d3.max(newbins, function(d) { return d.length; })])
                .range([height, 0]);
      console.log(bar);
      bar.data(newdata).attr("height", function(d) {return height - newy(d.length); });
    });
  </script>
@endsection
