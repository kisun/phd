@extends('layouts.app')

@section('content')
  <div class="page page-test">
    <button class="btn btn-warning">Resample</button>
    <div id="graph"></div>
  </div>
@endsection

@section('endmatter')
  <script src="https://d3js.org/d3.v4.min.js"></script>
  <script>
    var width = 500,
        height = 300,
        N = 10000;

    var svg = d3.select("#graph").append("svg")
        .attr("width", width)
        .attr("height", height)
        .append("g");

    var data = d3.range(N).map(function() { return d3.randomNormal(0, 2)(); });

    var x = d3.scaleLinear()
        .domain([-5, 5])
        .range([0, width]);

    var y = d3.scaleLinear()
        .domain([0, .2])
        .range([height, 0]);

    var line = d3.line()
        .x(function(d) { return x(d[0]); })
        .y(function(d) { return y(d[1]); });

    var kde = kernelDensityEstimator(epanechnikovKernel(7), x.ticks(100));

    svg.append("path")
        .datum(kde(data))
        .attr("class", "line")
        .attr("d", line);

    function kernelDensityEstimator(kernel, x) {
      return function(sample) {
        return x.map(function(x) {
          return [x, d3.mean(sample, function(v) { return kernel(x - v); })];
        });
      }
    }

    function epanechnikovKernel(scale) {
      return function(u) {
        return Math.abs(u /= scale) <= 1 ? .75 * (1 - u * u) / scale : 0;
      };
    }
  </script>
@endsection
