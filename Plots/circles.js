const margin = { top: 10, right: 30, bottom: 30, left: 60 },
  width = 800 - margin.left - margin.right,
  height = 700 - margin.top - margin.bottom;

// append the svg object to the body of the page
const svg = d3.select("#circles1")
  .append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform", `translate(${margin.left}, ${margin.top})`);

d3.csv("https://raw.githubusercontent.com/danter2000/STAT423-Project/main/Data/vizdata/hofcircles.csv").then(function (data) {
  const x = d3.scaleLinear()
    .domain([1, 80])
    .range([0, width]);

  const y = d3.scaleLinear()
    .domain([1, 250])
    .range([height, 0]);

  svg.append('g')
    .selectAll("dot")
    .data(data)
    .enter()
    .append("circle")
    .attr("cx", function (d) { return x(d.x); })
    .attr("cy", function (d) { return y(d.y); })
    .attr("r", 1)
});