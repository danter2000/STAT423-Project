let dataset, svg
let salarySizeScale, salaryXScale, categoryColorScale
let simulation, nodes
let categoryLegend, salaryLegend

const margin = { left: 170, top: 50, bottom: 50, right: 20 }
const width = 1000 - margin.left - margin.right
const height = 950 - margin.top - margin.bottom

d3.csv('https://raw.githubusercontent.com/danter2000/STAT423-Project/main/Data/vizdata/hofdistinct.csv', function (d) {
  return d
}).then(data => {
  dataset = data
  // console.log(dataset)
  setTimeout(drawInitial(), 100)
})

const x = d3.scaleLinear()
  .domain([1, 100])
  .range([1, 400]);

const y = d3.scaleLinear()
  .domain([1, 200])
  .range([1, 800]);

function drawInitial() {
  let svg = d3.select("#vis")
    .append('svg')
    .attr('width', 600)
    .attr('height', 600)
    .attr('margin-right', 'auto')
    .attr('margin-left', 'auto')
    .attr('opacity', 1)

  svg.append('g')
    .selectAll("dot")
    .data(dataset)
    .enter()
    .append("circle")
    .attr('fill', 'none')
    .attr('stroke', 'black')
    .attr("cx", function (d) { return x(d.x); })
    .attr("cy", function (d) { return y(d.y); })
    .attr("r", 1)
}

function draw1() {
  svg = d3.select("#vis").select("svg")

  svg.selectAll("circle")
    .transition()
    .duration(1000)
    .attr("opacity", 1)
}

function draw2() {
  svg = d3.select("#vis").select("svg")

  svg.selectAll("circle")
    .transition()
    .duration(1000)
    .attr("opacity", (d) => {
      if (d.eligible == 1) {
        return 1;
      } else {
        return 0.2;
      }
    })
}

function draw3() {
  svg = d3.select("#vis").select("svg")

  svg.selectAll("circle")
    .transition()
    .duration(1000)
    .attr("opacity", (d) => {
      if (d.inducted == 1) {
        return 1;
      } else if (d.inducted == 0 && d.eligible == 1) {
        return 0.5;
      } else {
        return 0.2
      }
    })
}

function draw4() {
  svg = d3.select("#vis").select("svg")



}

let activationFunctions = [
  draw1,
  draw2,
  draw3,
  draw4
]

//All the scrolling function
//Will draw a new graph based on the index provided by the scroll


let scroll = scroller()
  .container(d3.select('#graphic'))
scroll()

let lastIndex, activeIndex = 0

scroll.on('active', function (index) {
  d3.selectAll('.step')
    .transition().duration(500)
    .style('opacity', function (d, i) { return i === index ? 1 : 0.1; });

  activeIndex = index
  let sign = (activeIndex - lastIndex) < 0 ? -1 : 1;
  let scrolledSections = d3.range(lastIndex + sign, activeIndex + sign, sign);
  scrolledSections.forEach(i => {
    activationFunctions[i]();
  })
  lastIndex = activeIndex;

})

scroll.on('progress', function (index, progress) {
  if (index == 2 & progress > 0.7) {

  }
})