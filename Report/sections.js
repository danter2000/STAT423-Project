let dataset, dataset2, svg, circleLabel
let salarySizeScale, salaryXScale, categoryColorScale
let simulation, nodes
let categoryLegend, salaryLegend

const margin = { left: 170, top: 50, bottom: 50, right: 20 }
const width = 1000 - margin.left - margin.right
const height = 950 - margin.top - margin.bottom

d3.csv('https://raw.githubusercontent.com/danter2000/STAT423-Project/main/Data/vizdata/circlestest.csv', function (d) {
  return d
}).then(data => {
  dataset = data
  // console.log(dataset)
  setTimeout(getData, 100)
})

function getData() {
  d3.csv('https://raw.githubusercontent.com/danter2000/STAT423-Project/main/Data/posplayerWAR.csv', function (d, i) {
    return {
      player: d[''],
      war: d.WAR,
      group: i % 3
    }
  }).then(data => {
    dataset2 = data
    setTimeout(drawInitial, 100)
  })
}

const x = d3.scaleLinear()
  .domain([1, 10])
  .range([300, 550]);

const y = d3.scaleLinear()
  .domain([1, 20])
  .range([150, 650]);

function drawInitial() {
  let svg = d3.select("#vis")
    .append('svg')
    .attr('width', 800)
    .attr('height', 800)
    .attr('margin-right', 'auto')
    .attr('margin-left', 'auto')
    .attr('opacity', 1)

  svg.append('g')
    .selectAll("dot")
    .data(dataset)
    .enter()
    .append("circle")
    .attr('stroke', 'black')
    .attr('fill', '#4b6f84')
    .attr('opacity', 0)
    .attr("cx", function (d) { return x(d.x); })
    .attr("cy", function (d) { return y(d.y); })
    .attr("r", 10)
    .attr("class", function (d) {
      return d.type + " gen"
    })

  svg.append('g')
    .selectAll("dot")
    .data(dataset2)
    .enter()
    .append("circle")
    .attr("fill", "#4b6f84")
    .attr('opacity', 0)
    .attr("cx", function (d) {
      if (d.group == 0) {
        return x(1);
      } else if (d.group == 1) {
        return x(2);
      } else {
        return x(3);
      }
    })
    .attr("cy", 150)
    .attr("r", 4)
    .attr("class", "player")


  svg.selectAll("circle.gen")
    .transition()
    .duration(100)
    .attr('opacity', 1)

  circleLabel = svg.append("g")

  circleLabel.append("text")
    .text("Each circle represents roughly 100 players")
    .attr("x", x(11))
    .attr("y", y(1) - 60)
    .call(wrap, 150)

  circleLabel.append("line")
    .style("stroke", "black")
    .attr("x1", x(10) + 10)
    .attr("y1", y(1) - 15)
    .attr("x2", x(11))
    .attr("y2", y(1) - 40)
}

function draw1() {
  svg = d3.select("#vis").select("svg")

  svg.selectAll("circle.gen")
    .transition()
    .duration(1000)
    .attr("opacity", 1)

  circleLabel
    .transition()
    .duration(1000)
    .attr("opacity", 1)


}

function draw2() {
  svg = d3.select("#vis").select("svg")

  svg.selectAll(".all")
    .transition("circle")
    .duration(1000)
    .attr("opacity", 0.2)

  circleLabel
    .transition("label")
    .duration(1000)
    .attr("opacity", 0)
}

function draw3() {
  svg = d3.select("#vis").select("svg")

  svg.selectAll(".eligible")
    .transition()
    .duration(1000)
    .attr("opacity", 0.5)
}

function draw4() {
  svg = d3.select("#vis").select("svg")

  svg.selectAll(".all")
    .transition()
    .duration(1000)
    .attr("opacity", 0)

    svg.selectAll(".eligible")
    .transition()
    .duration(1000)
    .attr("opacity", 0)
}

function draw5() {
  svg = d3.select("#vis").select("svg")

  svg.selectAll(".inducted")
    .transition()
    .duration(1000)
    .attr("r", 10)

  svg.selectAll(".player")
    .transition()
    .duration(1000)
    .attr("opacity", 0)


}

function draw6() {
  svg = d3.select("#vis").select("svg")

  svg.selectAll(".inducted")
    .transition()
    .duration(1000)
    .attr("r", 0)

  svg.selectAll(".player")
    .transition()
    .duration(1000)
    .attr("opacity", 1)

}

let activationFunctions = [
  draw1,
  draw2,
  draw3,
  draw4,
  draw5,
  draw6
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
    console.log(activationFunctions[i])
    activationFunctions[i]();
  })
  lastIndex = activeIndex;

})

scroll.on('progress', function (index, progress) {
  if (index == 2 & progress > 0.7) {

  }
})

function wrap(text, width) {
  text.each(function () {
    var text = d3.select(this),
      words = text.text().split(/\s+/).reverse(),
      word,
      line = [],
      lineNumber = 0,
      lineHeight = 1.1, // ems
      x = text.attr("x"),
      y = text.attr("y"),
      dy = 0, //parseFloat(text.attr("dy")),
      tspan = text.text(null)
        .append("tspan")
        .attr("x", x)
        .attr("y", y)
        .attr("dy", dy + "em");
    while (word = words.pop()) {
      line.push(word);
      tspan.text(line.join(" "));
      if (tspan.node().getComputedTextLength() > width) {
        line.pop();
        tspan.text(line.join(" "));
        line = [word];
        tspan = text.append("tspan")
          .attr("x", x)
          .attr("y", y)
          .attr("dy", ++lineNumber * lineHeight + dy + "em")
          .text(word);
      }
    }
  });
}