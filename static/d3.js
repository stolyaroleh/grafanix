function drawSunburst(divId) {
  const arcRadius = 100;
  const layers = 4;
  const height = width = arcRadius * layers * 2;
  const labelChars = 20;

  function arcVisible(d) {
    return d.y1 <= layers && d.y0 >= 1 && d.x1 > d.x0;
  }

  function labelVisible(d) {
    return arcVisible(d) && (d.y1 - d.y0) * (d.x1 - d.x0) > 0.03;
  }

  function labelTransform(d) {
    const x = (d.x0 + d.x1) / 2 * 180 / Math.PI;
    const y = (d.y0 + d.y1) / 2 * arcRadius;
    return `rotate(${x - 90}) translate(${y},0) rotate(${x < 180 ? 0 : 180})`;
  }

  function prettySize(bytes) {
    if (bytes == 0) {
      return '0 B';
    }
    var i = Math.floor(Math.log(bytes) / Math.log(1024));
    return (bytes / Math.pow(1024, i)).toFixed(2) * 1 + ' ' + ['B', 'kB', 'MB', 'GB', 'TB'][i];
  }

  function prettyWhy(why) {
    return (
      `${why.file} contains:\n` +
      `"${why.reason}"\n`
    );
  }

  function title(d) {
    if (d.parent === null) {
      return "";
    }
    return (
      `${d.parent.data.name} depends on:\n\n` +
      `${d.data.name} (${prettySize(d.data.size)})\n\n` +
      `${d.data.why.map(prettyWhy).join()}`
    );
  }

  function truncate(d) {
    const name = d.data.name;
    if (name.length < labelChars) {
      return name;
    }
    return name.substr(0, labelChars - 3) + '...';
  }

  const partition = data => {
    const root = d3.hierarchy(data)
      .sum(d => d.children.length ? 0 : 1)
      .sort((a, b) => b.value - a.value);
    return d3.partition()
      .size([2 * Math.PI, root.height + 1])
      (root);
  }

  const arc = d3.arc()
    .startAngle(d => d.x0)
    .endAngle(d => d.x1)
    .padAngle(d => Math.min((d.x1 - d.x0) / 2, 0.005))
    .padRadius(arcRadius * 1.5)
    .innerRadius(d => d.y0 * arcRadius)
    .outerRadius(d => Math.max(d.y0 * arcRadius, d.y1 * arcRadius - 1));

  function draw(data, svg) {
    const root = partition(data);
    root.each(d => d.current = d);

    const color = d3.scaleOrdinal().range(d3.quantize(d3.interpolateRainbow, data.children.length + 1));
    
    const g = svg.append("g")
      .attr("transform", `translate(${width / 2},${width / 2})`);
    
    const path = g.append("g")
      .selectAll("path")
      .data(root.descendants())
      .enter().append("path")
        .attr("fill", d => { while (d.depth > 1) d = d.parent; return color(d.data.name); })
        .attr("fill-opacity", d => arcVisible(d.current) ? (d.children ? 0.6 : 0.4) : 0)
        .attr("d", d => arc(d.current));
    
    path.append("title")
      .style("white-space", "nowrap")
      .text(title);
  
    path.filter(d => d.children)
        .style("cursor", "pointer")
        .on("click", clicked);
  
    const label = g.append("g")
        .attr("pointer-events", "none")
        .attr("text-anchor", "middle")
        .style("user-select", "none")
      .selectAll("text")
      .data(root.descendants())
      .enter().append("text")
        .attr("dy", "0.35em")
        .attr("fill-opacity", d => +labelVisible(d.current))
        .attr("transform", d => labelTransform(d.current))
        .text(truncate);
  
    const parent = g.append("circle")
        .datum(root)
        .attr("r", arcRadius)
        .attr("fill", "none")
        .attr("pointer-events", "all")
        .on("click", clicked);
    
    const parentLabel = g.append("text")
      .attr("dy", "0.35em")
      .attr("text-anchor", "middle")
      .style("user-select", "none")
      .text(truncate(root));

    function clicked(p) {
      parent.datum(p.parent || root);
      parentLabel.text(truncate(p));
  
      root.each(d => d.target = {
        x0: Math.max(0, Math.min(1, (d.x0 - p.x0) / (p.x1 - p.x0))) * 2 * Math.PI,
        x1: Math.max(0, Math.min(1, (d.x1 - p.x0) / (p.x1 - p.x0))) * 2 * Math.PI,
        y0: Math.max(0, d.y0 - p.depth),
        y1: Math.max(0, d.y1 - p.depth)
      });
  
      const t = g.transition().duration(750);
  
      // Transition the data on all arcs, even the ones that aren’t visible,
      // so that if this transition is interrupted, entering arcs will start
      // the next transition from the desired position.
      path.transition(t)
          .tween("data", d => {
            const i = d3.interpolate(d.current, d.target);
            return t => d.current = i(t);
          })
        .filter(function(d) {
          return +this.getAttribute("fill-opacity") || arcVisible(d.target);
        })
          .attr("fill-opacity", d => arcVisible(d.target) ? (d.children ? 0.6 : 0.4) : 0)
          .attrTween("d", d => () => arc(d.current));
  
      label.filter(function(d) {
          return +this.getAttribute("fill-opacity") || labelVisible(d.target);
        }).transition(t)
          .attr("fill-opacity", d => +labelVisible(d.target))
          .attrTween("transform", d => () => labelTransform(d.current));
    }
  }

  var vis = d3.select("#" + divId);
  vis.selectAll("*").remove();

  const svg = vis.append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("preserveAspectRatio", "xMidYMid meet");

  var data = window.sessionStorage.getItem("data");
  if (data === null) {
    return;
  }

  try {
    draw(JSON.parse(data), svg);
  } catch (err) {
    console.log(err)
  }
}
