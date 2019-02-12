function drawSunburst(divId, useSize) {
  const arcRadius = 100;
  const layers = 4;
  const height = width = arcRadius * layers * 2;
  const maxLabelChars = 20;

  function arcVisible(arc) {
    return arc.y1 <= layers && arc.y0 >= 1 && arc.x1 > arc.x0;
  }

  function labelVisible(arc) {
    return arcVisible(arc) && (arc.y1 - arc.y0) * (arc.x1 - arc.x0) > 0.03;
  }

  function labelTransform(arc) {
    const x = (arc.x0 + arc.x1) / 2 * 180 / Math.PI;
    const y = (arc.y0 + arc.y1) / 2 * arcRadius;
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

  function title(node) {
    if (node.parent === null) {
      return (
        `${node.data.name} (${prettySize(node.data.size)})\n` +
        `closure: ${prettySize(node.data.closureSize)}\n\n`
      );
    }
    return (
      `${node.parent.data.name} depends on ${node.data.name}\n\n` +
      `size: ${prettySize(node.data.size)}\n` +
      `closure: ${prettySize(node.data.closureSize)}\n\n` +
      `${node.data.why.map(prettyWhy).join()}`
    );
  }

  function truncate(str) {
    if (str.length < maxLabelChars) {
      return str;
    }
    return str.substr(0, maxLabelChars - 3) + '...';
  }

  // Compute closure size relative to siblings
  function assignSize(node, sumOfSiblings) {
    const parentValue = node.parent ? node.parent.value : 1;
    node.value = node.data.closureSize / sumOfSiblings * parentValue;
    if (node.children) {
      const sumOfChildren = node.children.reduce((total, n) => total + n.data.closureSize, 0);
      node.children.forEach(n => assignSize(n, sumOfChildren));
    }
  }

  const partition = data => {
    // Preprocess
    const root = d3.hierarchy(data);
    if (useSize) {
      assignSize(root, root.data.closureSize);
    } else {
      root.count();
    }
    root.sort((a, b) => b.value - a.value);
    // Layout
    return d3.partition()
      .size([2 * Math.PI, root.height + 1])
      (root);
  };

  const arc = d3.arc()
    .startAngle(d => d.x0)
    .endAngle(d => d.x1)
    .padAngle(d => Math.min((d.x1 - d.x0) / 2, 0.01))
    .padRadius(arcRadius * 1.5)
    .innerRadius(d => d.y0 * arcRadius)
    .outerRadius(d => d.y1 * arcRadius - 2);

  function draw(data, svg) {
    const root = partition(data);
    root.each(d => d.current = d);

    const color = d3.scaleOrdinal().range(d3.quantize(d3.interpolateRainbow, data.children.length + 1));

    const g = svg
      .append("g")
        .attr("transform", `translate(${width / 2},${width / 2})`);

    const path = g
      .append("g")
        .selectAll("path")
        .data(root.descendants())
        .enter()
          .append("path")
            .attr("fill",
                  d => color(d.data.name))
            .attr("fill-opacity",
                  d => arcVisible(d.current)
                        ? (d.children ? 0.6 : 0.4)
                        : 0)
            .attr("d", d => arc(d.current));

    // Text that appears on hover
    path
      .append("title")
        .style("white-space", "nowrap")
        .text(title);

    path
      .filter(d => d.children)
        .style("cursor", "pointer")
        .on("click", clicked);

    const label = g
      .append("g")
        .attr("pointer-events", "none")
        .attr("text-anchor", "middle")
        .style("user-select", "none")
        .selectAll("text")
        .data(root.descendants())
        .enter()
          .append("text")
            .attr("dy", "0.35em")
            .attr("fill-opacity", d => +labelVisible(d.current))
            .attr("transform", d => labelTransform(d.current))
            .text(d => truncate(d.data.name));

    const parent = g
      .append("circle")
        .datum(root)
        .attr("r", arcRadius)
        .attr("fill", "none")
        .attr("pointer-events", "all")
        .style("cursor", "pointer")
        .on("click", clicked);
    const parentTitle = parent
      .append("title")
        .text(title(root));

    const parentLabel = g
      .append("text")
        .attr("dy", "0.35em")
        .attr("text-anchor", "middle")
        .attr("pointer-events", "none")
        .style("user-select", "none")
        .text(truncate(root.data.name));

    function clicked(p) {
      parent.datum(p.parent || root);
      parentLabel.text(truncate(p.data.name));
      parentTitle.text(title(p));

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
                  })
           .transition(t)
           .attr("fill-opacity", d => +labelVisible(d.target))
           .attrTween("transform", d => () => labelTransform(d.current));
    }
  }

  // Nuke previous contents
  var vis = d3.select("#" + divId);
  vis.selectAll("*").remove();

  var data = window.sessionStorage.getItem("data");
  if (data === null) {
    return;
  }

  const svg = vis.append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("preserveAspectRatio", "xMinYMid meet");

  try {
    draw(JSON.parse(data), svg);
  } catch (err) {
    console.log(err)
  }
}
