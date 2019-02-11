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

  function title(d) {
    if (d.parent === null) {
      return (
        `${d.data.name} (${prettySize(d.data.size)})\n` +
        `closure: ${prettySize(closureSize(d.data))}\n\n`
      );
    }
    return (
      `${d.parent.data.name} depends on ${d.data.name}\n\n` +
      `size: ${prettySize(d.data.size)}\n` +
      `closure: ${prettySize(closureSize(d.data))}\n\n` +
      `${d.data.why.map(prettyWhy).join()}`
    );
  }

  function truncate(str) {
    if (str.length < maxLabelChars) {
      return str;
    }
    return str.substr(0, maxLabelChars - 3) + '...';
  }

  const closureSize = d => {
    return d.size + d.children.reduce((a, b) => a + closureSize(b), 0);
  };

  // Given a node, compute:
  // 0. Its subtree size.
  // 1. Its closure size.
  // ..using a postorder traversal.
  function preprocess(node) {
    if (node.children) {
      node.subtreeSize = 0;
    } else {
      node.subtreeSize = 1;
    }
    node.closureSize = node.data.size;
    if (node.children) {
      node.subtreeSize += node.children.reduce((total, n) => total + n.subtreeSize, 0);
      node.closureSize += node.children.reduce((total, n) => total + n.closureSize, 0);
    }
  }

  // Given a node, either use its subtree size
  // or compute closure size, relative to siblings,
  // using a preorder traversal.
  function assignValues(node) {
    if (!useSize) {
      node.value = node.subtreeSize;
      return;
    }

    // Compute size, relative to siblings
    if (node.depth == 0) {
      node.value = 1;
      return;
    }

    const siblingClosureSize = node.parent.closureSize - node.parent.data.size;
    node.value = (node.closureSize / siblingClosureSize) * node.parent.value;
  }

  const partition = data => {
    // Preprocess
    const root = d3.hierarchy(data)
      .eachAfter(preprocess)
      .eachBefore(assignValues)
      .sort((a, b) => b.value - a.value);
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
                  d => {
                    // Color arcs based on parent position
                    while (d.depth > 1) d = d.parent;
                    return color(d.data.name);
                  })
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
