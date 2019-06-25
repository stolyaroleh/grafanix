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
    `"${why.reason}"`
  );
}

function truncate(str) {
  if (str.length < maxLabelChars) {
    return str;
  }
  return str.substr(0, maxLabelChars - 3) + '...';
}

function nodeTitle(d) {
  return (
    `${d.name} (${prettySize(d.size)})\n` +
    `closure: ${prettySize(d.closureSize)}\n`
  );
}

function linkTitle(d) {
  return (
    `${d.source.name} depends on ${d.target.name}\n\n` +
    `${d.why.map(prettyWhy).join('\n')}`
  );
}

function connectedness(edges) {
  var numIncoming = {};
  var numOutgoing = {};
  edges.forEach(e => {
    numIncoming[e.target] = (numIncoming[e.target] || 0) + 1;
    numOutgoing[e.source] = (numOutgoing[e.source] || 0) + 1;
  });
  return [numIncoming, numOutgoing];
}

function drawGraph(data) {
  // Nuke previous contents
  var vis = d3.select("#vis");
  vis.selectAll("*").remove();

  if (data === null) {
    return;
  }

  try {
    draw(JSON.parse(data));
  } catch (err) {
    console.log(err)
  }

  function draw(data) {
    const width = 1000;
    const height = 1000;
    const svg = vis.append('svg')
      .attr('viewBox', `${-width / 2} ${-height / 2} ${width} ${height}`);

    const zoomRect = svg
      .append('rect')
      .attr("fill", "none")
      .attr("pointer-events", "all")
      .attr('x', -5000)
      .attr('y', -5000)
      .attr('width', 10000)
      .attr('height', 10000)
      .call(
        d3.zoom()
          .on('zoom', zoomed)
      );

    const visParent = svg.append('g');

    [incoming, outgoing] = connectedness(data.links);
    const totalSize = data.nodes.reduce((total, n) => total + n.size, 0);
    const radius = d3.scaleSqrt().domain([0, totalSize]).range([10, 50]);
    const linkDistance = d3.scaleSqrt().clamp(true).domain([2, 10]).range([70, 200]);
    const collideRadius = d3.scaleSqrt().clamp(true).domain([2, 10]).range([20, 70]);

    const color = d3
      .scaleOrdinal()
      .range(d3.quantize(d3.interpolateRainbow, data.nodes.length + 1));

    const simulation = d3.forceSimulation(data.nodes)
      .force(
        'charge',
        d3.forceManyBody()
          .strength(-30)
      )
      .force(
        'link',
        d3.forceLink()
          .distance(link => {
            numOutgoing = outgoing[link.source.index];
            numIncoming = incoming[link.target.index];
            return linkDistance(numOutgoing + numIncoming);
          })
          .links(data.links)
      )
      .force(
        'collide',
        d3.forceCollide()
          .radius(d => {
            isRoot = d.sha == data.nodes[0].sha;
            numIncoming = incoming[d.index];
            numOutgoing = outgoing[d.index];
            return collideRadius(numIncoming + numOutgoing);
          })
      )
      .on('tick', ticked);

    const link = visParent.selectAll('.link').data(data.links);
    const line = link.enter()
      .append('line')
      .attr('class', 'link')
      .attr('stroke', d => color(d.source.name));

    const node = visParent.selectAll('.node').data(data.nodes);

    const g = node.enter()
      .append('g')
      .attr('class', 'node')
      .call(
        d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended)
      );

    const rect = g
      .append('rect')
      .attr('class', 'node_label_bg')
      .attr('fill', d => color(d.name))
      .attr('x', d => -0.2 * d.name.length * radius(d.size))
      .attr('y', d => -0.5 * radius(d.size))
      .attr('width', d => d.name.length * 0.4 * radius(d.size))
      .attr('height', d => radius(d.size))
      .on("click", clicked);

    // Text that appears on hover
    line
      .append('title')
      .style('white-space', 'nowrap')
      .text(linkTitle);
    rect
      .append('title')
      .style('white-space', 'nowrap')
      .text(nodeTitle);

    const label = g
      .append('text')
      .attr('class', 'node_label')
      .attr('font-size', d => `${0.5 * radius(d.size)}pt`)
      .attr('dy', '0.35em')
      .attr('pointer-events', 'none')
      .attr('fill',
        d => {
          const lab = d3.lab(color(d.name))
          return lab.l > 50 ? 'black' : 'white'
        }
      )
      .text(d => d.name);

    node.exit().remove()

    function ticked() {
      line.merge(link)
        .attr('x1', d => d.source.x)
        .attr('y1', d => d.source.y)
        .attr('x2', d => d.target.x)
        .attr('y2', d => d.target.y);
      g.merge(node)
        .attr(
          'transform',
          d => `translate(${d.x}, ${d.y})`
        );
    }

    function dragstarted(d) {
      if (!d3.event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }

    function dragged(d) {
      d.fx = d3.event.x;
      d.fy = d3.event.y;
    }

    function dragended(d) {
      if (!d3.event.active) simulation.alphaTarget(0);
    }

    function clicked(d) {
      if (d.fx == null) {
        d.fx = d.x;
        d.fy = d.y;
      } else {
        d.fx = null;
        d.fy = null;
      }
    }

    function zoomed() {
      visParent.attr(
        'transform',
        d3.event.transform
      );
    }
  }
}
