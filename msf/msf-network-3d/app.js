const ELEMENTS = {
  graph: document.getElementById("graph"),
  stats: document.getElementById("networkStats"),
  detailTitle: document.getElementById("detailTitle"),
  detailBody: document.getElementById("detailBody"),
  detailMeta: document.getElementById("detailMeta"),
  searchInput: document.getElementById("searchInput"),
  edgeDistance: document.getElementById("edgeDistance"),
  chargeStrength: document.getElementById("chargeStrength"),
  toggleTeam: document.getElementById("toggleTeam"),
  toggleSynergy: document.getElementById("toggleSynergy"),
  toggleInferred: document.getElementById("toggleInferred"),
  resetView: document.getElementById("resetView")
};

const SOURCE_COLORS = {
  team: "#63d4ff",
  synergy: "#ff8f69",
  inferred: "#b89fff"
};

const GRAPH_BG = "#060d17";
let graph;
let rawData;
let activeNodeId = null;
let neighborSet = new Set();

function parseCsv(url) {
  return new Promise((resolve, reject) => {
    Papa.parse(url, {
      header: true,
      download: true,
      skipEmptyLines: true,
      complete: ({ data }) => resolve(data),
      error: reject
    });
  });
}

function tidyValue(value) {
  return (value || "").toString().trim();
}

function prettyName(id) {
  const withSpaces = id
    .replace(/_/g, " ")
    .replace(/([a-z])([A-Z])/g, "$1 $2")
    .replace(/([A-Z]+)([A-Z][a-z])/g, "$1 $2")
    .replace(/\s+/g, " ")
    .trim();
  return withSpaces || id;
}

function hashColor(text) {
  let h = 0;
  for (let i = 0; i < text.length; i += 1) h = (h * 31 + text.charCodeAt(i)) % 360;
  return `hsl(${h}, 70%, 56%)`;
}

function addEdge(edgeMap, source, target, edgeType, group) {
  if (!source || !target || source === target) return;
  const [a, b] = [source, target].sort();
  const key = `${a}::${b}`;
  if (!edgeMap.has(key)) {
    edgeMap.set(key, {
      source: a,
      target: b,
      types: new Set(),
      groups: new Set()
    });
  }
  const edge = edgeMap.get(key);
  edge.types.add(edgeType);
  if (group) edge.groups.add(group);
}

function buildGraph(characters, memberships, pairSynergies) {
  const existingCharacters = new Set(
    characters.map((r) => tidyValue(r.character)).filter(Boolean)
  );

  const traitColumns = Object.keys(characters[0] || {}).filter(
    (name) => !["ID", "character", "real"].includes(name)
  );

  const nodes = characters
    .map((row) => {
      const id = tidyValue(row.character);
      if (!id) return null;
      const traits = traitColumns.filter((col) => tidyValue(row[col]));
      return {
        id,
        name: prettyName(id),
        real: tidyValue(row.real),
        traits,
        teams: []
      };
    })
    .filter(Boolean);

  const byId = new Map(nodes.map((n) => [n.id, n]));

  const groupMembers = new Map();
  memberships.forEach((row) => {
    const character = tidyValue(row.character);
    const group = tidyValue(row.group);
    if (!character || !group || !existingCharacters.has(character)) return;
    if (!groupMembers.has(group)) groupMembers.set(group, []);
    const bucket = groupMembers.get(group);
    if (!bucket.includes(character)) bucket.push(character);
    byId.get(character).teams.push(group);
  });

  const inferredMembers = new Map();
  const inferredPrefixes = ["Aim", "Hydra", "Kree", "Hand", "Shield", "Merc", "Ravager"];
  nodes.forEach((n) => {
    const m = n.id.match(/^([A-Za-z]+)_/);
    if (!m) return;
    const prefix = m[1];
    if (!inferredPrefixes.includes(prefix)) return;
    const group = `${prefix} Minions`;
    if (!inferredMembers.has(group)) inferredMembers.set(group, []);
    inferredMembers.get(group).push(n.id);
  });

  const edgeMap = new Map();

  groupMembers.forEach((members, group) => {
    for (let i = 0; i < members.length; i += 1) {
      for (let j = i + 1; j < members.length; j += 1) {
        addEdge(edgeMap, members[i], members[j], "team", group);
      }
    }
  });

  inferredMembers.forEach((members, group) => {
    for (let i = 0; i < members.length; i += 1) {
      for (let j = i + 1; j < members.length; j += 1) {
        addEdge(edgeMap, members[i], members[j], "inferred", group);
      }
    }
  });

  pairSynergies.forEach((row) => {
    const source = tidyValue(row.source);
    const target = tidyValue(row.target);
    const group = tidyValue(row.group) || "Pair Synergy";
    if (!existingCharacters.has(source) || !existingCharacters.has(target)) return;
    addEdge(edgeMap, source, target, "synergy", group);
  });

  const links = Array.from(edgeMap.values()).map((edge) => {
    const types = Array.from(edge.types);
    return {
      source: edge.source,
      target: edge.target,
      types,
      groups: Array.from(edge.groups),
      primaryType: types.includes("synergy") ? "synergy" : types[0] || "team"
    };
  });

  const degree = new Map(nodes.map((n) => [n.id, 0]));
  links.forEach((l) => {
    degree.set(l.source, degree.get(l.source) + 1);
    degree.set(l.target, degree.get(l.target) + 1);
  });

  nodes.forEach((n) => {
    n.degree = degree.get(n.id) || 0;
    n.color = n.teams.length ? hashColor(n.teams[0]) : "#8ca0b8";
  });

  return { nodes, links };
}

function filteredLinks(links) {
  const activeTypes = [];
  if (ELEMENTS.toggleTeam.checked) activeTypes.push("team");
  if (ELEMENTS.toggleSynergy.checked) activeTypes.push("synergy");
  if (ELEMENTS.toggleInferred.checked) activeTypes.push("inferred");

  if (activeTypes.length === 0) return [];

  return links.filter((link) => {
    return link.types.some((type) => activeTypes.includes(type));
  });
}

function getLinkSourceId(link) {
  return typeof link.source === "object" ? link.source.id : link.source;
}

function getLinkTargetId(link) {
  return typeof link.target === "object" ? link.target.id : link.target;
}

function recalcNeighborhood() {
  neighborSet = new Set();
  if (!activeNodeId || !rawData) return;
  neighborSet.add(activeNodeId);
  filteredLinks(rawData.links).forEach((l) => {
    const sourceId = getLinkSourceId(l);
    const targetId = getLinkTargetId(l);
    if (sourceId === activeNodeId) neighborSet.add(targetId);
    if (targetId === activeNodeId) neighborSet.add(sourceId);
  });
}

function currentGraphData() {
  const links = filteredLinks(rawData.links);
  return { nodes: rawData.nodes, links };
}

function linkColor(link) {
  if (link.types.includes("synergy") && ELEMENTS.toggleSynergy.checked) return SOURCE_COLORS.synergy;
  if (link.types.includes("team") && ELEMENTS.toggleTeam.checked) return SOURCE_COLORS.team;
  if (link.types.includes("inferred") && ELEMENTS.toggleInferred.checked) return SOURCE_COLORS.inferred;
  return "#5c6d80";
}

function renderStats() {
  const links = filteredLinks(rawData.links);
  const teams = new Set();
  rawData.nodes.forEach((n) => n.teams.forEach((t) => teams.add(t)));
  ELEMENTS.stats.textContent = `${rawData.nodes.length} nodes • ${links.length} links • ${teams.size} groups`;
}

function updateDetails(nodeId) {
  if (!nodeId) {
    ELEMENTS.detailTitle.textContent = "Select A Character";
    ELEMENTS.detailBody.textContent = "Click a node to inspect teams, pair synergies, and direct links.";
    ELEMENTS.detailMeta.innerHTML = `
      <div class="key"><span class="swatch" style="background:${SOURCE_COLORS.team}"></span> Team links</div>
      <div class="key"><span class="swatch" style="background:${SOURCE_COLORS.synergy}"></span> Pair synergies</div>
      <div class="key"><span class="swatch" style="background:${SOURCE_COLORS.inferred}"></span> Inferred minion links</div>
    `;
    return;
  }

  const node = rawData.nodes.find((n) => n.id === nodeId);
  const links = filteredLinks(rawData.links).filter(
    (l) => getLinkSourceId(l) === nodeId || getLinkTargetId(l) === nodeId
  );
  const connected = links
    .map((l) =>
      getLinkSourceId(l) === nodeId ? getLinkTargetId(l) : getLinkSourceId(l)
    )
    .map((id) => prettyName(id));

  ELEMENTS.detailTitle.textContent = node.name;
  ELEMENTS.detailBody.textContent = `${node.degree} total links across current sources.`;

  const teamList = node.teams.length
    ? node.teams.slice().sort().join("<br>")
    : "No curated team tags yet.";
  const connectedList = connected.length
    ? connected.slice(0, 18).join(", ") + (connected.length > 18 ? "..." : "")
    : "No links with active filters.";

  ELEMENTS.detailMeta.innerHTML = `
    <p><strong>Teams/Groups</strong><br>${teamList}</p>
    <p><strong>Connected Characters</strong><br>${connectedList}</p>
  `;
}

function refreshGraph() {
  recalcNeighborhood();
  const data = currentGraphData();
  graph.graphData(data);
  graph.nodeColor((n) => {
    if (!activeNodeId) return n.color;
    return neighborSet.has(n.id) ? n.color : "#35455a";
  });
  graph.linkColor(linkColor);
  graph.linkWidth((l) => {
    if (!activeNodeId) return l.types.includes("synergy") ? 1.8 : 1.05;
    return getLinkSourceId(l) === activeNodeId || getLinkTargetId(l) === activeNodeId
      ? 2.6
      : 0.28;
  });
  renderStats();
  updateDetails(activeNodeId);
}

function focusNode(node) {
  const distance = 120;
  const scale = 1 + distance / Math.hypot(node.x || 1, node.y || 1, node.z || 1);
  const pos = {
    x: (node.x || 0) * scale,
    y: (node.y || 0) * scale,
    z: (node.z || 0) * scale
  };
  graph.cameraPosition(pos, node, 1200);
}

function wireControls() {
  [ELEMENTS.toggleTeam, ELEMENTS.toggleSynergy, ELEMENTS.toggleInferred].forEach((el) => {
    el.addEventListener("change", () => refreshGraph());
  });

  ELEMENTS.edgeDistance.addEventListener("input", () => {
    graph.d3Force("link").distance(Number(ELEMENTS.edgeDistance.value));
    graph.numDimensions(3);
  });

  ELEMENTS.chargeStrength.addEventListener("input", () => {
    graph.d3Force("charge").strength(Number(ELEMENTS.chargeStrength.value));
    graph.numDimensions(3);
  });

  ELEMENTS.searchInput.addEventListener("input", () => {
    const q = ELEMENTS.searchInput.value.toLowerCase().trim();
    if (!q) {
      activeNodeId = null;
      refreshGraph();
      return;
    }
    const match = rawData.nodes.find(
      (n) => n.id.toLowerCase().includes(q) || n.name.toLowerCase().includes(q)
    );
    if (!match) return;
    activeNodeId = match.id;
    recalcNeighborhood();
    refreshGraph();
    focusNode(match);
  });

  ELEMENTS.resetView.addEventListener("click", () => {
    activeNodeId = null;
    refreshGraph();
    graph.cameraPosition({ x: 0, y: 0, z: 300 }, { x: 0, y: 0, z: 0 }, 900);
  });
}

async function init() {
  try {
    const [characters, memberships, synergies] = await Promise.all([
      parseCsv("../characters.csv"),
      parseCsv("./data/team_memberships.csv"),
      parseCsv("./data/synergy_pairs.csv")
    ]);

    rawData = buildGraph(characters, memberships, synergies);

    graph = ForceGraph3D()(ELEMENTS.graph)
      .backgroundColor(GRAPH_BG)
      .nodeOpacity(0.95)
      .nodeLabel((n) => {
        const real = n.real ? ` (${n.real})` : "";
        return `${n.name}${real}<br/>Links: ${n.degree}`;
      })
      .nodeVal((n) => Math.max(2.1, Math.sqrt(n.degree + 1) * 1.5))
      .linkOpacity(0.48)
      .onNodeClick((node) => {
        activeNodeId = node.id;
        recalcNeighborhood();
        refreshGraph();
        focusNode(node);
      })
      .onBackgroundClick(() => {
        activeNodeId = null;
        refreshGraph();
      });

    graph.d3Force("link").distance(Number(ELEMENTS.edgeDistance.value));
    graph.d3Force("charge").strength(Number(ELEMENTS.chargeStrength.value));

    wireControls();
    refreshGraph();
    graph.cameraPosition({ x: 0, y: 0, z: 300 });
  } catch (error) {
    ELEMENTS.stats.textContent = "Failed to load data";
    ELEMENTS.detailTitle.textContent = "Data Loading Error";
    ELEMENTS.detailBody.textContent = String(error);
  }
}

init();
