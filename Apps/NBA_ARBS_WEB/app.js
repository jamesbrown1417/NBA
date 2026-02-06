const PAGE_SIZE = 50;

const ui = {
  generatedAt: document.getElementById("generated-at"),
  refreshButton: document.getElementById("refresh-data"),
  refreshStatus: document.getElementById("refresh-status"),
  updatesNote: document.getElementById("updates-note"),
  datasetUpdates: document.getElementById("dataset-updates"),
  mainTabs: document.getElementById("main-tabs"),
  subTabs: document.getElementById("sub-tabs"),
  viewTitle: document.getElementById("view-title"),
  calculatorView: document.getElementById("calculator-view"),
  tableView: document.getElementById("table-view"),
  statusView: document.getElementById("status-view"),
  excludeSportsbetWrap: document.getElementById("exclude-sportsbet-wrap"),
  excludeSportsbet: document.getElementById("exclude-sportsbet"),
  multiLegMatchWrap: document.getElementById("multi-leg-match-wrap"),
  multiLegMatch: document.getElementById("multi-leg-match"),
  searchInput: document.getElementById("search-input"),
  tableMeta: document.getElementById("table-meta"),
  dataTable: document.getElementById("data-table"),
  pager: document.getElementById("pager"),
  odds1: document.getElementById("odds-1"),
  odds2: document.getElementById("odds-2"),
  stake1: document.getElementById("stake-1"),
  stake2: document.getElementById("calc-stake-2"),
  arbPct: document.getElementById("calc-arb"),
  profit: document.getElementById("calc-profit")
};

const state = {
  data: null,
  mainTab: "calculator",
  subTabs: {
    topDown: "all",
    arbs: "allArbs",
    middles: "allMiddles",
    multiLegs: "Dabble"
  },
  excludeSportsbetUnder: false,
  multiLegMatch: "All",
  refreshApiAvailable: false,
  search: "",
  sort: { key: null, dir: "asc" },
  page: 1
};

const mainTabConfig = [
  { id: "calculator", label: "Calculator" },
  { id: "topDown", label: "Top-Down" },
  { id: "arbs", label: "Arbs" },
  { id: "middles", label: "Middles" },
  { id: "multiLegs", label: "Multi Legs" }
];

const datasetLabels = {
  all_arbs: "all_arbs.rds",
  all_middles: "all_middles.rds",
  tab_miss_by_one: "tab_points_miss_by_one.rds",
  betright_miss_by_one: "betright_points_miss_by_one.rds",
  processed_odds: "processed_odds/*.rds"
};

function round2(value) {
  const num = Number(value);
  return Number.isFinite(num) ? Math.round(num * 100) / 100 : null;
}

function formatNumber(value) {
  if (value === null || value === undefined) {
    return "";
  }
  if (typeof value !== "number") {
    return String(value);
  }
  if (Number.isInteger(value)) {
    return String(value);
  }
  return value.toFixed(2).replace(/\.00$/, "").replace(/(\.\d)0$/, "$1");
}

function formatCurrency(value) {
  if (!Number.isFinite(value)) {
    return "-";
  }
  return `$${value.toFixed(2)}`;
}

function setRefreshStatus(message, isError = false) {
  ui.refreshStatus.textContent = message;
  ui.refreshStatus.classList.toggle("error", isError);
}

function updateRefreshButtonState(isBusy = false) {
  ui.refreshButton.disabled = isBusy || !state.refreshApiAvailable;
  if (isBusy) {
    ui.refreshButton.textContent = "Refreshing...";
    return;
  }
  ui.refreshButton.textContent = "Refresh Data";
}

function renderDatasetUpdates(datasetMeta = {}, generatedAt = null) {
  ui.datasetUpdates.innerHTML = "";

  const keys = Object.keys(datasetLabels);
  keys.forEach((key) => {
    const meta = datasetMeta[key] ?? {};
    const title = datasetLabels[key];
    const updatedAt = meta.file_mtime ?? meta.latest_file_mtime ?? "Unknown";
    const rows = Number.isFinite(Number(meta.rows)) ? Number(meta.rows) : null;
    const cols = Number.isFinite(Number(meta.cols)) ? Number(meta.cols) : null;
    const files = Number.isFinite(Number(meta.files)) ? Number(meta.files) : null;
    const source = meta.source ?? meta.source_dir ?? null;

    const item = document.createElement("li");
    item.className = "update-item";

    const heading = document.createElement("strong");
    heading.textContent = title;

    const updatedLine = document.createElement("span");
    updatedLine.textContent = `Updated: ${updatedAt}`;

    const shapeLine = document.createElement("span");
    const parts = [];
    if (rows !== null) {
      parts.push(`${rows} rows`);
    }
    if (cols !== null) {
      parts.push(`${cols} cols`);
    }
    if (files !== null) {
      parts.push(`${files} files`);
    }
    shapeLine.textContent = parts.length > 0 ? parts.join(" | ") : "Shape unavailable";

    const sourceLine = document.createElement("span");
    sourceLine.textContent = source ? `Source: ${source}` : "Source unavailable";

    item.appendChild(heading);
    item.appendChild(updatedLine);
    item.appendChild(shapeLine);
    item.appendChild(sourceLine);
    ui.datasetUpdates.appendChild(item);
  });

  ui.updatesNote.textContent = generatedAt
    ? `Dashboard JSON generated: ${generatedAt}`
    : "No generated dataset metadata found.";
}

function calculateArb() {
  const o1 = Number(ui.odds1.value);
  const s1 = Number(ui.stake1.value);
  const o2 = Number(ui.odds2.value);

  if (!Number.isFinite(o1) || !Number.isFinite(s1) || !Number.isFinite(o2) || o2 === 0) {
    ui.stake2.textContent = "-";
    ui.arbPct.textContent = "-";
    ui.profit.textContent = "-";
    return;
  }

  const stake2 = (s1 * o1) / o2;
  const arbPct = ((1 / o1) + (1 / o2) - 1) * -100;
  const profit = (s1 * o1) - (s1 + stake2);

  ui.stake2.textContent = formatCurrency(stake2);
  ui.arbPct.textContent = `${arbPct.toFixed(2)}%`;
  ui.profit.textContent = formatCurrency(profit);
}

function normalizeTopDownRows(rows) {
  return rows.map((row) => ({
    player: row.player_name,
    market: row.market_name,
    line: row.line,
    OP: row.over_price,
    OA: row.over_agency,
    UP: row.under_price,
    UA: row.under_agency,
    margin: round2(row.margin),
    match: row.match,
    player_team: row.player_team,
    opposition_team: row.opposition_team
  }));
}

function preprocessData(raw) {
  const allArbs = raw.all_arbs ?? [];
  const tabMissByOne = raw.tab_miss_by_one ?? [];
  const betrightMissByOne = raw.betright_miss_by_one ?? [];

  const processedOdds = (raw.processed_odds ?? [])
    .filter((row) => Number(row.empirical_prob_last_20) >= 0.9)
    .map((row) => ({
      match: row.match,
      player_name: row.player_name,
      market_name: row.market_name,
      line: row.line,
      over_price: row.over_price,
      agency: row.agency,
      empirical_prob_last_20: round2(Number(row.empirical_prob_last_20))
    }))
    .sort((a, b) => {
      const prob = Number(b.empirical_prob_last_20) - Number(a.empirical_prob_last_20);
      if (prob !== 0) {
        return prob;
      }
      return Number(b.over_price) - Number(a.over_price);
    });

  return {
    generatedAt: raw.generated_at ?? null,
    datasetMeta: raw.dataset_meta ?? {},
    allArbs,
    allMiddles: (raw.all_middles ?? []).map((row) => ({ ...row, margin: round2(row.margin) })),
    tabMissByOne,
    betrightMissByOne,
    allArbsTopDown: normalizeTopDownRows(allArbs),
    tabMissByOneTopDown: normalizeTopDownRows(tabMissByOne),
    betrightMissByOneTopDown: normalizeTopDownRows(betrightMissByOne),
    processedOdds
  };
}

function sportsbetRows(rows) {
  return rows.filter((row) => row.OA === "Sportsbet" || row.UA === "Sportsbet");
}

function applyExcludeSportsbetUnder(rows) {
  if (!state.excludeSportsbetUnder) {
    return rows;
  }
  return rows.filter((row) => row.UA !== "Sportsbet");
}

function sortByMatchThenMargin(rows) {
  return [...rows].sort((a, b) => {
    const matchCompare = String(a.match).localeCompare(String(b.match));
    if (matchCompare !== 0) {
      return matchCompare;
    }
    return Number(b.margin) - Number(a.margin);
  });
}

function topDownTabs() {
  const byAgency = (agency) => (row) => row.OA === agency || row.UA === agency;

  return [
    {
      id: "all",
      label: "All",
      getRows: (data) => applyExcludeSportsbetUnder(sportsbetRows(data.allArbsTopDown).filter((row) => Number(row.margin) > 0))
    },
    {
      id: "TAB",
      label: "TAB",
      getRows: (data) => applyExcludeSportsbetUnder(sportsbetRows(data.allArbsTopDown).filter(byAgency("TAB")).filter((row) => Number(row.margin) > 0))
    },
    {
      id: "Dabble",
      label: "Dabble",
      getRows: (data) => applyExcludeSportsbetUnder(sportsbetRows(data.allArbsTopDown).filter(byAgency("Dabble")).filter((row) => Number(row.margin) > 0))
    },
    {
      id: "Dabble Pickem",
      label: "Dabble Pick Em",
      getRows: (data) => applyExcludeSportsbetUnder(sportsbetRows(data.allArbsTopDown).filter(byAgency("Dabble Pickem")).filter((row) => Number(row.margin) > 0))
    },
    {
      id: "Bet365",
      label: "Bet365",
      getRows: (data) => applyExcludeSportsbetUnder(sportsbetRows(data.allArbsTopDown).filter(byAgency("Bet365")).filter((row) => Number(row.margin) > 0))
    },
    {
      id: "BetRight",
      label: "BetRight",
      getRows: (data) => applyExcludeSportsbetUnder(sportsbetRows(data.allArbsTopDown).filter(byAgency("BetRight")).filter((row) => Number(row.margin) > 0))
    },
    {
      id: "BetMakers",
      label: "BetMakers",
      getRows: (data) => applyExcludeSportsbetUnder(sportsbetRows(data.allArbsTopDown).filter(byAgency("BetMakers")).filter((row) => Number(row.margin) > 0))
    },
    {
      id: "Neds",
      label: "Neds",
      getRows: (data) => applyExcludeSportsbetUnder(sportsbetRows(data.allArbsTopDown).filter(byAgency("Neds")).filter((row) => Number(row.margin) > 0))
    },
    {
      id: "Pointsbet",
      label: "Pointsbet",
      getRows: (data) => applyExcludeSportsbetUnder(sportsbetRows(data.allArbsTopDown).filter(byAgency("Pointsbet")).filter((row) => Number(row.margin) > 0))
    },
    {
      id: "TAB Miss-by-1",
      label: "TAB Miss-by-1 Points",
      getRows: (data) => applyExcludeSportsbetUnder(
        sportsbetRows(data.tabMissByOneTopDown)
          .filter(byAgency("TAB"))
          .filter((row) => Number(row.line) >= 8.5)
          .filter((row) => Number(row.margin) > 0)
      )
    },
    {
      id: "BetRight Miss-by-1",
      label: "BetRight Miss-by-1 Points",
      getRows: (data) => applyExcludeSportsbetUnder(
        sportsbetRows(data.betrightMissByOneTopDown)
          .filter(byAgency("BetRight"))
          .filter((row) => Number(row.line) >= 8.5)
          .filter((row) => Number(row.margin) > 0)
      )
    }
  ];
}

function arbsTabs() {
  return [
    {
      id: "allArbs",
      label: "All Arbs",
      getRows: (data) =>
        data.allArbs
          .filter((row) => Number(row.margin) > 0)
          .filter((row) => row.over_agency !== "Unibet" && row.under_agency !== "Unibet")
          .map((row) => ({ ...row, margin: round2(row.margin) }))
    },
    {
      id: "unibetArbs",
      label: "Unibet Arbs",
      getRows: (data) =>
        sortByMatchThenMargin(
          data.allArbs
            .filter((row) => Number(row.margin) > 0)
            .filter((row) => row.over_agency === "Unibet" || row.under_agency === "Unibet")
            .map((row) => ({ ...row, margin: round2(row.margin) }))
        )
    },
    {
      id: "sportsbetUnderB365BR",
      label: "Sportsbet Under (Bet365/BetRight Over)",
      getRows: (data) =>
        [...data.allArbs]
          .filter((row) => Number(row.margin) > 0)
          .filter((row) => ["Bet365", "BetRight"].includes(row.over_agency))
          .filter((row) => ["Sportsbet", "Bet365", "BetRight"].includes(row.under_agency))
          .map((row) => ({ ...row, margin: round2(row.margin) }))
          .sort((a, b) => Number(b.margin) - Number(a.margin))
    },
    {
      id: "bet365BRNedsDabble3s",
      label: "Bet365, BetRight, Neds, Dabble 3s",
      getRows: (data) => {
        const dabbleThrees = data.allArbs
          .filter((row) => Number(row.margin) > 0)
          .filter((row) => ["Bet365", "BetRight", "Neds", "Dabble"].includes(row.over_agency))
          .filter((row) => ["Bet365", "BetRight", "Neds", "Dabble"].includes(row.under_agency))
          .filter((row) => row.market_name === "Player Threes");

        return data.allArbs
          .filter((row) => Number(row.margin) > 0)
          .filter((row) => ["Bet365", "BetRight", "Neds"].includes(row.over_agency))
          .filter((row) => ["Bet365", "BetRight", "Neds"].includes(row.under_agency))
          .filter((row) => row.over_agency !== "Unibet" && row.under_agency !== "Unibet")
          .concat(dabbleThrees)
          .map((row) => ({ ...row, margin: round2(row.margin) }))
          .sort((a, b) => Number(b.margin) - Number(a.margin));
      }
    },
    {
      id: "tabMissBy1Arbs",
      label: "TAB Miss-by-1 Points Arbs",
      getRows: (data) =>
        data.tabMissByOne
          .filter((row) => Number(row.margin) > 0)
          .filter((row) => Number(row.line) >= 8.5)
          .filter((row) => row.over_agency === "TAB" || row.under_agency === "TAB")
          .filter((row) => !["Neds", "Unibet"].includes(row.over_agency))
          .filter((row) => !["Neds", "Unibet"].includes(row.under_agency))
          .map((row) => ({ ...row, margin: round2(row.margin) }))
    }
  ];
}

function middleTabs() {
  return [
    {
      id: "allMiddles",
      label: "All Middles",
      getRows: (data) => data.allMiddles
    }
  ];
}

function multiLegTabs() {
  return ["Dabble", "BetRight", "TAB", "Neds", "Pointsbet", "Bet365"].map((agency) => ({
    id: agency,
    label: agency,
    getRows: (data) => {
      const filtered = data.processedOdds.filter((row) => row.agency === agency);
      if (state.multiLegMatch === "All") {
        return filtered;
      }
      return filtered.filter((row) => row.match === state.multiLegMatch);
    }
  }));
}

function currentSection() {
  if (state.mainTab === "topDown") {
    return { title: "Top-Down", tabs: topDownTabs() };
  }
  if (state.mainTab === "arbs") {
    return { title: "Arbs", tabs: arbsTabs() };
  }
  if (state.mainTab === "middles") {
    return { title: "Middles", tabs: middleTabs() };
  }
  if (state.mainTab === "multiLegs") {
    return { title: "Multi Legs", tabs: multiLegTabs() };
  }
  return { title: "", tabs: [] };
}

function getColumns(rows, preferred = []) {
  if (!rows.length) {
    return preferred;
  }
  const keys = Object.keys(rows[0]);
  const ordered = preferred.filter((col) => keys.includes(col));
  const remaining = keys.filter((col) => !ordered.includes(col));
  return [...ordered, ...remaining];
}

function sortRows(rows) {
  const { key, dir } = state.sort;
  if (!key) {
    return rows;
  }
  const factor = dir === "asc" ? 1 : -1;
  return [...rows].sort((a, b) => {
    const av = a[key];
    const bv = b[key];

    if (typeof av === "number" && typeof bv === "number") {
      return (av - bv) * factor;
    }

    return String(av ?? "").localeCompare(String(bv ?? "")) * factor;
  });
}

function filterBySearch(rows) {
  if (!state.search.trim()) {
    return rows;
  }
  const needle = state.search.trim().toLowerCase();
  return rows.filter((row) =>
    Object.values(row)
      .join(" ")
      .toLowerCase()
      .includes(needle)
  );
}

function renderMainTabs() {
  ui.mainTabs.innerHTML = "";
  mainTabConfig.forEach((tab) => {
    const button = document.createElement("button");
    button.className = `pill ${state.mainTab === tab.id ? "active" : ""}`;
    button.textContent = tab.label;
    button.addEventListener("click", () => {
      state.mainTab = tab.id;
      state.search = "";
      state.sort = { key: null, dir: "asc" };
      state.page = 1;
      render();
    });
    ui.mainTabs.appendChild(button);
  });
}

function renderSubTabs(section) {
  const selected = state.subTabs[state.mainTab];
  if (!section.tabs.length) {
    ui.subTabs.innerHTML = "";
    return;
  }

  ui.subTabs.innerHTML = "";
  section.tabs.forEach((tab) => {
    const button = document.createElement("button");
    button.className = `pill ${selected === tab.id ? "active" : ""}`;
    button.textContent = tab.label;
    button.addEventListener("click", () => {
      state.subTabs[state.mainTab] = tab.id;
      state.page = 1;
      state.sort = { key: null, dir: "asc" };
      if (state.mainTab === "multiLegs") {
        state.multiLegMatch = "All";
      }
      render();
    });
    ui.subTabs.appendChild(button);
  });
}

function renderMatchFilter(baseRows) {
  if (state.mainTab !== "multiLegs") {
    ui.multiLegMatchWrap.classList.add("hidden");
    return;
  }

  ui.multiLegMatchWrap.classList.remove("hidden");
  const matches = ["All", ...new Set(baseRows.map((row) => row.match).filter(Boolean))].sort((a, b) =>
    String(a).localeCompare(String(b))
  );

  if (!matches.includes(state.multiLegMatch)) {
    state.multiLegMatch = "All";
  }

  ui.multiLegMatch.innerHTML = "";
  matches.forEach((match) => {
    const option = document.createElement("option");
    option.value = match;
    option.textContent = match;
    ui.multiLegMatch.appendChild(option);
  });
  ui.multiLegMatch.value = state.multiLegMatch;
}

function renderTable(rows, preferredColumns = []) {
  const columns = getColumns(rows, preferredColumns);

  if (!rows.length || !columns.length) {
    ui.dataTable.innerHTML = "<tbody><tr><td>No rows found.</td></tr></tbody>";
    ui.pager.innerHTML = "";
    return;
  }

  const headerHtml = columns
    .map((column) => {
      const active = state.sort.key === column;
      const arrow = active ? (state.sort.dir === "asc" ? " ▲" : " ▼") : "";
      return `<th><button data-sort="${column}">${column}${arrow}</button></th>`;
    })
    .join("");

  const totalPages = Math.max(1, Math.ceil(rows.length / PAGE_SIZE));
  if (state.page > totalPages) {
    state.page = totalPages;
  }
  const start = (state.page - 1) * PAGE_SIZE;
  const paged = rows.slice(start, start + PAGE_SIZE);

  const bodyHtml = paged
    .map((row) => {
      const cells = columns.map((column) => `<td>${formatNumber(row[column])}</td>`).join("");
      return `<tr>${cells}</tr>`;
    })
    .join("");

  ui.dataTable.innerHTML = `<thead><tr>${headerHtml}</tr></thead><tbody>${bodyHtml}</tbody>`;

  ui.dataTable.querySelectorAll("[data-sort]").forEach((button) => {
    button.addEventListener("click", () => {
      const key = button.getAttribute("data-sort");
      if (state.sort.key === key) {
        state.sort.dir = state.sort.dir === "asc" ? "desc" : "asc";
      } else {
        state.sort = { key, dir: "asc" };
      }
      render();
    });
  });

  ui.pager.innerHTML = [
    `<button ${state.page <= 1 ? "disabled" : ""} id="prev-page">Prev</button>`,
    `<span>Page ${state.page} of ${totalPages}</span>`,
    `<button ${state.page >= totalPages ? "disabled" : ""} id="next-page">Next</button>`
  ].join("");

  const prev = document.getElementById("prev-page");
  const next = document.getElementById("next-page");
  if (prev) {
    prev.addEventListener("click", () => {
      state.page -= 1;
      render();
    });
  }
  if (next) {
    next.addEventListener("click", () => {
      state.page += 1;
      render();
    });
  }
}

function renderTableView() {
  const section = currentSection();
  const selectedId = state.subTabs[state.mainTab] || section.tabs[0]?.id;
  const selectedTab = section.tabs.find((tab) => tab.id === selectedId) ?? section.tabs[0];

  state.subTabs[state.mainTab] = selectedTab?.id;

  ui.viewTitle.textContent = section.title;
  ui.excludeSportsbetWrap.classList.toggle("hidden", state.mainTab !== "topDown");

  renderSubTabs(section);

  if (!selectedTab) {
    renderTable([]);
    return;
  }

  if (state.mainTab === "multiLegs") {
    const agencyRows = state.data.processedOdds.filter((row) => row.agency === selectedTab.id);
    renderMatchFilter(agencyRows);
  } else {
    ui.multiLegMatchWrap.classList.add("hidden");
  }

  const baseRows = selectedTab.getRows(state.data);
  const searched = filterBySearch(baseRows);
  const rows = sortRows(searched);

  const preferredColumns =
    state.mainTab === "topDown"
      ? ["player", "market", "line", "OP", "OA", "UP", "UA", "margin", "match", "player_team", "opposition_team"]
      : [];

  ui.tableMeta.textContent = `${rows.length} rows (${baseRows.length} before search)`;
  renderTable(rows, preferredColumns);
}

function render() {
  renderMainTabs();

  ui.searchInput.value = state.search;
  ui.excludeSportsbet.checked = state.excludeSportsbetUnder;

  if (state.mainTab === "calculator") {
    ui.calculatorView.classList.remove("hidden");
    ui.tableView.classList.add("hidden");
    return;
  }

  ui.calculatorView.classList.add("hidden");
  ui.tableView.classList.remove("hidden");
  renderTableView();
}

async function detectRefreshApi() {
  try {
    const response = await fetch("/api/health", { cache: "no-store" });
    if (!response.ok) {
      throw new Error("health endpoint unavailable");
    }
    const payload = await response.json();
    state.refreshApiAvailable = Boolean(payload.refreshSupported);
    if (state.refreshApiAvailable) {
      setRefreshStatus("Refresh endpoint ready");
    } else {
      setRefreshStatus("Refresh not supported by server");
    }
  } catch (error) {
    state.refreshApiAvailable = false;
    setRefreshStatus("Start with node server.js for in-app refresh");
  } finally {
    updateRefreshButtonState(false);
  }
}

async function loadData() {
  try {
    const response = await fetch("./data/nba-arbs-data.json", { cache: "no-store" });
    if (!response.ok) {
      throw new Error("Missing data file");
    }

    const raw = await response.json();
    state.data = preprocessData(raw);
    ui.generatedAt.textContent = state.data.generatedAt
      ? `Generated: ${state.data.generatedAt}`
      : "Generated timestamp unavailable";
    renderDatasetUpdates(state.data.datasetMeta, state.data.generatedAt);

    ui.statusView.classList.add("hidden");
    ui.calculatorView.classList.remove("hidden");
    ui.tableView.classList.remove("hidden");

    render();
    return true;
  } catch (error) {
    ui.generatedAt.textContent = "No generated data file found";
    renderDatasetUpdates({}, null);
    ui.calculatorView.classList.add("hidden");
    ui.tableView.classList.add("hidden");
    ui.statusView.classList.remove("hidden");
    return false;
  }
}

async function refreshDataFromApp() {
  if (!state.refreshApiAvailable) {
    setRefreshStatus("Refresh API unavailable", true);
    return;
  }

  updateRefreshButtonState(true);
  setRefreshStatus("Running export script...");

  try {
    const response = await fetch("/api/refresh", {
      method: "POST",
      headers: { "Content-Type": "application/json" }
    });
    const payload = await response.json();
    if (!response.ok || payload.success !== true) {
      throw new Error(payload.error || "Refresh failed");
    }

    const loaded = await loadData();
    if (loaded) {
      setRefreshStatus(`Refreshed at ${new Date().toLocaleTimeString()}`);
    } else {
      setRefreshStatus("Script ran but data load failed", true);
    }
  } catch (error) {
    setRefreshStatus(error.message, true);
  } finally {
    updateRefreshButtonState(false);
  }
}

ui.excludeSportsbet.addEventListener("change", (event) => {
  state.excludeSportsbetUnder = event.target.checked;
  state.page = 1;
  render();
});

ui.multiLegMatch.addEventListener("change", (event) => {
  state.multiLegMatch = event.target.value;
  state.page = 1;
  render();
});

ui.searchInput.addEventListener("input", (event) => {
  state.search = event.target.value;
  state.page = 1;
  render();
});

ui.refreshButton.addEventListener("click", refreshDataFromApp);

[ui.odds1, ui.odds2, ui.stake1].forEach((input) => {
  input.addEventListener("input", calculateArb);
});

calculateArb();
updateRefreshButtonState(false);
detectRefreshApi();
loadData();
