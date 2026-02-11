import { useCallback, useEffect, useMemo, useState } from "react";

type CellValue = string | number | boolean | null | undefined;
type TableRow = Record<string, CellValue>;
type SortDir = "asc" | "desc";
type MainTab = "calculator" | "topDown" | "arbs" | "middles" | "multiLegs";
type SidebarTab = "filters" | "status";

interface ArbFilters {
  marketNames: string[];
  overAgencies: string[];
  underAgencies: string[];
}

interface ArbFilterOptions {
  markets: string[];
  overAgencies: string[];
  underAgencies: string[];
}

interface SortState {
  key: string | null;
  dir: SortDir;
}

interface DatasetMetaEntry {
  source?: string;
  source_dir?: string;
  file_mtime?: string;
  latest_file_mtime?: string;
  earliest_file_mtime?: string;
  rows?: number;
  cols?: number;
  files?: number;
}

interface RawApiData {
  generated_at?: string;
  dataset_meta?: Record<string, DatasetMetaEntry>;
  all_arbs?: TableRow[];
  all_middles?: TableRow[];
  tab_miss_by_one?: TableRow[];
  betright_miss_by_one?: TableRow[];
  processed_odds?: TableRow[];
}

interface PreprocessedData {
  generatedAt: string | null;
  datasetMeta: Record<string, DatasetMetaEntry>;
  allArbs: TableRow[];
  allMiddles: TableRow[];
  tabMissByOne: TableRow[];
  betrightMissByOne: TableRow[];
  allArbsTopDown: TableRow[];
  tabMissByOneTopDown: TableRow[];
  betrightMissByOneTopDown: TableRow[];
  processedOdds: TableRow[];
}

interface TabDefinition {
  id: string;
  label: string;
  getRows: (data: PreprocessedData) => TableRow[];
}

const PAGE_SIZE = 50;
const MOBILE_BREAKPOINT = 860;
const EXCLUDED_DEFAULT_AGENCY = "Dabble Pickem";
const COMMON_OVER_AGENCIES = ["Bet365", "BetRight", "TAB", "Pointsbet"];
const COMMON_UNDER_AGENCIES = ["Sportsbet", "Bet365", "BetRight"];
const MULTI_LEG_AGENCIES = ["Dabble", "BetRight", "BetMakers", "TAB", "Neds", "Pointsbet", "Sportsbet", "Bet365"];

const mainTabConfig: Array<{ id: MainTab; label: string }> = [
  { id: "topDown", label: "Top-Down" },
  { id: "arbs", label: "Arbs" },
  { id: "middles", label: "Middles" },
  { id: "multiLegs", label: "Multi Legs" },
  { id: "calculator", label: "Calculator" }
];

const datasetLabels: Record<string, string> = {
  all_arbs: "all_arbs.rds",
  all_middles: "all_middles.rds",
  tab_miss_by_one: "tab_points_miss_by_one.rds",
  betright_miss_by_one: "betright_points_miss_by_one.rds",
  processed_odds: "processed_odds/*.rds"
};

const topDownPreferredColumns = [
  "player",
  "market",
  "line",
  "OP",
  "OA",
  "UP",
  "UA",
  "margin",
  "match",
  "player_team",
  "opposition_team"
];

function toNumber(value: CellValue): number {
  const num = Number(value);
  return Number.isFinite(num) ? num : Number.NaN;
}

function round2(value: CellValue): number | null {
  const num = toNumber(value);
  return Number.isFinite(num) ? Math.round(num * 100) / 100 : null;
}

function formatNumber(value: CellValue): string {
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

function formatCurrency(value: number): string {
  if (!Number.isFinite(value)) {
    return "-";
  }
  return `$${value.toFixed(2)}`;
}

function getMarginTierClass(marginValue: CellValue): string {
  const margin = toNumber(marginValue);
  if (!Number.isFinite(margin) || margin <= 0) {
    return "";
  }
  if (margin < 1) {
    return "margin-common";
  }
  if (margin < 2.5) {
    return "margin-uncommon";
  }
  if (margin < 5) {
    return "margin-rare";
  }
  return "margin-epic";
}

function normalizeTopDownRows(rows: TableRow[]): TableRow[] {
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

function preprocessData(raw: RawApiData): PreprocessedData {
  const allArbs = raw.all_arbs ?? [];
  const tabMissByOne = raw.tab_miss_by_one ?? [];
  const betrightMissByOne = raw.betright_miss_by_one ?? [];

  const processedOdds = (raw.processed_odds ?? [])
    .filter((row) => toNumber(row.empirical_prob_last_20) >= 0.9)
    .map((row) => ({
      match: row.match,
      player_name: row.player_name,
      market_name: row.market_name,
      line: row.line,
      over_price: row.over_price,
      agency: row.agency,
      empirical_prob_last_20: round2(row.empirical_prob_last_20)
    }))
    .sort((a, b) => {
      const prob = toNumber(b.empirical_prob_last_20) - toNumber(a.empirical_prob_last_20);
      if (prob !== 0) {
        return prob;
      }
      return toNumber(b.over_price) - toNumber(a.over_price);
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

function sportsbetRows(rows: TableRow[]): TableRow[] {
  return rows.filter((row) => row.OA === "Sportsbet" || row.UA === "Sportsbet");
}

function applyExcludeSportsbetUnder(rows: TableRow[], excludeSportsbetUnder: boolean): TableRow[] {
  if (!excludeSportsbetUnder) {
    return rows;
  }
  return rows.filter((row) => row.UA !== "Sportsbet");
}

function sortByMatchThenMargin(rows: TableRow[]): TableRow[] {
  return [...rows].sort((a, b) => {
    const matchCompare = String(a.match ?? "").localeCompare(String(b.match ?? ""));
    if (matchCompare !== 0) {
      return matchCompare;
    }
    return toNumber(b.margin) - toNumber(a.margin);
  });
}

function applyArbFilters(rows: TableRow[], filters: ArbFilters): TableRow[] {
  return rows.filter((row) => {
    const market = String(row.market_name ?? "");
    const overAgency = String(row.over_agency ?? "");
    const underAgency = String(row.under_agency ?? "");

    const marketOk = filters.marketNames.includes(market);
    const overOk = filters.overAgencies.includes(overAgency);
    const underOk = filters.underAgencies.includes(underAgency);

    return marketOk && overOk && underOk;
  });
}

function buildDefaultArbFilters(options: ArbFilterOptions): ArbFilters {
  return {
    marketNames: [...options.markets],
    overAgencies: options.overAgencies.filter((agency) => agency !== EXCLUDED_DEFAULT_AGENCY),
    underAgencies: options.underAgencies.filter((agency) => agency !== EXCLUDED_DEFAULT_AGENCY)
  };
}

function topDownTabs(excludeSportsbetUnder: boolean): TabDefinition[] {
  const byAgency = (agency: string) => (row: TableRow): boolean => row.OA === agency || row.UA === agency;

  return [
    {
      id: "all",
      label: "All",
      getRows: (data) =>
        applyExcludeSportsbetUnder(
          sportsbetRows(data.allArbsTopDown).filter((row) => toNumber(row.margin) > 0),
          excludeSportsbetUnder
        )
    },
    {
      id: "Bet365",
      label: "Bet365",
      getRows: (data) =>
        applyExcludeSportsbetUnder(
          sportsbetRows(data.allArbsTopDown)
            .filter(byAgency("Bet365"))
            .filter((row) => toNumber(row.margin) > 0),
          excludeSportsbetUnder
        )
    },
    {
      id: "TAB",
      label: "TAB",
      getRows: (data) =>
        applyExcludeSportsbetUnder(
          sportsbetRows(data.allArbsTopDown)
            .filter(byAgency("TAB"))
            .filter((row) => toNumber(row.margin) > 0),
          excludeSportsbetUnder
        )
    },
    {
      id: "Pointsbet",
      label: "Pointsbet",
      getRows: (data) =>
        applyExcludeSportsbetUnder(
          sportsbetRows(data.allArbsTopDown)
            .filter(byAgency("Pointsbet"))
            .filter((row) => toNumber(row.margin) > 0),
          excludeSportsbetUnder
        )
    },
    {
      id: "Neds",
      label: "Neds",
      getRows: (data) =>
        applyExcludeSportsbetUnder(
          sportsbetRows(data.allArbsTopDown)
            .filter(byAgency("Neds"))
            .filter((row) => toNumber(row.margin) > 0),
          excludeSportsbetUnder
        )
    },
    {
      id: "BetRight",
      label: "BetRight",
      getRows: (data) =>
        applyExcludeSportsbetUnder(
          sportsbetRows(data.allArbsTopDown)
            .filter(byAgency("BetRight"))
            .filter((row) => toNumber(row.margin) > 0),
          excludeSportsbetUnder
        )
    },
    {
      id: "Dabble",
      label: "Dabble",
      getRows: (data) =>
        applyExcludeSportsbetUnder(
          sportsbetRows(data.allArbsTopDown)
            .filter(byAgency("Dabble"))
            .filter((row) => toNumber(row.margin) > 0),
          excludeSportsbetUnder
        )
    },
    {
      id: "Dabble Pickem",
      label: "Dabble Pick Em",
      getRows: (data) =>
        applyExcludeSportsbetUnder(
          sportsbetRows(data.allArbsTopDown)
            .filter(byAgency("Dabble Pickem"))
            .filter((row) => toNumber(row.margin) > 0),
          excludeSportsbetUnder
        )
    },
    {
      id: "BetMakers",
      label: "BetMakers",
      getRows: (data) =>
        applyExcludeSportsbetUnder(
          sportsbetRows(data.allArbsTopDown)
            .filter(byAgency("BetMakers"))
            .filter((row) => toNumber(row.margin) > 0),
          excludeSportsbetUnder
        )
    }
  ];
}

function arbsTabs(): TabDefinition[] {
  return [
    {
      id: "allArbs",
      label: "All Arbs",
      getRows: (data) =>
        data.allArbs
          .filter((row) => toNumber(row.margin) > 0)
          .filter((row) => row.over_agency !== "Unibet" && row.under_agency !== "Unibet")
          .map((row) => ({ ...row, margin: round2(row.margin) }))
    },
    {
      id: "tabMissBy1Arbs",
      label: "TAB Miss-by-1 Points Arbs",
      getRows: (data) =>
        data.tabMissByOne
          .filter((row) => toNumber(row.margin) > 0)
          .filter((row) => toNumber(row.line) >= 8.5)
          .filter((row) => row.over_agency === "TAB" || row.under_agency === "TAB")
          .filter((row) => !["Neds", "Unibet"].includes(String(row.over_agency ?? "")))
          .filter((row) => !["Neds", "Unibet"].includes(String(row.under_agency ?? "")))
          .map((row) => ({ ...row, margin: round2(row.margin) }))
    },
    {
      id: "betrightMissBy1Arbs",
      label: "BetRight Miss-by-1 Points Arbs",
      getRows: (data) =>
        data.betrightMissByOne
          .filter((row) => toNumber(row.margin) > 0)
          .filter((row) => toNumber(row.line) >= 8.5)
          .filter((row) => row.over_agency === "BetRight" || row.under_agency === "BetRight")
          .filter((row) => !["Neds", "Unibet"].includes(String(row.over_agency ?? "")))
          .filter((row) => !["Neds", "Unibet"].includes(String(row.under_agency ?? "")))
          .map((row) => ({ ...row, margin: round2(row.margin) }))
    }
  ];
}

function middleTabs(): TabDefinition[] {
  return [
    {
      id: "allMiddles",
      label: "All Middles",
      getRows: (data) => data.allMiddles
    }
  ];
}

function multiLegTabs(multiLegMatch: string): TabDefinition[] {
  return MULTI_LEG_AGENCIES.map((agency) => ({
    id: agency,
    label: agency,
    getRows: (data: PreprocessedData) => {
      const filtered = data.processedOdds.filter((row) => row.agency === agency);
      if (multiLegMatch === "All") {
        return filtered;
      }
      return filtered.filter((row) => row.match === multiLegMatch);
    }
  }));
}

function getSection(mainTab: MainTab, excludeSportsbetUnder: boolean, multiLegMatch: string): { title: string; tabs: TabDefinition[] } {
  if (mainTab === "topDown") {
    return { title: "Top-Down", tabs: topDownTabs(excludeSportsbetUnder) };
  }
  if (mainTab === "arbs") {
    return { title: "Arbs", tabs: arbsTabs() };
  }
  if (mainTab === "middles") {
    return { title: "Middles", tabs: middleTabs() };
  }
  if (mainTab === "multiLegs") {
    return { title: "Multi Legs", tabs: multiLegTabs(multiLegMatch) };
  }
  return { title: "", tabs: [] };
}

function getColumns(rows: TableRow[], preferred: string[] = []): string[] {
  if (!rows.length) {
    return preferred;
  }
  const keys = Object.keys(rows[0]);
  const ordered = preferred.filter((column) => keys.includes(column));
  const remaining = keys.filter((column) => !ordered.includes(column));
  return [...ordered, ...remaining];
}

function filterBySearch(rows: TableRow[], search: string): TableRow[] {
  const needle = search.trim().toLowerCase();
  if (!needle) {
    return rows;
  }

  return rows.filter((row) =>
    Object.values(row)
      .map((value) => String(value ?? ""))
      .join(" ")
      .toLowerCase()
      .includes(needle)
  );
}

function sortRows(rows: TableRow[], sort: SortState): TableRow[] {
  const { key, dir } = sort;
  if (!key) {
    return rows;
  }

  const factor = dir === "asc" ? 1 : -1;

  return [...rows].sort((a, b) => {
    const aValue = a[key];
    const bValue = b[key];

    const aNum = toNumber(aValue);
    const bNum = toNumber(bValue);
    if (Number.isFinite(aNum) && Number.isFinite(bNum)) {
      return (aNum - bNum) * factor;
    }

    return String(aValue ?? "").localeCompare(String(bValue ?? "")) * factor;
  });
}

export default function App(): JSX.Element {
  const [data, setData] = useState<PreprocessedData | null>(null);
  const [dataError, setDataError] = useState<string | null>(null);

  const [mainTab, setMainTab] = useState<MainTab>("topDown");
  const [subTabs, setSubTabs] = useState<Record<MainTab, string>>({
    calculator: "",
    topDown: "all",
    arbs: "allArbs",
    middles: "allMiddles",
    multiLegs: "Dabble"
  });

  const [excludeSportsbetUnder, setExcludeSportsbetUnder] = useState(false);
  const [multiLegMatch, setMultiLegMatch] = useState("All");
  const [search, setSearch] = useState("");
  const [sort, setSort] = useState<SortState>({ key: null, dir: "asc" });
  const [page, setPage] = useState(1);
  const [sidebarTab, setSidebarTab] = useState<SidebarTab>("filters");
  const [isMobileViewport, setIsMobileViewport] = useState(false);
  const [isSidebarExpanded, setIsSidebarExpanded] = useState(true);
  const [arbFilters, setArbFilters] = useState<ArbFilters | null>(null);

  const [refreshApiAvailable, setRefreshApiAvailable] = useState(false);
  const [refreshBusy, setRefreshBusy] = useState(false);
  const [refreshStatus, setRefreshStatus] = useState("Checking refresh endpoint...");

  const [odds1, setOdds1] = useState("2.4");
  const [stake1, setStake1] = useState("100");
  const [odds2, setOdds2] = useState("1.8");

  const section = useMemo(
    () => getSection(mainTab, excludeSportsbetUnder, multiLegMatch),
    [mainTab, excludeSportsbetUnder, multiLegMatch]
  );

  const selectedSubtabId = subTabs[mainTab] || section.tabs[0]?.id || "";
  const selectedTab = section.tabs.find((tab) => tab.id === selectedSubtabId) ?? section.tabs[0] ?? null;

  useEffect(() => {
    if (!section.tabs.length) {
      return;
    }

    if (!section.tabs.some((tab) => tab.id === selectedSubtabId)) {
      setSubTabs((prev) => ({
        ...prev,
        [mainTab]: section.tabs[0].id
      }));
    }
  }, [mainTab, section.tabs, selectedSubtabId]);

  const dataUrl = `${import.meta.env.BASE_URL}data/nba-arbs-data.json`;

  const loadData = useCallback(async (): Promise<boolean> => {
    try {
      const response = await fetch(dataUrl, { cache: "no-store" });
      if (!response.ok) {
        throw new Error(`Failed to load data (${response.status})`);
      }

      const raw = (await response.json()) as RawApiData;
      setData(preprocessData(raw));
      setDataError(null);
      return true;
    } catch (error) {
      setData(null);
      setDataError(error instanceof Error ? error.message : "Data unavailable");
      return false;
    }
  }, [dataUrl]);

  const detectRefreshApi = useCallback(async () => {
    try {
      const response = await fetch("/api/health", { cache: "no-store" });
      if (!response.ok) {
        throw new Error("Health endpoint unavailable");
      }

      const payload = (await response.json()) as { refreshSupported?: boolean };
      const supported = Boolean(payload.refreshSupported);
      setRefreshApiAvailable(supported);
      setRefreshStatus(supported ? "Refresh endpoint ready" : "Refresh not supported by server");
    } catch (_error) {
      setRefreshApiAvailable(false);
      setRefreshStatus("Static mode: refresh disabled");
    }
  }, []);

  useEffect(() => {
    void detectRefreshApi();
    void loadData();
  }, [detectRefreshApi, loadData]);

  useEffect(() => {
    const mediaQuery = window.matchMedia(`(max-width: ${MOBILE_BREAKPOINT}px)`);

    const syncSidebarMode = (matches: boolean) => {
      setIsMobileViewport(matches);
      setIsSidebarExpanded(!matches);
    };

    syncSidebarMode(mediaQuery.matches);

    const onChange = (event: MediaQueryListEvent) => {
      syncSidebarMode(event.matches);
    };

    mediaQuery.addEventListener("change", onChange);
    return () => mediaQuery.removeEventListener("change", onChange);
  }, []);

  const refreshDataFromApp = useCallback(async () => {
    if (!refreshApiAvailable) {
      setRefreshStatus("Refresh API unavailable");
      return;
    }

    setRefreshBusy(true);
    setRefreshStatus("Running export script...");

    try {
      const response = await fetch("/api/refresh", {
        method: "POST",
        headers: { "Content-Type": "application/json" }
      });

      const payload = (await response.json()) as { success?: boolean; error?: string };
      if (!response.ok || payload.success !== true) {
        throw new Error(payload.error || "Refresh failed");
      }

      const loaded = await loadData();
      setRefreshStatus(loaded ? `Refreshed at ${new Date().toLocaleTimeString()}` : "Refresh completed but data failed to reload");
    } catch (error) {
      setRefreshStatus(error instanceof Error ? error.message : "Refresh failed");
    } finally {
      setRefreshBusy(false);
    }
  }, [loadData, refreshApiAvailable]);

  const calculated = useMemo(() => {
    const o1 = toNumber(odds1);
    const s1 = toNumber(stake1);
    const o2 = toNumber(odds2);

    if (!Number.isFinite(o1) || !Number.isFinite(s1) || !Number.isFinite(o2) || o2 === 0) {
      return {
        stake2: null,
        arbPct: null,
        profit: null
      };
    }

    const calcStake2 = (s1 * o1) / o2;
    const calcArbPct = ((1 / o1) + (1 / o2) - 1) * -100;
    const calcProfit = (s1 * o1) - (s1 + calcStake2);

    return {
      stake2: calcStake2,
      arbPct: calcArbPct,
      profit: calcProfit
    };
  }, [odds1, odds2, stake1]);

  const arbFilterOptions = useMemo<ArbFilterOptions>(() => {
    const empty: ArbFilterOptions = { markets: [], overAgencies: [], underAgencies: [] };
    if (!data) {
      return empty;
    }

    const sourceRows = [...data.allArbs, ...data.tabMissByOne, ...data.betrightMissByOne];
    const unique = (values: string[]) => Array.from(new Set(values.filter(Boolean))).sort((a, b) => a.localeCompare(b));

    return {
      markets: unique(sourceRows.map((row) => String(row.market_name ?? ""))),
      overAgencies: unique(sourceRows.map((row) => String(row.over_agency ?? ""))),
      underAgencies: unique(sourceRows.map((row) => String(row.under_agency ?? "")))
    };
  }, [data]);

  const defaultArbFilters = useMemo(() => buildDefaultArbFilters(arbFilterOptions), [arbFilterOptions]);
  const activeArbFilters = arbFilters ?? defaultArbFilters;

  useEffect(() => {
    setArbFilters((prev) => {
      if (!prev) {
        return prev;
      }
      return {
        marketNames: prev.marketNames.filter((item) => arbFilterOptions.markets.includes(item)),
        overAgencies: prev.overAgencies.filter((item) => arbFilterOptions.overAgencies.includes(item)),
        underAgencies: prev.underAgencies.filter((item) => arbFilterOptions.underAgencies.includes(item))
      };
    });
  }, [arbFilterOptions]);

  const toggleArbFilter = useCallback(
    (key: keyof ArbFilters, value: string) => {
      setArbFilters((prev) => {
        const base = prev ?? defaultArbFilters;
        const exists = base[key].includes(value);
        const nextValues = exists ? base[key].filter((item) => item !== value) : [...base[key], value];
        return { ...base, [key]: nextValues };
      });
      setPage(1);
    },
    [defaultArbFilters]
  );

  const resetArbFilters = useCallback(() => {
    setArbFilters(null);
    setPage(1);
  }, []);

  const applyCommonFilters = useCallback(() => {
    setArbFilters({
      marketNames: [...arbFilterOptions.markets],
      overAgencies: COMMON_OVER_AGENCIES.filter((agency) => arbFilterOptions.overAgencies.includes(agency)),
      underAgencies: COMMON_UNDER_AGENCIES.filter((agency) => arbFilterOptions.underAgencies.includes(agency))
    });
    setPage(1);
  }, [arbFilterOptions]);

  const baseRows = useMemo(() => {
    if (!data || !selectedTab) {
      return [];
    }
    return selectedTab.getRows(data);
  }, [data, selectedTab]);

  const filteredBaseRows = useMemo(() => {
    if (mainTab !== "arbs") {
      return baseRows;
    }
    return applyArbFilters(baseRows, activeArbFilters);
  }, [activeArbFilters, baseRows, mainTab]);

  const tabCounts = useMemo(() => {
    if (!data) {
      return {} as Record<string, number>;
    }

    return Object.fromEntries(
      section.tabs.map((tab) => {
        try {
          const rows = tab.getRows(data);
          const finalRows = mainTab === "arbs" ? applyArbFilters(rows, activeArbFilters) : rows;
          return [tab.id, finalRows.length];
        } catch (_error) {
          return [tab.id, 0];
        }
      })
    ) as Record<string, number>;
  }, [activeArbFilters, data, mainTab, section.tabs]);

  const matchOptions = useMemo(() => {
    if (!data || mainTab !== "multiLegs" || !selectedTab) {
      return ["All"];
    }

    const agencyRows = data.processedOdds.filter((row) => row.agency === selectedTab.id);
    const matches = Array.from(new Set(agencyRows.map((row) => String(row.match ?? "")).filter(Boolean))).sort((a, b) =>
      a.localeCompare(b)
    );

    return ["All", ...matches];
  }, [data, mainTab, selectedTab]);

  useEffect(() => {
    if (!matchOptions.includes(multiLegMatch)) {
      setMultiLegMatch("All");
    }
  }, [matchOptions, multiLegMatch]);

  const searchedRows = useMemo(() => filterBySearch(filteredBaseRows, search), [filteredBaseRows, search]);
  const sortedRows = useMemo(() => sortRows(searchedRows, sort), [searchedRows, sort]);

  const columns = useMemo(
    () => getColumns(sortedRows, mainTab === "topDown" ? topDownPreferredColumns : []),
    [mainTab, sortedRows]
  );
  const showCalculatorAction = mainTab === "arbs";
  const showMultiLegAction = mainTab === "topDown";

  const totalPages = Math.max(1, Math.ceil(sortedRows.length / PAGE_SIZE));
  const currentPage = Math.min(page, totalPages);

  useEffect(() => {
    if (page > totalPages) {
      setPage(totalPages);
    }
  }, [page, totalPages]);

  const pagedRows = useMemo(() => {
    const start = (currentPage - 1) * PAGE_SIZE;
    return sortedRows.slice(start, start + PAGE_SIZE);
  }, [currentPage, sortedRows]);

  const generatedLabel = data?.generatedAt ? `Generated: ${data.generatedAt}` : "No generated data loaded";
  const positiveMarginCount = useMemo(
    () => (data ? data.allArbs.filter((row) => toNumber(row.margin) > 0).length : 0),
    [data]
  );
  const epicMarginCount = useMemo(
    () => (data ? data.allArbs.filter((row) => toNumber(row.margin) >= 5).length : 0),
    [data]
  );

  const jumpToCalculator = useCallback(
    (row: TableRow) => {
      const over = toNumber(row.over_price);
      const under = toNumber(row.under_price);
      if (!Number.isFinite(over) || !Number.isFinite(under)) {
        return;
      }

      setOdds1(String(over));
      setOdds2(String(under));
      setStake1("100");
      setMainTab("calculator");
      setPage(1);
    },
    [setMainTab]
  );

  const jumpToMultiLegs = useCallback((row: TableRow) => {
    const agency = String(row.UA ?? "");
    const match = String(row.match ?? "");
    if (!MULTI_LEG_AGENCIES.includes(agency) || !match) {
      return;
    }

    setSubTabs((prev) => ({ ...prev, multiLegs: agency }));
    setMultiLegMatch(match);
    setMainTab("multiLegs");
    setSearch("");
    setSort({ key: null, dir: "asc" });
    setPage(1);
  }, []);

  const showSidebarContent = !isMobileViewport || isSidebarExpanded;

  return (
    <div className="app-shell">
      <div className="ambient ambient-a" />
      <div className="ambient ambient-b" />
      <div className="ambient ambient-c" />

      <header className="hero-panel fade-up">
        <div className="hero-copy">
          <h1 className="hero-title">NBA</h1>
          <p className="hero-subtitle">{generatedLabel}</p>
          <div className="hero-metrics">
            <span className="metric-chip">
              Positive Arbs
              <strong>{positiveMarginCount.toLocaleString()}</strong>
            </span>
            <span className="metric-chip">
              Margin 5.00+
              <strong>{epicMarginCount.toLocaleString()}</strong>
            </span>
            <span className="metric-chip">
              Live View Rows
              <strong>{sortedRows.length.toLocaleString()}</strong>
            </span>
          </div>
        </div>

        <div className="hero-actions">
          <div className="refresh-row">
            <button
              type="button"
              onClick={() => void refreshDataFromApp()}
              disabled={refreshBusy || !refreshApiAvailable}
              className="refresh-btn"
            >
              {refreshBusy ? "Refreshing..." : "Refresh Data"}
            </button>
            <span className={`refresh-status-chip ${refreshApiAvailable ? "online" : "offline"}`}>
              {refreshStatus}
            </span>
          </div>

          <div className="main-tabs-row">
            {mainTabConfig.map((tab) => {
              const active = mainTab === tab.id;
              return (
                <button
                  key={tab.id}
                  type="button"
                  onClick={() => {
                    setMainTab(tab.id);
                    setSearch("");
                    setSort({ key: null, dir: "asc" });
                    setPage(1);
                  }}
                  className={`main-tab-btn ${active ? "active" : ""}`}
                >
                  {tab.label}
                </button>
              );
            })}
          </div>
        </div>
      </header>

      <main className="dashboard-grid">
        <aside className="neo-card sidebar-card fade-up delay-1">
          {isMobileViewport && (
            <button
              type="button"
              className="sidebar-toggle-btn"
              onClick={() => setIsSidebarExpanded((prev) => !prev)}
            >
              <span>{isSidebarExpanded ? "Hide" : "Show"} {sidebarTab === "filters" ? "Filters" : "Scraper Status"}</span>
              <span className={`sidebar-toggle-icon ${isSidebarExpanded ? "open" : ""}`}>▾</span>
            </button>
          )}

          {showSidebarContent && (
            <>
              <div className="sidebar-tabs">
                <button
                  type="button"
                  className={`sidebar-tab-btn ${sidebarTab === "filters" ? "active" : ""}`}
                  onClick={() => setSidebarTab("filters")}
                >
                  Filters
                </button>
                <button
                  type="button"
                  className={`sidebar-tab-btn ${sidebarTab === "status" ? "active" : ""}`}
                  onClick={() => setSidebarTab("status")}
                >
                  Scraper Status
                </button>
              </div>

              {sidebarTab === "filters" ? (
                <>
                  <h2 className="panel-title">Arbs Filters</h2>
                  <p className="panel-subtitle">Applies to Arbs tab rows and subtab counts.</p>

                  <div className="filter-block">
                    <div className="filter-section">
                      <p className="filter-label">market_name</p>
                      <div className="filter-options">
                        {arbFilterOptions.markets.map((item) => (
                          <label key={item} className="filter-option">
                            <input
                              type="checkbox"
                              checked={activeArbFilters.marketNames.includes(item)}
                              onChange={() => toggleArbFilter("marketNames", item)}
                            />
                            <span>{item}</span>
                          </label>
                        ))}
                      </div>
                    </div>

                    <div className="filter-section">
                      <p className="filter-label">over_agency</p>
                      <div className="filter-options">
                        {arbFilterOptions.overAgencies.map((item) => (
                          <label key={item} className="filter-option">
                            <input
                              type="checkbox"
                              checked={activeArbFilters.overAgencies.includes(item)}
                              onChange={() => toggleArbFilter("overAgencies", item)}
                            />
                            <span>{item}</span>
                          </label>
                        ))}
                      </div>
                    </div>

                    <div className="filter-section">
                      <p className="filter-label">under_agency</p>
                      <div className="filter-options">
                        {arbFilterOptions.underAgencies.map((item) => (
                          <label key={item} className="filter-option">
                            <input
                              type="checkbox"
                              checked={activeArbFilters.underAgencies.includes(item)}
                              onChange={() => toggleArbFilter("underAgencies", item)}
                            />
                            <span>{item}</span>
                          </label>
                        ))}
                      </div>
                    </div>

                    <p className="filter-hint">
                      Defaults select all values except {EXCLUDED_DEFAULT_AGENCY}. Use the filter buttons under the Arbs title. {mainTab === "arbs" ? "Active on current view." : "Switch to Arbs tab to apply."}
                    </p>
                  </div>
                </>
              ) : (
                <>
                  <h2 className="panel-title">Scraper Status</h2>
                  <p className="panel-subtitle">
                    {data?.generatedAt ? `Dashboard JSON generated: ${data.generatedAt}` : "Waiting for data..."}
                  </p>

                  <ul className="dataset-list">
                    {Object.entries(datasetLabels).map(([key, label]) => {
                      const meta = data?.datasetMeta?.[key] ?? {};
                      const updatedAt = meta.file_mtime ?? meta.latest_file_mtime ?? "Unknown";
                      const source = meta.source ?? meta.source_dir ?? "Source unavailable";
                      const stats: string[] = [];
                      if (typeof meta.rows === "number") {
                        stats.push(`${meta.rows.toLocaleString()} rows`);
                      }
                      if (typeof meta.cols === "number") {
                        stats.push(`${meta.cols} cols`);
                      }
                      if (typeof meta.files === "number") {
                        stats.push(`${meta.files} files`);
                      }

                      return (
                        <li key={key} className="dataset-card">
                          <strong>{label}</strong>
                          <span>Updated: {updatedAt}</span>
                          <span>{stats.join(" | ") || "Shape unavailable"}</span>
                          <span className="truncate" title={source}>
                            Source: {source}
                          </span>
                        </li>
                      );
                    })}
                  </ul>

                  <div className="legend-block">
                    <h3>Margin</h3>
                    <ul>
                      <li>
                        <span className="legend-dot common" />
                        0.01 to 0.99
                      </li>
                      <li>
                        <span className="legend-dot uncommon" />
                        1.00 to 2.49
                      </li>
                      <li>
                        <span className="legend-dot rare" />
                        2.50 to 4.99
                      </li>
                      <li>
                        <span className="legend-dot epic" />
                        5.00+
                      </li>
                    </ul>
                  </div>
                </>
              )}
            </>
          )}
        </aside>

        <div className="content-stack">
          {mainTab === "calculator" ? (
            <section className="neo-card fade-up delay-2">
              <h2 className="panel-title">Arbitrage Calculator</h2>

              <div className="calculator-layout">
                <div className="calculator-inputs">
                  <h3>Inputs</h3>
                  <div className="calc-grid">
                    <label className="field">
                      Odds 1
                      <input
                        value={odds1}
                        onChange={(event) => setOdds1(event.target.value)}
                        type="number"
                        step="0.01"
                        className="field-input"
                      />
                    </label>
                <label className="field">
                  Stake 1
                  <input
                    value={stake1}
                    onChange={(event) => setStake1(event.target.value)}
                    type="number"
                    step="5"
                    className="field-input"
                  />
                </label>
                    <label className="field">
                      Odds 2
                      <input
                        value={odds2}
                        onChange={(event) => setOdds2(event.target.value)}
                        type="number"
                        step="0.01"
                        className="field-input"
                      />
                    </label>
                  </div>

                  <p className="calc-formula">
                    Stake 2 = (Stake 1 x Odds 1) / Odds 2
                  </p>
                </div>

                <div className="calculator-results">
                  <h3>Results</h3>
                  <div className="stats-grid">
                    <div className="stat-card">
                      <p>Stake 1</p>
                      <strong>
                        {Number.isFinite(toNumber(stake1)) ? formatCurrency(toNumber(stake1)) : "-"}
                      </strong>
                    </div>
                    <div className="stat-card">
                      <p>Stake 2</p>
                      <strong>
                        {calculated.stake2 === null ? "-" : formatCurrency(calculated.stake2)}
                      </strong>
                    </div>
                    <div className="stat-card">
                      <p>Arbitrage %</p>
                      <strong>
                        {calculated.arbPct === null ? "-" : `${calculated.arbPct.toFixed(2)}%`}
                      </strong>
                    </div>
                    <div className="stat-card">
                      <p>Total Profit</p>
                      <strong>
                        {calculated.profit === null ? "-" : formatCurrency(calculated.profit)}
                      </strong>
                    </div>
                  </div>
                </div>
              </div>
            </section>
          ) : (
            <section className="neo-card fade-up delay-2">
              <div className="table-header-row">
                <div className="table-header-left">
                  <h2 className="panel-title">{section.title}</h2>
                  {mainTab === "topDown" && (
                    <label className="switch-control">
                      <input
                        type="checkbox"
                        checked={excludeSportsbetUnder}
                        onChange={(event) => {
                          setExcludeSportsbetUnder(event.target.checked);
                          setPage(1);
                        }}
                      />
                      Exclude Sportsbet Unders
                    </label>
                  )}
                  {mainTab === "arbs" && (
                    <div className="arbs-filter-actions">
                      <button type="button" className="clear-filter-btn" onClick={resetArbFilters}>
                        Reset Default Filters
                      </button>
                      <button type="button" className="clear-filter-btn" onClick={applyCommonFilters}>
                        Common Filters
                      </button>
                    </div>
                  )}
                </div>

                <div className="controls-row">
                  {mainTab === "multiLegs" && (
                    <label className="select-control">
                      Match
                      <select
                        value={multiLegMatch}
                        onChange={(event) => {
                          setMultiLegMatch(event.target.value);
                          setPage(1);
                        }}
                        className="field-input"
                      >
                        {matchOptions.map((match) => (
                          <option key={match} value={match}>
                            {match}
                          </option>
                        ))}
                      </select>
                    </label>
                  )}

                  <input
                    type="search"
                    value={search}
                    onChange={(event) => {
                      setSearch(event.target.value);
                      setPage(1);
                    }}
                    placeholder="Search table..."
                    className="field-input search-input"
                  />
                </div>
              </div>

              <div className="subtabs-row">
                {section.tabs.map((tab, idx) => {
                  const active = selectedTab?.id === tab.id;
                  const count = tabCounts[tab.id] ?? 0;
                  return (
                    <button
                      key={tab.id}
                      type="button"
                      onClick={() => {
                        setSubTabs((prev) => ({ ...prev, [mainTab]: tab.id }));
                        setSort({ key: null, dir: "asc" });
                        setSearch("");
                        setPage(1);
                        if (mainTab === "multiLegs") {
                          setMultiLegMatch("All");
                        }
                      }}
                      className={`subtab-btn ${active ? "active" : ""}`}
                      style={{ animationDelay: `${idx * 35}ms` }}
                    >
                      <span>{tab.label}</span>
                      <span className="tab-count-badge">
                        {count.toLocaleString()}
                      </span>
                    </button>
                  );
                })}
              </div>

              <p className="table-meta">
                {sortedRows.length.toLocaleString()} rows ({filteredBaseRows.length.toLocaleString()} before search)
              </p>

              <div className="table-scroll">
                {columns.length === 0 || pagedRows.length === 0 ? (
                  <div className="empty-table">No rows found.</div>
                ) : (
                  <table className="data-table">
                    <thead>
                      <tr>
                        {columns.map((column) => {
                          const active = sort.key === column;
                          const arrow = active ? (sort.dir === "asc" ? " ▲" : " ▼") : "";

                          return (
                            <th key={column}>
                              <button
                                type="button"
                                className="table-sort-btn"
                                onClick={() => {
                                  setSort((prev) => {
                                    if (prev.key === column) {
                                      return { key: column, dir: prev.dir === "asc" ? "desc" : "asc" };
                                    }
                                    return { key: column, dir: "asc" };
                                  });
                                  setPage(1);
                                }}
                              >
                                {column}
                                {arrow}
                              </button>
                            </th>
                          );
                        })}
                        {showCalculatorAction && <th>Calculator</th>}
                        {showMultiLegAction && <th>Multi Legs</th>}
                      </tr>
                    </thead>
                    <tbody>
                      {pagedRows.map((row, index) => {
                        const tierClass = getMarginTierClass(row.margin);
                        const rowKey = `${index}-${String(row.match ?? "")}-${String(row.player_name ?? row.player ?? "")}`;
                        const hasOdds = Number.isFinite(toNumber(row.over_price)) && Number.isFinite(toNumber(row.under_price));
                        const underAgency = String(row.UA ?? "");
                        const hasMultiLegTarget = MULTI_LEG_AGENCIES.includes(underAgency) && Boolean(String(row.match ?? ""));

                        return (
                          <tr key={rowKey} className={tierClass || undefined}>
                            {columns.map((column) => (
                              <td key={`${rowKey}-${column}`}>{formatNumber(row[column])}</td>
                            ))}
                            {showCalculatorAction && (
                              <td>
                                <button
                                  type="button"
                                  className="pager-btn"
                                  disabled={!hasOdds}
                                  onClick={() => jumpToCalculator(row)}
                                >
                                  Use
                                </button>
                              </td>
                            )}
                            {showMultiLegAction && (
                              <td>
                                <button
                                  type="button"
                                  className="pager-btn"
                                  disabled={!hasMultiLegTarget}
                                  onClick={() => jumpToMultiLegs(row)}
                                >
                                  View
                                </button>
                              </td>
                            )}
                          </tr>
                        );
                      })}
                    </tbody>
                  </table>
                )}
              </div>

              <div className="pager-row">
                <button
                  type="button"
                  disabled={currentPage <= 1}
                  onClick={() => setPage((prev) => Math.max(1, prev - 1))}
                  className="pager-btn"
                >
                  Prev
                </button>
                <span className="pager-text">
                  Page {currentPage} of {totalPages}
                </span>
                <button
                  type="button"
                  disabled={currentPage >= totalPages}
                  onClick={() => setPage((prev) => Math.min(totalPages, prev + 1))}
                  className="pager-btn"
                >
                  Next
                </button>
              </div>

              {dataError && <p className="error-text">Data load error: {dataError}</p>}
            </section>
          )}
        </div>
      </main>
    </div>
  );
}
