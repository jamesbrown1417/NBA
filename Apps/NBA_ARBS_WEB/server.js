const http = require("http");
const fs = require("fs");
const path = require("path");
const { spawn } = require("child_process");

const host = "127.0.0.1";
const port = Number(process.env.PORT || 4173);
const appDir = __dirname;
const repoRoot = path.resolve(appDir, "..", "..");
const refreshScript = path.join(repoRoot, "Scripts", "export_nba_arbs_web_data.R");

let refreshInProgress = false;

const MIME_TYPES = {
  ".html": "text/html; charset=utf-8",
  ".js": "application/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".ico": "image/x-icon",
  ".txt": "text/plain; charset=utf-8"
};

function writeJson(res, status, payload) {
  res.writeHead(status, {
    "Content-Type": "application/json; charset=utf-8",
    "Cache-Control": "no-store"
  });
  res.end(JSON.stringify(payload));
}

function runRefreshScript() {
  return new Promise((resolve, reject) => {
    const child = spawn("Rscript", [refreshScript], {
      cwd: repoRoot,
      stdio: ["ignore", "pipe", "pipe"]
    });

    let stdout = "";
    let stderr = "";

    child.stdout.on("data", (chunk) => {
      stdout += chunk.toString();
    });

    child.stderr.on("data", (chunk) => {
      stderr += chunk.toString();
    });

    child.on("error", (error) => {
      reject(error);
    });

    child.on("close", (code) => {
      if (code !== 0) {
        reject(new Error(stderr || stdout || `Rscript exited with ${code}`));
        return;
      }
      resolve({ stdout, stderr });
    });
  });
}

function serveStatic(req, res, pathname) {
  const requestedPath = pathname === "/" ? "/index.html" : pathname;
  const normalized = path.normalize(decodeURIComponent(requestedPath)).replace(/^(\.\.[/\\])+/, "");
  const filePath = path.join(appDir, normalized);

  if (!filePath.startsWith(appDir)) {
    res.writeHead(403, { "Content-Type": "text/plain; charset=utf-8" });
    res.end("Forbidden");
    return;
  }

  fs.readFile(filePath, (error, data) => {
    if (error) {
      res.writeHead(404, { "Content-Type": "text/plain; charset=utf-8" });
      res.end("Not found");
      return;
    }

    const ext = path.extname(filePath);
    const contentType = MIME_TYPES[ext] || "application/octet-stream";
    const cacheControl = ext === ".json" ? "no-store" : "no-cache";

    res.writeHead(200, {
      "Content-Type": contentType,
      "Cache-Control": cacheControl
    });
    res.end(data);
  });
}

const server = http.createServer(async (req, res) => {
  const url = new URL(req.url, `http://${host}:${port}`);
  const pathname = url.pathname;

  if (pathname === "/api/health" && req.method === "GET") {
    writeJson(res, 200, { ok: true, refreshSupported: true });
    return;
  }

  if (pathname === "/api/refresh" && req.method === "POST") {
    if (refreshInProgress) {
      writeJson(res, 409, { success: false, error: "Refresh already in progress." });
      return;
    }

    refreshInProgress = true;
    try {
      const result = await runRefreshScript();
      writeJson(res, 200, {
        success: true,
        message: "Refresh complete.",
        output: result.stdout.trim()
      });
    } catch (error) {
      writeJson(res, 500, {
        success: false,
        error: error.message
      });
    } finally {
      refreshInProgress = false;
    }
    return;
  }

  if (req.method === "GET") {
    serveStatic(req, res, pathname);
    return;
  }

  res.writeHead(405, { "Content-Type": "text/plain; charset=utf-8" });
  res.end("Method not allowed");
});

server.listen(port, host, () => {
  console.log(`NBA Arbs web app: http://${host}:${port}`);
});

server.on("error", (error) => {
  console.error(`Failed to start server on ${host}:${port}: ${error.message}`);
  process.exit(1);
});
