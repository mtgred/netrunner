import assert from "node:assert/strict";
import {readFileSync} from "node:fs";
import test from "node:test";

const logSource = readFileSync("src/cljs/nr/gameboard/log.cljs", "utf8");
const gameboardStyles = readFileSync("src/css/gameboard.styl", "utf8");

test("game log exposes a scroll-to-bottom notification", () => {
  assert.match(
    logSource,
    /defn update-scroll-state![\s\S]*reset! scrolled-away-from-end\? \(not \(scrolled-to-end\? el 15\)\)/,
  );
  assert.match(logSource, /:on-scroll\s+#\(update-scroll-state!/s);
  assert.match(logSource, /when @scrolled-away-from-end\?/);
  assert.match(logSource, /button\.log-scroll-to-bottom/);
  assert.match(
    logSource,
    /:on-click\s+#\(when-let[\s\S]*set! \(\.-scrollTop n\) \(\.-scrollHeight n\)[\s\S]*reset! scrolled-away-from-end\? false/,
  );
  assert.match(gameboardStyles, /\.log-scroll-to-bottom/);
});
