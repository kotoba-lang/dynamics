#!/usr/local/bin/python3
"""cron-econ-audit.py - fleet-wide Hermes cron token economics audit.

no_agent script: stdout becomes the job report (empty stdout = silent).
Measures per-profile cron token spend (sessions source='cron'), runs/day
from jobs.json, success/fail from executions.db - last N days. Propose-only:
never edits jobs, only reports.

Env: CRON_ECON_DAYS (default 7), CRON_ECON_TOP (default 25)
"""
import json
import os
import sqlite3
import sys
import time
import collections

PROFILES_DIR = os.path.expanduser("~/.hermes/profiles")
DAYS = int(os.environ.get("CRON_ECON_DAYS", "7"))
TOP = int(os.environ.get("CRON_ECON_TOP", "25"))
SELF = "fleet-alloc"


def runs_per_day(s):
    if not isinstance(s, dict):
        return 0.0
    if s.get("kind") == "interval":
        return 24 * 60 / max(s.get("minutes", 0), 1)
    if s.get("kind") == "once":
        return 1.0
    if s.get("kind") == "cron":
        e = s.get("expr", "").split()
        if len(e) != 5:
            return 0.0
        def count(field, lo, hi):
            if field == "*":
                return hi - lo + 1
            tot = 0
            for part in field.split(","):
                if "/" in part:
                    a, step = part.split("/", 1)
                    step = int(step)
                    if a in ("*", ""):
                        rng = range(lo, hi + 1)
                    elif "-" in a:
                        x, y = a.split("-", 1)
                        rng = range(int(x), int(y) + 1)
                    else:
                        rng = range(int(a), hi + 1)
                    tot += len([x for x in rng if (x - lo) % step == 0])
                elif "-" in part:
                    x, y = part.split("-", 1)
                    tot += int(y) - int(x) + 1
                else:
                    tot += 1
            return tot
        return float(count(e[0], 0, 59) * count(e[1], 0, 23))
    return 0.0


def collect():
    since = time.time() - DAYS * 86400
    tok = collections.defaultdict(lambda: [0, 0, 0.0])
    execs = {}
    for d in sorted(os.listdir(PROFILES_DIR)):
        prof = d
        if prof == SELF or ".bak" in prof:
            continue
        sd = os.path.join(PROFILES_DIR, d, "state.db")
        if not os.path.exists(sd):
            continue
        try:
            con = sqlite3.connect("file:%s?mode=ro" % sd, uri=True)
            try:
                rows = con.execute(
                    "SELECT SUM(input_tokens), SUM(output_tokens),"
                    " SUM(estimated_cost_usd) FROM sessions"
                    " WHERE source='cron' AND started_at > ?",
                    (since,)).fetchone()
            except sqlite3.OperationalError:
                con.close()
                continue
            if rows and (rows[0] or rows[2]):
                t = tok[prof]
                t[0] += rows[0] or 0
                t[1] += rows[1] or 0
                t[2] += rows[2] or 0.0
            con.close()
        except sqlite3.Error:
            continue
    for d in sorted(os.listdir(PROFILES_DIR)):
        prof = d
        if prof == SELF or ".bak" in prof:
            continue
        edb = os.path.join(PROFILES_DIR, d, "cron", "executions.db")
        if not os.path.exists(edb):
            continue
        try:
            con = sqlite3.connect("file:%s?mode=ro" % edb, uri=True)
            st = dict(con.execute(
                "SELECT status, COUNT(*) FROM executions"
                " WHERE claimed_at > datetime('now', ?) GROUP BY status",
                ("-%d days" % DAYS,)).fetchall())
            if st:
                execs[prof] = st
            con.close()
        except sqlite3.Error:
            continue
    return tok, execs


def main():
    tok, execs = collect()
    cand = []
    for prof, (inp, outp, cost) in tok.items():
        jf = os.path.join(PROFILES_DIR, prof, "cron", "jobs.json")
        freq, njobs = 0.0, 0
        if os.path.exists(jf):
            try:
                with open(jf) as fh:
                    jobs = json.load(fh).get("jobs", [])
                jd = [j for j in jobs if isinstance(j, dict) and j.get("enabled", True)]
                njobs = len(jd)
                freq = sum(runs_per_day(j.get("schedule", {})) for j in jd)
            except (json.JSONDecodeError, OSError):
                pass
        st = execs.get(prof, {})
        done = st.get("completed", 0)
        fail = st.get("failed", 0)
        total_tok = inp + outp
        cand.append(dict(prof=prof, jobs=njobs, runs_per_day=freq,
                         tok_week=total_tok, tok_run=total_tok / done if done else 0,
                         cost_week=cost, done=done, fail=fail,
                         fail_rate=fail / (done + fail) if (done + fail) else None))
    cand.sort(key=lambda c: -c["tok_week"])
    tok_week = sum(c["tok_week"] for c in cand)
    cost_week = sum(c["cost_week"] for c in cand)
    lines = ["## cron token economics - fleet audit (last %dd)" % DAYS,
             "profiles measured: %d / cron tok: %s / est cost: $%.2f (~$%.2f/day)"
             % (len(cand), format(int(tok_week), ","), cost_week,
                cost_week / max(DAYS, 1))]
    lines.append("")
    lines.append("## top %d by token spend" % min(TOP, len(cand)))
    lines.append("profile | runs/day | tok/run | tok | $ | done/fail")
    for c in cand[:TOP]:
        fr = "%.0f%%" % (c["fail_rate"] * 100) if c["fail_rate"] is not None else "-"
        lines.append("%s | %.0f | %s | %s | $%.2f | %d/%d (%s)" % (
            c["prof"], c["runs_per_day"], format(int(c["tok_run"]), ","),
            format(int(c["tok_week"]), ","), c["cost_week"],
            c["done"], c["fail"], fr))
    alerts = [c for c in cand
              if (c["fail_rate"] is not None and c["fail_rate"] >= 0.5
                  and (c["done"] + c["fail"]) >= 5)
              or (c["tok_run"] > 500000 and c["runs_per_day"] >= 24)]
    if alerts:
        lines.append("")
        lines.append("## alerts (%d)" % len(alerts))
        for c in alerts:
            if c["fail_rate"] is not None and c["fail_rate"] >= 0.5:
                lines.append("%s: fail rate %.0f%% (%d/%d) - failing runs still burn tokens"
                             % (c["prof"], c["fail_rate"] * 100, c["done"], c["fail"]))
            else:
                lines.append("%s: %s tok/run x %.0f runs/day - freq x unit waste"
                             % (c["prof"], format(int(c["tok_run"]), ","),
                                c["runs_per_day"]))
    print("\n".join(lines))
    ledger = os.path.join(PROFILES_DIR, SELF, "workspace", "cron-econ-ledger.jsonl")
    os.makedirs(os.path.dirname(ledger), exist_ok=True)
    with open(ledger, "a") as fh:
        fh.write(json.dumps(dict(
            ts=time.time(), days=DAYS, total_tok=int(tok_week),
            cost=round(cost_week, 4), profiles=len(cand),
            top=[[c["prof"], int(c["tok_week"]), round(c["cost_week"], 4),
                  c["done"], c["fail"]] for c in cand[:10]])) + "\n")


if __name__ == "__main__":
    sys.exit(main())
