# RCBC Shiny Election Suite — Architecture & State

> **Last updated:** 2026-08-04
> **Branch:** `hestia-refactor`
> **Status:** Planning — Phase 0 (pre-refactor)

---

## Current State

### Project Overview

The `rcbc_shiny` project is a Shiny web app for running multi-winner ranked choice elections. It currently runs on `main` and supports:

- **Droop STV** (via the `vote` R package)
- **CPO-STV** (Condorcet Post-Combinatorial Sequential-Tally Voting — custom implementation)
- **Borda count** (with configurable tiebreakers)

The app reads election data from CSV ballot files, allows voters to submit ballots through a drag-and-drop interface, and produces tabulated results with candidate preference plots.

### Repository Structure (Pre-Refactor)

```
rcbc_shiny/
├── app.R                  # 1,311 lines — monolith (UI + server + helpers)
├── cpo_stv.R              # ~805 lines — CPO-STV algorithm (standalone)
├── startApp.R             # 5 lines — launcher
├── renv/                  # renv dependency management
├── renv.lock              # Dependency lock
├── .Rprofile              # RStudio project setup
├── .Rbuildignore
├── .gitignore
└── Sample Data/           # 11 sample election cycles (no cycle 3)
```

### Current Files

| File | Lines | Purpose |
|---|---|---|
| `app.R` | 1,311 | Full UI + server logic, election config, ballot handling, helper functions |
| `cpo_stv.R` | ~805 | CPO-STV election algorithm + Borda + STV tiebreakers |
| `startApp.R` | 5 | Launcher script |

### Known Issues & Decisions

- **No cycle 3 data** — only 11 sample cycles (1, 2, 4-13). Avoid hardcoding cycle counts.
- **Sample data folder naming** — folders use `rcbc-cycle-N` format in the repo, but the app accepts `cycle_N` format
- **Tiebreaker methods** — configurable, including random draw and Borda-based
- **`cpo_stv.R` is already a well-isolated library** with no Shiny dependencies — good candidate for extraction into the `voter` package
- **Phase 14 sample data** was recently added to the repo (commit 8dfce5e)

---

## Refactoring Plan

See the restructure plan document for the full plan.

### Branching Strategy

All work branches from and merges into `hestia-refactor`. PRs go to `hestia-refactor`, never `main`.

```
main ────────────────────────────────────
  └── hestia-refactor ───────────────────
        ├── scaffold              ← Phase 1
        ├── modules               ← Phase 2
        ├── business-logic        ← Phase 3
        ├── voter-package         ← Phase 4
        ├── tests                 ← Phase 5
        ├── containerize          ← Phase 6
        └── ci-cd                 ← Phase 7
```

### Living Documentation

This file is the agent's working memory. Updated at each phase boundary. Never committed to the repo.

---

## Development Workflow

### Safety Rules

- **Never commit directly to `main`** — all changes through PRs
- All commits use the GitHub App identity (Hermes)
- No code touches `main` until full refactor approved via `hestia-refactor`

### Container Setup

- **Production:** Standalone Shiny container on port 18323
- **Development:** RStudio container on port 8788
- Both use `rocker/` base images

---

## Session Log

| Date | Session | Action |
|---|---|---|
| 2026-07-31 | Initial review | Reviewed `app.R` (1,311 lines) and `cpo_stv.R` (~805 lines); produced restructuring plan |
| 2026-08-03 | Bibliography | Ingested 13 references into GBrain; drafted comprehensive restructure plan v0.4.0 |
| 2026-08-04 | Planning | Defined branching strategy (`hestia-refactor` → `main`), living documentation pattern (`.local/ARCHITECTURE.md`), and created initial architecture document |
