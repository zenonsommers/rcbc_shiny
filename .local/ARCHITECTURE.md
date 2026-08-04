# RCBC Shiny Election Suite — Architecture & State

> **Last updated:** 2026-08-04
> **Branch:** `hestia-refactor`
> **Status:** Phase 1 Complete (scaffold)

---

## Current State

### Project Overview

The `rcbc_shiny` project is a Shiny web app for running multi-winner ranked choice elections. It currently runs on `main` and supports:

- **Droop STV** (via the `vote` R package)
- **CPO-STV** (Condorcet Post-Combinatorial Sequential-Tally Voting — custom implementation)
- **Borda count** (with configurable tiebreakers)

The app reads election data from CSV ballot files, allows voters to submit ballots through a drag-and-drop interface, and produces tabulated results with candidate preference plots.

### Repository Structure (Post-Scaffold)

```
rcbc_shiny/
├── app.R                    # Thin entry point (15 lines)
├── global.R                 # Libraries, constants, helpers
├── ui.R                     # Main UI definition
├── server.R                 # Server logic (module instantiation)
├── cpo_stv.R                # Election engine (to move to voter/)
├── startApp.R               # Launcher
├── renv/                    # renv dependency management
├── renv.lock
├── .Rprofile
├── .Rbuildignore
├── .gitignore
│
├── R/                       # Module files
│   ├── mod-ui-components.R       # Shared widgets (modals, rank list)
│   ├── mod-ui-hub.R              # Hub page UI
│   ├── mod-ui-create.R           # Create election UI
│   ├── mod-ui-ballot.R           # Ballot submission UI
│   ├── mod-ui-process.R          # Process results UI
│   ├── mod-edit.R                # Edit election UI
│   ├── mod-electron.R            # Reactive state definitions
│   ├── mod-dark-mode.R           # Dark mode toggle UI
│   ├── mod-plots.R               # Plot rendering UI
│   ├── mod-processing.R          # Tabulation UI
│   ├── mod-results.R             # Results display UI
│   ├── mod-create-server.R       # Create election server
│   ├── mod-ballot-server.R       # Ballot submission server
│   ├── mod-edit-server.R         # Edit election server
│   ├── mod-hub-server.R          # Hub navigation server
│   ├── mod-dark-mode-server.R    # Dark mode server
│   ├── mod-file-io-server.R      # File I/O server
│   ├── mod-file-uploader-server.R# CSV upload server
│   ├── mod-plots-server.R        # Plot rendering server
│   ├── mod-processing-server.R   # Tabulation server
│   ├── mod-results-server.R      # Results display server
│   ├── mod-electron-server.R     # State manager server
│   └── mod-ballot-data.R         # Ballot data reactive helper
│
├── voter/               # R package — election engine
│   ├── DESCRIPTION
│   ├── NAMESPACE
│   ├── README.md
│   ├── LICENSE
│   ├── R/               # (skeleton — to be populated from cpo_stv.R)
│   ├── tests/testthat/
│   └── vignettes/
│       ├── introduction.Rmd
│       └── tiebreaking.Rmd
│
├── tests/
│   └── testthat/
│       └── testthat.R
│
├── docker/              # (to be created in Phase 6)
└── Sample Data/         # 12 sample election cycles
```

### Refactoring Progress

| Phase | Status | Description |
|-------|--------|-------------|
| Phase 0: Planning | ✅ Complete | Architecture doc, restructure plan v0.4.0 |
| Phase 1: Scaffold | ✅ **In Progress** | Directory structure, entry points, module skeletons |
| Phase 2: Modules | ⏳ Pending | Migrate `app.R` logic into modules |
| Phase 3: Business Logic | ⏳ Pending | Extract `cpo_stv.R` into `voter/` package |
| Phase 4: Tests | ⏳ Pending | Unit tests for `voter::`, integration tests |
| Phase 5: Containerize | ⏳ Pending | Docker setup |
| Phase 6: CI/CD | ⏳ Pending | GitHub Actions workflows |
| Phase 7: Final Merge | ⏳ Pending | Merge `hestia-refactor` → `main` |

### Known Issues & Decisions

- **No cycle 3 data** — only 11 sample cycles (1, 2, 4-13). Avoid hardcoding cycle counts.
- **Sample data folder naming** — folders use `rcbc-cycle-N` format in the repo, but the app accepts `cycle_N` format
- **Tiebreaker methods** — configurable, including random draw and Borda-based
- **`cpo_stv.R` is already a well-isolated library** with no Shiny dependencies — good candidate for extraction into the `voter` package
- **Phase 14 sample data** was recently added to the repo (commit 8dfce5e)

---

## Refactoring Plan

See `.local/RESTRUCTURE-PLAN.md` for the full plan (v0.4.0, 805 lines).

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

### Safety Rules

- **Never commit directly to `main`** — all changes through PRs
- All commits use the GitHub App identity (Hermes)
- No code touches `main` until full refactor approved via `hestia-refactor`

### Living Documentation

This file is the agent's working memory. Updated at each phase boundary. Never committed to the repo.

---

## Session Log

| Date | Session | Action |
|------|---------|--------|
| 2026-07-31 | Initial review | Reviewed `app.R` (1,311 lines) and `cpo_stv.R` (~805 lines); produced restructuring plan |
| 2026-08-03 | Bibliography | Ingested 13 references into GBrain; drafted comprehensive restructure plan v0.4.0 |
| 2026-08-04 | Planning | Defined branching strategy (`hestia-refactor` → `main`), living documentation pattern |
| 2026-08-04 | Phase 1 | Rebasing onto main, created full scaffold: `app.R` entry point, `global.R`, `ui.R`, `server.R`, `R/` modules (17 files), `voter/` package skeleton, tests dir |