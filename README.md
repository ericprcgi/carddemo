# CardDemo — Card Clearing Platform

Card clearing platform of **NordCard Clearing AB**, the card processing and clearing utility jointly owned by its member banks. CardDemo processes card transactions on behalf of the member banks and performs the daily clearing of the flows between them: card accounts, daily transaction posting, interest calculation, statements, member reconciliation reporting, and the settlement material sent to the central bank.

> **Before changing anything in this repository**, read the *Change Management Process* page in the CardDemo Confluence space. Every change requires a Jira issue in project **CARD**, a feature branch, and a Pull Request. This platform is shared financial infrastructure — its output feeds settlement at the central bank.

## Repository layout

| Path | Contents |
|---|---|
| `app/cbl/` | COBOL source. `CB*` = batch programs, `CO*` = CICS online programs |
| `app/cpy/` | Copybooks (record layouts, commareas) |
| `app/cpy-bms/` | Symbolic maps for CICS screens |
| `app/jcl/` | Batch job JCL (`POSTTRAN`, `INTCALC`, `TRANREPT`, `CREASTMT`, file define/load) |
| `app/bms/` | BMS screen definitions |
| `app/proc/`, `app/ctl/`, `app/csd/` | Procedures, control cards, CICS resource definitions |
| `app/scheduler/` | Control-M / CA7 scheduling definitions |
| `app/data/` | Sample/EBCDIC seed data |

## Key programs

| Program | Runs in | Purpose |
|---|---|---|
| `CBTRN02C` | Batch job `POSTTRAN` | Validates and posts the daily transaction file; writes rejects |
| `CBACT04C` | Batch job `INTCALC` | Interest calculation |
| `CBTRN03C` | Batch job `TRANREPT` | Daily transaction report (member reconciliation + settlement material) |
| `CBSTM03A/B` | Batch job `CREASTMT` | Monthly statements |
| `COSGN00C`, `COMEN01C`, … | CICS | Sign-on, menus, account/card/transaction screens |

Record layouts for the transaction chain live in `app/cpy/CVTRA*.cpy` — documented in the *Data Dictionary* page in Confluence, together with the **layout-change rules** (fixed record lengths; new fields are appended at the end, carved from the trailing `FILLER`).

## Development conventions

- COBOL fixed format: code in columns 7–72 only. Match the existing style; do not reformat lines you are not changing.
- Branches: `feature/CARD-<issue>-<short-description>`; PR titles: `CARD-<issue>: <summary>`.
- Add a short comment with the Jira issue key above modified sections.
- Keep diffs minimal and reviewable — this platform settles money between banks.

## Documentation & work tracking

- **Confluence:** space *CardDemo — Card Clearing Platform* (system overview, batch architecture, program guides, data dictionary, runbooks)
- **Jira:** project **CARD**

---
*Based on the AWS CardDemo sample application (Apache-2.0). Adapted as NordCard Clearing's demo estate.*
