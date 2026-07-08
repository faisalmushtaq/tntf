# Data assumptions

Everything the pipeline assumes about the raw team sheets, in one place.
If a future sheet breaks one of these, the parser will usually flag it in
`data/parse_issues.csv` rather than guess silently.

## Scores and results

1. **The number in brackets after a team name is that team's goals.**
   e.g. `Bibs (3)` means Bibs scored 3, even when seven players are listed
   below. Squad sizes are always derived from the listed names.
2. The winner is whichever side scored more; equal scores are draws.
3. A team header without a bracketed number is parsed with a missing score
   and flagged (`warning`), never guessed.

## Dates

4. Dates are **day/month/year**. Two-digit years are 20xx.
5. Text after the date on the same line (e.g. `9/6/26- the fight`) is stored
   as a match note and surfaced in the events table.
6. One match per date; duplicate dates would get suffixed match ids.

## Names and aliases

7. Aliases are resolved case-insensitively after stripping bullets, smart
   quotes, invisible unicode and trailing punctuation.
8. `data-raw/player_aliases.csv` is the single source of truth for mapping.
   Notable entries:
   - `Rik` → **Riz Khan** (assumed typo, 7/4/26)
   - `Comical A-Lee` → **Lee** (nickname, 24/3/26)
   - `Lee's mate` → **Lee's Mate**, marked as a guest
   - `Harris` → **Haris Farooq** (spelling)
   - `Dec` / `Declan` → **Declan** (surname unknown)
9. **Ambiguous aliases** (currently just `Tom`) may map to several canonical
   players. Resolution within a match:
   - candidates already named elsewhere in the same match are eliminated;
   - if the alias appears exactly as many times as the remaining candidates
     (the `Tom / Tom` case on 27/1/26), each occurrence takes one candidate;
   - a single remaining candidate resolves directly;
   - anything else is left unresolved, excluded from statistics and flagged
     as an `error` for a human.
   Every automatic decision is logged (`info`) in the parse issues.
10. A raw name that matches no alias becomes a **new canonical player**
    (flagged as `new-player` in `appearances.resolution`), so newcomers never
    need code changes.
11. If the same canonical player ends up twice in one match (e.g. `Matt` and
    `Matthew Eastwood` both listed), duplicates are dropped and flagged.

## Events

12. Parenthetical notes after a player's name (e.g. `(red card)`) become
    rows in `events`, linked to the player and match.

## Statistics

13. Win percentages carry **95% Wilson score intervals**, which behave well
    for the small samples involved. Players with fewer than 8 appearances
    are flagged `small_sample` and marked `*` in report tables.
14. "Goals for/against" at player level are the goals of the player's *team*
    while they were on the pitch — individual scorers are not recorded in
    the source data.
15. Attendance consistency = appearances ÷ matches played since the player's
    first appearance.
16. Network edges connect players who shared a team; weights are matches
    together; communities use Louvain on the weighted graph; betweenness and
    closeness use 1/weight as distance so frequent teammates are "close".
