# gestures

## CI is pinned

Every input to CI is pinned in the source (bats-lang/repository-prototype#269),
so a commit that passes keeps passing:

* `bats.lock` is committed: CI never runs `bats lock` for the package, so
  `bats check` and `bats build` fetch exactly the locked versions, and fail
  when the lock is missing or does not match `bats.toml`. A library's lock
  pins only its own CI and tests; its dependents still resolve their own.
* The compiler is the commit in `.github/bats-version`, read by every
  workflow that builds bats (and by the publish workflow).
* The package repository is fetched at the commit in
  `.github/repository-version`, so what the test packages under `tests/`
  lock (`bats lock --dev`) is pinned too.
* `publish.yml` and `relock-pins.yml` in bats-lang/repository-prototype are
  called by commit, never `@main`.

Pins move only through a reviewed pull request that runs the same CI. The
daily `relock.yml` (the shared `relock-pins.yml`) relocks against the
newest, moves the compiler and repository pins, pushes `relock/<date>`,
opens a pull request listing the old and new versions and dispatches
`check.yml` on it, so a breaking publish shows as a red relock pull
request and main stays green. GITHUB_TOKEN cannot change workflow files,
so without a `RELOCK_TOKEN` secret that pull request lists a workflow pin
that would move instead of moving it: move it in a pull request of its
own. A pull request that needs newer packages runs `bats lock
--repository <dir>` and commits `bats.lock` (and
`.github/repository-version`) with the change.

## The proofs are mutation-checked under the infinite-precision solver (bats-lang/quire#594)

The 32-bit solver can make a proof's context contradictory, so a lemma proves nothing. Every lemma of
`src/classify.bats` and `src/pointer.bats` was checked with a false variant of its statement, which
the compiler must reject (quire's `tests/static/mutation_check.py`, from a clone of bats-lang/quire,
with the gmp build as `--ats-home`): all 25 mutants but the two `[false] void` probes are rejected, and
those two (`tests/static/mutation_known.txt`) have a reject fixture instead
(`class-exclusive-same-class`, `locked-unlocked-both-locked`, run by `tests/static/run.sh`). A new lemma
comes with its false variant as a reject fixture, and is run through the checker (`--known` this file).
```
python3 quire/tests/static/mutation_check.py --root . --run WORK --ats-home GMP_HOME/.bats/ats2 \
  --inplace --known tests/static/mutation_known.txt src/classify.bats src/pointer.bats
```
