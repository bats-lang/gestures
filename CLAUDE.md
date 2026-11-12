# gestures

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
