# Difference Bound Matrices

[Difference Bound Matrices](https://en.wikipedia.org/wiki/Difference_bound_matrix) are a data structure that are used to represent certain kinds of convex-polygons called *Zones*.

Suppose $m \in \mathbb{N}$. Then, a *Zone* $Z$ is a subset of $\mathbb{R}^m$ that lies in the intersection of constraints like these:
- $\forall x \in Z, c_i \prec \pi_i(x) \prec d_i$
- $\forall x \in Z, c_{ij} \prec \pi_i(x) \prec \pi_j(x) + d_{ij}$

where $\pi_{i}$ is a projection, and $\prec$ is either $\leq$ or $<$, and $c_i, d_i, c_{ij}, d_{ij} \in \mathbb{R}$.

A zone can be represented as a Difference Bound Matrix (DBM) $M$ of size $m + 1 \times m + 1$ where $M_{ij} = (\prec, c_{ij})$.

These data structures occur in the context of [Timed Automata](https://en.wikipedia.org/wiki/Timed_automaton), where reachable sets of clock valuations are usually a (finite) union of zones.

Some common queries on DBMs are:
- Checking if a point is in the zone
- Checking if a zone is (non)empty
- Checking if a zone satisfies a guard (normally of the form, $\forall x \in Z, \pi_i(x) \prec d_i$)
- Checking if $Z_1 \subseteq Z_2$

Some common operations on DBMs are:
- Constraining via a new guard
- Taking the intersection of two zones
- Letting time elapse, i.e, removing the upper bounds $\pi_i(x) \prec d_i$
- Resetting, i.e, forcing the constraint $\pi_i(x) = k_i$

Other implementations:
- [UPPAL DBM Library](https://github.com/UPPAALModelChecker/UDBM)
- [A Rust Implementation](https://github.com/koehlma/momba/blob/ffbc3d172c3e633537f2a23617072bcce68462d8/engine/crates/clock-zones/src/zones.rs)
- [Masaki Waga's Implementation](https://github.com/Agnishom/qtpm/blob/master/src/dbm.hh)

Some resources:
- [Wikipedia Page](https://en.wikipedia.org/wiki/Difference_bound_matrix)
- [Some Lecture Slides](https://moves.rwth-aachen.de/wp-content/uploads/WS1617/amc/amc16_lec20_reduced.pdf)
- [Section 4 of *Timed Automata: Semantics, Algorithms and Tools*](http://people.irisa.fr/Nicolas.Markey/PDF/Papers/lncs3098-BY.pdf)
- [Srivathsan's Timed Automata Lectures](https://www.cmi.ac.in/~sri/Courses/TA/2022/index.html)