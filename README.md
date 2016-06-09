# erdos.metric

A Clojure library for working in metric spaces.

This is a Work In Progress project, please do not use it for anything serious.

# Metric

A metric is a real valued binary function `d : X x X -> [0, oo)` for which the following conditions are met.

1. `d(x,y) >= 0 and d(x,y) = 0 <-> x=y` (positive definite)
2. `d(x,y) = d(y,x)` (symmetry)
3. `d(x,z) <= d(x,y) + d(y,z)` (triangle inequality)

## Namespaces

|  namespace | description |
+------------+-------------+
| `erdos.metric.fn.core` | Common functionality for metrics |
| `erdos.metric.fn.seq`  | Metrics on sequences  |
| `erdos.metric.fn.set` | Metrics on set |
| `erdos.metric.fn.bag` | Metrics on multiset (bag) |
| `erdos.metric.fn.n-gram` | n-gram similarities and metrics |

## License

Copyright © 2016 Janos Erdos

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
