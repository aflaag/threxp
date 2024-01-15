# threxp

The language this program uses is the following:

```
M,N ::= k | x | M + N | M - N | M * N | M / N | $ x = M @ N !
```

where `$ x = M @ N !` is equivalent to `let x = M in N end` (in Standard ML notation), but `M` and `N` are evaluated concurrently.

