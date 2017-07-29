
def foo() = (1, 3)
def bar() = (foo, foo)

val ((lo, lc), (ro, rc)) = bar
