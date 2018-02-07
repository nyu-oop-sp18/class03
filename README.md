# Class 3

## Higher-Order Functions and Collections in Scala

In functional programming languages, functions are typically treated
as "*first-class citizens*". That is, functions may take other
functions as arguments and may again produce functions as return
values. A function that takes another function as argument is called a
*higher-order function*.

Higher-order functions provide a powerful mechanism for abstracting
over common computation patterns in programs. This mechanism is
particularly useful for designing libraries with rich interfaces
that support callbacks to client code. We will study these mechanisms
using the example of Scala's collection libraries.

### Higher-Order Functions in Scala

Before we dive into the intricacies of Scala collections, let us first
see how higher-order functions are defined in Scala. To this end,
suppose that we want to write a function `sumInts`
that takes the bounds `a` and `b` of a (half-open)
interval `[a,b)` of integer numbers and computes the sum of the
values in that interval. For example, `sumInts(1, 4)` should
yield `6`. The following recursive implementation does what
we want:

```scala
def sumInts(a: Int, b: Int) = {
  if (a < b) a + sum(f, a + 1, b) else 0
}
```

Now, consider the following function `sumSqrs` that computes
the sum of the squares of the numbers in an interval `[a,b)`:

```scala
def sumSqrs(a: Int, b: Int) = {
  if (a < b) a * a + sum(f, a + 1, b) else 0
}
```

The functions `sumInts` and `sumSqrs` are almost
identical. They only differ in the summand that is added in each
recursive call. In the case of `sumInts` it is `a`, and
in the case of `sumSqrs`, it is `a * a`. We can write
a higher-order function `sum` that abstracts from these
differences. The function `sum` takes another function
`f` as additional parameter. The function `f` captures
the computation that is performed in the summand: 

```scala
def sum(f: Int => Int, a: Int, b: Int) = {
  if (a < b) f(a) + sum(f, a + 1, b) else 0
}
```

The *function type* `Int => Int` of the parameter `f`
indicates that `f` is a function that takes a value of type
`Int` and maps it again to an `Int`.

We can now define the function `sumSqrs` by first defining a
function `square` that squares an integer number, and then
applying `sum` to `square`:

```scala
def square(i: Int) = i * i
def sumSqrs(a: Int, b: Int) = sum(square, a, b)
```

In order to make the use of higher-order functions more convenient,
Scala supports writing anonymous functions (aka function literals,
closures, and lambda abstractions). In
Scala, anonymous functions take the general form:

```scala
(x1: T1, ..., xn: Tn) => body
```

where the `xi` are the parameters of the function, the
`Ti` are the associated types, and `body` is the
body of the function. We can thus define the functions `sumInts`
and `sumSqrs` using anonymous functions as follows:

```scala
def sumInts(a: Int, b: Int) = sum((i: Int) => i, a, b)
def sumSqrs(a: Int, b: Int) = sum((i: Int) => i * i, a, b)
```

### Curried Functions in Scala

Reconsider our definition of `sumInts` and `sumSqrs` in
terms of `sum`:

```scala
def sumInts(a: Int, b: Int) = sum((i: Int) => i, a, b)
def sumSqrs(a: Int, b: Int) = sum((i: Int) => i * i, a, b)
```

One annoyance with these definitions is that we have to redeclare the
parameters `a` and `b` which are simply passed to
`sum`. We can avoid this by redefining `sum` as a function that first
takes the function parameter `f` and then returns another function
that takes the remaining parameters `a` and `b`. This technique is
referred to as *currying*.

There are various ways to define curried functions in Scala. One way
is to define the nested function explicitly by name using a nested
`def` declaration and then returning that function:

```scala
def sum(f: Int => Int): (Int, Int) => Int = {
  def sumHelp(a: Int, b: Int): Int = {
    if (a < b) f(i) + sum(f)(a+1, b) else 0
  }
  sumHelp
}
```

Using the curried version of `sum`, the definition of `sumInts` and
`sumSqrs` can be simplified like this:

```scala
def sumInts = sum((i: Int) => i)
def sumSqrs = sum((i: Int) => i * i)
```

Note that when we apply curried higher-order functions to anonymous
functions, then the compiler can often infer the parameter types. This
simplifies the definitions even further: 

```scala
def sumInts = sum(i => i)
def sumSqrs = sum(i => i * i)
```

In our curried version of `sum`, the function `sumHelp`
is not recursive and is directly returned after being declared. We can
thus simplify the definition of `sum` further by turning
`sumHelp` into an anonymous function:

```scala
def sum(f: Int => Int): (Int, Int) => Int = 
  (a: Int, b: Int) => {
    if (a < b) f(i) + sum(f)(a+1, b) else 0
  }
```

Since curried functions are so common in functional programs, the
Scala language provides a special syntax for them. Instead of nesting
the function declarations, we can write a curried function by
providing the parameters of the nested function in a separate
parameter list:

```scala
def sum(f: Int => Int)(a: Int, b: Int): Int = 
  if (a < b) f(a) + sum(f)(a, b) else 0
```

If we partially apply a curried function written in this form, we have
to make this explicit by appending the underscore symbol `_`
to the partial application. The definitions of
`sumInts` and `sumSqrs` thus look as follows in this
case:

```scala
def sumInts = sum(i => i)_
def sumSqrs = sum(i => i * i)_
```

### Function Objects

Recall that Scala supports *function objects*, which are objects that
provide a method named `apply`. Given such an object `fun`, the Scala
expression `fun(e)` simply expands to a call to the `apply` method of
`fun`: `fun.apply(e)`. 

The Scala compiler uses this mechanism to reduce anonymous functions
to objects during compilation. That is, whenever we create an
anonymous function, the compiler instead creates a function object and
whenever we pass a function to another function, we instead pass an
object reference.

For this purpose, the Scala standard library provides predefined
abstract classes for representing function objects. These classes take
the form `FunctionN` where `N` indicates the number of arguments that
the implemented function takes. For instance, here are the
corresponding class declarations for function objects that implement
functions with one, respectively, two arguments:

```scala
trait Function1[T1, R] {
  def apply(v1: T1): R
}

trait Function2[T1, T2, R] {
  def apply(v1: T1, v2: T2): R
}
```

Note the keyword `trait` here. Traits provide a form of multiple
inheritance in Scala. We will talk about this feature in more detail
later. For now, you can simply think of a trait as an abstract class.

The declarations of `Function1` and `Function2` are parametric in the
types of the arguments to the function and the type of the return
value.

Now reconsider our curried version of the `sum` function:

```scala
def sum(f: Int => Int): (Int, Int) => Int = 
  (a: Int, b: Int) => {
    if (a < b) f(i) + sum(f)(a+1, b) else 0
  }
  
def sumSqrs = sum((x: Int) => x * x)
```

Using the traits `Function1` and `Function2` we can *desugar* the two
anonymous functions in the declarations of `sum` and `sumSqrs` into
ordinary objects:

```scala
def sum(f: Function1[Int, Int]): Function2[Int, Int, Int] =
  new Function2[Int, Int, Int] {
    def apply(a: Int, b: Int): Int =
      if (a < b) f(a) + sum(f, a + 1, b) else 0
  }
  
def sumSqrs = sum(new Function1[Int, Int] { def apply(x: Int) = x * x })
```

This desugared implementation uses anonymous class declarations, which
is a feature that you also find in other OOP lanuages, including
Java. Note that the compiler effectively lifts anonymous class
declarations to regular top-level class declarations. Here is how this
would look like for our example:

```scala
def sum(f: Function1[Int, Int]): Function2[Int, Int, Int] =
  new Anon2(f)

class Anon2(f: Function1[Int, Int]) extends Function2[Int, Int, Int] {
  def apply(a: Int, b: Int): Int =
    if (a < b) f.apply(a) + Sum.sum.apply(f).apply(a + 1, b) else 0
}
  
def sumSqrs = sum(new Anon1())

class Anon1 extends Function1[Int, Int] {
  def apply(x: Int) = x * x
}
```

That is, every anonymous function in a Scala program will simply be
compiled to a top-level class declaration that is instantiated at the
point where the original anonymous function was created. The resulting
program makes only use of standard OOP features. Calls to anonymous
functions are simply translated to calls to the `apply` method of the
corresponding function object. This elimination technique also allows
you to simulate anonymous, higher-order, and curried functions in OOP
languages that do not provide these features directly, albeit at the
cost of producing code that is less syntactically succinct.

### Higher-Order Functions on Lists

A common use case of higher-order functions is to realize
callbacks to client code from within library functions. We discuss
this scenario using the specific example of the class `List`
in the Scala standard library.

#### The `List` data structure

Lists are one of the most important data structures in functional
programming languages. A list is a sequence of data values of some
common element type, e.g., a sequence of integer numbers
`3,6,1,2`. Unlike imperative linked lists, which you have probably
studied in your Data Structures course, lists in functional
programming languages are immutable. As with other immutable data
structures, immutable lists have the advantage that their
representation in memory can be shared across different list
instances. For example, the two lists `1,4,3` and `5,2,4,3` can share
their common sublist `4,3`. This feature enables immutable lists to be
used for space-efficient, high-level implementations of algorithms if
the data structure is used correctly. 

The list data type is defined recursively. We distinguish two cases:
the empty list, which we denote by `Nil`, and a non-empty list `hd ::
tl` (also called a *cons* cell) consisting of the head value `hd` and
a tail list `tl`. We can encode this data type using case classes in
Scala as follows:

```scala
sealed abstract class List {
  def ::(head: Int): List = new ::(head, this)
}
case object Nil extends List
case class ::(head: Int, tail: List) extends List
```

The generic class `List[A]` in the Scala standard library
generalizes this data structure to lists over an arbitrary element
type `A`. The empty list is also denoted by `Nil`
and a cons cell is denoted `hd :: tl`. We can thus
construct Scala lists as follows:

```scala
scala> val l1 = 1 :: (4 :: (2 :: Nil))
l1: List[Int] = List(1, 4, 2)

scala> val l2 = "apple" :: ("banana" :: Nil)
l2: List[String] = List(apple, banana) 
```

Note that any infix operator in Scala that ends in a colon symbol `:`
applies to the right operand. That is the expression `2 :: Nil` is
syntactic sugar for calling the `::` method on `Nil` with argument
`2`: `Nil.::(2)`. These infix operators are also right-associative,
so the parenthesis in the above examples can be omitted:

```scala
scala> val l1 = 1 :: 4 :: 2 :: Nil
l1: List[Int] = List(1, 4, 2)
```

As expected, we can use pattern matching to deconstruct lists into
their components:

```scala
scala> val h :: t = l1
h: Int = 1
t: List[Int] = List(4, 2)

scala> l match {
  case Nil => println("l is empty")
  case hd :: tl => println(s"l's head is $hd.")
}
l's head is 1.
```

Pattern matching also gives us a convenient way to define functions
that operate on lists. For example, the following function computes
the length of a list:

```scala
def length[A](l: List[A]): Int = l match {
  case Nil => 0
  case hd :: tl => 1 + length(tl)
}
```

Note that `length` is a *generic* function that is
parameteric in the type `A` of the elements stored in the
lists.

The next function is more interesting, it takes two lists `l1` and
`l2` and creates a new list by concatenating `l1` and `l2`.

```scala
def append[A](l1: List[A], l2: List[A]): List[A] = 
  l1 match {
  case Nil => l2
  case hd :: tl => hd :: append(tl, l2)
}
```

```scala
scala> append(List(3,4,1), List(2, 6))
res0: List[Int] = List(3,4,1,2,6)
```

The next function reverses a given list using tail-recursion:

```scala
def reverse[A](l: List[A]): List[A] = {
  def rev(l: List[A], acc: List[A]): List[A] = 
    l match {
      case Nil => acc
      case hd :: tl => rev(tl, hd :: acc)
    }
  rev(l, Nil)
}
```

```scala
scala> reverse(List(3,4,1,2))
res0: List[Int] = List(2,1,4,3)
```

#### The `map` function

In the earlier examples we saw that functions operating on lists
follow a common pattern: they traverse the list, decomposing it into
its elements, and then apply some operation to each of the
elements. We can extract these common patterns and implement them in
more general higher-order functions that abstract from the specific
operations being performed on the elements.

A particularly common operation on lists is to traverse a list and
apply some function to each element, obtaining a new list. For
example, suppose we have a list of `Double` values which we
want to scale by a given factor to obtain a list of scaled values. The
following function implements this operation:

```scala
def scale(factor: Double, l: List[Double]): List[Double] =
  l match {
    case Nil => Nil
    case hd :: tl => factor * hd :: scale(factor, tl)
  }
```

A similar operation is implemented by the following function, which
takes a list of integers and increments each element to obtain a new
list:

```scala
def incr(l: List[Int]): List[Int] =
  l match {
    case Nil => Nil
    case hd :: tl => hd + 1 :: incr(tl)
  }
```

The type of operation that is performed by `scale` and
`incr` is called a `map`. We can implement the map
operation as a higher-order function that abstracts from the concrete
operation that is applied to each element in the list:

```scala
def map[A, B](l: List[A])(op: A => B): List[B] = 
  l match {
    case Nil => Nil
    case hd :: tl => op(hd) :: map(tl)(op)
  }
```

The function `map` is parametric in both the element type
`A` of the input list, as well as the element type `B`
of the output list. That is, a map operation transforms a
`List[A]` into a `List[B]` by applying an operation
`op: A => B` to each element in the input list. Note that
the order of the elements in the input list is preserved.

We can now redefine `scale` and `incr` as instances of `map`:
```scala
def scale(factor: Double, l: List[Double]) =
  map(l)(x => factor * x)
def incr(l: List[Int]) = map(l)(x => x + 1)
```

Note that Scala provides a syntactic short form for defining anonymous
functions by replacing variables in expressions with underscores. This
notation is often useful to obtain succinct code when using
higher-order functions. For example, the Scala compiler will
automatically expand the following code to the above definitions of
`scale` and `incr`:

```scala
def scale(factor: Double, l: List[Double]) = 
  map(l)(factor * _)
def incr(l: List[Int]) = map(l)(_ + 1)
```

#### Folding Lists

We have seen that we can often identify common patterns in functions
on data structures and implement them in generic higher-order
functions. We can then conveniently reuse these generic functions,
reducing the amount of code we have to write. In this section, we will
look at the most general patterns for performing operations on
collections, namely `fold operations`.

As a motivating example, consider the following function, which
computes the sum of the values stored in a list of integers

```scala
def sum(l: List[Int]): Int = {
  case Nil => 0
  case hd :: tl => hd + sum(tl) 
}
```

Consider a list `l` of `n` integer values `d1` to `dn`:

```scala
val l = d1 :: d2 :: ...  :: dn :: Nil
```

Then unrolling the recursion of `sum` on `l` yields the
following computation

```scala
d1 + (d2 + ... (d2 + 0)...)
```
That is, in the *i*th recursive call, we add the current head `di` to
the sum of the values in the current tail. Here, we consider the sum
of an empty list `Nil` to be 0. If we represent this
computation as a tree, this tree looks as follows:

```scala
      +
     / \
    d1  +
       / \
      d2 ... 
           \
            +
           / \
          dn  0
```

With this representation, it is easy to see how to generalize from the
specific computation performed by the represented expression. That is,
in the general case we have a list of values of type `A` instead of
`Int`. Then, instead of the specific initial value `0` for the empty
list, we are given an initial value `z` of some type `B`. Finally,
instead of adding the current head to the sum of the current tail of
the list, we apply a generic operation `op` in each step. The
operation `op` takes the current value `di`, which is of type `A`, and
the result of the computation on the tail, which is of type `B`, and
returns again a value of type `B`. The resulting expanded recursive
computation is then represented by the following tree:

```scala
      op
     / \
    d1  op
       / \
      d2 ... 
           \
            op
           / \
          dn  z
```

We refer to this type of computation as a *fold* of the list
because the list is traversed and recursively folded into a single
value. Note that the tree is leaning towards the right. We therefore
refer to this type of fold operation as a *fold-right*. That is,
the recursive computation is performed in right-to-left order of the
values stored in the list.

The following higher-order function implements the fold-right operation:
```scala
def foldRight[A,B](l: List[A])(z: B)(op: (A, B) => B): B =
  l match {
    case Nil => z
    case hd :: tl => op(hd, foldRight(tl)(z)(op))
  }
```
We can now redefine `sum` in terms of `foldRight`:
```scala
def sum(l: List[Int]): Int = foldRight(l)(0)(_ + _)
```

Many of the other functions that we have seen before perform
fold-right operations on lists. In particular, we can define
`append` using `foldRight` as follows:

```scala
def append[A](l1: List[A], l2: List[A]): List[A] =
  foldRight(l1)(l2)(_ :: _)
```

Also the higher-order function `map` is just a special case of
a fold-right:

```scala
def map[A, B](l: List[A])(op: A => B): List[B] =
  foldRight(l)((Nil: List[B]))((h, l) => op(h) :: l)
```

Note that due to limitations of Scala's type inference algorithm, we
have to manually annotate the type `List[B]` of the empty list
constructor `Nil` that we use to build the resulting list
of the map operation.

All the above operations on lists have in common that they combine the
elements in the input list and the result of the recursive computation
in right-to-left order. We can also consider fold operations that
perform the computation in left-to-right order:

```scala
op(...(op(op(z, d1), d2), ...), dn)
```

The corresponding computation tree then looks as follows:
```scala
        op
       /  \
     ...  dn
     /
    op 
   /  \
  op  d2
 /  \
z   d1
```

Note that the tree is now leaning towards the left and the elements
are combined in left-to-right order. We therefore refer to this type
of computation as a *fold-left*.

The following function implements the generic fold-left operation on
lists:
```scala
def foldLeft[A,B](l: List[A])(z: B)(op: (B, A) => B): B =
  l match {
    case Nil => z
    case hd :: tl => foldLeft(tl)(op(z, hd))(op)
  }
```

Since addition is associative and commutative, we can alternatively
define `sum` using `foldLeft` instead of `foldRight`:

```scala
def sum(l: List[Int]): Int = foldLeft(l)(0)(_ + _)
```

In fact, this definition of `sum` is more efficient than our
previous implementations because `foldLeft` is tail-recursive,
whereas our implementation of `foldRight` is not. In general,
only one of the two types of fold operations can be used to implement
a specific computation on lists. For example, we can express
`reverse` in terms of a fold-left as follows:

```scala
def reverse[A](l: List[A]): List[A] =
  foldLeft(l)((Nil: List[A]))((l1, x) => x :: l1)
```

If we replaced `foldLeft` by `foldRight` in this
definition, we would not obtain the correct result. The computed
output list would be structurally identical to the input list.

### Scala's Collection Classes

Since higher-order functions on collections are so incredibly useful
for writing concise code, the data structures in the Scala standard
API already implement a large number of these functions. The functions
are realized as methods of the corresponding collection
classes. For example Scala's `List` class already provides
the methods `foldLeft`, `foldRight`, `map`, etc.

As with any programming language, you should study the Scala
standard API carefully so that you get an overview of the provided
functionality and so that you do not "reinvent the wheel" when you
write your own programs.

To get a glimpse of the expressive power of the functions implemented
in the collection classes, consider the following code snippet. The
code defines a list of integers and a list of strings and then folds
the two lists into a single string. This string consists of a comma
separated sequence of strings, where each string is a pair of elements
from the two lists concatenated together using the colon symbol:

```scala
scala> val l1 = List(1, 2, 3)
l1: List[Int] = List(1, 2, 3)

scala> val l2 = List("a", "b","c")
l2: List[String] = List(a, b, c)

scala> ((l1,l2) zipped) map 
(_ + ":" + _) reduce
(_ + ", " + _)
res0: String = 1:a, 2:b, 3:c
```

It is instructive to re-implement this code snippet in a language such
as Java to appreciate how much more concise and comprehensive the
functional implementation is.
