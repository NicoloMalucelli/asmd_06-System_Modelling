# System modelling

## Task 1: Verifier

In order to solve this task, I implemented a DSL that allows to test temporal logic on Petri nets.

To do so, I defined a trait <tt>Condition</tt> and implemented the eval method for each one of the operators
using an extension method. This allows the programmer to use the infix notation and make the code more readable.

The operator I implemented are: *always*, *eventually*, *next*, *until* and *weakUntil*, along with some basic
logical operators such as *and*, *or*, *not*, *<*, *>*, *== (is)*.

All the above Conditions have been tested (*scala.u06.modelling.LTLTest*).

Using this DSL is possible to describe simple temporal logic but also compose different conditions together in
a readable way, by passing in the eval method the Petri Net and the initial marking:

```
(*(B) is 1 weakUntil eventually(*(D)) eval(pNet2, MSet(*(A))

eventually(*(C)) until *(D)) eval(pNet3, MSet(*(A))
```

By using this DSL it's been easy to verify the safety property. In this specific case, the reader and writers
problem safety property has been verified using 3 readers (initial state R_WAIT) and 4 writers (initial state
W_WAIT):

```
always(
    not(*(WRITING) and *(READING)) and
    not(*(WRITING) > 1)
  ) eval(readersAndWriters, MSet(*(R_WAIT), *(R_WAIT), *(R_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT)))
```

_N.B._ *prerequisite to use this DSL is the Petri Net to be bounded.*

## Task 2: Designer

In this variation of the readers and writers basic problem, a writer that wants to write will always be satisfied.
The basic idea is to introduce a Token that is taken from the writer when they want to write. Readers need the token to
access the READY_TO_READ state, therefore if a reader want to read but a writer already said that want to write, the reader
must wait until the writer has finished. Many readers can access at the same time since the token is not consumed by the readers
(*MSet(\*(R_WAIT), \*(TOKEN)) ~~> MSet(\*(READY_TO_READ), \*(TOKEN))*):

```
  def readersAndWritersWithLiveness = PetriNet[Place](
    MSet(*(W_WAIT), *(TOKEN)) ~~> MSet(*(READY_TO_WRITE)),
    MSet(*(R_WAIT), *(TOKEN)) ~~> MSet(*(READY_TO_READ), *(TOKEN)),
    MSet(*(READY_TO_WRITE)) ~~> MSet(*(WRITING)) ^^^ MSet(*(READING)),
    MSet(*(READY_TO_READ)) ~~> MSet(*(READING)) ^^^ MSet(*(WRITING)),
    MSet(*(WRITING)) ~~> MSet(*(W_WAIT), *(TOKEN)),
    MSet(*(READING)) ~~> MSet(*(R_WAIT)),
  ).toSystem
```

Like in the normal case, the safety property has been tested using the implemented DSL:

```
always(
    not(*(WRITING) and *(READING)) and
    not(*(WRITING) > 1)
) eval(readersAndWritersWithLiveness, MSet(*(TOKEN), *(R_WAIT), *(R_WAIT), *(R_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT)))
```

In addition to that, also the liveness property has been tested, in other words has been tested that if a writer wants
to write, eventually they will (such property was not granted in the basic scenario):

```
(
    (*(TOKEN) is 1) weakUntil (eventually(*(WRITING) is 1))
) eval(readersAndWritersWithLiveness, MSet(*(TOKEN), *(R_WAIT), *(R_WAIT), *(R_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT)))
```

(this code is located in the verifier.Verifier file)

## Task 3: Artist

Both the extensions have been realized modifying some of the original definitions in the PetriNet
file and can be used both together or one at the time by programmer's discretion. 

### Petri Net extension - priorities

The priorities extension has been obtained by adding a new value to the transition class (<tt>Trn</tt>).
This new value has default value 1, therefore, if the programmer doesn't want to use the priorities extension,
they can just not indicate the value of this property. 

```
case class Trn[P](cond: Marking[P], eff: Marking[P], inh: Marking[P], priority: Int = 1)
```

The <tt>toSystem</tt> method has been modified so that only the transitions with the highest priority value among
the transitions that can actually be fired are returned from the method.

```
extension [P](pn: PetriNet[P])
    def toSystem: System[Marking[P]] = m =>
      (for
        Trn(cond, eff, inh, priority) <- pn   // get any transition
        if m disjoined inh          // check inhibition
        out <- m extract cond       // remove precondition
      yield (priority, out union eff))
        .maxes(_._1) // consider only the transitions with highest priority
        .map(_._2)          
```

An infix syntax has been defined so that priorities can be defined in the following way

```
def pnMEWithPriority = PetriNet[Place](
    MSet(*(N)) ~~> MSet(*(T)) priority 2,
    MSet(*(T)) ~~> MSet(*(C)) ^^^ MSet(*(C)),
    MSet(*(C)) ~~> MSet()
  ).toSystem
```

Here a simulation of the above described system:

```
println(pnMEWithPriority.paths(MSet(*(N), *(N)), 7).toList.mkString("\n"))

output --> List({N|N}, {N|T}, {T|T}, {T|C}, {T}, {C}, {})
```

### Petri Net extension - colors

Colors are defined by an enum inside the PetriNet object.

```
enum Color:
    case Black, Yellow, Red
```

A new class <tt>ColoredToken</tt> has been defined and the alias Marking[P] has been adapted and is now defined as
MSet[*[P]]. In the same way, the transition class has been modified to use Marking[P] in its values.

Tokens' default color is black, therefore the user can choose whether use the colors extensions or not: by not
specifying any color in the markings, each token is black, so all tokens are the same!

The toString method has been overridden so that, if the token is colored, the color of the token is showed when 
printing, while if the token is uncolored (black), the color is not shown.

```
@targetName("ColoredToken")
case class *[P](p: P, color: Color = Color.Black):
override def toString: String = color match
  case Color.Black => p.toString
  case c => c.toString + "_" + p

type Marking[P] = MSet[*[P]]

case class Trn[P](cond: Marking[P], eff: Marking[P], inh: Marking[P], priority: Int = 1)
```

By adopting this solution, it has not been necessary to modify the <tt>toSystem</tt> method since tokens are 
comparing considering both the place and the color parameter.

A Petri Net with colored transitions can be defined in the following way:

```
def pnMEWithColors = PetriNet[Place](
    MSet(*(A)) ~~> MSet(*(B)),
    MSet(*(B)) ~~> MSet(*(C, Red)),
    MSet(*(C, Red)) ~~> MSet(*(B, Red)),
    MSet(*(B, Red)) ~~> MSet(*(A)),
  ).toSystem
```

In the following cell, a simulation of the above defined system is shown

```
println(pnMEWithColors.paths(MSet(*(A), *(C, Color.Red)),3).toList.mkString("\n"))

output --> List({A|Red_C}, {B|Red_C}, {Red_C|Red_C})
           List({A|Red_C}, {B|Red_C}, {Red_B|B})
           List({A|Red_C}, {A|Red_B}, {Red_B|B})
           List({A|Red_C}, {A|Red_B}, {A|A})
```