# System modelling

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