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