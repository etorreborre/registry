# The Registry

#### The resolution algorithm

Let's imagine for a moment that you have a stack of functions to create "output values". You also put on your stack some "input" values. If you want a value of a given type you can:

 1. go through the list of existing values, if you find one with the desired type return it
 1. otherwise try to find a function returning a value of the given type
 1. if you find such a function apply the same algorithm to build all its input values
 1. every newly built value is put on top of the stack so it is available as an input to another function

You can eventually create a value out of the Registry if:

 - the value type is one of the existing values types
 - or if its type is the output type of one of the functions
 - the function inputs types are also existing value types or output types of other functions
 - there are no cycles!
