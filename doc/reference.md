# Reference guide

### Registry

The following combinators are available to create registries

###### Appending

 combinator             | meaning
 ---------------------- | -------
   `end`                | the empty registry
   `<:`                 | append an element to the registry
   `+:`                 | append an element `a` to the registry, but do not check that the inputs of `a` can already be produced by the registry
   `<+>`                | append 2 registries together

###### Creating registry elements

<table>
<tr><th width="150px;">combinator</th> <th>meaning</th></div></tr>
<tr><td><code>val @a</code></td>       <td>a value of type <code>a</code> which can be added to the registry</td></tr>
<tr><td><code>fun @f</code></td>       <td>a function of type <code>f</code> which can be added to the registry</td></tr>
<tr><td><code>valTo @m @a</code></td>  <td>a value of type <code>a</code> which is added as <code>m a</code> to the registry</td></tr>
<tr><td><code>funTo @m @f</code></td>  <td>a function of type <code>i1 -> i2 -> ... -> o<code> which is lifted into <code>m i1 -> m i2 -> ... -> m o</code> before being added to the registry</td></tr>
</table>


###### Lifting functions

It is also possible to only use `val` and `fun` and lift functions yourself with the following combinators:

<table>
<tr><th width="140px;">combinator</th>    <th>meaning</th></div></tr>
<tr><td><code>allTo @m</code></td>        <td>lift a function of type <code>i1 -> i2 -> ... -> o</code> to <code>m i1 -> m i2 -> ... -> m o</code></td></tr>
<tr><td><code>argsTo @m</code></td>       <td>lift a function of type <code>i1 -> i2 -> ... -> m o</code> to <code>m i1 -> m i2 -> ... -> m o</code></td></tr>
<tr><td><code>outTo @m nat</code></td>    <td>lift a function of type <code>i1 -> i2 -> ... -> n o</code> to <code>i1 -> i2 -> ... -> m o</code> using <code>nat :: forall x . n x -> m x</code></td></tr>
</table>

###### Making elements

 combinator             | meaning
 ---------------------- | -------
   `make @a`            | statically check that an element can be built and build it
   `makeFast @a`        | statically check that an element is one of the registry outputs and build it
   `makeEither @a`      | make a value and return `Left <an error>` if the value cannot be built
   `makeUnsafe @a`      | make a value and throw an exception if the value cannot be built

You can also make "specialized" values with `makeSpecialized @a @b`, `makeSpecializedFast @a @b`,... where the value `b` is made in the context of building a value of type `a`. This uses possible specializations of the registry. Similarly if the specialization is declared for a specific "path" in the value graph you can use `makeSpecializedPath`, `makeSpecializedPathFast` and so on.

###### Speed-up compilation times

<table>
<tr><th width="260px;">combinator</th>               <th>meaning</th></div></tr>
<tr><td><code>normalize</code></td>                  <td>make the list of types in the registry unique (no duplicated types) to speed-up compile times with <code>make</code></td></tr>
<tr><td><code>$(checkRegistry 'registry)</code></td> <td>[uses Template Haskell] check that any output type in the registry can be built. Once a registry is checked <code>makeFast</code> can safely be used. This is faster than <code>normalize</code></td></tr>
</table>


###### Tweaking the registry

<table>
<tr><th width="260px;">combinator</th>               <th>meaning</th></div></tr>
<tr><td><code>memoize @m @a</code></td>              <td>if a value of type <code>m a</code> is created, store the value <code>a</code> so that the same <code>a</code> is returned whenever </code>m a</code> is executed</td></tr>
<tr><td><code>memoizeAll @m</code></td>              <td>run <code>memoize</code> for all the effectful output types of a registry</td></tr>
<tr><td><code>specialize @a @b b</code></td>         <td>when trying to build a value of type <code>a</code> make sure that <code>b</code> is always used when a value of type </code>b</code> is required</td></tr>
<tr><td><code>specializeVal @a @b b</code></td>      <td>similar to <code>specialize</code> but uses <code>Show b</code> to display a better description when printing the registry out</td></tr>
<tr><td><code>specializeValTo @m @a @b b</code></td> <td>similar to <code>specializeVal</code> but "lifts" <code>b</code> into an <code>Applicative</code> context</td></tr>
<tr><td><code>specializePath @[as] @b b</code></td>  <td>specialize a value but only for a given "path of types" when those types are part of the current search context</td></tr>
<tr><td><code>tweak @a f</code></td>                 <td>modify a value of type <code>a</code> with a function <code>f :: a -> a</code> right after it has been created and before storing it</td></tr>
</table>


###### Type aliases

 alias             | meaning
 ----------------- | -------
   `out :- a `     | `Contains a out` means that `a` is in the list of types `out`
