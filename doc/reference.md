# Reference guide

### Registry

The following combinators are available to create registries

###### Appending

 combinator             | meaning
 ---------------------- | -------
   `end`                | the empty registry
   `<:`                 | append an element to the registry (a value, function or another registry)
   `<+`                 | append an element to the registry (a value, function or another registry), but don't perform any check
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
   `make @a`            | build an element from a registry and throw an exception if it cannot be build
   `makeEither @a`      | make a value and return `Left <an error>` if the value cannot be built
   `makeSafe @a`        | check statically that an element can be build registry before making it

The values which are "specialized" (see [#tweaking-the-registry]) can be built differently depending on the which other value we are trying to build:

 - use the `make` function to get the default value
 - use the `makeSpecialized @a @b` function to get a value of type `b` specialized in the context of building `a`
 - use the `makeSpecializedPath @path @b` function to get a value of type `b` specialized in the context of building a specific `path` of types in a data graph

###### Speed-up compilation times

<table>
<tr><th width="260px;">combinator</th>               <th>meaning</th></div></tr>
<tr><td><code>normalize</code></td>                  <td>make the list of types in the registry unique (no duplicated types) to speed-up compile times with <code>make</code></td></tr>
</table>


###### Tweaking the registry

<table>
<tr><th width="260px;">combinator</th>               <th>meaning</th></div></tr>
<tr><td><code>memoize @m @a</code></td>              <td>if a value of type <code>m a</code> is created, store the value <code>a</code> so that the same <code>a</code> is returned whenever </code>m a</code> is executed</td></tr>
<tr><td><code>memoizeAll @m</code></td>              <td>run <code>memoize</code> for all the effectful output types of a registry</td></tr>
<tr><td><code>specialize @a @b b</code></td>         <td>when trying to build a value of type <code>a</code> make sure that <code>b</code> is always used when a value of type </code>b</code> is required. <code>b</code> can either be a value created with <code>val</code>, or a function, created with <code>fun</code> (for example <code>fun (\url port -> makeHttp url port)</code>). Remember that values and functions can be lifted with <code>valTo</code> and <code>funTo</code> </td></tr>
<tr><td><code>specializePath @[as] @b b</code></td>  <td>specialize a value but only for a given "path of types" when those types are part of the current search context</td></tr>
<tr><td><code>tweak @a f</code></td>                 <td>modify a value of type <code>a</code> with a function <code>f :: a -> a</code> right after it has been created and before storing it</td></tr>
</table>


###### Type aliases

 alias             | meaning
 ----------------- | -------
   `out :- a `     | `Contains a out` means that `a` is in the list of types `out`
