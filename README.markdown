
clj-arrow is a small library for combining functions.

## Installation

In leiningen add `[clj-arrow "1.0.3-SNAPSHOT"]` to dependencies. Otherwise, put the 
[jar](https://github.com/downloads/odyssomay/clj-arrow/clj-arrow-1.0.3-SNAPSHOT.jar)
on the classpath. Only clojure is required as a dependency.

## Usage and installation

First `use`, or `require` clj-arrow.arrow. In this namespace these functions exist (see docstrings for usage):

### functions creating functions

- `>>>` - create a flow of functions to the right.
- `<<<` - create a flow of functions to the left.
- `fst` - apply a function to the first value of a pair.
- `snd` - apply a function to the second value of a pair.
- `***` - apply two functions to respective values of a pair.
- `&&&` - same as `***`, but takes a single value which is cloned to a pair.
- `|||` - an `if` conditional statement for functions.

### multithreading

- `***-multithread` - same as `***` but multithreads.
- `&&&-multithread` - same as `&&&` but multithreads.

## Multithreading

Using `***-multithread` or `&&&-multithread` work can be done in parallel.

```clojure
(time ((&&& #(Thread/sleep %) #(Thread/sleep %)) 1000))
;   "Elapsed time: 2006.765373 msecs"
;=> [nil nil]

(time ((&&&-multithread #(Thread/sleep %) #(Thread/sleep %)) 1000))
;   "Elapsed time: 1004.629826 msecs"
;=> [nil nil]
```

## Functions and arrows

While I only mention functions as arguments to the functions above, 
they could be used just as well with any type of arrow (functions can be seen as a subset of arrows).

### What is an arrow?

This is a delicate subject, which I must admit that I don't quite understand myself. 
However, for clj-arrow it's simple: any /thing/ (class, record ...) implementing 
the `clj-arrow.arrow/Arrow` protocol (or the underlying interface) is an arrow. The protocol defines the following functions:

- `run` - function to execute the arrow. 
- `>>>_int` - the arrow's implementation of `>>>`, takes only two arguments.
- `fst_int` - the arrow's implementation of `fst`.

An arrow implementing these functions must also not make any assumptions of what type of arrow is sent as the second argument to the `>>>_int` function.
If these conditions are met, the arrow will work with all the functions described in Usage.

## License (zlib)

Copyright (c) 2011 Jonathan Fischer Friberg

This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

3. This notice may not be removed or altered from any source distribution.

