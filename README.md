# org-editor

A Clojure library that can edit org-files, attempting to minimize diffs.

It's just for me, so it's not all that good or anything.

## Obtention

``` clojure
[com.gfredericks/org-editor "0.1.0"]
```

## Usage

``` clojure
(require '[com.gfredericks.org-editor :as org-editor])
```

Supposing you have an org-file called `my.org`:

```
You can have lines
before the first

heading

* Heading 1
  A few

  lines here
** Heading 1a
*** Whatever you want
* TODO Heading 2                                                       :atag:
* Welp!

** Last heading, 2 or whatever

   It's all just lines!
```

Then `(with-open [r (clojure.java.io/reader "my.org")] (org-editor/parse-file r))`
returns:

``` clojure
#::com.gfredericks.org-editor
{:prelude ("You can have lines"
           "before the first"
           ""
           "heading"
           ""),
 :sections [#::com.gfredericks.org-editor
            {:header "* Heading 1",
             :prelude ("  A few"
                       ""
                       "  lines here"),
             :sections [#::com.gfredericks.org-editor
                        {:header "** Heading 1a",
                         :prelude (),
                         :sections [#::com.gfredericks.org-editor
                                    {:header "*** Whatever you want",
                                     :prelude (),
                                     :sections []}]}]}
            #::com.gfredericks.org-editor
            {:header "* TODO Heading 2                                                       :atag:",
             :prelude (),
             :sections []}
            #::com.gfredericks.org-editor
            {:header "* Welp!",
             :prelude (""),
             :sections [{:header "** Last heading, 2 or whatever",
                         :prelude (""
                                   "   It's all just lines!"),
                         :sections []}]}]}
```

The base parse is fairly low level, it only parses the structure
created by the header lines.

The `org-editor/write-file` function writes the same structure back to
a file, and then there are some other functions for editing the parse
structure such as `org-editor/read-properties` and
`org-editor/prop-assoc`.

## License

Copyright © 2018 Gary Fredericks

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
