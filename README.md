# janet-schema

A schema library for Janet, inspired by plumatic/schema and jsonschema.

# Status

- [x] Basic type validation
- [x] Basic constraint validation
- [ ] Schema validation
- [ ] Data coercion based on given schema and coercer

# Usage

Import with:

```
(use staab.schema/schema)
```

## Basic

Create a schema:

```
(def schema {:stuff [{:a [:int]}]})
```

Check your data:

```
(pp (get-error schema {:stuff [{:a [1 2]} {:a [1 false 3]}]}))
# => {:code :type-error
      :path (:stuff 1 :a 1)
      :expected :int
      :actual :boolean
      :message "false is not a int"}
```

## Advanced

A schema is just a map with a `:t` property that's been registered as a type. `normalize` expands a schema into this format; literals are just a handy shorthand.

In addition to types, some extra modifiers can be added, depending on what type you're working with.

## Enum

Any schema can be annotated with `:enum`, which should be a tuple of allowed values.

```
(pp (get-error {:t :any :enum [1 :a "c"]} "d"))
# => {:message "\"d\" is not one of [1, :a, \"c\"]" ...}
```

## Maps

Map schemas can be annotated with `:required` properties, and `:closed`, which defaults to false.

```
(pp (get-error {:t :map :required [:a]} {}))
# => {:message "a is a required property" ...}

(pp (get-error {:t :map :closed true} {:a 1}))
# => {:message "a is not an allowed property" ...}
```

## Constraints

Constraints are additional rules that can be included on a schema which reference parts of the data and impose constraints.

Paths are the key part of constraints; in general they should be integer indexes or associative keys based on the data structure expected. The exception to this is when referencing all values in a dictionary, or a slice of some sequence. The syntax for this is `[:]` with optional indices before and after the colon in the case of indexed data types.

```

(pp
 (get-error
   {:t :map
    :constraints [{:t :intersection
                   :search [:a "[1:]"]
                   :target [:b "[:]"]}]}
   {:a [1 2 3] :b [1 2 3]}))
# => {:message "1 is not contained in the search path" ...}
```

# Disclaimer

This is pre-alpha software. Breaking changes are likely. Please open an issue if you'd like to use it in a project.
