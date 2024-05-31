
# Links and docs

- https://ocaml-ppx.github.io/ppxlib/ppxlib/matching-code.html#pattern_examples
- 

# MetaQuot

Rewrite expression *extension* with its payload directly. 

**Extension Payload**:

`[%expr <this is a payload>]`


# Workflow

- Name the extension rewriters. This will be called by driver when the extension
  point is met. 
- Register?
- Extract patterns
- Expand the payload

# Multi-level implementation

**What kind of extensions are needed?**
Those for operators to specify they *binding time*.
But what will be rewritten? We first need to know whether any of the arguments
are known. In case they are, we can replace the code with the known arguments.

We can replace a [%plus t e1 e2] with a new node [%plus t e1 e2]

# Example

Say I have a transformation like the following:

```ocaml
let add a b = [%plus 1 a b]

let add' = [%specialize add 1]
let cst = [%specializee add' 2]
```

These transformations are context free/local and I cannot modify add within
specialize. 

```ocaml
let add a b = [%plus 1 a b]

```

# Plan

We will use the following multi-level operations:

- `[%lift t e]`
- `[%lift t s e]`
- `[%plus t e1 e1]` and mul
- `[%minus t e1 e2]` and div

Next, we need to be able to specialize wrt. some inputs. 
We will use `[%run f e]`. This will run `f` and specialize wrt. passed as the
first argument to `f`. The result might be a function that can be specialized
again. Each time we specialize, we either need to generate code or evaluate
expressions.

As an example, we might have the function

``` 
let add a b = [%plus 2 [%lift 0 2 a] [%lift 0 2 b]]
```

We then specialize this with the first input:

```
let add' = [%run add 40] : fun b -> [%plus 1 [%lift 0 1 1] [%lift 1 b]] 
```

We can then either use the function or specialize it again:

```
[%run add' 2] : fun () -> 42
```

Which will compute the actual number.

The question is now, *how will we make this work in OCaml?*
First, we need to figure out how to do the first specialization. We can observe
that `[%run]` generates code according to some rules associated with binding
time. So we can definitely do a syntax extension to generate the needed
code. In order to do so, we first need to have seen the `add` (and later `add'`)
function.

This means we need an environment to keep track of so called multi-stage
functions. We can look up the structure items until we find a matching `add`
function, and then copy the expressions and modify them.

