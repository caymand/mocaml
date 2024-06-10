
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


It has occurred to me that `[%lift t s e]` is actually semantically
important. Consider this program:

```
[%%ml let add_ml a b = [%plus 2
          [%lift 2 a]           (* a has binding time t = 1 *)
          [%lift 2 b]]]         (* b has binding time t = 2 *)
```

There is no way to signal the binding times with this lift. Meanwhile, with the
annotation `[%lift t s e]` we know that `e` has binding time `t`, and it should
get coerced to a value with bt of `t+s`. Therefore, we would write `[%lift 1 1
e]`. That way, we see that `a` must be specialized once before its value is
known and twice for it to be used in the enclosing. 
Specializing the program would give us

```
[%%ml let add_ml a b = [%plus 2
          [%lift 1 1 a]           (* a has binding time t = 1 *)
          [%lift 2 b]]]         (* b has binding time t = 2 *)
let add = [%run 40 add_ml] = fun b -> [%plus 1 
                                       [%lift 0 1 40]
                                       [%lift 1 b]]
[%run 2 add] = 42
```

# Recursion

Lets say I have the following operation:

```
(Multi_level_ops.IfElse (
   (Multi_level_ops.Leq (
      (Multi_level_ops.Expr
         { Multi_level_ops.v = (Multi_level_ops.Ident "n"); t = 1 }),
      (Multi_level_ops.Expr
         { Multi_level_ops.v = (Multi_level_ops.Val 1); t = 1 })
      )),
   (Multi_level_ops.Expr
      { Multi_level_ops.v = (Multi_level_ops.Val 0); t = 1 }),
   (Multi_level_ops.Binop (1,
      (Multi_level_ops.Add (
         (Multi_level_ops.Expr
            { Multi_level_ops.v = (Multi_level_ops.Val 1); t = 1 }),
         (Multi_level_ops.App (1, sum,
            [(Multi_level_ops.Binop (1,
                (Multi_level_ops.Sub (
                   (Multi_level_ops.Expr
                      { Multi_level_ops.v = (Multi_level_ops.Ident "n");
                        t = 1 }),
                   (Multi_level_ops.Expr
                      { Multi_level_ops.v = (Multi_level_ops.Val 1); t = 1 })
                   ))
                ))
              ]
            ))
         ))
      ))
   ))
```

One transformation that could be done is:

```
(Multi_level_ops.IfElse (
   (Multi_level_ops.Leq (
      (Multi_level_ops.Expr
         { Multi_level_ops.v = (Multi_level_ops.Ident "n"); t = 1 }),
      (Multi_level_ops.Expr
         { Multi_level_ops.v = (Multi_level_ops.Val 1); t = 1 })
      )),
   (Multi_level_ops.Expr
      { Multi_level_ops.v = (Multi_level_ops.Val 0); t = 1 }),
   (Multi_level_ops.Binop (1,
      (Multi_level_ops.Add (
         (Multi_level_ops.Expr
            { Multi_level_ops.v = (Multi_level_ops.Val 1); t = 1 }),
		 (* Insert the function again *)
         (Multi_level_ops.IfElse (
			 (Multi_level_ops.Leq (
				(Multi_level_ops.Expr
				   { Multi_level_ops.v = (Multi_level_ops.Ident "n"); t = 1 }),
				(Multi_level_ops.Expr
				   { Multi_level_ops.v = (Multi_level_ops.Val 1); t = 1 })
				)),
			 (Multi_level_ops.Expr
				{ Multi_level_ops.v = (Multi_level_ops.Val 0); t = 1 }),
			 (Multi_level_ops.Binop (1,
				(Multi_level_ops.Add (
				   (Multi_level_ops.Expr
					  { Multi_level_ops.v = (Multi_level_ops.Val 1); t = 1 }),
				   (Multi_level_ops.App (1, sum,
					  [(Multi_level_ops.Binop (1,
						  (Multi_level_ops.Sub (
							 (Multi_level_ops.Expr
								{ Multi_level_ops.v = (Multi_level_ops.Ident "n");
								  t = 1 }),
							 (Multi_level_ops.Expr
								{ Multi_level_ops.v = (Multi_level_ops.Val 1); t = 1 })
							 ))
						  ))
						]
					  ))
				   ))
				))
			 ))
         ))
      ))
   ))
```

This would continue until the expression can be evaluated.

How can this expression be handled:

```ocaml
[%%ml let foo a b =
	if [%lift 1 a] < [%lift 1 1]
	then [%lift 2 b]
	else [%app 2 (foo (a-1) b )] (* should have lift on a and b *)
]
```

When we specialize this w.r.t `a` then we get a new function `mul b -> ...`. So
first we would transform this into something like

```
(Multi_level_ops.IfElse (
   (Multi_level_ops.Leq (
      (Multi_level_ops.Expr
         { Multi_level_ops.v = (Multi_level_ops.Ident "n"); t = 1 }),
      (Multi_level_ops.Expr
         { Multi_level_ops.v = (Multi_level_ops.Val 1); t = 1 })
      )),
   (Multi_level_ops.Expr
      { Multi_level_ops.v = (Multi_level_ops.Val 0); t = 1 }),
   (Multi_level_ops.App (1, sum,
       [(Multi_level_ops.Binop (1,
           (Multi_level_ops.Sub (
               (Multi_level_ops.Expr
                   { Multi_level_ops.v = (Multi_level_ops.Ident "n");
                       t = 1 }),
               (Multi_level_ops.Expr
                   { Multi_level_ops.v = (Multi_level_ops.Val 1); t = 1 })
                   ))
                ))
              ]
            ))
         ))
      ))
   ))
```
Then we start specializing and we can remove the branch. We continue and
eventually hit the application (a recursive call). We need to treat this as its
own new function and generate code.
