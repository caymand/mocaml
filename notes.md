
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
