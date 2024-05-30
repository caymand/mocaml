
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
