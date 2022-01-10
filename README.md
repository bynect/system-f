# System F

## Notation

Traditionally the following notation is used in type theoretic documents:

* Terms are denoted by a lowercase e `e`
* Types are denoted by a lowercase tau `τ`
* Term variables are denoted by a lowercase x `x`
* Type variables are denoted by a lowercase alpha `α` (or sometimes a lowercase beta `β`)
* Typing environments are denoted by an uppercase sigma `Γ`
* Kinding environments are denoted by an uppercase theta `Θ`

## Syntax

The syntax of System F is very similar to that of _simply typed lambda calculus_, with the addition of a way to express _universal quantification_.
The ML family languages are, at least from a theoretical point, based on System F, which encodes the concept of _parametric polymorphism_.

### Terms

```haskell
e = x         variable
  | λx:τ. e   abstraction
  | Λα. e     type abstraction
  | e e'      application
  | e [τ]     type application
```

### Types

```haskell
τ = α         type variable
  | τ → τ'    type function
  | ∀α. τ     universal quantifier
```

## Extensions

### Let

Thus `let x = e in e'` is equivalent to

```haskell
let x:τ = e in e'
```

Unlike ML, _let expressions_ can be desugared to abstractions directly in System F.
Thus they are equivalent to `(λx:τ. e') e`.

You can enable this extension by defining `SUGAR_LET`. FIXME

### System F Omega

TODO

## Usage

The program runs every file provided in the command line arguments in order, preserving the changes of the previous.
Upon encountering `-`, even when it is not the last argument, it will start an interactive session.

### Example

```haskell
$ ./Main Prim.txt -
I, id = Λα. λx:α. x
I, id : ∀α. α → α
-- Other definitions...
S, succ = λn:nat. Λα. λx:α. λf:α → α. f ((n [α]) x f)
S, succ : (∀α. α → (α → α) → α) → (∀α. α → (α → α) → α)
c> -- REPL starts
```

This invocation of the program first loads the file [`Prim.txt`](/Prim.txt), which contains some basic definitions (SKI, nats, ...).
Then it starts an interactive session, preserving the definitions of the aforementioned file.

## Features

### Type checking

Every expression you enter will be checked.

### Definitions

Substitutions can be easily defined for both terms and types.
Older definitions are shadowed by newer ones.
Terms and types use two distinct namespaces.

```haskell
-- Term substitution
zero = Λα. λx:α. λf:α → α. x

-- Type substitution
nat = [∀α. α → (α → α) → α]
```

### Evaluation

Not implemented yet. TODO

### Unicode support

The REPL accepts the following ASCII alternatives for some Unicode characters.

* `\` instead of lowercase lambda `λ`
* `/\` instead of uppercase lambda `Λ`
* `->` instead of unicode arrow `→`
* `forall` instead of turned uppercase a `∀`

If `SHOW_UNICODE` is defined Unicode characters are used to print expression, types, etc.

## License

This project is licensed under the terms and conditions of the Mozilla Public License Version 2.0.
