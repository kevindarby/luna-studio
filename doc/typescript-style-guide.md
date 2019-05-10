# TypeScript Style Guide
Like many style guides, this TypeScript style guide exists for two primary
reasons. The first is to provide guidelines that result in a consistent code
style across all of the Luna codebases, while the second is to guide people
towards a style that is expressive while still easy to read and understand.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Code Formatting](#code-formatting)
- [Comments](#comments)
    - [Documentation Comments](#documentation-comments)
    - [Source Notes](#source-notes)
    - [TODO Comments](#todo-comments)
    - [Other Comment Usage](#other-comment-usage)
- [Program Design](#program-design)

<!-- /MarkdownTOC -->

## Code Formatting
Rather than a whole host of complicated formatting rules for TypeScript, we make
use of [prettier](https://prettier.io/), an opinionated code formatter. All
TypeScript code should be formatted using prettier, though the point at which
you do this is up to you. We recommend either on-save in your editor, or using
a precommit hook.

The prettier configuration that can be used is located in the root of this
repository, and should be copied to all other repositories containing TypeScript
code if it is not already there.

## Comments
Comments are a tricky area to get right, as we have found that comments often
expire quickly and, in absence of a way to validate them, remain incorrect for
long periods of time. That is not to say, however, that we eschew comments
entirely. Instead, we make keeping comments up to date an integral part of our
programming practice, while also limiting the types of comments that we allow.

When we write comments, we try to follow one general guideline. A comment should
explain _what_ and _why_, without mentioning _how_. The _how_ should be
self-explanatory from reading the code, and if you find that it is not, that is
a sign that the code in question needs refactoring.

Code should be written in such a way that it guides you over what it does, and
comments should not be used as a crutch for badly-designed code.

### Documentation Comments
One of the primary forms of comment that we allow across the Luna codebases is
the doc comment. These are intended to be consumed by users of the API, and use
the standard Haddock syntax. Doc comments should:

- Provide a short one-line explanation of the object being documented.
- Provide a longer description of the object, including examples where relevant.
- Explain the arguments to a function where relevant.

They should not reference internal implementation details, or be used to explain
choices made in the function's implementation. See [Source Notes](#source-notes)
below for how to indicate that kind of information.

### Source Notes
Source Notes is a mechanism for moving detailed design information about a piece
of code out of the code itself. In doing so, it retains the key information
about the design while not impeding the flow of the code.

Source notes are detailed comments that, like all comments, explain both the
_what_ and the _why_ of the code being described. In very rare cases, it may
include some _how_, but only to refer to why a particular method was chosen to
achieve the goals in question.

A source note comment is broken into two parts:

1. **Referrer:** This is a small comment left at the point where the explanation
   is relevant. It takes the following form: `// Note [Note Name]`, where
   `Note Name` is a unique identifier across the codebase. These names should be
   descriptive, and make sure you search for it before using it, in case it is
   already in use.
2. **Source Note:** This is the comment itself, which is a large block comment
   placed after the first function in which it is referred to in the module. It
   uses the haskell block-comment syntax `/* ... */`, and the first line names
   the note using the same referrer as above: `/* Note [Note Name]`. The name(s)
   in the note are underlined using a string of the `~` (tilde) character.

A source note may contain sections within it where necessary. These are titled
using the following syntax: `== Note [Note Name (Section Name)]`, and can be
referred to from a referrer much as the main source note can be.

Sometimes it is necessary to reference a source note in another file, but this
should never be done in-line. Instead, a piece of code should reference a source
note in the same module that references the other note while providing
additional context.

An example, taken from the GHC codebase, can be seen below.

```typescript
export abstract class Foo implements MyInterface {
  foo() {
    frobnicate() // Note [Something Complicated]

    return "hello" // Note [Something Complicated (Extra)]
  }

/* Note [Something Complicated]
    ... Some explanation about the topic at hand...

== Note [Something Complicated (Extra)]
    ... Some explanation about the sub-topic at hand...
*/
}
```

A source note like this is useful whenever you have design decisions to explain,
but can also be used for:

- **Formulae and Algorithms:** If your code makes use of a mathematical formula,
  or algorithm, it should note where the design element came from, preferably
  with a link.
- **Safety:** Sometimes it is necessary to use an unsafe API in a context where
  it is trivially made safe. You should always use a source note to explain why
  its usage is safe in this context.

### TODO Comments
We follow a simple convention for `TODO` comments in our codebases:

- The line starts with `TODO` or `FIXME`.
- It is then followed by the author's initials `[ARA]`, or for multiple people
  `[ARA, WD]`, in square brackets.
- It is then followed by an explanation of what needs to be done.

For example:

```hs
// TODO [ARA] This is a bit of a kludge. Instead of X it should to Y, accounting
// for the fact that Z.
```

### Other Comment Usage
There are, of course, a few other situations where commenting is very useful:

- **Commenting Out:** You may comment out code while developing it, but if you
  commit any commented out code, it should be accompanied by an explanation of
  why said code can't just be deleted.
- **Bugs:** You can use comments to indicate bugs in our code, as well as
  third-party bugs. In both cases, the comment should link to the issue tracker
  where the bug has been reported.

## Program Design
This section intentionally left blank for Wojciech to fill in.