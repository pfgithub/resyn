some goals:

- the language should parse itself. in order to implement the language, only a basic parser should
  be required because past that point the language makes its own parser and parses itself
- the language should emit itself. in order to implement the language, only a basic interpereter
  should be required because the language can then be called to compile itself for a given platform
- imports should use a consistent syntax to easily be able to find out the complete source code area
  of a project.

some fun ideas:

- unconsting things. things will become const when you store them in a variable or pass them into a
  fn arg eg and you have to unconst them with `&` or something. like

  ```resyn
  \const value = {one = number: 2, three = 4};
  value.one = 3; // error: value.one is constant
  \const value = &{one = number: 2, three = 4};
  value.one = 3; // ok
  ```

- prefixed identifiers. this also works nicely with a custom tree-based editor because eg you can
  press backslash and it brings up the list of identifiers and stuff

- watchable values and watchable fns

  ```resyn
  widget Counter(value: $number, setValue: (number -> number) -> void) {
    return .div(value, .button(setValue(v => v++)))
  }
  ```

  you can pass watchable values to normal fns and it just tracks if the normal fns use them / what
  they access. anything they use will require a recomputation on change.

  this adds side effects to `@mapget` which isn't great, or it makes container types/interfaces
  required for `.` in a syntax

some issues:

- I want comptime to execute from top to bottom, but I can't do that because order independent
  declarations are kind of important
- I want to always have editor functionality (eg get types on hover) but I can't do that if
  something never gets analyzed. (to analyze a fn you need to know the type of something
  but the type of something might depend on a variable that's defined inside of a comptime
  only function which can only be known if it's called. ideally you should avoid comptime
  only functions as much as possible)
- I want to not have built in expressions that do too much at once (eg function currently
  gets the env and sets a new property on it and I'd rather you do the mapset eg) but also
  I don't want to make you have to do a withenv {...env.a, env.b} in both the return type
  branch and body branch of a fn. actually that's probably fine to do
