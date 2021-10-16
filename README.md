# FSharp List Dojo

Learn about pattern matching and recursion by creating a your own list using the discriminated union!


## Installation

Install the .NET5 SDK.

Clone this git repository.

Use the following command to restore the tool.

```
dotnet tool restore
```


## Getting started

Start the test runner.

```
dotnet watch run -p test
```

If you are using Visual Studio, you can also test it from Test Explorer.

At first, all tests are pending.
Open the file `TernaryTest.fs` in the `ListDojo.Test` project and remove one of the prefixes `p`.

```diff
- ptestList "tnot properties" [
+ testList "tnot properties" [
```

Open the `TernaryDojo.fs` file in the `ListDojo` project and implement the `tnot` function.

```diff
- let tnot a = Hole?implement_tnot
+ let tnot a =
+     match a with
+     | False -> True
+     | Null -> Null
+     | True -> False
```

Confirm that the test passes.

Repeat this process to pass all the test cases!

1. Remove one `p` prefix from the test file.
1. Implement the function to be tested.
1. Confirm that the test passes.


## List of materials

1. `TernaryDojo`
	- You will create your own tiny library for three-valued logic and learn about the basic use of discriminated unions and pattern matching.
1. `OptionDojo`
	- You will create your own `option` type, which is a type for safely handling `null`, and learn that discriminated unions are more useful than mere enums.
1. `ListDojo`
	- You will create your own `list` type using a discriminated union. Topics such as tail-recursion will also be covered.


## Pending tests

The `p` stands for pending.
The prefix `p` can be removed to include it in the test as below.

- `ptestList` -> `testList`
- `ptestCase` -> `testCase`
- `ptestProperty` -> `testProperty`

