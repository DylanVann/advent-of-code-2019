open TestFramework;
open Library.Day8;

let input = Library.Util.getProblemInput(8);

describe("day 8", ({test}) => {
  test("example", ({expect}) => {
    expect.int(a(~mem="123456789012", ~w=3, ~h=2)).toBe(1)
  });

  test("a", ({expect}) => {
    expect.int(a(~mem=input, ~w=25, ~h=6)).toBe(2500)
  });

  test("b example", ({expect}) => {
    let image = "0222112222120000";
    let output = " +\n+ \n";
    expect.string(b(~mem=image, ~w=2, ~h=2)).toEqual(output);
  });

  test("b", ({expect}) => {
    let output = b(~mem=input, ~w=25, ~h=6);
    let expected = " ++  +   ++  +  ++  +  + \n+  + +   ++  + +  + +  + \n+     + + +  + +  + ++++ \n+      +  +  + ++++ +  + \n+  +   +  +  + +  + +  + \n ++    +   ++  +  + +  + \n";
    expect.string(output).toEqual(expected);
  });
});