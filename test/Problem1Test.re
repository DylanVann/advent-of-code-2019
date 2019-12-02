open TestFramework;
open Library.Problem1;

let input = Library.Util.getProblemInput(1);

describe("problem 1", ({test}) => {
  test("a", ({expect}) => {
    expect.int(a(string_of_int(1969))).toBe(654);
    expect.int(a(input)).toBe(3423511);
  });

  test("b", ({expect}) => {
    expect.int(b(string_of_int(1969))).toBe(966);
    expect.int(b(input)).toBe(5132379);
  });
});