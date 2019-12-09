open TestFramework;
open Library.Day4;

let input = Library.Util.getProblemInput(4);

describe("day 4", ({test}) => {
  test("isValid", ({expect}) => {
    expect.bool(isValid(123444, true)).toBeTrue();
    expect.bool(isValid(123444, false)).toBeFalse();
    expect.bool(isValid(111122, false)).toBeTrue();
  });

  test("a", ({expect}) => {
    expect.int(a("000,111")).toBe(10);
    expect.int(a(input)).toBe(1686);
  });

  test("b", ({expect}) => {
    expect.int(b("000,111")).toBe(9);
    expect.int(b(input)).toBe(1145);
  });
});