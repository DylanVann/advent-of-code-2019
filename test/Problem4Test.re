open TestFramework;
open Library.Problem4;

describe("problem 4", ({test}) => {
  test("isValid", ({expect}) => {
    expect.bool(isValid(123444, true)).toBeTrue();
    expect.bool(isValid(123444, false)).toBeFalse();
    expect.bool(isValid(111122, false)).toBeTrue();
  });

  test("a", ({expect}) => {
    expect.int(a(000, 111)).toBe(10);
    expect.int(a(168630, 718098)).toBe(1686);
  });

  test("b", ({expect}) => {
    expect.int(b(000, 111)).toBe(9);
    expect.int(b(168630, 718098)).toBe(1145);
  });
});