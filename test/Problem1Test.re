open TestFramework;

let input = Library.Util.getProblemInput(1);

describe("problem 1", ({test}) => {
  test("problem 1 a", ({expect}) => {
    expect.int(Library.Problem1.problem_1_a(input)).toBe(3423511)
  })
});