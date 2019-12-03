open TestFramework;
open Library;
open Library.Problem3;

let input = Library.Util.getProblemInput(3);

describe("problem 3", ({test}) => {
  test("a", ({expect}) => {
    expect.int(
      a(
        "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83",
      ),
    ).
      toBe(
      159,
    );
    expect.int(
      a(
        "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
      ),
    ).
      toBe(
      135,
    );
    expect.int(a(input)).toBe(557);
  });

  test("b", ({expect}) => {
    expect.int(b(input)).toBe(56410);
  });
});