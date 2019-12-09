open TestFramework;
open Library.Day6;

let input = Library.Util.getProblemInput(6);

describe("day 6", ({test}) => {
  test("handles example input", ({expect}) => {
    let exampleInput = "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L";
    /*
             G - H       J - K - L
            /           /
     COM - B - C - D - E - F
                    \
                     I
     */
    expect.int(a(exampleInput)).toBe(42);
  });

  test("a", ({expect}) => {
    expect.int(a(input)).toBe(223251)
  });

  test("b", ({expect}) => {
    let example = "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN";
    expect.int(b(example)).toBe(4);
    expect.int(b(input)).toBe(430);
  });
});
