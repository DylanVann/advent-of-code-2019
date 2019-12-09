open TestFramework;
open Library.Day5;

let input = Library.Util.getProblemInput(5);

describe("day 5", ({test}) => {
  test("modes", ({expect}) => {
    expect.string(general("1101,100,-1,4,0", 0).mem).toEqual(
      "1101,100,-1,4,99",
    );
    expect.string(general("0101,100,4,4,-1", 0).mem).toEqual(
      "101,100,4,4,99",
    );
    expect.string(general("101,100,4,4,-1", 0).mem).toEqual(
      "101,100,4,4,99",
    );
    expect.string(general("3,0,4,0,99", 66).mem).toEqual("66,0,4,0,99");
    expect.int(List.hd(general("3,0,4,0,99", 66).outputs)).toBe(66);

    /* Is the input 8? */
    expect.int(List.hd(general("3,9,8,9,10,9,4,9,99,-1,8", 8).outputs)).toBe(
      1,
    );
    expect.int(List.hd(general("3,9,8,9,10,9,4,9,99,-1,8", 7).outputs)).toBe(
      0,
    );

    /* Is the input less than 8? */
    expect.int(List.hd(general("3,9,7,9,10,9,4,9,99,-1,8", 8).outputs)).toBe(
      0,
    );
    expect.int(List.hd(general("3,9,7,9,10,9,4,9,99,-1,8", 7).outputs)).toBe(
      1,
    );

    /* Is the input 8? (immediate) */
    expect.int(List.hd(general("3,3,1108,-1,8,3,4,3,99", 8).outputs)).toBe(
      1,
    );
    expect.int(List.hd(general("3,3,1108,-1,8,3,4,3,99", 7).outputs)).toBe(
      0,
    );

    /* Is input less than 8? (immediate) */
    expect.int(List.hd(general("3,3,1107,-1,8,3,4,3,99", 8).outputs)).toBe(
      0,
    );
    expect.int(List.hd(general("3,3,1107,-1,8,3,4,3,99", 7).outputs)).toBe(
      1,
    );

    /* More examples... */
    let program = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9";
    expect.int(List.hd(general(program, 0).outputs)).toBe(0);
    expect.int(List.hd(general(program, 66).outputs)).toBe(1);

    /* The big example... */
    let big = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99";
    expect.int(List.hd(general(big, 6).outputs)).toBe(999);
    expect.int(List.hd(general(big, 8).outputs)).toBe(1000);
    expect.int(List.hd(general(big, 22).outputs)).toBe(1001);
  });

  test("a", ({expect}) => {
    expect.int(List.hd(general(input, 1).outputs)).toBe(16348437)
  });

  test("b", ({expect}) => {
    expect.int(List.hd(general(input, 5).outputs)).toBe(6959377)
  });
});