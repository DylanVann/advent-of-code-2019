open TestFramework;
open Library.Day7;

let input = Library.Util.getProblemInput(7);

describe("day 7", ({test}) => {
  test("a example", ({expect}) => {
    let e1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0";
    expect.int(amplify(e1, [4, 3, 2, 1, 0])).toBe(43210);
    expect.list(getInputsForMaxOutput(e1).inputs).toEqual([4, 3, 2, 1, 0]);
    let e2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0";
    expect.int(amplify(e2, [0, 1, 2, 3, 4])).toBe(54321);
    expect.list(getInputsForMaxOutput(e2).inputs).toEqual([0, 1, 2, 3, 4]);
  });

  test("a", ({expect}) => {
    expect.int(a(input)).toBe(422858)
  });

  test("b example", ({expect}) => {
    let e1 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5";
    expect.int(getSignal(e1, [9, 8, 7, 6, 5])).toBe(139629729);
    let e2 = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10";
    expect.int(getSignal(e2, [9, 7, 8, 5, 6])).toBe(18216);
  });

  test("b", ({expect}) => {
    expect.int(b(input)).toBe(14897241)
  })
});