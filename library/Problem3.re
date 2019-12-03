type state = {
  x: int,
  y: int,
  steps: int,
};

let key_of_state = point =>
  string_of_int(point.x) ++ ":" ++ string_of_int(point.y);

exception InvalidInput;

let getNextPoint = (s: state, direction: string, steps: int) => {
  let withSteps = {...s, steps: s.steps + steps};
  switch (direction) {
  | "U" => {...withSteps, y: s.y + steps}
  | "D" => {...withSteps, y: s.y - steps}
  | "R" => {...withSteps, x: s.x + steps}
  | "L" => {...withSteps, x: s.x - steps}
  | _ => raise(InvalidInput)
  };
};

module StringMap = Map.Make(String);

type result = {
  minManhattanDistance: int,
  minCombinedSteps: int,
};

let calculate = (l1: list(string), l2: list(string)): result => {
  let myMap = ref(StringMap.empty);
  let minCombinedSteps = ref(max_int);
  let minManhattanDistance = ref(max_int);

  List.fold_left(
    (acc, v) => {
      let direction = String.make(1, v.[0]);
      let steps = int_of_string(String.sub(v, 1, String.length(v) - 1));
      for (s in 0 to steps) {
        let nextPoint = getNextPoint(acc, direction, s);
        let key = key_of_state(nextPoint);
        /* Only set the first time. */
        if (!StringMap.mem(key, myMap^)) {
          myMap := StringMap.add(key, nextPoint.steps, myMap^);
        };
      };
      getNextPoint(acc, direction, steps);
    },
    {x: 0, y: 0, steps: 0},
    l1,
  )
  |> ignore;

  List.fold_left(
    (acc, v) => {
      let direction = String.make(1, v.[0]);
      let steps = int_of_string(String.sub(v, 1, String.length(v) - 1));
      for (s in 0 to steps) {
        let nextPoint = getNextPoint(acc, direction, s);
        let key = key_of_state(nextPoint);
        let isIntersection: bool = StringMap.mem(key, myMap^);
        let isStart = nextPoint.x == 0 && nextPoint.y == 0;
        if (isIntersection && !isStart) {
          let prevSteps = StringMap.find(key, myMap^);
          let totalSteps = nextPoint.steps + prevSteps;
          let manhattanDistance = abs(nextPoint.x) + abs(nextPoint.y);
          if (manhattanDistance < minManhattanDistance^) {
            minManhattanDistance := manhattanDistance;
          };
          if (totalSteps < minCombinedSteps^) {
            minCombinedSteps := totalSteps;
          };
        };
      };
      getNextPoint(acc, direction, steps);
    },
    {x: 0, y: 0, steps: 0},
    l2,
  )
  |> ignore;

  {
    minManhattanDistance: minManhattanDistance^,
    minCombinedSteps: minCombinedSteps^,
  };
};

let calculateFromInput = input => {
  let lines = Util.splitLines(input);
  let l1 = Util.splitCommas(List.nth(lines, 0));
  let l2 = Util.splitCommas(List.nth(lines, 1));
  calculate(l1, l2);
};

let a = (input: string) => {
  calculateFromInput(input).minManhattanDistance;
};

let b = (input: string) => {
  calculateFromInput(input).minCombinedSteps;
};