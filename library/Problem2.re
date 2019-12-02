let parse = (input: string): array(int) => {
  let strings = Str.split(Str.regexp(","), input);
  Array.map(int_of_string, Array.of_list(strings));
};

exception InvalidInput;

let rec process = (input: array(int), position: int): array(int) => {
  switch (input[position]) {
  | 1
  | 2 =>
    let a = input[input[position + 1]];
    let b = input[input[position + 2]];
    let operator =
      switch (input[position]) {
      | 1 => Util.add
      | 2 => Util.multiply
      | _ => raise(InvalidInput)
      };
    let c = operator(a, b);
    input[input[position + 3]] = c;
    process(input, position + 4);
  | 99 => input
  | _ => raise(InvalidInput)
  };
};

/**
 * Test the processor without modifying 1 and 2.
 */
let general = (input: string): string => {
  let numbers = parse(input);
  let result = process(numbers, 0);
  String.concat(",", List.map(string_of_int, Array.to_list(result)));
};

let a = (input: string): string => {
  let numbers = parse(input);
  numbers[1] = 12;
  numbers[2] = 2;
  let result = process(numbers, 0);
  String.concat(",", List.map(string_of_int, Array.to_list(result)));
};

let b = (input: string, target: int): int => {
  let numbers = parse(input);
  let noun = ref(0);
  let verb = ref(0);
  for (i in 0 to 100) {
    for (j in 0 to 100) {
      let clone = Array.copy(numbers);
      clone[1] = i;
      clone[2] = j;
      let result = process(clone, 0);
      if (result[0] == target) {
        noun := i;
        verb := j;
      };
    };
  };
  noun^ * 100 + verb^;
};