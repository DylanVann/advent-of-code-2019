/** Get the input for a given problem. */
let getProblemInput = (number: int) => {
  let file = "input/" ++ "Day" ++ string_of_int(number) ++ ".txt";
  let ic = open_in(file);
  try({
    let numberOfBytes = in_channel_length(ic);
    let bytes = Bytes.create(numberOfBytes);
    really_input(ic, bytes, 0, numberOfBytes);
    let contents = Bytes.unsafe_to_string(bytes);
    close_in(ic);
    contents;
  }) {
  | e =>
    close_in_noerr(ic);
    raise(e);
  };
};

let add = (a, b) => a + b;

let multiply = (a, b) => a * b;

let splitLines = (input: string): list(string) =>
  Str.split(Str.regexp("\n"), input);

let splitCommas = (input: string): list(string) =>
  Str.split(Str.regexp(","), input);

let splitInts = (s: string): list(int) =>
  List.init(
    String.length(s),
    l => {
      let sub = String.make(1, s.[l]);
      int_of_string(sub);
    },
  );

let print_opt = n =>
  switch (n) {
  | None => print_endline("")
  | Some(n) => print_endline(string_of_int(n))
  };

let nth_opt = (list, n) =>
  if (n < 0) {
    None;
  } else if (n > List.length(list) - 1) {
    None;
  } else {
    Some(List.nth(list, n));
  };

let rec range = (start: int, end_: int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  };

let string_of_int_array = (mem: array(int)) =>
  String.concat(",", List.map(string_of_int, Array.to_list(mem)));

module StringMap = Map.Make(String);

let rec indexOf = (a, x, n): int =>
  if (n > List.length(a) - 1) {
    (-1);
  } else if (List.nth(a, n) == x) {
    n;
  } else {
    indexOf(a, x, n + 1);
  };

let rm = (x, l) => List.filter((!=)(x), l);

let rec permutations =
  fun
  | [] => []
  | [x] => [[x]]
  | l =>
    List.fold_left(
      (acc, x) => acc @ List.map(p => [x, ...p], permutations(rm(x, l))),
      [],
      l,
    );