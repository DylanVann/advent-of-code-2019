/** Get the input for a given problem. */
let getProblemInput = (number: int) => {
  let file = "input/" ++ string_of_int(number) ++ "/input.txt";
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

let splitLines = input => Str.split(Str.regexp("\n"), input);

let splitCommas = input => Str.split(Str.regexp(","), input);

let int_list_of_string = s =>
  List.init(String.length(s), l => int_of_string(String.make(1, s.[l])));

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