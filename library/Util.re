/** Returns a colorful "Hello, World" message.

    {4 Examples}

    {[
    print_endline(hello());
    ]} */
let hello = () =>
  Pastel.(
    <Pastel>
      <Pastel color=Red> "Hello" </Pastel>
      ", "
      <Pastel color=Green> "World" </Pastel>
      "!"
    </Pastel>
  );

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