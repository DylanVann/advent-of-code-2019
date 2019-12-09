let parse = (input: string): array(int) => {
  let strings = Str.split(Str.regexp(","), input);
  Array.map(int_of_string, Array.of_list(strings));
};

exception InvalidInput;

type opCode =
  | Add
  | Mult
  | Input
  | Output
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  | End;

type parameterMode =
  | Immediate
  | Position;

let getOpCode = (input: int): opCode =>
  switch (input) {
  | 1 => Add
  | 2 => Mult
  | 3 => Input
  | 4 => Output
  | 5 => JumpIfTrue
  | 6 => JumpIfFalse
  | 7 => LessThan
  | 8 => Equals
  | 99 => End
  | _ => raise(InvalidInput)
  };

let getParametersCount = (code: opCode) =>
  switch (code) {
  | Add => 3
  | Mult => 3
  | Input => 1
  | Output => 1
  | JumpIfTrue => 2
  | JumpIfFalse => 2
  | LessThan => 3
  | Equals => 3
  | End => 0
  };

type opCodeInfo = {
  opCode,
  parametersCount: int,
  modeDigits: int,
};

let getOpCodeInfo = (value: int): opCodeInfo => {
  let opCode = getOpCode(value mod 100);
  let modeDigits = int_of_float(floor(float_of_int(value) /. 100.0));
  {opCode, modeDigits, parametersCount: getParametersCount(opCode)};
};

let getParameterMode = (modeDigits: int, position: int): parameterMode => {
  let intList = Util.int_list_of_string(string_of_int(modeDigits));
  let atPos = Util.nth_opt(intList, List.length(intList) - 1 - position);
  switch (atPos) {
  | Some(0) => Position
  | Some(1) => Immediate
  | None => Position
  | _ => raise(InvalidInput)
  };
};

type state = {
  input: int,
  position: int,
  mem: array(int),
  outputs: list(int),
};

let getParameter = (s: state, num: int): int => {
  let {mem, position, outputs, input} = s;
  let value = mem[position];
  let {opCode, modeDigits, parametersCount} = getOpCodeInfo(value);
  let aMode = getParameterMode(modeDigits, num);
  switch (aMode) {
  | Immediate => mem[position + num + 1]
  | Position => mem[mem[position + num + 1]]
  };
};

let rec process = (s: state): state => {
  let {mem, position, outputs, input} = s;
  let value = mem[position];
  let {opCode, modeDigits, parametersCount} = getOpCodeInfo(value);
  let nextPosition = position + parametersCount + 1;
  switch (opCode) {
  | Add
  | Mult =>
    let a = getParameter(s, 0);
    let b = getParameter(s, 1);
    let operator =
      switch (opCode) {
      | Add => Util.add
      | Mult => Util.multiply
      | _ => raise(InvalidInput)
      };
    let output = operator(a, b);
    mem[mem[position + 3]] = output;
    process({...s, position: nextPosition});
  | Input =>
    mem[mem[position + 1]] = input;
    process({...s, position: nextPosition});
  | Output =>
    let outputNumber = getParameter(s, 0);
    process({
      ...s,
      position: nextPosition,
      outputs: [outputNumber, ...s.outputs],
    });
  | JumpIfTrue =>
    let a = getParameter(s, 0);
    if (a != 0) {
      process({...s, position: getParameter(s, 1)});
    } else {
      process({...s, position: nextPosition});
    };
  | JumpIfFalse =>
    let a = getParameter(s, 0);
    if (a == 0) {
      process({...s, position: getParameter(s, 1)});
    } else {
      process({...s, position: nextPosition});
    };
  | LessThan =>
    let a = getParameter(s, 0);
    let b = getParameter(s, 1);
    let outIdx = s.mem[s.position + 3];
    if (a < b) {
      mem[outIdx] = 1;
    } else {
      mem[outIdx] = 0;
    };
    process({...s, position: nextPosition});
  | Equals =>
    let a = getParameter(s, 0);
    let b = getParameter(s, 1);
    let outIdx = s.mem[s.position + 3];
    if (a == b) {
      mem[outIdx] = 1;
    } else {
      mem[outIdx] = 0;
    };
    process({...s, position: nextPosition});
  | End => s
  };
};

type out = {
  mem: string,
  outputs: list(int),
};

let general = (mem: string, input: int) => {
  let numbers = parse(mem);
  let res = process({input, outputs: [], position: 0, mem: numbers});
  {mem: Util.string_of_int_array(res.mem), outputs: res.outputs};
};