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
  let intList = Util.splitInts(string_of_int(modeDigits));
  let atPos = Util.nth_opt(intList, List.length(intList) - 1 - position);
  switch (atPos) {
  | Some(0) => Position
  | Some(1) => Immediate
  | None => Position
  | _ => raise(InvalidInput)
  };
};

type processState =
  | PDefault
  | PNeedsInput
  | PHasOutput
  | PDone;

type state = {
  state: processState,
  input: int,
  firstInput: option(int),
  position: int,
  mem: array(int),
  output: int,
};

let getParameter = (s: state, num: int): int => {
  let {mem, position} = s;
  let value = mem[position];
  let {opCode, modeDigits, parametersCount} = getOpCodeInfo(value);
  let aMode = getParameterMode(modeDigits, num);
  switch (aMode) {
  | Immediate => mem[position + num + 1]
  | Position => mem[mem[position + num + 1]]
  };
};

let rec process = (s: state): state => {
  let {mem, position} = s;
  let value = mem[position];
  let {opCode, modeDigits, parametersCount} = getOpCodeInfo(value);
  let nextPosition = position + parametersCount + 1;
  switch (opCode, s.state) {
  | (_, PNeedsInput) =>
    switch (s.firstInput) {
    | Some(x) =>
      mem[mem[position + 1]] = x;
      process({
        ...s,
        firstInput: None,
        state: PDefault,
        position: nextPosition,
      });
    | None =>
      mem[mem[position + 1]] = s.input;
      process({...s, state: PDefault, position: nextPosition});
    }
  | (_, PHasOutput) =>
    process({...s, state: PDefault, position: nextPosition})
  | (Add, _)
  | (Mult, _) =>
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
  | (Input, _) => {...s, state: PNeedsInput}
  | (Output, _) =>
    let output = getParameter(s, 0);
    {...s, state: PHasOutput, output};
  | (JumpIfTrue, _) =>
    let a = getParameter(s, 0);
    if (a != 0) {
      process({...s, position: getParameter(s, 1)});
    } else {
      process({...s, position: nextPosition});
    };
  | (JumpIfFalse, _) =>
    let a = getParameter(s, 0);
    if (a == 0) {
      process({...s, position: getParameter(s, 1)});
    } else {
      process({...s, position: nextPosition});
    };
  | (LessThan, _) =>
    let a = getParameter(s, 0);
    let b = getParameter(s, 1);
    let outIdx = s.mem[s.position + 3];
    if (a < b) {
      mem[outIdx] = 1;
    } else {
      mem[outIdx] = 0;
    };
    process({...s, position: nextPosition});
  | (Equals, _) =>
    let a = getParameter(s, 0);
    let b = getParameter(s, 1);
    let outIdx = s.mem[s.position + 3];
    if (a == b) {
      mem[outIdx] = 1;
    } else {
      mem[outIdx] = 0;
    };
    process({...s, position: nextPosition});
  | (End, _) => {...s, state: PDone}
  };
};

type out = {
  mem: string,
  output: int,
};

let amplify = (mem: string, inputs: list(int)): int => {
  let output =
    List.fold_left(
      (previousOutput: int, nextInput: int) => {
        let break = ref(false);
        let input = ref(-1);
        let output = ref(-1);
        let state =
          ref({
            state: PDefault,
            firstInput: None,
            input: 0,
            output: 0,
            position: 0,
            mem: parse(mem),
          });
        while (! break^) {
          let res = process({...state^, input: input^});
          state := res;
          switch (res.state) {
          | PNeedsInput => input := input^ == (-1) ? nextInput : previousOutput
          | PHasOutput => output := res.output
          | PDone => break := true
          | PDefault => raise(InvalidInput)
          };
        };
        output^;
      },
      0,
      inputs,
    );
  output;
};

type permState = {
  maxOutput: int,
  inputs: list(int),
};

let getInputsForMaxOutput = (mem: string) => {
  let inputs = [0, 1, 2, 3, 4];
  let permutations = Util.permutations(inputs);
  List.fold_left(
    (acc, perm) => {
      let a = amplify(mem, perm);
      if (a > acc.maxOutput) {
        {maxOutput: a, inputs: perm};
      } else {
        acc;
      };
    },
    {maxOutput: 0, inputs: []},
    permutations,
  );
};

let a = (mem: string) => {
  getInputsForMaxOutput(mem).maxOutput;
};

let getSignal = (mem: string, inputs: list(int)) => {
  let states: array(state) =
    Array.of_list(
      List.map(
        v =>
          {
            state: PDefault,
            firstInput: Some(v),
            input: 0,
            output: 0,
            position: 0,
            mem: parse(mem),
          },
        inputs,
      ),
    );

  let break = ref(false);
  let idx = ref(0);
  let output = ref(0);
  while (! break^) {
    let s = states[idx^];
    let sNext = process({...s, input: output^});
    states[idx^] = sNext;
    switch (sNext.state) {
    | PNeedsInput => ()
    | PHasOutput => output := sNext.output
    | PDone => break := true
    | PDefault => raise(InvalidInput)
    };
    idx := (idx^ + 1) mod 5;
  };

  output^;
};

let b = (mem: string) => {
  let inputs = [5, 6, 7, 8, 9];
  let permutations = Util.permutations(inputs);
  List.fold_left(
    (acc, perm) => {
      let a = getSignal(mem, perm);
      if (a > acc.maxOutput) {
        {maxOutput: a, inputs: perm};
      } else {
        acc;
      };
    },
    {maxOutput: 0, inputs: []},
    permutations,
  ).
    maxOutput;
};