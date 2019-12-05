let isValid = (v: int, allowMoreThanDoubles: bool): bool => {
  let stringRep = string_of_int(v);
  let maxNum = ref(0);
  let hasDouble = ref(false);
  let onlyIncreases = ref(true);
  let digits = Util.int_list_of_string(stringRep);
  List.mapi(
    (i: int, x1: int) => {
      let x0 = Util.nth_opt(digits, i - 1);
      let x2 = Util.nth_opt(digits, i + 1);
      let x3 = Util.nth_opt(digits, i + 2);
      let isIncreasing = x1 >= maxNum^;
      if (x1 > maxNum^) {
        maxNum := x1;
      };
      if (!isIncreasing) {
        onlyIncreases := false;
      };
      let isDouble =
        allowMoreThanDoubles
          ? Some(x1) == x2 : x0 != Some(x1) && Some(x1) == x2 && x2 != x3;
      if (isDouble) {
        hasDouble := true;
      };
    },
    digits,
  )
  |> ignore;

  hasDouble^ && onlyIncreases^;
};

let a = (a: int, b: int) => {
  let validPasswords = ref(0);
  for (i in a to b) {
    if (isValid(i, true)) {
      validPasswords := validPasswords^ + 1;
    };
  };
  validPasswords^;
};

let b = (a: int, b: int) => {
  let validPasswords = ref(0);
  for (i in a to b) {
    if (isValid(i, false)) {
      validPasswords := validPasswords^ + 1;
    };
  };
  validPasswords^;
};