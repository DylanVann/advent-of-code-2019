let getFuel = (mass: int): int =>
  int_of_float(floor(float_of_int(mass) /. 3.0)) - 2;

let rec getFuelAccountingForFuelForFuel = (mass: int): int => {
  let fuel = getFuel(mass);
  if (fuel < 0) {
    0;
  } else {
    fuel + getFuelAccountingForFuelForFuel(fuel);
  };
};

let a = input => {
  let lines = Str.split(Str.regexp("\n"), input);
  List.fold_left(
    (acc, mass) => acc + getFuel(int_of_string(mass)),
    0,
    lines,
  );
};

let b = input => {
  let lines = Str.split(Str.regexp("\n"), input);
  List.fold_left(
    (acc, mass) =>
      acc + getFuelAccountingForFuelForFuel(int_of_string(mass)),
    0,
    lines,
  );
};