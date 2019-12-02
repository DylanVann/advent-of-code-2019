let getFuel = (mass: int): int =>
  int_of_float(floor(float_of_int(mass) /. 3.0)) - 2;

let rec getFuelAccountingForFuelForFuel = (fuelMass: int): int => {
  let fuelForFuel = getFuel(fuelMass);
  if (fuelForFuel < 0) {
    fuelMass;
  } else {
    fuelMass + getFuelAccountingForFuelForFuel(fuelForFuel);
  };
};

let getFuelForModule = moduleMass => {
  let fuel = getFuel(moduleMass);
  getFuelAccountingForFuelForFuel(fuel);
};

let problem_1_a = input => {
  let lines = Str.split(Str.regexp("\n"), input);
  List.fold_left(
    (acc, mass) => acc + getFuel(int_of_string(mass)),
    0,
    lines,
  );
};

let problem_1_b = input => {
  let lines = Str.split(Str.regexp("\n"), input);
  List.fold_left(
    (acc, mass) => acc + getFuelForModule(int_of_string(mass)),
    0,
    lines,
  );
};