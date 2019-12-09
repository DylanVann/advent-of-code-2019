open Util;

type planet = {
  key: string,
  orbits: option(string),
};

let rec getOrbitsCount = (p: planet, map: StringMap.t(planet)) => {
  switch (p.orbits) {
  | Some(k) => 1 + getOrbitsCount(StringMap.find(k, map), map)
  | None => 0
  };
};

let getMap = (lines: list(string)) =>
  List.fold_left(
    (acc: StringMap.t(planet), line: string) => {
      let nextMap = ref(acc);
      let planets = Str.split(Str.regexp(")"), line);

      let orbitedKey = List.nth(planets, 0);
      let orbited = {orbits: None, key: orbitedKey};
      /* Orbited might already exist in the map. */
      /* If it does we don't need to add it. */
      if (!StringMap.mem(orbitedKey, nextMap^)) {
        nextMap := StringMap.add(orbitedKey, orbited, nextMap^);
      };

      let orbiterKey = List.nth(planets, 1);
      let orbiter = {orbits: Some(orbitedKey), key: orbiterKey};
      /* We always need to add the orbiter. */
      nextMap := StringMap.add(orbiterKey, orbiter, nextMap^);

      nextMap^;
    },
    StringMap.empty,
    lines,
  );

let a = (input: string) => {
  let lines = Util.splitLines(input);
  let map = getMap(lines);
  StringMap.fold(
    (key, a, count) => {count + getOrbitsCount(a, map)},
    map,
    0,
  );
};

let rec getOrbits = (p: planet, m: StringMap.t(planet)): list(string) => {
  switch (p.orbits) {
  | Some(k) => [k, ...getOrbits(StringMap.find(k, m), m)]
  | None => []
  };
};

let b = (input: string) => {
  let lines = Util.splitLines(input);
  let map = getMap(lines);
  let santaOrbits = getOrbits(StringMap.find("SAN", map), map);
  let myOrbits = getOrbits(StringMap.find("YOU", map), map);
  /* transfers = dist(santa, common) + dist(you, common) */
  let dist = ref(-1);
  List.iteri(
    (i: int, orbit: string) => {
      let indexOfOrbitInSantaOrbits = Util.indexOf(santaOrbits, orbit, 0);
      if (indexOfOrbitInSantaOrbits != (-1) && dist^ == (-1)) {
        dist := i + indexOfOrbitInSantaOrbits;
      };
    },
    myOrbits,
  );
  dist^;
};
