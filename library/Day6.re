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

let a = (input: string) => {
  let lines = Util.splitLines(input);
  let structure =
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

  let totalCount: int =
    StringMap.fold(
      (key, a, count) => {count + getOrbitsCount(a, structure)},
      structure,
      0,
    );

  totalCount;
};

let b = a;
