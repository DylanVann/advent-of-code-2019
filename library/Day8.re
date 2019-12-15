type digitWithIndex = {
  v: int,
  i: int,
};

type layerIndexAndZeros = {
  layer: int,
  zeros: int,
};

let countDigit = (layer, digit) =>
  Array.fold_left((acc, v) => v == digit ? acc + 1 : acc, 0, layer);

let getLayers = (~mem, ~w: int, ~h: int) => {
  let digits = Util.splitInts(mem);
  let nDigitsInLayer = w * h;
  let nLayers = List.length(digits) / (w * h);
  let layers = Array.make_matrix(nLayers, nDigitsInLayer, 0);
  List.mapi(
    (i, v) => {
      let li = i mod nDigitsInLayer;
      let layer = i / (w * h);
      layers[layer][li] = v;
    },
    digits,
  )
  |> ignore;
  layers;
};

let a = (~mem: string, ~w: int, ~h: int): int => {
  let layers = getLayers(~mem, ~w, ~h);
  let minZeros = ref(max_int);
  let minZerosLayerIdx = ref(0);
  Array.mapi(
    (i, layer) => {
      let zeros = countDigit(layer, 0);
      if (zeros < minZeros^) {
        minZeros := zeros;
        minZerosLayerIdx := i;
      };
    },
    layers,
  )
  |> ignore;

  let layerWithMostZeros = layers[minZerosLayerIdx^];
  let ones = countDigit(layerWithMostZeros, 1);
  let twos = countDigit(layerWithMostZeros, 2);
  ones * twos;
};

let getPixelColor = (layers: array(array(int)), x: int, y: int, w: int): int => {
  Array.fold_left(
    (acc, layer) => {
      let index = x + y * w;
      switch (acc, layer[index]) {
      | (2, x) => x
      | _ => acc
      };
    },
    2,
    layers,
  );
};

let b = (~mem: string, ~w: int, ~h: int): string => {
  let layers = getLayers(~mem, ~w, ~h);
  let image = ref("");
  for (y in 0 to h - 1) {
    for (x in 0 to w - 1) {
      let color = getPixelColor(layers, x, y, w);
      switch (color) {
      | 0 => image := image^ ++ " "
      | 1 => image := image^ ++ "+"
      | _ => raise(Invalid_argument("invalid"))
      };
    };
    image := image^ ++ "\n";
  };
  image^;
};