module type StylishSig = {
  type t;

  /* Helper functions */
  let empty: t;
  let concatList: (t, list(t)) => t;
  /* Styling */
  let bold: string => t;
  let invert: string => t;
  let dim: string => t;
  let underline: string => t;

  let normal: (~underline: bool=?, ~invert: bool=?, ~dim: bool=?, ~bold: bool=?, string) => t;
  let red: (~underline: bool=?, ~invert: bool=?, ~dim: bool=?, ~bold: bool=?, string) => t;
  let yellow: (~underline: bool=?, ~invert: bool=?, ~dim: bool=?, ~bold: bool=?, string) => t;
  let blue: (~underline: bool=?, ~invert: bool=?, ~dim: bool=?, ~bold: bool=?, string) => t;
  let green: (~underline: bool=?, ~invert: bool=?, ~dim: bool=?, ~bold: bool=?, string) => t;
  let cyan: (~underline: bool=?, ~invert: bool=?, ~dim: bool=?, ~bold: bool=?, string) => t;
  let purple: (~underline: bool=?, ~invert: bool=?, ~dim: bool=?, ~bold: bool=?, string) => t;
  let highlight:
    (
      ~underline: bool=?,
      ~invert: bool=?,
      ~dim: bool=?,
      ~bold: bool=?,
      ~color: (~underline: bool=?, ~invert: bool=?, ~dim: bool=?, ~bold: bool=?, string) => t=?,
      ~first: int=?,
      ~last: int=?,
      string
    ) =>
    t;
};

module ANSIStylish: StylishSig = {
  type t = string;
  let empty = "";
  let concatList = String.concat;

  let resetANSI = "\027[0m";

  let bold = s => "\027[1m" ++ s ++ resetANSI;

  let boldCode = b => b ? "\027[1m" : "";

  let invert = s => "\027[7m" ++ s ++ resetANSI;

  let invertCode = b => b ? "\027[7m" : "";

  let dim = s => "\027[2m" ++ s ++ resetANSI;

  let dimCode = b => b ? "\027[2m" : "";

  let underlineCode = u => u ? "\027[4m" : "";

  let normalCode = "\027[39m";

  let redCode = "\027[31m";

  let yellowCode = "\027[33m";

  let blueCode = "\027[34m";

  let greenCode = "\027[32m";

  let purpleCode = "\027[35m";

  let cyanCode = "\027[36m";

  let underline = s => underlineCode(true) ++ s ++ resetANSI;

  let normal = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) =>
    underlineCode(underline)
    ++ invertCode(invert)
    ++ dimCode(dim)
    ++ boldCode(bold)
    ++ normalCode
    ++ s
    ++ resetANSI;

  let red = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) =>
    underlineCode(underline)
    ++ invertCode(invert)
    ++ dimCode(dim)
    ++ boldCode(bold)
    ++ redCode
    ++ s
    ++ resetANSI;

  let yellow = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) =>
    underlineCode(underline)
    ++ invertCode(invert)
    ++ dimCode(dim)
    ++ boldCode(bold)
    ++ yellowCode
    ++ s
    ++ resetANSI;

  let blue = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) =>
    underlineCode(underline)
    ++ invertCode(invert)
    ++ dimCode(dim)
    ++ boldCode(bold)
    ++ blueCode
    ++ s
    ++ resetANSI;

  let green = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) =>
    underlineCode(underline)
    ++ invertCode(invert)
    ++ dimCode(dim)
    ++ boldCode(bold)
    ++ greenCode
    ++ s
    ++ resetANSI;

  let cyan = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) =>
    underlineCode(underline)
    ++ invertCode(invert)
    ++ dimCode(dim)
    ++ boldCode(bold)
    ++ cyanCode
    ++ s
    ++ resetANSI;

  let purple = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) =>
    underlineCode(underline)
    ++ invertCode(invert)
    ++ dimCode(dim)
    ++ boldCode(bold)
    ++ purpleCode
    ++ s
    ++ resetANSI;

  let stringSlice = (~first=0, ~last=?, str) => {
    let last =
      switch (last) {
      | Some(l) => min(l, String.length(str))
      | None => String.length(str)
      };
    if (last <= first) {
      "";
    } else {
      String.sub(str, first, last - first);
    };
  };
  let highlight =
      (
        ~underline=false,
        ~invert=false,
        ~dim=false,
        ~bold=false,
        ~color=normal,
        ~first=0,
        ~last=99999,
        str,
      ) =>
    stringSlice(~last=first, str)
    ++ color(~underline, ~dim, ~invert, ~bold, stringSlice(~first, ~last, str))
    ++ stringSlice(~first=last, str);
};

module HtmlStylish = {
  type t = string;
  let empty = "";
  let concatList = String.concat;

  let bold = s => {j|<span class="b">$s</span>|j};

  /* TODO */
  let invert = s => {j|<span>$s</span>|j};
  let dim = s => {j|<span class="gray">$s</span>|j};
  let underline = s => {j|<span class="underline">$s</span>|j};

  let makeColor = (~underline=false, ~invert=false, ~dim=false, ~bold=false, ~className=[], s) => {
    let className = ref(className);
    if (underline) {
      className := ["underline", ...className^];
    };
    if (dim) {
      className := ["gray", ...className^];
    };
    if (bold) {
      className := ["b", ...className^];
    };

    className^;
  };

  let normal = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) => {
    let className = makeColor(~underline, ~invert, ~dim, ~bold, s);
    {j|<span class="$(className)">$s</span>|j};
  };

  let red = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) => {
    let className = makeColor(~underline, ~invert, ~dim, ~bold, ~className=["red"], s);
    {j|<span class="$(className)">$s</span>|j};
  };

  let yellow = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) => {
    let className = makeColor(~underline, ~invert, ~dim, ~bold, ~className=["yellow"], s);
    {j|<span class="$(className)">$s</span>|j};
  };

  let blue = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) => {
    let className = makeColor(~underline, ~invert, ~dim, ~bold, ~className=["blue"], s);
    {j|<span class="$(className)">$s</span>|j};
  };

  let green = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) => {
    let className = makeColor(~underline, ~invert, ~dim, ~bold, ~className=["green"], s);
    {j|<span class="$(className)">$s</span>|j};
  };

  let cyan = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) => {
    let className = makeColor(~underline, ~invert, ~dim, ~bold, ~className=["cyan"], s);
    {j|<span class="$(className)">$s</span>|j};
  };

  let purple = (~underline=false, ~invert=false, ~dim=false, ~bold=false, s) => {
    let className = makeColor(~underline, ~invert, ~dim, ~bold, ~className=["purple"], s);
    {j|<span class="$(className)">$s</span>|j};
  };

  let stringSlice = (~first=0, ~last=?, str) => {
    let last =
      switch (last) {
      | Some(l) => min(l, String.length(str))
      | None => String.length(str)
      };
    if (last <= first) {
      "";
    } else {
      String.sub(str, first, last - first);
    };
  };
  let highlight =
      (
        ~underline=false,
        ~invert=false,
        ~dim=false,
        ~bold=false,
        ~color=normal,
        ~first=0,
        ~last=99999,
        str,
      ) =>
    stringSlice(~last=first, str)
    ++ color(~underline, ~dim, ~invert, ~bold, stringSlice(~first, ~last, str))
    ++ stringSlice(~first=last, str);
};
