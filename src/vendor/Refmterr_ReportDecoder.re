[@bs.val] external _stringify : Js.Json.t => string = "JSON.stringify";

open BetterErrorsTypes;

external error_of_json : Js.Json.t => error = "%identity";

let atomRange_of_json = json => {
  open Json.Decode;
  let pairInt = pair(int, int);
  json |> pair(pairInt, pairInt);
};

external shadyConversion : Js.Json.t => withFileInfo(error) = "%identity";

let withFileInfo_of_json = json =>
  Json.Decode.{
    filePath: json |> field("filePath", string),
    cachedContent: json |> field("cachedContent", list(string)),
    range: json |> field("range", atomRange_of_json),
    parsedContent: json |> field("parsedContent", error_of_json),
  };

let result_of_json = json =>
  Json.Decode.(
    if (Js.Array.isArray(json)) {
      let source: array(Js.Json.t) = Obj.magic(json: Js.Json.t);
      let length = Js.Array.length(source);
      let variantName = Array.unsafe_get(source, 0) |> string;
      switch (variantName, length - 1) {
      | ("Unparsable", 0) => Unparsable
      | ("ErrorContent", 1) => ErrorContent(withFileInfo_of_json(Array.unsafe_get(source, 1)))
      | _ => raise(DecodeError("Unhandle tag " ++ variantName))
      };
    } else {
      raise(DecodeError("Expected array, got " ++ _stringify(json)));
    }
  );
let parse = str => str |> Json.parseOrRaise |> result_of_json;
