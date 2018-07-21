[@bs.val] external _stringify : Js.Json.t => string = "JSON.stringify";

/* Decode variant helper */
let variant = (f, json) =>
  Json.Decode.(
    if (Js.Array.isArray(json)) {
      let source: array(Js.Json.t) = Obj.magic(json: Js.Json.t);
      let length = Js.Array.length(source) - 1;
      let variantName = Array.unsafe_get(source, 0) |> string;
      f(~variantName, ~length, ~source);
    } else {
      raise(DecodeError("Expected array, got " ++ _stringify(json)));
    }
  );

open BetterErrorsTypes;

external mismatchTypeArguments_of_json : Js.Json.t => mismatchTypeArguments = "%identity";
external unboundValue_of_json : Js.Json.t => unboundValue = "%identity";
external incompatibleType_of_json : Js.Json.t => incompatibleType = "%identity";

let error_of_json = json => {
  open Json.Decode;
  let f = (~variantName, ~length, ~source) =>
    switch (length) {
    | 0 when variantName == "NoErrorExtracted" => NoErrorExtracted
    | 1 =>
      let value = Array.unsafe_get(source, 1);
      switch (variantName) {
      | "Type_MismatchTypeArguments" =>
        Type_MismatchTypeArguments(value |> mismatchTypeArguments_of_json)
      | "Type_UnboundValue" => Type_UnboundValue(value |> unboundValue_of_json)
      /* | Type_SignatureItemMismatch(signatureItemMismatch)
         | Type_UnboundModule(unboundModule)
         | Type_UnboundRecordField(unboundRecordField)
         | Type_UnboundConstructor(unboundConstructor)
         | Type_UnboundTypeConstructor(unboundTypeConstructor)
         | Type_ArgumentCannotBeAppliedWithLabel(argumentCannotBeAppliedWithLabel)
         | Type_FunctionWrongLabel(functionWrongLabel)
         | Type_AppliedTooMany(appliedTooMany)
         | Type_RecordFieldNotInExpression(recordFieldNotInExpression)
         | Type_RecordFieldNotBelongPattern(recordFieldNotBelong)
         | Type_SomeRecordFieldsUndefined(string)
         | Type_RecordFieldError(recordFieldError)
         /* might be the same thing as above? jordan wrote "record expression" instead of "pattern" */
         | Type_FieldNotBelong(fieldNotBelong) */
      | "Type_IncompatibleType" => Type_IncompatibleType(value |> incompatibleType_of_json)
      /* | Type_NotAFunction(notAFunction)
         | File_SyntaxError(syntaxError)
         | Build_InconsistentAssumptions(inconsistentAssumptions)
         | File_IllegalCharacter(illegalCharacter) */
      | _ => raise(DecodeError("Unknown tag " ++ variantName))
      };
    | length => raise(DecodeError("Unexpected tag with length of " ++ string_of_int(length)))
    };
  json |> variant(f);
};

let atomRange_of_json = json => {
  open Json.Decode;
  let pairInt = pair(int, int);
  json |> pair(pairInt, pairInt);
};

let withFileInfo_of_json = json =>
  Json.Decode.{
    filePath: json |> field("filePath", string),
    cachedContent: json |> field("cachedContent", list(string)),
    range: json |> field("range", atomRange_of_json),
    parsedContent: json |> field("parsedContent", error_of_json),
  };

let result_of_json = json =>
  Json.Decode.(
    json
    |> variant((~variantName, ~length, ~source) =>
         switch (variantName, length) {
         | ("Unparsable", 0) => Unparsable
         | ("ErrorContent", 1) =>
           ErrorContent(withFileInfo_of_json(Array.unsafe_get(source, 1)))
         | _ => raise(DecodeError("Unhandle tag " ++ variantName))
         }
       )
  );

let parse = str => str |> Json.parseOrRaise |> result_of_json;
