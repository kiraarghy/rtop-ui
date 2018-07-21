[@bs.val] external _stringify : Js.Json.t => string = "JSON.stringify";

/* Decode variant helper */
let variant = (f, json) =>
  Json.Decode.(
    if (Js.Array.isArray(json)) {
      let source: array(Js.Json.t) = Obj.magic(json: Js.Json.t);
      let length = Js.Array.length(source) - 1;
      let variantName = Array.unsafe_get(source, 0) |> string;
      let raiseUnknownTag = () => raise(DecodeError("Unknown tag " ++ variantName));
      f(~variantName, ~length, ~source, ~raiseUnknownTag);
    } else {
      raise(DecodeError("Expected array, got " ++ _stringify(json)));
    }
  );

open BetterErrorsTypes;

let term_of_json = json => {
  let f = (~variantName, ~length, ~source as _, ~raiseUnknownTag) =>
    switch (variantName, length) {
    | ("Pattern", 0) => Pattern
    | ("Expression", 0) => Expression
    | _ => raiseUnknownTag()
    };
  json |> variant(f);
};

let incompat_of_json = json =>
  Json.Decode.{
    /* List of type equivalencies */
    actual: json |> field("actual", list(string)),
    /* List of type equivalencies */
    expected: json |> field("expected", list(string)),
  };

let incompatibleType_of_json = json =>
  Json.Decode.{
    term: json |> field("term", term_of_json),
    extra: json |> field("extra", string),
    main: json |> field("main", incompat_of_json),
    incompats: json |> field("incompats", list(incompat_of_json)),
    escapedScope: json |> optional(field("escapedScope", string)),
  };
[@deriving yojson]
type incompatibleType = {
  term,
  extra: string,
  /*
   * When the compiler points out incompatible parts. It's not clear if A or B
   * belongs to expected vs. observed. Extracted from extra.
   */
  main: incompat,
  incompats: list(incompat),
  /* Type constructor name that might escape scope. */
  escapedScope: option(string),
};

external mismatchTypeArguments_of_json : Js.Json.t => mismatchTypeArguments = "%identity";

let unboundValue_of_json = json =>
  Json.Decode.{
    suggestions: json |> optional(field("suggestions", list(string))),
    unboundValue: json |> field("unboundValue", string),
  };

let error_of_json = json => {
  open Json.Decode;
  let f = (~variantName, ~length, ~source, ~raiseUnknownTag) =>
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
      | _ => raiseUnknownTag()
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
  json
  |> variant((~variantName, ~length, ~source, ~raiseUnknownTag) =>
       switch (variantName, length) {
       | ("Unparsable", 0) => Unparsable
       | ("ErrorContent", 1) => ErrorContent(withFileInfo_of_json(Array.unsafe_get(source, 1)))
       | _ => raiseUnknownTag()
       }
     );

let parse = str => str |> Json.parseOrRaise |> result_of_json;
