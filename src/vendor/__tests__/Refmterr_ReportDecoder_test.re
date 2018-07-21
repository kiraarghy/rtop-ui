open Jest;
open Reason_Evaluator_Test_Helper;
open BetterErrorsTypes;

[@bs.val] external _stringify : Js.Json.t => string = "JSON.stringify";

let parse = Refmterr_ReportDecoder.parse;

let matcher = (~content, ~expected, ~print=false, ()) => {
  let eResult = execute(content);
  switch (eResult) {
  | Error(error) =>
    let berror = parseError(~content, ~error);
    if (print) {
      Js.log(berror |> Json.parseOrRaise |. Js.Json.stringifyWithSpace(2));
    };
    Expect.(expect(berror |. parse) |> toEqual(expected));
  | _ => raise(Invalid_argument("This code should have error"))
  };
};

describe("Error", () =>
  Expect.(
    test("Type_IncompatibleType", () => {
      let content = "let a: string = 0;";
      let expected =
        ErrorContent({
          filePath: "_none_",
          cachedContent: ["let a: string = 0;"],
          range: ((0, 16), (0, 17)),
          parsedContent:
            Type_IncompatibleType({
              term: Expression,
              extra: "",
              main: {
                actual: ["int"],
                expected: ["string"],
              },
              incompats: [],
              escapedScope: None,
            }),
        });

      matcher(~content, ~expected, ());
    })
  )
);
