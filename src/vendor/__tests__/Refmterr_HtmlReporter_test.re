open Jest;
open BetterErrorsTypes;
let report = Refmterr_HtmlReporter.prettyPrintParsedResult;

describe("Error", () => {
  open Expect;

  test("Type_IncompatibleType", () => {
    let content = "let a: string = 0;";
    let expected =
      ErrorContent({
        filePath: "_none_",
        cachedContent: [content],
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

    report(~originalRevLines=[content], ~refmttypePath=None, expected)
    |> String.concat("\n")
    |> expect
    |> toMatchSnapshot;
  });

  test("Type_UnboundValue", () => {
    let content = "let a = c;";
    let expected =
      ErrorContent({
        filePath: "_none_",
        cachedContent: [content],
        range: ((0, 8), (0, 9)),
        parsedContent: Type_UnboundValue({unboundValue: "c", suggestions: None}),
      });

    report(~originalRevLines=[content], ~refmttypePath=None, expected)
    |> String.concat("\n")
    |> expect
    |> toMatchSnapshot;
  });
});
