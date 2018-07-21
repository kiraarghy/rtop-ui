open BetterErrorsTypes;
let report = Refmterr_HtmlReporter.prettyPrintParsedResult;
let content = "let a = c;";
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
|. Belt.List.reduce("", (line, acc) =>
     line |> String.trim == "" ? acc : acc ++ "\n<p>" ++ line ++ "</p>"
   )
|> Js.log;
