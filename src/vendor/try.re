open BetterErrorsTypes;
let report = Refmterr_HtmlReporter.prettyPrintParsedResult;
let content = "let a = c;";
let expected =
  ErrorContent({
    filePath: "_none_",
    cachedContent: [content],
    range: ((0, 8), (0, 9)),
    parsedContent: Type_UnboundValue({unboundValue: "c", suggestions: None}),
  });

report(~originalRevLines=[content], ~refmttypePath=None, expected)
|> List.rev
|> String.concat("\n")
|> Js.log;
