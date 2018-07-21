open Jest;
open Reason_Evaluator_Test_Helper;

let parse = Refmterr_ReportDecoder.parse;
describe("parsing", () =>
  Expect.(
    test("ErrorContent", () => {
      let content = "let a: string = 0;";
      let eResult = execute(content);
      switch (eResult) {
      | Error(error) =>
        let berror = parseError(~content, ~error);
        switch (berror |. parse) {
        | ErrorContent(a) => expect(a) |> toMatchSnapshot
        | _ => raise(Invalid_argument("Wrong branch"))
        };
      | _ => raise(Invalid_argument("This code should have error"))
      };
    })
  )
);
