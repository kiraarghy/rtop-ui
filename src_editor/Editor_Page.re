module CodeBlock = {
  [@bs.module "./CodeBlock.js"]
  external reactClass : ReasonReact.reactClass = "default";

  [@bs.deriving abstract]
  type jsProps = {
    [@bs.optional]
    firstLineNumber: int,
    value: string,
    onChange: string => unit,
    onExecute: unit => unit,
  };

  let make = (~value, ~onChange, ~firstLineNumber=?, ~onExecute, children) =>
    ReasonReact.wrapJsForReason(
      ~reactClass,
      ~props=jsProps(~value, ~onChange, ~onExecute, ~firstLineNumber?, ()),
      children,
    );
};

type bcode = {
  bc_value: string,
  bc_firstLineNumber: int,
  bc_lines: int,
};

type block =
  | B_Code(bcode)
  | B_Text(string);

type state = {blocks: array(block)};

type action =
  | UpdateBlockValue(int, string)
  | ExecuteBlock(int);

let component = ReasonReact.reducerComponent("Editor_Page");

let make = _children => {
  ...component,
  initialState: () => {
    blocks: [|
      B_Code({
        bc_value: Editor_Loremipsum.code1,
        bc_firstLineNumber: 1,
        bc_lines: 11,
      }),
      B_Code({
        bc_value: Editor_Loremipsum.code2,
        bc_firstLineNumber: 12,
        bc_lines: 19,
      }),
    |],
  },
  reducer: (action, state) =>
    switch (action) {
    | ExecuteBlock(blockIndex) =>
      switch (state.blocks |. Belt.Array.get(blockIndex)) {
      | None => ReasonReact.NoUpdate
      | Some(b) =>
        switch (b) {
        | B_Text(_) => ReasonReact.NoUpdate
        | B_Code({bc_value}) =>
          ReasonReact.SideEffects(
            (
              self =>
                Js.Promise.(
                  Editor_Worker.execute(. bc_value)
                  |> then_(result => resolve(Js.log(result)))
                  |> catch(error => resolve(Js.log(error)))
                  |> ignore
                )
            ),
          )
        }
      }

    | UpdateBlockValue(blockIndex, newValue) =>
      ReasonReact.Update({
        /* ...state, */
        blocks: {
          let nextFirstLineNumber = ref(None);
          state.blocks
          |. Belt.Array.mapWithIndexU((. i, block) =>
               switch (block) {
               | B_Code(bcode) =>
                 switch (nextFirstLineNumber^) {
                 | None =>
                   if (i != blockIndex) {
                     block;
                   } else {
                     let newValueLines =
                       newValue |> Js.String.split("\n") |> Js.Array.length;
                     (
                       if (bcode.bc_lines != newValueLines) {
                         nextFirstLineNumber := Some(newValueLines + 1);
                       }
                     )
                     |> ignore;
                     B_Code({
                       ...bcode,
                       bc_value: newValue,
                       bc_lines: newValueLines,
                     });
                   }
                 | Some(lineNum) =>
                   nextFirstLineNumber := Some(lineNum + bcode.bc_lines + 1);
                   B_Code({...bcode, bc_firstLineNumber: lineNum});
                 }
               | B_Text(_content) => failwith("not implemented")
               }
             );
        },
      })
    },
  render: ({send, state}) =>
    <div className="pageSizer">
      (
        state.blocks
        |. Belt.Array.mapWithIndexU((. index, block) =>
             switch (block) {
             | B_Code({bc_value, bc_firstLineNumber}) =>
               <div key=(index |> string_of_int) className="cell__container">
                 <div className="source-editor">
                   <CodeBlock
                     value=bc_value
                     onChange=(
                       newValue => send(UpdateBlockValue(index, newValue))
                     )
                     onExecute=(() => send(ExecuteBlock(index)))
                     firstLineNumber=bc_firstLineNumber
                   />
                 </div>
               </div>
             | B_Text(_) => failwith("not implemented")
             }
           )
        |. ReasonReact.array
      )
    </div>,
};
