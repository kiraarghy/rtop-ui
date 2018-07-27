type singleExecuteResult = {
  evaluate: option(string),
  stderr: option(string),
  stdout: option(string),
};

type loc = {
  line: int,
  col: int,
  offset: int,
};
let loc_of_string = ({line, col}) => {j|{line: $(line), ch: $(col)}|j};
let pos_of_string = ((from, to_)) =>
  "editor.getRange("
  ++ loc_of_string(from)
  ++ ","
  ++ loc_of_string(to_)
  ++ ")";

type wholeProgramExecuteResult = {
  buffer: string,
  executeResult: singleExecuteResult,
  pos: (loc, loc),
};
