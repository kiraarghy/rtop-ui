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

type wholeProgramExecuteResult = {
  buffer: string,
  executeResult: singleExecuteResult,
  pos: (loc, loc),
};
