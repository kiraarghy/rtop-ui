let content = {|["ErrorContent",{"filePath":"_none_","cachedContent":["let a: string = 0 ;"],"range":[[0,16],[0,17]],"parsedContent":["Type_IncompatibleType",{"term":["Expression"],"extra":"","main":{"actual":["int"],"expected":["string"]},"incompats":[],"escapedScope":null}]}]|};

let noop = json => json;

let parse = str => {
  let p = Json.parseOrRaise(str);
  open Json.Decode;

  let (resultType, content) = p |> pair(string, noop);
  ();
  /* Js.log(res); */
};

content |> parse;
