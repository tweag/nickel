{
  enum = {
    Tag
      | doc m%"
          Contract to enforce the value is an enum tag.

          # Examples

          ```nickel
            (`foo | Tag) =>
              `foo
            (`FooBar | Tag) =>
              `FooBar
            ("tag" | Tag) =>
              error
          ```
          "%
      = contract.from_predicate builtin.is_enum,

    TagOrString
      | doc m%%"
            Accepts both enum tags and strings. Strings are automatically
            converted to an enum tag.

            `TagOrString` is typically used in conjunction with an enum type, to
            accept tags represented as strings (e.g. coming from a JSON
            serialization) as well.

            # Examples

            ``` nickel
            let Schema = {
              protocol
                | enum.TagOrString
                | [| `http, `ftp |],
              port
                | Number,
              method
                | enum.TagOrString
                | [| `GET, `POST |]
            } in
            let serialized =
              m%"
                {"protocol": "http", "port": 80, "method": "GET"}
              "%
              |> builtin.deserialize `Json
            in

            serialized | Schema
            ```
          "%%
      = fun label value =>
        %typeof% value
        |> match {
          `String => %enum_from_str% value,
          `Enum => value,
          _ =>
            contract.blame_with_message
              "expected either a string or an enum tag"
              label,
        },
  }
}
