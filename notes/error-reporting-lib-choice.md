PRs #96, #97 and #98 aim to improve source mapping (conversion of offsets to line and column in a source file, as done e.g. in [the rust compiler](https://github.com/rust-lang/rust/blob/master/src/librustc_span/source_map.rs)), and improve error reporting, as well as to decouple one from the other. However it requires some effort to write and maintain code for consistent, informative and pretty error messages and resolving positions from multiple sources: as @aspiwack [suggested](https://github.com/tweag/nickel/pull/96#issuecomment-656097649), it is worth to look if there isn't already a library that can achieve this for us.

## Libraries review

We initially selected 4 candidates:

- [Codespan](https://crates.io/crates/codespan) (and [codespan-reporting](https://crates.io/crates/codespan-reporting)). Inspired by codemap (below) and the rust compiler
- [Language-reporting](https://crates.io/crates/language-reporting). Fork of codespan.
- [Codemap](https://crates.io/crates/codemap) (and [codemap-diagnostic](https://crates.io/crates/codemap-diagnostic)). The original effort of extracting source mapping and diagnostic capabilities from rustc and make it a library.
- [Annotate-snippets](https://crates.io/crates/annotate-snippets). Pretty printing of annotated code snippets. Now used by rustc.

Language-reporting started as a fork of codespan, but its features have been backported and it seems to be [now deprecated](https://github.com/wycats/language-reporting/issues/9), so we exclude it from the test. All of them are based on or inspired by the rust compiler error reporting infrastructure and hence have similar models.

### Criteria

- **Sustainability**: will the library be maintained in the long term ?
- **Features**
- **Usability**
- **Modularity** and ease of integration in Nickel
- **Configurability**
- **Aesthetics**

### Sustainability

| Libraries → <br>Stats ↴    | Codespan                                                            | Codemap                                                               | Annotate-snippets                                                    |
|------------------------|---------------------------------------------------------------------|-----------------------------------------------------------------------|----------------------------------------------------------------------|
| Downloads              | 290K (~0,75K/day)                                                       | 27K (45/day)                                                          | 574K (2,5K/day)                                                      |
| Reverse deps           | 28                                                                  | 7                                                                     | 12                                                                   |
| Used by                | cargo-deny<br>CXX<br>gleam<br>gluon<br>nushell                                  | starlark (rust impl)                                                  | dhall (rust impl.)<br>rustfmt<br>rust<br>jrsonnet                             |
| Contributors           | 19                                                                  | 4                                                                     | 10                                                                   |
| Last updated           | 19 days ago                                                         | 1 year ago                                                            | 11 days ago                                                          |
| Overall sustainability | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+)High | ![#ffa500](https://via.placeholder.com/15/ffa500/000000?text=+)Medium | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+) High |

### Features

| Libraries → <br>Features ↴ | Codespan                                                            | Codemap                                                             | Annotate-snippets                                                   |
|------------------------|---------------------------------------------------------------------|---------------------------------------------------------------------|---------------------------------------------------------------------|
| Multiple source files management       | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+)Yes  | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+) Yes | ![#ff0000](https://via.placeholder.com/15/ff0000/000000?text=+) No  |
| Source mapping         | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+) Yes | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+) Yes | ![#ff0000](https://via.placeholder.com/15/ff0000/000000?text=+) No  |
| Snippet annotation     | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+) Yes | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+) Yes | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+) Yes |
| Colors                 | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+) Yes | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+) Yes | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+) Yes |

### Modularity
| Libraries  | Codespan                                                                          | Codemap                                                   | Annotate-snippets |
|------------|-----------------------------------------------------------------------------------|-----------------------------------------------------------|-------------------|
| Modularity | ![#00ff00](https://via.placeholder.com/15/00ff00/000000?text=+)Good:<br> codespan-reporting defines a file trait for source mapping that is independent from codespan | ![#ffa500](https://via.placeholder.com/15/ffa500/000000?text=+)Partial:<br> codemap-diagnostic is built on the types of codemap and depends on it | N/A               |

### Usability, configurability and aesthetics
On these aspects, the three libraries are relatively similar: both on source mapping, where a dictionary of source files allows to manage multiple sources and map absolute offsets to line and columns, and on error reporting, where the user constructs a structured representation of an annotation with labels, spans, error code, severity, and so on. Each one gives reasonable freedom in the configuration of the output format, and produces messages that resemble to the ones produced by the rust compiler.

## Conclusion
Annotate-snippets is a well established library, backed by the rust compiler itself, but it does not handle source mapping, which is also needed in Nickel. Codespan and codemap are quite similar, with respect to features as well as usage, but codespan seems more active, more used and more modular: accordingly, we recommend the usage of codespan. 

### Annex 1: code examples

#### Codespan
```rust
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

// `files::SimpleFile` and `files::SimpleFiles` help you get up and running with
// `codespan-reporting` quickly! More complicated use cases can be supported
// by creating custom implementations of the `files::Files` trait.

let mut files = SimpleFiles::new();

let file_id = files.add(
    "FizzBuzz.fun",
    unindent::unindent(
        r#"
            module FizzBuzz where

            fizz₁ : Nat → String
            fizz₁ num = case (mod num 5) (mod num 3) of
                0 0 => "FizzBuzz"
                0 _ => "Fizz"
                _ 0 => "Buzz"
                _ _ => num

            fizz₂ : Nat → String
            fizz₂ num =
                case (mod num 5) (mod num 3) of
                    0 0 => "FizzBuzz"
                    0 _ => "Fizz"
                    _ 0 => "Buzz"
                    _ _ => num
        "#,
    ),
);

// We normally recommend creating a custom diagnostic data type for your
// application, and then converting that to `codespan-reporting`'s diagnostic
// type, but for the sake of this example we construct it directly.

let diagnostic = Diagnostic::error()
    .with_message("`case` clauses have incompatible types")
    .with_code("E0308")
    .with_labels(vec![
        Label::primary(file_id, 328..331).with_message("expected `String`, found `Nat`"),
        Label::secondary(file_id, 211..331).with_message("`case` clauses have incompatible types"),
        Label::secondary(file_id, 258..268).with_message("this is found to be of type `String`"),
        Label::secondary(file_id, 284..290).with_message("this is found to be of type `String`"),
        Label::secondary(file_id, 306..312).with_message("this is found to be of type `String`"),
        Label::secondary(file_id, 186..192).with_message("expected type `String` found here"),
    ])
    .with_notes(vec![unindent::unindent(
        "
            expected type `String`
                found type `Nat`
        ",
    )]);

// We now set up the writer and configuration, and then finally render the
// diagnostic to standard error.

let writer = StandardStream::stderr(ColorChoice::Always);
let config = codespan_reporting::term::Config::default();

term::emit(&mut writer.lock(), &config, &files, &diagnostic)?;
```

#### Codemap
```rust
extern crate codemap_diagnostic;
extern crate codemap;
use codemap::{ CodeMap };
use codemap_diagnostic::{ Level, SpanLabel, SpanStyle, Diagnostic, ColorConfig, Emitter };


fn main() {
    let file = r#"
pub fn look_up_pos(&self, pos: Pos) -> Loc {
    let file = self.find_file(pos);
    let position = file.find_line_col(pos);
    Loc { file, position }
}
"#;

    let mut codemap = CodeMap::new();
    let file_span = codemap.add_file("test.rs".to_owned(), file.to_owned()).span;
    let fn_span = file_span.subspan(8, 19);
    let ret_span = file_span.subspan(40, 43);
    let var_span = file_span.subspan(54, 58);

    let sl = SpanLabel { span: fn_span, style: SpanStyle::Primary, label:Some("function name".to_owned()) };
    let sl2 = SpanLabel { span: ret_span, style: SpanStyle::Primary, label:Some("returns".to_owned()) };
    let d1 = Diagnostic { level:Level::Error, message:"Test error".to_owned(), code:Some("C000".to_owned()), spans: vec![sl, sl2] };

    let sl3 = SpanLabel { span: var_span, style: SpanStyle::Primary, label:Some("variable".to_owned()) };
    let d2 = Diagnostic { level:Level::Warning, message:"Test warning".to_owned(), code:Some("W000".to_owned()), spans: vec![sl3] };

    let d3 = Diagnostic { level: Level::Help, message:"Help message".to_owned(), code: None, spans: vec![] };

    let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(&codemap));
    emitter.emit(&[d1, d2, d3]);
}
```

#### Annotate-snippets
```rust
use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};

fn main() {
    let snippet = Snippet {
        title: Some(Annotation {
            label: Some("expected type, found `22`"),
            id: None,
            annotation_type: AnnotationType::Error,
        }),
        footer: vec![],
        slices: vec![Slice {
            source: r#"                annotations: vec![SourceAnnotation {
                label: "expected struct `annotate_snippets::snippet::Slice`, found reference"
                    ,
                range: <22, 25>,"#,
            line_start: 26,
            origin: Some("examples/footer.rs"),
            fold: true,
            annotations: vec![
                SourceAnnotation {
                    label: "",
                    annotation_type: AnnotationType::Error,
                    range: (205, 207),
                },
                SourceAnnotation {
                    label: "while parsing this struct",
                    annotation_type: AnnotationType::Info,
                    range: (34, 50),
                },
            ],
        }],
        opt: FormatOptions {
            color: true,
            ..Default::default()
        },
    };

    let dl = DisplayList::from(snippet);
    println!("{}", dl);
}
```


### Annex 2: Look & feel

#### Codespan
![Example preview](https://raw.githubusercontent.com/brendanzab/codespan/master/codespan-reporting/assets/readme_preview.svg?sanitize=true)

#### Codemap
![screenshot](https://kevinmehall.net/2017/codemap-diagnostic-screenshot.png)

#### Annotate-snippets
```
error[E0308]: mismatched types
  --> src/format.rs:52:1
   |
51 |   ) -> Option<String> {
   |        -------------- expected `Option<String>` because of return type
52 | /     for ann in annotations {
53 | |         match (ann.range.0, ann.range.1) {
54 | |             (None, None) => continue,
55 | |             (Some(start), Some(end)) if start > end_index => continue,
...  |
71 | |         }
72 | |     }
   | |_____^ expected enum `std::option::Option`, found ()
```
