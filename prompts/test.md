<config model="gpt-4.5"  max_tokens="8000" />

<msg role="developer">
    You are a helpful ai assistant
</msg>
<msg role="user">
summarize the following:
<agent src="/Users/dakotamurphy/chatgpt/prompts/output_file_test.md" local />
</msg>

<msg role="assistant">
	<raw>
		The file you provided is the interface documentation (`markup.mli`) for the Markup.ml library, a robust OCaml library for parsing and writing streaming HTML and XML with error recovery capabilities. It includes the following key features and concepts:
		
		### Main Features:
		- **Error-Recovering Parsing:** Can recover gracefully from malformed input or, alternatively, report strictly all parsing errors.
		- **Streaming & Lazy:** Processes input incrementally and only as needed through streaming parsers and writers.
		- **Automatic Encoding Detection:** Recognizes the character encoding automatically.
		- **Both HTML and XML Support:** Conforms to relevant HTML5 and XML specifications for parsing and serialization.
		- **Document & Fragment Parsing:** Supports parsing full documents and fragments of HTML/XML.
		- **Signal-based (SAX-style) Parsing:** Generates parsing signals (`Start_element`, `End_element`, `Text`, etc.) rather than a full-tree initially; provides utilities to convert these signals into DOM-style trees.
		
		### Core API Overview:
		The API revolves around a few key functions, mainly:
		- `parse_html` / `write_html`
		- `parse_xml` / `write_xml`
		
		These functions transform streams of characters into signal streams (and vice versa).
		
		### Utility Functions and Streams:
		- Provides utility functions like `to_string`, `to_buffer`, `to_channel`, and `to_file` for byte streams.
		- Support functions for various operations and transformations on streams, including `map`, `filter`, `next`, `peek`, `fold`, and others.
		
		### Key data types introduced:
		- **Streams:** Defined with phantom types (`'a`, `'sync` or `'async`) to enforce correct synchronous or asynchronous usage via the type system.
		- **Signals:** Represent the parser output in a structured way, allowing easy conversion to DOM-style trees.
		- **Errors:** Elaborate and descriptive types for different kinds of parsing errors, each convertible to human-readable strings via `to_string`.
		
		### Advanced features:
		- **Namespaces and Encodings Handling:** Explicit handling and reporting for namespace conflicts and encoding problems.
		- **Tree Conversion Utility:** Functions like `tree` and `trees` allow convenient and customizable conversion from signal streams into user-defined data structures (trees or DOM-like structures).
		- **Async Support Module:** Markup.ml provides modules (`Markup_lwt` and `Markup_lwt_unix`) to support asynchronous/non-blocking use cases (e.g. with Lwt).
		
		### Conformance and Limitations:
		The documentation explicitly states the current conformance status against HTML5, XML, and Namespaces specifications, highlighting deviations mainly concerning certain error recoveries or non-local adjustments. Some deviations are marked to be corrected in future versions, whereas others are intentional for simplicity and performance reasons.
		
		### Provided examples:
		Examples are included, demonstrating typical use-cases such as correcting and pretty-printing HTML input, reporting XML parsing errors explicitly, and building custom data structures from parsed HTML.
		
		Overall, Markup.ml provides a complete and flexible OCaml-based solution for resilient textual parsing of HTML/XML streams, offering rich facilities for error handling, stream manipulation, and DOM-tree conversion.
	</raw>
</msg>

<msg role="user">

</msg>