import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { execFileSync } from "node:child_process";

/** Parse Dune S-expressions without interpreting strings or comments as syntax. */
export function parseDune(text) {
  let offset = 0;
  function whitespace() {
    while (offset < text.length) {
      if (/\s/.test(text[offset])) offset++;
      else if (text[offset] === ";") {
        while (offset < text.length && text[offset] !== "\n") offset++;
      } else if (text.startsWith("#;", offset)) {
        offset += 2;
        expression();
      } else break;
    }
  }
  function expression() {
    whitespace();
    assert.ok(offset < text.length, "Truncated Dune expression");
    if (text[offset] === "(") {
      offset++;
      const values = [];
      whitespace();
      while (text[offset] !== ")") {
        values.push(expression());
        whitespace();
      }
      offset++;
      return values;
    }
    assert.notEqual(text[offset], ")", "Unexpected Dune closing parenthesis");
    if (text[offset] === '"') {
      offset++;
      let value = "",
        closed = false;
      while (offset < text.length) {
        const character = text[offset++];
        if (character === '"') {
          closed = true;
          break;
        }
        if (character === "\\") {
          assert.ok(offset < text.length, "Truncated Dune string escape");
          const escaped = text[offset++];
          const escapes = {
            "\\": "\\",
            '"': '"',
            n: "\n",
            r: "\r",
            t: "\t",
            b: "\b",
          };
          if (Object.hasOwn(escapes, escaped)) value += escapes[escaped];
          else if (/\d/.test(escaped)) {
            const digits = escaped + text.slice(offset, offset + 2);
            assert.match(digits, /^\d{3}$/, "Invalid numeric Dune escape");
            assert.ok(Number(digits) <= 255, "Out-of-range Dune escape");
            value += String.fromCharCode(Number(digits));
            offset += 2;
          } else if (escaped === "x") {
            const digits = text.slice(offset, offset + 2);
            assert.match(
              digits,
              /^[a-fA-F0-9]{2}$/,
              "Invalid hexadecimal Dune escape",
            );
            value += String.fromCharCode(parseInt(digits, 16));
            offset += 2;
          } else if (escaped === "\n") {
            while (text[offset] === " " || text[offset] === "\t") offset++;
          } else
            assert.fail(
              "Unsupported Dune string escape; review parser coverage",
            );
        } else value += character;
      }
      assert.ok(closed, "Unterminated Dune string");
      return value;
    }
    const start = offset;
    while (offset < text.length && !/[\s();]/.test(text[offset])) offset++;
    assert.ok(offset > start, "Invalid Dune atom");
    return text.slice(start, offset);
  }
  const values = [];
  whitespace();
  while (offset < text.length) {
    values.push(expression());
    whitespace();
  }
  return values;
}

const restricted = new Set([
  "include",
  "dynamic_include",
  "subdir",
  "dirs",
  "data_only_dirs",
  "ignored_subdirs",
]);
const supportedStanzas = new Set([
  "alias",
  "copy_files",
  "copy_files#",
  "env",
  "executable",
  "executables",
  "include_subdirs",
  "install",
  "library",
  "menhir",
  "ocamllex",
  "ocamlyacc",
  "rule",
  "test",
  "tests",
  "cram",
]);
function field(stanza, name) {
  return stanza.find((value) => Array.isArray(value) && value[0] === name);
}
function ruleAliases(stanza) {
  const singular = field(stanza, "alias"),
    plural = field(stanza, "aliases");
  assert.ok(
    !(singular && plural),
    "Rule alias and aliases are mutually exclusive",
  );
  return singular ? singular.slice(1) : plural ? plural.slice(1) : [];
}
function references(value, output) {
  if (!Array.isArray(value)) return;
  if (["alias", "alias_rec", "include", "dynamic_include"].includes(value[0])) {
    output.push(value);
    return;
  }
  for (const child of value) references(child, output);
}

function checkNormalClosure(stanzas, filename) {
  const graph = new Map();
  function add(name, values) {
    assert.ok(
      typeof name === "string" && name && !name.includes("%{"),
      `${filename}: invalid declared alias`,
    );
    const edges = graph.get(name) ?? [];
    for (const value of values) references(value, edges);
    graph.set(name, edges);
  }
  for (const stanza of stanzas) {
    if (stanza[0] === "alias")
      add(
        field(stanza, "name")?.[1],
        stanza.slice(1).filter((value) => value[0] !== "name"),
      );
    if (stanza[0] === "rule")
      for (const name of ruleAliases(stanza))
        add(
          name,
          stanza
            .slice(1)
            .filter((value) => !["alias", "aliases"].includes(value[0])),
        );
    if (stanza[0] === "library" && field(stanza, "inline_tests")) {
      const name = field(stanza, "name")?.[1];
      add("runtest", [["alias", `runtest-${name}`]]);
      add(`runtest-${name}`, [field(stanza, "inline_tests")]);
    }
    if (["test", "tests", "cram"].includes(stanza[0]))
      add("runtest", stanza.slice(1));
  }
  const visited = new Set(),
    active = new Set();
  function visit(name) {
    assert.ok(!active.has(name), `${filename}: cyclic normal alias closure`);
    if (visited.has(name)) return;
    active.add(name);
    for (const reference of graph.get(name) ?? []) {
      assert.ok(
        !["include", "dynamic_include"].includes(reference[0]),
        `${filename}: unsupported included normal dependency`,
      );
      assert.equal(
        reference[0],
        "alias",
        `${filename}: recursive normal alias dependency`,
      );
      const target = reference[1];
      assert.ok(
        reference.length === 2 &&
          typeof target === "string" &&
          /^[A-Za-z0-9_.-]+$/.test(target) &&
          target !== "." &&
          target !== "..",
        `${filename}: cross-directory or dynamic normal alias dependency`,
      );
      visit(target);
    }
    active.delete(name);
    visited.add(name);
  }
  visit("runtest");
}

function hasNormalAlias(stanzas) {
  return stanzas.some(
    (stanza) =>
      (stanza[0] === "library" && field(stanza, "inline_tests")) ||
      ["test", "tests", "cram"].includes(stanza[0]) ||
      (stanza[0] === "rule" && ruleAliases(stanza).includes("runtest")) ||
      (stanza[0] === "alias" && field(stanza, "name")?.[1] === "runtest"),
  );
}

/** Inventory normal direct alias owners, while auditing every tracked Dune file.
 * @param {{path: string, text: string}[]} files */
export function inventoryFromFiles(files) {
  assert.ok(Array.isArray(files), "Invalid Dune source inventory");
  const directories = new Set(),
    owners = new Set();
  let rootSeen = false;
  for (const file of files) {
    assert.ok(
      typeof file.path === "string" &&
        /^(?:[A-Za-z0-9_.-]+\/)*dune$/.test(file.path) &&
        file.path.split("/").every((part) => part !== "." && part !== ".."),
      "Unsafe Dune source path",
    );
    if (/^(website|scratch)\//.test(file.path)) continue;
    const owner = path.posix.dirname(file.path);
    assert.ok(!directories.has(owner), "Duplicate Dune source owner");
    directories.add(owner);
    const stanzas = parseDune(file.text);
    for (const stanza of stanzas) {
      assert.ok(
        Array.isArray(stanza) && typeof stanza[0] === "string",
        `${file.path}: invalid Dune stanza`,
      );
      if (owner === "." && stanza[0] === "dirs") {
        assert.deepEqual(
          stanza,
          ["dirs", ":standard", "\\", "website", "scratch"],
          "Root Dune traversal changed; review normal coverage",
        );
        assert.ok(!rootSeen, "Duplicate root Dune traversal");
        rootSeen = true;
      } else {
        assert.ok(
          !restricted.has(stanza[0]),
          `${file.path}: unsupported source traversal ${stanza[0]}`,
        );
        assert.ok(
          supportedStanzas.has(stanza[0]),
          `${file.path}: unknown stanza ${stanza[0]}; review normal coverage`,
        );
      }
    }
    checkNormalClosure(stanzas, file.path);
    if (hasNormalAlias(stanzas)) owners.add(owner);
  }
  assert.ok(rootSeen, "Missing authoritative root Dune traversal");
  return [...owners].sort().map((id) => ({ id, alias: `@@${id}/runtest` }));
}

/** Discover only regular tracked source files; never follow repository symlinks. */
export function discoverInventory(root = ".") {
  const tracked = execFileSync("git", ["ls-files", "-z"], {
    cwd: root,
    encoding: "utf8",
  })
    .split("\0")
    .filter(Boolean);
  const names = tracked.filter(
    (name) => name === "dune" || name.endsWith("/dune"),
  );
  const files = names.map((name) => {
    const components = name.split("/");
    for (let count = 1; count <= components.length; count++) {
      const stat = fs.lstatSync(path.join(root, ...components.slice(0, count)));
      assert.ok(
        !stat.isSymbolicLink() &&
          (count === components.length ? stat.isFile() : stat.isDirectory()),
        `Nonregular Dune source path: ${name}`,
      );
    }
    return { path: name, text: fs.readFileSync(path.join(root, name), "utf8") };
  });
  const inventory = inventoryFromFiles(files);
  const owners = new Set(inventory.map(({ id }) => id));
  for (const name of tracked) {
    if (/^(website|scratch)\//.test(name)) continue;
    // Cram files can introduce runtest aliases without a Dune file in their own
    // directory. Require an explicit normal owner so the plan cannot omit them.
    if (name.endsWith(".t"))
      assert.ok(
        owners.has(path.posix.dirname(name)),
        `Cram directory has no inventoried Dune owner: ${name}`,
      );
  }
  return inventory;
}
