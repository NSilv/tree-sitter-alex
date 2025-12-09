/**
 * @file Alex parser
 * @author Nicholas <n.silvestrin98@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "alex",
  extras: _ => ["\r"],
  rules: {
    // TODO: add the actual grammar rules
    document: $ =>
      repeat(choice($.haskell_block, $.line)),
    haskell_block: $ =>
      seq(
        $.open_block,
        "\n",
        optional($.code),
        $.close_block,
        optional("\n")
      ),
    open_block: _ => "{",
    close_block: _ => "}",
    code: _ => repeat1(seq(/[^\n]*/, "\n")),
    line: $ => prec.right(repeat1(seq(/[^\n]*/, "\n")))
  }
});
