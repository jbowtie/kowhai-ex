defmodule Kowhai do
  alias Kowhai.Parsers

  @moduledoc """
  Documentation for Parsers.

  think about what this should look like:

  use Parsers

  defgrammar Calc do
    "SUMexpr" ~> [~n{expr}, "+" <|> "-", ~n{expr}] <<~ fn([left, op, right])
    "expr" ~> [number, "*" <|> "/", number] <<~ fn([left, op, right])
    "__START__" ~> ~n{SUMexpr}
  end

  "1+2*3" |> Calc
  {:ok, 7}

  Parser.parse(S, "aaa")

  add a sigil that calls ref() instead of exporting ref
    that helps rule names stand out
  add a defgrammar macro that defines a grammar you can pipe IOdata into and get whatever output
  add a module that makes it easy to chain/output AST from rule
  """
  defmacro __using__(_opts) do
    quote do
      import Kowhai.Parsers,
        only: [<|>: 2, <<~: 2, rule: 3, rule: 1, ref: 1, skip: 1, optional: 1, many: 1]
    end
  end

  def parse(input, grammar) when is_map(grammar) do
    rule = Map.fetch!(grammar, "__START__")
    Parsers.parse(input, grammar, rule)
  end

  def parse(input, rule) do
    # no named rules
    Parsers.parse(input, %{}, rule)
  end

  # use named start rule in grammar
  def parse(input, grammar, ref) when is_binary(ref) do
    rule = Map.fetch!(grammar, ref)
    Parsers.parse(input, grammar, rule)
  end

  def parse(input, grammar, rule) do
    Parsers.parse(input, grammar, rule)
  end
end
