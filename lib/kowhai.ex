defmodule Kowhai do
  alias Kowhai.Parsers

  @moduledoc """
  Documentation for Parsers.

  think about what this should look like:

  use Parsers

  rule S do
    ~ebnf/"a", S/ ^^^ parseF
  end

  Parser.parse(S, "aaa")
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
