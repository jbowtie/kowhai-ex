defmodule ParsersTest do
  use ExUnit.Case
  doctest Parsers
  use Parsers

  test "literal match" do
    assert Parsers.match("abc", {:literal, "a"}) == {:ok, "a", "bc"}
    assert Parsers.match("abc", {:literal, "b"}) == {:err, "expected b", "abc"}
  end

  test "regex" do
    num = {:regex, "\\d+"}
    assert Parsers.match("12a", num) == {:ok, "12", "a"}
    assert Parsers.match("12a", num, &String.to_integer/1) == {:ok, 12, "a"}
    assert Parsers.match("a12", num) == {:err, "Regex \\d+ did not match", "a12"}
    assert Parsers.match("a12", num, &String.to_integer/1) == {:err, "Regex \\d+ did not match", "a12"}
  end

  test "OR parser" do
    a = {:literal, "a"}
    b = {:literal, "b"}
    c = {:literal, "c"}
    abc = {:or, [a, b, c]}
    assert {:ok, ["c"]} == Parsers.parse("c", abc)
    assert {:err, ["expected a", "expected b", "expected c"]} == Parsers.parse("d", abc)
  end

  test "parse degenerate SEQ (one value)" do
    a = {:literal, "a"}
    x = {:seq, [a]}
    assert {:ok, ["a"]} == Parsers.parse("a", x)
    assert {:err, ["expected a"]} == Parsers.parse("c", x)
  end

  test "SEQ parser" do
    a = {:literal, "a"}
    b = {:literal, "b"}
    c = {:literal, "c"}
    x = {:seq, [a, b, c]}
    assert {:ok, [["a", "b", "c"]]} == Parsers.parse("abc", x)
  end

  test "Combined SEQ and OR parsers" do
    a = {:literal, "a"}
    b = {:literal, "b"}
    c = {:literal, "c"}
    d = {:literal, "d"}
    ab = {:or, [a, b]}
    cd = {:or, [c, d]}
    x = {:seq, [ab, cd]}
    assert {:ok, [["b", "d"]]} == Parsers.parse("bd", x)
    x = {:seq, [a, b, cd]}
    assert {:ok, [["a", "b", "c"]]} == Parsers.parse("abc", x)
  end

  test "named productions" do
    # build a grammar with named rules
    grammar =
      Map.new()
      |> rule("S2", ["b", "c"])

    # uses a named rule for the top level
    assert {:ok, [["b", "c"]]} == Parsers.parse("bc", grammar, "S2")
  end

  test "anon rule referencing named rule" do
    # build a grammar with named rules
    grammar =
      Map.new()
      |> rule("S2", ["b", "c"])

    # create an anonymous rule
    x = rule(["a", ref("S2")])
    # uses the anon rule for our top level
    assert {:ok, [["a", ["b", "c"]]]} == Parsers.parse("abc", grammar, x)
  end

  test "special start rule" do
    # build a grammar with named rules
    grammar =
      Map.new()
      |> rule("BC", ["b", "c"])
      |> rule("__START__", ref("BC"))

    # uses a named rule for the top level
    assert {:ok, [["b", "c"]]} == Parsers.parse("bc", grammar)
  end

  test "compose parsers" do
    # literal
    assert {:literal, "a"} == rule("a")
    # sequence
    assert {:seq, [{:literal, "a"}, {:literal, "b"}]} == rule(["a", "b"])
    # a OR b
    assert {:or, [{:literal, "a"}, {:literal, "b"}]} == "a" <|> "b"
    # a OR b OR c
    assert {:or, [{:literal, "a"}, {:literal, "b"}, {:literal, "c"}]} == "a" <|> "b" <|> "c"
  end

  test "left recursive rule" do
    # S ::= S a | a
    grammar =
      Map.new()
      |> rule("S", [ref("S"), "a"] <|> "a")
      |> rule("__START__", ref("S"))

    input = "aaaa"
    # IO.inspect grammar

    assert {:ok, [[[["a", "a"], "a"], "a"]]} == Parsers.parse(input, grammar)
  end

  test "right recursive rule" do
    # S ::= a S | a
    grammar =
      Map.new()
      |> rule("S", "a" <|> ["a", ref("S")])
      |> rule("__START__", ref("S"))

    input = "aaaa"
    # IO.inspect grammar

    assert {:ok, [["a", ["a", ["a", "a"]]]]} == Parsers.parse(input, grammar)
  end

  @tag :torture
  test "highly ambiguous grammar" do
    # S ::= S S S | S S | a
    grammar =
      Map.new()
      |> rule("S", [ref("S"), ref("S"), ref("S")] <|> [ref("S"), ref("S")] <|> "a")
      |> rule("__START__", ref("S"))

    input = "aaaaaaaaaaa"
    # IO.inspect grammar

    assert {:ok, [["a", "a"]]} == Parsers.parse(input, grammar)
  end
end
