defmodule ParsersTest do
  use ExUnit.Case
  doctest Kowhai.Parsers
  use Kowhai.Parsers
  alias Kowhai.Parsers

  test "literal match" do
    assert Parsers.match("abc", {:literal, "a"}) == {:ok, "a", "bc"}
    assert Parsers.match("abc", {:literal, "b"}) == {:err, "expected b", "abc"}
  end

  test "regex" do
    num = {:regex, "\\d+"}
    assert Parsers.match("12a", num) == {:ok, "12", "a"}
    assert Parsers.match("12a", num, &String.to_integer/1) == {:ok, 12, "a"}
    assert Parsers.match("a12", num) == {:err, "Regex \\d+ did not match", "a12"}

    assert Parsers.match("a12", num, &String.to_integer/1) ==
             {:err, "Regex \\d+ did not match", "a12"}
  end

  test "literal eval" do
    a = "1" <<~ &String.to_integer/1
    assert {:literal, "1", &String.to_integer/1} == a
    assert {:ok, 1} == Parsers.parse("1", a)
  end

  test "regex eval" do
    a = ~r/\d+/ <<~ &String.to_integer/1
    assert {:regex, "\\d+", &String.to_integer/1} == a
    assert {:ok, 123} == Parsers.parse("123", a)
  end

  test "OR parser" do
    a = {:literal, "a"}
    b = {:literal, "b"}
    c = {:literal, "c"}
    abc = {:or, [a, b, c]}
    # check expansion of custom grammar
    assert "a" <|> "b" <|> "c" == abc
    assert {:ok, "c"} == Parsers.parse("c", abc)
    assert {:err, ["expected a", "expected b", "expected c"]} == Parsers.parse("d", abc)
  end

  test "parse degenerate SEQ (one value)" do
    a = {:literal, "a"}
    x = {:seq, [a]}
    assert {:ok, "a"} == Parsers.parse("a", x)
    assert {:err, ["expected a"]} == Parsers.parse("c", x)
  end

  test "SEQ parser" do
    a = {:literal, "a"}
    b = {:literal, "b"}
    c = {:literal, "c"}
    x = {:seq, [a, b, c]}
    assert {:ok, ["a", "b", "c"]} == Parsers.parse("abc", x)
  end

  test "drop ignored productions" do
    a = {:literal, "a"}
    b = {:literal, "b"} <<~ fn _ -> :ignore end
    c = {:literal, "c"}
    x = {:seq, [a, b, c]}
    assert {:ok, ["a", "c"]} == Parsers.parse("abc", x)
    whitespace = skip(~r/\s+/)
    # skip middle
    x = {:seq, [a, whitespace, c]}
    assert {:ok, ["a", "c"]} == Parsers.parse("a   c", x)
    # skip left
    x = {:seq, [whitespace, c]}
    assert {:ok, ["c"]} == Parsers.parse("   c", x)
    # skip right
    x = {:seq, [a, whitespace]}
    assert {:ok, ["a"]} == Parsers.parse("a   ", x)
    # skip all
    x = {:seq, [skip(a), skip(c)]}
    assert {:ok, []} == Parsers.parse("ac", x)
  end

  test "optional productions" do
    a = {:literal, "a"}
    b = optional("b")
    c = {:literal, "c"}
    x = {:seq, [a, b, c]}
    assert {:ok, ["a", "b", "c"]} == Parsers.parse("abc", x)
    assert {:ok, ["a", "c"]} == Parsers.parse("ac", x)
  end

  test "Combined SEQ and OR parsers" do
    a = {:literal, "a"}
    b = {:literal, "b"}
    c = {:literal, "c"}
    d = {:literal, "d"}
    ab = {:or, [a, b]}
    cd = {:or, [c, d]}
    x = {:seq, [ab, cd]}
    assert {:ok, ["b", "d"]} == Parsers.parse("bd", x)
    x = {:seq, [a, b, cd]}
    assert {:ok, ["a", "b", "c"]} == Parsers.parse("abc", x)
  end

  test "named productions" do
    # build a grammar with named rules
    grammar =
      Map.new()
      |> rule("S2", ["b", "c"])

    # uses a named rule for the top level
    assert {:ok, ["b", "c"]} == Parsers.parse("bc", grammar, "S2")
  end

  test "anon rule referencing named rule" do
    # build a grammar with named rules
    grammar =
      Map.new()
      |> rule("S2", ["b", "c"])

    # create an anonymous rule
    x = rule(["a", ref("S2")])
    # uses the anon rule for our top level
    assert {:ok, ["a", ["b", "c"]]} == Parsers.parse("abc", grammar, x)
  end

  test "special start rule" do
    # build a grammar with named rules
    grammar =
      Map.new()
      |> rule("BC", ["b", "c"])
      |> rule("__START__", ref("BC"))

    # uses a named rule for the top level
    assert {:ok, ["b", "c"]} == Parsers.parse("bc", grammar)
  end

  test "compose parsers" do
    # literal
    assert {:literal, "a"} == rule("a")
    # regex
    assert {:regex, "a"} == rule(~r/a/)
    # rule
    assert {:rule, "a"} == ref("a")
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

    assert {:ok, [[["a", "a"], "a"], "a"]} == Parsers.parse(input, grammar)
  end

  test "right recursive rule" do
    # S ::= a S | a
    grammar =
      Map.new()
      |> rule("S", "a" <|> ["a", ref("S")])
      |> rule("__START__", ref("S"))

    input = "aaaa"
    # IO.inspect grammar

    assert {:ok, ["a", ["a", ["a", "a"]]]} == Parsers.parse(input, grammar)
  end

  test "one-or-more productions" do
    a = {:literal, "a"}
    b = {:many, {:literal, "b"}}
    x = {:seq, [a, b]}
    # check that many expands to same struct
    assert b == many("b")
    assert {:ok, ["a", "b"]} == Parsers.parse("ab", x)
    assert {:ok, ["a", ["b", "b"]]} == Parsers.parse("abb", x)
    assert {:ok, ["a", ["b", "b", "b"]]} == Parsers.parse("abbb", x)
    assert {:err, ["expected b"]} == Parsers.parse("a", x)
  end

  test "zero-or-more productions" do
    a = {:literal, "a"}
    b = "b" |> many |> optional
    x = {:seq, [a, b]}
    assert {:ok, ["a", "b"]} == Parsers.parse("ab", x)
    assert {:ok, ["a", ["b", "b", "b"]]} == Parsers.parse("abbb", x)
    assert {:ok, ["a"]} == Parsers.parse("a", x)
  end

  test "simple calculator grammar" do
    # sumexpr := expr +|- expr
    # expr := (number *|/ number) | number
    grammar =
      Map.new()
      |> rule("Number", ~r/\d+/ <<~ &String.to_integer/1)
      |> rule(
        "sumexpr",
        [ref("expr"), "+" <|> "-", ref("expr")]
        <<~ fn [x, op, y] ->
          case op do
            "+" -> x + y
            "-" -> x - y
          end
        end
      )
      |> rule(
        "expr",
        [ref("Number"), "*" <|> "/", ref("Number")]
        <<~ fn [x, op, y] ->
          case op do
            "*" -> x * y
            "/" -> x / y
          end
        end
        <|> ref("Number")
      )
      |> rule("__START__", ref("sumexpr"))

    input = "2+3"
    {:ok, output} = Parsers.parse(input, grammar)
    assert 5 == output

    input2 = "2+3*4"
    {:ok, output2} = Parsers.parse(input2, grammar)
    assert 14 == output2
  end

  test "ambiguous rule" do
    # S ::= 2 | \d+
    grammar =
      Map.new()
      |> rule("S1", "2")
      |> rule("S2", ~r/\d/ <<~ &String.to_integer/1)
      |> rule("S", ref("S1") <|> ref("S2"))
      |> rule("__START__", ref("S"))

    input = "2"

    assert {:ambiguous, [2, "2"]} == Parsers.parse(input, grammar)
  end

  test "ambiguous grammar" do
    # S ::= S S a | S a | a
    grammar =
      Map.new()
      |> rule("S", [ref("S"), ref("S"), "a"] <|> [ref("S"), "a"] <|> "a")
      |> rule("__START__", ref("S"))

    input = "aaaaa"

    assert {:ok, [[[["a", "a"], "a"], "a"], "a"]} == Parsers.parse(input, grammar)
  end

  @tag :torture
  test "highly ambiguous grammar" do
    # this *should* work (see sigma-2 in GLL paper(
    # resolves as incomplete (instead of infinite loop)
    # which is only half-reasonable
    # S ::= S S S | S S | a
    grammar =
      Map.new()
      |> rule("S", [ref("S"), ref("S"), ref("S")] <|> [ref("S"), ref("S")] <|> "a")
      |> rule("__START__", ref("S"))

    input = "aaaaa"
    # IO.inspect grammar

    assert {:ok, [[[["a", "a"], "a"], "a"], "a"]} == Parsers.parse(input, grammar)
  end
end
