defmodule Kowhai.Parsers do

  # match nil / epsilon rule
  defp matchLiteral(input, nil) do
    {:ok, :ignore, input}
  end

  # match a literal string
  defp matchLiteral(input, expected) do
    if String.starts_with?(input, expected) do
      tail = String.slice(input, String.length(expected)..-1)
      {:ok, expected, tail}
    else
      {:err, "expected #{expected}", input}
    end
  end

  defp matchRegex(input, expected) do
    r = Regex.compile!("^#{expected}")

    case Regex.run(r, input, return: :index) do
      nil -> {:err, "Regex #{expected} did not match", input}
      [{0, len}] -> {:ok, String.slice(input, 0..(len - 1)), String.slice(input, len..-1)}
    end
  end

  def match(input, {:regex, expected}) do
    matchRegex(input, expected)
  end

  def match(input, {:literal, expected}) do
    matchLiteral(input, expected)
  end

  def match(input, {op, rule, exec}) do
    case match(input, {op, rule}) do
      {:ok, result, tail} -> {:ok, exec.(result), tail}
      {:err, msg, loc} -> {:err, msg, loc}
    end
  end

  def match(input, rule, exec) do
    case match(input, rule) do
      {:ok, result, tail} -> {:ok, exec.(result), tail}
      {:err, msg, loc} -> {:err, msg, loc}
    end
  end

  defp chain(gll, input, {op, rule, exec}, cont) do
    chain(gll, input, {op, rule}, fn x ->
      res =
        case x do
          {:ok, result, tail} -> {:ok, exec.(result), tail}
          {:err, msg, loc} -> {:err, msg, loc}
        end

      cont.(res)
    end)
  end

  # each of these should take a function called [cont]
  defp chain(gll, input, {:or, expected}, cont) do
    # keep list of results
    {:ok, results} = Agent.start(fn -> MapSet.new() end)
    # for each p in expected:
    for p <- expected do
      # if result not in list, call cont(result), append result to list
      add(gll, p, input, fn x ->
        res = Agent.get(results, & &1)

        if !MapSet.member?(res, x) do
          cont.(x)
          Agent.update(results, &MapSet.put(&1, x))
        end
      end)
    end
  end

  # handle (presumably degenerate) sequence of single value
  defp chain(gll, input, {:seq, [left]}, cont) do
    chain(gll, input, left, fn x -> cont.(x) end)
  end

  # handle sequence of two values
  defp chain(gll, input, {:seq, [left, right]}, cont) do
    chain(gll, input, left, fn x ->
      case x do
        {:ok, :ignore, remainder} ->
          chain(gll, remainder, right, fn y ->
            case y do
              {:ok, :ignore, rt} -> cont.({:ok, [], rt})
              {:ok, v2, tail} -> cont.({:ok, [v2], tail})
              _ -> cont.(y)
            end
          end)

        {:ok, v1, remainder} ->
          chain(gll, remainder, right, fn y ->
            case y do
              {:ok, :ignore, rt} -> cont.({:ok, [v1], rt})
              {:ok, v2, tail} -> cont.({:ok, [v1, v2], tail})
              _ -> cont.(y)
            end
          end)

        _ ->
          cont.(x)
      end
    end)
  end

  # handle sequence of 3 or more values
  defp chain(gll, input, {:seq, [head | tail]}, cont) do
    # TODO: should be able to rewrite as reduce_while
    chain(gll, input, head, fn x ->
      case x do
        {:ok, :ignore, remainder} ->
          chain(gll, remainder, {:seq, tail}, fn y ->
            case y do
              {:ok, :ignore, rt} -> cont.({:ok, [], rt})
              {:ok, v2, rt} -> cont.({:ok, [v2], rt})
              _ -> cont.(y)
            end
          end)

        {:ok, v1, remainder} ->
          chain(gll, remainder, {:seq, tail}, fn y ->
            case y do
              {:ok, :ignore, rt} -> cont.({:ok, [v1 | []], rt})
              {:ok, v2, rt} -> cont.({:ok, [v1 | v2], rt})
              _ -> cont.(y)
            end
          end)

        _ ->
          cont.(x)
      end
    end)
  end

  # named non-terminals allow recursive definitions
  defp chain(gll, input, {:many, rule}, cont) do
    chain(gll, input, rule, fn x ->
      case x do
        {:ok, v1, remainder} ->
          chain(gll, remainder, {:many, rule}, fn y ->
            case y do
              {:ok, v2, rt} when is_list(v2) -> cont.({:ok, [v1 | v2], rt})
              {:ok, v2, rt} -> cont.({:ok, [v1, v2], rt})
              _ -> cont.({:ok, v1, remainder})
            end
          end)

        _ ->
          cont.(x)
      end
    end)
  end

  # named non-terminals allow recursive definitions
  defp chain(gll, input, {:rule, ref}, cont) do
    s = Agent.get(gll, & &1)

    case Map.fetch(s.rules, ref) do
      {:ok, rule} -> chain(gll, input, rule, fn x -> cont.(x) end)
      :error -> cont.({:err, "Named rule #{ref} does not exist in grammar", input})
    end
  end

  # for terminal types (literal, regex) we can simply forward to match
  defp chain(_t, input, rule, cont) do
    cont.(match(input, rule))
  end

  # here's the grammar and start rule
  def parse(input, grammar, rule) do
    {:ok, gll} =
      Agent.start(fn ->
        %{stack: [], backlinks: %{}, done: MapSet.new(), popped: %{}, rules: grammar, saved: %{}}
      end)

    {:ok, success} = Agent.start(fn -> MapSet.new() end)
    {:ok, failure} = Agent.start(fn -> MapSet.new() end)

    chain(gll, input, rule, fn x ->
      case x do
        {:ok, result, ""} -> Agent.update(success, &MapSet.put(&1, result))
        {:err, msg, _} -> Agent.update(failure, &MapSet.put(&1, msg))
        _ -> Agent.update(failure, &MapSet.put(&1, "INCOMPLETE: #{inspect(x)}"))
      end
    end)

    pop_stack(gll)
    s = Agent.get(success, & &1)

    case MapSet.size(s) do
      1 ->
        # only one success
        {:ok, MapSet.to_list(s) |> Enum.at(0)}

      0 ->
        # no successes, return all the errors
        # assume client will choose which to display
        e = Agent.get(failure, & &1)
        {:err, MapSet.to_list(e)}

      _ ->
        # ambiguous, return all so client can choose one
        {:ambiguous, MapSet.to_list(s)}
    end
  end

  defp pop_stack(gll) do
    state = Agent.get(gll, & &1)

    if length(state.stack) > 0 do
      [{parser, input} | tail] = state.stack
      # pop the stack
      news = %{state | stack: tail}
      Agent.update(gll, fn _ -> news end)

      # chain the item
      chain(gll, input, parser, fn x ->
        s = Agent.get(gll, & &1)

        # append x to s.popped when x is OK
        case x do
          {:ok, _, _} ->
            results = [x | Map.get(s.popped, {parser, input}, [])]
            s2 = %{s | popped: Map.put(s.popped, {parser, input}, results)}
            Agent.update(gll, fn _ -> s2 end)

          _ ->
            s
        end

        # IO.puts ("BL: #{inspect s.backlinks}\nSAVED: #{inspect s.saved}")
        # get backlinks (if any)
        # skip any already saved for this result
        # execute and save for this result
        Map.get(s.backlinks, {parser, input}, [])
        |> Enum.reject(fn cb ->
          bls = Agent.get(gll, & &1)
          saved = Map.get(bls.saved, x, [])
          isM = Enum.member?(saved, cb)
          # IO.puts "MATCHING #{inspect saved} TO #{inspect cb} FOR #{inspect x}"
          # if isM, do: IO.puts "MEMBER FOUND for #{inspect x}"
          isM
        end)
        |> Enum.each(fn cb ->
          # IO.puts "EXEC #{inspect cb} FOR #{inspect x}"
          Agent.update(gll, fn gs ->
            %{gs | saved: Map.put(gs.saved, x, [cb | Map.get(gs.saved, x, [])])}
          end)

          cb.(x)
        end)
      end)

      pop_stack(gll)
    end
  end

  defp add(gll, parser, input, cb) do
    state = Agent.get(gll, & &1)
    key = {parser, input}
    # backlinks[{input, parser}] append cont
    links = [cb | Map.get(state.backlinks, key, [])]
    state = %{state | backlinks: Map.put(state.backlinks, key, links)}
    # if results = popped[{parser, input}], cont(r) for r in results, return
    ress = Map.get(state.popped, key, [])

    news =
      if length(ress) > 0 do
        Enum.each(ress, fn x -> cb.(x) end)
        state
      else
        # push {parser, input} onto stack unless in done
        #   prevents cycles when eval non-terminals
        if MapSet.member?(state.done, key) do
          state
        else
          %{state | stack: [key | state.stack], done: MapSet.put(state.done, key)}
        end
      end

    Agent.update(gll, fn _ -> news end)
  end

  # everything after here is syntactic sugar for building up grammars
  def rule(grammar, name, body) do
    grammar
    |> Map.put_new(name, build(body))
  end

  def rule(body) do
    build(body)
  end

  def ref(name) do
    {:rule, name}
  end

  # extend existing disjunction parsers
  def {:or, list} <|> y do
    # more efficient to use [head|tail] but less intuitive
    # actual order doesn't matter but tests need to reflect that!
    {:or, list ++ [build(y)]}
  end

  def x <|> y do
    {:or, [build(x), build(y)]}
  end

  # parser <<~ eval function
  def x <<~ y do
    build(x) |> add_eval(y)
  end

  def skip(x) do
    x <<~ fn _ -> :ignore end
  end

  # or with the nil rule
  def optional(x) do
    build(x) <|> {:literal, nil}
  end

  def many(x) do
    {:many, build(x)}
  end

  # list becomes seq parser
  defp build(list) when is_list(list) do
    parsers =
      list
      |> Enum.map(&build/1)

    {:seq, parsers}
  end

  # handles case where already expanded
  defp build({op, rule, eval}) do
    {op, rule, eval}
  end

  defp build({op, rule}) do
    {op, rule}
  end

  defp build(a = %Regex{}) do
    {:regex, a.source}
  end

  # otherwise literal
  defp build(a) do
    {:literal, a}
  end

  defp add_eval({op, rule}, eval) do
    {op, rule, eval}
  end
end
