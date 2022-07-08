defmodule ConcurrencyTest do
  use ExUnit.Case
  doctest Concurrency

  test "greets the world" do
    assert Concurrency.hello() == :world
  end
end
