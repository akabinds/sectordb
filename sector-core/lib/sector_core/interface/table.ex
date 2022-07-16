defmodule SectorCore.Native.Interface.Table do
  @moduledoc false

  @enforce_keys [:identifier]
  defstruct identifier: nil,
            columns: %{}
end
